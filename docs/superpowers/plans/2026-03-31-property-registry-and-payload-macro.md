# Property Registry & Declarative Payload Builder — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Eliminate property definition duplication by creating a centralized registry that feeds parse, validation, and payload generation from a single declaration per module.

**Architecture:** A new `org-canvas-define-properties` macro lets each module declare its properties once into a global registry. Validation queries this registry at runtime (replacing the hand-maintained `org-canvas--validate-specs` constant). A new `org-canvas-define-payload` macro generates `build-payload` functions from field specs. Migration is incremental — each step passes all tests.

**Tech Stack:** Emacs Lisp, Eldev (test/lint/complexity), buttercup (tests)

---

## File Map

- **Modify:** `lisp/org-canvas-core-config.el` — Add `org-canvas--property-registry` defvar, `org-canvas-register-properties` function, `org-canvas--get-property-spec` accessor
- **Modify:** `lisp/org-canvas-core-sync.el` — Add `org-canvas-define-properties` macro (near existing `org-canvas-define-parse`), add `org-canvas-define-payload` macro
- **Modify:** `lisp/org-canvas-validate.el` — Replace `org-canvas--validate-specs` constant with registry query; keep type validators, structural validators, and engine unchanged
- **Modify:** `lisp/org-canvas-announcements.el` — Migrate to `org-canvas-define-properties` + `org-canvas-define-payload`
- **Modify:** `lisp/org-canvas-pages.el` — Migrate to `org-canvas-define-properties` + `org-canvas-define-payload`
- **Modify:** `lisp/org-canvas-group-categories.el` — Migrate to `org-canvas-define-properties` + `org-canvas-define-payload`
- **Modify:** `lisp/org-canvas-assignment-groups.el` — Migrate to `org-canvas-define-properties`
- **Modify:** `lisp/org-canvas-calendar.el` — Migrate to `org-canvas-define-properties` + `org-canvas-define-payload`
- **Modify:** `lisp/org-canvas-discussions.el` — Migrate to `org-canvas-define-properties` (payload stays manual — graded sub-payload is too complex)
- **Modify:** `lisp/org-canvas-assignments.el` — Migrate to `org-canvas-define-properties` (validation only; parse+payload stay manual)
- **Modify:** `lisp/org-canvas-quizzes.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Modify:** `lisp/org-canvas-new-quizzes.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Modify:** `lisp/org-canvas-modules.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Modify:** `lisp/org-canvas-rubrics.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Modify:** `lisp/org-canvas-outcomes.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Modify:** `lisp/org-canvas-files.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Modify:** `lisp/org-canvas-settings.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Modify:** `lisp/org-canvas-sections.el` — Migrate to `org-canvas-define-properties` (validation only)
- **Create:** `test/org-canvas-property-registry-test.el` — Tests for the registry, payload macro, and validation integration
- **Modify:** `test/org-canvas-validate-test.el` — Update integration tests to use registry instead of constant
- **Modify:** `Eldev` — Add `org-canvas-property-registry` to undercover fileset (if in separate file) or verify existing coverage

---

## Task 1: Property Registry Infrastructure

Add the global registry data structure and registration function in `core-config.el`, plus the `org-canvas-define-properties` macro in `core-sync.el`.

**Files:**
- Modify: `lisp/org-canvas-core-config.el`
- Modify: `lisp/org-canvas-core-sync.el`
- Create: `test/org-canvas-property-registry-test.el`

### Registry Design

The registry is a hash-table keyed by feature name (string). Each entry is a plist:

```elisp
(:label "Announcements"               ;; display name for validation report
 :file-var org-canvas-announcements-file  ;; symbol
 :query "LEVEL=1"                     ;; org-map-entries query
 :id-key :canvas-id                   ;; output plist key for ID
 :id-property "CANVAS_ID"             ;; org property for ID
 :title-key :title                    ;; output plist key for title
 :entity-name "Announcement"          ;; singular for error messages
 :body-key :message                   ;; output plist key for HTML body (nil = none)
 :properties                          ;; list of property specs
 ((:org-prop "PUBLISHED"              ;; Org property name
   :data-key :published               ;; output plist key (for parse)
   :api-key "published"               ;; Canvas API field name (for payload, nil = skip)
   :type boolean                      ;; string|boolean|timestamp|number|enum|csv-enum|link
   :default t                         ;; default value (booleans/enums)
   :values nil                        ;; enum allowed values
   :target-file nil                   ;; link: target file-var symbol
   :link-id-property nil              ;; link: ID property on target
   :required nil                      ;; if t, always in payload; if nil, only when non-nil
   :boolean-json nil)                 ;; if t, convert to JSON boolean in payload
  ...)
 :date-order (("UNLOCK_AT" "DUE_AT" "LOCK_AT"))  ;; for validation
 :structural-fn nil                   ;; custom structural validator
 :after-read nil                      ;; parse hook
 :after-transform nil)                ;; parse hook
```

- [ ] **Step 1: Write failing tests for registry registration and query**

```elisp
;; test/org-canvas-property-registry-test.el
(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)

(describe "org-canvas--property-registry"
  (before-each
    (setq org-canvas--property-registry (make-hash-table :test 'equal)))

  (describe "org-canvas-register-properties"
    (it "stores a spec in the registry"
      (org-canvas-register-properties
       "announcements"
       :label "Announcements"
       :file-var 'org-canvas-announcements-file
       :query "LEVEL=1"
       :properties
       ((:org-prop "PUBLISHED" :data-key :published :type boolean :default t)))
      (expect (gethash "announcements" org-canvas--property-registry) :not :to-be nil))

    (it "does not overwrite existing entry on re-register"
      (org-canvas-register-properties
       "announcements"
       :label "Announcements"
       :file-var 'org-canvas-announcements-file
       :query "LEVEL=1"
       :properties ((:org-prop "PUBLISHED" :data-key :published :type boolean)))
      (let ((first (gethash "announcements" org-canvas--property-registry)))
        (org-canvas-register-properties
         "announcements"
         :label "Announcements V2"
         :file-var 'org-canvas-announcements-file
         :query "LEVEL=1"
         :properties nil)
        (expect (gethash "announcements" org-canvas--property-registry) :to-equal first)))

    (it "stores multiple features independently"
      (org-canvas-register-properties
       "announcements"
       :label "Announcements"
       :file-var 'org-canvas-announcements-file
       :query "LEVEL=1"
       :properties nil)
      (org-canvas-register-properties
       "pages"
       :label "Pages"
       :file-var 'org-canvas-pages-file
       :query "LEVEL=1"
       :properties nil)
      (expect (hash-table-count org-canvas--property-registry) :to-equal 2)))

  (describe "org-canvas--get-validate-specs-from-registry"
    (it "returns a validate-spec-compatible list"
      (org-canvas-register-properties
       "announcements"
       :label "Announcements"
       :file-var 'org-canvas-announcements-file
       :query "LEVEL=1"
       :properties
       ((:org-prop "PUBLISHED" :data-key :published :type boolean)
        (:org-prop "DELAYED_POST_AT" :data-key :delayed_post_at :type timestamp)))
      (let ((specs (org-canvas--get-validate-specs-from-registry)))
        (expect (length specs) :to-equal 1)
        (let ((spec (car specs)))
          (expect (plist-get spec :label) :to-equal "Announcements")
          (expect (plist-get spec :file) :to-equal 'org-canvas-announcements-file)
          (expect (plist-get spec :query) :to-equal "LEVEL=1")
          (let ((props (plist-get spec :properties)))
            (expect (length props) :to-equal 2)
            (expect (plist-get (car props) :name) :to-equal "PUBLISHED")
            (expect (plist-get (car props) :type) :to-equal 'boolean)))))

    (it "converts link properties with target-file and id-property"
      (org-canvas-register-properties
       "assignments"
       :label "Assignments"
       :file-var 'org-canvas-assignments-file
       :query "LEVEL=1"
       :properties
       ((:org-prop "GROUP" :data-key :group :type link
         :target-file org-canvas-assignment-groups-file
         :link-id-property "CANVAS_ID")))
      (let* ((specs (org-canvas--get-validate-specs-from-registry))
             (prop (car (plist-get (car specs) :properties))))
        (expect (plist-get prop :type) :to-equal 'link)
        (expect (plist-get prop :target-file) :to-equal 'org-canvas-assignment-groups-file)
        (expect (plist-get prop :id-property) :to-equal "CANVAS_ID")))

    (it "includes date-order and structural-fn"
      (org-canvas-register-properties
       "assignments"
       :label "Assignments"
       :file-var 'org-canvas-assignments-file
       :query "LEVEL=1"
       :date-order (("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
       :structural-fn #'org-canvas--validate-assignment-structure
       :properties nil)
      (let ((spec (car (org-canvas--get-validate-specs-from-registry))))
        (expect (plist-get spec :date-order) :to-equal '(("UNLOCK_AT" "DUE_AT" "LOCK_AT")))
        (expect (plist-get spec :structural-fn)
                :to-equal #'org-canvas--validate-assignment-structure)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `eldev test "property-registry"`
Expected: FAIL — `org-canvas--property-registry` and `org-canvas-register-properties` not defined

- [ ] **Step 3: Implement registry in core-config.el**

Add after the existing `org-canvas--feature-registry` block (around line 119):

```elisp
(defvar org-canvas--property-registry (make-hash-table :test 'equal)
  "Hash-table of feature-name → property spec plist.
Populated at module load time by `org-canvas-register-properties'.
Queried by validation, parse, and payload generation.")

(defun org-canvas-register-properties (feature-name &rest plist)
  "Register property definitions for FEATURE-NAME.
PLIST keys: :label :file-var :query :properties :date-order
:structural-fn :id-key :id-property :title-key :entity-name
:body-key :after-read :after-transform.
Does nothing if FEATURE-NAME is already registered."
  (unless (gethash feature-name org-canvas--property-registry)
    (puthash feature-name plist org-canvas--property-registry)))
```

- [ ] **Step 4: Implement validation spec converter**

Add in `core-config.el` after the register function:

```elisp
(defun org-canvas--property-to-validate-prop (prop)
  "Convert a registry property spec PROP to a validate-spec property."
  (let ((result (list :name (plist-get prop :org-prop)
                      :type (plist-get prop :type))))
    (when (plist-get prop :values)
      (setq result (plist-put result :values (plist-get prop :values))))
    (when (eq (plist-get prop :type) 'link)
      (setq result (plist-put result :target-file (plist-get prop :target-file)))
      (setq result (plist-put result :id-property (plist-get prop :link-id-property))))
    result))

(defun org-canvas--get-validate-specs-from-registry ()
  "Build validation specs from the property registry.
Returns a list compatible with `org-canvas--validate-spec'."
  (let ((specs nil))
    (maphash
     (lambda (_name plist)
       (push (list :label (plist-get plist :label)
                   :file (plist-get plist :file-var)
                   :query (plist-get plist :query)
                   :properties (mapcar #'org-canvas--property-to-validate-prop
                                       (plist-get plist :properties))
                   :date-order (plist-get plist :date-order)
                   :structural-fn (plist-get plist :structural-fn))
             specs))
     org-canvas--property-registry)
    (nreverse specs)))
```

- [ ] **Step 5: Run the new tests**

Run: `eldev test "property-registry"`
Expected: PASS (all 6 tests)

- [ ] **Step 6: Run full test suite**

Run: `eldev test`
Expected: All 2361+ tests pass (no regressions)

- [ ] **Step 7: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean — no new warnings, no functions above threshold

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-core-config.el test/org-canvas-property-registry-test.el
git commit -m "feat: add centralized property registry infrastructure

Add org-canvas--property-registry hash-table, org-canvas-register-properties
for module self-registration, and org-canvas--get-validate-specs-from-registry
for generating validation specs from the registry.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Migrate Announcements to Property Registry

The simplest module. Proves the pattern works end-to-end.

**Files:**
- Modify: `lisp/org-canvas-announcements.el`
- Modify: `test/org-canvas-property-registry-test.el`

- [ ] **Step 1: Write a test that announcements registers into the property registry**

Add to `test/org-canvas-property-registry-test.el`:

```elisp
(describe "Module registration: announcements"
  (it "registers properties into the registry at load time"
    ;; announcements.el is already loaded by test-helper
    (let ((spec (gethash "announcements" org-canvas--property-registry)))
      (expect spec :not :to-be nil)
      (expect (plist-get spec :label) :to-equal "Announcements")
      (expect (plist-get spec :file-var) :to-equal 'org-canvas-announcements-file)
      (expect (plist-get spec :query) :to-equal "LEVEL=1")
      (expect (length (plist-get spec :properties)) :to-equal 3))))
```

Note: We register 3 validation properties (PUBLISHED, POST_AT, ALLOW_COMMENTS) matching the current `org-canvas--validate-specs` entry for Announcements (fixing a latent bug where validation checked DELAYED_POST_AT but the Org property is POST_AT). SPECIFIC_SECTIONS is a parse-only property (not in current validation spec) so it stays in the parse macro only.

- [ ] **Step 2: Run the test to verify it fails**

Run: `eldev test "Module registration: announcements"`
Expected: FAIL — announcements hasn't registered yet

- [ ] **Step 3: Add org-canvas-register-properties call to announcements.el**

Add after the existing `org-canvas-register-feature` call (after line 48):

```elisp
(org-canvas-register-properties "announcements"
  :label "Announcements"
  :file-var 'org-canvas-announcements-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "PUBLISHED" :data-key :published :type boolean :default t)
   (:org-prop "POST_AT" :data-key :delayed_post_at :type timestamp
    :api-key "delayed_post_at")
   (:org-prop "ALLOW_COMMENTS" :data-key :allow_discussion_comments :type boolean
    :api-key nil)))  ;; ALLOW_COMMENTS has custom payload logic (lock_at), no direct mapping
```

Note: The `org-canvas-define-parse` and manual `build-payload` stay unchanged for now. This step only adds the registration — validation will consume it in Task 5.

- [ ] **Step 4: Run the new test**

Run: `eldev test "Module registration: announcements"`
Expected: PASS

- [ ] **Step 5: Run full test suite**

Run: `eldev test`
Expected: All tests pass (announcements still uses its existing parse/payload/validation)

- [ ] **Step 6: Commit**

```bash
git add lisp/org-canvas-announcements.el test/org-canvas-property-registry-test.el
git commit -m "feat: register announcements properties in centralized registry

First module to adopt org-canvas-register-properties. Existing parse,
payload, and validation code unchanged — registry will be consumed
by validation in a later step.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Migrate All Remaining Modules to Property Registry

Register properties for every module that has a validation spec. Each module gets an `org-canvas-register-properties` call. Tests verify all registrations.

**Files:**
- Modify: `lisp/org-canvas-pages.el`
- Modify: `lisp/org-canvas-group-categories.el`
- Modify: `lisp/org-canvas-assignment-groups.el`
- Modify: `lisp/org-canvas-calendar.el`
- Modify: `lisp/org-canvas-discussions.el`
- Modify: `lisp/org-canvas-assignments.el`
- Modify: `lisp/org-canvas-quizzes.el`
- Modify: `lisp/org-canvas-new-quizzes.el`
- Modify: `lisp/org-canvas-modules.el`
- Modify: `lisp/org-canvas-rubrics.el`
- Modify: `lisp/org-canvas-outcomes.el`
- Modify: `lisp/org-canvas-files.el`
- Modify: `lisp/org-canvas-settings.el`
- Modify: `lisp/org-canvas-sections.el`
- Modify: `test/org-canvas-property-registry-test.el`

The property specs must match the current `org-canvas--validate-specs` entries exactly — same types, values, date-order, and structural-fn. This ensures the switchover in Task 5 is a no-op for validation behavior.

- [ ] **Step 1: Write a comprehensive test that all modules register**

Add to `test/org-canvas-property-registry-test.el`:

```elisp
(describe "All module registrations"
  ;; Each entry: (feature-name expected-label expected-file-var property-count)
  (dolist (entry '(("announcements"      "Announcements"      org-canvas-announcements-file       3)
                   ("pages"              "Pages"              org-canvas-pages-file                5)
                   ("quizzes"            "Quizzes"            org-canvas-quizzes-file             18)
                   ("quiz-questions"     "Quiz Questions"     org-canvas-quizzes-file              5)
                   ("modules"            "Modules"            org-canvas-modules-file              4)
                   ("module-items"       "Module Items"       org-canvas-modules-file              5)
                   ("rubrics"            "Rubrics"            org-canvas-rubrics-file              1)
                   ("outcome-groups"     "Outcome Groups"     org-canvas-outcomes-file             0)
                   ("outcomes"           "Outcomes"           org-canvas-outcomes-file             3)
                   ("discussions"        "Discussions"        org-canvas-discussions-file          15)
                   ("assignments"        "Assignments"        org-canvas-assignments-file         24)
                   ("files"              "Files"              org-canvas-files-file                4)
                   ("assignment-groups"  "Assignment Groups"  org-canvas-assignment-groups-file    4)
                   ("new-quizzes"        "New Quizzes"        org-canvas-new-quizzes-file          7)
                   ("new-quiz-items"     "New Quiz Items"     org-canvas-new-quizzes-file          3)
                   ("settings"           "Settings"           org-canvas-settings-file            25)
                   ("sections"           "Sections"           org-canvas-sections-file             0)
                   ("group-categories"   "Group Categories"   org-canvas-group-categories-file     4)
                   ("calendar-events"    "Calendar Events"    org-canvas-calendar-events-file      3)))
    (let ((name (nth 0 entry))
          (label (nth 1 entry))
          (file-var (nth 2 entry))
          (prop-count (nth 3 entry)))
      (it (format "registers %s with %d properties" name prop-count)
        (let ((spec (gethash name org-canvas--property-registry)))
          (expect spec :not :to-be nil)
          (expect (plist-get spec :label) :to-equal label)
          (expect (plist-get spec :file-var) :to-equal file-var)
          (expect (length (plist-get spec :properties)) :to-equal prop-count))))))
```

- [ ] **Step 2: Run the test to verify it fails (only announcements passes)**

Run: `eldev test "All module registrations"`
Expected: 1 pass (announcements), 18 failures

- [ ] **Step 3: Add registrations to all remaining modules**

For each module, add an `org-canvas-register-properties` call after the existing `org-canvas-register-feature` (or after the `defcustom` for modules that don't use `register-feature`). The property list must match the current `org-canvas--validate-specs` entry.

**Pages** — add after line 53 of `org-canvas-pages.el`:
```elisp
(org-canvas-register-properties "pages"
  :label "Pages"
  :file-var 'org-canvas-pages-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "PUBLISHED"       :data-key :published       :type boolean :default t
    :api-key "published" :boolean-json t)
   (:org-prop "FRONT_PAGE"      :data-key :front_page      :type boolean
    :api-key "front_page" :boolean-json t)
   (:org-prop "EDITING_ROLES"   :data-key :editing_roles   :type csv-enum
    :values org-canvas--valid-editing-roles :api-key "editing_roles")
   (:org-prop "TODO_DATE"       :data-key :student_todo_at :type timestamp
    :api-key "student_todo_at")
   (:org-prop "NOTIFY_OF_UPDATE" :data-key :notify_of_update :type boolean
    :api-key "notify_of_update" :boolean-json t)))
```

**Group Categories** — add after line 42 of `org-canvas-group-categories.el`:
```elisp
(org-canvas-register-properties "group-categories"
  :label "Group Categories"
  :file-var 'org-canvas-group-categories-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "SELF_SIGNUP"        :data-key :self_signup        :type enum
    :values org-canvas--valid-self-signup-values :api-key "self_signup")
   (:org-prop "GROUP_LIMIT"        :data-key :group_limit        :type number
    :api-key "group_limit")
   (:org-prop "AUTO_LEADER"        :data-key :auto_leader        :type enum
    :values org-canvas--valid-auto-leader-values :api-key "auto_leader")
   (:org-prop "CREATE_GROUP_COUNT" :data-key :create_group_count :type number
    :api-key "create_group_count")))
```

**Assignment Groups** — add after line 55 of `org-canvas-assignment-groups.el`:
```elisp
(org-canvas-register-properties "assignment-groups"
  :label "Assignment Groups"
  :file-var 'org-canvas-assignment-groups-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "WEIGHT"       :data-key :group_weight  :type number :api-key "group_weight")
   (:org-prop "DROP_LOWEST"  :data-key :drop_lowest   :type number :api-key nil)
   (:org-prop "DROP_HIGHEST" :data-key :drop_highest  :type number :api-key nil)
   (:org-prop "POSITION"     :data-key :position      :type number :api-key "position"))
  :structural-fn #'org-canvas--validate-drop-rules)
```

**Calendar Events** — add after line 44 of `org-canvas-calendar.el`:
```elisp
(org-canvas-register-properties "calendar-events"
  :label "Calendar Events"
  :file-var 'org-canvas-calendar-events-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "START_AT"         :data-key :start_at         :type timestamp
    :api-key "start_at" :required t)
   (:org-prop "END_AT"           :data-key :end_at           :type timestamp
    :api-key "end_at")
   (:org-prop "ALL_DAY"          :data-key :all_day          :type boolean
    :api-key "all_day" :boolean-json t)))
```

**Discussions** — add after line 54 of `org-canvas-discussions.el`:
```elisp
(org-canvas-register-properties "discussions"
  :label "Discussions"
  :file-var 'org-canvas-discussions-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "PUBLISHED"          :data-key :published              :type boolean)
   (:org-prop "DISCUSSION_TYPE"    :data-key :discussion_type        :type enum
    :values org-canvas--valid-discussion-types)
   (:org-prop "GRADING_TYPE"       :data-key :grading_type           :type enum
    :values org-canvas--valid-grading-types)
   (:org-prop "POINTS"             :data-key :points_possible        :type number)
   (:org-prop "POST_FIRST"         :data-key :require_initial_post   :type boolean)
   (:org-prop "PINNED"             :data-key :pinned                 :type boolean)
   (:org-prop "AVAILABLE_FROM"     :data-key :delayed_post_at        :type timestamp)
   (:org-prop "DUE_AT"             :data-key :due_at                 :type timestamp)
   (:org-prop "LOCK_AT"            :data-key :lock_at                :type timestamp)
   (:org-prop "ALLOW_RATING"       :data-key :allow_rating           :type boolean)
   (:org-prop "ONLY_GRADERS_CAN_RATE" :data-key :only_graders_can_rate :type boolean)
   (:org-prop "SORT_BY_RATING"     :data-key :sort_by_rating         :type boolean)
   (:org-prop "GROUP_CATEGORY"     :data-key :group_category_id      :type number)
   (:org-prop "GROUP"              :data-key :assignment_group_id     :type link
    :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID")
   (:org-prop "RUBRIC_LINK"        :data-key :rubric_id              :type link
    :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID"))
  :date-order (("AVAILABLE_FROM" "DUE_AT" "LOCK_AT")))
```

**Assignments** — add after the `register-feature` call in `org-canvas-assignments.el`:
```elisp
(org-canvas-register-properties "assignments"
  :label "Assignments"
  :file-var 'org-canvas-assignments-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "POINTS"           :data-key :points_possible    :type number)
   (:org-prop "GRADING_TYPE"     :data-key :grading_type       :type enum
    :values org-canvas--valid-grading-types)
   (:org-prop "PUBLISHED"        :data-key :published          :type boolean)
   (:org-prop "SUBMISSION"       :data-key :submission_types   :type csv-enum
    :values org-canvas--valid-submission-types)
   (:org-prop "MAX_ATTEMPTS"     :data-key :allowed_attempts   :type number)
   (:org-prop "DUE_AT"           :data-key :due_at             :type timestamp)
   (:org-prop "UNLOCK_AT"        :data-key :unlock_at          :type timestamp)
   (:org-prop "LOCK_AT"          :data-key :lock_at            :type timestamp)
   (:org-prop "PEER_REVIEWS"     :data-key :peer_reviews       :type boolean)
   (:org-prop "PEER_REVIEW_COUNT" :data-key :peer_review_count :type number)
   (:org-prop "PEER_REVIEW_DUE_AT" :data-key :peer_reviews_due_at :type timestamp)
   (:org-prop "GROUP"            :data-key :assignment_group_id :type link
    :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID")
   (:org-prop "RUBRIC_LINK"      :data-key :rubric_id          :type link
    :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID")
   (:org-prop "OMIT_FROM_GRADES" :data-key :omit_from_final_grade :type boolean)
   (:org-prop "ANONYMOUS_GRADING" :data-key :anonymous_grading :type boolean)
   (:org-prop "NOTIFY_OF_UPDATE" :data-key :notify_of_update   :type boolean)
   (:org-prop "AUTOMATIC_PEER_REVIEWS" :data-key :automatic_peer_reviews :type boolean)
   (:org-prop "GRADE_INDIVIDUALLY" :data-key :grade_group_students_individually :type boolean)
   (:org-prop "ONLY_VISIBLE_TO_OVERRIDES" :data-key :only_visible_to_overrides :type boolean)
   (:org-prop "MODERATED_GRADING" :data-key :moderated_grading :type boolean)
   (:org-prop "GRADER_COUNT"     :data-key :grader_count       :type number)
   (:org-prop "MUTED"            :data-key :muted              :type boolean)
   (:org-prop "TURNITIN_ENABLED" :data-key :turnitin_enabled   :type boolean)
   (:org-prop "GRADING_STANDARD_ID" :data-key :grading_standard_id :type number)
   (:org-prop "GROUP_CATEGORY_ID" :data-key :group_category_id :type number)
   (:org-prop "POSITION"         :data-key :position           :type number))
  :date-order (("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
  :structural-fn #'org-canvas--validate-assignment-structure)
```

**Quizzes** — register two entries (quizzes + quiz-questions), add in `org-canvas-quizzes.el`:
```elisp
(org-canvas-register-properties "quizzes"
  :label "Quizzes"
  :file-var 'org-canvas-quizzes-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "QUIZ_TYPE"        :data-key :quiz_type        :type enum
    :values org-canvas--valid-quiz-types)
   (:org-prop "PUBLISHED"        :data-key :published        :type boolean)
   (:org-prop "SHUFFLE_ANSWERS"  :data-key :shuffle_answers  :type boolean)
   (:org-prop "TIME_LIMIT"       :data-key :time_limit       :type number)
   (:org-prop "ALLOWED_ATTEMPTS" :data-key :allowed_attempts :type number)
   (:org-prop "DUE_AT"           :data-key :due_at           :type timestamp)
   (:org-prop "UNLOCK_AT"        :data-key :unlock_at        :type timestamp)
   (:org-prop "LOCK_AT"          :data-key :lock_at          :type timestamp)
   (:org-prop "SHOW_CORRECT_ANSWERS" :data-key :show_correct_answers :type boolean)
   (:org-prop "SHOW_CORRECT_ANSWERS_AT" :data-key :show_correct_answers_at :type timestamp)
   (:org-prop "HIDE_CORRECT_ANSWERS_AT" :data-key :hide_correct_answers_at :type timestamp)
   (:org-prop "HIDE_RESULTS"     :data-key :hide_results     :type enum
    :values org-canvas--valid-hide-results)
   (:org-prop "SCORING_POLICY"   :data-key :scoring_policy   :type enum
    :values org-canvas--valid-scoring-policies)
   (:org-prop "ONE_QUESTION_AT_A_TIME" :data-key :one_question_at_a_time :type boolean)
   (:org-prop "CANT_GO_BACK"     :data-key :cant_go_back     :type boolean)
   (:org-prop "SHOW_CORRECT_ANSWERS_LAST_ATTEMPT" :data-key :show_correct_answers_last_attempt :type boolean)
   (:org-prop "ONE_TIME_RESULTS" :data-key :one_time_results :type boolean)
   (:org-prop "ONLY_VISIBLE_TO_OVERRIDES" :data-key :only_visible_to_overrides :type boolean)
   (:org-prop "GROUP"            :data-key :assignment_group_id :type link
    :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID"))
  :date-order (("UNLOCK_AT" "DUE_AT" "LOCK_AT"))
  :structural-fn #'org-canvas--validate-quiz-point-total)

(org-canvas-register-properties "quiz-questions"
  :label "Quiz Questions"
  :file-var 'org-canvas-quizzes-file
  :query "LEVEL=2"
  :properties
  ((:org-prop "TYPE"              :data-key :type             :type enum
    :values org-canvas--valid-question-types)
   (:org-prop "POINTS"            :data-key :points           :type number)
   (:org-prop "PICK_COUNT"        :data-key :pick_count       :type number)
   (:org-prop "QUESTION_POINTS"   :data-key :question_points  :type number)
   (:org-prop "QUESTION_BANK_ID"  :data-key :question_bank_id :type number)))
```

**New Quizzes** — register two entries, add in `org-canvas-new-quizzes.el`:
```elisp
(org-canvas-register-properties "new-quizzes"
  :label "New Quizzes"
  :file-var 'org-canvas-new-quizzes-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "TIME_LIMIT"       :data-key :time_limit       :type number)
   (:org-prop "SHUFFLE_ANSWERS"  :data-key :shuffle_answers  :type boolean)
   (:org-prop "ONE_AT_A_TIME"    :data-key :one_at_a_time    :type boolean)
   (:org-prop "ALLOWED_ATTEMPTS" :data-key :allowed_attempts :type number)
   (:org-prop "SCORING_POLICY"   :data-key :scoring_policy   :type enum
    :values org-canvas--valid-new-quiz-scoring-policies)
   (:org-prop "GROUP"            :data-key :assignment_group_id :type link
    :target-file org-canvas-assignment-groups-file :link-id-property "CANVAS_ID")
   (:org-prop "RUBRIC_LINK"      :data-key :rubric_id        :type link
    :target-file org-canvas-rubrics-file :link-id-property "CANVAS_ID")))

(org-canvas-register-properties "new-quiz-items"
  :label "New Quiz Items"
  :file-var 'org-canvas-new-quizzes-file
  :query "LEVEL=2"
  :properties
  ((:org-prop "POINTS"  :data-key :points  :type number)
   (:org-prop "TYPE"    :data-key :type    :type enum
    :values org-canvas--valid-new-quiz-types)
   (:org-prop "OUTCOME" :data-key :outcome :type link
    :target-file org-canvas-outcomes-file :link-id-property "CANVAS_ID")))
```

**Modules** — register two entries, add in `org-canvas-modules.el`:
```elisp
(org-canvas-register-properties "modules"
  :label "Modules"
  :file-var 'org-canvas-modules-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "PUBLISHED"                     :data-key :published                     :type boolean)
   (:org-prop "UNLOCK_AT"                     :data-key :unlock_at                     :type timestamp)
   (:org-prop "REQUIRE_SEQUENTIAL_PROGRESSION" :data-key :require_sequential_progression :type boolean)
   (:org-prop "PUBLISH_FINAL_GRADE"           :data-key :publish_final_grade           :type boolean)))

(org-canvas-register-properties "module-items"
  :label "Module Items"
  :file-var 'org-canvas-modules-file
  :query "LEVEL=2"
  :properties
  ((:org-prop "INDENT"                 :data-key :indent                 :type number)
   (:org-prop "COMPLETION_REQUIREMENT" :data-key :completion_requirement :type enum
    :values org-canvas--valid-completion-requirements)
   (:org-prop "MIN_SCORE"             :data-key :min_score              :type number)
   (:org-prop "NEW_TAB"              :data-key :new_tab                :type boolean)
   (:org-prop "PUBLISHED"            :data-key :published              :type boolean))
  :structural-fn #'org-canvas--validate-module-item-link)
```

**Rubrics** — add in `org-canvas-rubrics.el`:
```elisp
(org-canvas-register-properties "rubrics"
  :label "Rubrics"
  :file-var 'org-canvas-rubrics-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "FREE_FORM_CRITERION_COMMENTS" :data-key :free-form :type boolean))
  :structural-fn #'org-canvas--validate-rubric-structure)
```

**Outcomes** — register two entries, add in `org-canvas-outcomes.el`:
```elisp
(org-canvas-register-properties "outcome-groups"
  :label "Outcome Groups"
  :file-var 'org-canvas-outcomes-file
  :query "LEVEL=1"
  :properties nil)

(org-canvas-register-properties "outcomes"
  :label "Outcomes"
  :file-var 'org-canvas-outcomes-file
  :query "LEVEL=2"
  :properties
  ((:org-prop "CALCULATION_METHOD" :data-key :calculation_method :type enum
    :values org-canvas--valid-calculation-methods)
   (:org-prop "CALCULATION_INT"    :data-key :calculation_int    :type number)
   (:org-prop "MASTERY_POINTS"     :data-key :mastery_points     :type number)))
```

**Files** — add in `org-canvas-files.el`:
```elisp
(org-canvas-register-properties "files"
  :label "Files"
  :file-var 'org-canvas-files-file
  :query "LEVEL>0"
  :properties
  ((:org-prop "PUBLISHED"        :data-key :published        :type boolean)
   (:org-prop "UNLOCK_AT"        :data-key :unlock_at        :type timestamp)
   (:org-prop "LOCK_AT"          :data-key :lock_at          :type timestamp)
   (:org-prop "USE_JUSTIFICATION" :data-key :use_justification :type enum
    :values org-canvas--valid-use-justifications))
  :structural-fn #'org-canvas--validate-file-structure)
```

**Settings** — add in `org-canvas-settings.el`:
```elisp
(org-canvas-register-properties "settings"
  :label "Settings"
  :file-var 'org-canvas-settings-file
  :query "LEVEL=1"
  :properties
  ((:org-prop "APPLY_WEIGHTS"                    :data-key :apply_weights                    :type boolean)
   (:org-prop "HIDE_FINAL_GRADES"                :data-key :hide_final_grades                :type boolean)
   (:org-prop "PUBLIC_SYLLABUS"                   :data-key :public_syllabus                   :type boolean)
   (:org-prop "IS_PUBLIC"                         :data-key :is_public                         :type boolean)
   (:org-prop "DEFAULT_VIEW"                      :data-key :default_view                      :type enum
    :values org-canvas--valid-views)
   (:org-prop "LICENSE"                           :data-key :license                           :type enum
    :values org-canvas--valid-licenses)
   (:org-prop "START_AT"                          :data-key :start_at                          :type timestamp)
   (:org-prop "END_AT"                            :data-key :end_at                            :type timestamp)
   (:org-prop "ALLOW_STUDENT_DISCUSSION_TOPICS"   :data-key :allow_student_discussion_topics   :type boolean)
   (:org-prop "ALLOW_STUDENT_DISCUSSION_EDITING"  :data-key :allow_student_discussion_editing  :type boolean)
   (:org-prop "ALLOW_STUDENT_FORUM_ATTACHMENTS"   :data-key :allow_student_forum_attachments   :type boolean)
   (:org-prop "LOCK_ALL_ANNOUNCEMENTS"            :data-key :lock_all_announcements            :type boolean)
   (:org-prop "RESTRICT_STUDENT_FUTURE_VIEW"      :data-key :restrict_student_future_view      :type boolean)
   (:org-prop "RESTRICT_STUDENT_PAST_VIEW"        :data-key :restrict_student_past_view        :type boolean)
   (:org-prop "SHOW_ANNOUNCEMENTS_ON_HOME_PAGE"   :data-key :show_announcements_on_home_page   :type boolean)
   (:org-prop "HOME_PAGE_ANNOUNCEMENT_LIMIT"      :data-key :home_page_announcement_limit      :type number)
   (:org-prop "HIDE_DISTRIBUTION_GRAPHS"          :data-key :hide_distribution_graphs          :type boolean)
   (:org-prop "GRADING_STANDARD_ID"               :data-key :grading_standard_id               :type number)
   (:org-prop "LATE_SUBMISSION_DEDUCTION"          :data-key :late_submission_deduction          :type number)
   (:org-prop "LATE_SUBMISSION_DEDUCTION_ENABLED"  :data-key :late_submission_deduction_enabled  :type boolean)
   (:org-prop "LATE_SUBMISSION_INTERVAL"           :data-key :late_submission_interval           :type enum
    :values org-canvas--valid-late-intervals)
   (:org-prop "LATE_SUBMISSION_MINIMUM_PERCENT"    :data-key :late_submission_minimum_percent    :type number)
   (:org-prop "LATE_SUBMISSION_MINIMUM_PERCENT_ENABLED" :data-key :late_submission_minimum_percent_enabled :type boolean)
   (:org-prop "MISSING_SUBMISSION_DEDUCTION"       :data-key :missing_submission_deduction       :type number)
   (:org-prop "MISSING_SUBMISSION_DEDUCTION_ENABLED" :data-key :missing_submission_deduction_enabled :type boolean)))
```

**Sections** — add in `org-canvas-sections.el`:
```elisp
(org-canvas-register-properties "sections"
  :label "Sections"
  :file-var 'org-canvas-sections-file
  :query "LEVEL=1"
  :properties nil
  :structural-fn #'org-canvas--validate-section-structure)
```

- [ ] **Step 4: Run the comprehensive test**

Run: `eldev test "All module registrations"`
Expected: All 19 entries pass

- [ ] **Step 5: Run full test suite**

Run: `eldev test`
Expected: All tests pass

- [ ] **Step 6: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean

- [ ] **Step 7: Commit**

```bash
git add lisp/org-canvas-*.el test/org-canvas-property-registry-test.el
git commit -m "feat: register all modules in centralized property registry

All 19 validation targets now self-register via org-canvas-register-properties.
Property specs match existing org-canvas--validate-specs entries exactly.
Registry is populated but not yet consumed — switchover happens next.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Define-Properties Macro (Syntactic Sugar)

Replace the verbose `org-canvas-register-properties` calls with a cleaner `org-canvas-define-properties` macro that provides compile-time validation and a more consistent syntax with the existing `define-parse` / `define-sync` macros.

**Files:**
- Modify: `lisp/org-canvas-core-sync.el`
- Modify: `lisp/org-canvas-announcements.el` (convert as example)
- Modify: `test/org-canvas-property-registry-test.el`

- [ ] **Step 1: Write test for the macro**

Add to `test/org-canvas-property-registry-test.el`:

```elisp
(describe "org-canvas-define-properties macro"
  (before-each
    (setq org-canvas--property-registry (make-hash-table :test 'equal)))

  (it "registers properties from macro form"
    (eval
     '(org-canvas-define-properties "test-feature"
        :label "Test Feature"
        :file-var org-canvas-test-file
        :query "LEVEL=1"
        :properties
        (("PUBLISHED" :published :type boolean :default t :api-key "published" :boolean-json t)
         ("DUE_AT"    :due_at    :type timestamp :api-key "due_at")))
     t)
    (let ((spec (gethash "test-feature" org-canvas--property-registry)))
      (expect spec :not :to-be nil)
      (expect (plist-get spec :label) :to-equal "Test Feature")
      (let ((props (plist-get spec :properties)))
        (expect (length props) :to-equal 2)
        (let ((pub (car props)))
          (expect (plist-get pub :org-prop) :to-equal "PUBLISHED")
          (expect (plist-get pub :data-key) :to-equal :published)
          (expect (plist-get pub :type) :to-equal 'boolean)
          (expect (plist-get pub :default) :to-equal t)
          (expect (plist-get pub :api-key) :to-equal "published")
          (expect (plist-get pub :boolean-json) :to-equal t)))))

  (it "errors at compile time if :label is missing"
    (expect
     (eval '(org-canvas-define-properties "bad"
              :file-var org-canvas-test-file
              :query "LEVEL=1"
              :properties nil)
           t)
     :to-throw 'error))

  (it "errors at compile time if :file-var is missing"
    (expect
     (eval '(org-canvas-define-properties "bad"
              :label "Bad"
              :query "LEVEL=1"
              :properties nil)
           t)
     :to-throw 'error)))
```

- [ ] **Step 2: Run test to verify failure**

Run: `eldev test "org-canvas-define-properties macro"`
Expected: FAIL — macro not defined

- [ ] **Step 3: Implement the macro in core-sync.el**

Add before the existing `org-canvas-define-parse` section (around line 38):

```elisp
;;;; 5a. Declarative Property Registration Macro
;;
;; Central declaration of all properties for a module.
;; Feeds into validation, parse, and payload generation.

(defmacro org-canvas-define-properties (feature-name &rest args)
  "Register property definitions for FEATURE-NAME (a string).

ARGS is a plist with required keys :label, :file-var, :query,
:properties, and optional keys :date-order, :structural-fn,
:id-key, :id-property, :title-key, :entity-name, :body-key,
:after-read, :after-transform.

:properties is a list of (ORG-PROP DATA-KEY &rest OPTS) tuples.
OPTS plist: :type, :default, :values, :api-key, :boolean-json,
:required, :target-file, :link-id-property.

Example:
  (org-canvas-define-properties \"announcements\"
    :label \"Announcements\"
    :file-var org-canvas-announcements-file
    :query \"LEVEL=1\"
    :properties
    ((\"PUBLISHED\" :published :type boolean :default t :api-key \"published\")
     (\"POST_AT\"   :delayed_post_at :type timestamp :api-key \"delayed_post_at\")))"
  (declare (indent 1))
  (unless (plist-get args :label)
    (error "org-canvas-define-properties %s: :label is required" feature-name))
  (unless (plist-get args :file-var)
    (error "org-canvas-define-properties %s: :file-var is required" feature-name))
  (let ((props-raw (plist-get args :properties))
        (label (plist-get args :label))
        (file-var (plist-get args :file-var))
        (query (or (plist-get args :query) "LEVEL=1"))
        (date-order (plist-get args :date-order))
        (structural-fn (plist-get args :structural-fn))
        (id-key (or (plist-get args :id-key) :canvas-id))
        (id-property (or (plist-get args :id-property) "CANVAS_ID"))
        (title-key (or (plist-get args :title-key) :title))
        (entity-name (plist-get args :entity-name))
        (body-key (plist-get args :body-key))
        (after-read (plist-get args :after-read))
        (after-transform (plist-get args :after-transform)))
    ;; Convert compact property tuples to full plists
    (let ((props-expanded
           (mapcar (lambda (spec)
                     (let ((org-prop (nth 0 spec))
                           (data-key (nth 1 spec))
                           (opts (cddr spec)))
                       (list :org-prop org-prop
                             :data-key data-key
                             :type (or (plist-get opts :type) 'string)
                             :default (plist-get opts :default)
                             :values (plist-get opts :values)
                             :api-key (plist-get opts :api-key)
                             :boolean-json (plist-get opts :boolean-json)
                             :required (plist-get opts :required)
                             :target-file (plist-get opts :target-file)
                             :link-id-property (plist-get opts :link-id-property))))
                   props-raw)))
      `(org-canvas-register-properties ,feature-name
         :label ,label
         :file-var ',file-var
         :query ,query
         :properties ',props-expanded
         ,@(when date-order (list :date-order `',date-order))
         ,@(when structural-fn (list :structural-fn structural-fn))
         :id-key ,id-key
         :id-property ,id-property
         :title-key ,title-key
         ,@(when entity-name (list :entity-name entity-name))
         ,@(when body-key (list :body-key body-key))
         ,@(when after-read (list :after-read after-read))
         ,@(when after-transform (list :after-transform after-transform))))))
```

- [ ] **Step 4: Run the macro tests**

Run: `eldev test "org-canvas-define-properties macro"`
Expected: PASS

- [ ] **Step 5: Convert announcements to use the macro (proving the syntax)**

Replace the `org-canvas-register-properties` call in announcements.el with:

```elisp
(org-canvas-define-properties "announcements"
  :label "Announcements"
  :file-var org-canvas-announcements-file
  :query "LEVEL=1"
  :body-key :message
  :properties
  (("PUBLISHED"     :published                 :type boolean :default t
    :api-key "published" :boolean-json t)
   ("POST_AT"         :delayed_post_at         :type timestamp
    :api-key "delayed_post_at")
   ("ALLOW_COMMENTS" :allow_discussion_comments :type boolean)))
```

- [ ] **Step 6: Run full test suite**

Run: `eldev test`
Expected: All tests pass

- [ ] **Step 7: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-core-sync.el lisp/org-canvas-announcements.el test/org-canvas-property-registry-test.el
git commit -m "feat: add org-canvas-define-properties macro

Syntactic sugar over org-canvas-register-properties with compile-time
validation and compact property tuple syntax. Convert announcements as
proof of concept.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Switch Validation to Use Registry

Replace `org-canvas--validate-specs` constant with registry queries. This is the key deduplication step.

**Files:**
- Modify: `lisp/org-canvas-validate.el`
- Modify: `test/org-canvas-validate-test.el`
- Modify: `test/org-canvas-property-registry-test.el`

- [ ] **Step 1: Write a test that registry specs match the old constant**

Add to `test/org-canvas-property-registry-test.el`:

```elisp
(describe "Registry-to-validation spec equivalence"
  (it "produces the same number of specs as the old constant"
    (let ((registry-specs (org-canvas--get-validate-specs-from-registry)))
      ;; Old constant had 19 entries
      (expect (length registry-specs) :to-equal 19)))

  (it "covers every label from the old constant"
    (let* ((registry-specs (org-canvas--get-validate-specs-from-registry))
           (labels (mapcar (lambda (s) (plist-get s :label)) registry-specs)))
      (dolist (expected-label '("Assignments" "Pages" "Quizzes" "Quiz Questions"
                                "Modules" "Module Items" "Rubrics" "Outcome Groups"
                                "Outcomes" "Discussions" "Announcements" "Files"
                                "Assignment Groups" "New Quizzes" "New Quiz Items"
                                "Settings" "Sections" "Group Categories"
                                "Calendar Events"))
        (expect labels :to-contain expected-label))))

  (it "has matching property counts for each label"
    (let ((registry-specs (org-canvas--get-validate-specs-from-registry)))
      ;; Spot-check a few key modules
      (dolist (spec registry-specs)
        (let ((label (plist-get spec :label))
              (prop-count (length (plist-get spec :properties))))
          (pcase label
            ("Announcements" (expect prop-count :to-equal 3))
            ("Assignments" (expect prop-count :to-equal 24))
            ("Settings" (expect prop-count :to-equal 25))
            ("Sections" (expect prop-count :to-equal 0))))))))
```

- [ ] **Step 2: Run the equivalence test**

Run: `eldev test "Registry-to-validation spec equivalence"`
Expected: PASS (registry already populated by all modules)

- [ ] **Step 3: Replace the constant in validate.el**

In `lisp/org-canvas-validate.el`, replace the entire `org-canvas--validate-specs` defconst (lines 178-419) with:

```elisp
(defun org-canvas--validate-specs ()
  "Return validation specs from the property registry.
This replaces the former `org-canvas--validate-specs' constant."
  (org-canvas--get-validate-specs-from-registry))
```

- [ ] **Step 4: Update all references from constant to function call**

In `lisp/org-canvas-validate.el`, find every reference to `org-canvas--validate-specs` as a variable and change it to a function call:

`org-canvas--validate-run-all-specs` (around line 807): change `(dolist (spec org-canvas--validate-specs)` to `(dolist (spec (org-canvas--validate-specs))`

- [ ] **Step 5: Run the full validation test suite**

Run: `eldev test "validate"`
Expected: All validation tests pass

- [ ] **Step 6: Run full test suite**

Run: `eldev test`
Expected: All tests pass

- [ ] **Step 7: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-validate.el test/org-canvas-property-registry-test.el
git commit -m "refactor: replace validate-specs constant with registry query

org-canvas--validate-specs is now a function that queries the property
registry instead of a hand-maintained constant. Property definitions
are no longer duplicated between modules and validate.el.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Convert Remaining Modules to define-properties Macro

Replace the `org-canvas-register-properties` calls added in Task 3 with the cleaner `org-canvas-define-properties` macro syntax.

**Files:**
- Modify: All feature module files listed in Task 3

- [ ] **Step 1: Convert all modules**

For each module, replace the `org-canvas-register-properties` call with an `org-canvas-define-properties` call using the compact tuple syntax. The conversion is mechanical — the content is the same, just the syntax changes.

Example — pages.el becomes:
```elisp
(org-canvas-define-properties "pages"
  :label "Pages"
  :file-var org-canvas-pages-file
  :query "LEVEL=1"
  :id-key :canvas-url
  :id-property "CANVAS_URL"
  :entity-name "Page"
  :body-key :body
  :properties
  (("PUBLISHED"       :published       :type boolean :default t
    :api-key "published" :boolean-json t)
   ("FRONT_PAGE"      :front_page      :type boolean
    :api-key "front_page" :boolean-json t)
   ("EDITING_ROLES"   :editing_roles   :type csv-enum
    :values org-canvas--valid-editing-roles :api-key "editing_roles")
   ("TODO_DATE"       :student_todo_at :type timestamp
    :api-key "student_todo_at")
   ("NOTIFY_OF_UPDATE" :notify_of_update :type boolean
    :api-key "notify_of_update" :boolean-json t)))
```

Repeat for all modules. Modules that only use the registry for validation (assignments, quizzes, etc.) don't need `:api-key` on their properties.

- [ ] **Step 2: Run full test suite**

Run: `eldev test`
Expected: All tests pass

- [ ] **Step 3: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean

- [ ] **Step 4: Commit**

```bash
git add lisp/org-canvas-*.el
git commit -m "refactor: convert all modules to org-canvas-define-properties macro

Replace verbose org-canvas-register-properties calls with the cleaner
macro syntax across all 19 module registrations.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Declarative Payload Builder Macro

Add `org-canvas-define-payload` that generates `build-payload` functions from property specs.

**Files:**
- Modify: `lisp/org-canvas-core-sync.el`
- Modify: `test/org-canvas-property-registry-test.el`

### Payload Macro Design

The macro generates a function that:
1. Logs the Stage 2 header
2. Creates the payload structure (alist or hash-table)
3. Adds required fields
4. Conditionally adds non-nil fields
5. Converts booleans to JSON format where needed
6. Wraps in a key if `:wrapper-key` specified
7. Calls `:post-build-fn` if provided
8. Logs completion

```elisp
;; Alist output (announcements-style):
(org-canvas-define-payload announcement
  :format alist
  :title-key :title
  :title-api-key title
  :body-key :message
  :body-api-key message
  :static-fields ((is_announcement . t)
                  (discussion_type . "side_comment"))
  :post-build-fn #'org-canvas--announcement-post-build)

;; Hash-table output with wrapper (pages-style):
(org-canvas-define-payload page
  :format hash-table
  :wrapper-key "wiki_page"
  :title-key :title
  :title-api-key "title"
  :body-key :body
  :body-api-key "body")

;; Hash-table output with wrapper (calendar-style):
(org-canvas-define-payload calendar-event
  :format hash-table
  :wrapper-key "calendar_event"
  :title-key :title
  :title-api-key "title"
  :body-key :description
  :body-api-key "description"
  :extra-required-fn #'org-canvas--calendar-event-extra-required)
```

The macro reads the `:api-key`, `:boolean-json`, and `:required` fields from the property registry to determine which fields to include and how.

- [ ] **Step 1: Write failing tests for the payload macro**

Add to `test/org-canvas-property-registry-test.el`:

```elisp
(describe "org-canvas-define-payload macro"
  (before-each
    (setq org-canvas--property-registry (make-hash-table :test 'equal))
    (org-canvas-register-properties "test-payload"
      :label "Test"
      :file-var 'org-canvas-test-file
      :query "LEVEL=1"
      :properties
      '((:org-prop "PUBLISHED" :data-key :published :type boolean :default t
         :api-key "published" :boolean-json t)
        (:org-prop "DUE_AT" :data-key :due_at :type timestamp :api-key "due_at")
        (:org-prop "NOTE" :data-key :note :type string :api-key "note"))))

  (describe "alist format"
    (before-each
      (eval
       '(org-canvas-define-payload test-payload
          :registry-key "test-payload"
          :format alist
          :title-key :title
          :title-api-key title)
       t))

    (it "generates a build-payload function"
      (expect (fboundp 'org-canvas--test-payload-build-payload) :to-be-truthy))

    (it "includes title in payload"
      (spy-on 'elog-info)
      (spy-on 'elog-debug)
      (let ((result (org-canvas--test-payload-build-payload
                     '(:title "My Item" :published t :due_at "2026-06-01T00:00:00Z"))))
        (expect (alist-get 'title result) :to-equal "My Item")))

    (it "converts boolean to JSON format"
      (spy-on 'elog-info)
      (spy-on 'elog-debug)
      (let ((result (org-canvas--test-payload-build-payload
                     '(:title "My Item" :published t))))
        (expect (alist-get 'published result) :to-equal t)))

    (it "skips nil optional fields"
      (spy-on 'elog-info)
      (spy-on 'elog-debug)
      (let ((result (org-canvas--test-payload-build-payload
                     '(:title "My Item" :published t :due_at nil :note nil))))
        (expect (assq 'due_at result) :to-be nil)
        (expect (assq 'note result) :to-be nil)))

    (it "includes non-nil optional fields"
      (spy-on 'elog-info)
      (spy-on 'elog-debug)
      (let ((result (org-canvas--test-payload-build-payload
                     '(:title "My Item" :due_at "2026-06-01T00:00:00Z"))))
        (expect (alist-get 'due_at result) :to-equal "2026-06-01T00:00:00Z"))))

  (describe "hash-table format with wrapper"
    (before-each
      (eval
       '(org-canvas-define-payload test-ht
          :registry-key "test-payload"
          :format hash-table
          :wrapper-key "wiki_page"
          :title-key :title
          :title-api-key "title"
          :body-key :body
          :body-api-key "body")
       t))

    (it "wraps payload in wrapper key"
      (spy-on 'elog-info)
      (spy-on 'elog-debug)
      (let ((result (org-canvas--test-ht-build-payload
                     '(:title "Page" :body "<p>hi</p>" :published t))))
        (expect (hash-table-p result) :to-be-truthy)
        (expect (gethash "wiki_page" result) :not :to-be nil)
        (let ((inner (gethash "wiki_page" result)))
          (expect (gethash "title" inner) :to-equal "Page")
          (expect (gethash "body" inner) :to-equal "<p>hi</p>")
          (expect (gethash "published" inner) :to-equal t)))))

  (describe "static fields"
    (before-each
      (eval
       '(org-canvas-define-payload test-static
          :registry-key "test-payload"
          :format alist
          :title-key :title
          :title-api-key title
          :static-fields ((is_announcement . t)
                          (discussion_type . "side_comment")))
       t))

    (it "includes static fields in payload"
      (spy-on 'elog-info)
      (spy-on 'elog-debug)
      (let ((result (org-canvas--test-static-build-payload
                     '(:title "Ann" :published t))))
        (expect (alist-get 'is_announcement result) :to-equal t)
        (expect (alist-get 'discussion_type result) :to-equal "side_comment"))))

  (describe "post-build-fn"
    (before-each
      (eval
       '(org-canvas-define-payload test-post
          :registry-key "test-payload"
          :format alist
          :title-key :title
          :title-api-key title
          :post-build-fn (lambda (data payload)
                           (push `(custom . ,(plist-get data :note)) payload)))
       t))

    (it "calls post-build-fn with data and payload"
      (spy-on 'elog-info)
      (spy-on 'elog-debug)
      (let ((result (org-canvas--test-post-build-payload
                     '(:title "Item" :note "hello"))))
        (expect (alist-get 'custom result) :to-equal "hello")))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `eldev test "org-canvas-define-payload macro"`
Expected: FAIL — macro not defined

- [ ] **Step 3: Implement the payload macro in core-sync.el**

Add after the `org-canvas-define-properties` macro:

```elisp
;;;; 5c. Declarative Payload Builder Macro
;;
;; Generates build-payload functions from registry property specs.

(defmacro org-canvas-define-payload (feature &rest args)
  "Generate a build-payload function for FEATURE from property registry.

FEATURE is an unquoted symbol. ARGS is a plist:
  :registry-key - String key in `org-canvas--property-registry'
  :format - `alist' or `hash-table'
  :wrapper-key - String key to wrap hash-table payload (hash-table only)
  :title-key - Plist key for title in data (default :title)
  :title-api-key - API key for title (symbol for alist, string for hash)
  :body-key - Plist key for body in data (optional)
  :body-api-key - API key for body (optional)
  :static-fields - Alist of fixed key-value pairs (alist format only)
  :post-build-fn - (lambda (data payload) ...) called after building
  :extra-required-fn - (lambda (data inner) ...) for extra required fields"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas--%s-build-payload" feature-name)))
         (fmt (or (plist-get args :format) 'alist))
         (registry-key (plist-get args :registry-key))
         (wrapper-key (plist-get args :wrapper-key))
         (title-key (or (plist-get args :title-key) :title))
         (title-api-key (plist-get args :title-api-key))
         (body-key (plist-get args :body-key))
         (body-api-key (plist-get args :body-api-key))
         (static-fields (plist-get args :static-fields))
         (post-build-fn (plist-get args :post-build-fn))
         (extra-required-fn (plist-get args :extra-required-fn)))
    (unless registry-key
      (error "org-canvas-define-payload %s: :registry-key is required" feature-name))
    (pcase fmt
      ('alist
       `(defun ,fn-name (data)
          ,(format "Convert DATA to Canvas API payload for %s.\nGenerated by `org-canvas-define-payload'." feature-name)
          (let ((title (plist-get data ,title-key)))
            (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%%s'" title)
            (let ((payload (org-canvas--payload-build-alist
                            data ,registry-key ',title-api-key
                            ,@(when body-key (list body-key))
                            ,@(when body-api-key (list `',body-api-key))
                            ,@(when static-fields (list `',static-fields)))))
              ,@(when post-build-fn
                  `((setq payload (funcall ,post-build-fn data payload))))
              (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
              payload))))
      ('hash-table
       `(defun ,fn-name (data)
          ,(format "Convert DATA to Canvas API payload for %s.\nGenerated by `org-canvas-define-payload'." feature-name)
          (let ((title (plist-get data ,title-key)))
            (elog-info org-canvas--logger "[Stage 2: Transform] Building payload for '%%s'" title)
            (let ((payload (org-canvas--payload-build-hash-table
                            data ,registry-key ,title-api-key
                            ,@(when body-key (list body-key))
                            ,@(when body-api-key (list body-api-key))
                            ,@(when wrapper-key (list wrapper-key))
                            ,@(when extra-required-fn (list extra-required-fn)))))
              ,@(when post-build-fn
                  `((setq payload (funcall ,post-build-fn data payload))))
              (elog-debug org-canvas--logger "[Stage 2: Transform] Payload complete")
              payload))))
      (_ (error "org-canvas-define-payload %s: :format must be alist or hash-table" feature-name)))))
```

- [ ] **Step 4: Implement the runtime payload builders**

Add in `core-sync.el` before the macro:

```elisp
(defun org-canvas--payload-build-alist (data registry-key title-api-key
                                        &optional body-key body-api-key static-fields)
  "Build an alist payload for DATA using properties from REGISTRY-KEY.
TITLE-API-KEY is the alist key for the title.
BODY-KEY/BODY-API-KEY are for optional body content.
STATIC-FIELDS are fixed key-value pairs to include."
  (let* ((spec (gethash registry-key org-canvas--property-registry))
         (props (plist-get spec :properties))
         (title-key (or (plist-get spec :title-key) :title))
         (payload (list (cons title-api-key (plist-get data title-key)))))
    ;; Add body if specified
    (when (and body-key body-api-key)
      (let ((body-val (plist-get data body-key)))
        (when body-val
          (push (cons body-api-key body-val) payload))))
    ;; Add static fields
    (dolist (field static-fields)
      (push field payload))
    ;; Add properties that have :api-key
    (dolist (prop props)
      (let ((api-key (plist-get prop :api-key))
            (data-key (plist-get prop :data-key))
            (boolean-json (plist-get prop :boolean-json))
            (required (plist-get prop :required)))
        (when api-key
          (let ((val (plist-get data data-key)))
            (cond
             ((and required val)
              (push (cons (intern api-key) (if boolean-json (org-canvas--to-json-boolean val) val))
                    payload))
             ((and (not required) val)
              (push (cons (intern api-key) (if boolean-json (org-canvas--to-json-boolean val) val))
                    payload))
             (required
              ;; required but nil — still include (some APIs need explicit false)
              (push (cons (intern api-key)
                          (if boolean-json :json-false val))
                    payload)))))))
    (nreverse payload)))

(defun org-canvas--payload-build-hash-table (data registry-key title-api-key
                                             &optional body-key body-api-key
                                             wrapper-key extra-required-fn)
  "Build a hash-table payload for DATA using properties from REGISTRY-KEY.
TITLE-API-KEY is the hash key for the title (string).
BODY-KEY/BODY-API-KEY are for optional body content.
WRAPPER-KEY wraps the inner hash in an outer hash.
EXTRA-REQUIRED-FN is called with (data inner-hash) for extra fields."
  (let* ((spec (gethash registry-key org-canvas--property-registry))
         (props (plist-get spec :properties))
         (title-key (or (plist-get spec :title-key) :title))
         (inner (make-hash-table :test 'equal)))
    ;; Title
    (puthash title-api-key (plist-get data title-key) inner)
    ;; Body
    (when (and body-key body-api-key)
      (let ((body-val (plist-get data body-key)))
        (when body-val (puthash body-api-key body-val inner))))
    ;; Extra required fields
    (when extra-required-fn
      (funcall extra-required-fn data inner))
    ;; Properties with :api-key
    (dolist (prop props)
      (let ((api-key (plist-get prop :api-key))
            (data-key (plist-get prop :data-key))
            (boolean-json (plist-get prop :boolean-json)))
        (when api-key
          (let ((val (plist-get data data-key)))
            (when val
              (puthash api-key
                       (if boolean-json (org-canvas--to-json-boolean val) val)
                       inner))))))
    ;; Wrap if needed
    (if wrapper-key
        (let ((outer (make-hash-table :test 'equal)))
          (puthash wrapper-key inner outer)
          outer)
      inner)))
```

- [ ] **Step 5: Run the payload macro tests**

Run: `eldev test "org-canvas-define-payload macro"`
Expected: PASS

- [ ] **Step 6: Run full test suite**

Run: `eldev test`
Expected: All tests pass

- [ ] **Step 7: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-core-sync.el test/org-canvas-property-registry-test.el
git commit -m "feat: add org-canvas-define-payload macro

Generates build-payload functions from property registry specs.
Supports alist and hash-table output formats, wrapper keys, static
fields, and post-build hooks for custom logic.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 8: Migrate Simple Modules to define-payload

Replace manual build-payload functions with `org-canvas-define-payload` for modules where it's a clean fit.

**Files:**
- Modify: `lisp/org-canvas-announcements.el`
- Modify: `lisp/org-canvas-pages.el`
- Modify: `lisp/org-canvas-group-categories.el`
- Modify: `lisp/org-canvas-calendar.el`

### Announcements

- [ ] **Step 1: Replace the manual build-payload in announcements.el**

Replace the `org-canvas--announcement-build-payload` function (lines 62-87) with:

```elisp
(defun org-canvas--announcement-post-build (data payload)
  "Apply announcement-specific payload transformations.
DATA is the parsed plist, PAYLOAD is the alist so far."
  ;; Lock comments if ALLOW_COMMENTS is false
  (unless (plist-get data :allow_discussion_comments)
    (push `(lock_at . ,(org-canvas-current-iso8601-timestamp)) payload))
  ;; Resolve section names to IDs
  (when (plist-get data :specific_sections)
    (let ((resolved (org-canvas--resolve-section-names-to-ids
                     (plist-get data :specific_sections))))
      (when resolved
        (push `(specific_sections . ,resolved) payload))))
  payload)

(org-canvas-define-payload announcement
  :registry-key "announcements"
  :format alist
  :title-key :title
  :title-api-key title
  :body-key :message
  :body-api-key message
  :static-fields ((is_announcement . t)
                  (discussion_type . "side_comment"))
  :post-build-fn #'org-canvas--announcement-post-build)
```

- [ ] **Step 2: Run announcement tests**

Run: `eldev test "announcement"`
Expected: All announcement tests pass

### Pages

- [ ] **Step 3: Replace the manual build-payload in pages.el**

Replace the `org-canvas--page-build-payload` function (lines 89-117) with:

```elisp
(org-canvas-define-payload page
  :registry-key "pages"
  :format hash-table
  :wrapper-key "wiki_page"
  :title-key :title
  :title-api-key "title"
  :body-key :body
  :body-api-key "body")
```

- [ ] **Step 4: Run page tests**

Run: `eldev test "page"`
Expected: All page tests pass

### Group Categories

- [ ] **Step 5: Replace the manual build-payload in group-categories.el**

Replace the `org-canvas--group-category-build-payload` function (lines 58-71) with:

```elisp
(org-canvas-define-payload group-category
  :registry-key "group-categories"
  :format alist
  :title-key :title
  :title-api-key name)
```

- [ ] **Step 6: Run group-category tests**

Run: `eldev test "group-categor"`
Expected: All group-category tests pass

### Calendar Events

- [ ] **Step 7: Replace the manual build-payload in calendar.el**

Replace the `org-canvas--calendar-event-build-payload` function (lines 66-89) with:

```elisp
(defun org-canvas--calendar-event-extra-required (data inner)
  "Add calendar-event-specific required fields to INNER hash.
DATA is the parsed plist."
  (puthash "context_code" (format "course_%s" org-canvas-course-id) inner)
  (puthash "start_at" (plist-get data :start_at) inner))

(org-canvas-define-payload calendar-event
  :registry-key "calendar-events"
  :format hash-table
  :wrapper-key "calendar_event"
  :title-key :title
  :title-api-key "title"
  :body-key :description
  :body-api-key "description"
  :extra-required-fn #'org-canvas--calendar-event-extra-required)
```

- [ ] **Step 8: Run calendar tests**

Run: `eldev test "calendar"`
Expected: All calendar tests pass

- [ ] **Step 9: Run full test suite**

Run: `eldev test`
Expected: All tests pass

- [ ] **Step 10: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean

- [ ] **Step 11: Commit**

```bash
git add lisp/org-canvas-announcements.el lisp/org-canvas-pages.el lisp/org-canvas-group-categories.el lisp/org-canvas-calendar.el
git commit -m "refactor: migrate 4 modules to org-canvas-define-payload

Replace manual build-payload functions in announcements, pages,
group-categories, and calendar with declarative macro. Custom logic
preserved via :post-build-fn and :extra-required-fn hooks.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 9: Connect define-parse to Registry (Optional Enhancement)

Make `org-canvas-define-parse` read from the registry when properties are already registered, eliminating the last duplication between the parse spec and the properties declaration.

**Files:**
- Modify: `lisp/org-canvas-core-sync.el`
- Modify: `lisp/org-canvas-announcements.el` (convert as proof)
- Modify: `test/org-canvas-property-registry-test.el`

- [ ] **Step 1: Write test for registry-backed parse**

Add to `test/org-canvas-property-registry-test.el`:

```elisp
(describe "org-canvas-define-parse with :from-registry"
  (before-each
    (setq org-canvas--property-registry (make-hash-table :test 'equal))
    (org-canvas-register-properties "test-rp"
      :label "Test"
      :file-var 'org-canvas-test-file
      :query "LEVEL=1"
      :body-key :message
      :entity-name "Test item"
      :properties
      '((:org-prop "PUBLISHED" :data-key :published :type boolean :default t)
        (:org-prop "DUE_AT"    :data-key :due_at    :type timestamp)))
    (eval
     '(org-canvas-define-parse test-rp
        :from-registry "test-rp")
     t))

  (it "generates parse-entry function"
    (expect (fboundp 'org-canvas--test-rp-parse-entry) :to-be-truthy))

  (it "generates read-props function"
    (expect (fboundp 'org-canvas--test-rp-read-props) :to-be-truthy))

  (it "generates transform-props function"
    (expect (fboundp 'org-canvas--test-rp-transform-props) :to-be-truthy))

  (it "parses properties correctly"
    (with-temp-org-buffer
     "* Test Heading
:PROPERTIES:
:PUBLISHED: true
:DUE_AT: <2026-06-01 Mon 09:00>
:END:
Some body text
"
     (org-back-to-heading)
     (let ((data (org-canvas--test-rp-parse-entry)))
       (expect (plist-get data :title) :to-equal "Test Heading")
       (expect (plist-get data :published) :to-equal t)
       (expect (plist-get data :due_at) :to-match "2026-06-01")
       (expect (plist-get data :message) :not :to-be nil)))))
```

- [ ] **Step 2: Run to verify failure**

Run: `eldev test "define-parse with :from-registry"`
Expected: FAIL

- [ ] **Step 3: Extend define-parse to support :from-registry**

In `org-canvas-define-parse` macro in `core-sync.el`, add at the start of the `let*` body (after line 100):

```elisp
;; When :from-registry is provided, pull specs from the registry
(let* ((from-registry (plist-get args :from-registry)))
  (when from-registry
    (let ((reg-spec (gethash from-registry org-canvas--property-registry)))
      (unless reg-spec
        (error "org-canvas-define-parse %s: registry key '%s' not found"
               feature from-registry))
      ;; Convert registry properties to define-parse format
      (unless (plist-get args :properties)
        (setq args (plist-put args :properties
                              (mapcar (lambda (p)
                                        (let ((spec (list (plist-get p :org-prop)
                                                          (plist-get p :data-key))))
                                          (when (plist-get p :type)
                                            (setq spec (append spec (list :type (plist-get p :type)))))
                                          (when (plist-get p :default)
                                            (setq spec (append spec (list :default (plist-get p :default)))))
                                          (when (plist-get p :values)
                                            (setq spec (append spec (list :values (plist-get p :values)))))
                                          spec))
                                      (plist-get reg-spec :properties)))))
      ;; Pull other settings from registry if not explicitly provided
      (unless (plist-get args :body)
        (let ((bk (plist-get reg-spec :body-key)))
          (when bk (setq args (plist-put args :body bk)))))
      (unless (plist-get args :entity-name)
        (let ((en (plist-get reg-spec :entity-name)))
          (when en (setq args (plist-put args :entity-name en)))))
      (unless (plist-get args :id-key)
        (let ((ik (plist-get reg-spec :id-key)))
          (when ik (setq args (plist-put args :id-key ik)))))
      (unless (plist-get args :id-property)
        (let ((ip (plist-get reg-spec :id-property)))
          (when ip (setq args (plist-put args :id-property ip)))))
      (unless (plist-get args :title-key)
        (let ((tk (plist-get reg-spec :title-key)))
          (when tk (setq args (plist-put args :title-key tk)))))
      (unless (plist-get args :after-read)
        (let ((ar (plist-get reg-spec :after-read)))
          (when ar (setq args (plist-put args :after-read ar)))))
      (unless (plist-get args :after-transform)
        (let ((at (plist-get reg-spec :after-transform)))
          (when at (setq args (plist-put args :after-transform at))))))))
```

Note: This approach reads the registry at macro-expansion time (compile time), which is fine because modules register properties at load time before `define-parse` is evaluated.

- [ ] **Step 4: Run the registry-backed parse tests**

Run: `eldev test "define-parse with :from-registry"`
Expected: PASS

- [ ] **Step 5: Convert announcements to use :from-registry**

Replace in announcements.el:

```elisp
;; Before:
(org-canvas-define-parse announcement
  :body :message
  :properties
  (("PUBLISHED"        :published                 :type boolean :default t)
   ("POST_AT"          :delayed_post_at           :type timestamp)
   ("ALLOW_COMMENTS"   :allow_discussion_comments :type boolean)
   ("SPECIFIC_SECTIONS" :specific_sections        :type string)))

;; After:
(org-canvas-define-parse announcement
  :from-registry "announcements"
  :properties
  (("SPECIFIC_SECTIONS" :specific_sections :type string)))  ;; not in registry (parse-only)
```

Note: SPECIFIC_SECTIONS is not in the validation registry (it's not validated), so it's passed as an explicit override. When both `:from-registry` and `:properties` are provided, the explicit properties are appended to the registry ones. POST_AT in the registry is named DELAYED_POST_AT — this is the Org property name, which should match. Actually, looking at the current code, the parse spec uses "POST_AT" as the Org property while the validation spec uses "DELAYED_POST_AT". These are different property names — POST_AT is what's in the Org file, DELAYED_POST_AT is what Canvas calls it. The registry entry should use "POST_AT" (the Org property name) with `:data-key :delayed_post_at`. Let me verify this is correct in the registry entry from Task 2.

Wait — in Task 2, I wrote `"DELAYED_POST_AT"` as the `:org-prop`, but in the Org file it's actually `POST_AT`. Looking at the current announcements parse spec: `("POST_AT" :delayed_post_at :type timestamp)` — yes, the Org property is "POST_AT". But in the validation spec it's `(:name "DELAYED_POST_AT" :type timestamp)`. This is a discrepancy in the current code — validation checks DELAYED_POST_AT but the actual Org property is POST_AT. The registry should use the correct Org property name "POST_AT". Update Task 2's announcements registration to use "POST_AT" instead.

- [ ] **Step 6: Run announcement tests**

Run: `eldev test "announcement"`
Expected: All pass

- [ ] **Step 7: Run full test suite**

Run: `eldev test`
Expected: All pass

- [ ] **Step 8: Commit**

```bash
git add lisp/org-canvas-core-sync.el lisp/org-canvas-announcements.el test/org-canvas-property-registry-test.el
git commit -m "feat: extend define-parse to read from property registry

Add :from-registry option to org-canvas-define-parse that pulls property
specs, body-key, entity-name, and ID settings from the centralized
registry. Convert announcements as proof of concept.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Task 10: Final Cleanup and CLAUDE.md Update

- [ ] **Step 1: Run full test suite one final time**

Run: `eldev test`
Expected: All tests pass

- [ ] **Step 2: Run lint and complexity**

Run: `eldev lint && eldev complexity`
Expected: Clean

- [ ] **Step 3: Update CLAUDE.md**

Add a new subsection under "### Shared Infrastructure" documenting:
- `org-canvas-define-properties` macro and its role
- `org-canvas-define-payload` macro and which modules use it
- The property registry and how validation consumes it
- Which modules use `:from-registry` in their parse specs

Update the "### Module Structure" section to mention the registry.

Update the test count if it changed.

- [ ] **Step 4: Run final checks**

Run: `eldev test && eldev lint && eldev complexity`
Expected: All clean

- [ ] **Step 5: Commit**

```bash
git add CLAUDE.md
git commit -m "docs: update CLAUDE.md with property registry architecture

Document org-canvas-define-properties, org-canvas-define-payload,
property registry, and migration status for all modules.

Co-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>"
```

---

## Important Notes

### Announcement POST_AT vs DELAYED_POST_AT Discrepancy

The current codebase has a mismatch:
- **Parse spec** (announcements.el): Org property `"POST_AT"` → data key `:delayed_post_at`
- **Validation spec** (validate.el): Checks `"DELAYED_POST_AT"`

This means validation currently checks a property named DELAYED_POST_AT that doesn't exist in the Org file (the Org file uses POST_AT). The registry entry should use `"POST_AT"` (matching the actual Org property) to fix this latent bug. Test this carefully — if existing Org files use DELAYED_POST_AT as the property name, we should match that instead.

### Modules That Stay Manual

These modules use the property registry for validation but keep manual parse and/or payload:
- **assignments** — manual parse (30+ properties, link resolution), manual payload (nested structure, peer reviews, optional field specs)
- **discussions** — manual parse (link resolution), manual payload (graded sub-payload)
- **quizzes** — manual everything (nested questions)
- **new-quizzes** — manual everything (nested items, different API)
- **modules** — manual everything (parent-child items)
- **outcomes** — manual everything (hierarchical)
- **rubrics** — manual everything (criteria tables)
- **files** — manual everything (3-step upload)
- **settings** — manual everything (tabs, late policy)
- **sections** — pull-only
- **assignment-groups** — manual payload (drop rules conditional on update vs create)

### Macro Expansion Order

`org-canvas-define-properties` must appear before `org-canvas-define-parse :from-registry` in each module file. Since both are top-level forms evaluated at load time, order within the file matters.

### Test Isolation

Tests that manipulate `org-canvas--property-registry` must reset it in `before-each` to avoid cross-test contamination. The real registry is populated once at load time and should not be modified during tests (use a fresh hash-table in test setups).
