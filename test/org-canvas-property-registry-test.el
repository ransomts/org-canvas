;;; org-canvas-property-registry-test.el --- Tests for property registry  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the centralized property registry in org-canvas-core-config.el.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'org-canvas-core)

(describe "Property Registry"
  :var (saved-registry)
  (before-each
    (setq saved-registry org-canvas--property-registry)
    (setq org-canvas--property-registry (make-hash-table :test 'equal)))
  (after-each
    (setq org-canvas--property-registry saved-registry))

  (describe "org-canvas-register-properties"
    (it "stores a spec in the registry"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)
          (:org-prop "FRONT_PAGE" :data-key :front_page :type boolean)))
      (expect (gethash "pages" org-canvas--property-registry) :not :to-be nil))

    (it "does not overwrite an existing entry"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)))
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "EDITED" :data-key :edited :type boolean)))
      ;; Should still have the original property, not the overwritten one
      (let* ((spec (gethash "pages" org-canvas--property-registry))
             (props (plist-get spec :properties))
             (first-prop (car props)))
        (expect (plist-get first-prop :org-prop) :to-equal "PUBLISHED")))

    (it "stores multiple features independently"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)))
      (org-canvas-register-properties "assignments"
        :file-var 'org-canvas-assignments-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "POINTS" :data-key :points :type number)))
      (expect (hash-table-count org-canvas--property-registry) :to-equal 2)
      (expect (gethash "pages" org-canvas--property-registry) :not :to-be nil)
      (expect (gethash "assignments" org-canvas--property-registry) :not :to-be nil)))

  (describe "org-canvas--property-to-validate-prop"
    (it "converts a basic property spec"
      (let ((result (org-canvas--property-to-validate-prop
                     '(:org-prop "PUBLISHED" :data-key :published :type boolean))))
        (expect (plist-get result :name) :to-equal "PUBLISHED")
        (expect (plist-get result :type) :to-equal 'boolean)))

    (it "converts an enum property with :values"
      (let ((result (org-canvas--property-to-validate-prop
                     '(:org-prop "GRADING_TYPE" :data-key :grading_type
                       :type enum :values ("points" "percent")))))
        (expect (plist-get result :name) :to-equal "GRADING_TYPE")
        (expect (plist-get result :type) :to-equal 'enum)
        (expect (plist-get result :values) :to-equal '("points" "percent"))))

    (it "converts a link property correctly"
      (let ((result (org-canvas--property-to-validate-prop
                     '(:org-prop "GROUP" :data-key :group
                       :type link
                       :target-file org-canvas-assignment-groups-file
                       :link-id-property "CANVAS_ID"))))
        (expect (plist-get result :name) :to-equal "GROUP")
        (expect (plist-get result :type) :to-equal 'link)
        (expect (plist-get result :target-file) :to-equal 'org-canvas-assignment-groups-file)
        (expect (plist-get result :id-property) :to-equal "CANVAS_ID"))))

  (describe "org-canvas--get-validate-specs-from-registry"
    (it "returns correct structure from registry"
      (org-canvas-register-properties "pages"
        :file-var 'org-canvas-pages-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "PUBLISHED" :data-key :published :type boolean)))
      (let* ((specs (org-canvas--get-validate-specs-from-registry))
             (spec (car specs)))
        (expect (length specs) :to-equal 1)
        (expect (plist-get spec :label) :to-equal "pages")
        (expect (plist-get spec :file) :to-equal 'org-canvas-pages-file)
        (expect (plist-get spec :query) :to-equal "LEVEL=1")
        (let* ((props (plist-get spec :properties))
               (prop (car props)))
          (expect (plist-get prop :name) :to-equal "PUBLISHED")
          (expect (plist-get prop :type) :to-equal 'boolean))))

    (it "includes :date-order in output"
      (org-canvas-register-properties "assignments"
        :file-var 'org-canvas-assignments-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "POINTS" :data-key :points :type number))
        :date-order '(("UNLOCK_AT" "DUE_AT" "LOCK_AT")))
      (let* ((specs (org-canvas--get-validate-specs-from-registry))
             (spec (car specs)))
        (expect (plist-get spec :date-order)
                :to-equal '(("UNLOCK_AT" "DUE_AT" "LOCK_AT")))))

    (it "includes :structural-fn in output"
      (org-canvas-register-properties "assignments"
        :file-var 'org-canvas-assignments-file
        :query "LEVEL=1"
        :properties
        '((:org-prop "POINTS" :data-key :points :type number))
        :structural-fn #'org-canvas--validate-assignment-structure)
      (let* ((specs (org-canvas--get-validate-specs-from-registry))
             (spec (car specs)))
        (expect (plist-get spec :structural-fn)
                :to-equal #'org-canvas--validate-assignment-structure)))))

(provide 'org-canvas-property-registry-test)
;;; org-canvas-property-registry-test.el ends here
