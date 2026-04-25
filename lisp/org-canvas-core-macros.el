;;; org-canvas-core-macros.el --- Declarative payload and parse DSL -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Declarative code-generation macros used by feature modules:
;;
;;   `org-canvas-define-payload' - generate build-payload functions from
;;                                 property registry specs.
;;   `org-canvas-define-parse'   - generate read-props, transform-props,
;;                                 and parse-entry from a declarative spec.
;;
;; These macros are runtime-independent: once expanded, the generated
;; functions call helpers in org-canvas-core-org and org-canvas-core-config.

;;; Code:

(require 'org-canvas-core-config)
(require 'org-canvas-core-org)

;;;; Declarative Payload Builder
;;
;; Runtime helpers and macro for generating build-payload functions
;; from property registry specs.

(defun org-canvas--payload-build-alist (data registry-key title-api-key
                                        &optional body-key body-api-key
                                        static-fields)
  "Build an alist payload from DATA using property specs in REGISTRY-KEY.
TITLE-API-KEY is the alist key symbol for the title field.
BODY-KEY/BODY-API-KEY add body content when both non-nil.
STATIC-FIELDS is an alist of fixed key-value pairs."
  (let* ((spec (gethash registry-key org-canvas--property-registry))
         (properties (plist-get spec :properties))
         (title-data-key (or (plist-get spec :title-key) :title))
         (base (list (cons title-api-key (plist-get data title-data-key)))))
    ;; Add body if specified
    (when (and body-key body-api-key)
      (push (cons body-api-key (plist-get data body-key)) base))
    ;; Add static fields
    (dolist (field static-fields)
      (push field base))
    ;; Add properties from registry
    (dolist (prop properties)
      (let* ((api-key (plist-get prop :api-key))
             (data-key (plist-get prop :data-key))
             (boolean-json (plist-get prop :boolean-json))
             (required (plist-get prop :required))
             (value (plist-get data data-key)))
        (when (and api-key (or value required boolean-json))
          (let ((final-value (if boolean-json
                                 (org-canvas--to-json-boolean value)
                               value)))
            (push (cons (intern api-key) final-value) base)))))
    (nreverse base)))

(defun org-canvas--payload-build-hash-table (data registry-key title-api-key
                                             &optional body-key body-api-key
                                             wrapper-key extra-required-fn)
  "Build a hash-table payload from DATA using property specs in REGISTRY-KEY.
TITLE-API-KEY is the hash key string for the title field.
BODY-KEY/BODY-API-KEY add body content when both non-nil.
WRAPPER-KEY wraps inner hash in an outer hash.
EXTRA-REQUIRED-FN is (lambda (data inner-hash)) for extra required fields."
  (let* ((spec (gethash registry-key org-canvas--property-registry))
         (properties (plist-get spec :properties))
         (title-data-key (or (plist-get spec :title-key) :title))
         (inner (make-hash-table :test 'equal)))
    ;; Add title
    (puthash title-api-key (plist-get data title-data-key) inner)
    ;; Add body if specified
    (when (and body-key body-api-key)
      (puthash body-api-key (plist-get data body-key) inner))
    ;; Add properties from registry
    (dolist (prop properties)
      (let* ((api-key (plist-get prop :api-key))
             (data-key (plist-get prop :data-key))
             (boolean-json (plist-get prop :boolean-json))
             (required (plist-get prop :required))
             (value (plist-get data data-key)))
        (when (and api-key (or value required boolean-json))
          (puthash api-key
                   (if boolean-json
                       (org-canvas--to-json-boolean value)
                     value)
                   inner))))
    ;; Call extra-required-fn if provided
    (when extra-required-fn
      (funcall extra-required-fn data inner))
    ;; Wrap if wrapper-key provided
    (if wrapper-key
        (let ((outer (make-hash-table :test 'equal)))
          (puthash wrapper-key inner outer)
          outer)
      inner)))

(defmacro org-canvas-define-payload (feature &rest args)
  "Generate `org-canvas--FEATURE-build-payload' from property registry.
FEATURE is an unquoted symbol.  ARGS is a plist:
  :registry-key STRING - key in `org-canvas--property-registry'
  :format (alist | hash-table) - output format
  :title-api-key - API key for title (symbol for alist, string for hash-table)
  :body-key KEYWORD - plist key for body in data (optional)
  :body-api-key - API key for body (optional)
  :wrapper-key STRING - wrap hash-table in outer hash (hash-table only)
  :static-fields ALIST - fixed fields (alist only)
  :post-build-fn FUNCTION - (lambda (data payload) payload) escape hatch
  :extra-required-fn FUNCTION - (lambda (data inner-hash)) for hash-table"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (fn-name (intern (format "org-canvas--%s-build-payload" feature-name)))
         (format-type (plist-get args :format))
         (registry-key (plist-get args :registry-key))
         (title-api-key (plist-get args :title-api-key))
         (body-key (plist-get args :body-key))
         (body-api-key (plist-get args :body-api-key))
         (wrapper-key (plist-get args :wrapper-key))
         (static-fields (plist-get args :static-fields))
         (post-build-fn (plist-get args :post-build-fn))
         (extra-required-fn (plist-get args :extra-required-fn)))
    (unless (memq format-type '(alist hash-table))
      (error "Org-canvas-define-payload %s: :format must be alist or hash-table"
             feature-name))
    (unless registry-key
      (error "Org-canvas-define-payload %s: :registry-key is required"
             feature-name))
    (unless title-api-key
      (error "Org-canvas-define-payload %s: :title-api-key is required"
             feature-name))
    (let ((builder-call
           (if (eq format-type 'alist)
               `(org-canvas--payload-build-alist
                 data ,registry-key ',title-api-key
                 ,body-key ',body-api-key
                 ',static-fields)
             `(org-canvas--payload-build-hash-table
               data ,registry-key ,title-api-key
               ,body-key ,body-api-key
               ,wrapper-key ,extra-required-fn))))
      `(defun ,fn-name (data)
         ,(format "Convert extracted DATA plist into a Canvas API payload for %s." feature-name)
         (let ((title (plist-get data ,(or (plist-get args :title-key) :title))))
           (org-canvas--log-info org-canvas--logger
                      "[Stage 2: Transform] Building payload for '%%s'" title)
           (let ((payload ,builder-call))
             ,@(when post-build-fn
                 `((setq payload (funcall ,post-build-fn data payload))))
             (org-canvas--log-info org-canvas--logger
                        "[Stage 2: Transform] Payload complete")
             payload))))))

;;;; Declarative Parse Macro
;;
;; Generates read-props, transform-props, and parse-entry from a
;; declarative property spec.

(defun org-canvas--parse-gen-raw-key (prop-name)
  "Generate a raw plist key from Org PROP-NAME string.
E.g., \"PUBLISHED\" → :published-raw, \"POST_AT\" → :post-at-raw."
  (intern (format ":%s-raw" (downcase (replace-regexp-in-string "_" "-" prop-name)))))

(defun org-canvas--parse-gen-transform-form (prop-name _data-key type default values)
  "Generate the transform expression for a single property.
PROP-NAME is the Org property name.  DATA-KEY is the output plist key.
TYPE is one of: string, boolean, timestamp, number, enum.
DEFAULT is the default value for booleans.  VALUES is the enum list."
  (let ((raw-key (org-canvas--parse-gen-raw-key prop-name)))
    (pcase type
      ('boolean
       (if default
           `(org-canvas--interpret-boolean (plist-get raw ,raw-key) ,default)
         `(org-canvas--interpret-boolean (plist-get raw ,raw-key))))
      ('timestamp
       `(org-canvas-org-parse-timestamp (plist-get raw ,raw-key)))
      ('number
       `(let ((v (plist-get raw ,raw-key)))
          (when v (org-canvas--safe-string-to-number v ,prop-name))))
      ('enum
       `(org-canvas--validate-property
         (plist-get raw ,raw-key) ,values ,prop-name
         ,(when default default)))
      (_ ; string
       `(plist-get raw ,raw-key)))))

(defmacro org-canvas-define-parse (feature &rest args)
  "Define read-props, transform-props, and parse-entry for FEATURE.

FEATURE is an unquoted symbol like \\='announcement.

ARGS is a plist with the following keys:
  :properties - List of (ORG-PROP DATA-KEY &rest OPTS) specs.
    Each spec defines one Org property to read and transform.
    OPTS plist: :type (string|boolean|timestamp|number|enum),
    :default (for booleans/enums), :values (for enums).
  :body - Output plist key for exported HTML body (nil = none).
  :title-key - Output plist key for the title (default :title).
  :id-key - Output plist key for canvas ID (default :canvas-id).
  :id-property - Org property for canvas ID (default CANVAS_ID).
  :entity-name - Display name for errors (default: FEATURE).
  :after-read - Post-read hook: (lambda (raw pom) raw).
  :after-transform - Post-transform hook: (lambda (data) data).

Example:
  (org-canvas-define-parse announcement
    :body :message
    :properties
    ((\"PUBLISHED\"      :published      :type boolean :default t)
     (\"POST_AT\"        :delayed_post_at :type timestamp)
     (\"ALLOW_COMMENTS\" :allow_discussion_comments
                        :type boolean)
     (\"SPECIFIC_SECTIONS\" :specific_sections
                           :type string)))"
  (declare (indent 1))
  (let* ((feature-name (symbol-name feature))
         (read-fn (intern (format "org-canvas--%s-read-props" feature-name)))
         (transform-fn (intern (format "org-canvas--%s-transform-props" feature-name)))
         (parse-fn (intern (format "org-canvas--%s-parse-entry" feature-name)))
         (props (plist-get args :properties))
         (body-key (plist-get args :body))
         (title-key (or (plist-get args :title-key) :title))
         (id-key (or (plist-get args :id-key) :canvas-id))
         (id-property (or (plist-get args :id-property) "CANVAS_ID"))
         (entity-name (or (plist-get args :entity-name)
                          (capitalize feature-name)))
         (after-read (plist-get args :after-read))
         (after-transform (plist-get args :after-transform))
         ;; Generate the raw-key for id-property
         (id-raw-key (if (string= id-property "CANVAS_ID")
                         :canvas-id
                       (intern (format ":%s" (downcase
                                              (replace-regexp-in-string
                                               "_" "-" id-property))))))
         ;; Build read-props body: list of :key-raw (org-entry-get pom "PROP")
         (read-forms
          (apply #'append
                 (mapcar (lambda (spec)
                           (let ((prop-name (nth 0 spec))
                                 (raw-key (org-canvas--parse-gen-raw-key (nth 0 spec))))
                             (list raw-key `(org-entry-get pom ,prop-name))))
                         props)))
         ;; Build transform-props body: list of :data-key (transform-expr)
         (transform-forms
          (apply #'append
                 (mapcar (lambda (spec)
                           (let* ((prop-name (nth 0 spec))
                                  (data-key (nth 1 spec))
                                  (opts (cddr spec))
                                  (type (or (plist-get opts :type) 'string))
                                  (default (plist-get opts :default))
                                  (values (plist-get opts :values)))
                             (list data-key
                                   (org-canvas--parse-gen-transform-form
                                    prop-name data-key type default values))))
                         props))))
    `(progn
       (defun ,read-fn (pom)
         ,(format "Read raw property strings from the %s heading at POM." feature-name)
         ,(let ((base-form `(list :title-raw (org-get-heading t t t t)
                                  ,id-raw-key (org-entry-get pom ,id-property)
                                  ,@read-forms)))
            (if after-read
                `(let ((raw ,base-form))
                   (funcall ,after-read raw pom))
              base-form)))

       (defun ,transform-fn (raw)
         ,(format "Transform raw property strings RAW into typed %s data.\nPure function — no buffer access." feature-name)
         ,(let ((base-form `(list ,title-key (org-canvas--strip-statistics-cookie
                                              (plist-get raw :title-raw))
                                  ,id-key (plist-get raw ,id-raw-key)
                                  ,@transform-forms)))
            (if after-transform
                `(let ((data ,base-form))
                   (funcall ,after-transform data))
              base-form)))

       (defun ,parse-fn ()
         ,(format "Extract %s data from the Org heading at point." feature-name)
         (org-back-to-heading t)
         (org-canvas--log-debug org-canvas--logger
                     "[Stage 1: Parse] Starting extraction at point %%d" (point))
         (let* ((pom (point))
                (raw (,read-fn pom))
                (data (,transform-fn raw)))
           (org-canvas--require-title (plist-get data ,title-key) pom ,entity-name)
           (org-canvas--log-info org-canvas--logger
                      "[Stage 1: Parse] Processing %s: '%%s' (ID: %%s)"
                      ,entity-name
                      (plist-get data ,title-key)
                      (or (plist-get data ,id-key) "NEW"))
           ,@(when body-key
               `((org-canvas--log-debug org-canvas--logger
                             "[Stage 1: Export] Exporting subtree to HTML...")
                 (let ((content (org-canvas--export-subtree-body-to-html)))
                   (org-canvas--log-info org-canvas--logger
                              "[Stage 1: Parse] Body size: %%d chars"
                              (length content))
                   (plist-put data ,body-key content))))
           (plist-put data :pom pom)
           data)))))

(provide 'org-canvas-core-macros)
;;; org-canvas-core-macros.el ends here
