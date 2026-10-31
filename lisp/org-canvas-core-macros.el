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
(require 'org-canvas-core-html)

;;;; Declarative Payload Builder
;;
;; Runtime helpers and macro for generating build-payload functions
;; from property registry specs.

(cl-defstruct (org-canvas--payload-options (:copier nil)
               (:constructor org-canvas--payload-options))
  "What a generated build-payload function was told to build.
REGISTRY-KEY names the property registry entry the payload's
properties come from.  TITLE-API-KEY is the payload's key for the
title -- a symbol in an alist, a string in a hash table.  BODY-KEY and
BODY-API-KEY add a body when both are non-nil.  STATIC-FIELDS are
fixed key-value pairs an alist payload always carries, WRAPPER-KEY the
hash a hash-table payload is wrapped in, and EXTRA-REQUIRED-FN a
function that adds the fields only the module can compute.  Every slot
is a constant of the generated function, so none of them varies with
the entry being built."
  registry-key title-api-key body-key body-api-key
  static-fields wrapper-key extra-required-fn)

(defun org-canvas--payload-registry (options)
  "Return the property registry entry OPTIONS' payload is built from."
  (gethash (org-canvas--payload-options-registry-key options)
           org-canvas--property-registry))

(defun org-canvas--payload-title (data options)
  "Return DATA's title as (API-KEY . TITLE), the way OPTIONS names it.
A registry entry that syncs some other property as the title names it
in its `:title-key'."
  (let ((spec (org-canvas--payload-registry options)))
    (cons (org-canvas--payload-options-title-api-key options)
          (plist-get data (or (plist-get spec :title-key) :title)))))

(defun org-canvas--payload-property-pairs (options data)
  "Return (API-KEY . VALUE) for each property OPTIONS takes from DATA.
API-KEY is the registry's string, which a hash-table payload takes as
it stands and an alist payload interns.  A property DATA carries no
value for is left out, unless the registry marks it `:required' or
`:boolean-json': Canvas reads a missing boolean as false, so a payload
that means yes has to say it."
  (let (pairs)
    (dolist (prop (plist-get (org-canvas--payload-registry options) :properties))
      (let ((api-key (plist-get prop :api-key))
            (value (plist-get data (plist-get prop :data-key))))
        (when (and api-key (or value (plist-get prop :required)
                               (plist-get prop :boolean-json)))
          (push (cons api-key
                      (if (plist-get prop :boolean-json)
                          (org-canvas--to-json-boolean value)
                        value))
                pairs))))
    (nreverse pairs)))

(defun org-canvas--payload-build-alist (data options)
  "Build an alist payload from DATA and OPTIONS.
OPTIONS is an `org-canvas--payload-options'; its `static-fields' are
added as given, and the property pairs last."
  (let ((base (list (org-canvas--payload-title data options)))
        (body-key (org-canvas--payload-options-body-key options))
        (body-api-key (org-canvas--payload-options-body-api-key options)))
    ;; Add body if specified
    (when (and body-key body-api-key)
      (push (cons body-api-key (plist-get data body-key)) base))
    ;; Add static fields
    (dolist (field (org-canvas--payload-options-static-fields options))
      (push field base))
    ;; Add properties from registry
    (dolist (pair (org-canvas--payload-property-pairs options data))
      (push (cons (intern (car pair)) (cdr pair)) base))
    (nreverse base)))

(defun org-canvas--payload-build-hash-table (data options)
  "Build a hash-table payload from DATA and OPTIONS.
OPTIONS is an `org-canvas--payload-options'.  Its `extra-required-fn',
when set, is called with DATA and the table before the table is
wrapped in a further one named by its `wrapper-key'."
  (let ((inner (make-hash-table :test 'equal))
        (title (org-canvas--payload-title data options))
        (body-key (org-canvas--payload-options-body-key options))
        (body-api-key (org-canvas--payload-options-body-api-key options))
        (extra-required-fn (org-canvas--payload-options-extra-required-fn options))
        (wrapper-key (org-canvas--payload-options-wrapper-key options)))
    ;; Add title
    (puthash (car title) (cdr title) inner)
    ;; Add body if specified
    (when (and body-key body-api-key)
      (puthash body-api-key (plist-get data body-key) inner))
    ;; Add properties from registry
    (dolist (pair (org-canvas--payload-property-pairs options data))
      (puthash (car pair) (cdr pair) inner))
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
    (let* ((alist-p (eq format-type 'alist))
           ;; An alist payload's keys are symbols, a hash table's strings;
           ;; the module names each the way its format reads it.
           (options
            `(org-canvas--payload-options
              :registry-key ,registry-key
              :title-api-key ,(if alist-p `',title-api-key title-api-key)
              :body-key ,body-key
              :body-api-key ,(if alist-p `',body-api-key body-api-key)
              :static-fields ,(and static-fields (list 'quote static-fields))
              :wrapper-key ,wrapper-key
              :extra-required-fn ,extra-required-fn))
           (builder-call
            (if alist-p
                `(org-canvas--payload-build-alist data ,options)
              `(org-canvas--payload-build-hash-table data ,options))))
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
E.g., \"PUBLISHED\" → :published-raw, \"DELAYED_POST_AT\" → :delayed-post-at-raw."
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

(defun org-canvas--parse-gen-id-raw-key (id-property)
  "Return the raw plist key ID-PROPERTY is read into.
CANVAS_ID keeps its :canvas-id key; any other property name is
downcased with underscores turned into dashes."
  (if (string= id-property "CANVAS_ID")
      :canvas-id
    (intern (format ":%s" (downcase
                           (replace-regexp-in-string "_" "-" id-property))))))

(defun org-canvas--parse-gen-read-forms (props)
  "Return the plist forms reading each of PROPS into its raw key.
PROPS is the :properties list `org-canvas-define-parse' received: one
raw plist key followed by the `org-entry-get' that fills it, per spec."
  (apply #'append
         (mapcar (lambda (spec)
                   (let ((prop-name (nth 0 spec))
                         (raw-key (org-canvas--parse-gen-raw-key (nth 0 spec))))
                     (list raw-key `(org-entry-get pom ,prop-name))))
                 props)))

(defun org-canvas--parse-gen-transform-forms (props)
  "Return the plist forms transforming each of PROPS into typed data.
PROPS is the :properties list `org-canvas-define-parse' received; each
spec contributes its DATA-KEY and the expression
`org-canvas--parse-gen-transform-form' makes for its options."
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
                 props)))

(defun org-canvas--parse-gen-spec (feature args)
  "Return the generation spec `org-canvas-define-parse' works from.
FEATURE and ARGS are the macro's arguments; the spec is a plist of the
generated function names, the resolved options and the read and
transform forms, so the emitters below never re-read ARGS."
  (let* ((feature-name (symbol-name feature))
         (id-property (or (plist-get args :id-property) "CANVAS_ID")))
    (list :feature-name feature-name
          :read-fn (intern (format "org-canvas--%s-read-props" feature-name))
          :transform-fn (intern (format "org-canvas--%s-transform-props"
                                        feature-name))
          :parse-fn (intern (format "org-canvas--%s-parse-entry" feature-name))
          :read-forms (org-canvas--parse-gen-read-forms
                       (plist-get args :properties))
          :transform-forms (org-canvas--parse-gen-transform-forms
                            (plist-get args :properties))
          :body-key (plist-get args :body)
          :title-key (or (plist-get args :title-key) :title)
          :id-key (or (plist-get args :id-key) :canvas-id)
          :id-property id-property
          :id-raw-key (org-canvas--parse-gen-id-raw-key id-property)
          :entity-name (or (plist-get args :entity-name)
                           (capitalize feature-name))
          :after-read (plist-get args :after-read)
          :after-transform (plist-get args :after-transform))))

(defun org-canvas--parse-gen-read-fn (spec)
  "Return the read-props defun form SPEC describes."
  (let ((feature-name (plist-get spec :feature-name))
        (base-form `(list :title-raw (org-get-heading t t t t)
                          ,(plist-get spec :id-raw-key)
                          (org-entry-get pom ,(plist-get spec :id-property))
                          ,@(plist-get spec :read-forms)))
        (after-read (plist-get spec :after-read)))
    `(defun ,(plist-get spec :read-fn) (pom)
       ,(format "Read raw property strings from the %s heading at POM." feature-name)
       ,(if after-read
            `(let ((raw ,base-form))
               (funcall ,after-read raw pom))
          base-form))))

(defun org-canvas--parse-gen-transform-fn (spec)
  "Return the transform-props defun form SPEC describes."
  (let* ((feature-name (plist-get spec :feature-name))
         (title-key (plist-get spec :title-key))
         (id-key (plist-get spec :id-key))
         (base-form `(list ,title-key (org-canvas--strip-statistics-cookie
                                      (plist-get raw :title-raw))
                          ,id-key (plist-get raw ,(plist-get spec :id-raw-key))
                          ,@(plist-get spec :transform-forms)))
        (after-transform (plist-get spec :after-transform)))
    `(defun ,(plist-get spec :transform-fn) (raw)
       ,(format "Transform raw property strings RAW into typed %s data.\nPure function — no buffer access." feature-name)
       ,(if after-transform
            `(let ((data ,base-form))
               (funcall ,after-transform data))
          base-form))))

(defun org-canvas--parse-gen-entry-fn (spec)
  "Return the parse-entry defun form SPEC describes.
The body reads the raw properties, transforms them, refuses a missing
title and, when the spec names a body key, exports the subtree into it."
  (let ((feature-name (plist-get spec :feature-name))
        (title-key (plist-get spec :title-key))
        (id-key (plist-get spec :id-key))
        (entity-name (plist-get spec :entity-name))
        (body-key (plist-get spec :body-key)))
    `(defun ,(plist-get spec :parse-fn) ()
       ,(format "Extract %s data from the Org heading at point." feature-name)
       (org-back-to-heading t)
       (org-canvas--log-debug org-canvas--logger
                   "[Stage 1: Parse] Starting extraction at point %%d" (point))
       (let* ((pom (point))
              (raw (,(plist-get spec :read-fn) pom))
              (data (,(plist-get spec :transform-fn) raw)))
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
         data))))

(defun org-canvas--parse-gen-forms (feature args)
  "Return the forms `org-canvas-define-parse' emits for FEATURE and ARGS."
  (let ((spec (org-canvas--parse-gen-spec feature args)))
    `(progn
       ,(org-canvas--parse-gen-read-fn spec)
       ,(org-canvas--parse-gen-transform-fn spec)
       ,(org-canvas--parse-gen-entry-fn spec))))

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
     (\"DELAYED_POST_AT\" :delayed_post_at :type timestamp)
     (\"ALLOW_COMMENTS\" :allow_discussion_comments
                        :type boolean)
     (\"SPECIFIC_SECTIONS\" :specific_sections
                           :type string)))"
  (declare (indent 1))
  (org-canvas--parse-gen-forms feature args))

(provide 'org-canvas-core-macros)
;;; org-canvas-core-macros.el ends here
