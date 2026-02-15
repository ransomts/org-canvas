;;; org-canvas-core.el --- Core utilities for org-canvas -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains the core, shared components of the org-canvas package.
;; It provides the foundation that all feature modules build upon.
;;
;; ARCHITECTURE OVERVIEW
;; =====================
;; The core module is organized into layers, each in its own sub-module:
;;
;;   org-canvas-core-config - User customizations, logging
;;   org-canvas-core-api    - HTTP communication with Canvas REST API
;;   org-canvas-core-org    - Org interaction, link resolution, pull helpers
;;   org-canvas-core-sync   - Sync pipeline, conflict, push/delete infrastructure
;;
;; DEPENDENCY RULES
;; ================
;; - All feature modules require org-canvas-core (this file)
;; - Feature modules must NOT depend on each other
;; - Core sub-modules must NOT import any feature modules (prevents circular deps)
;;
;; 4-STAGE PIPELINE PATTERN
;; ========================
;; Every feature module follows this consistent pattern:
;;
;;   1. Parse      - Extract data from Org heading properties
;;   2. Build      - Convert to Canvas API format (hash-tables)
;;   3. Execute    - Call API with timeout/404 recovery
;;   4. Finalize   - Save CANVAS_ID and LAST_SYNCED to Org file
;;
;; Use `org-canvas-define-sync' macro to generate sync functions that
;; follow this pattern with proper logging and error handling.

;;; Code:

(require 'org-canvas-core-config)
(require 'org-canvas-core-api)
(require 'org-canvas-core-org)
(require 'org-canvas-core-sync)

(provide 'org-canvas-core)
;;; org-canvas-core.el ends here
