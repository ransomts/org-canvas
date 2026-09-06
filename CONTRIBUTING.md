# Contributing to org-canvas

Thank you for your interest in contributing! This guide covers the development workflow, coding conventions, and testing practices.

## Development Setup

1. **Install Emacs** (29.1+; CI tests on 29.3 and 30.1).
2. **Install [Eldev](https://github.com/emacs-eldev/eldev)**:
   ```bash
   curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
   ```
3. **Clone and configure git hooks**:
   ```bash
   git clone https://github.com/ransomts/org-canvas.git
   cd org-canvas
   git config core.hooksPath .githooks
   eldev prepare    # Install dependencies
   ```
   The pre-push hook runs tests on both Emacs 29 and 30 (via nix-shell) and blocks pushes below 99% coverage.
4. **Verify your setup**:
   ```bash
   eldev compile     # Verify everything builds
   eldev test        # Run all ~3400 specs
   eldev lint        # Run linter (must be clean)
   eldev complexity  # Check cognitive complexity
   ```

## Architecture

### Module Structure

```
lisp/
├── org-canvas.el              # Main entry point, orchestrates all modules
├── org-canvas-core.el         # Meta-require for all core-* files
├── org-canvas-core-config.el  # Config, constants, shared enum values
├── org-canvas-core-api.el     # API request helpers, rate limiting
├── org-canvas-core-org.el     # Org property/buffer helpers, HTML export
├── org-canvas-core-sync.el    # Sync pipeline macros, push/pull/delete infra
├── org-canvas-validate.el     # Offline validation engine
└── org-canvas-{feature}.el    # Feature modules (one per content type)
```

### Dependency Rules

- All feature modules require `org-canvas-core`.
- **Feature modules must NOT depend on each other.** Cross-feature references go through `org-canvas-core` helpers.
- `org-canvas-core` must NOT import any feature modules (prevents circular deps).
- `org-canvas.el` orchestrates by requiring all modules.

### 4-Stage Pipeline

Every feature module follows this consistent pattern:

1. **Parse** (`org-canvas--{feature}-parse-entry`) — Extract data from Org heading properties
2. **Build Payload** (`org-canvas--{feature}-build-payload`) — Convert to Canvas API format
3. **Execute** (`org-canvas--{feature}-push-to-api`) — Call API with error handling
4. **Finalize** (`org-canvas--{feature}-finalize`) — Save CANVAS_ID and LAST_SYNCED

Most modules use the `org-canvas-define-sync` macro to generate the sync function from these four stages. Some modules (files, quizzes, modules, outcomes) have custom sync logic due to non-standard API patterns.

### Adding a New Content Type

1. Create `lisp/org-canvas-{feature}.el` with the 4-stage pipeline functions.
2. Use `org-canvas-define-sync` if the API follows the standard REST pattern.
3. Create `test/org-canvas-{feature}-test.el` with tests for all 4 stages plus sync/pull integration.
4. Register the module in `lisp/org-canvas.el` (require, sync order, pull order, delete).
5. Add the file to `eldev-undercover-fileset` in `Eldev`.
6. Mock the new sync/delete functions in `test/org-canvas-test.el`.
7. Add documentation to `documentation/manual.org`.

## Code Conventions

### Naming

| Pattern | Usage |
|---------|-------|
| `org-canvas--function-name` | Private (double dash) |
| `org-canvas-function-name` | Public (single dash) |
| `org-canvas-sync-{feature}` | Push entry point |
| `org-canvas-pull-{feature}` | Pull entry point |
| `org-canvas-sync-{feature}-at-point` | Sync single item |
| `org-canvas-delete-all-{feature}` | Delete all items |
| `org-canvas-delete-{feature}-at-point` | Delete single item |

### Logging

Use `elog` with `org-canvas--logger`:

```elisp
(elog-info org-canvas--logger "[Stage 1: Parse] Processing '%s'" title)
(elog-warning org-canvas--logger "[Sync] Skipped '%s': missing CANVAS_ID" title)
```

Stage markers follow the format `[Stage N: StageName]`.

### JSON/API

- Modules with nested payloads use hash-tables: `(make-hash-table)`
- Flat-payload modules use alists: `'((key . val))`
- Booleans: `t` for true, `:json-false` for false
- Org properties are always strings — compare with `"true"`/`"false"`

### Error Handling

- Wrap API calls in `condition-case`.
- Use `org-canvas--timeout-error-p` to detect timeout errors.
- On 404 for PUT: retry as POST.
- On 429/403 rate limit: retry with backoff.
- Continue processing other items if one fails.

### Complexity

Functions must stay below cognitive complexity 15. Run `eldev complexity` to check. Common patterns for reducing complexity:

- **Single-item helpers**: Extract the body of a `dolist` into a named helper.
- **Data-driven loops**: Replace repetitive `when`-blocks with a field-spec constant and `dolist` + `pcase`.
- **Conflict extraction**: Use `org-canvas--push-check-and-resolve-conflict` instead of inline conflict logic.

## Testing

### Test Structure

```
test/
├── test-helper.el                    # Common fixtures, mocks, macros
├── org-canvas-core-{sub}-test.el     # Core utility tests (5 files)
├── org-canvas-test.el                # Orchestration/integration tests
└── org-canvas-{feature}-test.el      # Feature module tests (14 files)
```

Tests use the [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) framework.

### Running Tests

```bash
eldev test                    # All tests
eldev test "assignments"      # Tests matching pattern
eldev test -u "on,text,dontsend"  # With coverage report
```

### Test Utilities

**`with-temp-org-buffer`** — Create a temp Org buffer with content:

```elisp
(with-temp-org-buffer
 "* Heading
:PROPERTIES:
:CANVAS_ID: 123
:END:
"
 (org-back-to-heading)
 (should (equal (org-entry-get (point) "CANVAS_ID") "123")))
```

**`with-mock-api`** — Mock API calls without network access:

```elisp
(with-mock-api
 (org-canvas--announcement-push-to-api data payload)
 (should (test-org-canvas-api-called-p 'POST "discussion_topics")))
```

**`with-sync-test-env`** — Suppress log clearing and display-buffer side effects during sync tests.

### Emacs Version Compatibility

Some org-mode functions behave differently between Emacs 29.x and 30.x. Use skip conditions for version-specific tests:

```elisp
(unless test-org-canvas-emacs-30-p
  (signal 'buttercup-pending "Requires Emacs 30+ org-mode"))
```

### Writing Good Tests

- Test all 4 pipeline stages independently.
- Use `with-temp-org-buffer` for Org property tests (creates real temp files).
- Mock `plz` directly to test API internals.
- Test with 3+ headings when verifying link resolution (catches `save-excursion` bugs).
- Use `spy-on` + `:to-have-been-called-with` for message verification.
- Pre-bind `:type` in `let*` to avoid Emacs 29 `oclosure-lambda` shadowing.

### Coverage

Coverage target is 99%+. Run `eldev test -u "on,text,dontsend"` to check. The pre-push hook blocks pushes below the threshold.

## Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add calendar event recurrence support
fix: handle nil CANVAS_ID in module link resolution
refactor: extract title validation into shared helper
test: add rate-limit retry integration test
docs: update manual for new quiz properties
```

## Pull Request Process

1. Create a feature branch from `main`.
2. Make your changes, ensuring:
   - `eldev test` passes (all specs green)
   - `eldev lint` has no warnings
   - `eldev complexity` has 0 functions above threshold
   - Coverage stays at 99%+
3. Open a PR against `main` with a summary and test plan.
4. CI runs tests on Emacs 29.3 and 30.1, plus lint and complexity checks.

## Pre-push Hook

The `.githooks/pre-push` hook automatically:
- Runs `eldev test` on Emacs 29 (via nix-shell, no coverage)
- Runs `eldev test` on Emacs 30 with coverage
- Blocks the push if coverage drops below 99%

If you need to bypass it temporarily (not recommended):
```bash
git push --no-verify
```

## Documentation

- **[Reference manual](documentation/manual.org)** — Complete property specs, command reference, file formats
- **[Workflows guide](documentation/workflows.org)** — Semester/weekly/daily/per-assignment workflows
- **[FAQ & Recipes](documentation/faq.org)** — Quick answers to "how do I..." questions

When adding features, update the relevant documentation files.

## Questions?

Open an issue on [GitHub](https://github.com/ransomts/org-canvas/issues).

## License

By contributing, you agree that your contributions will be licensed under the [GPL-3.0 License](LICENSE).
