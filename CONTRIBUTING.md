# Contributing to org-canvas

## Development Setup

1. **Install Emacs** (29.3+ or 30.1+).
2. **Install [Eldev](https://github.com/emacs-eldev/eldev)**:
   ```bash
   curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
   ```
3. **Clone and configure git hooks**:
   ```bash
   git clone <repo-url> && cd org-canvas
   git config core.hooksPath .githooks
   ```
   The pre-push hook runs tests on both Emacs 29 and 30 (via nix-shell) and blocks pushes below 99% coverage.
4. **Verify your setup**:
   ```bash
   eldev test          # Run all tests
   eldev lint          # Run linter
   eldev complexity    # Check cognitive complexity
   ```

## Running Tests

```bash
eldev test              # All tests (~1940 specs)
eldev test "pattern"    # Tests matching pattern
eldev test -u "on,text,dontsend"   # With coverage report
```

### Test structure

Tests live in `test/` and use the [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) framework. Each feature module has a corresponding `*-test.el` file covering all 4 pipeline stages (Parse, Build, Push, Finalize). Shared test utilities are in `test/test-helper.el`.

### Emacs version notes

Some tests skip on Emacs 29.x due to org-mode differences. These use `(signal 'buttercup-pending ...)` and are documented in the test files. CI runs the matrix `[29.3, 29.4, 30.1]`.

## Code Style

### Naming conventions

| Scope | Pattern | Example |
|-------|---------|---------|
| Private functions | `org-canvas--name` | `org-canvas--require-title` |
| Public functions | `org-canvas-name` | `org-canvas-sync-pages` |
| Sync entry points | `org-canvas-sync-{feature}` | `org-canvas-sync-announcements` |
| Delete commands | `org-canvas-delete-all-{feature}` | `org-canvas-delete-all-pages` |

### Module conventions

- All feature modules require `org-canvas-core` (never each other).
- Every feature follows the 4-stage pipeline: Parse, Build Payload, Push, Finalize.
- Use `org-canvas--require-title` for title validation in parse functions.
- Use hash-tables for nested payloads, alists for flat payloads.
- Boolean handling: `t` for true, `:json-false` for false.
- Keep functions below cognitive complexity 15 (`eldev complexity`).

### Logging

Use `elog` with `org-canvas--logger`. Prefix stage markers: `[Stage N: StageName]`.

## Commit Messages

Use [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add calendar event recurrence support
fix: handle nil CANVAS_ID in module link resolution
refactor: extract title validation into shared helper
test: add rate-limit retry integration test
chore: pin GitHub Actions to commit SHAs
docs: update CONTRIBUTING.md with test patterns
```

## Pull Request Process

1. Create a feature branch from `main`.
2. Make your changes, ensuring:
   - `eldev test` passes (all specs green)
   - `eldev lint` has no warnings
   - `eldev complexity` has no violations above budget (2 max)
   - Coverage stays at 99%+ (`eldev test -u "on,text,dontsend"`)
3. Open a PR against `main`. Include a summary and test plan.
4. CI will run tests on Emacs 29.3, 29.4, and 30.1 plus lint and complexity checks.

## Pre-push Hook

The `.githooks/pre-push` hook automatically:
- Runs `eldev test` on Emacs 29 (via nix-shell, no coverage)
- Runs `eldev test` on Emacs 30 with coverage
- Blocks the push if coverage drops below 99%

If you need to bypass it temporarily (not recommended):
```bash
git push --no-verify
```

## Adding a New Feature Module

1. Create `lisp/org-canvas-{feature}.el` with the 4-stage pipeline.
2. Create `test/org-canvas-{feature}-test.el` covering all stages.
3. Register the module in `lisp/org-canvas.el` (require + sync ordering).
4. Add the file to `eldev-undercover-fileset` in `Eldev`.
5. Mock new sync/delete functions in `test/org-canvas-test.el`.
6. Run the full verification suite before submitting.
