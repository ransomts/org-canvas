# Setup and Build

How to build, lint and test org-canvas, what CI runs, and its dependencies. Linked from [CLAUDE.md](../CLAUDE.md).

This project uses Eldev (Elisp Development Tool):

```bash
eldev prepare        # Install dependencies (first run, or after Eldev changes)
eldev test           # Run tests; the last line prints the spec count (about 40 s)
eldev compile        # Compile Elisp files
eldev lint           # Run linter
eldev complexity     # Cognitive complexity report (target: 0 functions above 15)
eldev package        # Create distributable package
eldev clean all      # Clear the Eldev cache — mandatory after editing a macro
```

**Always run `eldev lint` before committing or pushing.** Fix any warnings before proceeding. CI lints on Emacs 30.1, whose checkdoc rejects a third-person verb such as "holds" anywhere in a docstring's first line; a newer local Emacs accepts it, so keep first lines imperative throughout (PRs #89/#90/#189/#258/#259 went red on exactly this). The pre-push hook lints under the pinned Emacs 30 for that reason; a bare `eldev lint` on a newer Emacs is not the CI check.

`org-canvas.el` at the repository root is a symlink to `lisp/org-canvas.el` so the package linter finds `Package-Requires` while the load path stays in `lisp/` (testing.org, "Eldev Test Configuration"). CI runs the tests on Emacs 29.3, 29.4 and 30.1; the pre-push hook runs 29 and 30 through nix-shell. CI runs on every pull request whatever its base. `main` requires the checks to pass but not an up-to-date head (`strict` is off), so independent PRs merge in any order; a PR stacked on another branch is retargeted to `main` by GitHub when the lower one merges and its branch is deleted. Auto-merge is on: queue a ready PR with `gh pr merge N --auto --merge`.

CI (`.github/workflows/ci.yml`) runs Emacs 30.1's suite in one job, with coverage for Codecov; Emacs 29.3 and 29.4 each run in three shards (`.github/workflows/test-shards.yml`, files split by `scripts/test-shard.sh` using `test/shard-weights.txt`), and the isolation check runs in three shards too. An aggregator job per sharded check reports under one name (`test (29.3)`, `test (29.4)`, `isolation`), failing unless every shard passed. Branch protection on `main` requires `test (29.3)`, `test (29.4)`, `test (30.1)`, `lint` and `complexity` (`isolation` is not required); rename a job only together with the protection rule. Lint, complexity and `generated-artifacts` (the OpenAPI contract fixture regenerated as a no-op, then the changelog check) are single jobs; the mutation ratchet runs weekly or on dispatch, never on a PR. A new push to a PR cancels that PR's earlier run; runs on `main` never cancel each other. Each kind of job keeps its own `.eldev` cache (`cache-purpose` in `.github/actions/setup-emacs-env`), restored from `main` by a PR, so a run with unchanged dependencies does not need ELPA (#471). Dependabot opens one grouped PR a week for the actions. testing.org, "CI Pipeline".

**Changelog: one fragment per PR, never CHANGELOG.org.** A PR adds `changelog.d/<issue>.org` (a `* Added`/`* Changed`/`* Fixed` heading, then `- #<issue> ...` items; `changelog.d/README.org`), which CI validates with `scripts/changelog-collect.py --check`; at release time `scripts/changelog-collect.py` folds them into CHANGELOG.org and deletes them. When every PR wrote the top of the same section, each merge conflicted with the next.


## Dependencies

External: `plz`, `transient` (0.4+), `org` (9.6+), `ox-html`; `pandoc` is optional (HTML→Org on pull). Emacs floor: see `Package-Requires` in `lisp/org-canvas.el`. Logging is in-tree (`org-canvas-core-log.el`); `request.el` is not a dependency (its leftover requires were dead code from the plz migration).

## gh CLI Quirk

`gh issue view N` exits non-zero due to a GraphQL Projects-classic deprecation warning even on success — use `gh issue view N --json body --jq .body` instead.
