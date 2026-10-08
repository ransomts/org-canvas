# Testing Guide

Commands, isolation rules, layout and helpers for the test suite. Linked from [CLAUDE.md](../CLAUDE.md); the full reference is [architecture/testing.org](architecture/testing.org).

```bash
eldev test                                                    # All tests; the last line is the spec count
eldev test "core"                                             # Tests matching a pattern (no -p flag)
eldev test -u "on,codecov,dontsend" -U coverage/coverage.json # Per-file coverage; pre-push gate is 99%
python3 scripts/patch-coverage.py                            # After the line above: lisp/ lines added since origin/main that no test runs (codecov/patch)
eldev test -u "on,text,dontsend"                              # Text summary (may end in overflow-error; prefer JSON)
ELDEV_JUNIT=1 JUNIT_REPORT_FILE=test-results.xml eldev test   # JUnit XML for Codecov
grep -rn buttercup-pending test/                              # Specs skipped on Emacs 29.x
scripts/test-each-file.sh                                     # Every test file alone (~2.5 min); --shard K/N for one CI shard
scripts/test-shard.sh 2/3                                     # The files of CI shard 2 of 3 (eldev test $(scripts/test-shard.sh 2/3))
scripts/test-each-file.sh --timings > test/shard-weights.txt  # Refresh the shard weights when shards drift apart
eldev exec -f scripts/graphql-introspect.el                   # Refresh the GraphQL fixture from the live instance, once per semester; also writes the gitignored SDL test/contract/instance-schema.graphql (test/contract/README.md)
```

Every test file must pass on its own (`scripts/test-each-file.sh`, CI's sharded `isolation` job; #260): test-helper loads the whole package, and a spec that sets global state — the log level above all — restores it. A spec that runs `org-canvas-init` or otherwise `setq`s the course settings wraps itself in `with-org-canvas-course-globals`; test-helper fails any spec that leaves one changed (#353).

Layout: `test/test-helper.el` (fixtures, mocks, macros, network guard); `test/org-canvas-core-{config,api,org,html,pull,sync,conflict,delete,usability}-test.el`; `test/org-canvas-test.el` (orchestration); one `test/org-canvas-{feature}-test.el` per module; `test/org-canvas-validate-test.el`; `test/org-canvas-dry-run-test.el`; `test/org-canvas-doc-reference-test.el` (the manual's generated Property Reference); `test/org-canvas-contract-test.el` and `test/org-canvas-graphql-contract-test.el` (REST payloads and every registered read's query parameters against the OpenAPI spec, #273; the GraphQL documents and their variables against the Canvas GraphQL schema, #269); `test/contract/` (both fixtures and their generators; the GraphQL one regenerates from the instance's introspection or the canvas-lms SDL, see its README); `test/mutation/` (mutation-testing harness); `test/docgen/` (generates the Property Reference from the registry).

Utilities (test-helper.el; full reference in testing.org):
- `with-temp-org-buffer` — file-backed temp Org buffer; org functions misbehave in `with-temp-buffer`
- `with-mock-api` — records calls; assert with `test-org-canvas-api-called-p`, `test-org-canvas-api-call-count`
- `with-sync-test-env`, `with-org-canvas-test-config`, `with-html-to-org-identity`, `with-nonexistent-canvas-files` (binds every `org-canvas-*-file`, since defcustom defaults are evaluated at load)
- Generators: `test-org-canvas-define-common-{parse,transform,push}-tests`, `with-pull-property-test`
- Emacs 29: pre-bind `:type` in `let*` before `expect` (oclosure shadowing); skip with `(signal 'buttercup-pending ...)` under `test-org-canvas-emacs-30-p`
- Commands take a bare `(interactive)` and read prompted arguments in the body through a helper: a sexp spec such as `(interactive (list (completing-read ...)))` makes edebug skip parts of the defun, so undercover counts body lines as unrun however many specs call it (bisected 2026-04-25 in `org-canvas-activate-course`). A string spec avoids that but bypasses a mocked `read-string` and can hang a batch run
- Mock `plz` directly (`cl-letf`) to test `org-canvas-api-request` internals; `plz-error` is signalled as the struct, not a list

Adding a module: also add it to `eldev-undercover-fileset` in `Eldev` and mock its sync/delete functions in `test/org-canvas-test.el` (module-developer-guide.org, Steps 10–12).
