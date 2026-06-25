# Canvas request-body contract tests

`org-canvas-contract-test.el` checks that each module's `build-payload`
output conforms to the Canvas create-operation request schema declared in
the OpenAPI spec (`documentation/architecture/canvas-openapi3.yaml`).

## Files

- `extract-canvas-contract.py` — parses the OpenAPI YAML and emits a compact
  JSON fixture (required fields, per-field type/enum, and the wrapper key).
- `canvas-contract.json` — generated fixture, read by the elisp test with the
  built-in JSON reader (no YAML dependency at test time). **Committed.**

## Regenerating

Re-run after the OpenAPI spec changes:

```bash
python3 test/contract/extract-canvas-contract.py
```

Requires PyYAML (`pip install pyyaml`).

## Coverage and exceptions

Covered: assignments, quizzes, modules, pages, calendar, assignment-groups,
group-categories, announcements, discussions.

Not covered (and why): rubrics (no documented create operation in the spec),
outcomes (hierarchical multi-endpoint create), new-quizzes (different API,
`/api/quiz/v1/`), sections (pull-only), files (multipart upload).

Justified deviations live in `org-canvas-contract--exceptions` in the test —
each entry documents a field a module emits that the documented operation
omits but Canvas honors (currently only `module[published]`).
