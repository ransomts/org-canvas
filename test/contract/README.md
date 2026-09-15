# Canvas request-body contract tests

`org-canvas-contract-test.el` checks that each module's `build-payload`
output conforms to the Canvas create-operation request schema declared in
the OpenAPI spec (`documentation/architecture/canvas-openapi3.yaml`).
The GraphQL side, `org-canvas-graphql-contract-test.el`, has its own
section at the end.

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

## Pull-side contract

The fixture also captures the documented *response* object fields (typed) for
modules whose pull-item is a clean property-setter (`MODULE_READ_SCHEMAS` in
the generator; currently assignments and assignment-groups). The
"Canvas response (pull) contract" tests feed a pull-item the full documented
response shape and assert it tolerates every field and reads the ones it
depends on — the read-path analog of the request-body contract.

# GraphQL contract (issue #269)

Five GraphQL documents travel to Canvas as strings: the post-policy
mutations (assignments, settings), `postAssignmentGrades` (submissions), and
the checkpoints query and `updateDiscussionTopic` mutation (discussions).
`org-canvas-graphql-contract-test.el` checks each — every selected field
exists on its parent type and is not deprecated, every argument exists and
a variable's declared type fits it, every non-null input field is supplied,
enum literals are members — and runs the callers with the sender mocked to
check the variables they build against the fixture's input types (required
keys, scalar kinds, a `Boolean!` that is never nil, a list that is a vector).

## Files

- `extract-canvas-graphql-contract.py` — reads the documents out of `lisp/`
  (the `"query ("` / `"mutation ("` string literals), validates them against
  a Canvas GraphQL schema, and emits the types they reach.
- `canvas-graphql-contract.json` — the fixture: for each reached type its
  fields with type references, arguments and deprecation reason (objects),
  input fields with nullability and default (inputs), values (enums), plus
  provenance. **Committed**; CI reads it and never touches a schema.

## Two schema sources

The instance's own introspection is what the code actually talks to, so
prefer it when regenerating.  The token comes from the environment, never
the command line, and never reaches the fixture:

```bash
CANVAS_API_TOKEN=... python3 test/contract/extract-canvas-graphql-contract.py \
    --introspect https://canvas.example.edu
```

The SDL `instructure/canvas-lms` ships as `schema.graphql` is the fallback
when no token is at hand — a baseline, since the public mirror lags the
instances by months (its last push predated the running code by four months
at the time of writing).  Pin the ref so the provenance says what it was:

```bash
curl -sSLo /tmp/schema.graphql \
  https://raw.githubusercontent.com/instructure/canvas-lms/<sha>/schema.graphql
python3 test/contract/extract-canvas-graphql-contract.py --sdl /tmp/schema.graphql --ref <sha>
```

Requires graphql-core (`pip install graphql-core`).  The script validates
each document first and refuses to write a fixture for one that does not
parse or validate, naming the file and the errors.

## When to re-run

- A document changes or a new one is added (add its file to `SOURCES` in
  the script and its symbol to `org-canvas-graphql-contract--documents` in
  the test; a document that names a type the fixture lacks fails the test
  with "is not in the fixture", which is the cue).
- A live GraphQL run fails with a validation error.
- A Canvas release lands.

What the check proves and what it cannot: the *shape* — that the fields,
arguments and input types the documents name exist on the schema they were
checked against.  A feature flag does not shape the schema (`checkpoints` is
in the SDL whether or not the account flag is on; the flag is enforced when
the field resolves), so neither source answers "is the flag on".
