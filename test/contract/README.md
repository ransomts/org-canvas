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

## Read-parameter contract (issue #273)

The fixture also captures, per module, the query parameters the spec
documents on the list and show operations (`MODULE_READ_OPS` in the
generator, the `reads` key in the fixture). The "Canvas read-parameter
contract" tests check every registered feature's `:list-params` against
its list operation, `:item-params` against its show operation, and the
property registry's `:body-list-params` against the list — a misspelled or
unsupported query parameter is silently ignored by Canvas, and nothing else
in the code path would notice. Justified deviations live in
`org-canvas-contract--read-exceptions` (pages' `include[]=body`, which the
documented list operation omits but Canvas honors). The test also pins the
parameter the issue was about: assignments read with
`override_assignment_dates=false` on both operations, and quizzes declare
nothing, since neither quiz operation documents it.

A pull-only module with no feature registry entry lists its read in
`PULL_ONLY_READ_OPS`; the fixture marks it `"pull_only": true` and the
generic loop skips it, leaving a spec of its own to check the module's
parameter constants. Currently only people (issue #290): the roster read
and the per-person departure read, both on `list_enrollments_courses`.

# GraphQL contract (issue #269)

The GraphQL documents travel to Canvas as strings: the post-policy
mutations (assignments, settings), `postAssignmentGrades` and the document
processor reports query (submissions, issue #351), the checkpoints query
and `updateDiscussionTopic` mutation (discussions), and the
document-processor query (assignments, issue #350).
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

## Standing procedure: regenerate at each semester boundary

Canvas ships every three weeks and the committed fixture is a snapshot,
so it drifts silently until a document changes or a live run fails.  The
agreed cadence is once per term, before the first push of the semester:

```bash
python3 -m venv ~/.local/share/org-canvas-venv && ~/.local/share/org-canvas-venv/bin/pip install graphql-core   # once
ORG_CANVAS_PYTHON=~/.local/share/org-canvas-venv/bin/python eldev exec -f scripts/graphql-introspect.el
eldev test test/org-canvas-graphql-contract-test.el
git add test/contract/canvas-graphql-contract.json && git commit -m "Regenerate the GraphQL contract fixture from the instance"
```

`scripts/graphql-introspect.el` reads the credentials the way the package
does — the file `ORG_CANVAS_CREDENTIALS` names, else
`lisp/org-canvas-credentials.el`, else `auth-source` for the host of
`org-canvas-base-url` — and hands the token to the extractor through the
child's process environment only: never a command line, so never the
shell history, and never the script's output (it prints the token's
length, nothing more).  A 401 means the token is expired or revoked.

Read the diff before committing.  Types and fields *added* around the
documents are Canvas moving on and cost nothing; a field the documents use
that is *removed* or `@deprecated` fails the test, which names the
document and the field, and that is a code change to make before pushing
anything that sends it.

## Two schema sources

The instance's own introspection is what the code actually talks to, so
prefer it when regenerating; the procedure above uses it.  To run the
extractor by hand, the token comes from the environment and never reaches
the fixture — export it from a file rather than typing it on a command
line the shell would remember:

```bash
CANVAS_API_TOKEN=$(cat ~/.canvas-token) python3 test/contract/extract-canvas-graphql-contract.py \
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

To add a new document's types without regenerating the rest — the
instance out of reach, the fixture otherwise current — pass
`--supplement`: the fixture's existing types are kept as they are, only
the missing ones come from the SDL, and the provenance lists them under
`supplements` with the ref.  The next introspection replaces them.  The
document-processor query's `Assignment`, `AssignmentConnection`,
`ExternalTool`, `LtiAssetProcessor` and `LtiAssetProcessorConnection`
came in this way (canvas-lms `1c9f0bb8`), and so did the similarity
reports query's `Submission`, `SubmissionConnection`, `LtiAssetReport`,
`LtiAssetReportConnection` and `TotalCountPageInfo` (issue #351;
canvas-lms `318f2ad0`, whose `schema.graphql` is the same file):

```bash
python3 test/contract/extract-canvas-graphql-contract.py --supplement \
    --sdl /tmp/schema.graphql --ref <sha>
```

Requires graphql-core (`pip install graphql-core`).  The script validates
each document first and refuses to write a fixture for one that does not
parse or validate, naming the file and the errors.

## When to re-run, besides the semester boundary

- A document changes or a new one is added (add its file to `SOURCES` in
  the script and its symbol to `org-canvas-graphql-contract--documents` in
  the test; a document that names a type the fixture lacks fails the test
  with "is not in the fixture", which is the cue).
- A live GraphQL run fails with a validation error.

What the check proves and what it cannot: the *shape* — that the fields,
arguments and input types the documents name exist on the schema they were
checked against.  A feature flag does not shape the schema (`checkpoints` is
in the SDL whether or not the account flag is on; the flag is enforced when
the field resolves), so neither source answers "is the flag on".
