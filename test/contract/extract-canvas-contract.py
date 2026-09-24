#!/usr/bin/env python3
"""Extract request-body contracts from the Canvas OpenAPI spec.

Reads documentation/architecture/canvas-openapi3.yaml and emits a compact
JSON fixture (canvas-contract.json) mapping each org-canvas module to the
Canvas create-operation's accepted request parameters: the wrapper key (if
any), the required fields, and per-field type/enum constraints.

The elisp contract test (test/org-canvas-contract-test.el) reads the JSON
fixture with the built-in json-reader, so no YAML dependency is needed at
test time.  Re-run this script when the OpenAPI spec changes:

    python3 test/contract/extract-canvas-contract.py
"""
import json
import os
import re
import sys

try:
    import yaml
except ImportError:
    sys.exit("PyYAML is required: pip install pyyaml")

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
SPEC = os.path.join(ROOT, "documentation", "architecture", "canvas-openapi3.yaml")
OUT = os.path.join(HERE, "canvas-contract.json")

# org-canvas module -> Canvas create operationId.  Announcements and
# discussions share the discussion_topics create operation.  Modules whose
# create operation is absent from this spec (rubrics), uses a different API
# (new-quizzes -> /api/quiz/v1/), is hierarchical (outcomes), or is pull-only
# (sections) are intentionally not covered here.
MODULE_OPS = {
    "announcements": "create_new_discussion_topic_courses",
    "discussions": "create_new_discussion_topic_courses",
    "assignments": "create_assignment",
    "assignment-groups": "create_assignment_group",
    "group-categories": "create_group_category_courses",
    "calendar": "create_calendar_event",
    "modules": "create_module",
    "quizzes": "create_quiz",
    "pages": "create_page_courses",
}

# org-canvas module -> (list operationId, show operationId): the read side.
# The feature registry's `:list-params' travel on the list operation and
# its `:item-params' on the show operation, and the contract test checks
# that every parameter a feature declares is one the spec documents there.
# Assignments read with override_assignment_dates=false on both, or a
# teacher gets a student's extension as the assignment's own dates (issue
# #273); quizzes are deliberately absent from that rule, since their show
# and list operations document no such parameter and Canvas's quiz
# serializer substitutes dates only for a reader who has been a student.
MODULE_READ_OPS = {
    "announcements": ("list_discussion_topics_courses", "get_single_topic_courses"),
    "discussions": ("list_discussion_topics_courses", "get_single_topic_courses"),
    "assignments": ("list_assignments", "get_single_assignment"),
    "assignment-groups": ("list_assignment_groups", "get_assignment_group"),
    "group-categories": ("list_group_categories_for_context_courses",
                         "get_single_group_category"),
    "calendar": ("list_calendar_events", "get_single_calendar_event_or_assignment"),
    "modules": ("list_modules", "show_module"),
    "quizzes": ("list_quizzes_in_course", "get_single_quiz"),
    "pages": ("list_pages_courses", "show_page_courses"),
}

# Pull-only modules -> (list operationId,): reads with no feature registry
# entry, whose query parameters the module keeps in its own constants.
# people: the roster read and the per-person departure read (issue #290)
# both go to the course enrollments index with `state[]' and `user_id'.
PULL_ONLY_READ_OPS = {
    "people": ("list_enrollments_courses",),
}

# Modules whose read (pull) response object is documented as a component
# schema.  Used to contract-check that pull tolerates the full documented
# response shape.  Only modules with a clean property-setter pull-item are
# listed (others fetch detail or need buffer context to test in isolation).
MODULE_READ_SCHEMAS = {
    "assignments": "Assignment",
    "assignment-groups": "AssignmentGroup",
}

BRACKET = re.compile(r"^([^\[]+)\[([^\]]+)\]")


def split_param(name):
    """Return (wrapper, field) for a form-param name.

    'assignment[name]'              -> ('assignment', 'name')
    'calendar_event[child][X][a]'   -> ('calendar_event', 'child')
    'name'                          -> (None, 'name')
    """
    m = BRACKET.match(name)
    if m:
        return m.group(1), m.group(2)
    return None, name


def find_op(spec, opid):
    for path, methods in spec["paths"].items():
        for method, op in methods.items():
            if isinstance(op, dict) and op.get("operationId") == opid:
                return op
    return None


def extract(op):
    content = op.get("requestBody", {}).get("content", {})
    schema = next(iter(content.values()), {}).get("schema", {}) if content else {}
    props = schema.get("properties", {})
    raw_required = schema.get("required", []) or []

    # Detect a single common wrapper shared by every bracketed property.
    wrappers = {split_param(n)[0] for n in props if split_param(n)[0]}
    wrapper = wrappers.pop() if len(wrappers) == 1 else None

    fields = {}
    for name, pschema in props.items():
        w, field = split_param(name)
        # Only fold names under the detected wrapper; keep the second segment.
        if wrapper and w != wrapper:
            continue
        entry = fields.setdefault(field, {})
        if isinstance(pschema, dict):
            if "type" in pschema and "type" not in entry:
                entry["type"] = pschema["type"]
            if "enum" in pschema:
                entry["enum"] = pschema["enum"]

    required = sorted({split_param(n)[1] for n in raw_required})
    return {
        "wrapper": wrapper,
        "required": required,
        "fields": fields,
    }


def extract_query_params(op):
    """Return the sorted names of OP's documented query parameters.

    The spec names an array parameter without the brackets org-canvas
    sends (`include`, not `include[]`); the elisp side strips them too.
    """
    return sorted(p["name"] for p in op.get("parameters", []) or []
                  if isinstance(p, dict) and p.get("in") == "query")


def extract_response_fields(spec, schema_name):
    """Return {field: type} for a component response schema."""
    schema = spec.get("components", {}).get("schemas", {}).get(schema_name, {})
    props = schema.get("properties", {}) or {}
    return {name: (p.get("type") if isinstance(p, dict) else None)
            for name, p in props.items()}


def main():
    spec = yaml.safe_load(open(SPEC))
    out = {}
    for module, opid in MODULE_OPS.items():
        op = find_op(spec, opid)
        if op is None:
            sys.exit(f"operationId not found: {opid} (module {module})")
        contract = extract(op)
        contract["operationId"] = opid
        read_ops = MODULE_READ_OPS.get(module)
        if read_ops:
            reads = {}
            for kind, read_opid in zip(("list", "item"), read_ops):
                read_op = find_op(spec, read_opid)
                if read_op is None:
                    sys.exit(f"operationId not found: {read_opid} (module {module})")
                reads[kind] = {"operationId": read_opid,
                               "params": extract_query_params(read_op)}
            contract["reads"] = reads
        schema_name = MODULE_READ_SCHEMAS.get(module)
        if schema_name:
            fields = extract_response_fields(spec, schema_name)
            if not fields:
                sys.exit(f"response schema empty: {schema_name} (module {module})")
            contract["response_schema"] = schema_name
            contract["response_fields"] = fields
        out[module] = contract

    for module, read_ops in PULL_ONLY_READ_OPS.items():
        reads = {}
        for kind, read_opid in zip(("list", "item"), read_ops):
            read_op = find_op(spec, read_opid)
            if read_op is None:
                sys.exit(f"operationId not found: {read_opid} (module {module})")
            reads[kind] = {"operationId": read_opid,
                           "params": extract_query_params(read_op)}
        out[module] = {"pull_only": True, "reads": reads}

    with open(OUT, "w") as fh:
        json.dump(out, fh, indent=2, sort_keys=True)
        fh.write("\n")
    print(f"wrote {OUT} ({len(out)} modules)")


if __name__ == "__main__":
    main()
