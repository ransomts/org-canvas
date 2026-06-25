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
        schema_name = MODULE_READ_SCHEMAS.get(module)
        if schema_name:
            fields = extract_response_fields(spec, schema_name)
            if not fields:
                sys.exit(f"response schema empty: {schema_name} (module {module})")
            contract["response_schema"] = schema_name
            contract["response_fields"] = fields
        out[module] = contract

    with open(OUT, "w") as fh:
        json.dump(out, fh, indent=2, sort_keys=True)
        fh.write("\n")
    print(f"wrote {OUT} ({len(out)} modules)")


if __name__ == "__main__":
    main()
