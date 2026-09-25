#!/usr/bin/env python3
"""Extract the GraphQL contract org-canvas's documents rely on from Canvas.

The GraphQL documents org-canvas sends (the post-policy mutations in
assignments.el and settings.el, postAssignmentGrades in submissions.el,
the checkpoints query and updateDiscussionTopic mutation in
discussions.el, and the document-processor query in assignments.el)
travel as opaque strings.  This script reads them out of
lisp/, validates each against a Canvas GraphQL schema, and emits a compact
JSON fixture (canvas-graphql-contract.json) holding only the types the
documents reach: every field of each reached object type with its type
reference, arguments and deprecation reason; every input field with its
nullability and default; enum values; plus provenance.  The elisp contract
test (test/org-canvas-graphql-contract-test.el) checks the documents and
the variable builders against that fixture with no schema at hand.

Two schema sources, one preferred:

    # The instance's own introspection: what the code actually talks to.
    # The token comes from the environment, never the command line, and
    # never reaches the fixture.
    CANVAS_API_TOKEN=... python3 test/contract/extract-canvas-graphql-contract.py \\
        --introspect https://canvas.example.edu

    # The SDL instructure/canvas-lms ships as schema.graphql, at a pinned
    # ref.  A baseline: the public mirror lags the instances by months.
    python3 test/contract/extract-canvas-graphql-contract.py \\
        --sdl schema.graphql --ref 318f2ad0495dd4d1e2fd3657497fbf71c91bda13

Requires graphql-core (pip install graphql-core).
"""
import argparse
import datetime
import json
import os
import re
import sys
import urllib.error
import urllib.request

try:
    from graphql import (
        GraphQLEnumType,
        GraphQLInputObjectType,
        GraphQLInterfaceType,
        GraphQLObjectType,
        GraphQLScalarType,
        GraphQLUnionType,
        Undefined,
        TypeInfo,
        TypeInfoVisitor,
        Visitor,
        build_client_schema,
        build_schema,
        get_introspection_query,
        get_named_type,
        parse,
        validate,
        visit,
    )
except ImportError:
    sys.exit("graphql-core is required: pip install graphql-core")

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
OUT = os.path.join(HERE, "canvas-graphql-contract.json")

# The lisp files that carry a GraphQL document.  Each document is a
# one-line string literal beginning with `query (' or `mutation ('; the
# regexp below reads them out, so a document changed in lisp is picked up
# by the next regeneration without editing this list.
SOURCES = [
    "lisp/org-canvas-assignments.el",
    "lisp/org-canvas-settings.el",
    "lisp/org-canvas-submissions.el",
    "lisp/org-canvas-discussions.el",
]
DOCUMENT = re.compile(r'"((?:query|mutation) \([^"\\]*)"')


def read_documents():
    """Return [(file, document)] for every GraphQL document in SOURCES."""
    found = []
    for rel in SOURCES:
        text = open(os.path.join(ROOT, rel), encoding="utf-8").read()
        for match in DOCUMENT.finditer(text):
            found.append((rel, match.group(1)))
    if not found:
        sys.exit("no GraphQL documents found under lisp/")
    return found


def load_schema_sdl(path):
    return build_schema(open(path, encoding="utf-8").read())


def load_schema_introspection(base_url):
    token = os.environ.get("CANVAS_API_TOKEN")
    if not token:
        sys.exit("--introspect needs CANVAS_API_TOKEN in the environment")
    body = json.dumps({"query": get_introspection_query(descriptions=False)}).encode()
    request = urllib.request.Request(
        base_url.rstrip("/") + "/api/graphql",
        data=body,
        headers={"Authorization": "Bearer " + token,
                 "Content-Type": "application/json"},
    )
    try:
        with urllib.request.urlopen(request) as reply:
            payload = json.load(reply)
    except urllib.error.HTTPError as err:
        sys.exit(f"introspection failed: HTTP {err.code} from {request.full_url}"
                 + (" (expired or revoked token?)" if err.code == 401 else ""))
    except urllib.error.URLError as err:
        sys.exit(f"introspection failed: {err.reason}")
    if "errors" in payload:
        sys.exit("introspection failed: " + json.dumps(payload["errors"]))
    return build_client_schema(payload["data"])


def reached_types(schema, documents):
    """Return the named types the DOCUMENTS touch, validating each first."""
    names = set()
    for rel, source in documents:
        ast = parse(source)
        errors = validate(schema, ast)
        if errors:
            lines = "\n  ".join(str(e).splitlines()[0] for e in errors)
            sys.exit(f"{rel}: document does not validate against the schema:\n  {lines}")
        info = TypeInfo(schema)

        class Collector(Visitor):
            def enter(self, *_args):
                argument = info.get_argument()
                for t in (info.get_parent_type(), info.get_type(), info.get_input_type(),
                          argument.type if argument else None):
                    if t is not None:
                        names.add(get_named_type(t).name)

        visit(ast, TypeInfoVisitor(info, Collector()))
    # An input object drags in every type its fields name, transitively,
    # since a variable builder may fill any of them.
    queue = list(names)
    while queue:
        t = schema.type_map[queue.pop()]
        if isinstance(t, GraphQLInputObjectType):
            for field in t.fields.values():
                name = get_named_type(field.type).name
                if name not in names:
                    names.add(name)
                    queue.append(name)
    return names


def describe(t):
    """Return the fixture entry for the named type T."""
    if isinstance(t, (GraphQLObjectType, GraphQLInterfaceType)):
        return {
            "kind": "INTERFACE" if isinstance(t, GraphQLInterfaceType) else "OBJECT",
            "fields": {
                name: {
                    "type": str(f.type),
                    "args": {a: str(arg.type) for a, arg in f.args.items()},
                    "deprecated": f.deprecation_reason,
                }
                for name, f in t.fields.items()
            },
        }
    if isinstance(t, GraphQLInputObjectType):
        return {
            "kind": "INPUT_OBJECT",
            "fields": {
                name: {
                    "type": str(f.type),
                    "hasDefault": f.default_value is not Undefined,
                    "deprecated": f.deprecation_reason,
                }
                for name, f in t.fields.items()
            },
        }
    if isinstance(t, GraphQLEnumType):
        return {"kind": "ENUM",
                "values": {name: v.deprecation_reason for name, v in t.values.items()}}
    if isinstance(t, GraphQLUnionType):
        return {"kind": "UNION", "types": [x.name for x in t.types]}
    if isinstance(t, GraphQLScalarType):
        return {"kind": "SCALAR"}
    sys.exit(f"unexpected type kind: {t}")


def supplement(path, fresh):
    """Return the fixture at PATH with the types of FRESH it lacks added.

    For a new document when the schema the fixture was generated from is
    out of reach (an SDL baseline beside an instance's introspection):
    the types already there are kept as they are, the added ones are
    named in the provenance with their source, and every type the
    documents no longer reach is dropped.
    """
    old = json.load(open(path, encoding="utf-8"))
    added = sorted(set(fresh["types"]) - set(old["types"]))
    types = {name: old["types"].get(name, fresh["types"][name])
             for name in fresh["types"]}
    provenance = dict(old["provenance"])
    if added:
        note = dict(fresh["provenance"])
        note["types"] = added
        provenance["supplements"] = provenance.get("supplements", []) + [note]
    return {"provenance": provenance, "roots": old["roots"],
            "documents": fresh["documents"], "types": dict(sorted(types.items()))}


def main():
    parser = argparse.ArgumentParser(
        description="Extract the GraphQL contract org-canvas's documents rely on from Canvas.")
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("--sdl", metavar="PATH", help="schema.graphql from instructure/canvas-lms")
    source.add_argument("--introspect", metavar="BASE_URL",
                        help="Canvas instance to introspect (token in CANVAS_API_TOKEN)")
    parser.add_argument("--ref", help="the canvas-lms tag or sha the SDL came from (provenance)")
    parser.add_argument("--out", default=OUT, help=f"fixture path (default {OUT})")
    parser.add_argument("--supplement", action="store_true",
                        help="keep the fixture at --out and add only the types it "
                             "lacks, recording where they came from")
    args = parser.parse_args()

    if args.sdl:
        schema = load_schema_sdl(args.sdl)
        provenance = {"source": "sdl", "ref": args.ref or "unknown",
                      "repository": "https://github.com/instructure/canvas-lms"}
    else:
        schema = load_schema_introspection(args.introspect)
        host = re.sub(r"^https?://", "", args.introspect).rstrip("/")
        provenance = {"source": "introspection", "instance": host}
    provenance["generated"] = datetime.date.today().isoformat()

    documents = read_documents()
    names = reached_types(schema, documents)
    fixture = {
        "provenance": provenance,
        "roots": {"query": schema.query_type.name,
                  "mutation": schema.mutation_type.name},
        "documents": sorted({rel for rel, _ in documents}),
        "types": {name: describe(schema.type_map[name]) for name in sorted(names)},
    }
    if args.supplement:
        fixture = supplement(args.out, fixture)
    with open(args.out, "w", encoding="utf-8") as fh:
        json.dump(fixture, fh, indent=1, sort_keys=True)
        fh.write("\n")
    print(f"wrote {args.out}: {len(documents)} documents, {len(names)} types "
          f"({provenance['source']})")


if __name__ == "__main__":
    main()
