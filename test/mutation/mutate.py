#!/usr/bin/env python3
"""Mutation testing for org-canvas.

Measures the *depth* of the test suite, not its breadth: a high line-coverage
suite can still miss bugs if its assertions are weak.  This harness applies
small, syntax-safe mutations to the Elisp sources, runs the test suite against
each one, and reports which mutations the suite FAILED to catch ("survivors").

  * killed   = at least one test failed  -> the suite caught the change (good)
  * survived = all tests still passed     -> a blind spot (weak/no assertion)

mutation score = killed / (killed + survived).  Survivors are the actionable
output: each is a line where you could break behavior and no test would notice
(or a semantically-equivalent mutant a human should dismiss).

Usage:
    python3 test/mutation/mutate.py                 # default bounded run
    python3 test/mutation/mutate.py --files lisp/org-canvas-core-org.el
    python3 test/mutation/mutate.py --max 40 --timeout 90
    python3 test/mutation/mutate.py --pattern core  # scope tests (faster, less precise)

This is on-demand dev tooling (each mutation runs the suite, ~20s), not a
per-commit gate.  Files are always restored, even on Ctrl-C.
"""
import argparse
import glob
import os
import random
import re
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))

# Each operator: (name, compiled-regex, replacement).  Regexes use lookarounds
# so they only match whole Lisp tokens — never a substring of a longer symbol,
# keyword, or string — which keeps every mutation syntactically valid.
OPERATORS = [
    # boolean literals
    ("t->nil", re.compile(r"(?<![\w:&-])t(?![\w-])"), "nil"),
    ("nil->t", re.compile(r"(?<![\w:&-])nil(?![\w-])"), "t"),
    # numeric comparison boundaries
    ("gt->ge", re.compile(r"(?<![\w<>=/-])>(?![\w<>=-])"), ">="),
    ("lt->le", re.compile(r"(?<![\w<>=/-])<(?![\w<>=-])"), "<="),
    # equality strength
    ("equal->eq", re.compile(r"\(equal(?=\s)"), "(eq"),
    # arithmetic
    ("1+->1-", re.compile(r"\(1\+(?=\s)"), "(1-"),
    # boolean connective swap
    ("and->or", re.compile(r"\(and(?=\s)"), "(or"),
    ("or->and", re.compile(r"\(or(?=\s)"), "(and"),
]

def code_mask(text):
    """Return a per-char bool list: True where TEXT is code, False inside a
    string literal, a `;' comment, or a `?' char literal.  Mutating those
    regions only produces equivalent mutants (docstring/comment prose), so we
    exclude them up front to avoid wasting whole test runs on noise."""
    mask = [True] * len(text)
    i, n = 0, len(text)
    in_str = in_comment = False
    while i < n:
        c = text[i]
        if in_comment:
            mask[i] = False
            if c == "\n":
                in_comment = False
            i += 1
        elif in_str:
            mask[i] = False
            if c == "\\" and i + 1 < n:
                mask[i + 1] = False
                i += 2
            else:
                if c == '"':
                    in_str = False
                i += 1
        elif c == '"':
            in_str = True
            mask[i] = False
            i += 1
        elif c == ";":
            in_comment = True
            mask[i] = False
            i += 1
        elif c == "?":            # char literal, e.g. ?a or ?\n — don't mutate
            mask[i] = False
            if i + 1 < n and text[i + 1] == "\\" and i + 2 < n:
                mask[i + 1] = mask[i + 2] = False
                i += 3
            elif i + 1 < n:
                mask[i + 1] = False
                i += 2
            else:
                i += 1
        else:
            i += 1
    return mask


def discover_sites(files):
    """Return mutation sites (dicts) found in code regions only."""
    sites = []
    for path in files:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
        mask = code_mask(text)
        for name, rx, repl in OPERATORS:
            for m in rx.finditer(text):
                s, e = m.start(), m.end()
                if not all(mask[s:e]):
                    continue            # inside string/comment/char-literal
                sites.append({
                    "file": path, "start": s, "end": e,
                    "lineno": text.count("\n", 0, s),
                    "op": name, "repl": repl, "orig": text[s:e],
                })
    return sites


def parse_failed(output):
    """Return number of failed specs from buttercup output, or None if unknown."""
    m = re.search(r"Ran \d+ (?:out of \d+ )?specs?, (\d+) failed", output)
    return int(m.group(1)) if m else None


def run_suite(pattern, timeout):
    """Run the test suite; return (status, failed) where status in
    killed/survived/error/timeout."""
    cmd = ["eldev", "test"]
    if pattern:
        cmd.append(pattern)
    try:
        proc = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True,
                              timeout=timeout)
    except subprocess.TimeoutExpired:
        return ("timeout", None)
    failed = parse_failed(proc.stdout + proc.stderr)
    if failed is None:
        # Could not parse a spec count: compile/load error counts as caught,
        # but flag it so equivalent/compile noise is visible.
        return ("error", None)
    return (("survived" if failed == 0 else "killed"), failed)


def apply_mutation(path, originals, site):
    text = originals[path]
    mutated = text[:site["start"]] + site["repl"] + text[site["end"]:]
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(mutated)


def restore(path, originals):
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(originals[path])


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--files", nargs="*", help="Elisp files (default: all lisp/*.el)")
    ap.add_argument("--max", type=int, default=12,
                    help="max mutations to run (sampled); 0 = all (default 12)")
    ap.add_argument("--timeout", type=int, default=120,
                    help="per-run test timeout in seconds (default 120)")
    ap.add_argument("--pattern", default=None,
                    help="eldev test pattern to scope tests (faster, less precise)")
    ap.add_argument("--seed", type=int, default=1, help="sampling seed (default 1)")
    args = ap.parse_args()

    files = args.files or sorted(glob.glob(os.path.join(ROOT, "lisp", "*.el")))
    files = [os.path.abspath(f) for f in files]

    sites = discover_sites(files)
    total_found = len(sites)
    random.seed(args.seed)
    random.shuffle(sites)
    if args.max and len(sites) > args.max:
        skipped = len(sites) - args.max
        sites = sites[:args.max]
    else:
        skipped = 0

    print(f"Discovered {total_found} mutation sites across {len(files)} file(s).")
    if skipped:
        print(f"Running a sample of {len(sites)} (seed={args.seed}); "
              f"{skipped} not run this pass. Use --max 0 to run all.")
    print(f"Test command: eldev test{(' ' + args.pattern) if args.pattern else ''}"
          f"  (timeout {args.timeout}s each)\n")

    # Snapshot originals for guaranteed restore.
    originals = {}
    for path in files:
        with open(path, encoding="utf-8") as fh:
            originals[path] = fh.read()

    killed = survived = errored = 0
    survivors = []
    start = time.time()
    try:
        for n, site in enumerate(sites, 1):
            rel = os.path.relpath(site["file"], ROOT)
            label = (f"{rel}:{site['lineno'] + 1} "
                     f"[{site['op']}: {site['orig']}->{site['repl']}]")
            apply_mutation(site["file"], originals, site)
            try:
                status, failed = run_suite(args.pattern, args.timeout)
            finally:
                restore(site["file"], originals)
            mark = {"killed": "killed ", "survived": "SURVIVED",
                    "error": "error  ", "timeout": "killed*"}[status]
            print(f"[{n}/{len(sites)}] {mark}  {label}")
            if status == "survived":
                survived += 1
                survivors.append(label)
            elif status == "error":
                errored += 1   # compile/parse failure: not a behavior catch
            else:
                killed += 1
    except KeyboardInterrupt:
        print("\nInterrupted — restoring files.")
        for path in files:
            restore(path, originals)
        sys.exit(130)

    scored = killed + survived
    score = (killed / scored * 100) if scored else 0.0
    elapsed = int(time.time() - start)
    print("\n" + "=" * 60)
    print(f"Mutation score: {score:.1f}%  ({killed} killed / {scored} scored)")
    print(f"  survived: {survived}   killed: {killed}   "
          f"compile-errors: {errored}   ({elapsed}s)")
    if survivors:
        print("\nSurviving mutants (no test caught these — review for weak "
              "assertions or equivalent mutants):")
        for s in survivors:
            print(f"  - {s}")
    # Non-zero exit if any mutant survived, so CI/manual runs can gate.
    sys.exit(1 if survivors else 0)


if __name__ == "__main__":
    main()
