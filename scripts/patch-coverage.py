#!/usr/bin/env python3
"""Report the lisp/ lines a branch adds that no test runs.

Codecov's patch check judges only the lines a pull request changes, so
a PR can be green on every job and still fail there.  This answers the
same question locally, before a push:

    eldev test -u "on,codecov,dontsend" -U coverage/coverage.json
    python3 scripts/patch-coverage.py            # against origin/main
    python3 scripts/patch-coverage.py main       # or any other base

Every added line under lisp/ that the coverage report counts as code
and marks as never run is printed as FILE LINE, followed by a
"patch coverage HIT/TOTAL" line.  Exits 1 when any line is uncovered,
2 when the coverage report is missing.
"""

import json
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
REPORT = ROOT / "coverage" / "coverage.json"
HUNK = re.compile(r"@@ -\S+ \+(\d+)(?:,(\d+))? @@")


def added_lines(base):
    """Yield (path, line) for every line under lisp/ added since BASE."""
    diff = subprocess.run(
        ["git", "diff", "-U0", base, "--", "lisp/"],
        cwd=ROOT, capture_output=True, text=True, check=True).stdout
    path = None
    for line in diff.splitlines():
        if line.startswith("+++ "):
            path = line[6:] if line.startswith("+++ b/") else None
            continue
        m = HUNK.match(line)
        if m and path:
            start, count = int(m.group(1)), int(m.group(2) or 1)
            for n in range(start, start + count):
                yield path, n


def main():
    base = sys.argv[1] if len(sys.argv) > 1 else "origin/main"
    if not REPORT.exists():
        print(f"{REPORT.relative_to(ROOT)} not found; run "
              'eldev test -u "on,codecov,dontsend" -U coverage/coverage.json',
              file=sys.stderr)
        return 2
    files = {f["name"]: f["coverage"]
             for f in json.loads(REPORT.read_text())["source_files"]}
    total = hit = 0
    for path, n in added_lines(base):
        hits = next((v for k, v in files.items() if k.endswith(path)), None)
        if hits is None or n - 1 >= len(hits) or hits[n - 1] is None:
            continue            # not code: a comment, a blank, a docstring line
        total += 1
        if hits[n - 1] > 0:
            hit += 1
        else:
            print(path, n)
    print(f"patch coverage {hit}/{total}")
    return 0 if hit == total else 1


if __name__ == "__main__":
    sys.exit(main())
