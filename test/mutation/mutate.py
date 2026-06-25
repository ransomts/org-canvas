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
    python3 test/mutation/mutate.py                      # bounded sample, serial
    python3 test/mutation/mutate.py --jobs 6             # parallel (isolated copies)
    python3 test/mutation/mutate.py --files lisp/org-canvas-core-org.el --max 0
    python3 test/mutation/mutate.py --pattern core       # scope tests (faster, less precise)

Each mutation runs the suite (~20s).  With --jobs N, N isolated copies of the
repo run in parallel (each with its own .eldev build cache), giving ~N x
throughput.  Files are always restored; exit status is non-zero if any mutant
survived.
"""
import argparse
import concurrent.futures
import glob
import json
import os
import random
import re
import shutil
import subprocess
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))

# Each operator: (name, compiled-regex, replacement).  Regexes use lookarounds
# so they only match whole Lisp tokens — never a substring of a longer symbol,
# keyword, or string — which keeps every mutation syntactically valid.
OPERATORS = [
    ("t->nil", re.compile(r"(?<![\w:&-])t(?![\w-])"), "nil"),
    ("nil->t", re.compile(r"(?<![\w:&-])nil(?![\w-])"), "t"),
    ("gt->ge", re.compile(r"(?<![\w<>=/-])>(?![\w<>=-])"), ">="),
    ("lt->le", re.compile(r"(?<![\w<>=/-])<(?![\w<>=-])"), "<="),
    ("equal->eq", re.compile(r"\(equal(?=\s)"), "(eq"),
    ("1+->1-", re.compile(r"\(1\+(?=\s)"), "(1-"),
    ("and->or", re.compile(r"\(and(?=\s)"), "(or"),
    ("or->and", re.compile(r"\(or(?=\s)"), "(and"),
]


def code_mask(text):
    """Return a per-char bool list: True where TEXT is code, False inside a
    string literal, a `;' comment, or a `?' char literal.  Mutating those
    regions only produces equivalent mutants (docstring/comment prose)."""
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
        elif c == "?":
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
    """Return mutation sites (dicts) found in code regions only.
    `file' is an absolute path; `rel' is relative to ROOT (for sharding)."""
    sites = []
    for path in files:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
        mask = code_mask(text)
        for name, rx, repl in OPERATORS:
            for m in rx.finditer(text):
                s, e = m.start(), m.end()
                if not all(mask[s:e]):
                    continue
                sites.append({
                    "file": path, "rel": os.path.relpath(path, ROOT),
                    "start": s, "end": e,
                    "lineno": text.count("\n", 0, s),
                    "op": name, "repl": repl, "orig": text[s:e],
                })
    return sites


def parse_failed(output):
    m = re.search(r"Ran \d+ (?:out of \d+ )?specs?, (\d+) failed", output)
    return int(m.group(1)) if m else None


def run_suite(cwd, pattern, timeout):
    cmd = ["eldev", "test"]
    if pattern:
        cmd.append(pattern)
    try:
        proc = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True,
                              timeout=timeout)
    except subprocess.TimeoutExpired:
        return ("timeout", None)
    failed = parse_failed(proc.stdout + proc.stderr)
    if failed is None:
        return ("error", None)
    return (("survived" if failed == 0 else "killed"), failed)


def label_of(site):
    return (f"{site['rel']}:{site['lineno'] + 1} "
            f"[{site['op']}: {site['orig']}->{site['repl']}]")


def execute(sites, root, pattern, timeout, quiet=False):
    """Run each site's mutation against the suite rooted at ROOT.
    Files are snapshot and restored.  Returns a results dict."""
    files = sorted({s["file"] for s in sites})
    originals = {p: open(p, encoding="utf-8").read() for p in files}
    killed = survived = errored = 0
    survivors = []
    try:
        for n, site in enumerate(sites, 1):
            text = originals[site["file"]]
            mutated = text[:site["start"]] + site["repl"] + text[site["end"]:]
            with open(site["file"], "w", encoding="utf-8") as fh:
                fh.write(mutated)
            try:
                status, _ = run_suite(root, pattern, timeout)
            finally:
                with open(site["file"], "w", encoding="utf-8") as fh:
                    fh.write(text)
            if status == "survived":
                survived += 1
                survivors.append(label_of(site))
            elif status == "error":
                errored += 1
            else:
                killed += 1
            if not quiet:
                mark = {"killed": "killed ", "survived": "SURVIVED",
                        "error": "error  ", "timeout": "killed*"}[status]
                print(f"[{n}/{len(sites)}] {mark}  {label_of(site)}", flush=True)
    finally:
        for p, text in originals.items():
            with open(p, "w", encoding="utf-8") as fh:
                fh.write(text)
    return {"killed": killed, "survived": survived, "errored": errored,
            "survivors": survivors}


def run_one_worker(idx, shard, timeout, pattern):
    """Copy the repo to a temp dir and run mutate.py on SHARD there."""
    dest = tempfile.mkdtemp(prefix=f"mut-w{idx}-")
    workdir = os.path.join(dest, "repo")
    shutil.copytree(ROOT, workdir,
                    ignore=shutil.ignore_patterns(".git", "coverage"))
    sites_file = os.path.join(workdir, "mut-sites.json")
    result_file = os.path.join(workdir, "mut-result.json")
    with open(sites_file, "w") as fh:
        json.dump(shard, fh)
    cmd = [sys.executable, os.path.join(workdir, "test/mutation/mutate.py"),
           "--sites-file", sites_file, "--json", result_file,
           "--timeout", str(timeout)]
    if pattern:
        cmd += ["--pattern", pattern]
    try:
        subprocess.run(cmd, cwd=workdir, capture_output=True, text=True)
        with open(result_file) as fh:
            res = json.load(fh)
    finally:
        shutil.rmtree(dest, ignore_errors=True)
    return res


def shard_sites(sites, jobs):
    """Round-robin sites into JOBS balanced groups."""
    groups = [[] for _ in range(jobs)]
    for i, s in enumerate(sites):
        # strip non-serializable / absolute fields; workers resolve via rel
        groups[i % jobs].append({k: s[k] for k in
                                 ("rel", "start", "end", "lineno", "op",
                                  "repl", "orig")})
    return [g for g in groups if g]


def run_parallel(sites, jobs, timeout, pattern):
    groups = shard_sites(sites, jobs)
    print(f"Running {len(sites)} mutations across {len(groups)} parallel "
          f"workers (isolated copies)...\n", flush=True)
    merged = {"killed": 0, "survived": 0, "errored": 0, "survivors": []}
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(groups)) as ex:
        futs = {ex.submit(run_one_worker, i, g, timeout, pattern): i
                for i, g in enumerate(groups)}
        for fut in concurrent.futures.as_completed(futs):
            res = fut.result()
            for k in ("killed", "survived", "errored"):
                merged[k] += res[k]
            merged["survivors"] += res["survivors"]
            done = merged["killed"] + merged["survived"] + merged["errored"]
            print(f"  worker {futs[fut]} done "
                  f"({res['killed']}k/{res['survived']}s/{res['errored']}e) "
                  f"— {done}/{len(sites)} total", flush=True)
    return merged


def report(result, elapsed):
    killed, survived = result["killed"], result["survived"]
    scored = killed + survived
    score = (killed / scored * 100) if scored else 0.0
    print("\n" + "=" * 60)
    print(f"Mutation score: {score:.1f}%  ({killed} killed / {scored} scored)")
    print(f"  survived: {survived}   killed: {killed}   "
          f"compile-errors: {result['errored']}   ({elapsed}s)")
    if result["survivors"]:
        print("\nSurviving mutants (no test caught these — review for weak "
              "assertions or equivalent mutants):")
        for s in sorted(result["survivors"]):
            print(f"  - {s}")


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--files", nargs="*", help="Elisp files (default: all lisp/*.el)")
    ap.add_argument("--max", type=int, default=12,
                    help="max mutations (sampled); 0 = all (default 12)")
    ap.add_argument("--jobs", type=int, default=1,
                    help="parallel workers, each an isolated repo copy (default 1)")
    ap.add_argument("--timeout", type=int, default=120,
                    help="per-run test timeout seconds (default 120)")
    ap.add_argument("--pattern", default=None, help="eldev test pattern (faster, less precise)")
    ap.add_argument("--seed", type=int, default=1, help="sampling seed (default 1)")
    ap.add_argument("--sites-file", help="(worker) JSON list of sites to run")
    ap.add_argument("--json", dest="json_out", help="write results JSON to this path")
    args = ap.parse_args()

    start = time.time()

    # Worker mode: run an explicit site list rooted at this copy.
    if args.sites_file:
        with open(args.sites_file) as fh:
            shard = json.load(fh)
        for s in shard:
            s["file"] = os.path.join(ROOT, s["rel"])
        result = execute(shard, ROOT, args.pattern, args.timeout, quiet=True)
        if args.json_out:
            with open(args.json_out, "w") as fh:
                json.dump(result, fh)
        return

    files = args.files or sorted(glob.glob(os.path.join(ROOT, "lisp", "*.el")))
    files = [os.path.abspath(f) for f in files]
    sites = discover_sites(files)
    total = len(sites)
    random.seed(args.seed)
    random.shuffle(sites)
    skipped = 0
    if args.max and len(sites) > args.max:
        skipped = len(sites) - args.max
        sites = sites[:args.max]

    print(f"Discovered {total} mutation sites across {len(files)} file(s).")
    if skipped:
        print(f"Running a sample of {len(sites)} (seed={args.seed}); "
              f"{skipped} not run this pass. Use --max 0 to run all.")
    print(f"Test command: eldev test{(' ' + args.pattern) if args.pattern else ''}"
          f"  (timeout {args.timeout}s each)\n", flush=True)

    if args.jobs > 1:
        result = run_parallel(sites, args.jobs, args.timeout, args.pattern)
    else:
        result = execute(sites, ROOT, args.pattern, args.timeout)

    report(result, int(time.time() - start))
    if args.json_out:
        with open(args.json_out, "w") as fh:
            json.dump(result, fh)
    sys.exit(1 if result["survivors"] else 0)


if __name__ == "__main__":
    main()
