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
import signal
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
                line_start = text.rfind("\n", 0, s) + 1
                line_end = text.find("\n", s)
                line_end = len(text) if line_end == -1 else line_end
                sites.append({
                    "file": path, "rel": os.path.relpath(path, ROOT),
                    "start": s, "end": e,
                    "lineno": text.count("\n", 0, s),
                    "op": name, "repl": repl, "orig": text[s:e],
                    "line": " ".join(text[line_start:line_end].split()),
                })
    return sites


def parse_failed(output):
    m = re.search(r"Ran \d+ (?:out of \d+ )?specs?, (\d+) failed", output)
    return int(m.group(1)) if m else None


def warm_cache(root):
    """Byte-compile ROOT once so its .elc cache is fresh.  Worker copies
    (made with mtime-preserving copytree) then inherit a warm cache and skip
    the expensive full recompile on their first test run; each mutation only
    recompiles the single file it touched."""
    subprocess.run(["eldev", "compile"], cwd=root, capture_output=True)


def run_suite(cwd, pattern, timeout):
    """Run the suite in CWD, returning (status, failed-count).

    The child is started in its own process group so a timeout can kill
    the whole tree.  `eldev' is a shell wrapper that spawns
    `emacs --batch'; killing only the wrapper leaves the Emacs behind as
    a CPU-bound orphan that never exits.  Those accumulate — one per
    timed-out mutation — until they saturate the machine, which makes
    further suites time out, which leaks more orphans.  Reap the group.
    """
    cmd = ["eldev", "test"]
    if pattern:
        cmd.append(pattern)
    proc = subprocess.Popen(cmd, cwd=cwd, stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, text=True,
                            start_new_session=True)
    try:
        out, err = proc.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        except (ProcessLookupError, PermissionError):
            proc.kill()
        proc.communicate()
        return ("timeout", None)
    failed = parse_failed(out + err)
    if failed is None:
        return ("error", None)
    return (("survived" if failed == 0 else "killed"), failed)


def label_of(site):
    return (f"{site['rel']}:{site['lineno'] + 1} "
            f"[{site['op']}: {site['orig']}->{site['repl']}]")


def key_of(site):
    """Return a line-number-independent identity for a mutation site.

    The baseline file is keyed on this rather than on `label_of', whose
    line number shifts whenever anything above it in the file changes —
    a baseline keyed that way would go stale on every unrelated edit.
    Identical lines carrying the same operator share a key, so accepting
    one accepts all of them; that is the intended trade for stability.
    """
    return f"{site['rel']} :: {site['op']} :: {site['line']}"


def parse_baseline(path):
    """Read accepted-survivor keys from PATH.

    Blank lines and `#' comments are ignored, so each entry can carry the
    reason it was accepted on the line above it.  A missing file means an
    empty baseline: every survivor is then new.
    """
    if not path or not os.path.exists(path):
        return []
    keys = []
    with open(path, encoding="utf-8") as fh:
        for raw in fh:
            line = raw.strip()
            if line and not line.startswith("#"):
                keys.append(line)
    return keys


def execute(sites, root, pattern, timeout, quiet=False):
    """Run each site's mutation against the suite rooted at ROOT.
    Files are snapshot and restored.  Returns a results dict."""
    files = sorted({s["file"] for s in sites})
    originals = {p: open(p, encoding="utf-8").read() for p in files}
    killed = survived = errored = 0
    survivors = []
    timed_out = []
    survivor_keys = []
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
                survivor_keys.append(key_of(site))
            elif status == "error":
                errored += 1
            elif status == "timeout":
                timed_out.append(label_of(site))
            else:
                killed += 1
            if not quiet:
                mark = {"killed": "killed ", "survived": "SURVIVED",
                        "error": "error  ", "timeout": "TIMEOUT"}[status]
                print(f"[{n}/{len(sites)}] {mark}  {label_of(site)}", flush=True)
    finally:
        for p, text in originals.items():
            with open(p, "w", encoding="utf-8") as fh:
                fh.write(text)
    return {"killed": killed, "survived": survived, "errored": errored,
            "timeouts": len(timed_out), "timed_out": timed_out,
            "survivors": survivors, "survivor_keys": survivor_keys}


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
        proc = subprocess.run(cmd, cwd=workdir, capture_output=True, text=True)
        if not os.path.exists(result_file):
            # Surface the worker's own error.  Without this the driver dies
            # on a bare FileNotFoundError for mut-result.json, which says
            # nothing about why the worker failed and discards the whole run.
            tail = (proc.stderr or proc.stdout or "").strip().splitlines()[-15:]
            raise RuntimeError(
                f"worker {idx} produced no result (exit {proc.returncode}):\n"
                + "\n".join(tail))
        with open(result_file) as fh:
            res = json.load(fh)
    finally:
        shutil.rmtree(dest, ignore_errors=True)
    return res


def shard_sites(sites, jobs):
    """Round-robin sites into JOBS balanced groups."""
    groups = [[] for _ in range(jobs)]
    for i, s in enumerate(sites):
        # Strip non-serializable / absolute fields; workers resolve via rel.
        # `line' must survive the trip: workers build the baseline key from
        # it, and dropping it makes `key_of' raise inside the worker the
        # first time a mutant survives.
        groups[i % jobs].append({k: s[k] for k in
                                 ("rel", "start", "end", "lineno", "op",
                                  "repl", "orig", "line")})
    return [g for g in groups if g]


def run_parallel(sites, jobs, timeout, pattern):
    groups = shard_sites(sites, jobs)
    print(f"Running {len(sites)} mutations across {len(groups)} parallel "
          f"workers (isolated copies)...\n", flush=True)
    merged = {"killed": 0, "survived": 0, "errored": 0, "timeouts": 0,
              "timed_out": [], "survivors": [], "survivor_keys": []}
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(groups)) as ex:
        futs = {ex.submit(run_one_worker, i, g, timeout, pattern): i
                for i, g in enumerate(groups)}
        for fut in concurrent.futures.as_completed(futs):
            res = fut.result()
            for k in ("killed", "survived", "errored", "timeouts"):
                merged[k] += res.get(k, 0)
            merged["survivors"] += res["survivors"]
            merged["survivor_keys"] += res.get("survivor_keys", [])
            merged["timed_out"] += res.get("timed_out", [])
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
    if result.get("timeouts"):
        print(f"\n{result['timeouts']} mutation(s) timed out, excluded from "
              f"the score (neither killed nor survived):")
        for label in sorted(result.get("timed_out", [])):
            print(f"  - {label}")
        print("\n  A timeout is usually a NON-TERMINATING mutant rather than a "
              "slow machine —\n"
              "  flipping a loop guard (and->or, 1+->1-, lt->le) can make the "
              "loop spin\n"
              "  forever.  Check whether each site above is a `while' guard: "
              "if so the\n"
              "  mutant is effectively caught, since it would hang CI too, and "
              "it is NOT a\n"
              "  survivor — it must not appear in the baseline.  If instead the "
              "sites look\n"
              "  unrelated to loops, the machine was loaded; re-run when idle "
              "(check for\n"
              "  stray `emacs --batch' first) before trusting this run.")
    if result["survivors"]:
        print("\nSurviving mutants (no test caught these — review for weak "
              "assertions or equivalent mutants):")
        for s in sorted(result["survivors"]):
            print(f"  - {s}")


BASELINE_HEADER = """\
# Accepted mutation survivors.
#
# Each entry is a mutation the suite does not catch and that we have
# decided not to chase — almost always because the mutant is equivalent
# (the mutated code behaves identically, so no test could tell) or
# because it is masked by a surrounding guard.  Put the reason on a
# comment line above the entry.
#
# Anything that survives and is NOT listed here is a regression: a line
# whose behavior you could change with nothing to stop you.  That is
# what `--baseline' fails on.  The score itself is not the target; a
# clean diff against this file is.
#
# Format: FILE :: OPERATOR :: SOURCE LINE   (line numbers deliberately
# excluded so unrelated edits above a site do not invalidate the entry)
#
# Regenerate with:
#   python3 test/mutation/mutate.py --files <files> --jobs 8 --max 0 \\
#       --write-baseline test/mutation/accepted-survivors.txt
"""


def write_baseline(path, result):
    """Write this run's survivors to PATH as an accepted-survivors file."""
    keys = sorted(set(result.get("survivor_keys", [])))
    os.makedirs(os.path.dirname(os.path.abspath(path)), exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(BASELINE_HEADER)
        for key in keys:
            fh.write("\n" + key + "\n")


def check_baseline(path, result, sites):
    """Compare this run's survivors against the accepted list at PATH.

    Returns a process exit status: non-zero when a survivor is not in the
    baseline.  Entries in the baseline that no longer survive are
    reported too — they are not failures, but leaving them in place would
    let a real regression hide behind a stale accept.
    """
    accepted = set(parse_baseline(path))
    current = set(result.get("survivor_keys", []))
    scoped = {key_of(s) for s in sites}
    new = sorted(current - accepted)
    # Only consider accepted entries that this run actually covered;
    # a partial run must not report every out-of-scope entry as stale.
    stale = sorted((accepted & scoped) - current)

    print("\n" + "=" * 60)
    print(f"Baseline: {len(accepted)} accepted, {len(current)} surviving now.")
    if stale:
        print(f"\n{len(stale)} baseline entr(y/ies) no longer survive — "
              f"a test now covers them.  Remove from {path}:")
        for key in stale:
            print(f"  - {key}")
    if new:
        print(f"\n{len(new)} NEW survivor(s) — behavior you could change "
              f"with no test objecting:")
        for key in new:
            print(f"  - {key}")
        print("\nEither add an assertion that kills it, or record it in "
              f"{path} with the reason it is acceptable.")
        return 1
    print("\nNo new survivors.")
    return 0


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
    ap.add_argument("--baseline", default=None,
                    help="file of accepted survivors; fail only on NEW ones")
    ap.add_argument("--write-baseline", default=None,
                    help="write this run's survivors to a baseline file")
    ap.add_argument("--min-score", type=float, default=None,
                    help="exit non-zero if mutation score (%% killed) is below "
                         "this floor; for CI ratcheting (default: fail on any "
                         "survivor)")
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

    print("Warming bytecode cache (eldev compile)...", flush=True)
    warm_cache(ROOT)

    if args.jobs > 1:
        result = run_parallel(sites, args.jobs, args.timeout, args.pattern)
    else:
        result = execute(sites, ROOT, args.pattern, args.timeout)

    report(result, int(time.time() - start))
    if args.json_out:
        with open(args.json_out, "w") as fh:
            json.dump(result, fh)
    if args.write_baseline:
        write_baseline(args.write_baseline, result)
        print(f"\nWrote {len(result.get('survivor_keys', []))} survivor key(s) "
              f"to {args.write_baseline}")
        sys.exit(0)

    if args.baseline:
        sys.exit(check_baseline(args.baseline, result, sites))

    if args.min_score is not None:
        scored = result["killed"] + result["survived"]
        score = (result["killed"] / scored * 100) if scored else 100.0
        if score < args.min_score:
            print(f"\nMutation score {score:.1f}% is below floor "
                  f"{args.min_score:.1f}% — assertion depth regressed.")
            sys.exit(1)
        sys.exit(0)
    sys.exit(1 if result["survivors"] else 0)


if __name__ == "__main__":
    main()
