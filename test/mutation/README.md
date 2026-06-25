# Mutation testing

`mutate.py` measures the **depth** of the test suite — whether its assertions
actually catch behavior changes — rather than line coverage, which only shows
code was executed.

It applies one small, syntax-safe change at a time to the Elisp sources, runs
the suite, and classifies the result:

- **killed** — a test failed, so the suite caught the change (good).
- **survived** — all tests still passed. Either a weak/missing assertion, or a
  semantically-equivalent mutant a human should dismiss.

`mutation score = killed / (killed + survived)`. Surviving mutants are the
actionable output: lines where you could break behavior unnoticed.

## Usage

```bash
# Bounded sample across all of lisp/ (12 mutations)
python3 test/mutation/mutate.py

# Focus one file, run more, give each run more time
python3 test/mutation/mutate.py --files lisp/org-canvas-calendar.el --max 0 --timeout 90

# Scope the tests for speed during development (less precise — a mutation may
# be caught only by a test outside the pattern)
python3 test/mutation/mutate.py --files lisp/org-canvas-pages.el --pattern pages
```

Each mutation runs the whole suite (~20s), so this is **on-demand** tooling,
not a per-commit gate. Files are always restored, even on Ctrl-C. Exit status
is non-zero if any mutant survived.

Before running, the harness byte-compiles the repo once (`eldev compile`) so
parallel worker copies inherit a warm `.elc` cache and skip the full recompile
on their first test run — each mutation then only recompiles the one file it
touched.

## Mutation operators

Token-boundary regexes (never match inside a longer symbol, string, comment,
or `?x` char literal): `t`↔`nil`, `>`→`>=`, `<`→`<=`, `equal`→`eq`,
`(1+`→`(1-`, `and`↔`or`.

## CI ratchet

The `mutation` job in `.github/workflows/ci.yml` runs a sampled mutation pass
weekly (and on manual dispatch) over the validation engine and core-sync with
`--min-score 40`, failing if the score drops below the floor. `--min-score N`
exits non-zero when the score (% killed) is under N, so assertion depth can't
silently regress as the suite grows. It never blocks PRs.

## Workflow

1. Run on a file or subsystem.
2. For each survivor, decide: weak assertion (add/strengthen a test until it is
   killed) or equivalent mutant (a change with no observable effect — dismiss).
3. Re-run to confirm new tests kill the mutant.

Example: a run on `org-canvas-calendar.el` surfaced `:required t -> nil` at the
`START_AT` registry property as a survivor — nothing checked that a required
field is still emitted when unset. Adding the "always emits start_at even when
unset" test killed it (100%).
