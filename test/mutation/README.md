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

## Read the survivor list, not the score

The percentage is a weak instrument and should be treated as directional only:

- **It is unstable under sampling.** The default bounded sample scored 83.3% on
  `files.el` + `sections.el`; a full pass over the same two files at the same
  commit scored 57.4%. A 26-point spread from sampling alone. Never quote a
  sampled score as a file's depth.
- **It has a floor it cannot cross.** 48 of those 83 survivors were `t`/`nil`
  flag flips in argument positions like `(org-get-heading t t t t)`, where the
  mutated code behaves identically. Killing every *real* survivor lands near
  75%, not 100%.
- **The denominator moves with the code.** Adding a function adds sites, so the
  score shifts without any test getting better or worse.
- **It cannot tell a deliberate kill from an incidental one.** Two counter
  tests written for issue #38 asserted `"2 files"` against a message that reads
  `"-2 files"` when the counter is negated — the regex matched, the spec
  passed, the mutant lived. An aggregate would have moved either way.

So the gate is **`accepted-survivors.txt`**, not a number: every survivor is
either killed or recorded there with a reason, and CI fails on anything new.

## Accepted survivors

`accepted-survivors.txt` lists mutants we have decided not to chase — usually
equivalent mutants, or ones masked by a surrounding guard. Entries are keyed
`FILE :: OPERATOR :: SOURCE LINE`, deliberately without line numbers so an
unrelated edit above a site does not invalidate the entry.

> **The file is not populated yet.** The tooling and the tests that kill the
> real survivors are in place, but generating it needs one clean full pass —
> see the TODO at the bottom of `accepted-survivors.txt` for the exact command.
> Until then `--baseline` will report every survivor as new.

```bash
# Fail only on survivors that are NOT in the accepted list
python3 test/mutation/mutate.py --files lisp/org-canvas-files.el --jobs 8 \
    --max 0 --baseline test/mutation/accepted-survivors.txt

# Regenerate after deliberately accepting new ones (then annotate reasons)
python3 test/mutation/mutate.py --files lisp/org-canvas-files.el --jobs 8 \
    --max 0 --write-baseline test/mutation/accepted-survivors.txt
```

`--baseline` also reports accepted entries that *no longer* survive, so a
stale accept cannot hide a later regression behind it. Those are reported, not
failed — a partial run only considers entries whose sites it actually covered.

**Verify a kill per-mutant, not by watching the score move.** Apply the exact
mutation by hand, confirm the exact spec fails, restore. That is a causal claim
about one line and one test; the aggregate is not.

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
