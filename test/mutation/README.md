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

The file is populated from clean full passes and every entry carries the reason
it is accepted. Nine files are covered, in three groups:

| Files | Sites | Accepted entries |
|-------|-------|------------------|
| `files.el`, `sections.el` | 427 | 36 |
| `validate.el`, `validate-issue.el`, `validate-content.el`, `validate-grading.el` | 351 | 67 |
| `core-sync.el`, `core-push.el`, `core-heading.el` | 285 | 51 |

The last two rows are issue #575's triage: 636 sites, 433 killed by the existing
suite, 197 surviving under 119 distinct keys (identical source lines share a key
by design). Two survivors were argued about rather than waved through: the
dry-run sentinel's `equal`/`eq` was killed by a new spec, and the `PAREN`
argument of the raw-HTML `regexp-opt` was accepted as an equivalence after
checking that PAREN only chooses capturing vs shy for the outer group.

Two further entries, for `drift.el` and `quiz-accommodations.el`, came from an
earlier ad-hoc triage; no shard runs those files yet, so nothing checks them
and adding a file to a shard is what would.

Extending mutation coverage to another file means running a full pass scoped to
it, deciding on each survivor, and adding the file to a CI shard (see below).
**Say which kind of entry you are writing**, because the two call for different
follow-up:

- *Equivalent* — the mutated code behaves identically (a flag on
  `(org-get-heading t t t t)`, a `condition-case` variable that is never read).
  Nothing to do, ever.
- *Coverage gap* — the mutant is genuinely different and no test notices.
  Accepted for now, but it is a debt: when a mutant this file accepts turns out
  to matter, the entry is the first place to look. The #575 entries that are
  gaps cluster in three places: findings whose `property`/`pending-sync` column
  no fixture asserts, guards for degenerate input (an empty table cell, a
  missing file, a twin with no id) no fixture builds, and boundary comparisons
  that only differ on a value no fixture uses.

Triage a **key**, not a line: `key_of` collapses every site whose source line is
byte-identical, so accepting one accepts all of them, and that is the intended
trade for stability under edits.

```bash
# Fail only on survivors that are NOT in the accepted list
python3 test/mutation/mutate.py --files lisp/org-canvas-files.el --jobs 8 \
    --max 0 --baseline test/mutation/accepted-survivors.txt

# Regenerate after deliberately accepting new ones (then annotate reasons)
python3 test/mutation/mutate.py --files lisp/org-canvas-files.el --jobs 8 \
    --max 0 --write-baseline test/mutation/accepted-survivors.txt
```

`--write-baseline` rewrites the whole file from the run and **drops every
comment**, including the reasons. To add entries to the existing file, append
them instead, taking each key from a `--json` run so it is byte-exact:

```bash
python3 test/mutation/mutate.py --files lisp/org-canvas-calendar.el --jobs 8 \
    --max 0 --json /tmp/mut.json
# append  "# <reason>" lines followed by  "FILE :: OPERATOR :: LINE"  entries
```

`--baseline` also reports accepted entries that *no longer* survive, so a
stale accept cannot hide a later regression behind it. Those are reported, not
failed — a partial run only considers entries whose sites it actually covered.

**Verify a kill per-mutant, not by watching the score move.** Apply the exact
mutation by hand, confirm the exact spec fails, restore. That is a causal claim
about one line and one test; the aggregate is not.

Two scoring hazards worth knowing:

- **Any unrelated spec failure during a mutant's run scores it killed.** The
  harness only sees the failure count, not which spec failed, so one
  spontaneous flake anywhere in a full pass gets misattributed as a kill.
  A mutant that a full pass reports killed but survives repeated targeted
  runs is this hazard, not a contradiction in your tests. De-flaking the
  suite (issue #43) is the real fix.
- **Timeouts are non-terminating mutants, not slow machines** — flipping a
  `while` guard (`and->or`, `1+->1-`) removes the exit condition. They are
  excluded from the score and listed individually; they are effectively
  caught (they would hang CI too) and must not enter the baseline.

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

The `mutation` job in `.github/workflows/ci.yml` runs weekly (and on manual
dispatch) and never blocks PRs. It is a **matrix of four shards**, each doing a
full pass over a disjoint, balanced set of files with
`--baseline test/mutation/accepted-survivors.txt`:

| Shard | Files | Sites |
|-------|-------|-------|
| `files` | `files.el` | 203 |
| `sections` | `sections.el` | 224 |
| `validate` | the four validate modules | 351 |
| `sync-push-heading` | `core-sync.el`, `core-push.el`, `core-heading.el` | 285 |

Each shard fails only on a survivor absent from the accepted list; stale entries
are reported, not failed. The pass is sharded because ~1,060 mutations at ~20s
each do not fit one runner's 90-minute budget at `--jobs 3`, and sharding keeps
a single shard's flake from hiding the other three. `--baseline` only treats an
entry as stale when the run covered its site, so shards do not accuse each other
of leaving entries behind.

There is no score floor any more: a sampled score was never a measurement (see
above), and every file the job runs now has a triaged baseline.

## Workflow

1. Run on a file or subsystem.
2. For each survivor, decide: weak assertion (add/strengthen a test until it is
   killed) or equivalent mutant (a change with no observable effect — dismiss).
3. Re-run to confirm new tests kill the mutant.

Example: a run on `org-canvas-calendar.el` surfaced `:required t -> nil` at the
`START_AT` registry property as a survivor — nothing checked that a required
field is still emitted when unset. Adding the "always emits start_at even when
unset" test killed it (100%).
