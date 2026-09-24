#!/usr/bin/env bash
# Print the test files of one CI shard, one per line.
#
#   scripts/test-shard.sh K/N     files of shard K (1-based) out of N
#   scripts/test-shard.sh all     every test file
#
# Every test file lands in exactly one shard, so the N shards together
# run the whole suite; test/shard-weights.txt (seconds per file, a file
# missing there counts as 3) only balances them.  Files are placed
# heaviest first on the lightest shard, which is deterministic: the
# same tree gives the same split on every runner.
set -euo pipefail

cd "$(dirname "$0")/.."

files() {
  # test/org-canvas-test.el does not match the *-test.el glob but is a
  # test file like the others.
  ls test/org-canvas-test.el test/org-canvas-*-test.el
}

spec=${1:-}
case "$spec" in
  all) files; exit 0 ;;
  */*) k=${spec%/*}; n=${spec#*/} ;;
  *) echo "usage: $0 K/N | all" >&2; exit 2 ;;
esac
if ! [[ "$k" =~ ^[0-9]+$ && "$n" =~ ^[0-9]+$ ]] || [ "$k" -lt 1 ] || [ "$k" -gt "$n" ]; then
  echo "$0: shard must be K/N with 1 <= K <= N, not '$spec'" >&2
  exit 2
fi

files | awk -v k="$k" -v n="$n" '
  FNR == NR { if ($0 !~ /^#/ && NF == 2) weight[$2] = $1; next }
  { f[++count] = $0; w[count] = ($0 in weight) ? weight[$0] : 3 }
  END {
    # Heaviest first; ties by name, so the order never depends on ls.
    for (i = 1; i <= count; i++) order[i] = i
    for (i = 2; i <= count; i++) {
      j = i
      while (j > 1 && (w[order[j]] > w[order[j-1]] ||
                       (w[order[j]] == w[order[j-1]] && f[order[j]] < f[order[j-1]]))) {
        t = order[j]; order[j] = order[j-1]; order[j-1] = t; j--
      }
    }
    for (s = 1; s <= n; s++) load[s] = 0
    for (i = 1; i <= count; i++) {
      best = 1
      for (s = 2; s <= n; s++) if (load[s] < load[best]) best = s
      load[best] += w[order[i]]
      shard[order[i]] = best
    }
    for (i = 1; i <= count; i++) if (shard[i] == k) print f[i]
  }' test/shard-weights.txt -
