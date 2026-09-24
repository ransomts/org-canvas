#!/usr/bin/env bash
# Run every test file on its own and fail if any of them fails alone.
#
#   scripts/test-each-file.sh               every test file
#   scripts/test-each-file.sh --shard K/N   one CI shard's files (scripts/test-shard.sh)
#   scripts/test-each-file.sh --timings     print "SECONDS FILE" per file, for
#                                           test/shard-weights.txt
#
# `eldev test "pattern"` is how a developer runs a subset, and the full
# suite hides what a file needs from the files loaded before it: 18 of
# the test files failed in isolation on 2026-09-12 while the suite was
# green — modules another file had required, a logger a spec had
# silenced, a temp file another spec had left behind (issue #260).
# CI runs this as the isolation job, split in three shards on Emacs 30.1.
set -uo pipefail

cd "$(dirname "$0")/.." || exit 2

shard=all
timings=
while [ $# -gt 0 ]; do
  case "$1" in
    --shard) shard=$2; shift 2 ;;
    --timings) timings=1; shift ;;
    *) echo "usage: $0 [--shard K/N] [--timings]" >&2; exit 2 ;;
  esac
done

# Not `mapfile < <(...)`: a failure inside a process substitution is
# lost, and a mistyped shard would pass having run nothing.
list=$(scripts/test-shard.sh "$shard") || exit 2
if [ -z "$list" ]; then
  echo "No test files in shard $shard."
  exit 0
fi
mapfile -t files <<<"$list"

failed=()
for file in "${files[@]}"; do
  start=$(date +%s.%N)
  summary=$(eldev test "$file" 2>&1 | grep -E '^Ran [0-9]+' || true)
  if [ -n "$timings" ]; then
    awk -v s="$start" -v e="$(date +%s.%N)" -v f="$file" 'BEGIN { printf "%.1f %s\n", e - s, f }'
  fi
  case "$summary" in
    *" 0 failed"*) [ -n "$timings" ] || printf '%-45s %s\n' "$file" "$summary" ;;
    *)
      printf '%-45s %s\n' "$file" "${summary:-did not run (load error?)}" >&2
      failed+=("$file")
      ;;
  esac
done

if [ "${#failed[@]}" -gt 0 ]; then
  echo >&2
  echo "These test files fail when run alone (eldev test <file>):" >&2
  printf '  %s\n' "${failed[@]}" >&2
  exit 1
fi
[ -n "$timings" ] || { echo; echo "Every test file passes on its own (${#files[@]} files, shard $shard)."; }
