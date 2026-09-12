#!/usr/bin/env bash
# Run every test file on its own and fail if any of them fails alone.
#
# `eldev test "pattern"` is how a developer runs a subset, and the full
# suite hides what a file needs from the files loaded before it: 18 of
# the test files failed in isolation on 2026-09-12 while the suite was
# green — modules another file had required, a logger a spec had
# silenced, a temp file another spec had left behind (issue #260).
# CI runs this on Emacs 30.1; it takes about two minutes.
set -uo pipefail

cd "$(dirname "$0")/.."

failed=()
for file in test/org-canvas-*-test.el; do
  summary=$(eldev test "$file" 2>&1 | grep -E '^Ran [0-9]+' || true)
  case "$summary" in
    *" 0 failed"*) printf '%-45s %s\n' "$file" "$summary" ;;
    *)
      printf '%-45s %s\n' "$file" "${summary:-did not run (load error?)}"
      failed+=("$file")
      ;;
  esac
done

if [ "${#failed[@]}" -gt 0 ]; then
  echo
  echo "These test files fail when run alone (eldev test <file>):"
  printf '  %s\n' "${failed[@]}"
  exit 1
fi
echo
echo "Every test file passes on its own."
