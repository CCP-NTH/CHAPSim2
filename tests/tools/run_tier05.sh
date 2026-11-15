#!/usr/bin/env bash
# Tier 0.5 the way CI runs it: every tests/tools/check_*.py (except the comparator
# check_metrics.py) in a CLEAN export of the repository, not in the working tree.
#
# Why: a working tree that has run the test suite holds untracked solver output a
# CI runner never sees, so a checker can pass locally and fail in CI (and the
# converse: a checker that needs a file nobody tracked).
#
#   tests/tools/run_tier05.sh          export = tracked files as they are in the working
#                                      tree (uncommitted edits included) + untracked,
#                                      non-ignored files outside docs/superpowers/
#   tests/tools/run_tier05.sh --head   export = the committed tree (git archive HEAD)
#
# The known-failing list is read from KNOWN_FAILING_TABLE in .github/workflows/ci.yml,
# so this script and CI cannot disagree about it. Exit status follows CI: a new failure
# or a known-failing checker that now passes is an error.
set -euo pipefail

REPO="$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel)"
MODE="${1:-worktree}"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/tier05_XXXXXX")"
trap '/bin/rm -rf "$WORK"' EXIT

cd "$REPO"
if [ "$MODE" = "--head" ]; then
  git archive HEAD | tar -x -C "$WORK"
  echo ">>> Tier 0.5 on git archive HEAD ($(git rev-parse --short HEAD)) in $WORK"
elif [ "$MODE" = "worktree" ]; then
  { git ls-files -z; git ls-files -z -o --exclude-standard -- . ':!docs/superpowers/'; } |
    tar --null -T - -cf - 2>/dev/null | tar -x -C "$WORK"
  echo ">>> Tier 0.5 on an export of the working tree (tracked + untracked outside docs/superpowers) in $WORK"
else
  echo "usage: $0 [--head]" >&2
  exit 2
fi

# the table is either empty on one line (KNOWN_FAILING_TABLE="") or one "name|reason" per line
KNOWN="$(awk '/KNOWN_FAILING_TABLE=""/{exit} /KNOWN_FAILING_TABLE="/{f=1; next} f && /^[[:space:]]*"[[:space:]]*$/{exit} f' \
          .github/workflows/ci.yml | sed 's/^[[:space:]]*//' | awk -F'|' 'NF>=1 && $1!="" {print $1}')"

cd "$WORK"
new_failures=""; unexpected_passes=""
for f in tests/tools/check_*.py; do
  b="$(basename "$f")"
  [ "$b" = check_metrics.py ] && continue
  if python3 "$f" > "$WORK/.log_$b" 2>&1; then passed=yes; else passed=no; fi
  if printf '%s\n' "$KNOWN" | grep -qx "$b"; then known=yes; else known=no; fi
  if   [ $passed = yes ] && [ $known = no ];  then v="pass"
  elif [ $passed = no ]  && [ $known = yes ]; then v="known failure (ci.yml KNOWN_FAILING_TABLE)"
  elif [ $passed = no ]; then v="NEW FAILURE"; new_failures="$new_failures $b"
  else v="now passing - remove from KNOWN_FAILING_TABLE"; unexpected_passes="$unexpected_passes $b"; fi
  printf '%-45s %s\n' "$b" "$v"
  if [ $passed = no ] && [ $known = no ]; then sed 's/^/    | /' "$WORK/.log_$b" | tail -n 20; fi
done

status=0
[ -n "$new_failures" ] && { echo ">>> NEW FAILURES:$new_failures"; status=1; }
[ -n "$unexpected_passes" ] && { echo ">>> NOW PASSING (update ci.yml):$unexpected_passes"; status=1; }
[ $status -eq 0 ] && echo ">>> Tier 0.5: no new failure"
exit $status
