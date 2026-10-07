#!/usr/bin/env bash
set -euo pipefail

REF_FILE="reference.json"
NEW_FILE="regression_test_metrics.json"
SCRIPT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

while IFS= read -r src; do
  d="$(dirname "$src")"
  dst="${d}/${REF_FILE}"

  if [[ -f "$dst" ]]; then
    cp -f "$src" "$dst"
    echo "[OK   ] Updated ${dst#${SCRIPT_ROOT}/}"
  else
    echo "[SKIP ] ${src#${SCRIPT_ROOT}/} has no ${REF_FILE}"
  fi
done < <(find "$SCRIPT_ROOT" -mindepth 2 -type f -name "$NEW_FILE" | sort)
