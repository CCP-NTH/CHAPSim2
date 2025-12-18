#!/usr/bin/env bash
set -o pipefail

REF_FILE="reference.json"
NEW_FILE="regression_test_metrics.json"

for d in */; do
  src="${d%/}/${NEW_FILE}"
  dst="${d%/}/${REF_FILE}"

  if [[ -f "$src" ]]; then
    cp -f "$src" "$dst"
    echo "[OK   ] Updated $dst"
  else
    echo "[SKIP ] $src not found"
  fi
done