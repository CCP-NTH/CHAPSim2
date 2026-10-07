#!/usr/bin/env bash
set -euo pipefail

SCRIPT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SUBRUNNERS=(
  "functional/restart/run_functional.sh"
  "functional/mesh_mapping/run_functional.sh"
)

for runner in "${SUBRUNNERS[@]}"; do
  runner_path="${SCRIPT_ROOT}/${runner}"
  if [[ ! -x "$runner_path" ]]; then
    echo "Missing executable functional runner: ${runner}"
    exit 2
  fi
  echo "========================================"
  echo "Running ${runner}"
  echo "========================================"
  "$runner_path" "$@"
done
