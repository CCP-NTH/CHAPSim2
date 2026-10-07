#!/usr/bin/env bash
set -o pipefail

# Case lists are shared with the local runner so the two cannot drift apart.
# This driver used to carry its own hardcoded copy, which fell 15 cases behind
# - and the missing 15 were exactly the newest work (all MHD, all LES, all three
# high-order cases), so an HPC run validated only what was already covered.
# shellcheck source=case_lists.sh
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/case_lists.sh"

# Default to the widest coverage here: an HPC campaign is exactly where the full
# suite is wanted. Override with REGRESSION_SUITE=standard / INCLUDE_FUNCTIONAL_TESTS=n.
REGRESSION_SUITE="${REGRESSION_SUITE:-extended}"
INCLUDE_FUNCTIONAL="${INCLUDE_FUNCTIONAL_TESTS:-y}"

if [[ "$REGRESSION_SUITE" == "extended" ]]; then
  CASES=("${EXTENDED_CASES[@]}")
  if [[ "$INCLUDE_FUNCTIONAL" =~ ^(y|yes|true|1)$ ]]; then
    CASES+=("${FUNCTIONAL_CASES[@]}")
  fi
else
  CASES=("${STANDARD_CASES[@]}")
fi

echo ">>> Regression suite: ${REGRESSION_SUITE}"
echo ">>> Functional tests: ${INCLUDE_FUNCTIONAL}"
echo ">>> Found ${#CASES[@]} test case(s)"

TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0

FAILED_CASES=()

# --------------------------------------------------
# Ask whether to build solver
# --------------------------------------------------
# Timed out and CI-guarded: an unguarded read blocks forever with no tty, which
# is how this script is run under sbatch.
build_choice="${BUILD_CHOICE:-N}"
if [[ "${CI:-false}" != "true" ]]; then
    read -t 10 -p "Do you want to build the solver? [N/y]: " build_choice || true
fi
build_choice=${build_choice:-N}
build_choice=$(echo "$build_choice" | tr '[:upper:]' '[:lower:]')

SCRIPT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS_ROOT="$(cd "${SCRIPT_ROOT}/.." && pwd)"
PROJECT_ROOT="$(cd "${TESTS_ROOT}/.." && pwd)"

if [[ "$build_choice" == "y" || "$build_choice" == "yes" ]]; then
    echo ">>> Building solver"
    "${PROJECT_ROOT}/build_chapsim.sh" || { echo "BUILD FAILED"; exit 1; }
else
    echo ">>> Skipping solver build"
fi
echo "========================================"

# --------------------------------------------------
# Loop over cases
# --------------------------------------------------
for case in "${CASES[@]}"; do
  ((TOTAL++))

  echo "----------------------------------------"
  echo ">>> Running test: ${case}"

  CASE_DIR="${SCRIPT_ROOT}/${case}"
  if [[ ! -d "${CASE_DIR}" ]]; then
    echo "[SKIP ] ${case} (missing directory)"
    ((SKIPPED++))
    continue
  fi

  pushd "${CASE_DIR}" > /dev/null

  # -----------------------------
  # Run solver
  # -----------------------------
  echo ">>> Running solver for ${case} ..."
  if ! "${TESTS_ROOT}/run_archer2.sh"; then
    echo "[FAIL ] ${case} (run_archer2.sh failed)"
    ((FAILED++))
    FAILED_CASES+=("${case}")
    popd > /dev/null
    continue
  fi
  echo ">>> Solver finished for ${case}"

  # -----------------------------
  # Wait for metrics file (max 200s)
  # -----------------------------
  METRIC_FILE="regression_test_metrics.json"
  MAX_WAIT=200
  WAITED=0

  echo ">>> Waiting for ${METRIC_FILE} (timeout ${MAX_WAIT}s)..."
  while [[ ! -f "$METRIC_FILE" && "$WAITED" -lt "$MAX_WAIT" ]]; do
    sleep 1
    ((WAITED++))
  done

  if [[ ! -f "$METRIC_FILE" ]]; then
    echo "[FAIL ] ${case} (timeout waiting for metrics)"
    ((FAILED++))
    FAILED_CASES+=("${case}")
    popd > /dev/null
    continue
  fi

  echo ">>> Metrics file detected after ${WAITED}s"

  # -----------------------------
  # Check metrics
  # -----------------------------
  if python3 "${TESTS_ROOT}/tools/check_metrics.py" \
      "${CASE_DIR}/${METRIC_FILE}" \
      "${CASE_DIR}/reference.json" \
      "${TESTS_ROOT}/tools/tolerances.json"; then
    echo "[PASS ] ${case}"
    ((PASSED++))
  else
    echo "[FAIL ] ${case} (metrics check failed)"
    ((FAILED++))
    FAILED_CASES+=("${case}")
  fi

  popd > /dev/null
done

# --------------------------------------------------
# Final summary
# --------------------------------------------------
echo "========================================"
echo "SUMMARY"
echo "----------------------------------------"
printf "PASSED : %3d / %3d\n" "$PASSED"  "$TOTAL"
printf "FAILED : %3d / %3d\n" "$FAILED"  "$TOTAL"
printf "SKIPPED: %3d / %3d\n" "$SKIPPED" "$TOTAL"
echo "----------------------------------------"

if [[ "$FAILED" -ne 0 ]]; then
  echo "Failed cases:"
  for c in "${FAILED_CASES[@]}"; do
    echo "  - $c"
  done
  echo "========================================"
  exit 1
else
  echo "All executed cases passed ✅"
  echo "========================================"
  exit 0
fi
