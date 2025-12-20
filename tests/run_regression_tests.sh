#!/usr/bin/env bash
set -euo pipefail

CASES=(
  tgv_iso
  tgv_scp
  channel_iso_periodic
  channel_iso_inout
  channel_scp_periodic
  channel_scp_inout
  annular_iso_periodic
  annular_iso_inout
  annular_scp_periodic
  annular_scp_inout
  pipe_iso_periodic
  pipe_iso_inout
  pipe_scp_periodic
  pipe_scp_inout
)

TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0
FAILED_CASES=()

# Run script name
RUN_SCRIPT="./run_chapsim.sh"

# --------------------------------------------------
# Ask whether to build solver
# --------------------------------------------------
MAX_WAIT=10
DEFAULT_CHOICE="n"
BUILD_MODE="non-interactive"
BUILD_SCRIPT="../build_chapsim.sh"

if [[ "${CI:-false}" == "true" ]]; then
    build_choice=$DEFAULT_CHOICE
else
    read -t $MAX_WAIT -p "Do you want to build the solver? [N/y]: " build_choice || true
    build_choice=${build_choice:-n}
fi

# Use default if empty or timeout
build_choice=${build_choice:-$DEFAULT_CHOICE}
build_choice=$(echo "$build_choice" | tr '[:upper:]' '[:lower:]')

if [[ "$build_choice" == "y" || "$build_choice" == "yes" ]]; then
    echo ">>> Building solver"
    # Pass the mode from parent script to build script
    export CHAPSIM_MODE="${BUILD_MODE}"
    $BUILD_SCRIPT || { echo "BUILD FAILED"; exit 1; }
else
    echo ">>> Skipping solver build"
fi

echo "========================================"
SCRIPT_ROOT=$(pwd)
echo "Test root directory: $SCRIPT_ROOT"
echo "Found ${#CASES[@]} test case(s)"
echo "========================================"
echo ""

# Debug: verify we're about to enter the loop
# echo "DEBUG: About to enter test loop with ${#CASES[@]} cases..."
# if [[ ${#CASES[@]} -eq 0 ]]; then
#   echo "ERROR: No test cases found!"
#   exit 1
# fi
# echo "DEBUG: First case = ${CASES[0]}"
# echo ""

# --------------------------------------------------
# Loop over cases
# --------------------------------------------------
for case in "${CASES[@]}"; do
  TOTAL=$((TOTAL + 1))
  echo "----------------------------------------"
  echo ">>> Running test: ${case} [${TOTAL}/${#CASES[@]}]"
  
  # Check if directory exists
  if [[ ! -d "${case}" ]]; then
    echo "[SKIP ] ${case} (missing directory)"
    SKIPPED=$((SKIPPED + 1))
    continue
  fi
  
  pushd "${case}" > /dev/null 2>&1 || {
    echo "[FAIL ] ${case} (cannot enter directory)"
    FAILED=$((FAILED + 1))
    FAILED_CASES+=("${case}")
    continue
  }
  
  # Run solver
  export RUN_MODE=regression
  echo "  Running solver for ${case}..."
  RUN_EXIT_CODE=0
  if ! ${RUN_SCRIPT}; then
    RUN_EXIT_CODE=$?
    echo "[FAIL ] ${case} (${RUN_SCRIPT} failed with exit code ${RUN_EXIT_CODE})"
    FAILED=$((FAILED + 1))
    FAILED_CASES+=("${case}")
    popd > /dev/null 2>&1 || true
    continue
  fi
  
  echo "  Solver finished for ${case}"
  
  # Wait for metrics file (max 200s)
  METRIC_FILE="regression_test_metrics.json"
  MAX_WAIT_METRIC=200
  WAITED=0
  echo "  Waiting for ${METRIC_FILE} (timeout ${MAX_WAIT_METRIC}s)..."
  
  while [[ ! -f "$METRIC_FILE" && "$WAITED" -lt "$MAX_WAIT_METRIC" ]]; do
    sleep 1
    ((WAITED++))
  done
  
  if [[ ! -f "$METRIC_FILE" ]]; then
    echo "[FAIL ] ${case} (timeout waiting for metrics after ${WAITED}s)"
    FAILED=$((FAILED + 1))
    FAILED_CASES+=("${case}")
    popd > /dev/null 2>&1 || true
    continue
  fi
  
  echo "  Metrics file detected after ${WAITED}s"
  
  # Check metrics
  METRICS_EXIT_CODE=0
  if ! python3 "${SCRIPT_ROOT}/tools/check_metrics.py" \
      "${SCRIPT_ROOT}/${case}/${METRIC_FILE}" \
      "${SCRIPT_ROOT}/${case}/reference.json" \
      "${SCRIPT_ROOT}/tools/tolerances.json"; then
    METRICS_EXIT_CODE=$?
    echo "[FAIL ] ${case} (metrics check failed with exit code ${METRICS_EXIT_CODE})"
    ((FAILED++))
    FAILED_CASES+=("${case}")
  else
    echo "[PASS ] ${case}"
    PASSED=$((PASSED + 1))
  fi
  
  popd > /dev/null || true
done

echo ""
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