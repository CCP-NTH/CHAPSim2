#!/usr/bin/env bash
set -euo pipefail

echo "========================================"
echo "Running SMOKE tests"
echo "========================================"
export RUN_MODE=smoke

# Define smoke test cases
SMOKE_CASES=(
  tgv_iso
  channel_iso_inout
  channel_scp_periodic
  pipe_scp_inout
)

TOTAL=0
FAILED=0
PASSED=0
SKIPPED=0
FAILED_CASES=()

# Number of iterations for smoke test
NITER=10

# Run script name
RUN_SCRIPT="./run_chapsim.sh"

# --------------------------------------------------
# Ask whether to build solver
# --------------------------------------------------
MAX_WAIT=5
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
echo "Found ${#SMOKE_CASES[@]} test case(s)"
echo "========================================"
echo ""

# Debug: verify we're about to enter the loop
echo "DEBUG: About to enter smoke test loop with ${#SMOKE_CASES[@]} cases..."
if [[ ${#SMOKE_CASES[@]} -eq 0 ]]; then
  echo "ERROR: No smoke test cases found!"
  exit 1
fi
echo "DEBUG: First case = ${SMOKE_CASES[0]}"
echo ""

# --------------------------------------------------
# Loop over cases
# --------------------------------------------------
for case in "${SMOKE_CASES[@]}"; do
  TOTAL=$((TOTAL + 1))
  echo "----------------------------------------"
  echo ">>> Smoke test: ${case} [${TOTAL}/${#SMOKE_CASES[@]}]"
  
  # Check if directory exists
  if [[ ! -d "${case}" ]]; then
    echo "[SKIP ] ${case} (missing directory)"
    SKIPPED=$((SKIPPED + 1))
    continue
  fi
  
  # Enter case directory
  pushd "${case}" > /dev/null 2>&1 || {
    echo "[FAIL ] ${case} (cannot enter directory)"
    FAILED=$((FAILED + 1))
    FAILED_CASES+=("${case}")
    continue
  }
  
  # Clean previous output
  echo "  Cleaning previous outputs..."
  rm -f *.log *.dat *.out *.err regression_test_metrics.json 2>/dev/null || true
  rm -rf 2_*/ 3_*/ 4_*/ 2>/dev/null || true
  
  # Run solver with CHAPSIM_NITER
  echo "  Running solver with CHAPSIM_NITER=${NITER}..."
  RUN_EXIT_CODE=0
  if ! CHAPSIM_NITER=${NITER} ${RUN_SCRIPT}; then
    RUN_EXIT_CODE=$?
    echo "[FAIL ] ${case} (exit code: ${RUN_EXIT_CODE})"
    FAILED=$((FAILED + 1))
    FAILED_CASES+=("${case}")
  else
    echo "[PASS ] ${case}"
    PASSED=$((PASSED + 1))
  fi
  
  popd > /dev/null 2>&1 || true
done

echo ""
echo "========================================"
echo "SMOKE TEST SUMMARY"
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