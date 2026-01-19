#!/usr/bin/env bash
set -o pipefail

CASES=(
  tgv_iso
  tgv_scp
  #
  channel_iso_periodic
  channel_iso_inout
  channel_scp_periodic
  channel_scp_inout
  #
  annular_iso_periodic
  annular_iso_inout
  annular_scp_periodic
  annular_scp_inout
  #
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

# --------------------------------------------------
# Ask whether to build solver
# --------------------------------------------------
read -p "Do you want to build the solver? [N/y]: " build_choice
build_choice=${build_choice:-N}
build_choice=$(echo "$build_choice" | tr '[:upper:]' '[:lower:]')

if [[ "$build_choice" == "y" || "$build_choice" == "yes" ]]; then
    echo ">>> Building solver"
    ../build_make_noninteractive.sh || { echo "BUILD FAILED"; exit 1; }
else
    echo ">>> Skipping solver build"
fi
echo "========================================"

SCRIPT_ROOT=$(pwd)

# --------------------------------------------------
# Loop over cases
# --------------------------------------------------
for case in "${CASES[@]}"; do
  ((TOTAL++))

  echo "----------------------------------------"
  echo ">>> Running test: ${case}"

  if [[ ! -d "${case}" ]]; then
    echo "[SKIP ] ${case} (missing directory)"
    ((SKIPPED++))
    continue
  fi

  pushd "${case}" > /dev/null

  # -----------------------------
  # Run solver
  # -----------------------------
  echo ">>> Running solver for ${case} ..."
  if ! ./run_archer2.sh; then
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
  if python3 "${SCRIPT_ROOT}/tools/check_metrics.py" \
      "${SCRIPT_ROOT}/${case}/${METRIC_FILE}" \
      "${SCRIPT_ROOT}/${case}/reference.json" \
      "${SCRIPT_ROOT}/tools/tolerances.json"; then
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
