#!/usr/bin/env bash
set -euo pipefail

echo "========================================"
echo "Running SMOKE tests"
echo "========================================"
export RUN_MODE=smoke
SMOKE_CASES=(
  tgv_iso
  #
  channel_iso_inout
  channel_scp_periodic
  #
  pipe_scp_inout
)

FAILED=0
PASSED=0
# --------------------------------------------------
# Ask whether to build solver
# --------------------------------------------------
MAX_WAIT=5
DEFAULT_CHOICE="n"
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
    ../build_make_noninteractive.sh || { echo "BUILD FAILED"; exit 1; }
else
    echo ">>> Skipping solver build"
fi
echo "========================================"

SCRIPT_ROOT=$(pwd)
# --------------------------------------------------
# Loop over cases
# --------------------------------------------------
for case in "${SMOKE_CASES[@]}"; do
  echo "----------------------------------------"
  echo ">>> Smoke test: ${case}"

  if [[ ! -d "${case}" ]]; then
    echo "[FAIL ] ${case} (missing directory)"
    ((FAILED++))
    continue
  fi

  pushd "${case}" > /dev/null

  # Clean previous output
  rm -f *.log *.dat *.out *.err regression_test_metrics.json
  rm -rf 2_*/ 3_*/ 4_*/

  # Run solver (run.sh decides mpirun/serial)
  if CHAPSIM_NITER=10 ./run_chapsim.sh; then
    echo "[PASS ] ${case}"
    ((PASSED++))
  else
    echo "[FAIL ] ${case}"
    ((FAILED++))
  fi

  popd > /dev/null
done

echo "========================================"
echo "SMOKE SUMMARY: PASSED=${PASSED}, FAILED=${FAILED}"
echo "========================================"

if [[ ${FAILED} -ne 0 ]]; then
  exit 1
fi