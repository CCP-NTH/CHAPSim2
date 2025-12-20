
#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# CHAPSim regression test runner
# Supports:
#   - Run test cases + check metrics
#   - Check metrics only (no solver run)
# =============================================================================

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

RUN_SCRIPT="./run_chapsim.sh"
BUILD_SCRIPT="../build_chapsim.sh"
SCRIPT_ROOT="$(pwd)"

MAX_WAIT=10
DEFAULT_BUILD_CHOICE="n"
DEFAULT_TEST_MODE="run"   # run | check

# =============================================================================
# Step 0: Run cases or metrics-only?
# =============================================================================
TEST_MODE="$DEFAULT_TEST_MODE"

if [[ "${CI:-false}" != "true" ]]; then
    read -t "$MAX_WAIT" -p \
        "Run test cases or check metrics only? [r]un/[c]heck (default: r): " \
        TEST_MODE_INPUT || true
    TEST_MODE_INPUT="${TEST_MODE_INPUT:-$DEFAULT_TEST_MODE}"
    TEST_MODE_INPUT="$(echo "$TEST_MODE_INPUT" | tr '[:upper:]' '[:lower:]')"

    if [[ "$TEST_MODE_INPUT" == "check" || "$TEST_MODE_INPUT" == "c" ]]; then
        TEST_MODE="check"
    else
        TEST_MODE="run"
    fi
fi

echo ">>> Test mode: $TEST_MODE"
echo "========================================"
echo "Test root directory: $SCRIPT_ROOT"
echo "Found ${#CASES[@]} test case(s)"
echo "========================================"
echo ""

# =============================================================================
# Step 1: Build solver?
# =============================================================================
if [[ "$TEST_MODE" == "run" ]]; then
  BUILD_CHOICE="$DEFAULT_BUILD_CHOICE"

  if [[ "${CI:-false}" != "true" ]]; then
      read -t "$MAX_WAIT" -p "Do you want to build the solver? [N/y]: " BUILD_CHOICE || true
      BUILD_CHOICE="${BUILD_CHOICE:-$DEFAULT_BUILD_CHOICE}"
  fi

  BUILD_CHOICE="$(echo "$BUILD_CHOICE" | tr '[:upper:]' '[:lower:]')"

  if [[ "$BUILD_CHOICE" =~ ^(y|yes)$ ]]; then
      echo ">>> Building solver"
      "$BUILD_SCRIPT" || { echo "❌ BUILD FAILED"; exit 1; }
  else
      echo ">>> Skipping solver build"
  fi

  echo ""
fi

# =============================================================================
# Step 2: Loop over cases
# =============================================================================
for case in "${CASES[@]}"; do
    TOTAL=$((TOTAL + 1))
    echo "----------------------------------------"
    echo ">>> Case: ${case} [${TOTAL}/${#CASES[@]}]"

    if [[ ! -d "$case" ]]; then
        echo "[SKIP ] ${case} (missing directory)"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    pushd "$case" > /dev/null || {
        echo "[FAIL ] ${case} (cannot enter directory)"
        FAILED=$((FAILED + 1))
        FAILED_CASES+=("$case")
        continue
    }

    METRIC_FILE="regression_test_metrics.json"

    # -------------------------------------------------------------------------
    # Run solver (only in run mode)
    # -------------------------------------------------------------------------
    if [[ "$TEST_MODE" == "run" ]]; then
        export RUN_MODE=regression
        echo "  Running solver..."
        if ! "$RUN_SCRIPT"; then
            echo "[FAIL ] ${case} (solver failed)"
            FAILED=$((FAILED + 1))
            FAILED_CASES+=("$case")
            popd > /dev/null
            continue
        fi

        echo "  Solver finished"

        # Wait for metrics file
        MAX_WAIT_METRIC=200
        WAITED=0
        echo "  Waiting for ${METRIC_FILE} (timeout ${MAX_WAIT_METRIC}s)..."

        while [[ ! -f "$METRIC_FILE" && "$WAITED" -lt "$MAX_WAIT_METRIC" ]]; do
            sleep 1
            ((WAITED++))
        done

        if [[ ! -f "$METRIC_FILE" ]]; then
            echo "[FAIL ] ${case} (metrics not generated)"
            FAILED=$((FAILED + 1))
            FAILED_CASES+=("$case")
            popd > /dev/null
            continue
        fi
    else
        # ---------------------------------------------------------------------
        # Metrics-only mode
        # ---------------------------------------------------------------------
        echo "  Metrics-only mode: skipping solver run"

        if [[ ! -f "$METRIC_FILE" ]]; then
            echo "[SKIP ] ${case} (metrics file not found)"
            SKIPPED=$((SKIPPED + 1))
            popd > /dev/null
            continue
        fi
    fi

    # -------------------------------------------------------------------------
    # Metrics check (common path)
    # -------------------------------------------------------------------------
    if ! python3 "${SCRIPT_ROOT}/tools/check_metrics.py" \
        "${SCRIPT_ROOT}/${case}/${METRIC_FILE}" \
        "${SCRIPT_ROOT}/${case}/reference.json" \
        "${SCRIPT_ROOT}/tools/tolerances.json"; then
        echo "[FAIL ] ${case} (metrics check failed)"
        FAILED=$((FAILED + 1))
        FAILED_CASES+=("$case")
    else
        echo "[PASS ] ${case}"
        PASSED=$((PASSED + 1))
    fi

    popd > /dev/null
done

# =============================================================================
# Final summary
# =============================================================================
echo ""
echo "========================================"
echo "SUMMARY"
echo "----------------------------------------"
printf "MODE   : %s\n" "$TEST_MODE"
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