#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# CHAPSim regression test runner
# Supports:
#   - Run test cases + check metrics
#   - Check metrics only (no solver run)
# =============================================================================

# Case lists live in one file shared with the ARCHER2 driver, so the two cannot
# drift apart again. See case_lists.sh.
# shellcheck source=case_lists.sh
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/case_lists.sh"

CASES=()

TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0
FAILED_CASES=()
SOLVER_FAILED_CASES=()
METRICS_FAILED_CASES=()
OTHER_FAILED_CASES=()

RUN_SCRIPT="./run_chapsim.sh"
SCRIPT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS_ROOT="$(cd "${SCRIPT_ROOT}/.." && pwd)"
PROJECT_ROOT="$(cd "${TESTS_ROOT}/.." && pwd)"
BUILD_SCRIPT="${PROJECT_ROOT}/build_chapsim.sh"

MAX_WAIT=10
DEFAULT_BUILD_CHOICE="n"
DEFAULT_TEST_MODE="run"   # run | check
DEFAULT_REGRESSION_SUITE="standard"  # standard | extended
DEFAULT_INCLUDE_FUNCTIONAL="n"       # y | n, only used with extended suite

# =============================================================================
# Step 0: Run cases or metrics-only?
# =============================================================================
TEST_MODE="${TEST_MODE:-$DEFAULT_TEST_MODE}"
# honour an exported value, as TEST_MODE above does; a plain assignment here
# clobbered the environment before the CI branch below could read it back.
REGRESSION_SUITE="${REGRESSION_SUITE:-$DEFAULT_REGRESSION_SUITE}"
# Canonical external name is INCLUDE_FUNCTIONAL_TESTS, which is what the ARCHER2
# driver and the CI branch below already use. INCLUDE_FUNCTIONAL is kept as a
# deprecated alias: it used to be documented, and the CI branch silently
# clobbered it, so an exported INCLUDE_FUNCTIONAL=y ran 23 cases instead of 35.
INCLUDE_FUNCTIONAL="${INCLUDE_FUNCTIONAL_TESTS:-${INCLUDE_FUNCTIONAL:-$DEFAULT_INCLUDE_FUNCTIONAL}}"

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

    read -t "$MAX_WAIT" -p \
        "Run [s]tandard or [e]xtended regression? (default: s): " \
        REGRESSION_SUITE_INPUT || true
    REGRESSION_SUITE_INPUT="${REGRESSION_SUITE_INPUT:-$DEFAULT_REGRESSION_SUITE}"
    REGRESSION_SUITE_INPUT="$(echo "$REGRESSION_SUITE_INPUT" | tr '[:upper:]' '[:lower:]')"

    if [[ "$REGRESSION_SUITE_INPUT" == "extended" || "$REGRESSION_SUITE_INPUT" == "e" ]]; then
        REGRESSION_SUITE="extended"

        read -t "$MAX_WAIT" -p \
            "Include functional tests in extended regression? [y/N]: " \
            INCLUDE_FUNCTIONAL_INPUT || true
        INCLUDE_FUNCTIONAL_INPUT="${INCLUDE_FUNCTIONAL_INPUT:-$DEFAULT_INCLUDE_FUNCTIONAL}"
        INCLUDE_FUNCTIONAL_INPUT="$(echo "$INCLUDE_FUNCTIONAL_INPUT" | tr '[:upper:]' '[:lower:]')"

        if [[ "$INCLUDE_FUNCTIONAL_INPUT" =~ ^(y|yes)$ ]]; then
            INCLUDE_FUNCTIONAL="y"
        else
            INCLUDE_FUNCTIONAL="n"
        fi
    else
        REGRESSION_SUITE="standard"
    fi
else
    REGRESSION_SUITE="${REGRESSION_SUITE:-$DEFAULT_REGRESSION_SUITE}"
    REGRESSION_SUITE="$(echo "$REGRESSION_SUITE" | tr '[:upper:]' '[:lower:]')"
    # already resolved above from INCLUDE_FUNCTIONAL_TESTS / INCLUDE_FUNCTIONAL
    INCLUDE_FUNCTIONAL="$(echo "$INCLUDE_FUNCTIONAL" | tr '[:upper:]' '[:lower:]')"
fi

if [[ "$REGRESSION_SUITE" == "extended" ]]; then
    CASES=("${EXTENDED_CASES[@]}")
    if [[ "$INCLUDE_FUNCTIONAL" =~ ^(y|yes|true|1)$ ]]; then
        INCLUDE_FUNCTIONAL="y"
        CASES+=("${FUNCTIONAL_CASES[@]}")
    else
        INCLUDE_FUNCTIONAL="n"
    fi
else
    CASES=("${STANDARD_CASES[@]}")
    INCLUDE_FUNCTIONAL="n"
fi

echo ">>> Test mode: $TEST_MODE"
echo ">>> Regression suite: $REGRESSION_SUITE"
echo ">>> Functional tests: $INCLUDE_FUNCTIONAL"
echo "========================================"
echo "Regression root directory: $SCRIPT_ROOT"
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

    CASE_DIR="${SCRIPT_ROOT}/${case}"
    if [[ ! -d "$CASE_DIR" ]]; then
        echo "[SKIP ] ${case} (missing directory)"
        SKIPPED=$((SKIPPED + 1))
        continue
    fi

    pushd "$CASE_DIR" > /dev/null || {
        echo "[FAIL ] ${case} (cannot enter directory)"
        FAILED=$((FAILED + 1))
        FAILED_CASES+=("$case")
        OTHER_FAILED_CASES+=("$case (cannot enter directory)")
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
            SOLVER_FAILED_CASES+=("$case")
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
            METRICS_FAILED_CASES+=("$case (metrics not generated)")
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
    if ! python3 "${TESTS_ROOT}/tools/check_metrics.py" \
        "${CASE_DIR}/${METRIC_FILE}" \
        "${CASE_DIR}/reference.json" \
        "${TESTS_ROOT}/tools/tolerances.json"; then
        echo "[FAIL ] ${case} (metrics check failed)"
        FAILED=$((FAILED + 1))
        FAILED_CASES+=("$case")
        METRICS_FAILED_CASES+=("$case")
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
printf "  SOLVER : %3d\n" "${#SOLVER_FAILED_CASES[@]}"
printf "  METRICS: %3d\n" "${#METRICS_FAILED_CASES[@]}"
printf "  OTHER  : %3d\n" "${#OTHER_FAILED_CASES[@]}"
printf "SKIPPED: %3d / %3d\n" "$SKIPPED" "$TOTAL"
echo "----------------------------------------"

if [[ "$FAILED" -ne 0 ]]; then
    if [[ ${#SOLVER_FAILED_CASES[@]} -ne 0 ]]; then
        echo "Solver failed cases:"
        for c in "${SOLVER_FAILED_CASES[@]}"; do
            echo "  - $c"
        done
    fi
    if [[ ${#METRICS_FAILED_CASES[@]} -ne 0 ]]; then
        echo "Metrics check failed cases:"
        for c in "${METRICS_FAILED_CASES[@]}"; do
            echo "  - $c"
        done
    fi
    if [[ ${#OTHER_FAILED_CASES[@]} -ne 0 ]]; then
        echo "Other failed cases:"
        for c in "${OTHER_FAILED_CASES[@]}"; do
            echo "  - $c"
        done
    fi
    echo "========================================"
    exit 1
else
    echo "All executed cases passed ✅"
    echo "========================================"
    exit 0
fi
