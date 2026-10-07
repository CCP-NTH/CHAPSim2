#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# CHAPSim functional test runner: Restart equivalence
# For each case:
#   - Run run_continuous
#   - Copy *_40.bin and *_40.dat -> run_restart/1_data
#   - Run run_restart
#   - Compare regression_test_metrics.json (restart vs continuous)
#
# Supports:
#   - Run test cases + check metrics
#   - Check metrics only (no solver run)
# =============================================================================

CASES=(
  tgv_iso
  tgv_scp
  channel_scp_inout
  pipe_scp_inout
  tgv_iso_stats
)

# Cases whose run_restart keeps the continuous stat_istart, so the statistics
# accumulators are reloaded and continued. For these the instantaneous metrics
# are not enough: the time-averaged fields and their sample count are compared
# directly against the uninterrupted run. Every other case restarts statistics
# from empty and has nothing to compare.
STATS_COMPARE_CASES=(
  tgv_iso_stats
)

# Iteration both runs end at, i.e. the statistics checkpoint to compare.
STATS_COMPARE_ITER=50
# Array-relative, matching the instantaneous restart tolerances: a restart
# reproduces the flow to round-off, not bitwise, and the averages inherit that.
STATS_COMPARE_RTOL=1e-10

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
TESTS_ROOT="$(cd "${SCRIPT_ROOT}/../.." && pwd)"
PROJECT_ROOT="$(cd "${TESTS_ROOT}/.." && pwd)"
BUILD_SCRIPT="${PROJECT_ROOT}/build_chapsim.sh"

# Comparator reused from main tests/tools
CHECK_TOOL="${TESTS_ROOT}/tools/check_metrics.py"
STATS_TOOL="${TESTS_ROOT}/tools/compare_stats_restart.py"

# Restart-specific tolerances stored locally in this functional folder
TOL_FILE="${SCRIPT_ROOT}/tolerance_restart.json"

# Restart data movement rule
RESTART_PATTERNS=( "*_40.bin" "*_40.dat" )

# Interactive defaults (same style as regression runner)
MAX_WAIT=10
DEFAULT_BUILD_CHOICE="n"
DEFAULT_TEST_MODE="run"   # run | check

# Metrics file name produced by runs
METRIC_FILE="regression_test_metrics.json"

# =============================================================================
# Step 0: Run cases or metrics-only?
# =============================================================================
TEST_MODE="${TEST_MODE:-$DEFAULT_TEST_MODE}"

if [[ "${CI:-false}" != "true" ]]; then
    read -t "$MAX_WAIT" -p \
        "Run functional tests or check metrics only? [r]un/[c]heck (default: r): " \
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
echo "Functional test: restart"
echo "Test root directory: $SCRIPT_ROOT"
echo "Found ${#CASES[@]} test case(s)"
echo "Restart copy patterns: ${RESTART_PATTERNS[*]}"
echo "Comparator: ${CHECK_TOOL}"
echo "Tolerances: ${TOL_FILE}"
echo "========================================"
echo ""

# Basic sanity
[[ -f "${CHECK_TOOL}" ]] || { echo "❌ Missing ${CHECK_TOOL}"; exit 2; }
[[ -f "${TOL_FILE}"   ]] || { echo "❌ Missing ${TOL_FILE}"; exit 2; }

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
      "${BUILD_SCRIPT}" || { echo "❌ BUILD FAILED"; exit 1; }
  else
      echo ">>> Skipping solver build"
  fi

  echo ""
fi

# =============================================================================
# Helpers
# =============================================================================
run_one() {
  local rundir="$1"     # run_continuous or run_restart
  pushd "$rundir" > /dev/null || return 2

  if [[ ! -x "$RUN_SCRIPT" ]]; then
    echo "❌ Missing executable ${rundir}/${RUN_SCRIPT}"
    popd > /dev/null
    return 2
  fi

  # Make sure we don't accidentally reuse old metrics
  rm -f "${METRIC_FILE}" || true

  echo "  Running solver in $(pwd)..."
  if ! "$RUN_SCRIPT"; then
    popd > /dev/null
    return 1
  fi

  # Wait for metrics to appear
  local MAX_WAIT_METRIC=200
  local WAITED=0
  echo "  Waiting for ${METRIC_FILE} (timeout ${MAX_WAIT_METRIC}s)..."
  while [[ ! -f "${METRIC_FILE}" && "$WAITED" -lt "$MAX_WAIT_METRIC" ]]; do
      sleep 1
      ((WAITED++))
  done

  if [[ ! -f "${METRIC_FILE}" ]]; then
    echo "❌ Metrics not generated: ${METRIC_FILE}"
    popd > /dev/null
    return 3
  fi

  popd > /dev/null
  return 0
}

copy_restart_bins() {
  local cont_dir="$1"  # run_continuous
  local rest_dir="$2"  # run_restart

  local src="${cont_dir}/1_data"
  local dst="${rest_dir}/1_data"

  mkdir -p "$dst"

  shopt -s nullglob
  local files=()
  local pattern
  for pattern in "${RESTART_PATTERNS[@]}"; do
    files+=( "${src}"/${pattern} )
  done
  shopt -u nullglob

  if [[ ${#files[@]} -eq 0 ]]; then
    echo "❌ No files matched '${RESTART_PATTERNS[*]}' in ${src}"
    return 1
  fi

  cp -f "${files[@]}" "${dst}/"
  return 0
}

check_metrics_pair() {
  local cont_json="$1"
  local rest_json="$2"

  # restart is "new", continuous is "ref"
  python3 "${CHECK_TOOL}" "${rest_json}" "${cont_json}" "${TOL_FILE}"
}

wants_stats_compare() {
  local case_name="$1"
  local c
  for c in "${STATS_COMPARE_CASES[@]}"; do
    [[ "$c" == "$case_name" ]] && return 0
  done
  return 1
}

check_stats_pair() {
  local case_dir="$1"
  local groups="flow_stats"

  # The thermal accumulators live in their own bundle, present only when the
  # case is thermal; the comparator reports a missing file as a failure, so
  # only ask for the group when it exists.
  if [[ -f "${case_dir}/run_continuous/1_data/domain1_thermo_stats_${STATS_COMPARE_ITER}.bin" ]]; then
    groups="${groups},thermo_stats"
  fi

  python3 "${STATS_TOOL}" \
      "${case_dir}/run_continuous/1_data" \
      "${case_dir}/run_restart/1_data" \
      "${STATS_COMPARE_ITER}" \
      --rtol "${STATS_COMPARE_RTOL}" --groups "${groups}"
}

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

    CONT_DIR="run_continuous"
    REST_DIR="run_restart"

    if [[ ! -d "$CONT_DIR" || ! -d "$REST_DIR" ]]; then
        echo "[SKIP ] ${case} (missing ${CONT_DIR}/ or ${REST_DIR}/)"
        SKIPPED=$((SKIPPED + 1))
        popd > /dev/null
        continue
    fi

    CONT_JSON="${CASE_DIR}/${CONT_DIR}/${METRIC_FILE}"
    REST_JSON="${CASE_DIR}/${REST_DIR}/${METRIC_FILE}"

    # -------------------------------------------------------------------------
    # Run solver (only in run mode)
    # -------------------------------------------------------------------------
    if [[ "$TEST_MODE" == "run" ]]; then
        export RUN_MODE=functional

        echo "  1) Continuous run"
        if run_one "${CONT_DIR}"; then
            :
        else
            RUN_EXIT_CODE=$?
            if [[ "$RUN_EXIT_CODE" -eq 1 ]]; then
                echo "[FAIL ] ${case} (continuous solver failed)"
                SOLVER_FAILED_CASES+=("$case (continuous run)")
            elif [[ "$RUN_EXIT_CODE" -eq 3 ]]; then
                echo "[FAIL ] ${case} (continuous metrics not generated)"
                METRICS_FAILED_CASES+=("$case (continuous metrics not generated)")
            else
                echo "[FAIL ] ${case} (continuous run setup failed)"
                OTHER_FAILED_CASES+=("$case (continuous run setup failed)")
            fi
            FAILED=$((FAILED + 1))
            FAILED_CASES+=("$case")
            popd > /dev/null
            continue
        fi

        echo "  2) Copy restart checkpoint files (${RESTART_PATTERNS[*]})"
        if ! copy_restart_bins "${CONT_DIR}" "${REST_DIR}"; then
            echo "[FAIL ] ${case} (restart bin copy failed)"
            FAILED=$((FAILED + 1))
            FAILED_CASES+=("$case")
            OTHER_FAILED_CASES+=("$case (restart bin copy failed)")
            popd > /dev/null
            continue
        fi

        echo "  3) Restart run"
        if run_one "${REST_DIR}"; then
            :
        else
            RUN_EXIT_CODE=$?
            if [[ "$RUN_EXIT_CODE" -eq 1 ]]; then
                echo "[FAIL ] ${case} (restart solver failed)"
                SOLVER_FAILED_CASES+=("$case (restart run)")
            elif [[ "$RUN_EXIT_CODE" -eq 3 ]]; then
                echo "[FAIL ] ${case} (restart metrics not generated)"
                METRICS_FAILED_CASES+=("$case (restart metrics not generated)")
            else
                echo "[FAIL ] ${case} (restart run setup failed)"
                OTHER_FAILED_CASES+=("$case (restart run setup failed)")
            fi
            FAILED=$((FAILED + 1))
            FAILED_CASES+=("$case")
            popd > /dev/null
            continue
        fi
    else
        # ---------------------------------------------------------------------
        # Metrics-only mode
        # ---------------------------------------------------------------------
        echo "  Metrics-only mode: skipping solver runs"

        if [[ ! -f "${CONT_JSON}" ]]; then
            echo "[SKIP ] ${case} (missing continuous metrics)"
            SKIPPED=$((SKIPPED + 1))
            popd > /dev/null
            continue
        fi
        if [[ ! -f "${REST_JSON}" ]]; then
            echo "[SKIP ] ${case} (missing restart metrics)"
            SKIPPED=$((SKIPPED + 1))
            popd > /dev/null
            continue
        fi
    fi

    # -------------------------------------------------------------------------
    # Metrics check
    # -------------------------------------------------------------------------
    echo "  4) Compare metrics (restart vs continuous)"
    CASE_OK=1
    if ! check_metrics_pair "${CONT_JSON}" "${REST_JSON}"; then
        echo "[FAIL ] ${case} (metrics check failed)"
        METRICS_FAILED_CASES+=("$case")
        CASE_OK=0
    fi

    if [[ "$CASE_OK" -eq 1 ]] && wants_stats_compare "${case}"; then
        echo "  5) Compare time-averaged statistics (restart vs continuous)"
        if ! check_stats_pair "${CASE_DIR}"; then
            echo "[FAIL ] ${case} (statistics check failed)"
            METRICS_FAILED_CASES+=("$case (statistics)")
            CASE_OK=0
        fi
    fi

    if [[ "$CASE_OK" -eq 1 ]]; then
        echo "[PASS ] ${case}"
        PASSED=$((PASSED + 1))
    else
        FAILED=$((FAILED + 1))
        FAILED_CASES+=("$case")
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
