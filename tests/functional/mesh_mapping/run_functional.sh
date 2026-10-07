#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# CHAPSim functional test runner: mesh remapping (is_prerun= .true.)
#
# Each case is a linear chain of step directories. A step either
#   - runs the solver normally and produces regression_test_metrics.json, or
#   - is a remapping prerun (is_prerun= .true. in its input_chapsim.ini), which
#     reads a restart written on the SOURCE mesh plus input_chapsim_tgt.ini and
#     writes an iteration-0 restart on the TARGET mesh, then stops.
#
# Two kinds of assertion are made:
#
#   identity:<dir>  The target mesh equals the source mesh, so remap-then-run
#                   must reproduce a plain restart of the same field. This is
#                   the strong correctness invariant: it is independent of any
#                   stored baseline and fails loudly if the remap loses or
#                   corrupts any part of the state (interior, boundary planes,
#                   AB2 histories, thermal properties).
#
#   reference       The mesh genuinely changes, so there is nothing to compare
#                   against analytically. Guard against silent drift with a
#                   stored reference.json, exactly as the regression suite does.
#
# Supports run mode and metrics-only (check) mode, same style as the other
# functional runners.
# =============================================================================

CASES=(
  channel_scp_inout
  pipe_iso_periodic
)

# Per-case step chain, one step per line:
#   <rundir>|<copy_from_dir>|<copy_tag>|<check_spec>
# copy_from_dir/copy_tag: restart files "*<tag>.bin" and "*<tag>.dat" are copied
# from that step's 1_data into this step's 1_data before running. Empty = none.
# check_spec: "" (no check), "identity:<dir>", or "reference".
CHAIN_channel_scp_inout="\
step0_source|||
step1_control|step0_source|_20|
step2_identity_remap|step0_source|_20|
step3_identity_run|step2_identity_remap|_0|identity:step1_control
step4_clamp_remap|step0_source|_20|
step5_clamp_run|step4_clamp_remap|_0|reference
step6_extend_remap|step0_source|_20|
step7_extend_run|step6_extend_remap|_0|reference"

CHAIN_pipe_iso_periodic="\
step0_source|||
step1_control|step0_source|_20|
step2_identity_remap|step0_source|_20|
step3_identity_run|step2_identity_remap|_0|identity:step1_control
step4_remap|step0_source|_20|
step5_remap_run|step4_remap|_0|reference"

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

# Mesh-mapping specific tolerances stored locally in this functional folder
TOL_IDENTITY="${SCRIPT_ROOT}/tolerance_identity.json"
TOL_REFERENCE="${SCRIPT_ROOT}/tolerance_reference.json"

# Interactive defaults (same style as the regression runner)
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
echo "Functional test: mesh mapping"
echo "Test root directory: $SCRIPT_ROOT"
echo "Found ${#CASES[@]} test case(s)"
echo "Comparator: ${CHECK_TOOL}"
echo "Tolerances: ${TOL_IDENTITY}"
echo "            ${TOL_REFERENCE}"
echo "========================================"
echo ""

# Basic sanity
[[ -f "${CHECK_TOOL}"    ]] || { echo "❌ Missing ${CHECK_TOOL}"; exit 2; }
[[ -f "${TOL_IDENTITY}"  ]] || { echo "❌ Missing ${TOL_IDENTITY}"; exit 2; }
[[ -f "${TOL_REFERENCE}" ]] || { echo "❌ Missing ${TOL_REFERENCE}"; exit 2; }

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

# A prerun step stops at iteration 0 and writes no metrics file.
is_prerun_step() {
  local rundir="$1"
  grep -qiE '^[[:space:]]*is_prerun[[:space:]]*=[[:space:]]*\.true\.' \
       "${rundir}/input_chapsim.ini"
}

run_one() {
  local rundir="$1"
  local expect_metrics="$2"   # yes | no

  pushd "$rundir" > /dev/null || return 2

  if [[ ! -x "$RUN_SCRIPT" ]]; then
    echo "❌ Missing executable ${rundir}/${RUN_SCRIPT}"
    popd > /dev/null
    return 2
  fi

  # Make sure we don't accidentally reuse old metrics
  rm -f "${METRIC_FILE}" || true

  echo "  Running solver in $(pwd)..."
  # stdin must be detached: the step chain is fed to the caller's `while read`
  # loop from a here-string, and mpirun would otherwise swallow it.
  if ! "$RUN_SCRIPT" < /dev/null; then
    popd > /dev/null
    return 1
  fi

  if [[ "$expect_metrics" == "no" ]]; then
    popd > /dev/null
    return 0
  fi

  # Wait for metrics to appear
  local MAX_WAIT_METRIC=200
  local WAITED=0
  echo "  Waiting for ${METRIC_FILE} (timeout ${MAX_WAIT_METRIC}s)..."
  while [[ ! -f "${METRIC_FILE}" && "$WAITED" -lt "$MAX_WAIT_METRIC" ]]; do
      sleep 1
      WAITED=$((WAITED + 1))
  done

  if [[ ! -f "${METRIC_FILE}" ]]; then
    echo "❌ Metrics not generated: ${METRIC_FILE}"
    popd > /dev/null
    return 3
  fi

  popd > /dev/null
  return 0
}

# Copy restart files tagged "<tag>.bin" / "<tag>.dat" from one step to the next.
copy_restart_bins() {
  local src_dir="$1"
  local dst_dir="$2"
  local tag="$3"

  local src="${src_dir}/1_data"
  local dst="${dst_dir}/1_data"

  mkdir -p "$dst"

  shopt -s nullglob
  local files=( "${src}"/*"${tag}".bin "${src}"/*"${tag}".dat )
  shopt -u nullglob

  if [[ ${#files[@]} -eq 0 ]]; then
    echo "❌ No files matched '*${tag}.{bin,dat}' in ${src}"
    return 1
  fi

  cp -f "${files[@]}" "${dst}/"
  return 0
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

    chain_var="CHAIN_${case}"
    CHAIN="${!chain_var:-}"
    if [[ -z "$CHAIN" ]]; then
        echo "[SKIP ] ${case} (no step chain defined)"
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

    CASE_OK=1
    CASE_CHECKED=0
    STEP_NO=0

    while IFS='|' read -r RUNDIR SRCDIR TAG CHECKSPEC; do
        [[ -z "$RUNDIR" ]] && continue
        STEP_NO=$((STEP_NO + 1))

        if [[ ! -d "$RUNDIR" ]]; then
            echo "  ${STEP_NO}) ${RUNDIR}: missing directory"
            CASE_OK=0
            OTHER_FAILED_CASES+=("$case (missing ${RUNDIR})")
            break
        fi

        if [[ "$TEST_MODE" == "run" ]]; then
            export RUN_MODE=functional

            if [[ -n "$SRCDIR" ]]; then
                echo "  ${STEP_NO}) ${RUNDIR}: seed 1_data with *${TAG}.{bin,dat} from ${SRCDIR}"
                if ! copy_restart_bins "$SRCDIR" "$RUNDIR" "$TAG"; then
                    CASE_OK=0
                    OTHER_FAILED_CASES+=("$case (restart copy into ${RUNDIR} failed)")
                    break
                fi
            fi

            EXPECT_METRICS="yes"
            if is_prerun_step "$RUNDIR"; then
                EXPECT_METRICS="no"
                echo "  ${STEP_NO}) ${RUNDIR}: remapping prerun"
            else
                echo "  ${STEP_NO}) ${RUNDIR}: solver run"
            fi

            if run_one "$RUNDIR" "$EXPECT_METRICS"; then
                :
            else
                RUN_EXIT_CODE=$?
                if [[ "$RUN_EXIT_CODE" -eq 1 ]]; then
                    SOLVER_FAILED_CASES+=("$case (${RUNDIR} solver failed)")
                elif [[ "$RUN_EXIT_CODE" -eq 3 ]]; then
                    METRICS_FAILED_CASES+=("$case (${RUNDIR} metrics not generated)")
                else
                    OTHER_FAILED_CASES+=("$case (${RUNDIR} run setup failed)")
                fi
                CASE_OK=0
                break
            fi
        fi

        # ---------------------------------------------------------------------
        # Assertion for this step
        # ---------------------------------------------------------------------
        [[ -z "$CHECKSPEC" ]] && continue

        NEW_JSON="${CASE_DIR}/${RUNDIR}/${METRIC_FILE}"
        if [[ ! -f "$NEW_JSON" ]]; then
            echo "     missing metrics: ${RUNDIR}/${METRIC_FILE}"
            CASE_OK=0
            METRICS_FAILED_CASES+=("$case (${RUNDIR} metrics missing)")
            break
        fi

        if [[ "$CHECKSPEC" == reference ]]; then
            REF_JSON="${CASE_DIR}/${RUNDIR}/reference.json"
            TOL_FILE="${TOL_REFERENCE}"
            echo "     check: ${RUNDIR} vs its stored reference.json"
        else
            REF_DIR="${CHECKSPEC#identity:}"
            REF_JSON="${CASE_DIR}/${REF_DIR}/${METRIC_FILE}"
            TOL_FILE="${TOL_IDENTITY}"
            echo "     check: ${RUNDIR} vs ${REF_DIR} (identity remap)"
        fi

        if [[ ! -f "$REF_JSON" ]]; then
            echo "     missing reference: ${REF_JSON}"
            CASE_OK=0
            METRICS_FAILED_CASES+=("$case (${RUNDIR} reference missing)")
            break
        fi

        CASE_CHECKED=$((CASE_CHECKED + 1))
        if ! python3 "${CHECK_TOOL}" "${NEW_JSON}" "${REF_JSON}" "${TOL_FILE}"; then
            CASE_OK=0
            METRICS_FAILED_CASES+=("$case (${RUNDIR} metrics check failed)")
            break
        fi
    done <<< "$CHAIN"

    if [[ "$CASE_OK" -eq 1 && "$CASE_CHECKED" -gt 0 ]]; then
        echo "[PASS ] ${case} (${CASE_CHECKED} metric comparison(s))"
        PASSED=$((PASSED + 1))
    elif [[ "$CASE_OK" -eq 1 ]]; then
        echo "[SKIP ] ${case} (no metric comparison performed)"
        SKIPPED=$((SKIPPED + 1))
    else
        echo "[FAIL ] ${case}"
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
