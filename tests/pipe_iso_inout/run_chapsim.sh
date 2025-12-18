#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------
# Clean previous outputs
# -------------------------------------------------
rm -f *.log *.dat fort* *.err *.out regression_test_metrics.json
rm -rf 2_*/ 3_*/ 4_*/

# -------------------------------------------------
# Executable and MPI size
# -------------------------------------------------
EXEC="../../bin/CHAPSim"
NP=${NP:-4}   # allow override: NP=128 ./run.sh

timestamp=$(date +'%Y-%m-%d_%H.%M')
OUTPUT="output_chapsim2_${timestamp}.log"

# -------------------------------------------------
# Select launcher automatically
# -------------------------------------------------
if [[ -n "${SLURM_JOB_ID:-}" ]]; then
    echo ">>> Running under SLURM (srun)"
    LAUNCHER="srun"
    CMD="srun --distribution=block:block --hint=nomultithread ${EXEC}"

elif command -v mpirun >/dev/null 2>&1; then
    echo ">>> Running with mpirun"
    LAUNCHER="mpirun"
    CMD="mpirun -np ${NP} ${EXEC}"

else
    echo ">>> Running in serial mode"
    LAUNCHER="serial"
    CMD="${EXEC}"
fi

echo ">>> Launcher : ${LAUNCHER}"
echo ">>> Command  : ${CMD}"
echo ">>> Output   : ${OUTPUT}"

# -------------------------------------------------
# Run
# -------------------------------------------------
${CMD} > "${OUTPUT}" 2>&1
