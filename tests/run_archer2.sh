#!/bin/bash
set -euo pipefail

# -----------------------------
# Configuration (non-interactive defaults)
#
# ACCOUNT and EXEC_PATH are site-specific: set them here, or export them
# before running, to your own ARCHER2 project allocation and installation.
# -----------------------------
SLURM_FILE="sbatch_archer2_submit.slurm"
JOB_NAME="job"
JOB_TIME="00:10:00"
JOB_NODES=1
TASK_NODE=128
ACCOUNT="${ACCOUNT:-your-archer2-project}"
EXEC_PATH="${EXEC_PATH:-/path/to/CHAPSim2}"
SRC_DIR="${EXEC_PATH}/src"
SUBMIT_JOB=true        # true to automatically submit job

# -----------------------------
# Generate SLURM file (overwrite if exists)
# -----------------------------
echo "Generating $SLURM_FILE..."

cat << EOF > "$SLURM_FILE"
#!/bin/sh
#SBATCH --job-name=${JOB_NAME}
#SBATCH --output=job%j.out
#SBATCH --error=job%j.err
#SBATCH --time=${JOB_TIME}
#SBATCH --nodes=${JOB_NODES}
#SBATCH --tasks-per-node=${TASK_NODE}
#SBATCH --cpus-per-task=1
#SBATCH --account=${ACCOUNT}
#SBATCH --partition=standard
#SBATCH --qos=short

export OMP_NUM_THREADS=1

srun --distribution=block:block --hint=nomultithread ${EXEC_PATH}/bin/CHAPSim < input_chapsim.ini
EOF

echo "$SLURM_FILE generated."

# -----------------------------
# Submit the job if enabled
# -----------------------------
if [[ "$SUBMIT_JOB" = true ]]; then
    echo "Submitting job with sbatch..."
    sbatch "$SLURM_FILE"
else
    echo "Job submission skipped."
fi
