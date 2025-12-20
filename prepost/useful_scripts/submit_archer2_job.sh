#!/usr/bin/env bash
set -euo pipefail

SLURM_FILE="sbatch_archer2_submit.slurm"

# Check if SLURM file exists
if [[ -f "$SLURM_FILE" ]]; then
    echo "$SLURM_FILE already exists."
    read -p "Do you want to regenerate it? (y/n) [default: n]: " regenerate
    regenerate=${regenerate:-n}
else
    regenerate="y"
fi

# If regeneration is needed, prompt for job parameters and generate SLURM file
if [[ "$regenerate" == "y" || "$regenerate" == "Y" ]]; then
    echo "Generating $SLURM_FILE..."

    # Prompt user for job name with default
    read -p "Enter job name [default: job]: " job_name
    job_name=${job_name:-job}

    # Prompt user for job time with default
    read -p "Enter job time [default: 23:00:00]: " job_time
    job_time=${job_time:-23:00:00}

    # Prompt user for number of nodes with default
    read -p "Enter number of nodes [default: 1]: " job_nodes
    job_nodes=${job_nodes:-1}

    # Prompt user for SLURM account
    default_account="e01-dares-wang"
    read -p "Enter SLURM account [default: ${default_account}]: " user_account
    account=${user_account:-$default_account}

    # Prompt user for CHAPSim executable path
    default_exec_path="/work/e01/e01/wwange01/CHAPSim2"
    read -p "Enter CHAPSim executable path [default: ${default_exec_path}]: " user_exec_path
    exec_path=${user_exec_path:-$default_exec_path}

    # Write SLURM script
    cat << EOF > "$SLURM_FILE"
#!/bin/sh
#SBATCH --job-name=${job_name}
#SBATCH --output=job%j.out
#SBATCH --error=job%j.err
#SBATCH --time=${job_time}
#SBATCH --nodes=${job_nodes}
#SBATCH --tasks-per-node=128
#SBATCH --cpus-per-task=1

#SBATCH --account=${account}
#SBATCH --partition=standard
#SBATCH --qos=standard

export OMP_NUM_THREADS=1

srun --distribution=block:block --hint=nomultithread ${exec_path}/bin/CHAPSim < input_chapsim.ini
EOF

    echo "$SLURM_FILE generated."
else
    echo "Using existing $SLURM_FILE."
fi

# Ask if user wants to back up source code
read -p "Do you want to back up source code from CHAPSim2/src to local ./0_src/? [y/N]: " backup_answer
backup_answer=${backup_answer:-n}

if [[ "$backup_answer" =~ ^[Yy]$ ]]; then
    SRC_DIR="/work/e01/e01/wwange01/CHAPSim2/src"
    DEST_DIR="./0_src"

    echo "Backing up source code from $SRC_DIR to $DEST_DIR..."
    mkdir -p "$DEST_DIR"
    cp -r "$SRC_DIR"/* "$DEST_DIR"/

    if [[ $? -eq 0 ]]; then
        echo "Backup completed successfully."
    else
        echo "Warning: Backup may have failed."
    fi
else
    echo "No backup performed."
fi

# Ask if user wants to submit the job
read -p "Do you want to submit the job now? [Y/n]: " submit_answer
submit_answer=${submit_answer:-y}

if [[ "$submit_answer" =~ ^[Yy]$ ]]; then
    echo "Submitting job with sbatch..."
    sbatch "$SLURM_FILE"
else
    echo "Job submission skipped."
fi
