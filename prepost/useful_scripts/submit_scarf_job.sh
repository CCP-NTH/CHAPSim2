#!/usr/bin/env bash
set -euo pipefail

SLURM_FILE="sbatch_scarf_submit.slurm"

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
    read -p "Enter job time [default: 11:30:00]: " job_time
    job_time=${job_time:-23:00:00}

    # Prompt user for number of nodes with default
    read -p "Enter number of procs [default: 1]: " job_procs
    job_procs=${job_procs:-1}

    # Prompt user for CHAPSim executable path
    default_exec_path="/work4/scd/scarf909/CHAPSim/CHAPSim2"
    read -p "Enter CHAPSim executable path [default: ${default_exec_path}]: " user_exec_path
    exec_path=${user_exec_path:-$default_exec_path}

    # Write SLURM script
    cat << EOF > "$SLURM_FILE"
#!/bin/bash
#SBATCH --job-name=${job_name}           # Job name
#SBATCH --output=${job_name}%j.out       # Standard output (%j = job ID)
#SBATCH --error=${job_name}%j.err        # Standard error
#SBATCH --time=${job_time}               # Run time (hh:mm:ss)
#SBATCH --ntasks=${job_procs}            # Total number of MPI tasks
#SBATCH --partition=scarf                # Partition name
##SBATCH --constraint="[scarf15|scarf16|scarf17|scarf18]"  # Optional node constraint

# Load MPI module if needed (only if modules are used on your system)
# module load openmpi

# Correct way to launch an MPI program with SLURM
srun ${exec_path}/bin/CHAPSim

EOF

    echo "$SLURM_FILE generated."
else
    echo "Using existing $SLURM_FILE."
fi

# Ask if user wants to back up source code
read -p "Do you want to back up source code from CHAPSim2/src to local ./0_src/? [y/N]: " backup_answer
backup_answer=${backup_answer:-n}

if [[ "$backup_answer" =~ ^[Yy]$ ]]; then
    SRC_DIR=${exec_path}/src
    DEST_DIR=./0_src

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
