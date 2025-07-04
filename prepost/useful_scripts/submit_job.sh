#!/bin/bash

SLURM_FILE="sbatch_archer2_submit.slurm"

if [[ -f "$SLURM_FILE" ]]; then
    echo "$SLURM_FILE already exists."
    read -p "Do you want to regenerate it? (y/n) [default: n]: " regenerate
    regenerate=${regenerate:-n}
else
    regenerate="y"
fi

if [[ "$regenerate" == "y" || "$regenerate" == "Y" ]]; then
    echo "Generating $SLURM_FILE..."
    bash sbatch_archer2_interactive.sh > "$SLURM_FILE"
    if [[ $? -ne 0 ]]; then
        echo "Error: Failed to generate $SLURM_FILE."
        exit 1
    fi
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

    # Create destination directory if it doesn't exist
    mkdir -p "$DEST_DIR"

    # Copy source files (preserving directory structure)
    cp -r "$SRC_DIR"/* "$DEST_DIR"/

    if [[ $? -eq 0 ]]; then
        echo "Backup completed successfully."
    else
        echo "Warning: Backup may have failed."
    fi
else
    echo "No backup performed."
fi

echo "Submitting job with sbatch..."
sbatch "$SLURM_FILE"
