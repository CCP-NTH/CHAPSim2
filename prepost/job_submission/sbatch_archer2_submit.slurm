#!/bin/sh
#
# ARCHER2 example. Before submitting, set the account on the --account line
# to your own project allocation and point CHAPSIM_DIR at your installation.
#
#SBATCH --job-name=channel
#SBATCH --output=job%j.out
#SBATCH --error=job%j.err
#SBATCH --time=23:00:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=128
#SBATCH --cpus-per-task=1

#SBATCH --account=your-archer2-project
#SBATCH --partition=standard
#SBATCH --qos=standard

export OMP_NUM_THREADS=1

CHAPSIM_DIR=${CHAPSIM_DIR:-/path/to/CHAPSim2}

srun --distribution=block:block --hint=nomultithread ${CHAPSIM_DIR}/bin/CHAPSim < input_chapsim.ini
