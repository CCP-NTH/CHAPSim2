#!/bin/sh
#SBATCH --job-name=channel
#SBATCH --output=job%j.out
#SBATCH --error=job%j.err
#SBATCH --time=23:00:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=128
#SBATCH --cpus-per-task=1

#SBATCH --account=e01-dares-wang
#SBATCH --partition=standard
#SBATCH --qos=standard

export OMP_NUM_THREADS=1

srun --distribution=block:block --hint=nomultithread /work/e01/e01/wwange01/CHAPSim2/bin/CHAPSim < input_chapsim.ini
