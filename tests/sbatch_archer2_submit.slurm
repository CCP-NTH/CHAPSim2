#!/bin/sh
#SBATCH --job-name=job
#SBATCH --output=job%j.out
#SBATCH --error=job%j.err
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=128
#SBATCH --cpus-per-task=1
#SBATCH --account=c01-eng
#SBATCH --partition=standard
#SBATCH --qos=short

export OMP_NUM_THREADS=1

srun --distribution=block:block --hint=nomultithread /work/c01/c01/wwangdl/CHAPSim2/bin/CHAPSim < input_chapsim.ini
