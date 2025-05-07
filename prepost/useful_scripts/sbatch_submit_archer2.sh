#!/bin/sh
# slurm job options (job-name, compute nodes, job time)
#SBATCH --job-name=channel6cp
#SBATCH --output=job%j.out
#SBATCH --error=job%j.err
#SBATCH --time 24:00:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=128
#SBATCH --cpus-per-task=1

# Place [budget code] below with your budget code
#SBATCH --account=e814-stfc
#SBATCH --partition=standard
#SBATCH --qos=standard #short  #standard

# Set the number of threads to 1
#   This prevents any threaded system libraries from automatically
#   using threading.
export OMP_NUM_THREADS=1

# Launch the parallel job
#   Using 512 MPI processes and 128 MPI processes per node
#   srun picks up the distribution from the sbatch options

srun --distribution=block:block --hint=nomultithread /work/e814/e814/wwange814/CHAPSim2/bin/CHAPSim < input_chapsim.ini  

