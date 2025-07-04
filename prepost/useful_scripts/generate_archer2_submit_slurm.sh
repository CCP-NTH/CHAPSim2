#!/bin/bash

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

# Generate SLURM job script with user inputs
cat << EOF
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
