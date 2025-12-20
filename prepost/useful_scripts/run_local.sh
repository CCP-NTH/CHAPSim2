#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# CHAPSim2 Runner Script with Input File Management
# =============================================================================
# 
# ## Overview
# This script automates the execution of CHAPSim2 CFD simulations with 
# integrated input file management. It provides functionality to generate, 
# regenerate, and manage CHAPSim2 input configuration files while handling
# source code archiving, process management, and debugging options.
#
# ## Key Features
# * **Automatic input file management:** Detects and manages input_chapsim.ini files
# * **Interactive file generation:** Uses autoinput.py utility for configuration
# * **Source code archiving:** Timestamps and archives source files
# * **Multi-processor support:** Configurable parallel execution
# * **Debug integration:** Valgrind and LLDB debugging options
# * **Process management:** Background execution with PID tracking
# * **Safe file handling:** Preserves original files during regeneration
#
# ## System Requirements
# * **Shell:** Unix/Linux shell environment
# * **Python:** Version 3.6+ (for input file generation)
# * **MPI:** Implementation for parallel execution
# * **CHAPSim2:** Compiled binary
# * **Optional:** Valgrind, LLDB for debugging
#
# ##  Usage
# 1. **Configure** the `chapsim_dir` variable below to point to your CHAPSim2 installation
# 2. **Execute** the script:
#    ```bash
#    ./run_chapsim.sh
#    ```
# 3. **Follow prompts** for:
#    - Input file generation/regeneration
#    - Number of processors
#    - Debug options
#
# ## Input File Management Workflow
# * **Detection:** Automatically detects existing `input_chapsim.ini` files
# * **Generation:** Creates new configuration via `autoinput.py` → `input_chapsim_auto.ini`
# * **Safety:** Asks before replacing existing configuration files
# * **Preservation:** Keeps original files for comparison and rollback
#
# ## 🔧 Configuration
# **Primary Settings:**
# * `chapsim_dir`: Path to CHAPSim2 installation directory
# * `autoinput.py`: Located at `$chapsim_dir/prepost/autoinput/autouty.py`
# * **Output files:** Timestamped logs in current directory
# * **Source backup:** Archived in `0_src/` with timestamps
#
# ## License
# This script is released under the **BSD 3-Clause License**.
# 
# For CHAPSim2 license information, visit: https://github.com/CHAPSim/CHAPSim2
#
# ## Author Information
# **Wei Wang** - Senior Computational Scientist  
# Scientific Computing Department, UKRI-STFC  
# 📧 Email: wei.wang@stfc.ac.uk  
# =============================================================================

# Define the base directory for CHAPSim
# chapsim_dir="/Users/wei.wang/Work_RSDevelopment/1_CHAPSim/CHAPSim2"
chapsim_dir="/Users/wei.wang/Work_RSDevelopment/1_CHAPSim/CHAPSim2"

# Check if input_chapsim.ini exists and ask user if they want to regenerate it
iniautogen_file="$chapsim_dir/prepost/autoinput/autoinput.py"
if [ -f "input_chapsim.ini" ]; then
    echo "File 'input_chapsim.ini' already exists."
    read -p "Do you want to regenerate it? (y/n, default is n): " regenerate
    regenerate=${regenerate:-n}  # Default to 'n' if empty
    
    if [ "$regenerate" = "y" ] || [ "$regenerate" = "Y" ]; then
        echo "Regenerating input configuration..."
        python3 $iniautogen_file
        if [ $? -eq 0 ]; then
            echo "New configuration generated as input_chapsim_auto.ini"
            read -p "Do you want to replace the old input_chapsim.ini with the new one? (y/n, default is n): " replace
            replace=${replace:-n}  # Default to 'n' if empty
            
            if [ "$replace" = "y" ] || [ "$replace" = "Y" ]; then
                cp input_chapsim_auto.ini input_chapsim.ini
                echo "input_chapsim.ini has been replaced with the new configuration."
            else
                echo "Keeping existing input_chapsim.ini file. New configuration saved as input_chapsim_auto.ini"
            fi
        else
            echo "Error: Failed to regenerate input configuration"
            exit 1
        fi
    else
        echo "Using existing input_chapsim.ini file."
    fi
else
    echo "File 'input_chapsim.ini' not found."
    read -p "Do you want to generate it? (y/n, default is y): " generate
    generate=${generate:-y}  # Default to 'y' if empty
    
    if [ "$generate" = "y" ] || [ "$generate" = "Y" ]; then
        echo "Generating input_chapsim.ini..."
        python3 $iniautogen_file
        if [ $? -eq 0 ]; then
            cp input_chapsim_auto.ini input_chapsim.ini
            echo "input_chapsim.ini has been generated successfully."
        else
            echo "Error: Failed to generate input_chapsim.ini"
            exit 1
        fi
    else
        echo "The input_chapsim.ini file is required to run CHAPSim."
    fi
fi

# Ask user if they want to review the input file before proceeding
echo ""
read -p "Do you want to review the input file before running the simulation? (y/n, default is n): " review_input
review_input=${review_input:-n}  # Default to 'n' if empty

if [ "$review_input" = "y" ] || [ "$review_input" = "Y" ]; then
    echo "Please review your input file (input_chapsim.ini) and run this script again when ready."
    echo "Script will now exit for input file review."
    exit 0
else
    echo "Proceeding with simulation setup..."
fi

# Define base filename with timestamp
timestamp=$(date +'%Y-%m-%d_%H.%M')
base_filename="output_chapsim2_${timestamp}.log"
output_file="$base_filename"

# Ensure unique output filename
count=2
while [ -e "$output_file" ]; do
    output_file="${base_filename%.*}_$count.${base_filename##*.}"
    count=$((count + 1))
done



# Ask user if they want to backup the source code locally
echo ""
read -p "Do you want to backup the source code locally? (y/n, default is n): " src_backup
src_backup=${review_input:-n}  # Default to 'n' if empty

if [ "$src_backup" = "y" ] || [ "$src_backup" = "Y" ]; then
    mkdir -p "0_src"
    src_dir="src_${timestamp}"
    cp -r "$chapsim_dir/src" "0_src/$src_dir"
    echo "Source files copied to: $src_dir"
fi

# If an argument is provided, use it as the number of processors
if [ -n "$1" ]; then
    num_processors="$1"
else
    # No argument provided: ask the user
    read -p "Enter the number of processors (default is 1): " num_processors
    num_processors=${num_processors:-1}
fi

echo "Using $num_processors processor(s)..."

# Validate input: Ensure num_processors is a positive integer
if ! [ "$num_processors" -eq "$num_processors" ] 2>/dev/null || [ "$num_processors" -le 0 ]; then
    echo "Invalid input. Using default value: 1"
    num_processors=1
fi

# Ask user if they want to enable Valgrind or LLDB (default is no)
read -p "Enable Valgrind or LLDB? (y/n, default is n): " debug
debug=${debug:-n}  # Default to 'n' if empty

if [ "$debug" = "y" ]; then
    read -p "Choose LLDB [1] or Valgrind [2] (default is LLDB): " debug_option
    debug_option=${debug_option:-1}  # Default to LLDB if empty
    
    if [ "$debug_option" -eq 2 ]; then
        echo "Running with Valgrind..."
        mpirun -np "$num_processors" valgrind --track-origins=yes --leak-check=full --error-limit=no \
            "$chapsim_dir/bin/CHAPSim" 
        echo "Valgrind output will be saved in valgrind_output_<PID>.log"
    else
        echo "Running with LLDB..."
        echo "LLDB commands: run, bt (backtrace), q (quit)..."
        mpirun -np "$num_processors" xterm -e lldb -- "$chapsim_dir/bin/CHAPSim"
    fi
else
    # Run CHAPSim normally
    echo "Running CHAPSim with $num_processors processor(s)..."
    nohup mpirun -np "$num_processors" "$chapsim_dir/bin/CHAPSim" > "$output_file" 2>&1 & 
    mpirun_pid=$!
    echo "CHAPSim started with PID: $mpirun_pid"
    echo "Output is being logged to: $output_file"
fi
