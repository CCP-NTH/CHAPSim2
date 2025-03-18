#!/bin/sh

# Define the base directory for CHAPSim
chap_sim_dir="$HOME_WORK/CHAPSim2"

# Check if input_chapsim.ini exists
if [ ! -f "input_chapsim.ini" ]; then
    echo "input_chapsim.ini not found."
    
    # Check if input_chapsim_by_python.ini exists
    if [ -f "input_chapsim_by_python.ini" ]; then
        cp "input_chapsim_by_python.ini" "input_chapsim.ini"
        echo "input_chapsim_by_python.ini copied to input_chapsim.ini."
    else
        echo "Error: No input_chapsim.ini file found."
        exit 1
    fi
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

# Check if '0_src' directory exists, if not, create it
if [ ! -d "0_src" ]; then
    mkdir "0_src"
    echo "'0_src' directory created."
fi

# Create a timestamped directory for the source files
src_dir="src_${timestamp}"
cp -r "$chap_sim_dir/src" "0_src/$src_dir"

# Ask user for the number of processors (default is 4 if not provided)
read -p "Enter the number of processors (default is 4): " num_processors

# Validate input: Ensure num_processors is a positive integer
if ! [ "$num_processors" -eq "$num_processors" ] 2>/dev/null || [ "$num_processors" -le 0 ]; then
    echo "Invalid input. Using default value: 4"
    num_processors=4
fi

# Ask user if they want to enable Valgrind (default is no)
read -p "Enable Valgrind? (y/n, default is n): " use_valgrind
use_valgrind=${use_valgrind:-n}  # Default to 'n' if empty

# Run CHAPSim normally
nohup mpirun -np "$num_processors" "$chap_sim_dir/bin/CHAPSim" > "$output_file" &

echo "Command is running in the background with $num_processors process(es). Output will be saved to $output_file."

# If user chose Valgrind, run with memory checking
if [ "$use_valgrind" = "y" ]; then
    echo "Running with Valgrind..."
    mpirun -np "$num_processors" valgrind --track-origins=yes --leak-check=full --error-limit=no \
        "$chap_sim_dir/bin/CHAPSim" 2> "valgrind_output_%p.log" &
    echo "Valgrind output will be saved in valgrind_output_<PID>.log"
fi
