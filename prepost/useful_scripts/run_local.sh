#!/bin/sh

# Define the base directory for CHAPSim
chap_sim_dir="/Users/wei.wang/Work_RSDevelopment/1_CHAPSim/CHAPSim2"

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

# Ensure '0_src' directory exists
mkdir -p "0_src"

# Create a timestamped directory for the source files
src_dir="src_${timestamp}"
cp -r "$chap_sim_dir/src" "0_src/$src_dir"
echo "Source files copied to: $src_dir"

# Ask user for the number of processors (default is 1 if not provided)
read -p "Enter the number of processors (default is 1): " num_processors
num_processors=${num_processors:-1}

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
            "$chap_sim_dir/bin/CHAPSim" 
        echo "Valgrind output will be saved in valgrind_output_<PID>.log"
    else
        echo "Running with LLDB..."
        echo "LLDB commands: run, bt (backtrace), q (quit)..."
        mpirun -np "$num_processors" xterm -e lldb -- "$chap_sim_dir/bin/CHAPSim"
    fi
else
    # Run CHAPSim normally
    echo "Running CHAPSim with $num_processors processor(s)..."
    nohup mpirun -np "$num_processors" "$chap_sim_dir/bin/CHAPSim" > "$output_file" 2>&1 & 
    mpirun_pid=$!
    echo "CHAPSim started with PID: $mpirun_pid"
    echo "Output is being logged to: $output_file"
fi