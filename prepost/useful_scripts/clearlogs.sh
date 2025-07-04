#!/bin/sh

# Function to ask yes/no questions with default
ask_yes_no() {
    local prompt="$1"
    local default="$2"
    local response
    
    if [ "$default" = "y" ]; then
        prompt="$prompt (Y/n): "
    else
        prompt="$prompt (y/N): "
    fi
    
    read -p "$prompt" response
    
    # If empty response, use default
    if [ -z "$response" ]; then
        response="$default"
    fi
    
    # Convert to lowercase for comparison
    response=$(echo "$response" | tr '[:upper:]' '[:lower:]')
    
    [ "$response" = "y" ] || [ "$response" = "yes" ]
}

# Print a warning before deleting files
echo "Warning: This will delete all .log, .dat, fort*, .err, and .out files, as well as files in directories starting with 2_, 3_, 4_."
echo "Also, files in 1_data excluding '*outlet*' will be deleted and core dumps will be removed."

if ! ask_yes_no "Are you sure you want to continue?" "n"; then
    echo "Cleanup aborted."
    exit 1
fi

# Ask about deleting 0_src folder
delete_0_src=false
if [ -d "0_src" ]; then
    if ask_yes_no "Delete '0_src' folder?" "n"; then
        delete_0_src=true
    fi
fi

# Ask about deleting *outlet* files in 1_data folder
delete_outlet_files=false
outlet_files_exist=false
if [ -d "1_data" ]; then
    # Check if any *outlet* files exist
    if find 1_data -name "*outlet*" -print -quit | grep -q .; then
        outlet_files_exist=true
        if ask_yes_no "Delete files containing '*outlet*' in '1_data' folder?" "n"; then
            delete_outlet_files=true
        fi
    fi
fi

# Delete specific file types in the current directory
echo "Deleting .log, .dat, fort*, .err, .out files..."
rm -f *.log *.dat fort* *.err *.out

# Delete directories starting with 2_, 3_, 4_
echo "Deleting directories starting with 2_, 3_, 4_..."
for prefix in 2_ 3_ 4_; do
  for dir in ${prefix}*/ ; do
    if [ -d "$dir" ]; then
      echo "Cleaning directory: $dir"
      # Delete all files except .py and .sh
      find "$dir" -type f ! \( -name "*.py" -o -name "*.sh" \) -exec rm -f {} +

      # Optionally, delete empty subdirectories
      find "$dir" -type d -empty -delete
    fi
  done
done

# Delete 0_src folder if requested
if [ "$delete_0_src" = true ]; then
    echo "Deleting '0_src' directory..."
    rm -rf 0_src
fi

# Clean up files in the 1_data directory
if [ -d "1_data" ]; then
    if [ "$delete_outlet_files" = true ]; then
        echo "Cleaning up all files in '1_data' directory..."
        find 1_data -type f -exec rm -f {} \;
    else
        echo "Cleaning up files in '1_data' directory excluding '*outlet*'..."
        find 1_data -type f ! -name "*outlet*" -exec rm -f {} \;
    fi
fi

# Remove core dumps
echo "Deleting core dump files..."
rm -f core

echo "Cleanup completed."
