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
#if ! ask_yes_no "Delete all log and data files?" "y"; then
#    echo "Cleanup aborted."
#    exit 1
#fi

# Ask about deleting 0_src folder
delete_0_src=false
if [ -d "0_src" ]; then
    #if ask_yes_no "Delete '0_src' folder?" "n"; then
        delete_0_src=true
    #fi
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

# Clean 1_data: keep only *outlet* and domain0*
if [ -d "1_data" ]; then
    echo "Cleaning 1_data (keeping *outlet* and domain0*)..."
    find 1_data -type f ! \( -name "*outlet*" -o -name "domain0*" \) -exec rm -f {} +
    find 1_data -type d -empty -delete
fi

# Delete 0_src folder if requested
if [ "$delete_0_src" = true ]; then
    echo "Deleting '0_src' directory..."
    rm -rf 0_src
fi

# Remove core dumps
echo "Deleting core dump files..."
rm -f core

echo "Cleanup completed."
