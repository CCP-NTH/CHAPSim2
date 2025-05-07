#!/bin/sh

# Print a warning before deleting files
echo "Warning: This will delete all .log, .dat, fort*, .err, and .out files, as well as files in directories starting with 2_, 3_, 4_."
echo "Also, files in 1_data excluding '*outlet*' will be deleted and core dumps will be removed."
read -p "Are you sure you want to continue? (y/n): " confirm

if [ "$confirm" != "y" ]; then
    echo "Cleanup aborted."
    exit 1
fi

# Delete specific file types in the current directory
echo "Deleting .log, .dat, fort*, .err, .out files..."
rm -f *.log *.dat fort* *.err *.out

# Delete directories starting with 2_, 3_, 4_
echo "Deleting directories starting with 2_, 3_, 4_..."
rm -rf 2_* 3_* 4_*

# Clean up files in the 1_data directory excluding '*outlet*'
echo "Cleaning up files in '1_data' directory excluding '*outlet*'..."
find 1_data -type f ! -name "*outlet*" -exec rm -f {} \;

# Remove core dumps
echo "Deleting core dump files..."
rm -f core

echo "Cleanup completed."