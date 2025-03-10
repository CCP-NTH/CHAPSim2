#!/bin/bash

# -----------------------------------------------------------------------------
# CHAPSim2 Build Script
# This script compiles the CHAPSim2 project, handles dependencies, and offers
# options for cleaning builds and enabling debug mode.
# -----------------------------------------------------------------------------

# Define relative paths
REL_PATH_LIB="./lib/2decomp-fft/build"
REL_PATH_BUILD="./build"
REL_PATH_BIN="./bin"
LIB_FILE="$REL_PATH_LIB/opt/lib/libdecomp2d.a"
LIB_FILE_64="$REL_PATH_LIB/opt/lib64/libdecomp2d.a"

# Ensure necessary directories exist
for dir in "$REL_PATH_LIB" "$REL_PATH_BUILD" "$REL_PATH_BIN"; do
    if [ ! -d "$dir" ]; then
        echo "Creating directory: $dir"
        mkdir -p "$dir" || { echo "Error: Failed to create $dir"; exit 1; }
    fi
done

# Determine which library file to use
if [ -f "$LIB_FILE_64" ]; then
    LIB_FILE="$LIB_FILE_64"
    echo "Using lib64 version: $LIB_FILE"
else
    echo "Using lib version: $LIB_FILE"
fi

# Resolve absolute paths
PATH_LIB=$(realpath "$REL_PATH_LIB" 2>/dev/null || readlink -f "$REL_PATH_LIB")
PATH_BUILD=$(realpath "$REL_PATH_BUILD" 2>/dev/null || readlink -f "$REL_PATH_BUILD")
PATH_BIN=$(realpath "$REL_PATH_BIN" 2>/dev/null || readlink -f "$REL_PATH_BIN")

# -----------------------------------------------------------------------------
# Function to get yes/no input
get_yes_no_input() {
    local prompt="$1"
    local default="$2"
    read -p "$prompt [$default]: " input
    input=$(echo "$input" | tr '[:upper:]' '[:lower:]')  # Convert input to lowercase using tr

    # Default to the given default if input is empty
    input=${input:-$default}

    # Check if the input is valid
    if [[ "$input" != "yes" && "$input" != "no" && "$input" != "y" && "$input" != "n" ]]; then
        input="no"  # If invalid input, default to "no"
    fi

    echo "$input"
}
# -----------------------------------------------------------------------------
# Step 1: Check and Build the Library
# -----------------------------------------------------------------------------
if [[ -f "$LIB_FILE" ]]; then
    LIB_REBUILD=$(get_yes_no_input "Rebuild 2decomp library?" "no")
    if [[ "$LIB_REBUILD" =~ ^(yes|y)$ ]]; then
        cd "$PATH_LIB" || { echo "Error: Cannot access $PATH_LIB"; exit 1; }
        ./build_cmake.sh || { echo "Error: CMake build failed in $PATH_LIB"; exit 1; }
    fi
else
    echo "Library not found. Running CMake in $PATH_LIB..."
    cd "$PATH_LIB" || { echo "Error: Cannot access $PATH_LIB"; exit 1; }
    ./build_cmake.sh || { echo "Error: CMake build failed in $PATH_LIB"; exit 1; }
fi

# -----------------------------------------------------------------------------
# Step 2: Prompt for Build Options
# -----------------------------------------------------------------------------
CLEAN_BUILD=$(get_yes_no_input "Run 'make clean' before compiling CHAPSim?" "no")

if [[ "$CLEAN_BUILD" == "only" ]]; then
    echo "Running 'make clean' in $PATH_BUILD..."
    cd "$PATH_BUILD" || { echo "Error: Cannot access $PATH_BUILD"; exit 1; }
    make clean || echo "Warning: 'make clean' failed."
    exit 0
fi

DEBUG_MODE=$(get_yes_no_input "Run in debug mode?" "no")

MAKE_TARGET="make"
[[ "$DEBUG_MODE" =~ ^(yes|y)$ ]] && MAKE_TARGET="make cfg=gnu"
[[ "$CLEAN_BUILD" =~ ^(yes|y)$ ]] && MAKE_TARGET="make clean && $MAKE_TARGET all"

# -----------------------------------------------------------------------------
# Step 3: Run the Build
# -----------------------------------------------------------------------------
echo "Executing: $MAKE_TARGET in $PATH_BUILD..."
cd "$PATH_BUILD" || { echo "Error: Cannot access $PATH_BUILD"; exit 1; }
eval $MAKE_TARGET || { echo "Error: Build failed in $PATH_BUILD."; exit 1; }

# -----------------------------------------------------------------------------
# Completion Message
# -----------------------------------------------------------------------------
echo "✅ CHAPSim2 successfully compiled."