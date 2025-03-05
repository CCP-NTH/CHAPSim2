#!/bin/bash

# -----------------------------------------------------------------------------
# CHAPSim2 Build Script
# This script compiles the CHAPSim2 project while providing options for clean builds
# and debug mode configuration. It handles library dependencies and user interaction.
# -----------------------------------------------------------------------------

# Define relative paths to the directories containing the Makefiles
REL_PATH_LIB="./lib/2decomp-fft/build"
REL_PATH_BUILD="./build"
REL_PATH_BIN="./bin"
LIB_FILE="$REL_PATH_LIB/opt/lib/libdecomp2d.a"

# Resolve absolute paths
PATH_LIB=$(realpath "$REL_PATH_LIB" 2>/dev/null || readlink -f "$REL_PATH_LIB")
PATH_BUILD=$(realpath "$REL_PATH_BUILD" 2>/dev/null || readlink -f "$REL_PATH_BUILD")
PATH_BIN=$(realpath "$REL_PATH_BIN" 2>/dev/null || readlink -f "$REL_PATH_BIN")

# Ensure the bin folder exists
if [[ ! -d "$PATH_BIN" ]]; then
  echo "Creating bin directory: $PATH_BIN"
  mkdir -p "$PATH_BIN" || { echo "Error: Failed to create bin directory"; exit 1; }
fi

# -----------------------------------------------------------------------------
# Step 1: Check and Build the Library
# -----------------------------------------------------------------------------
if [[ -f "$LIB_FILE" ]]; then
  echo "Library found: $LIB_FILE (Skipping build)"
else
  echo "Library not found. Running CMake in $PATH_LIB..."
  cd "$PATH_LIB" || { echo "Error: Cannot access $PATH_LIB"; exit 1; }
  ./build_cmake.sh || { echo "Error: CMake build failed in $PATH_LIB"; exit 1; }
fi

# -----------------------------------------------------------------------------
# Step 2: Prompt for Build Options
# -----------------------------------------------------------------------------
read -p "Run 'make clean' before compiling CHAPSim? (yes/no/only): " CLEAN_BUILD
CLEAN_BUILD=$(echo "$CLEAN_BUILD" | tr '[:upper:]' '[:lower:]') # Normalize input

case "$CLEAN_BUILD" in
  yes|y) CLEAN_BUILD="yes" ;;
  no|n) CLEAN_BUILD="no" ;;
  only) CLEAN_BUILD="only" ;;
  *) echo "Invalid input. Please enter 'yes', 'no', or 'only'."; exit 1 ;;
esac

if [[ "$CLEAN_BUILD" == "only" ]]; then
  echo "Running 'make clean' in $PATH_BUILD..."
  cd "$PATH_BUILD" || { echo "Error: Cannot access $PATH_BUILD"; exit 1; }
  make clean || echo "Warning: 'make clean' failed in $PATH_BUILD."
  exit 0
fi

read -p "Run in debug mode? (yes/no): " DEBUG_MODE
DEBUG_MODE=$(echo "$DEBUG_MODE" | tr '[:upper:]' '[:lower:]')

case "$DEBUG_MODE" in
  yes|y) DEBUG_MODE="yes"; MAKE_TARGET="make cfg=gnu" ;;
  no|n) DEBUG_MODE="no"; MAKE_TARGET="make" ;;
  *) echo "Invalid input. Please enter 'yes' or 'no'."; exit 1 ;;
esac

# -----------------------------------------------------------------------------
# Step 3: Configure the Build Target
# -----------------------------------------------------------------------------
if [[ "$CLEAN_BUILD" == "yes" ]]; then
  MAKE_TARGET="make clean && $MAKE_TARGET all"
fi

# -----------------------------------------------------------------------------
# Step 4: Run the Build
# -----------------------------------------------------------------------------
echo "Running '$MAKE_TARGET' in $PATH_BUILD..."
cd "$PATH_BUILD" || { echo "Error: Cannot access $PATH_BUILD"; exit 1; }
eval $MAKE_TARGET || { echo "Error: Build failed in $PATH_BUILD. Exiting."; exit 1; }

# -----------------------------------------------------------------------------
# Completion Message
# -----------------------------------------------------------------------------
echo "✅ CHAPSim2 successfully compiled."
