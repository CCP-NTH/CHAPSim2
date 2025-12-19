#!/usr/bin/env bash
set -euo pipefail
# =============================================================================
# CHAPSim2 Build System v1.0 (non-interactive version)
# Automatically builds solver using default options
# =============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DECOMP_GIT_URL="https://github.com/2decomp-fft/2decomp-fft.git"
REL_PATH_LIB="$SCRIPT_DIR/lib/2decomp-fft/build"
REL_PATH_LIB_ROOT="$SCRIPT_DIR/lib/2decomp-fft"
REL_PATH_BUILD="$SCRIPT_DIR/build"
REL_PATH_BIN="$SCRIPT_DIR/bin"
LIB_FILE="$REL_PATH_LIB/opt/lib/libdecomp2d.a"
LIB_FILE_64="$REL_PATH_LIB/opt/lib64/libdecomp2d.a"

# Ensure directories exist
mkdir -p "$REL_PATH_BUILD" "$REL_PATH_BIN"

# Use 64-bit library if present
if [ -f "$LIB_FILE_64" ]; then
    LIB_FILE="$LIB_FILE_64"
    echo "Using lib64: $LIB_FILE"
fi

PATH_LIB=$(realpath "$REL_PATH_LIB" 2>/dev/null || readlink -f "$REL_PATH_LIB")
PATH_LIB_ROOT=$(realpath "$REL_PATH_LIB_ROOT" 2>/dev/null || readlink -f "$REL_PATH_LIB_ROOT")
PATH_BUILD=$(realpath "$REL_PATH_BUILD" 2>/dev/null || readlink -f "$REL_PATH_BUILD")
PATH_BIN=$(realpath "$REL_PATH_BIN" 2>/dev/null || readlink -f "$REL_PATH_BIN")

# -----------------------------------------------------------------------------
# Clone or update 2decomp-fft repo (non-interactive)
# -----------------------------------------------------------------------------
if [ ! -d "$PATH_LIB_ROOT/.git" ]; then
    echo "Cloning 2decomp-fft..."
    git clone "$DECOMP_GIT_URL" "$PATH_LIB_ROOT"
else
    echo "Updating 2decomp-fft..."
    git -C "$PATH_LIB_ROOT" fetch origin
    git -C "$PATH_LIB_ROOT" reset --hard origin/$(git -C "$PATH_LIB_ROOT" rev-parse --abbrev-ref HEAD)
fi

# -----------------------------------------------------------------------------
# Build 2decomp-fft library (always non-interactive)
# -----------------------------------------------------------------------------
mkdir -p "$REL_PATH_LIB"
cd "$REL_PATH_LIB"
if [ ! -f build_cmake_2decomp.sh ]; then
    # Copy from project build or repo root
    if [ -f "$REL_PATH_BUILD/build_cmake_2decomp.sh" ]; then
        cp "$REL_PATH_BUILD/build_cmake_2decomp.sh" .
    elif [ -f "$REL_PATH_LIB_ROOT/build_cmake_2decomp.sh" ]; then
        cp "$REL_PATH_LIB_ROOT/build_cmake_2decomp.sh" .
    else
        echo "Error: build_cmake_2decomp.sh not found"; exit 1
    fi
fi
chmod +x build_cmake_2decomp.sh
./build_cmake_2decomp.sh || { echo "Error: CMake build failed"; exit 1; }

# -----------------------------------------------------------------------------
# Build CHAPSim2 solver (non-interactive, default build mode)
# -----------------------------------------------------------------------------
cd "$PATH_BUILD"
# -----------------------------------------------------------------------------
# Build CHAPSim2 solver (non-interactive, default build mode)
# -----------------------------------------------------------------------------
cd "$PATH_BUILD"
echo "Cleaning previous builds and compiling solver..."
make clean || echo "Warning: make clean failed, continuing..."
make all || { echo "Error: Solver build failed"; exit 1; }

echo "✅ CHAPSim2 successfully compiled (non-interactive)"
