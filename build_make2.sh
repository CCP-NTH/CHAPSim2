#!/usr/bin/env bash
set -euo pipefail
# =============================================================================
# CHAPSim2 Build System v1.0
# 
# Description:
#   Automated compilation and dependency management system for the CHAPSim2 
#   computational fluid dynamics solver. Provides comprehensive build 
#   orchestration including third-party libraries, configuration management,
#   and development workflow support.
#
# Capabilities:
#   • Automatic dependency detection and compilation
#   • Configurable build modes (debug/release)
#   • Selective clean build operations
#   • Third-party library integration
#   • Build artifact management
#
# Author:     W. Wang, Science and Technology Facilities Council (STFC)
# Created:    27 June 2025
# Updated:    19 December 2025
# Version:    1.0
# =============================================================================
MAX_TIME=5  # Timeout for user inputs (seconds)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DECOMP_GIT_URL="https://github.com/2decomp-fft/2decomp-fft.git"
REL_PATH_LIB="$SCRIPT_DIR/lib/2decomp-fft/build"
REL_PATH_LIB_ROOT="$SCRIPT_DIR/lib/2decomp-fft"
REL_PATH_BUILD="$SCRIPT_DIR/build"
REL_PATH_BIN="$SCRIPT_DIR/bin"
LIB_FILE="$REL_PATH_LIB/opt/lib/libdecomp2d.a"
LIB_FILE_64="$REL_PATH_LIB/opt/lib64/libdecomp2d.a"
# -----------------------------------------------------------------------------
# Ensure necessary directories exist (except for 2decomp-fft which will be handled by git)
# -----------------------------------------------------------------------------
for dir in "$REL_PATH_BUILD" "$REL_PATH_BIN"; do
    if [ ! -d "$dir" ]; then
        echo "Creating directory: $dir"
        mkdir -p "$dir" || { echo "Error: Failed to create $dir"; exit 1; }
    fi
done
# -----------------------------------------------------------------------------
# Determine which library file to use
# -----------------------------------------------------------------------------
if [ -f "$LIB_FILE_64" ]; then
    LIB_FILE="$LIB_FILE_64"
    echo "Using lib64 version: $LIB_FILE"
else
    echo "Using lib version: $LIB_FILE"
fi
# -----------------------------------------------------------------------------
# Helper: yes/no input with default + timeout
# -----------------------------------------------------------------------------
get_yes_no_input() {
    local prompt="$1"
    local default="$2"
    local input
    read -t "$MAX_TIME" -p "$prompt [$default]: " input || input="$default"
    input=$(echo "${input:-$default}" | tr '[:upper:]' '[:lower:]')
    if [[ "$input" =~ ^(yes|y|no|n|only)$ ]]; then
        echo "$input"
    else
        echo "$default"
    fi
}
# -----------------------------------------------------------------------------
# Helper: choice input with default + timeout
# -----------------------------------------------------------------------------
get_choice_input() {
    local prompt="$1"
    local choices="$2"
    local default="$3"

    IFS=',' read -r -a valid_choices <<< "$choices"
    local choice

    # Non-interactive: return default immediately
    if [[ "${INTERACTIVE:-0}" -ne 1 ]]; then
        echo "$default"
        return 0
    fi

    while true; do
        # Prompt to stderr
        echo -n "$prompt ($choices) [$default]: " >&2

        # Read with timeout
        if ! read -t "$MAX_TIME" choice; then
            echo "$default"
            return 0
        fi

        choice="${choice:-$default}"
        choice=$(echo "$choice" | tr '[:upper:]' '[:lower:]')

        for c in "${valid_choices[@]}"; do
            if [[ "$choice" == "$c" ]]; then
                echo "$choice"
                return 0
            fi
        done

        echo "Invalid choice. Please enter one of: $choices" >&2
        echo "Please try again." >&2
    done
}
# -----------------------------------------------------------------------------
# Step 0: Select interactive or non-interactive mode
# -----------------------------------------------------------------------------
MODE=$(get_yes_no_input "Use interactive mode? (y/N)" "n")
[[ "$MODE" =~ ^(y|yes)$ ]] && INTERACTIVE=1 || INTERACTIVE=0
echo ">>> Build mode: $([[ $INTERACTIVE -eq 1 ]] && echo Interactive || echo Non-interactive)"

# -----------------------------------------------------------------------------
# Ensure directories
# -----------------------------------------------------------------------------
mkdir -p "$REL_PATH_BUILD" "$REL_PATH_BIN"

# -----------------------------------------------------------------------------
# Library selection
# -----------------------------------------------------------------------------
[[ -f "$LIB_FILE_64" ]] && LIB_FILE="$LIB_FILE_64" && echo "Using lib64: $LIB_FILE" || echo "Using lib: $LIB_FILE"
PATH_LIB=$(realpath "$REL_PATH_LIB" 2>/dev/null || readlink -f "$REL_PATH_LIB")
PATH_LIB_ROOT=$(realpath "$REL_PATH_LIB_ROOT" 2>/dev/null || readlink -f "$REL_PATH_LIB_ROOT")
PATH_BUILD=$(realpath "$REL_PATH_BUILD" 2>/dev/null || readlink -f "$REL_PATH_BUILD")
PATH_BIN=$(realpath "$REL_PATH_BIN" 2>/dev/null || readlink -f "$REL_PATH_BIN")

# -----------------------------------------------------------------------------
# Step 1: Clone / update 2decomp-fft repo
# -----------------------------------------------------------------------------
setup_2decomp_git() {
    mkdir -p "$SCRIPT_DIR/lib"
    if [ ! -d "$PATH_LIB_ROOT/.git" ]; then
        [[ -d "$PATH_LIB_ROOT" ]] && rm -rf "$PATH_LIB_ROOT"
        echo "Cloning 2decomp-fft..."
        git clone "$DECOMP_GIT_URL" "$PATH_LIB_ROOT"
    else
        echo "Updating 2decomp-fft..."
        git -C "$PATH_LIB_ROOT" fetch origin
        git -C "$PATH_LIB_ROOT" reset --hard origin/$(git -C "$PATH_LIB_ROOT" rev-parse --abbrev-ref HEAD)
    fi
}
REFRESH_GIT=$(get_yes_no_input "Setup/refresh 2decomp-fft git repository? (y/N)" "n")
[[ "$REFRESH_GIT" =~ ^(y|yes)$ ]] && setup_2decomp_git
# -----------------------------------------------------------------------------
# Step 2: Build 2decomp-fft library
# -----------------------------------------------------------------------------
mkdir -p "$REL_PATH_LIB"
cd "$REL_PATH_LIB"
for f in "$REL_PATH_BUILD/build_cmake_2decomp.sh" "$REL_PATH_LIB_ROOT/build_cmake_2decomp.sh"; do
    [[ -f "$f" ]] && { cp "$f" ./build_cmake_2decomp.sh; break; }
done
[[ -f build_cmake_2decomp.sh ]] || { echo "Error: build_cmake_2decomp.sh not found"; exit 1; }
chmod +x build_cmake_2decomp.sh
./build_cmake_2decomp.sh || { echo "Error: CMake build failed"; exit 1; }
# -----------------------------------------------------------------------------
# Step 3: CHAPSim2 solver build
# -----------------------------------------------------------------------------
cd "$PATH_BUILD"

if [[ $INTERACTIVE -eq 1 ]]; then
    CLEAN_BUILD=$(get_choice_input "Clean build?" "y,n,clean" "n")
    echo -n "CHAPSim build mode: [A]default, [b]gnu-o3, [c]gnu-g, [d]gnu-debug, [e]intel, [f]cray"
    BUILD_MODE=$(get_choice_input "Select build mode" "A,b,c,d,e,f" "a")
else
    CLEAN_BUILD="n"
    BUILD_MODE="a"
fi

case "$BUILD_MODE" in
    a|"") MAKE_TARGET="make all" ;;
    b) MAKE_TARGET="make cfg=gnu-o3" ;;
    c) MAKE_TARGET="make cfg=gnu-g" ;;
    d) MAKE_TARGET="make cfg=gnu-debug" ;;
    e) MAKE_TARGET="make cfg=intel" ;;
    f) MAKE_TARGET="make cfg=cray" ;;
    *) MAKE_TARGET="make all" ;;
esac

[[ "$CLEAN_BUILD" =~ ^(y|yes|clean)$ ]] && MAKE_TARGET="make clean && $MAKE_TARGET"

echo "Running build: $MAKE_TARGET"
eval "$MAKE_TARGET" || { echo "Error: Solver build failed"; exit 1; }

echo "✅ CHAPSim2 successfully compiled."