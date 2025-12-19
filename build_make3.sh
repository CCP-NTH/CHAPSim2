#!/usr/bin/env bash
set -euo pipefail
# =============================================================================
# CHAPSim2 Build System v2.0 (Refactored for 2decomp-fft reuse)
# =============================================================================

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
MAX_TIME=5
INTERACTIVE_MODE=""
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DECOMP_GIT_URL="https://github.com/2decomp-fft/2decomp-fft.git"

REL_PATH_LIB="$SCRIPT_DIR/lib/2decomp-fft/build"
REL_PATH_LIB_ROOT="$SCRIPT_DIR/lib/2decomp-fft"
REL_PATH_BUILD="$SCRIPT_DIR/build"
REL_PATH_BIN="$SCRIPT_DIR/bin"
LIB_FILE="$REL_PATH_LIB/opt/lib/libdecomp2d.a"
LIB_FILE_64="$REL_PATH_LIB/opt/lib64/libdecomp2d.a"

# Ensure necessary directories exist
for dir in "$REL_PATH_BUILD" "$REL_PATH_BIN"; do
    mkdir -p "$dir"
done

# -----------------------------------------------------------------------------
# Input helper functions (unchanged)
# -----------------------------------------------------------------------------
read_with_timeout() {
    local prompt="$1"
    local default="$2"
    local timeout="$3"
    local input=""
    
    [[ "$INTERACTIVE_MODE" == "non-interactive" ]] && { echo "$default"; return 0; }

    echo -n "$prompt [$default] (${timeout}s timeout): " >&2
    if read -t "$timeout" input 2>/dev/null; then
        echo "${input:-$default}"
    else
        echo "" >&2
        echo "⏱ Timeout - using default: $default" >&2
        echo "$default"
    fi
}

get_yes_no_input() {
    local prompt="$1"
    local default="$2"
    local input
    input=$(read_with_timeout "$prompt (y/n)" "$default" "$MAX_TIME")
    input=$(echo "$input" | tr '[:upper:]' '[:lower:]')
    [[ "$input" == "yes" || "$input" == "y" ]] && echo "yes" && return
    echo "no"
}

get_choice_input() {
    local prompt_msg="$1"
    local choices_str="$2"
    local default="$3"
    IFS=',' read -r -a valid_choices <<< "$choices_str"
    local choice

    [[ "$INTERACTIVE_MODE" == "non-interactive" ]] && { echo "$default"; return 0; }

    while true; do
        choice=$(read_with_timeout "$prompt_msg ($choices_str)" "$default" "$MAX_TIME")
        choice=$(echo "$choice" | tr '[:upper:]' '[:lower:]')
        for valid_choice in "${valid_choices[@]}"; do
            [[ "$choice" == "$valid_choice" ]] && { echo "$choice"; return 0; }
        done
        [[ -z "$choice" ]] && { echo "$default"; return 0; }
        echo "Invalid choice. Please enter one of: $choices_str" >&2
    done
}

# -----------------------------------------------------------------------------
# Interactive mode selection (unchanged)
# -----------------------------------------------------------------------------
echo "Select build mode:"
echo "  [I]nteractive - prompts with ${MAX_TIME}s timeout"
echo "  [N]on-interactive - uses all defaults (default)"
echo ""
MODE_INPUT=""
if read -t "$MAX_TIME" -p "Mode selection [i/N]: " MODE_INPUT 2>/dev/null; then
    MODE_INPUT=$(echo "${MODE_INPUT:-i}" | tr '[:upper:]' '[:lower:]')
else
    echo ""
    echo "⏱ Timeout - defaulting to non-interactive mode"
    MODE_INPUT="n"
fi
INTERACTIVE_MODE=$([[ "$MODE_INPUT" == "n" || "$MODE_INPUT" == "non-interactive" ]] && echo "non-interactive" || echo "interactive")
echo "Mode: $INTERACTIVE_MODE"

# -----------------------------------------------------------------------------
# Step 0: Optional Git Setup / Refresh (2decomp-fft)
# -----------------------------------------------------------------------------
REFRESH_GIT=$(get_yes_no_input "Setup/refresh 2decomp-fft git repository? (y/N)" "no")
LIB_REBUILD="no"

# Ensure build_cmake_2decomp.sh exists
BUILD_CMAKE_LIB="$REL_PATH_LIB/build_cmake_2decomp.sh"
BUILD_CMAKE_ROOT="$REL_PATH_LIB_ROOT/build_cmake_2decomp.sh"
BUILD_CMAKE_BUILD="$REL_PATH_BUILD/build_cmake_2decomp.sh"

if [ ! -f "$BUILD_CMAKE_LIB" ]; then
    if [ -f "$BUILD_CMAKE_BUILD" ]; then
        cp "$BUILD_CMAKE_BUILD" "$BUILD_CMAKE_LIB"
    elif [ -f "$BUILD_CMAKE_ROOT" ]; then
        cp "$BUILD_CMAKE_ROOT" "$BUILD_CMAKE_LIB"
    else
        echo "❌ Error: build_cmake_2decomp.sh not found"
        exit 1
    fi
    chmod +x "$BUILD_CMAKE_LIB"
fi

# Source script1 to reuse library functions
source "$BUILD_CMAKE_LIB"

[[ "$REFRESH_GIT" =~ ^(yes|y)$ ]] && { setup_2decomp_git && LIB_REBUILD="yes"; }

# -----------------------------------------------------------------------------
# Step 1: Build/Validate 2decomp-fft library
# -----------------------------------------------------------------------------
if [ -f "$LIB_FILE_64" ]; then
    LIB_FILE="$LIB_FILE_64"
    echo "Using lib64 version: $LIB_FILE"
else
    echo "Using lib version: $LIB_FILE"
fi

if [[ -f "$LIB_FILE" && "$LIB_REBUILD" != "yes" ]]; then
    if validate_library "$LIB_FILE"; then
        LIB_REBUILD=$(get_yes_no_input "Rebuild 2decomp library?" "no")
    else
        echo "⚠️ Existing library is invalid or corrupted."
        LIB_REBUILD="yes"
    fi
fi

if [[ "$LIB_REBUILD" =~ ^(yes|y)$ || ! -f "$LIB_FILE" ]]; then
    echo "Building 2decomp-fft library..."
    cd "$REL_PATH_LIB" || { echo "Error: Cannot access $REL_PATH_LIB"; exit 1; }
    ./build_cmake_2decomp.sh
    cd - > /dev/null

    if ! validate_library "$LIB_FILE"; then
        echo "❌ Error: Library build completed but validation failed."
        exit 1
    fi
fi

# -----------------------------------------------------------------------------
# Step 2: CHAPSim Build Options (unchanged)
# -----------------------------------------------------------------------------
CLEAN_BUILD=$(get_yes_no_input "Perform a clean build? (y/N/clean)" "no")
[[ "$CLEAN_BUILD" == "clean" ]] && { cd "$REL_PATH_BUILD" && make clean && exit 0 && cd -; }

BUILD_MODE=$(get_choice_input "Select CHAPSim build mode" "A,b,c,d,e,f" "a")
case "$BUILD_MODE" in
    a) MAKE_TARGET="make all" ;;
    b) MAKE_TARGET="make cfg=gnu-o3" ;;
    c) MAKE_TARGET="make cfg=gnu-g" ;;
    d) MAKE_TARGET="make cfg=gnu-debug" ;;
    e) MAKE_TARGET="make cfg=intel" ;;
    f) MAKE_TARGET="make cfg=cray" ;;
    *) MAKE_TARGET="make all" ;;
esac

[[ "$CLEAN_BUILD" =~ ^(yes|y)$ ]] && MAKE_TARGET="make clean && $MAKE_TARGET"

# -----------------------------------------------------------------------------
# Step 3: Run CHAPSim Build (unchanged)
# -----------------------------------------------------------------------------
echo "Executing: $MAKE_TARGET in $REL_PATH_BUILD"
cd "$REL_PATH_BUILD" || { echo "Error: Cannot access $REL_PATH_BUILD"; exit 1; }
eval "$MAKE_TARGET"
cd - > /dev/null

# -----------------------------------------------------------------------------
# Completion Message (unchanged)
# -----------------------------------------------------------------------------
echo ""
echo "========================================================================="
echo "✅ CHAPSim2 successfully compiled!"
echo "========================================================================="
echo "Build configuration: $BUILD_MODE"
echo "Mode: $INTERACTIVE_MODE"
echo "Binary location: $REL_PATH_BIN"
echo ""
