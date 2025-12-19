#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# CHAPSim2 Build System v1.2
# Interactive + Non-interactive (timeout-aware) + macOS-safe
# =============================================================================

MAX_TIME=5
INTERACTIVE=0
LIB_REBUILD="no"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DECOMP_GIT_URL="https://github.com/2decomp-fft/2decomp-fft.git"

REL_PATH_LIB_ROOT="$SCRIPT_DIR/lib/2decomp-fft"
REL_PATH_LIB="$REL_PATH_LIB_ROOT/build"
REL_PATH_BUILD="$SCRIPT_DIR/build"
REL_PATH_BIN="$SCRIPT_DIR/bin"

LIB_FILE="$REL_PATH_LIB/opt/lib/libdecomp2d.a"
LIB_FILE_64="$REL_PATH_LIB/opt/lib64/libdecomp2d.a"

# -----------------------------------------------------------------------------
# Enforce macOS toolchain & architecture consistency
# -----------------------------------------------------------------------------
# if [[ "$(uname)" == "Darwin" ]]; then
#     export CC=clang
#     export CXX=clang++
#     export FC=gfortran
#     ARCH="$(uname -m)"
#     export CFLAGS="-arch $ARCH"
#     export CXXFLAGS="-arch $ARCH"
#     echo ">>> macOS detected: enforcing clang + arch=$ARCH"
# fi
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
# Timeout-aware yes/no input
# -----------------------------------------------------------------------------
get_yes_no_input() {
    local prompt="$1" default="$2" input
    if [[ $INTERACTIVE -eq 1 ]]; then
        read -t "$MAX_TIME" -p "$prompt [$default]: " input || input="$default"
        input="${input:-$default}"
        input=$(echo "$input" | tr '[:upper:]' '[:lower:]')
        [[ "$input" =~ ^(yes|y|no|n)$ ]] && echo "$input" || echo "$default"
    else
        echo "$default"
    fi
}

# -----------------------------------------------------------------------------
# Timeout-aware choice input (merged, lossless)
# -----------------------------------------------------------------------------
get_choice_input() {
    local prompt="$1" choices="$2" default="$3"
    IFS=',' read -r -a valid_choices <<< "$choices"
    local choice

    if [[ $INTERACTIVE -eq 1 ]]; then
        read -t "$MAX_TIME" -p "$prompt ($choices) [$default]: " choice || choice="$default"
        choice="${choice:-$default}"
        choice=$(echo "$choice" | tr '[:upper:]' '[:lower:]')
        for c in "${valid_choices[@]}"; do
            [[ "$choice" == "$c" ]] && { echo "$choice"; return 0; }
        done
        echo "$default"
    else
        echo "$default"
    fi
}

# -----------------------------------------------------------------------------
# Step 0: Ask interactive or non-interactive (ALWAYS asked)
# -----------------------------------------------------------------------------
read -t "$MAX_TIME" -p "Use interactive mode? (y/N): " MODE || MODE="n"
MODE=$(echo "${MODE:-n}" | tr '[:upper:]' '[:lower:]')
[[ "$MODE" =~ ^(y|yes)$ ]] && INTERACTIVE=1 || INTERACTIVE=0

echo ">>> Mode: $([[ $INTERACTIVE -eq 1 ]] && echo Interactive || echo Non-interactive)"

# -----------------------------------------------------------------------------
# Git: clone or refresh 2decomp-fft (correct logic)
# -----------------------------------------------------------------------------
setup_2decomp_git() {

    # --- Clone if missing ---
    if [[ ! -d "$REL_PATH_LIB_ROOT/.git" ]]; then
        if [[ -d "$REL_PATH_LIB_ROOT" ]]; then
            echo "Found non-git directory: $REL_PATH_LIB_ROOT"
            REMOVE=$(get_yes_no_input "Remove and clone fresh?" "yes")
            [[ "$REMOVE" =~ ^(yes|y)$ ]] || return 1
            rm -rf "$REL_PATH_LIB_ROOT"
        fi
        echo "Cloning 2decomp-fft..."
        git clone "$DECOMP_GIT_URL" "$REL_PATH_LIB_ROOT"
        LIB_REBUILD="yes"
        return 0
    fi

    # --- Repo exists: ask whether to refresh ---
    REFRESH_EXISTING=$(get_yes_no_input \
        "2decomp-fft already exists. Refresh from git?" "no")

    [[ "$REFRESH_EXISTING" =~ ^(yes|y)$ ]] || return 0

    cd "$REL_PATH_LIB_ROOT"

    # Fix remote if needed
    CURRENT_REMOTE=$(git remote get-url origin)
    if [[ "$CURRENT_REMOTE" != "$DECOMP_GIT_URL" ]]; then
        UPDATE=$(get_yes_no_input "Fix remote URL?" "yes")
        [[ "$UPDATE" =~ ^(yes|y)$ ]] && git remote set-url origin "$DECOMP_GIT_URL"
    fi

    # Stash local changes if any
    if ! git diff-index --quiet HEAD --; then
        STASH=$(get_yes_no_input "Stash local changes?" "yes")
        [[ "$STASH" =~ ^(yes|y)$ ]] && git stash push -m "Auto-stash $(date)"
    fi

    git fetch origin
    BRANCH=$(git rev-parse --abbrev-ref HEAD)

    echo "Refreshing 2decomp-fft from origin/$BRANCH ..."
    git pull origin "$BRANCH" || {
        echo "❌ Git pull failed"
        return 1
    }

    # FORCE rebuild if user explicitly asked to refresh
    LIB_REBUILD="yes"
}

# -----------------------------------------------------------------------------
# Ask whether to setup/refresh git repo
# -----------------------------------------------------------------------------
REFRESH_GIT=$(get_yes_no_input "Setup/refresh 2decomp-fft git repository?" "no")
[[ "$REFRESH_GIT" =~ ^(yes|y)$ ]] && setup_2decomp_git

# -----------------------------------------------------------------------------
# Build 2decomp-fft library
# -----------------------------------------------------------------------------
mkdir -p "$REL_PATH_LIB"
cd "$REL_PATH_LIB"

for f in \
    "$REL_PATH_BUILD/build_cmake_2decomp.sh" \
    "$REL_PATH_LIB_ROOT/build_cmake_2decomp.sh"
do
    [[ -f "$f" ]] && { cp "$f" ./build_cmake_2decomp.sh; break; }
done

[[ -f build_cmake_2decomp.sh ]] || { echo "❌ build_cmake_2decomp.sh not found"; exit 1; }
chmod +x build_cmake_2decomp.sh

# Force rebuild if library missing
if [[ ! -f "$LIB_FILE" && ! -f "$LIB_FILE_64" ]]; then
    LIB_REBUILD="yes"
fi

if [[ "$LIB_REBUILD" =~ ^(yes|y)$ ]]; then
    shopt -s extglob
    find . -maxdepth 1 -not -name 'build_cmake_2decomp.sh' -not -name '.' -exec rm -rf {} +
    ./build_cmake_2decomp.sh
else
    echo "Using existing 2decomp-fft build."
fi

# -----------------------------------------------------------------------------
# CHAPSim solver build
# -----------------------------------------------------------------------------
cd "$REL_PATH_BUILD"

CLEAN_BUILD=$(get_choice_input \
    "Perform clean build?" "y,N,clean" "n")

BUILD_MODE=$(get_choice_input \
    "Select CHAPSim build mode" "a,b,c,d,e,f" "a")

case "$BUILD_MODE" in
    a) MAKE_TARGET="make all" ;;
    b) MAKE_TARGET="make cfg=gnu-o3" ;;
    c) MAKE_TARGET="make cfg=gnu-g" ;;
    d) MAKE_TARGET="make cfg=gnu-debug" ;;
    e) MAKE_TARGET="make cfg=intel" ;;
    f) MAKE_TARGET="make cfg=cray" ;;
    *) MAKE_TARGET="make all" ;;
esac

[[ "$CLEAN_BUILD" =~ ^(y|yes|clean)$ ]] && MAKE_TARGET="make clean && $MAKE_TARGET"

echo "Running: $MAKE_TARGET"
eval "$MAKE_TARGET"

echo "✅ CHAPSim2 successfully compiled."