#!/bin/bash

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
# Version:    1.0
# =============================================================================

# Define relative paths and repository URL
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DECOMP_GIT_URL="https://github.com/2decomp-fft/2decomp-fft.git"
REL_PATH_LIB="$SCRIPT_DIR/lib/2decomp-fft/build"
REL_PATH_LIB_ROOT="$SCRIPT_DIR/lib/2decomp-fft"
REL_PATH_BUILD="$SCRIPT_DIR/build"
REL_PATH_BIN="$SCRIPT_DIR/bin"
LIB_FILE="$REL_PATH_LIB/opt/lib/libdecomp2d.a"
LIB_FILE_64="$REL_PATH_LIB/opt/lib64/libdecomp2d.a"

# Ensure necessary directories exist (except for 2decomp-fft which will be handled by git)
for dir in "$REL_PATH_BUILD" "$REL_PATH_BIN"; do
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
PATH_LIB_ROOT=$(realpath "$REL_PATH_LIB_ROOT" 2>/dev/null || readlink -f "$REL_PATH_LIB_ROOT")
PATH_BUILD=$(realpath "$REL_PATH_BUILD" 2>/dev/null || readlink -f "$REL_PATH_BUILD")
PATH_BIN=$(realpath "$REL_PATH_BIN" 2>/dev/null || readlink -f "$REL_PATH_BIN")

# -----------------------------------------------------------------------------
# Function to get yes/no input
get_yes_no_input() {
    local prompt="$1"
    local default="$2"
    local input

    read -p "$prompt [$default]: " input
    input=$(echo "${input:-$default}" | tr '[:upper:]' '[:lower:]')

    if [[ "$input" == "yes" || "$input" == "y" || "$input" == "no" || "$input" == "n" || "$input" == "only" ]]; then
        echo "$input"
    else
        echo "no"
    fi
}

# -----------------------------------------------------------------------------
# Function to get a choice from the user
get_choice_input() {
    local prompt_msg="$1"
    local choices_str="$2"
    IFS=',' read -r -a valid_choices <<< "$choices_str"
    local choice

    while true; do
        # Print prompt to stderr
        echo -n "$prompt_msg ($choices_str): " >&2
        read choice
        choice=$(echo "$choice" | tr '[:upper:]' '[:lower:]')

        for valid_choice in "${valid_choices[@]}"; do
            if [[ "$choice" == "$valid_choice" ]]; then
                echo "$choice"  # Only this goes to stdout
                return 0
            fi
        done
        echo "Invalid choice. Please enter one of: $choices_str" >&2
        echo "Please try again." >&2
    done
}
# -----------------------------------------------------------------------------
# Function to clone or refresh 2decomp-fft git repository
setup_2decomp_git() {
    local lib_parent_dir="$SCRIPT_DIR/lib"
    
    # Ensure lib directory exists
    if [ ! -d "$lib_parent_dir" ]; then
        echo "Creating lib directory: $lib_parent_dir"
        mkdir -p "$lib_parent_dir" || { echo "Error: Failed to create $lib_parent_dir"; return 1; }
    fi
    
    # Check if 2decomp-fft directory exists AND contains a git repository
    if [ ! -d "$PATH_LIB_ROOT" ] || [ ! -d "$PATH_LIB_ROOT/.git" ]; then
        if [ -d "$PATH_LIB_ROOT" ] && [ ! -d "$PATH_LIB_ROOT/.git" ]; then
            echo "Found existing directory $PATH_LIB_ROOT but it's not a git repository."
            REMOVE_EXISTING=$(get_yes_no_input "Remove existing directory and clone fresh?" "yes")
            if [[ "$REMOVE_EXISTING" =~ ^(yes|y)$ ]]; then
                echo "Removing existing directory..."
                rm -rf "$PATH_LIB_ROOT" || { echo "Error: Failed to remove existing directory"; return 1; }
            else
                echo "Cannot proceed without a proper git repository."
                return 1
            fi
        fi
        
        echo "2decomp-fft repository not found. Cloning from GitHub..."
        cd "$lib_parent_dir" || { echo "Error: Cannot access $lib_parent_dir"; return 1; }
        
        echo "Cloning: $DECOMP_GIT_URL"
        if git clone "$DECOMP_GIT_URL"; then
            echo "✅ Repository cloned successfully!"
            cd - > /dev/null  # Return to original directory
            return 0
        else
            echo "❌ Error: Failed to clone repository"
            cd - > /dev/null  # Return to original directory
            return 1
        fi
    fi
    
    # Repository exists, proceed with refresh
    echo "Refreshing existing 2decomp-fft git repository..."
    
    cd "$PATH_LIB_ROOT" || { echo "Error: Cannot access $PATH_LIB_ROOT"; return 1; }
    
    echo "Current directory: $(pwd)"
    
    # Verify remote URL matches expected repository
    CURRENT_REMOTE=$(git remote get-url origin 2>/dev/null)
    if [ "$CURRENT_REMOTE" != "$DECOMP_GIT_URL" ]; then
        echo "Warning: Remote URL mismatch!"
        echo "Expected: $DECOMP_GIT_URL"
        echo "Current:  $CURRENT_REMOTE"
        UPDATE_REMOTE=$(get_yes_no_input "Update remote URL to match expected repository?" "yes")
        if [[ "$UPDATE_REMOTE" =~ ^(yes|y)$ ]]; then
            git remote set-url origin "$DECOMP_GIT_URL" || {
                echo "Error: Failed to update remote URL"
                return 1
            }
            echo "Remote URL updated successfully."
        fi
    fi
    
    # Check if there are uncommitted changes
    if ! git diff-index --quiet HEAD -- 2>/dev/null; then
        echo "Warning: There are uncommitted changes in the repository."
        git status --porcelain
        echo ""
        STASH_CHANGES=$(get_yes_no_input "Stash local changes before updating?" "yes")
        if [[ "$STASH_CHANGES" =~ ^(yes|y)$ ]]; then
            echo "Stashing local changes..."
            git stash push -m "Auto-stash before refresh $(date)" || {
                echo "Error: Failed to stash changes"
                return 1
            }
            echo "Changes stashed successfully."
        else
            echo "Warning: Proceeding with uncommitted changes. This may cause conflicts."
        fi
    fi
    
    # Get current branch
    CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    echo "Current branch: $CURRENT_BRANCH"
    
    # Fetch the latest changes
    echo "Fetching latest changes from remote..."
    git fetch origin || { echo "Error: Failed to fetch from remote"; return 1; }
    
    # Get the latest commit hash before update
    OLD_COMMIT=$(git rev-parse HEAD)
    
    # Check if we're behind the remote
    LOCAL_COMMIT=$(git rev-parse HEAD)
    REMOTE_COMMIT=$(git rev-parse "origin/$CURRENT_BRANCH" 2>/dev/null)
    
    if [ "$LOCAL_COMMIT" = "$REMOTE_COMMIT" ]; then
        echo "Repository is already up to date."
        return 0
    fi
    
    # Pull the latest changes
    echo "Pulling latest changes..."
    if ! git pull origin "$CURRENT_BRANCH"; then
        echo "❌ Error: Failed to pull latest changes"
        echo "You may need to resolve conflicts manually."
        echo "Run 'git status' in $PATH_LIB_ROOT to see the current state."
        return 1
    fi
    
    # Get the latest commit hash after update
    NEW_COMMIT=$(git rev-parse HEAD)
    
    echo "✅ Repository updated successfully!"
    echo "Updated from commit: ${OLD_COMMIT:0:8} to ${NEW_COMMIT:0:8}"
    
    # Show what changed
    echo ""
    echo "Recent changes:"
    git log --oneline -5 "$OLD_COMMIT..$NEW_COMMIT" 2>/dev/null || echo "Unable to show recent changes"
    echo ""
    
    return 0
}

# -----------------------------------------------------------------------------
# Step 0: Git Repository Setup/Refresh (Optional)
# -----------------------------------------------------------------------------
REFRESH_GIT=$(get_yes_no_input "Setup/refresh 2decomp-fft git repository? (y/n)" "no")
if [[ "$REFRESH_GIT" =~ ^(yes|y)$ ]]; then
    if setup_2decomp_git; then
        echo "Git repository setup/refresh completed successfully."
        # Force rebuild after git refresh or clone
        echo "Forcing library rebuild after git update..."
        LIB_REBUILD="yes"
    else
        echo "❌ Error: Git repository setup/refresh failed."
        CONTINUE_ANYWAY=$(get_yes_no_input "Continue with build anyway? (y/n)" "no")
        if [[ ! "$CONTINUE_ANYWAY" =~ ^(yes|y)$ ]]; then
            echo "Build aborted."
            exit 1
        fi
    fi
fi

# Create 2decomp-fft build directory if it doesn't exist (after git setup)
if [ ! -d "$REL_PATH_LIB" ]; then
    echo "Creating directory: $REL_PATH_LIB"
    mkdir -p "$REL_PATH_LIB" || { echo "Error: Failed to create $REL_PATH_LIB"; exit 1; }
fi

# Check if build_cmake_2decomp.sh exists in the library build directory, if not copy from other locations
BUILD_CMAKE_LIB="$REL_PATH_LIB/build_cmake_2decomp.sh"
BUILD_CMAKE_ROOT="$REL_PATH_LIB_ROOT/build_cmake_2decomp.sh"  # In 2decomp-fft root
BUILD_CMAKE_BUILD="$REL_PATH_BUILD/build_cmake_2decomp.sh"   # In project build directory

echo "Debug: Checking for build_cmake_2decomp.sh in the following locations:"
echo "  Target: $BUILD_CMAKE_LIB"
echo "  2decomp root: $BUILD_CMAKE_ROOT" 
echo "  Project build: $BUILD_CMAKE_BUILD"
echo "  Current working directory: $(pwd)"

echo "REL_PATH_BUILD: '$REL_PATH_BUILD'"
echo "BUILD_CMAKE_BUILD: '$BUILD_CMAKE_BUILD'"
echo "Current working directory: $(pwd)"

if [ ! -f "$BUILD_CMAKE_LIB" ]; then
    echo "build_cmake_2decomp.sh not found in $REL_PATH_LIB"
    
    # Debug: Show what's actually in the build directory
    echo ""
    echo "Debug: Contents of $REL_PATH_BUILD directory:"
    ls -la "$REL_PATH_BUILD" 2>/dev/null || echo "Directory $REL_PATH_BUILD not accessible"
    echo ""
    
    # First try project build directory (since you confirmed it's there)
    if [ -f "$BUILD_CMAKE_BUILD" ]; then
        echo "Found build_cmake_2decomp.sh in project build directory"
        echo "Copying build_cmake_2decomp.sh from $REL_PATH_BUILD to $REL_PATH_LIB"
        cp "$BUILD_CMAKE_BUILD" "$BUILD_CMAKE_LIB" || { 
            echo "Error: Failed to copy build_cmake_2decomp.sh"; exit 1; 
        }
        # Make it executable
        chmod +x "$BUILD_CMAKE_LIB" || { 
            echo "Warning: Failed to make build_cmake_2decomp.sh executable"; 
        }
        echo "✅ build_cmake_2decomp.sh copied from project build directory and made executable"
    # Then try 2decomp-fft root directory
    elif [ -f "$BUILD_CMAKE_ROOT" ]; then
        echo "Found build_cmake_2decomp.sh in 2decomp-fft root directory"
        echo "Copying build_cmake_2decomp.sh from $REL_PATH_LIB_ROOT to $REL_PATH_LIB"
        cp "$BUILD_CMAKE_ROOT" "$BUILD_CMAKE_LIB" || { 
            echo "Error: Failed to copy build_cmake_2decomp.sh"; exit 1; 
        }
        # Make it executable
        chmod +x "$BUILD_CMAKE_LIB" || { 
            echo "Warning: Failed to make build_cmake_2decomp.sh executable"; 
        }
        echo "✅ build_cmake_2decomp.sh copied from 2decomp-fft root and made executable"
    else
        echo "❌ Error: build_cmake_2decomp.sh not found in any of these locations:"
        echo "  - $BUILD_CMAKE_LIB"
        echo "  - $BUILD_CMAKE_BUILD"
        echo "  - $BUILD_CMAKE_ROOT"
        echo ""
        echo "Debug: Detailed file check results:"
        echo "  $(ls -la "$BUILD_CMAKE_BUILD" 2>&1)"
        echo "  $(ls -la "$BUILD_CMAKE_ROOT" 2>&1)"
        echo ""
        echo "Please verify the exact filename and location of build_cmake_2decomp.sh"
        exit 1
    fi
else
    echo "build_cmake_2decomp.sh found in $REL_PATH_LIB"
fi

# -----------------------------------------------------------------------------
# Step 1: Check and Build the Library
# -----------------------------------------------------------------------------
if [[ -f "$LIB_FILE" && "$LIB_REBUILD" != "yes" ]]; then
    LIB_REBUILD=$(get_yes_no_input "Rebuild 2decomp library?(y/n)" "no")
    if [[ "$LIB_REBUILD" =~ ^(yes|y)$ ]]; then
        cd "$PATH_LIB" || { echo "Error: Cannot access $PATH_LIB"; exit 1; }
        shopt -s extglob
        find . -maxdepth 1 -not -name 'build_cmake_2decomp.sh' -not -name '.' -exec rm -rv {} +
        ./build_cmake_2decomp.sh || { echo "Error: CMake build failed in $PATH_LIB"; exit 1; }
    fi
else
    echo "Library not found or rebuild forced. Running CMake in $PATH_LIB..."
    cd "$PATH_LIB" || { echo "Error: Cannot access $PATH_LIB"; exit 1; }
    ./build_cmake_2decomp.sh || { echo "Error: CMake build failed in $PATH_LIB"; exit 1; }
fi

# -----------------------------------------------------------------------------
# Step 2: Prompt for Build Options
# -----------------------------------------------------------------------------
CLEAN_BUILD=$(get_yes_no_input "Perform a clean build first? (y/n) (Enter 'only' to just clean and exit)" "no")

if [[ "$CLEAN_BUILD" == "only" ]]; then
    echo "Running 'make clean' in $PATH_BUILD..."
    if [[ ! -d "$PATH_BUILD" ]]; then
        echo "Error: Cannot access $PATH_BUILD"
        exit 1
    fi
    cd "$PATH_BUILD" || { echo "Error: Cannot cd to $PATH_BUILD"; exit 1; }
    make clean || echo "Warning: 'make clean' failed."
    exit 0
fi

# Prompt user and capture single-letter choice
BUILD_MODE=$(get_choice_input "Select CHAPSim build mode: [a]default, [b]gnu-o3, [c]gnu-g, [d]gnu-debug, [e]intel, [f]cray" "a,b,c,d,e,f")

# Determine make target based on selection
case "$BUILD_MODE" in
    a)
        MAKE_TARGET="make all"
        ;;
    b)
        MAKE_TARGET="make cfg=gnu-o3"
        ;;
    c)
        MAKE_TARGET="make cfg=gnu-g"
        ;;
    d)
        MAKE_TARGET="make cfg=gnu-debug"
        ;;
    e)
        MAKE_TARGET="make cfg=intel"
        ;;
    f)
        MAKE_TARGET="make cfg=cray"
        ;;
    *)
        echo "Error: Unexpected build mode '$BUILD_MODE'. Exiting."
        exit 1
        ;;
esac

# Optionally run it
echo "Running: $MAKE_TARGET"
eval $MAKE_TARGET

if [[ "$CLEAN_BUILD" =~ ^(yes|y)$ ]]; then
    MAKE_TARGET="make clean && $MAKE_TARGET"
fi

echo "Running build command: $MAKE_TARGET"
cd "$PATH_BUILD" || { echo "Error: Cannot cd to $PATH_BUILD"; exit 1; }
$MAKE_TARGET
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
