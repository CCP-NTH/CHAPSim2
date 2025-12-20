#!/usr/bin/env bash
set -euo pipefail
# =============================================================================
# CHAPSim2 Build System v2.0
# 
# Description:
#   Automated compilation and dependency management system for the CHAPSim2 
#   computational fluid dynamics solver. Provides comprehensive build 
#   orchestration including third-party libraries, configuration management,
#   and development workflow support.
#
# Features:
#   • Interactive and non-interactive modes with configurable timeouts
#   • Automatic dependency detection and compilation
#   • Configurable build modes (debug/release)
#   • Selective clean build operations
#   • Third-party library integration
#   • Build artifact management
#
# Author:     W. Wang, Science and Technology Facilities Council (STFC)
# Enhanced:   December 2025
# Version:    2.0
# =============================================================================
# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
MAX_TIME=10  # Maximum wait time in seconds for user input
INTERACTIVE_MODE=""  # Will be set based on user choice
DECOMP_GIT_URL="https://github.com/2decomp-fft/2decomp-fft.git"

# Define relative paths and repository URL
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REL_PATH_BUILD="$SCRIPT_DIR/build"
REL_PATH_BIN="$SCRIPT_DIR/bin"
REL_PATH_LIB_ROOT="$SCRIPT_DIR/lib/2decomp-fft"
REL_PATH_LIB_BUILD="$REL_PATH_LIB_ROOT/build"
LIB_FILE="$REL_PATH_LIB_BUILD/opt/lib/libdecomp2d.a"
LIB_FILE_64="$REL_PATH_LIB_BUILD/opt/lib64/libdecomp2d.a"

# Initialize absolute paths (will be resolved after directories exist)
PATH_LIB=""
PATH_LIB_ROOT=""
PATH_BUILD=""
PATH_BIN=""

# Ensure lib directory exists
LIB_PARENT_DIR="$SCRIPT_DIR/lib"
if [ ! -d "$LIB_PARENT_DIR" ]; then
    echo "Creating lib directory: $LIB_PARENT_DIR"
    mkdir -p "$LIB_PARENT_DIR" || { echo "Error: Failed to create $LIB_PARENT_DIR"; exit 1; }
fi

# Now resolve absolute paths after directories are created
PATH_LIB=$(realpath "$REL_PATH_LIB_BUILD" 2>/dev/null || echo "$REL_PATH_LIB_BUILD")
PATH_LIB_ROOT=$(realpath "$REL_PATH_LIB_ROOT" 2>/dev/null || echo "$REL_PATH_LIB_ROOT")
PATH_BUILD=$(realpath "$REL_PATH_BUILD" 2>/dev/null || echo "$REL_PATH_BUILD")
PATH_BIN=$(realpath "$REL_PATH_BIN" 2>/dev/null || echo "$REL_PATH_BIN")

# =============================================================================
# FUNCTION DEFINITIONS
# =============================================================================

# Function to read input with timeout
# Returns: user input or default value if timeout
read_with_timeout() {
    local prompt="$1"
    local default="$2"
    local timeout="$3"
    local input=""
    
    if [[ "$INTERACTIVE_MODE" == "non-interactive" ]]; then
        echo "$default"
        return 0
    fi
    
    # Display prompt
    echo -n "$prompt [$default] (${timeout}s timeout): " >&2
    
    # Read with timeout
    if read -t "$timeout" input 2>/dev/null; then
        # User provided input
        echo "${input:-$default}"
    else
        # Timeout occurred
        echo "" >&2
        echo "⏱ Timeout - using default: $default" >&2
        echo "$default"
    fi
}

# Function to get yes/no input with timeout
get_yes_no_input() {
    local prompt="$1"
    local default="$2"
    local input
    
    input=$(read_with_timeout "$prompt (y/n)" "$default" "$MAX_TIME")
    input=$(echo "$input" | tr '[:upper:]' '[:lower:]')
    
    if [[ "$input" == "yes" || "$input" == "y" ]]; then
        echo "yes"
    elif [[ "$input" == "no" || "$input" == "n" ]]; then
        echo "no"
    elif [[ "$input" == "c" ]]; then
        echo "clean"
    else
        echo "$default"
    fi
}

# Function to get a choice from the user with timeout
get_choice_input() {
    local prompt="$1"
    local choices="$2"
    local default="$3"
    IFS=',' read -r -a valid_choices <<< "$choices"
    local choice

    # Non-interactive: always return default
    if [[ "$INTERACTIVE_MODE" == "non-interactive" ]]; then
        echo "$default"
        return 0
    fi

    while true; do
        choice=$(read_with_timeout "$prompt ($choices)" "$default" "$MAX_TIME")
        choice=$(echo "$choice" | tr '[:upper:]' '[:lower:]')

        for valid_choice in "${valid_choices[@]}"; do
            if [[ "$choice" == "$valid_choice" ]]; then
                echo "$choice"
                return 0
            fi
        done

        echo "Invalid choice. Please enter one of: $choices" >&2
    done
}

# Function to check if 2decomp-fft repository exists
check_2decomp_exists() {
    if [ -d "$PATH_LIB_ROOT/.git" ]; then
        return 0  # Repository exists (has .git directory)
    else
        return 1  # Repository does not exist
    fi
}

# Function to clone 2decomp-fft git repository
clone_2decomp_git() {
    echo "2decomp-fft repository not found. Cloning from GitHub..."
    echo "Cloning: $DECOMP_GIT_URL"
    
    # Remove existing directory if it exists but is not a git repo
    if [ -d "$PATH_LIB_ROOT" ] && [ ! -d "$PATH_LIB_ROOT/.git" ]; then
        echo "⚠️  Found existing directory $PATH_LIB_ROOT but it's not a git repository."
        echo "Removing and re-cloning..."
        rm -rf "$PATH_LIB_ROOT" || { echo "Error: Failed to remove existing directory"; return 1; }
    fi
    
    cd "$LIB_PARENT_DIR" || { echo "Error: Cannot access $LIB_PARENT_DIR"; return 1; }
    
    if git clone "$DECOMP_GIT_URL"; then
        echo "✅ Repository cloned successfully!"
        cd - > /dev/null
        return 0
    else
        echo "❌ Error: Failed to clone repository"
        cd - > /dev/null
        return 1
    fi
}

# Function to clone or refresh 2decomp-fft git repository
setup_2decomp_git() {
    # Check if directory exists but is not a git repo
    if [ -d "$PATH_LIB_ROOT" ] && [ ! -d "$PATH_LIB_ROOT/.git" ]; then
        echo "Found existing directory $PATH_LIB_ROOT but it's not a git repository."
        REMOVE_EXISTING=$(get_yes_no_input "Remove existing directory and clone fresh?" "yes")
        if [[ "$REMOVE_EXISTING" =~ ^(yes|y)$ ]]; then
            echo "Removing existing directory..."
            rm -rf "$PATH_LIB_ROOT" || { echo "Error: Failed to remove existing directory"; return 1; }
            # Now clone
            clone_2decomp_git || return 1
            return 0
        else
            echo "Cannot proceed without a proper git repository."
            return 1
        fi
    fi
    
    # Repository exists as a git repo, proceed with refresh
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
        cd - > /dev/null
        return 0
    fi
    
    # Pull the latest changes
    echo "Pulling latest changes..."
    if ! git pull origin "$CURRENT_BRANCH"; then
        echo "❌ Error: Failed to pull latest changes"
        echo "You may need to resolve conflicts manually."
        echo "Run 'git status' in $PATH_LIB_ROOT to see the current state."
        cd - > /dev/null
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
    
    cd - > /dev/null
    return 0
}

# Function to validate library file
validate_library() {
    local lib_path="$1"
    
    if [ ! -f "$lib_path" ]; then
        echo "❌ Library file not found: $lib_path"
        return 1
    fi
    
    # Check if it's a valid archive using 'ar'
    if ! ar -t "$lib_path" > /dev/null 2>&1; then
        echo "❌ Library file is corrupted or invalid: $lib_path"
        echo "   The archive cannot be read by 'ar' command."
        return 1
    fi
    
    # Check if archive contains any object files
    local obj_count=$(ar -t "$lib_path" 2>/dev/null | wc -l | tr -d ' ')
    if [ "$obj_count" -eq 0 ]; then
        echo "❌ Library file is empty: $lib_path"
        return 1
    fi
    
    # Check for suspicious entries (like lone '/')
    if ar -t "$lib_path" 2>/dev/null | grep -E "^/$|^//" > /dev/null 2>&1; then
        echo "❌ Library contains suspicious '/' entries: $lib_path"
        echo "   This will cause linking errors. Library needs to be rebuilt."
        ar -t "$lib_path" | grep -E "^/$|^//"
        return 1
    fi
    
    # On macOS, check if it's a valid mach-o archive
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if ! file "$lib_path" | grep -q "current ar archive"; then
            echo "❌ Library file is not a valid archive format: $lib_path"
            file "$lib_path"
            return 1
        fi
        
        # Run ranlib to rebuild the index
        echo "Running ranlib on library..."
        ranlib "$lib_path" 2>/dev/null || echo "⚠️  Warning: ranlib failed"
    fi
    
    echo "✅ Library validated: $lib_path ($obj_count object files)"
    return 0
}

# =============================================================================
# MAIN SCRIPT EXECUTION
# =============================================================================
echo "========================================================================="
echo "  CHAPSim2 Build System v2.0"
echo "========================================================================="
echo ""

# Determine Interactive vs Non-Interactive Mode
# Can be set via environment variable: export CHAPSIM_MODE=non-interactive or interactive
CHAPSIM_MODE_ENV="${CHAPSIM_MODE:-}"  # Get env var or empty string

if [[ -n "$CHAPSIM_MODE_ENV" ]]; then
    # Environment variable is set, validate and use it
    CHAPSIM_MODE_ENV=$(echo "$CHAPSIM_MODE_ENV" | tr '[:upper:]' '[:lower:]')
    if [[ "$CHAPSIM_MODE_ENV" =~ ^(interactive|non-interactive)$ ]]; then
        INTERACTIVE_MODE="$CHAPSIM_MODE_ENV"
        echo "Using CHAPSIM_MODE from environment: $INTERACTIVE_MODE"
    else
        echo "⚠️  Warning: Invalid CHAPSIM_MODE='$CHAPSIM_MODE_ENV'. Must be 'interactive' or 'non-interactive'."
        echo "Falling back to user input..."
        CHAPSIM_MODE_ENV=""  # Reset to empty so we ask user below
    fi
fi

# If CHAPSIM_MODE was not set or was invalid, ask user
if [[ -z "$CHAPSIM_MODE_ENV" ]]; then
    echo "Select build mode:"
    echo "  [I]nteractive - prompts with ${MAX_TIME}s timeout"
    echo "  [N]on-interactive - uses all defaults"
    echo ""
    
    MODE_INPUT=$(read_with_timeout "Mode selection [i/N]" "n" "$MAX_TIME")
    MODE_INPUT=$(echo "$MODE_INPUT" | tr '[:upper:]' '[:lower:]')
    
    if [[ "$MODE_INPUT" =~ ^(i|interactive)$ ]]; then
        INTERACTIVE_MODE="interactive"
    else
        INTERACTIVE_MODE="non-interactive"
    fi
fi

if [[ "$INTERACTIVE_MODE" == "interactive" ]]; then
    echo "Running in INTERACTIVE mode (${MAX_TIME}s timeout per prompt)"
else
    echo "Running in NON-INTERACTIVE mode (using all defaults)"
fi

echo ""
echo "========================================================================="
echo ""

# Step 0: Git Repository Setup - Check if repository exists first
# =============================================================================
echo "Step 0: Checking 2decomp-fft library..."
echo ""

# Ensure necessary directories exist
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

echo ""

REFRESH_GIT="no"
LIB_REBUILD="no"

# Check if library files exist
if [[ ! -f "$LIB_FILE" && ! -f "$LIB_FILE_64" ]]; then
    echo "⚠️  Library files not found."
    
    # Check if repository exists
    if check_2decomp_exists; then
        echo "   2decomp-fft repository found but library not built."
        LIB_REBUILD="yes"
    else
        echo "   2decomp-fft repository not found."
        echo "   Will clone repository and build library."
        if clone_2decomp_git; then
            LIB_REBUILD="yes"
        else
            echo "❌ Error: Failed to clone 2decomp-fft repository."
            CONTINUE_ANYWAY=$(get_yes_no_input "Continue with build anyway?" "no")
            if [[ ! "$CONTINUE_ANYWAY" =~ ^(yes|y)$ ]]; then
                echo "Build aborted."
                exit 1
            fi
        fi
    fi
else
    echo "✅ Library files found."
    REFRESH_GIT=$(get_yes_no_input "Refresh 2decomp-fft git repository?" "no")
    
    if [[ "$REFRESH_GIT" =~ ^(yes|y)$ ]]; then
        if setup_2decomp_git; then
            echo "Git repository setup/refresh completed successfully."
            LIB_REBUILD="yes"
        else
            echo "❌ Error: Git repository setup/refresh failed."
            CONTINUE_ANYWAY=$(get_yes_no_input "Continue with build anyway?" "no")
            if [[ ! "$CONTINUE_ANYWAY" =~ ^(yes|y)$ ]]; then
                echo "Build aborted."
                exit 1
            fi
        fi
    else
        LIB_REBUILD=$(get_yes_no_input "Rebuild 2decomp-fft library?" "no")
    fi
fi

# Check if build_cmake_2decomp.sh exists
BUILD_CMAKE_LIB="$REL_PATH_LIB_BUILD/build_cmake_2decomp.sh"
BUILD_CMAKE_BUILD="$REL_PATH_BUILD/build_cmake_2decomp.sh"

if [[ "$LIB_REBUILD" =~ ^(yes|y)$ ]]; then
    if [ ! -f "$BUILD_CMAKE_LIB" ]; then
        echo "build_cmake_2decomp.sh not found in $REL_PATH_LIB_BUILD"
        if [ -f "$BUILD_CMAKE_BUILD" ]; then
            echo "Found build_cmake_2decomp.sh in $REL_PATH_BUILD"
            
            # Create build directory if it doesn't exist
            if [ ! -d "$REL_PATH_LIB_BUILD" ]; then
                echo "Creating build directory: $REL_PATH_LIB_BUILD"
                mkdir -p "$REL_PATH_LIB_BUILD" || { 
                    echo "Error: Failed to create $REL_PATH_LIB_BUILD"; exit 1; 
                }
            fi
            
            echo "Copying to $REL_PATH_LIB_BUILD"
            cp "$BUILD_CMAKE_BUILD" "$BUILD_CMAKE_LIB" || { 
                echo "Error: Failed to copy build_cmake_2decomp.sh"; exit 1; 
            }
            chmod +x "$BUILD_CMAKE_LIB" || { 
                echo "Warning: Failed to make build_cmake_2decomp.sh executable"; 
            }
        else
            echo "❌ Error: build_cmake_2decomp.sh not found in any expected location"
            echo "  Checked:"
            echo "    - $BUILD_CMAKE_LIB"
            echo "    - $BUILD_CMAKE_BUILD"
            exit 1
        fi
    fi
fi

# Step 1: Check and Build the Library
# =============================================================================
if [[ "$LIB_REBUILD" =~ ^(yes|y)$ ]]; then
    echo ""
    echo "========================================================================="
    echo "Step 1: Building / rebuilding 2decomp-fft library..."
    echo "========================================================================="
    echo ""
    
    cd "$PATH_LIB" || { echo "Error: Cannot access $PATH_LIB"; exit 1; }
    
    if [[ -f "$LIB_FILE" || -f "$LIB_FILE_64" ]]; then
        echo "Cleaning previous build artifacts..."
        find . -maxdepth 1 \
            -not -name 'build_cmake_2decomp.sh' \
            -not -name '.' \
            -exec rm -rf {} +
    fi
    
    ./build_cmake_2decomp.sh || {
        echo "❌ Error: CMake build failed in $PATH_LIB"
        exit 1
    }
    
    cd - > /dev/null
    
    # Determine which library exists
    if [[ -f "$LIB_FILE_64" ]]; then
        ACTUAL_LIB="$LIB_FILE_64"
    elif [[ -f "$LIB_FILE" ]]; then
        ACTUAL_LIB="$LIB_FILE"
    else
        echo "❌ Error: No 2decomp-fft library found to validate"
        exit 1
    fi
    
    # Always validate after build
    if ! validate_library "$ACTUAL_LIB"; then
        echo "❌ Error: Library build completed but validation failed:"
        echo "   $ACTUAL_LIB"
        echo "   Please check build_cmake_2decomp.sh and CMake configuration."
        exit 1
    fi
else
    echo "✅ Skipping library build (using existing 2decomp-fft library)."
fi

echo ""

# Step 2: Prompt for Build Options
# =============================================================================
echo "========================================================================="
echo "Step 2: CHAPSim2 Build Configuration"
echo "========================================================================="
echo ""

CLEAN_BUILD=$(get_yes_no_input "Perform a clean build of CHAPSim?" "no")

if [[ "$CLEAN_BUILD" == "clean" ]]; then
    echo "Running 'make clean' in $PATH_BUILD..."
    cd "$PATH_BUILD" || {
        echo "Error: Cannot cd to $PATH_BUILD"
        exit 1
    }
    make clean || echo "Warning: 'make clean' failed."
    cd - >/dev/null || true
fi

# Prompt for build mode
echo ""
echo "CHAPSim build mode: a=default, b=gnu-o3, c=gnu-g, d=gnu-debug, e=intel, f=cray"
BUILD_MODE=$(get_choice_input "Select CHAPSim build mode" "a,b,c,d,e,f" "a")

# Determine make target based on selection
case "$BUILD_MODE" in
    a) MAKE_TARGET="make all" ;;
    b) MAKE_TARGET="make cfg=gnu-o3" ;;
    c) MAKE_TARGET="make cfg=gnu-g" ;;
    d) MAKE_TARGET="make cfg=gnu-debug" ;;
    e) MAKE_TARGET="make cfg=intel" ;;
    f) MAKE_TARGET="make cfg=cray" ;;
    *)
        echo "Error: Unexpected build mode '$BUILD_MODE'. Using default."
        MAKE_TARGET="make all"
        ;;
esac

# Add clean if requested
if [[ "$CLEAN_BUILD" =~ ^(yes|y)$ ]]; then
    MAKE_TARGET="make clean && $MAKE_TARGET"
fi

# Step 3: Run the Build
# =============================================================================
echo ""
echo "========================================================================="
echo "Step 3: Compiling CHAPSim2"
echo "========================================================================="
echo "Executing: $MAKE_TARGET"
echo "Directory: $PATH_BUILD"
echo "========================================================================="
echo ""

cd "$PATH_BUILD" || { echo "Error: Cannot access $PATH_BUILD"; exit 1; }
eval "$MAKE_TARGET" || { echo "Error: Build failed in $PATH_BUILD."; exit 1; }

# Completion Message
# =============================================================================
echo ""
echo "========================================================================="
echo "✅ CHAPSim2 successfully compiled!"
echo "========================================================================="
echo ""
echo "Build configuration: $MAKE_TARGET"
echo "Binary location: $PATH_BIN"
echo ""