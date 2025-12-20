#!/bin/bash
set -euo pipefail

# =============================================================================
# 2decomp-fft Library Build Script with Architecture Auto-Detection
# =============================================================================

echo "========================================================================="
echo "  2decomp-fft Library Build"
echo "========================================================================="
echo ""

# -----------------------------------------------------------------------------
# Detect Architecture and Platform
# -----------------------------------------------------------------------------
detect_architecture() {
    local arch=""
    local platform=""
    
    # Detect OS
    if [[ "$OSTYPE" == "darwin"* ]]; then
        platform="macOS"
        
        # Detect Mac architecture
        if [[ $(uname -m) == "arm64" ]]; then
            arch="arm64"
            echo "Detected: Apple Silicon (M1/M2/M3/M4)"
        elif [[ $(uname -m) == "x86_64" ]]; then
            arch="x86_64"
            echo "Detected: Intel Mac"
        else
            arch=$(uname -m)
            echo "Detected: macOS - $(uname -m)"
        fi
        export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
        export AR=/usr/bin/ar
        export RANLIB=/usr/bin/ranlib
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        platform="Linux"
        arch=$(uname -m)
        echo "🐧 Detected: Linux - $arch"
    else
        platform="Unknown"
        arch=$(uname -m)
        echo "❓ Detected: $OSTYPE - $arch"
    fi
    
    echo "Platform: $platform"
    echo "Architecture: $arch"
    echo ""
}

# -----------------------------------------------------------------------------
# Set Architecture-Specific Compiler Flags
# -----------------------------------------------------------------------------
set_compiler_flags() {
    local arch="$1"
    local platform="$2"
    
    export FC=${FC:-mpif90}
    
    if [[ "$platform" == "macOS" ]]; then
        if [[ "$arch" == "arm64" ]]; then
            # Apple Silicon
            export FFLAGS="-arch arm64 -fallow-argument-mismatch"
            export FCFLAGS="-arch arm64 -fallow-argument-mismatch"
            export CFLAGS="-arch arm64"
            export CXXFLAGS="-arch arm64"
            echo "✅ Using Apple Silicon flags: -arch arm64"
        elif [[ "$arch" == "x86_64" ]]; then
            # Intel Mac
            export FFLAGS="-arch x86_64 -fallow-argument-mismatch"
            export FCFLAGS="-arch x86_64 -fallow-argument-mismatch"
            export CFLAGS="-arch x86_64"
            export CXXFLAGS="-arch x86_64"
            echo "✅ Using Intel Mac flags: -arch x86_64"
        fi
    else
        # Linux or other Unix-like systems
        export FFLAGS="-fallow-argument-mismatch"
        export FCFLAGS="-fallow-argument-mismatch"
        echo "✅ Using standard Unix flags"
    fi
    
    echo "FC: $FC"
    echo "FFLAGS: ${FFLAGS:-none}"
    echo ""
}

# -----------------------------------------------------------------------------
# Clean Previous Build
# -----------------------------------------------------------------------------
clean_build() {
    echo "Cleaning previous build artifacts..."
    rm -rf CMakeCache.txt CMakeFiles/ cmake_install.cmake Makefile
    rm -rf opt/ lib/ lib64/ include/ bin/
    rm -rf src/CMakeFiles/ examples/CMakeFiles/ 
    echo "✅ Clean complete"
    echo ""
}

# -----------------------------------------------------------------------------
# Configure with CMake
# -----------------------------------------------------------------------------
configure_cmake() {
    echo "Configuring with CMake..."
    
    local install_prefix="$(pwd)/opt"
    
    cmake -S ../ -B ./ \
        -DCMAKE_INSTALL_PREFIX="$install_prefix" \
        -DCMAKE_Fortran_COMPILER="$FC" \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=OFF \
        -DCMAKE_Fortran_FLAGS="${FFLAGS:-}" \
        -DCMAKE_C_FLAGS="${CFLAGS:-}" \
        -DCMAKE_CXX_FLAGS="${CXXFLAGS:-}" \
        || { echo "❌ CMake configuration failed"; return 1; }
    
    echo "✅ CMake configuration complete"
    echo ""
}

# -----------------------------------------------------------------------------
# Build the Library
# -----------------------------------------------------------------------------
build_library() {
    echo "Building library..."
    
    cmake --build ./ || { echo "❌ Build failed"; return 1; }
    
    echo "✅ Build complete"
    echo ""
}

# -----------------------------------------------------------------------------
# Install the Library
# -----------------------------------------------------------------------------
install_library() {
    echo "Installing library..."
    
    cmake --install ./ || { echo "❌ Installation failed"; return 1; }
    
    echo "✅ Installation complete"
    echo ""
}

# -----------------------------------------------------------------------------
# Verify Installation
# -----------------------------------------------------------------------------
verify_installation() {
    echo "Verifying installation..."
    
    local lib_path=""
    
    # Check both lib and lib64 directories
    if [ -f "./opt/lib64/libdecomp2d.a" ]; then
        lib_path="./opt/lib64/libdecomp2d.a"
    elif [ -f "./opt/lib/libdecomp2d.a" ]; then
        lib_path="./opt/lib/libdecomp2d.a"
    else
        echo "❌ ERROR: libdecomp2d.a not found in opt/lib or opt/lib64"
        return 1
    fi
    
    echo "Library location: $lib_path"
    
    # Check file type
    echo ""
    echo "File information:"
    file "$lib_path"
    
    # List archive contents (first 10 entries)
    echo ""
    echo "Archive contents (first 10 entries):"
    if ar -t "$lib_path" > /dev/null 2>&1; then
        ar -t "$lib_path" | head -10
        local obj_count=$(ar -t "$lib_path" | wc -l | tr -d ' ')
        echo "..."
        echo "Total object files: $obj_count"
    else
        echo "❌ ERROR: Cannot read archive contents"
        return 1
    fi
    
    # Check for suspicious entries
    echo ""
    echo "Checking for suspicious entries..."
    if ar -t "$lib_path" | grep -E "^/$|^//" > /dev/null 2>&1; then
        echo "⚠️  WARNING: Found suspicious '/' entries in archive!"
        ar -t "$lib_path" | grep -E "^/$|^//"
        return 1
    else
        echo "✅ No suspicious entries found"
    fi
    
    # On macOS, check architecture
    if command -v lipo &> /dev/null; then
        echo ""
        echo "Architecture information:"
        lipo -info "$lib_path" 2>&1 || echo "Note: lipo info not available for static libraries"
    fi
    
    # Rebuild archive index
    echo ""
    echo "Rebuilding archive index with ranlib..."
    ranlib "$lib_path" || echo "⚠️  Warning: ranlib failed"
    
    echo ""
    echo "✅ Verification complete"
    echo ""
}

# =============================================================================
# Main Execution
# =============================================================================

# Detect system
ARCH=$(uname -m)
PLATFORM=""
if [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="macOS"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    PLATFORM="Linux"
else
    PLATFORM="Unknown"
fi

# Run build steps
detect_architecture
set_compiler_flags "$ARCH" "$PLATFORM"
clean_build
configure_cmake || exit 1
build_library || exit 1
install_library || exit 1
verify_installation || exit 1

echo "========================================================================="
echo "✅ 2decomp-fft library build completed successfully!"
echo "========================================================================="
echo ""
echo "Library installed to: $(pwd)/opt"
echo ""

# Show final library location
if [ -f "./opt/lib64/libdecomp2d.a" ]; then
    echo "Use this path in your Makefile: ../lib/2decomp-fft/build/opt/lib64"
elif [ -f "./opt/lib/libdecomp2d.a" ]; then
    echo "Use this path in your Makefile: ../lib/2decomp-fft/build/opt/lib"
fi
echo ""