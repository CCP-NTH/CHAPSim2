#!/bin/bash
set -euo pipefail

# =============================================================================
# 2decomp-fft Library Build Script with Architecture Auto-Detection
# - On ARCHER2/Cray: auto-load cray-fftw (if available) and use FFTW_F03
# - Otherwise: if FFTW_ROOT exists and looks valid, use FFTW_F03; else generic
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

    if [[ "$OSTYPE" == "darwin"* ]]; then
        platform="macOS"
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

    # default (may be overridden later on Cray)
    export FC=${FC:-mpif90}

    if [[ "$platform" == "macOS" ]]; then
        if [[ "$arch" == "arm64" ]]; then
            export FFLAGS="-arch arm64 -fallow-argument-mismatch"
            export FCFLAGS="-arch arm64 -fallow-argument-mismatch"
            export CFLAGS="-arch arm64"
            export CXXFLAGS="-arch arm64"
            echo "✅ Using Apple Silicon flags: -arch arm64"
        elif [[ "$arch" == "x86_64" ]]; then
            export FFLAGS="-arch x86_64 -fallow-argument-mismatch"
            export FCFLAGS="-arch x86_64 -fallow-argument-mismatch"
            export CFLAGS="-arch x86_64"
            export CXXFLAGS="-arch x86_64"
            echo "✅ Using Intel Mac flags: -arch x86_64"
        fi
    else
        export FFLAGS="-fallow-argument-mismatch"
        export FCFLAGS="-fallow-argument-mismatch"
        echo "✅ Using standard Unix flags"
    fi

    echo "FC (initial): $FC"
    echo "FFLAGS: ${FFLAGS:-none}"
    echo ""
}

# -----------------------------------------------------------------------------
# ARCHER2 / Cray environment setup:
# - detect Cray PE
# - load cray-fftw module if module system exists
# - use Cray wrapper compilers (ftn/cc/CC)
# - set FFTW_ROOT from CRAY_FFTW_PREFIX when available
# -----------------------------------------------------------------------------
setup_archer2_cray_env() {
    # module is often a shell function; source init if needed
    if ! command -v module >/dev/null 2>&1; then
        if [[ -f /etc/profile.d/modules.sh ]]; then
            # shellcheck disable=SC1091
            source /etc/profile.d/modules.sh
        fi
    fi

    local is_cray=false
    if [[ -n "${CRAYPE_VERSION:-}" ]] || [[ -n "${PE_ENV:-}" ]] || [[ -n "${CRAY_FFTW_PREFIX:-}" ]]; then
        is_cray=true
    fi

    if [[ "$is_cray" == true ]]; then
        echo "🛰️  Detected Cray PE / ARCHER2-like environment"
        echo "   CRAYPE_VERSION=${CRAYPE_VERSION:-unknown}  PE_ENV=${PE_ENV:-unknown}"

        # Prefer Cray wrappers unless user already forced something
        export FC=${FC:-ftn}
        export CC=${CC:-cc}
        export CXX=${CXX:-CC}

        # Load cray-fftw if possible and not already loaded
        if command -v module >/dev/null 2>&1; then
            if ! module -t list 2>&1 | grep -q '^cray-fftw/'; then
                echo "Loading module: cray-fftw"
                module load cray-fftw || echo "⚠️  Warning: module load cray-fftw failed"
            else
                echo "✅ cray-fftw already loaded"
            fi
        fi

        # Prefer CRAY_FFTW_PREFIX as FFTW_ROOT
        if [[ -n "${CRAY_FFTW_PREFIX:-}" ]] && [[ -d "${CRAY_FFTW_PREFIX}" ]]; then
            export FFTW_ROOT="${FFTW_ROOT:-$CRAY_FFTW_PREFIX}"
            echo "✅ FFTW_ROOT set from CRAY_FFTW_PREFIX: $FFTW_ROOT"
        fi

        echo "FC (Cray): $FC"
        echo "CC (Cray): $CC"
        echo ""
    fi
}

# -----------------------------------------------------------------------------
# Choose FFT backend.
#
# Default is the generic transform bundled inside 2decomp-fft: it needs nothing
# beyond the vendored sources, so a clean checkout always builds, which is what
# CI relies on. FFTW is opt-in via CHAPSIM_FFT=fftw and is the faster choice for
# production runs. See docs/guidance/docs/fft-backend.md.
#
# Opting in is explicit rather than automatic, and a failed opt-in is fatal
# rather than a silent downgrade. Both of those are deliberate: this function
# used to select FFTW whenever FFTW_ROOT merely looked plausible, and it passed
# -DFFT_Choice=FFTW_F03 where the library matches "fftw_f03" case-sensitively
# (lib/2decomp-fft/cmake/fft/fft.cmake:34). The value was accepted, the match
# failed, and every such build announced an FFTW backend while compiling the
# generic one. A loud failure is worth more here than a working-but-wrong build.
# -----------------------------------------------------------------------------

# Echo the directory under $1 that holds libfftw3, or return 1. Handles lib,
# lib64 and Debian/Ubuntu multiarch (lib/x86_64-linux-gnu, lib/aarch64-...).
find_fftw_libdir() {
    local root="$1" d
    for d in "$root/lib" "$root/lib64" "$root"/lib/*-linux-gnu; do
        if compgen -G "$d/libfftw3.*" >/dev/null 2>&1; then
            echo "$d"
            return 0
        fi
    done
    return 1
}

choose_fft_backend() {
    FFT_CHOICE="generic"
    FFTW_CMAKE_ARGS=""
    FFTW_LINK_FLAGS=""

    local want
    want="$(echo "${CHAPSIM_FFT:-generic}" | tr '[:upper:]' '[:lower:]')"

    local fftw_root="${FFTW_ROOT:-/usr/local}"
    fftw_root="${fftw_root%/}"

    local inc_ok=false libdir=""
    if [[ -f "$fftw_root/include/fftw3.f03" ]] || [[ -f "$fftw_root/include/fftw3.h" ]]; then
        inc_ok=true
    fi
    libdir="$(find_fftw_libdir "$fftw_root" || true)"

    if [[ "$want" != "fftw" && "$want" != "fftw_f03" ]]; then
        echo "   -> Using FFT backend: generic (bundled in 2decomp-fft)"
        if [[ "$inc_ok" == true && -n "$libdir" ]]; then
            echo "      FFTW is installed at $fftw_root but is not enabled."
            echo "      To use it:  CHAPSIM_FFT=fftw ./build_chapsim.sh"
        fi
        echo ""
        return 0
    fi

    if [[ "$inc_ok" != true ]] || [[ -z "$libdir" ]]; then
        echo "❌ CHAPSIM_FFT=$want was requested, but no usable FFTW was found."
        echo "   Looked under FFTW_ROOT: $fftw_root"
        echo "   header (include/fftw3.h or fftw3.f03): $inc_ok"
        echo "   library (lib, lib64 or lib/<arch>-linux-gnu): ${libdir:-not found}"
        echo ""
        echo "   Install FFTW and point FFTW_ROOT at its prefix, for example:"
        echo "     Ubuntu/Debian : sudo apt install libfftw3-dev && export FFTW_ROOT=/usr"
        echo "     from source   : export FFTW_ROOT=/usr/local"
        echo "     ARCHER2/Cray  : module load cray-fftw   (FFTW_ROOT is then set for you)"
        echo ""
        echo "   Not falling back to generic: you asked for FFTW, so a generic"
        echo "   build here would be the silent downgrade this check exists to stop."
        return 1
    fi

    # Lower case is required - the library matches it case-sensitively.
    FFT_CHOICE="fftw_f03"
    FFTW_CMAKE_ARGS="-DFFT_Choice=fftw_f03 -DFFTW_ROOT=$fftw_root"
    FFTW_LINK_FLAGS="-L$libdir -lfftw3"
    echo "✅ FFTW found at: $fftw_root"
    echo "   library directory: $libdir"
    echo "   -> Using FFT backend: fftw_f03"
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
        ${FFTW_CMAKE_ARGS} \
        || { echo "❌ CMake configuration failed"; return 1; }

    echo "✅ CMake configuration complete"
    echo ""
}

# -----------------------------------------------------------------------------
# Build the Library
# -----------------------------------------------------------------------------
build_library() {
    echo "Building library..."
    local log_file="build_2decomp.log"
    if cmake --build ./ --parallel >"$log_file" 2>&1; then
        echo "✅ Build complete"
    else
        echo "❌ Build failed"
        echo "Last 30 lines from $log_file:"
        tail -30 "$log_file"
        return 1
    fi
    echo "Build log saved to: $(pwd)/$log_file"
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
# Record the backend for the solver build.
#
# build/Makefile includes this fragment to decide whether to link FFTW. Writing
# it here keeps one source of truth: the flags that link the solver are the ones
# the library was actually configured with, even if the user later runs `make`
# on its own without re-running this script.
# -----------------------------------------------------------------------------
write_backend_fragment() {
    local out="$(pwd)/opt/chapsim_fft_backend.mk"
    cat > "$out" <<EOF
# Generated by build/build_cmake_2decomp.sh - do not edit.
# FFT backend that lib/2decomp-fft was built with.
CHAPSIM_FFT_BACKEND = ${FFT_CHOICE}
CHAPSIM_FFT_LDFLAGS = ${FFTW_LINK_FLAGS}
EOF
    echo "Recorded FFT backend for the solver build: $out"
    echo ""
}

# -----------------------------------------------------------------------------
# Verify Installation
# -----------------------------------------------------------------------------
verify_installation() {
    echo "Verifying installation..."

    local lib_path=""

    if [ -f "./opt/lib64/libdecomp2d.a" ]; then
        lib_path="./opt/lib64/libdecomp2d.a"
    elif [ -f "./opt/lib/libdecomp2d.a" ]; then
        lib_path="./opt/lib/libdecomp2d.a"
    else
        echo "❌ ERROR: libdecomp2d.a not found in opt/lib or opt/lib64"
        return 1
    fi

    echo "📍 Library location: $lib_path"

    echo ""
    echo "File information:"
    file "$lib_path"

    echo ""
    echo "Archive contents (first 10 entries):"
    if ar -t "$lib_path" > /dev/null 2>&1; then
        ar -t "$lib_path" | head -10
        local obj_count
        obj_count=$(ar -t "$lib_path" | wc -l | tr -d ' ')
        echo "..."
        echo "Total object files: $obj_count"
    else
        echo "❌ ERROR: Cannot read archive contents"
        return 1
    fi

    echo ""
    echo "Checking for suspicious entries..."
    if ar -t "$lib_path" | grep -E "^/$|^//" > /dev/null 2>&1; then
        echo "⚠️  WARNING: Found suspicious '/' entries in archive!"
        ar -t "$lib_path" | grep -E "^/$|^//"
        return 1
    else
        echo "✅ No suspicious entries found"
    fi

    if command -v lipo &> /dev/null; then
        echo ""
        echo "Architecture information:"
        lipo -info "$lib_path" 2>&1 || echo "Note: lipo info not available for static libraries"
    fi

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

ARCH=$(uname -m)
PLATFORM=""
if [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="macOS"
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    PLATFORM="Linux"
else
    PLATFORM="Unknown"
fi

detect_architecture
set_compiler_flags "$ARCH" "$PLATFORM"

# ARCHER2/Cray optional setup (loads cray-fftw and sets FFTW_ROOT automatically)
setup_archer2_cray_env

# Decide FFT backend (generic unless CHAPSIM_FFT=fftw; fatal if FFTW is asked
# for and missing, rather than downgrading silently)
choose_fft_backend || exit 1

clean_build
configure_cmake || exit 1
build_library || exit 1
install_library || exit 1
write_backend_fragment || exit 1
verify_installation || exit 1

echo "========================================================================="
echo "✅ 2decomp-fft library build completed successfully!"
echo "========================================================================="
echo ""
echo "Library installed to: $(pwd)/opt"
echo "FFT backend selected: ${FFT_CHOICE}"
echo ""

if [ -f "./opt/lib64/libdecomp2d.a" ]; then
    echo "Use this path in your Makefile: ../lib/2decomp-fft/build/opt/lib64"
elif [ -f "./opt/lib/libdecomp2d.a" ]; then
    echo "Use this path in your Makefile: ../lib/2decomp-fft/build/opt/lib"
fi
echo ""
