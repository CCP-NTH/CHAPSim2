#!/usr/bin/env bash
# Build the CPU solver of a committed source tree outside the work tree (used for the fixture):
#   tests/restart_compact/build_cpu.sh COMMIT DEST
# DEST/bin/CHAPSim; links the 2DECOMP&FFT and FISHPACK builds of the given library checkouts
# (LIB_2DECOMP, LIB_FISHPACK; default: this repository's lib/). Serial make: the Makefile's module
# dependencies are not safe for make -j.
set -euo pipefail
C=${1:?commit}; D=${2:?dest}
R="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
[ -e "$D" ] && { echo "$D exists"; exit 2; }
mkdir -p "$D/lib" "$D/bin" "$D/obj"
git -C "$R" archive "$C" src build/Makefile | tar -x -C "$D"
ln -s "${LIB_2DECOMP:-$R/lib/2decomp-fft}" "$D/lib/2decomp-fft"
ln -s "${LIB_FISHPACK:-$R/lib/fishpack4.1}" "$D/lib/fishpack4.1"
git -C "$R" rev-parse "$C" > "$D/COMMIT"
( cd "$D/build" && make > ../build_cpu.log 2>&1 )
sha256sum "$D/bin/CHAPSim"
