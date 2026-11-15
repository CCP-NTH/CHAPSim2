#!/usr/bin/env bash
# Build the property-interpolation unit driver against the current solver objects (obj/, build/ modules).
# Run after ./build_chapsim.sh. Uses the default solver flags that matter for the numerics.
set -eu
R=$(cd "$(dirname "$0")/../.." && pwd)
OBJS=$(ls $R/obj/*.o | grep -v '/chapsim.o$')
LIB=$(ls $R/lib/2decomp-fft/build/opt/lib*/libdecomp2d.a | head -1)
mpif90 -O3 -fdefault-real-8 -fdefault-double-8 -fallow-argument-mismatch -cpp -DDOUBLE_PREC -fopenmp \
  -ffree-line-length-512 -I$R/build -I$R/lib/2decomp-fft/build/opt/include \
  -o $(dirname "$0")/test_property_interp $(dirname "$0")/test_property_interp.f90 $OBJS $LIB
echo built
