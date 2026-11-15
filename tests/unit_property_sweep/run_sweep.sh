#!/usr/bin/env bash
# Property sweep of one solver build, for a bit-for-bit comparison with another build.
#
#   tests/unit_property_sweep/run_sweep.sh OBJ_DIR MOD_DIR OUT_DIR
#
# OBJ_DIR/MOD_DIR: the object and module directories of a cfg=default build (e.g. obj/ and build/).
# Builds the driver against them with the solver's numerical flags, then sweeps every fluid
# (table fluids in linear and pchip mode) into OUT_DIR. Compare two builds with
#   for f in A/*.bin; do cmp -s "$f" "B/$(basename $f)" || echo "DIFF $(basename $f)"; done
set -eu
R=$(cd "$(dirname "$0")/../.." && pwd)
OBJ=$(cd "$1" && pwd); MOD=$(cd "$2" && pwd); OUT=$3
mkdir -p "$OUT"; OUT=$(cd "$OUT" && pwd)
OBJS=$(ls "$OBJ"/*.o | grep -v '/chapsim.o$')
LIB=$(ls "$R"/lib/2decomp-fft/build/opt/lib*/libdecomp2d.a | head -1)
mpif90 -O3 -fdefault-real-8 -fdefault-double-8 -fallow-argument-mismatch -cpp -DDOUBLE_PREC -fopenmp \
  -ffree-line-length-512 -finit-local-zero -I"$MOD" -I"$R"/lib/2decomp-fft/build/opt/include \
  -o "$OUT/test_property_sweep" "$R/tests/unit_property_sweep/test_property_sweep.f90" $OBJS $LIB
cd "$OUT"; mkdir -p 4_check
cp "$R"/validation/thermal_properties/NIST_WATER_23.5MP.DAT .
# The CO2 table is swept when present; the public distribution does not include it
# (validation/thermal_properties/README.md), so compare builds on the same tree.
CO2="$R"/validation/thermal_properties/NIST_CO2_8MP.DAT
if [ -f "$CO2" ]; then cp "$CO2" .; else echo "SKIP co2: NIST_CO2_8MP.DAT not present" >&2; fi
for m in linear pchip; do
  mpirun -np 1 ./test_property_sweep $m water 645.15 > /dev/null
  if [ -f NIST_CO2_8MP.DAT ]; then mpirun -np 1 ./test_property_sweep $m co2 304.25 > /dev/null; fi
done
for fr in "sodium 600" "lead 700" "bismuth 700" "lbe 600" "lithium 600" "flibe 800" "pbli 573"; do
  set -- $fr
  mpirun -np 1 ./test_property_sweep linear "$1" "$2" > /dev/null
done
ls "$OUT"/*.bin | wc -l
