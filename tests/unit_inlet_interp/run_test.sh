#!/usr/bin/env bash
# Inlet-database interpolation test (solver level). Run after ./build_chapsim.sh.
# Builds a synthetic analytic database (32 theta points, 24 radial cells), runs bin/CHAPSim on a
# 32 x 24 x 64 pipe (theta interpolation 32 -> 64) with two decompositions and on a 32 x 24 x 32
# pipe (matching mesh, direct read), then checks the inlet planes the solver wrote to its
# checkpoints (check_inlet_interp.py), plus the decomposition guard of the interpolation halo
# (a valid uneven 1 x 12 layout and an incompatible 1 x 8 layout with a 12-point database).
# A few minutes on up to 12 ranks; scratch < 1 GB.
# usage: run_test.sh [scratch_dir]
set -eu
H=$(cd "$(dirname "$0")" && pwd)
R=$(cd "$H/../.." && pwd)
W=${1:-$(mktemp -d /tmp/chapsim_inlet_interp.XXXX)}
EXE=${CHAPSIM_EXE:-$R/bin/CHAPSim}   # CHAPSIM_EXE overrides, e.g. to test another executable
[ -x "$EXE" ] || { echo "missing $EXE (build first)"; exit 2; }
LZZ=6.283185307179586
run_case() {   # name ncz p_row p_col np [database theta count, default 32] [allow_fail]
  local d=$W/$1
  mkdir -p "$d/1_data"
  python3 "$H/make_db.py" "$d/1_data" 24 "${6:-32}" 25 $LZZ 0 25
  sed -e "s/NCZ/$2/" -e "s/PROW/$3/" -e "s/PCOL/$4/" "$H/input_chapsim.template.ini" > "$d/input_chapsim.ini"
  if [[ -n "${7:-}" ]]; then
    (cd "$d" && mpirun --bind-to none -np "$5" "$EXE" < input_chapsim.ini > run.log 2>&1) || true
    return
  fi
  (cd "$d" && mpirun --bind-to none -np "$5" "$EXE" < input_chapsim.ini > run.log 2>&1) || { echo "solver failed in $d"; exit 1; }
  grep -q 'finished successfully' "$d/run.log" || { echo "solver did not finish in $d"; exit 1; }
}
run_case interp_a 64 1 2 2
run_case interp_b 64 2 1 2
run_case identity 32 1 2 2
# halo/decomposition guard: uneven 1 x 12 split needs 3 halo layers, all within the adjacent
# source blocks (must run, planes as interp_a); a 12-point database on 1 x 8 needs layers the
# adjacent 1-cell blocks do not hold (must stop with the diagnostic before any interpolated value)
run_case guard_ok 64 1 12 12
run_case guard_bad 64 1 8 8 12 allow_fail
grep -q 'cross-section mesh differs' "$W/interp_a/run.log" || { echo "interpolation path not active"; exit 1; }
! grep -q 'cross-section mesh differs' "$W/identity/run.log" || { echo "identity case interpolated"; exit 1; }
python3 "$H/check_inlet_interp.py" "$W"
