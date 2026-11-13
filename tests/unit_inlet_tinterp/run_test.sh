#!/usr/bin/env bash
# Temporal inlet-database interpolation test (solver level). Run after ./build_chapsim.sh.
# Synthetic time-dependent database: 2 blocks x 25 samples, dt_db = 5e-4 (make_db.py), 26 x 24 x 32
# pipe on the database mesh (no spatial interpolation). Cases:
#   ref1   dt = dt_db, ndb_sampling_dt = dt_db (R = 1)        nokey  dt = dt_db, no key
#   r3_a   dt = dt_db/3, ndb_sampling_dt = dt_db (R = 3), decomposition 1 x 2
#   r3_b   as r3_a, decomposition 2 x 1
#   rst_N  restart of r3_a from its checkpoint N (40, 73, 75, 148) to the same end
#   bad_ratio, bad_meta   input validation (must stop)
# then check_inlet_tinterp.py. A few minutes on 2 ranks; scratch about 1 GB, removed on success
# unless a scratch dir is given.
# usage: run_test.sh [scratch_dir]
set -eu
H=$(cd "$(dirname "$0")" && pwd)
R=$(cd "$H/../.." && pwd)
KEEP=${1:+1}
W=${1:-$(mktemp -d /tmp/chapsim_inlet_tinterp.XXXX)}
EXE=${CHAPSIM_EXE:-$R/bin/CHAPSim}   # CHAPSIM_EXE overrides, e.g. to test another executable
[ -x "$EXE" ] || { echo "missing $EXE (build first)"; exit 2; }
LZZ=6.283185307179586
DTDB=5.0e-4
DT3=1.6666666666666666e-04
PERIOD=12
NR3=155
mkinput() {   # dir dt niter prow pcol keyline initfl irestart
  local d=$1
  mkdir -p "$d/1_data"
  if [[ -n "$6" ]]; then
    sed -e "s/DTRUN/$2/" -e "s/NITER/$3/" -e "s/PROW/$4/" -e "s/PCOL/$5/" -e "s/KEYLINE/$6/" \
        -e "s/INITFL/$7/" -e "s/IRST/$8/" "$H/input_chapsim.template.ini" > "$d/input_chapsim.ini"
  else
    sed -e "s/DTRUN/$2/" -e "s/NITER/$3/" -e "s/PROW/$4/" -e "s/PCOL/$5/" -e "/KEYLINE/d" \
        -e "s/INITFL/$7/" -e "s/IRST/$8/" "$H/input_chapsim.template.ini" > "$d/input_chapsim.ini"
  fi
  python3 "$H/make_db.py" "$d/1_data" 24 32 25 2 $DTDB $LZZ $PERIOD
}
run() {   # dir np [allow_fail]
  (cd "$1" && mpirun --bind-to none -np "$2" "$EXE" < input_chapsim.ini > run.log 2>&1) || true
  if [[ -z "${3:-}" ]]; then
    grep -q 'finished successfully' "$1/run.log" || { echo "solver did not finish in $1"; exit 1; }
  fi
}
mkinput $W/ref1  $DTDB 52   1 2 "ndb_sampling_dt= $DTDB" poiseuille 0; run $W/ref1 2
mkinput $W/nokey $DTDB 52   1 2 ""                         poiseuille 0; run $W/nokey 2
mkinput $W/r3_a  $DT3  $NR3 1 2 "ndb_sampling_dt= $DTDB" poiseuille 0; run $W/r3_a 2
mkinput $W/r3_b  $DT3  $NR3 2 1 "ndb_sampling_dt= $DTDB" poiseuille 0; run $W/r3_b 2
for N in 40 73 75 148; do
  mkinput $W/rst_$N $DT3 $NR3 1 2 "ndb_sampling_dt= $DTDB" restart $N
  for f in $W/r3_a/1_data/domain1_flow_restart_$N.bin $W/r3_a/1_data/domain1_flow_restart_meta_$N.dat \
           $W/r3_a/1_data/domain1_checkpoint_meta_$N.dat; do cp "$f" $W/rst_$N/1_data/; done
  run $W/rst_$N 2
done
mkinput $W/bad_ratio 4.0e-4 5 1 2 "ndb_sampling_dt= $DTDB" poiseuille 0; run $W/bad_ratio 2 allow_fail
mkinput $W/bad_meta  $DTDB  5 1 2 "ndb_sampling_dt= 1.0e-3" poiseuille 0; run $W/bad_meta 2 allow_fail
rc=0
python3 "$H/check_inlet_tinterp.py" "$W" 3 $PERIOD || rc=$?
[[ $rc -eq 0 && -z "$KEEP" ]] && rm -rf "$W"
exit $rc
