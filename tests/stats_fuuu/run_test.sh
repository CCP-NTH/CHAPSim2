#!/usr/bin/env bash
# t_avg_fuuu must hold the Favre triple moment rho u_i u_j u_k, and statistics written with the
# former weight rho*T must not be continued.
#
#   tests/stats_fuuu/run_test.sh [EXE] [WORKDIR]        (NP=2)
#
# Case: tests/regression/channel_scp_periodic_Tw (stat_level 3), thermal field from step 1,
# ref_t0 = 640 K below tini = 645.15 K so that T /= 1 in every cell, 4 steps with stat_istart = 3:
# exactly one sample. Check 1 (tests/tools/stats_fuuu_identity.py): fuuu_ijk f = fuu_ij fu_k to
# round-off. Check 2: the same flow statistics with the key fuuu_definition removed (a file of the
# former definition) must stop a statistics restart.
# Exit 0 if both checks pass.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXE=$(realpath "${1:-$ROOT/bin/CHAPSim}")
WORK=${2:-$(mktemp -d /tmp/chapsim_stats_fuuu.XXXXXX)}
NP=${NP:-2}
TMPL=$ROOT/tests/regression/channel_scp_periodic_Tw
mkdir -p "$WORK"
mkrun() {  # DIR FIRST LAST ISTART
  local d=$1; rm -rf "$d"; mkdir -p "$d/1_data"
  cp "$TMPL"/*.DAT "$TMPL"/input_chapsim.ini "$d/"
  sed -i -e "s/^\( *ref_t0 *= *\).*/\1640.0/" \
         -e "s/^\( *niterflowfirst *= *\).*/\1$2/" -e "s/^\( *niterflowlast *= *\).*/\1$3/" \
         -e "s/^\( *niterthermofirst *= *\).*/\1$2/" -e "s/^\( *niterthermolast *= *\).*/\1$3/" \
         -e "s/^\( *ckpt_nfre *= *\).*/\14/" -e "s/^\( *visu_nfre *= *\).*/\1100000/" \
         -e "s/^\( *stat_istart *= *\).*/\1$4/" "$d/input_chapsim.ini"
}
status=0
mkrun "$WORK/one" 1 4 3
( cd "$WORK/one" && mpirun -np "$NP" "$EXE" > run.log 2>&1 < /dev/null )
grep -q 'finished successfully' "$WORK/one/run.log" || { echo "run FAILED ($WORK/one/run.log)"; exit 2; }
python3 -I "$ROOT/tests/tools/stats_fuuu_identity.py" "$WORK/one/1_data" 4 || status=1
mkrun "$WORK/legacy" 5 8 3
sed -i -e "/^\[flow\]/,/^\[/ s/^\( *initfl *= *\).*/\1restart/" -e "s/^\( *inittm *= *\).*/\1restart/" \
       -e "s/^\( *irestartfrom *= *\).*/\14/" "$WORK/legacy/input_chapsim.ini"
cp "$WORK"/one/1_data/*_4.* "$WORK/legacy/1_data/"
sed -i '/^fuuu_definition /d' "$WORK/legacy/1_data/domain1_flow_stats_meta_4.dat"
( cd "$WORK/legacy" && mpirun -np "$NP" "$EXE" > run.log 2>&1 < /dev/null )
if grep -q 'former definition rho\*T' "$WORK/legacy/run.log"; then echo "legacy statistics: restart STOPPED as required"
else echo "legacy statistics: NOT stopped ($WORK/legacy/run.log)"; status=1; fi
exit $status
