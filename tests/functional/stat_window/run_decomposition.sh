#!/usr/bin/env bash
# Decomposition independence of the statistical-window post-processing mode.
#
# One data set per case is produced once (NP_DNS ranks, default 4); the same
# snapshots are then post-processed with each rank count in NPS (default
# "2 4 8"), one run after another (concurrent OpenMPI MPI-IO jobs can deadlock
# on the shared-file-pointer semaphores). Every run must take zero
# time steps and leave the case files sha256-identical; its window output is
# compared with the NP_DNS one:
#   - 3-D statistics bundles (reconstruction is pointwise on global arrays):
#     tests/tools/compare_stats_restart.py, array-relative 1e-10
#   - y-profiles (plane averages, reduction order depends on the decomposition):
#     tests/tools/compare_stat_window_profiles.py scaled by the 3-D field, 1e-10
# Cases: thermal MHD inlet-outlet channel (flow incl. Favre, thermo, MHD) and
# isothermal MHD periodic channel (flow, MHD, profiles).
#
# Usage: tests/functional/stat_window/run_decomposition.sh
#        NPS="2 4 8" NP_DNS=4 WORK_DIR=<scratch> KEEP=1
set -u
R=$(cd "$(dirname "$0")/../../.." && pwd)
BIN=$R/bin/CHAPSim
NPS=${NPS:-"2 4 8"}
NP_DNS=${NP_DNS:-4}
WORK=${WORK_DIR:-$(mktemp -d /tmp/chapsim_statwin_np.XXXXXX)}
CMP=$R/tests/tools/compare_stats_restart.py
CMPP=$R/tests/tools/compare_stat_window_profiles.py
RTOL=1e-10
PASS=0
FAIL=0
mkdir -p "$WORK"
echo "statistical-window decomposition tests, work dir $WORK, DNS NP=$NP_DNS, post NP: $NPS"
ok()   { echo "  PASS $*"; PASS=$((PASS + 1)); }
bad()  { echo "  FAIL $*"; FAIL=$((FAIL + 1)); }
check(){ local name=$1; shift; if "$@" > "$WORK/last_check.out" 2>&1; then ok "$name"; \
         sed -n '/worst\|resolved/s/^/      /p' "$WORK/last_check.out"; \
         else bad "$name"; sed 's/^/      /' "$WORK/last_check.out"; fi; }
set_key() { sed -i "0,/^$2=.*/s||$2= $3|" "$1"; }
case_sha() { ( cd "$1" && find . -type f -not -path './5_postprocess/*' -not -name 'post_*.log' \
      -not -name 'input_chapsim.ini' -not -name 'input_dns.ini.bak' -print0 | sort -z | xargs -0 sha256sum ); }

post_np() {   # dir np a b -> window output moved to $WORK/<case>_np<np>
  local d=$1 np=$2 a=$3 b=$4 name before
  name=$(basename "$d")_np$np
  cp "$d/input_chapsim.ini" "$d/input_dns.ini.bak"
  set_key "$d/input_chapsim.ini" is_postprocess .true.
  sed -i "/^is_postprocess=/a pp_stat_window= $a, $b" "$d/input_chapsim.ini"
  before=$(case_sha "$d")
  ( cd "$d" && mpirun -np "$np" "$BIN" > "post_np$np.log" 2>&1 ); local st=$?
  cp "$d/input_dns.ini.bak" "$d/input_chapsim.ini"
  if [[ $st -eq 0 ]] && grep -q '0 time steps advanced' "$d/post_np$np.log" \
     && ! grep -q -E 'Time Step =|Solving the governing equations' "$d/post_np$np.log" \
     && [[ "$before" == "$(case_sha "$d")" ]]; then
    ok "$(basename "$d") post-processing at NP=$np: zero steps, originals unchanged"
  else bad "$(basename "$d") post-processing at NP=$np"; fi
  mv "$d/5_postprocess/window_${a}_${b}" "$WORK/$name"
}

# thermal MHD inlet-outlet channel
MT_SRC=$R/tests/functional/MHD_channel_scp_inout_Tw
M=$WORK/mhd_thermal
mkdir -p $M/1_data; cp $MT_SRC/input_chapsim.ini $MT_SRC/*.DAT $M/; cp $MT_SRC/1_data/* $M/1_data/
set_key $M/input_chapsim.ini ckpt_nfre 10
set_key $M/input_chapsim.ini visu_nfre 1000
set_key $M/input_chapsim.ini niterthermofirst 1
( cd $M && mpirun -np "$NP_DNS" "$BIN" > run.log 2>&1 ) || bad "thermal MHD DNS"
# isothermal MHD periodic channel
MP_SRC=$R/tests/functional/MHD_channel_iso_periodic_Btilt
P=$WORK/mhd_periodic
mkdir -p $P; cp $MP_SRC/input_chapsim.ini $P/
set_key $P/input_chapsim.ini ckpt_nfre 10
set_key $P/input_chapsim.ini visu_nfre 1000
sed -i '/^stat_nskip=/a stat_visu_nfre= 10' $P/input_chapsim.ini
( cd $P && mpirun -np "$NP_DNS" "$BIN" > run.log 2>&1 ) || bad "MHD periodic DNS"

for np in $NP_DNS $NPS; do
  [[ -d $WORK/mhd_thermal_np$np ]] && continue
  post_np $M "$np" 40 60
  post_np $P "$np" 40 60
done
for np in $NPS; do
  [[ $np == "$NP_DNS" ]] && continue
  check "thermal MHD bundles NP=$np vs NP=$NP_DNS (flow, thermo, mhd)" \
    python3 $CMP $WORK/mhd_thermal_np$NP_DNS/1_data $WORK/mhd_thermal_np$np/1_data 60 --rtol $RTOL \
    --groups flow_stats,thermo_stats,mhd_stats
  check "MHD periodic bundles NP=$np vs NP=$NP_DNS (flow, mhd)" \
    python3 $CMP $WORK/mhd_periodic_np$NP_DNS/1_data $WORK/mhd_periodic_np$np/1_data 60 --rtol $RTOL \
    --groups flow_stats,mhd_stats
  for g in flow mhd; do
    check "MHD periodic $g y-profile NP=$np vs NP=$NP_DNS" \
      python3 $CMPP $WORK/mhd_periodic_np$NP_DNS/2_visu/data/domain1_tsp_avg_${g}_yprofile_60.dat \
      $WORK/mhd_periodic_np$np/2_visu/data/domain1_tsp_avg_${g}_yprofile_60.dat --rtol $RTOL \
      --bundle-dir $WORK/mhd_periodic_np$NP_DNS/1_data --iteration 60 --group ${g}_stats
  done
done

echo "== stat_window decomposition: $PASS passed, $FAIL failed"
if [[ ${KEEP:-0} != 1 && $FAIL -eq 0 ]]; then rm -rf "$WORK"; else echo "work dir kept: $WORK"; fi
[[ $FAIL -eq 0 ]]
