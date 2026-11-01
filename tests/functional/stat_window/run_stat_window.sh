#!/usr/bin/env bash
# Functional tests of the statistical-window post-processing mode
# (is_postprocess = .true., pp_stat_window = a, b).
#
#   C  the window rebuilt from two cumulative snapshots equals the statistics
#      accumulated directly over the same samples (a run with stat_istart = a,
#      whose flow trajectory is bitwise the same: statistics never feed back):
#        - isothermal TGV (flow group), bundled and per-field layouts
#        - thermal TGV (flow incl. Favre, thermo)
#        - thermal MHD inlet-outlet channel (flow incl. Favre, thermo, MHD)
#        - isothermal MHD periodic channel (flow, MHD, y-profiles; profiles also
#          against validation/tools/scripts/rebuild_tavg_from_new_start.py)
#      and a = stat_istart returns the stored cumulative statistics bitwise.
#   D  a window spanning a normal restart is rebuilt correctly; histories that
#      cannot be combined are rejected: a restart that started a fresh window,
#      a fresh thermal field at a restart (its counter restarts at 0 under the
#      same stat_istart), and synthetically damaged snapshots (no sample count,
#      wrong count, truncated bundle, changed signature, changed mesh,
#      inconsistent intermediate snapshot, missing snapshot).
#   J  flow/thermal joint interval: consistent windows recorded as such; a
#      window reaching before the segment the input describes recorded as not
#      established; a late energy-equation start labelled by the run
#      iteration (label_clock run) recorded as consistent, and the same files
#      without that key (as written by builds before 2026-10-30, whose thermo
#      labels trailed the flow labels) rejected; flow/MHD recorded as
#      consistent (shared counter)
#   E  the post-processing run advances zero time steps and leaves every file
#      of the case byte-identical (sha256) apart from the new window
#      directory (5_postprocess/window_<a>_<b>); an existing window directory
#      and invalid keys (incl. the retired pp_output_dir) are rejected
#      without a DNS step. Negative control: the same harness applied to a
#      run with is_postprocess = .false. must report a failure.
#
# Tolerance for C/D, array-relative per field: 1e-10. Direct and rebuilt
# averages differ only by rounding, bounded by ~kappa N eps max|A| / max|A_ab|
# (kappa = (N_a+N_b)/(N_b-N_a) <= 3, N <= 40 here), i.e. O(1e-13); the
# restart-spanning comparison inherits the restart's round-off reproduction of
# the flow, for which tests/functional/restart uses the same 1e-10.
#
# Usage: tests/functional/stat_window/run_stat_window.sh
#        NP=4 (default)  WORK_DIR=<scratch> (default mktemp)  KEEP=1 keeps it
set -u
R=$(cd "$(dirname "$0")/../../.." && pwd)
BIN=$R/bin/CHAPSim
NP=${NP:-4}
WORK=${WORK_DIR:-$(mktemp -d /tmp/chapsim_statwin.XXXXXX)}
CMP=$R/tests/tools/compare_stats_restart.py
CMPP=$R/tests/tools/compare_stat_window_profiles.py
REBUILD=$R/validation/tools/scripts/rebuild_tavg_from_new_start.py
RTOL=1e-10
PASS=0
FAIL=0
mkdir -p "$WORK"
echo "statistical-window functional tests, work dir $WORK, NP=$NP"
[[ -x $BIN ]] || { echo "missing $BIN - build first"; exit 2; }

ok()   { echo "  PASS $*"; PASS=$((PASS + 1)); }
bad()  { echo "  FAIL $*"; FAIL=$((FAIL + 1)); }
done_section() {  # free a section's scratch once it passed (disk); kept on failure or KEEP=1
  [[ ${KEEP:-0} == 1 || $FAIL -gt 0 ]] || rm -rf "$1"
}
check(){ local name=$1; shift; if "$@" > "$WORK/last_check.out" 2>&1; then ok "$name"; \
         else bad "$name"; sed 's/^/      /' "$WORK/last_check.out"; fi; }

set_key() {   # file key value  (key= value lines; first occurrence only)
  sed -i "0,/^$2=.*/s||$2= $3|" "$1"
}
run_dns() {   # dir: run the case as it is
  ( cd "$1" && mpirun -np "$NP" "$BIN" > run.log 2>&1 )
}
make_post_input() {   # dir a b [extra key=value lines...]
  local d=$1 a=$2 b=$3; shift 3
  cp "$d/input_chapsim.ini" "$d/input_dns.ini.bak" 2>/dev/null || true
  set_key "$d/input_chapsim.ini" is_postprocess .true.
  sed -i '/^pp_stat_window=/d' "$d/input_chapsim.ini"
  sed -i "/^is_postprocess=/a pp_stat_window= $a, $b" "$d/input_chapsim.ini"
  local line
  for line in "$@"; do sed -i "/^pp_stat_window=/a $line" "$d/input_chapsim.ini"; done
}
run_post() {  # dir a b [extra lines] -> exit status; log in dir/post_<a>_<b>.log
  local d=$1 a=$2 b=$3; shift 3
  make_post_input "$d" "$a" "$b" "$@"
  ( cd "$d" && mpirun -np "$NP" "$BIN" > "post_${a}_${b}.log" 2>&1 )
  local st=$?
  cp "$d/input_dns.ini.bak" "$d/input_chapsim.ini"
  return $st
}
case_sha() {  # dir -> sha256 of every file except post-processing output, logs and the input
  ( cd "$1" && find . -type f -not -path './5_postprocess/*' -not -name 'post_*.log' \
      -not -name 'input_chapsim.ini' -not -name 'input_dns.ini.bak' -print0 \
      | sort -z | xargs -0 sha256sum )
}
zero_step_log() {  # log: post-processing completed and no time step taken
  grep -q '0 time steps advanced' "$1" || { echo "no completion line in $1"; return 1; }
  if grep -q -E 'Time Step =|Solving the governing equations|CFL No\.' "$1"; then
    echo "time-stepping output found in $1"; return 1; fi
  return 0
}
expect_reject() {  # name dir a b pattern [extra lines]
  local name=$1 d=$2 a=$3 b=$4 pat=$5; shift 5
  local before after had_window=0
  [[ -e "$d/5_postprocess/window_${a}_${b}" ]] && had_window=1
  before=$(case_sha "$d")
  if run_post "$d" "$a" "$b" "$@"; then bad "$name: run was not rejected"; return; fi
  after=$(case_sha "$d")
  local log="$d/post_${a}_${b}.log"
  if ! grep -q -- "$pat" "$log"; then bad "$name: diagnostic '$pat' not found"; tail -n 5 "$log" | sed 's/^/      /'; return; fi
  if grep -q -E 'Time Step =|Solving the governing equations' "$log"; then bad "$name: time stepping entered"; return; fi
  if [[ "$before" != "$after" ]]; then bad "$name: case files changed"; return; fi
  if [[ $had_window -eq 0 && -e "$d/5_postprocess/window_${a}_${b}" ]]; then
    bad "$name: window directory left behind"; return; fi
  ok "$name (rejected: $pat)"
}
e_harness() {  # name dir a b : Test E on a run that is expected to be post-processing only
  local name=$1 d=$2 a=$3 b=$4
  local before after st
  before=$(case_sha "$d")
  run_post "$d" "$a" "$b"; st=$?
  after=$(case_sha "$d")
  local log="$d/post_${a}_${b}.log"
  [[ $st -eq 0 ]] || { echo "exit status $st"; return 1; }
  zero_step_log "$log" || return 1
  [[ "$before" == "$after" ]] || { echo "case files changed or added outside 5_postprocess:"; \
      diff <(echo "$before") <(echo "$after") | head -n 5; return 1; }
  [[ -f "$d/5_postprocess/window_${a}_${b}/stat_window_provenance.dat" ]] || { echo "no provenance record"; return 1; }
  echo "zero time steps, $(echo "$before" | wc -l) case files unchanged (sha256), exit 0"
}

#==============================================================================
# isothermal TGV: flow group, restart, rejections
#==============================================================================
echo "== TGV (isothermal, flow statistics)"
TGV_IN=$R/tests/functional/restart/tgv_iso_stats/run_continuous/input_chapsim.ini
new_tgv() {   # dir stat_istart niterflowlast
  mkdir -p "$1"; cp "$TGV_IN" "$1/input_chapsim.ini"
  set_key "$1/input_chapsim.ini" niterflowlast "$3"
  set_key "$1/input_chapsim.ini" ckpt_nfre 10
  set_key "$1/input_chapsim.ini" visu_nfre 1000
  set_key "$1/input_chapsim.ini" stat_istart "$2"
}
restart_tgv() {  # dir from niterflowfirst niterflowlast stat_istart
  set_key "$1/input_chapsim.ini" initfl restart
  set_key "$1/input_chapsim.ini" irestartfrom "$2"
  set_key "$1/input_chapsim.ini" niterflowfirst "$3"
  set_key "$1/input_chapsim.ini" niterflowlast "$4"
  set_key "$1/input_chapsim.ini" stat_istart "$5"
}
T=$WORK/tgv
new_tgv $T/cont 20 60;  run_dns $T/cont  || bad "TGV continuous run"
new_tgv $T/ref40 40 60; run_dns $T/ref40 || bad "TGV reference stat_istart=40"
new_tgv $T/ref30 30 60; run_dns $T/ref30 || bad "TGV reference stat_istart=30"
check "C  TGV flow trajectory independent of stat_istart (bitwise oracle)" \
  cmp $T/cont/1_data/domain1_flow_restart_60.bin $T/ref40/1_data/domain1_flow_restart_60.bin

# E on the first post-processing run, then C on its output
check "E  TGV post-processing (40,60]: zero steps, originals unchanged" e_harness tgv $T/cont 40 60
check "C  TGV window (40,60] = direct accumulation from stat_istart 40" \
  python3 $CMP $T/ref40/1_data $T/cont/5_postprocess/window_40_60/1_data 60 --rtol $RTOL
run_post $T/cont 20 60 || bad "TGV window (20,60]"
check "C  TGV a = stat_istart returns the cumulative statistics bitwise" \
  cmp $T/cont/1_data/domain1_flow_stats_60.bin $T/cont/5_postprocess/window_20_60/1_data/domain1_flow_stats_60.bin
run_post $T/cont 30 60 || bad "TGV window (30,60]"

# restart at 40 with the accumulation continued; window (30,60] spans it
new_tgv $T/rst 20 40; run_dns $T/rst || bad "TGV restart stage 1"
cp -r $T/rst $T/rst_stage1
restart_tgv $T/rst 40 41 60 20; run_dns $T/rst || bad "TGV restart stage 2"
run_post $T/rst 30 60 || bad "TGV window (30,60] across the restart"
check "D  TGV window across a restart = direct accumulation from stat_istart 30" \
  python3 $CMP $T/ref30/1_data $T/rst/5_postprocess/window_30_60/1_data 60 --rtol $RTOL
check "D  TGV window across a restart = same window of the uninterrupted run" \
  python3 $CMP $T/cont/5_postprocess/window_30_60/1_data $T/rst/5_postprocess/window_30_60/1_data 60 --rtol $RTOL

# a restart that started a fresh window (stat_istart raised to the restart)
cp -r $T/rst_stage1 $T/reset
restart_tgv $T/reset 40 41 60 40; run_dns $T/reset || bad "TGV reset restart"
set_key $T/reset/input_chapsim.ini stat_istart 20
expect_reject "D  TGV fresh statistics window at a restart" $T/reset 30 60 "accumulated from stat_istart = 40"

# synthetically damaged snapshots, each on a fresh copy of the continuous run
damaged() {   # name window_a window_b pattern edit-command...
  local name=$1 a=$2 b=$3 pat=$4; shift 4
  rm -rf $T/dmg; mkdir -p $T/dmg
  cp $T/cont/input_chapsim.ini $T/dmg/; cp -r $T/cont/1_data $T/dmg/
  ( cd $T/dmg/1_data && eval "$@" )
  expect_reject "D  $name" $T/dmg $a $b "$pat"
}
damaged "snapshot without a sample count (pre-bb0c2f48 file)" 40 60 "carries no explicit sample count" \
  "sed -i '/^nsamples/d;/^stat_istart/d' domain1_flow_stats_meta_60.dat"
damaged "sample count inconsistent with the window" 40 60 "Continuous sampling is required" \
  "sed -i 's/^nsamples 40/nsamples 39/' domain1_flow_stats_meta_60.dat"
damaged "truncated statistics bundle" 40 60 "truncated or belongs to a different mesh" \
  "truncate -s -8 domain1_flow_stats_60.bin"
damaged "changed statistics signature (stat_level)" 40 60 "signature mismatch" \
  "sed -i 's/stat_level=3/stat_level=2/' domain1_flow_stats_meta_40.dat"
damaged "mesh recorded in the checkpoint differs" 40 60 "differs from the input mesh" \
  "sed -i 's/^nc .*/nc 32 32 64/' domain1_flow_restart_meta_60.dat"
damaged "inconsistent intermediate snapshot" 40 60 "breaks the continuous" \
  "sed -i 's/^nsamples 30/nsamples 29/' domain1_flow_stats_meta_50.dat"
damaged "missing snapshot at b" 40 55 "is missing" "true"

# E: existing window directory, invalid keys
wsha() { ( cd "$1" && find . -type f -print0 | sort -z | xargs -0 sha256sum ); }
WSHA=$(wsha $T/cont/5_postprocess/window_40_60)
expect_reject "E  existing window directory is not overwritten" $T/cont 40 60 "already exists"
check "E  existing window results unchanged after the rejected run" \
  test "$WSHA" == "$(wsha $T/cont/5_postprocess/window_40_60)"
expect_reject "E  pp_stat_window with b <= a" $T/cont 60 40 "Expected 0 <= a < b"
expect_reject "E  pp_output_dir is not an input (fixed 5_postprocess)" $T/cont 40 60 "pp_output_dir is not an" \
  "pp_output_dir= elsewhere"
expect_reject "E  a before stat_istart" $T/cont 10 60 "lies before stat_istart"
# missing pp_stat_window: hand-made input
cp $T/cont/input_chapsim.ini $T/cont/input_dns.ini.bak
set_key $T/cont/input_chapsim.ini is_postprocess .true.
before=$(case_sha $T/cont)
( cd $T/cont && mpirun -np "$NP" "$BIN" > post_missing.log 2>&1 ); st=$?
cp $T/cont/input_dns.ini.bak $T/cont/input_chapsim.ini
if [[ $st -ne 0 ]] && grep -q "requires pp_stat_window" $T/cont/post_missing.log \
   && ! grep -q "Time Step =" $T/cont/post_missing.log && [[ "$before" == "$(case_sha $T/cont)" ]]; then
  ok "E  missing pp_stat_window stops before any DNS step"
else bad "E  missing pp_stat_window"; fi

# negative control: the E harness must catch a run that time-steps
cp -r $T/rst_stage1 $T/negctl
set_key $T/negctl/input_chapsim.ini niterflowlast 41
make_post_input $T/negctl 20 40
set_key $T/negctl/input_chapsim.ini is_postprocess .false.     # keys present, mode off
before=$(case_sha $T/negctl)
( cd $T/negctl && mpirun -np "$NP" "$BIN" > post_20_40.log 2>&1 )
if zero_step_log $T/negctl/post_20_40.log > /dev/null 2>&1 || [[ "$before" == "$(case_sha $T/negctl)" ]]; then
  bad "E  negative control: harness did not detect a time-stepping run"
else ok "E  negative control: a run with is_postprocess = .false. is detected (steps taken, files changed)"; fi

# per-field restart layout: one file per component, 3-D t_avg visualisation
new_tgv $T/pf_cont 20 60; set_key $T/pf_cont/input_chapsim.ini restart_data_layout per_field
new_tgv $T/pf_ref40 40 60; set_key $T/pf_ref40/input_chapsim.ini restart_data_layout per_field
run_dns $T/pf_cont || bad "TGV per-field continuous run"
run_dns $T/pf_ref40 || bad "TGV per-field reference"
check "E  TGV per-field post-processing (40,60]: zero steps, originals unchanged" e_harness pf $T/pf_cont 40 60
pf_compare() {   # per-field t_avg files of the window against the direct run
  python3 - "$T/pf_ref40/1_data" "$T/pf_cont/5_postprocess/window_40_60/1_data" "$RTOL" <<'EOF'
import glob, os, sys
import numpy as np
ref, out, rtol = sys.argv[1], sys.argv[2], float(sys.argv[3])
files = sorted(glob.glob(os.path.join(ref, "domain1_t_avg_*_60.bin")))
worst, name = 0.0, "none"
for f in files:
    a = np.fromfile(f)
    b = np.fromfile(os.path.join(out, os.path.basename(f)))
    e = float(np.max(np.abs(a - b)) / max(np.max(np.abs(a)), 1e-30))
    if e > worst:
        worst, name = e, os.path.basename(f)
print("  %d per-field files, worst %s rel %.3e (tol %.1e)" % (len(files), name, worst, rtol))
sys.exit(0 if files and worst <= rtol else 1)
EOF
}
check "C  TGV per-field window (40,60] = direct accumulation" pf_compare
xdmf_refs() (    # every binary a window xdmf file refers to exists inside the window (subshell: cd)
  local x r n=0
  cd "$T/pf_cont/5_postprocess/window_40_60/2_visu/xdmf" || return 1
  for x in *.xdmf; do
    for r in $(grep -o '[.][^<> ]*\.bin' "$x" | sort -u); do
      [[ -f "$r" ]] || { echo "missing $r in $x"; return 1; }; n=$((n + 1))
    done
  done
  echo "$n references resolved"
)
check "C  TGV per-field 3-D xdmf references resolve inside the window" xdmf_refs

done_section $T

#==============================================================================
# thermal TGV: flow (incl. Favre) and thermo groups; a fresh thermal field at a
# restart (thermo accumulators restart empty under the same stat_istart)
#==============================================================================
echo "== thermal TGV (flow, Favre and thermo statistics)"
TS_SRC=$R/tests/functional/restart/tgv_scp/run_continuous
new_ts() {   # dir stat_istart niterlast
  mkdir -p "$1"; cp $TS_SRC/input_chapsim.ini $TS_SRC/*.DAT "$1/"
  set_key "$1/input_chapsim.ini" niterflowlast "$3"
  set_key "$1/input_chapsim.ini" niterthermolast "$3"
  set_key "$1/input_chapsim.ini" ckpt_nfre 10
  set_key "$1/input_chapsim.ini" visu_nfre 1000
  set_key "$1/input_chapsim.ini" stat_istart "$2"
}
S=$WORK/tgv_thermal
new_ts $S/cont 20 60; run_dns $S/cont || bad "thermal TGV continuous run"
new_ts $S/ref40 40 60; run_dns $S/ref40 || bad "thermal TGV reference stat_istart=40"
check "E  thermal TGV post-processing (40,60]: zero steps, originals unchanged" e_harness ts $S/cont 40 60
check "C  thermal TGV window (40,60] = direct accumulation (flow incl. Favre, thermo)" \
  python3 $CMP $S/ref40/1_data $S/cont/5_postprocess/window_40_60/1_data 60 --rtol $RTOL --groups flow_stats,thermo_stats
check "J  thermal TGV joint flow/thermo interval recorded as consistent" \
  grep -q "joint_interval flow_stats+thermo_stats consistent" $S/cont/5_postprocess/window_40_60/stat_window_provenance.dat
# stage 1 to 40, then two restarts from it
new_ts $S/st1 20 40; run_dns $S/st1 || bad "thermal TGV stage 1"
# (a) flow restarted at 40, thermal field initialised afresh: the thermo
# counter restarts at 0 under the unchanged stat_istart - a reset no key records
cp -r $S/st1 $S/fresh
set_key $S/fresh/input_chapsim.ini initfl restart
set_key $S/fresh/input_chapsim.ini irestartfrom 40          # first occurrence: the flow
set_key $S/fresh/input_chapsim.ini niterflowfirst 41
set_key $S/fresh/input_chapsim.ini niterflowlast 60
set_key $S/fresh/input_chapsim.ini niterthermofirst 41
set_key $S/fresh/input_chapsim.ini niterthermolast 60
run_dns $S/fresh || bad "thermal TGV restart with a fresh thermal field"
expect_reject "D  thermal TGV fresh thermal field at a restart (thermo counter reset)" $S/fresh 40 60 \
  "thermo_stats counts give"
# (b) both fields restarted at 40: a window reaching before the restart is
# rebuilt; both thermal ends carry label_clock run (run-iteration labels in
# every segment), so the joint interval is consistent. An end written by an
# older build (no label_clock) cannot be aligned: recorded as not established
cp -r $S/st1 $S/both
set_key $S/both/input_chapsim.ini initfl restart
set_key $S/both/input_chapsim.ini inittm restart
sed -i 's/^irestartfrom=.*/irestartfrom= 40/' $S/both/input_chapsim.ini
set_key $S/both/input_chapsim.ini niterflowfirst 41
set_key $S/both/input_chapsim.ini niterflowlast 60
set_key $S/both/input_chapsim.ini niterthermofirst 41
set_key $S/both/input_chapsim.ini niterthermolast 60
run_dns $S/both || bad "thermal TGV restart of both fields"
check "J  thermal TGV window across a restart: rebuilt, zero steps, originals unchanged" e_harness both $S/both 30 60
check "J  thermal TGV window across a restart, both ends label_clock run: recorded as consistent" \
  grep -q "joint_interval flow_stats+thermo_stats consistent: both thermal snapshots" \
  $S/both/5_postprocess/window_30_60/stat_window_provenance.dat
cp -r $S/both $S/both_old; rm -rf $S/both_old/5_postprocess
sed -i '/^label_clock /d' $S/both_old/1_data/domain1_thermo_stats_meta_30.dat
run_post $S/both_old 30 60 || bad "thermal TGV window (30,60] with an older-build end"
check "J  thermal TGV window end from an older build (no label_clock) recorded as NOT ESTABLISHED" \
  grep -q "joint_interval flow_stats+thermo_stats NOT ESTABLISHED: thermo_stats 30 predates label_clock run" \
  $S/both_old/5_postprocess/window_30_60/stat_window_provenance.dat
run_post $S/both 40 60 || bad "thermal TGV window (40,60] after the restart"
check "J  thermal TGV window inside the input's segment recorded as consistent" \
  grep -q "joint_interval flow_stats+thermo_stats consistent" $S/both/5_postprocess/window_40_60/stat_window_provenance.dat
# (c) energy equation started 10 steps after the flow: the thermal clock runs
# with the flow, so thermo snapshots carry the run iteration (label_clock run)
new_ts $S/lag 20 60; set_key $S/lag/input_chapsim.ini niterthermofirst 11
run_dns $S/lag || bad "thermal TGV with a late energy equation"
check "J  thermal TGV late start: thermo labels follow the run iteration (files at 50 and 60, label_clock run)" \
  grep -q "^label_clock run" $S/lag/1_data/domain1_thermo_stats_meta_60.dat
run_post $S/lag 30 50 || bad "thermal TGV late start: window (30,50]"
check "J  thermal TGV late start: joint interval recorded as consistent (label_clock run)" \
  grep -q "joint_interval flow_stats+thermo_stats consistent: both thermal snapshots" \
  $S/lag/5_postprocess/window_30_50/stat_window_provenance.dat
# (d) the same files without label_clock, as builds before 2026-10-30 wrote them
# (thermo labels trailing the flow labels by 10): the input-based derivation
# identifies the mismatch - rejected before output
cp -r $S/lag $S/lag_old; rm -rf $S/lag_old/5_postprocess
sed -i '/^label_clock /d' $S/lag_old/1_data/domain1_thermo_stats_meta_*.dat
expect_reject "J  thermal TGV late energy equation, older-build labels: flow/thermo intervals differ" \
  $S/lag_old 30 50 "cover different physical intervals"

done_section $S

#==============================================================================
# thermal MHD inlet-outlet channel: flow (incl. Favre), thermo and MHD groups
#==============================================================================
echo "== thermal MHD inlet-outlet channel (flow, Favre, thermo, MHD statistics)"
MT_SRC=$R/tests/functional/MHD_channel_scp_inout_Tw
new_mt() {   # dir stat_istart
  mkdir -p "$1/1_data"
  cp $MT_SRC/input_chapsim.ini $MT_SRC/*.DAT "$1/"
  cp $MT_SRC/1_data/* "$1/1_data/"
  set_key "$1/input_chapsim.ini" ckpt_nfre 10
  set_key "$1/input_chapsim.ini" visu_nfre 1000
  set_key "$1/input_chapsim.ini" stat_istart "$2"
  # start the energy equation with the flow, so that flow and thermo snapshots
  # of one label belong to one solver step (see the thermal TGV section)
  set_key "$1/input_chapsim.ini" niterthermofirst 1
}
M=$WORK/mhd_thermal
new_mt $M/cont 30; run_dns $M/cont || bad "thermal MHD continuous run"
new_mt $M/ref40 40; run_dns $M/ref40 || bad "thermal MHD reference stat_istart=40"
check "E  thermal MHD post-processing (40,60]: zero steps, originals unchanged" e_harness mt $M/cont 40 60
check "C  thermal MHD window (40,60] = direct accumulation (flow, thermo, mhd)" \
  python3 $CMP $M/ref40/1_data $M/cont/5_postprocess/window_40_60/1_data 60 --rtol $RTOL \
  --groups flow_stats,thermo_stats,mhd_stats

done_section $M

#==============================================================================
# isothermal MHD periodic channel: flow and MHD bundles, y-profiles
#==============================================================================
echo "== isothermal MHD periodic channel (bundles and y-profiles)"
MP_SRC=$R/tests/functional/MHD_channel_iso_periodic_Btilt
new_mp() {   # dir stat_istart
  mkdir -p "$1"; cp $MP_SRC/input_chapsim.ini "$1/"
  set_key "$1/input_chapsim.ini" ckpt_nfre 10
  set_key "$1/input_chapsim.ini" visu_nfre 1000
  set_key "$1/input_chapsim.ini" stat_istart "$2"
  sed -i '/^stat_nskip=/a stat_visu_nfre= 10' "$1/input_chapsim.ini"
}
P=$WORK/mhd_periodic
new_mp $P/cont 30; run_dns $P/cont || bad "MHD periodic continuous run"
new_mp $P/ref40 40; run_dns $P/ref40 || bad "MHD periodic reference stat_istart=40"
check "E  MHD periodic post-processing (40,60]: zero steps, originals unchanged" e_harness mp $P/cont 40 60
check "C  MHD periodic window (40,60] = direct accumulation (flow, mhd)" \
  python3 $CMP $P/ref40/1_data $P/cont/5_postprocess/window_40_60/1_data 60 --rtol $RTOL --groups flow_stats,mhd_stats
check "J  MHD periodic flow/MHD interval recorded as consistent (shared counter), no thermo entry" \
  bash -c "grep -q 'joint_interval flow_stats+mhd_stats consistent' $P/cont/5_postprocess/window_40_60/stat_window_provenance.dat && ! grep -q 'flow_stats+thermo_stats' $P/cont/5_postprocess/window_40_60/stat_window_provenance.dat"
python3 -I $REBUILD --input-dir $P/cont/2_visu/data --output-dir $P/rebuild_py --old-start 30 --new-start 40 \
  --target-iter 60 > $P/rebuild_py.log 2>&1 || bad "rebuild_tavg_from_new_start.py"
for prof in $(cd $P/ref40/2_visu/data && ls domain1_tsp_avg_*_60.dat); do
  grp=flow_stats; [[ $prof == *_mhd_* ]] && grp=mhd_stats
  check "C  profile $prof = direct accumulation" \
    python3 $CMPP $P/ref40/2_visu/data/$prof $P/cont/5_postprocess/window_40_60/2_visu/data/$prof --rtol $RTOL \
    --bundle-dir $P/ref40/1_data --iteration 60 --group $grp
  check "C  profile $prof = rebuild_tavg_from_new_start.py" \
    python3 $CMPP $P/rebuild_py/$prof $P/cont/5_postprocess/window_40_60/2_visu/data/$prof --rtol $RTOL \
    --bundle-dir $P/ref40/1_data --iteration 60 --group $grp
done

done_section $P
echo "== stat_window: $PASS passed, $FAIL failed"
if [[ ${KEEP:-0} != 1 && $FAIL -eq 0 ]]; then rm -rf "$WORK"; else echo "work dir kept: $WORK"; fi
[[ $FAIL -eq 0 ]]
