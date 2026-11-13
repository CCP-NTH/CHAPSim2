#!/usr/bin/env bash
# Reader checks added in the 2026-11 backlog pass. Each mutated input must stop with the named
# message (non-zero exit, no "finished successfully", no hang); each valid input must finish
# and, where stated, reproduce the uninterrupted run bit for bit.
#
#   tests/restart_metadata_checks/run_test.sh [EXE] [WORKDIR]
#
# stats     tests/regression/channel_iso_periodic, statistics from step 1, checkpoints at 2, 4:
#   valid   continuation 3..4 from 2: flow_restart_4 and flow_stats_4 bundles equal the full run
#   reject  flow_stats_2.bin truncated (8 bytes) / padded (8 bytes): statistics size check
# geometry  same continuation with the input changed, bundle unchanged:
#   reject  lxx 8.0 -> 8.5 (domain-length), rstret 0.10 -> 0.12 (mesh-stretching)
# time      continuation without checkpoint_meta_2.dat: time taken from the bundle manifest
#   valid   finishes with the note; checkpoint_meta_4 and flow_restart_4 equal the full run
# database  tests/functional/inlet_database/{same,diff}_yz_mesh (tracked bundled database),
#           steps 1..4: valid as shipped; reject with every database .bin truncated (8 bytes)
# mhd       tests/functional/MHD_channel_iso_peridic, statistics from step 1, checkpoint 2:
#   valid   checkpoint_meta_2 lists mhd_stats; continuation 3..4 reads it and finishes
#   reject  mhd_stats_2.bin truncated: statistics size check
# Environment: NP (default 4), TMAX (default 300 s). Exit 0 if every check passes.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXE=${1:-$ROOT/bin/CHAPSim}
WORK=${2:-$(mktemp -d /tmp/chapsim_meta_checks.XXXXXX)}
NP=${NP:-4}; TMAX=${TMAX:-300}
mkdir -p "$WORK"; WORK=$(cd "$WORK" && pwd)
NPASS=0; NFAIL=0
v() { if [ "$1" = 0 ]; then NPASS=$((NPASS + 1)); echo "PASS  $2"; else NFAIL=$((NFAIL + 1)); echo "FAIL  $2"; fi; }

mk() {  # dir template first last ckpt from [extra sed expressions...]
  local d=$1 tmpl=$2 first=$3 last=$4 ckpt=$5 from=$6; shift 6
  rm -rf "$d"; mkdir -p "$d/1_data"
  for f in $(cd "$tmpl" && git -C "$ROOT" ls-files -- "$tmpl/1_data" | sed "s#^.*/1_data/##"); do
    cp "$tmpl/1_data/$f" "$d/1_data/"; done
  cp "$tmpl"/*.DAT "$d/" 2>/dev/null || true
  local s=( -e "s/^\( *niterflowfirst *= *\).*/\1$first/" -e "s/^\( *niterflowlast *= *\).*/\1$last/"
            -e "s/^\( *niterthermofirst *= *\).*/\1$first/" -e "s/^\( *niterthermolast *= *\).*/\1$last/"
            -e "s/^\( *ckpt_nfre *= *\).*/\1$ckpt/" -e "s/^\( *visu_nfre *= *\).*/\1100000/"
            -e "s/^\( *stat_visu_nfre *= *\).*/\1100000/" -e "s/^\( *stat_istart *= *\).*/\10/" )
  [ "$from" -gt 0 ] && s+=( -e "s/^\( *initfl *= *\).*/\1restart/" -e "s/^\( *inittm *= *\).*/\1restart/"
                            -e "s/^\( *irestartfrom *= *\).*/\1$from/" )
  sed "${s[@]}" "$@" "$tmpl/input_chapsim.ini" > "$d/input_chapsim.ini"
}
run() {  # dir
  ( cd "$1" && timeout "$TMAX" mpirun -np "$NP" "$EXE" > run.log 2>&1 < /dev/null ); echo $? > "$1/exit_code"
  grep -q 'finished successfully' "$1/run.log"; }
seed() { cp "$1"/1_data/domain1_*_"$3".* "$2/1_data/"; }   # src dst iter
reject() {  # dir message label
  run "$1"; local ok=$?
  if [ $ok -ne 0 ] && [ "$(cat "$1/exit_code")" != 0 ] && [ "$(cat "$1/exit_code")" != 124 ] && grep -q "$2" "$1/run.log"; then
    v 0 "reject: $3"; else v 1 "reject: $3 (exit $(cat "$1/exit_code"); expected '$2')"; fi; }
same() { cmp -s "$1" "$2"; v $? "$3"; }
trunc() { local f=$1 s; s=$(stat -c %s "$f"); truncate -s $((s - 8)) "$f"; }
pad()   { head -c 8 /dev/zero >> "$1"; }

########## stats, geometry, time (periodic isothermal channel)
P=$ROOT/tests/regression/channel_iso_periodic; S=$WORK/periodic
NODB='s/^\( *is_record_xoutlet_read_xinlet *= *\).*/\1.false.,.false./'
mk "$S/full" "$P" 1 4 2 0 -e "$NODB"; run "$S/full"; v $? "valid: periodic 1..4 with statistics finishes"
mk "$S/cont" "$P" 3 4 2 2 -e "$NODB"; seed "$S/full" "$S/cont" 2; run "$S/cont"; v $? "valid: periodic continuation 3..4 finishes"
same "$S/full/1_data/domain1_flow_restart_4.bin" "$S/cont/1_data/domain1_flow_restart_4.bin" "valid: continuation flow_restart_4 = full run"
same "$S/full/1_data/domain1_flow_stats_4.bin"   "$S/cont/1_data/domain1_flow_stats_4.bin"   "valid: continuation flow_stats_4 = full run"

mk "$S/stats_short" "$P" 3 4 2 2 -e "$NODB"; seed "$S/full" "$S/stats_short" 2; trunc "$S/stats_short/1_data/domain1_flow_stats_2.bin"
reject "$S/stats_short" "Statistics bundle size mismatch (file too short)" "flow statistics bundle truncated"
mk "$S/stats_long" "$P" 3 4 2 2 -e "$NODB"; seed "$S/full" "$S/stats_long" 2; pad "$S/stats_long/1_data/domain1_flow_stats_2.bin"
reject "$S/stats_long" "Statistics bundle size mismatch (file too long)" "flow statistics bundle padded"

mk "$S/geo_len" "$P" 3 4 2 2 -e "$NODB" -e "s/^\( *lxx *= *\).*/\18.5/"; seed "$S/full" "$S/geo_len" 2
reject "$S/geo_len" "Bundle domain-length mismatch" "restart onto another domain length (lxx)"
mk "$S/geo_str" "$P" 3 4 2 2 -e "$NODB" -e "s/^\( *rstret *= *\)\(.*\),0\.10/\1\2,0.12/"; seed "$S/full" "$S/geo_str" 2
grep -q "^rstret= .*,0.12" "$S/geo_str/input_chapsim.ini" || echo "WARNING: rstret edit did not apply"
reject "$S/geo_str" "Bundle mesh-stretching mismatch" "restart onto another stretching (rstret)"

mk "$S/no_ckpt_meta" "$P" 3 4 2 2 -e "$NODB"; seed "$S/full" "$S/no_ckpt_meta" 2; rm -f "$S/no_ckpt_meta/1_data/domain1_checkpoint_meta_2.dat"
run "$S/no_ckpt_meta"; v $? "valid: continuation without checkpoint_meta finishes"
grep -q "taken from the bundle manifest" "$S/no_ckpt_meta/run.log"; v $? "valid: restart time read from the bundle manifest"
same "$S/full/1_data/domain1_checkpoint_meta_4.dat" "$S/no_ckpt_meta/1_data/domain1_checkpoint_meta_4.dat" "valid: checkpoint_meta_4 (time) = full run"
same "$S/full/1_data/domain1_flow_restart_4.bin" "$S/no_ckpt_meta/1_data/domain1_flow_restart_4.bin" "valid: flow_restart_4 = full run"

########## inlet database bundles (same mesh, interpolated mesh)
for c in same_yz_mesh diff_yz_mesh; do
  T=$ROOT/tests/functional/inlet_database/$c; D=$WORK/db_$c
  mk "$D/valid" "$T" 1 4 100 0; run "$D/valid"; v $? "valid: inlet database $c 1..4 finishes"
  mk "$D/short" "$T" 1 4 100 0; for f in "$D"/short/1_data/domain1_xoutlet_database_[0-9]*.bin; do trunc "$f"; done
  reject "$D/short" "Bundle size mismatch (file too short)" "inlet database bundle truncated ($c)"
done

########## MHD statistics manifest and size check
M=$ROOT/tests/functional/MHD_channel_iso_peridic; H=$WORK/mhd
mk "$H/full" "$M" 1 4 2 0 -e "s/^\( *is_record_xoutlet_read_xinlet *= *\).*/\1.false.,.false./"
run "$H/full"; v $? "valid: MHD 1..4 with statistics finishes"
grep -q "^mhd_stats " "$H/full/1_data/domain1_checkpoint_meta_2.dat"; v $? "valid: checkpoint_meta_2 lists mhd_stats"
mk "$H/cont" "$M" 3 4 2 2 -e "s/^\( *is_record_xoutlet_read_xinlet *= *\).*/\1.false.,.false./"; seed "$H/full" "$H/cont" 2
run "$H/cont"; v $? "valid: MHD continuation 3..4 (reads mhd_stats) finishes"
mk "$H/short" "$M" 3 4 2 2 -e "s/^\( *is_record_xoutlet_read_xinlet *= *\).*/\1.false.,.false./"; seed "$H/full" "$H/short" 2
trunc "$H/short/1_data/domain1_mhd_stats_2.bin"
reject "$H/short" "Statistics bundle size mismatch (file too short)" "MHD statistics bundle truncated"

echo "restart_metadata_checks: $NPASS passed, $NFAIL failed (work dir $WORK)"
[ "$NFAIL" -eq 0 ]
