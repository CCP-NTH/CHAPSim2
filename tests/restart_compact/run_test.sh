#!/usr/bin/env bash
# Native compact flow-restart fixture and the reader contract it must satisfy.
#
#   tests/restart_compact/run_test.sh EXE WORKDIR [EXE2]
#
# EXE writes and reads every bundle (the CPU solver; restart history modes are described in
# docs/guidance/docs/restart-io.md). WORKDIR must not exist. EXE2 (optional, a second CPU
# build) repeats the generation and the heated MHD+LES starts, to show that both builds write and
# read the same bytes. Environment: NP4 (default 4) ranks for the 2x2 cases; LEAN=1 records the
# sha256 of every run's restart files (WORKDIR/run_files.sha256) and then removes them, keeping the
# fixture, the logs and the inputs.
#
# Case: tests/restart_compact/input_iso.ini, the small periodic isothermal channel (32x48x32,
# two-sided stretched y mesh, CD2, RK3, dt 1e-3, Re 5000, constant mass flux). Steps:
#  gen     e30   1..30 exact   from a Poiseuille field -> the exact reference bundle at 30
#          c30   1..30 compact from the same field      -> the native compact bundle at 30
#          c30x4 as c30 on 2x2 ranks                    -> decomposition dependence of the bytes
#          (restart_history_mode applies to reading too: a compact run cannot start from an
#          exact bundle, see reject exact_in_compact)
#  fixture WORKDIR/fixture/{exact,compact}: only domain1_flow_restart_30.bin, its manifest and
#          domain1_checkpoint_meta_30.dat; every other file at step 30 is listed and left out
#  equiv   the restart_bundle_tool.py conversion of the exact bundle equals the native compact bundle
#  cont    isothermal continuation 31..40: compact mode from the compact fixture, exact mode
#          from the exact fixture; q and pr at 40 must be bitwise identical (RK3 does not use
#          the stored momentum RHS at the first substep)
#  heated  heated sodium Tw start (fresh thermal field, coupled flow) 31..40 from each fixture,
#          for DNS, MHD (Ha 10, B along y) and MHD+LES (WALE), 1 rank, and MHD+LES on 2x2;
#          the flow and thermal bundles at 40 must be byte-identical between the two starts
#  reject  malformed or mismatched input must stop the run (or is recorded as accepted)
# Results: WORKDIR/results.txt (PASS/FAIL/OBSERVED lines) and WORKDIR/fixture/SHA256SUMS.
# Exit 0 if every PASS/FAIL check passes; OBSERVED lines record behaviour without a verdict.
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXE=${1:?executable}; W=${2:?workdir}; EXE2=${3:-}
NP4=${NP4:-4}
[ -e "$W" ] && { echo "$W exists"; exit 2; }
mkdir -p "$W"; W=$(cd "$W" && pwd)
TOOL="python3 $HERE/../tools/restart_bundle_tool.py"
OUT="$W/results.txt"; : > "$OUT"; NF=0; NP=0
v() { if [ "$1" = 0 ]; then NP=$((NP + 1)); echo "PASS  $2" | tee -a "$OUT"; else NF=$((NF + 1)); echo "FAIL  $2" | tee -a "$OUT"; fi; }
obs() { echo "OBSERVED  $*" | tee -a "$OUT"; }

iso_input() {  # dir initfl from first last ckpt history prow pcol
  sed -e "s/@INITFL@/$2/" -e "s/@FROM@/$3/" -e "s/@FIRST@/$4/" -e "s/@LAST@/$5/" -e "s/@CKPT@/$6/" \
      -e "s/@HISTORY@/$7/" -e "s/@PROW@/$8/" -e "s/@PCOL@/$9/" "$HERE/input_iso.ini" > "$1/input_chapsim.ini"
}
heated_input() {  # dir variant(dns|mhd|mhdles) prow pcol
  sed -e "s/@FLOWFROM@/30/" -e "s/@FIRST@/31/" -e "s/@LAST@/40/" -e "s/@CKPT@/10/" \
      -e "s/@PROW@/$3/" -e "s/@PCOL@/$4/" "$HERE/input_heated.ini" > "$1/input_chapsim.ini"
  case $2 in mhd|mhdles)
    sed -i "/^\[mesh\]/i [mhd]\nimhd_xdom= .true.\nNStuart= .false., 0.0\nNHartmn= .true., 10\nB_static= 0.0, 1.0, 0.0\n" "$1/input_chapsim.ini";; esac
  [ "$2" = mhdles ] && sed -i "/^\[mesh\]/i [les]\nLESmode= wale\n" "$1/input_chapsim.ini"
  return 0
}
run() {  # dir exe nranks
  ( cd "$1" && mpirun -np "$3" "$2" > run.log 2>&1 < /dev/null ); echo $? > "$1/exit_status"
  rm -rf "$1/2_visu"      # initial visualisation output: not used by any check
  grep -q 'finished successfully' "$1/run.log"
}
seed() {  # dir fixture_subdir: the run reads nothing but the fixture files
  mkdir -p "$1/1_data"; cp "$W/fixture/$2"/* "$1/1_data/"; rm -f "$1/1_data/SHA256SUMS"
}

gen() {  # exe tag: the history mode governs reading as well as writing, so a compact bundle can only
          # come from a run that is compact from its first step; every run starts from the same
          # Poiseuille field and the write mode does not change the trajectory
  local E=$1 T=$2 d
  for c in e30:exact:1:0:0 c30:compact:1:0:0 c30x4:compact:$NP4:2:2; do
    IFS=: read -r lab hist n pr pc <<< "$c"
    [ "$lab" = c30x4 ] && [ "$T" != gen ] && continue
    d=$W/$T/$lab; mkdir -p "$d"
    iso_input "$d" poiseuille 0 1 30 30 "$hist" "$pr" "$pc"
    run "$d" "$E" "$n"; v $? "[$T] $lab 1..30 from Poiseuille ($hist, $n rank(s)) finished"
  done
}

########## gen
gen "$EXE" gen
########## fixture
mkdir -p "$W/fixture/exact" "$W/fixture/compact"
for k in exact:e30 compact:c30; do
  IFS=: read -r sub lab <<< "$k"
  for f in domain1_flow_restart_30.bin domain1_flow_restart_meta_30.dat domain1_checkpoint_meta_30.dat; do
    cp "$W/gen/$lab/1_data/$f" "$W/fixture/$sub/"
  done
  ls "$W/gen/$lab/1_data/" | grep '_30\.' | grep -v -x -e domain1_flow_restart_30.bin -e domain1_flow_restart_meta_30.dat \
     -e domain1_checkpoint_meta_30.dat > "$W/fixture/excluded_$sub.txt"
  obs "fixture/$sub: other step-30 files written by the run and left out: $( [ -s "$W/fixture/excluded_$sub.txt" ] && tr '\n' ' ' < "$W/fixture/excluded_$sub.txt" || echo none)"
  ( cd "$W/fixture/$sub" && sha256sum domain1_* > SHA256SUMS )
done
cat "$W/fixture/exact/SHA256SUMS" "$W/fixture/compact/SHA256SUMS" | sed 's/^/    /' | tee -a "$OUT"
$TOOL check "$W/fixture/compact/domain1_flow_restart_30.bin" > "$W/check_compact.txt" 2>&1
v $? "native compact bundle meets the compact contract (restart_bundle_tool.py check: fields, key order, shapes, file length)"
$TOOL compare "$W/fixture/exact/domain1_flow_restart_30.bin" "$W/fixture/compact/domain1_flow_restart_30.bin" qx qy qz pr > "$W/cmp_fixture.txt" 2>&1
v $? "compact qx,qy,qz,pr equal the exact bundle's (same trajectory)"
cmp -s "$W/fixture/exact/domain1_checkpoint_meta_30.dat" "$W/fixture/compact/domain1_checkpoint_meta_30.dat"
v $? "checkpoint_meta identical for the exact and compact runs"
diff "$W/fixture/exact/domain1_flow_restart_meta_30.dat" "$W/fixture/compact/domain1_flow_restart_meta_30.dat" > "$W/diff_manifests.txt"
[ "$(grep -c '^[<>]' "$W/diff_manifests.txt")" = 4 ] && grep -q '^< fields' "$W/diff_manifests.txt" && grep -q '^< field_shapes' "$W/diff_manifests.txt"
v $? "exact and compact manifests differ only in the fields and field_shapes lines"
cmp -s "$W/gen/c30/1_data/domain1_flow_restart_30.bin" "$W/gen/c30x4/1_data/domain1_flow_restart_30.bin"
r=$?; [ $r = 0 ] && v 0 "compact bundle written on 2x2 ranks is byte-identical to the 1-rank one" \
               || obs "compact bundle written on 2x2 differs from the 1-rank one ($( $TOOL compare "$W/gen/c30/1_data/domain1_flow_restart_30.bin" "$W/gen/c30x4/1_data/domain1_flow_restart_30.bin" | tail -n 1)); the trajectories are not bitwise across decompositions"
cmp -s "$W/gen/c30/1_data/domain1_flow_restart_meta_30.dat" "$W/gen/c30x4/1_data/domain1_flow_restart_meta_30.dat"
v $? "compact manifest independent of the decomposition"

########## equiv
mkdir -p "$W/converted"
$TOOL convert "$W/fixture/exact/domain1_flow_restart_30.bin" "$W/converted" > "$W/convert.txt" 2>&1
cmp -s "$W/converted/domain1_flow_restart_30.bin" "$W/fixture/compact/domain1_flow_restart_30.bin"
v $? "converted exact->compact .bin byte-identical to the native compact .bin"
cmp -s "$W/converted/domain1_flow_restart_meta_30.dat" "$W/fixture/compact/domain1_flow_restart_meta_30.dat"
v $? "converted manifest byte-identical to the native compact manifest"

########## cont
for c in cc:compact:compact ce:exact:exact; do
  IFS=: read -r lab fx hist <<< "$c"; d=$W/cont/$lab; seed "$d" "$fx"
  iso_input "$d" restart 30 31 40 10 "$hist" 0 0; run "$d" "$EXE" 1; v $? "cont $lab ($hist mode from the $fx fixture) 31..40 finished"
done
grep -q 'restart_history_mode=compact: momentum RHS history was not read' "$W/cont/cc/run.log"; v $? "cont cc took the compact reader (warning printed)"
$TOOL compare "$W/cont/ce/1_data/domain1_flow_restart_40.bin" "$W/cont/cc/1_data/domain1_flow_restart_40.bin" qx qy qz pr > "$W/cmp_cont.txt" 2>&1
v $? "compact and exact continuations bitwise identical at 40 (q, pr) [$(tail -n 1 "$W/cmp_cont.txt")]"
cmp -s "$W/cont/ce/1_data/domain1_checkpoint_meta_40.dat" "$W/cont/cc/1_data/domain1_checkpoint_meta_40.dat"
v $? "continuations report the same time/dt at 40 (checkpoint_meta identical)"

########## heated
heated() {  # exe tag variant n prow pcol
  local E=$1 T=$2 var=$3 n=$4 pr=$5 pc=$6 fx d lab=$3_$4
  for fx in exact compact; do
    d=$W/$T/$lab/$fx; seed "$d" "$fx"; heated_input "$d" "$var" "$pr" "$pc"
    run "$d" "$E" "$n"; v $? "[$T] heated $var on $n rank(s) from the $fx fixture finished"
  done
  grep -q 'freshly initialised thermal field' "$W/$T/$lab/compact/run.log"; v $? "[$T] $lab: initial-read path taken (note printed)"
  for g in flow_restart thermo_restart; do
    cmp -s "$W/$T/$lab/exact/1_data/domain1_${g}_40.bin" "$W/$T/$lab/compact/1_data/domain1_${g}_40.bin"
    v $? "[$T] $lab: $g at 40 byte-identical (start from compact vs exact)"
    cmp -s "$W/$T/$lab/exact/1_data/domain1_${g}_meta_40.dat" "$W/$T/$lab/compact/1_data/domain1_${g}_meta_40.dat"
    v $? "[$T] $lab: ${g}_meta at 40 identical"
  done
}
heated "$EXE" heated dns 1 0 0
heated "$EXE" heated mhd 1 0 0
heated "$EXE" heated mhdles 1 0 0
heated "$EXE" heated mhdles "$NP4" 2 2

########## reject (compact mode continuation unless noted; one step, no checkpoint)
rej() {  # label mode(compact|exact|heated) fixture expect(stop|accept) message-regex mutation...
  local lab=$1 mode=$2 fx=$3 exp=$4 re=$5 d; shift 5
  d=$W/reject/$lab; seed "$d" "$fx"
  if [ "$mode" = heated ]; then heated_input "$d" dns 0 0; sed -i -e 's/^\( *niterflowlast *= *\).*/\131/' -e 's/^\( *niterthermolast *= *\).*/\131/' -e 's/^\( *ckpt_nfre *= *\).*/\1100/' "$d/input_chapsim.ini"
  else iso_input "$d" restart 30 31 31 100 "$mode" 0 0; fi
  ( cd "$d/1_data" && eval "$*" )
  run "$d" "$EXE" 1; local fin=$?
  local msg; msg=$(grep -m1 -o '!ERROR! <<.*' "$d/run.log" | cut -c11-150)
  if [ "$exp" = stop ]; then
    [ $fin != 0 ] && [ "$(cat "$d/exit_status")" != 0 ] && grep -q -E "$re" "$d/run.log"
    v $? "reject $lab: stops with \"${msg:-no error message}\""
  else
    obs "reject $lab: $( [ $fin = 0 ] && echo "ACCEPTED (run finished)" || echo "stopped: ${msg:-?}" ); $(grep -m1 -o -E "$re" "$d/run.log" || true)"
  fi
}
B=domain1_flow_restart_30.bin; M=domain1_flow_restart_meta_30.dat; C=domain1_checkpoint_meta_30.dat
rej missing_bin      compact compact stop 'does not exist' "rm $B"
rej missing_meta     compact compact stop 'metadata file .* does not exist' "rm $M"
rej bad_header       compact compact stop 'Unsupported bundle metadata format' "sed -i '1s/.*/CHAPSim_bundle_v1/' $M"
rej key_order        compact compact stop 'Expected metadata key' "sed -i '/^time_end /d' $M"
rej wrong_fields     compact compact stop 'field list mismatch' "sed -i 's/^fields .*/fields qx:dpcc,qy:dcpc,qz:dccp/' $M"
rej wrong_shapes     compact compact stop 'field-shape mismatch' "sed -i 's/^field_shapes qx=\([0-9]*\),/field_shapes qx=1\1,/' $M"
rej wrong_nc         compact compact stop 'cell-count mismatch' "sed -i 's/^nc 32 48 32/nc 32 48 16/' $M"
rej wrong_iter       compact compact stop 'iteration mismatch' "sed -i 's/^iter 30/iter 29/' $M"
rej exact_in_compact compact exact   stop 'field list mismatch' "true"
rej compact_in_exact exact   compact stop 'field list mismatch' "true"
rej heated_bad_fields heated compact stop 'field list mismatch' "sed -i 's/^fields .*/fields qx:dpcc,qy:dcpc,qz:dccp/' $M"
rej heated_missing_meta heated compact stop 'does not exist' "rm $M"
# statistics window opened before the fixture iteration: the fixture carries no statistics
rej heated_stats_window heated compact stop 'tatistics|does not exist' "sed -i 's/^\\( *stat_istart *= *\\).*/\\110/' ../input_chapsim.ini"
rej truncated_bin    compact compact accept 'Reading 1_data[^ ]*' "truncate -s -8 $B"
rej short_bin_half   compact compact accept 'Reading 1_data[^ ]*' "truncate -s 409600 $B"
rej no_ckpt_meta     compact compact accept 'restart time is estimated' "rm $C"
rej stretch_changed  compact compact accept 'Reading 1_data[^ ]*' "sed -i 's/^stretch .*/stretch 0 0  5.0000000000000000E-001/' $M"
rej length_changed   compact compact accept 'Reading 1_data[^ ]*' "sed -i 's/^length .*/length  1.6000000000000000E+001 -1.0000000000000000E+000  1.0000000000000000E+000  4.0000000000000000E+000/' $M"
rej exact_bin_compact_meta compact compact accept 'Reading 1_data[^ ]*' "cp ../../../fixture/exact/$B $B"
rej heated_exact_bin_compact_meta heated compact accept 'freshly initialised' "cp ../../../fixture/exact/$B $B"
# what the truncated reads gave: the first q/pr values the solver printed after the read
for lab in truncated_bin short_bin_half; do
  obs "reject $lab: $(grep -m3 -i -E 'max.*(qx|pr)|qx.*max' "$W/reject/$lab/run.log" | tr -s ' ' | tr '\n' ';' | cut -c1-200)"
done

########## second build
if [ -n "$EXE2" ]; then
  gen "$EXE2" gen2
  for lab in e30 c30; do
    cmp -s "$W/gen/$lab/1_data/domain1_flow_restart_30.bin" "$W/gen2/$lab/1_data/domain1_flow_restart_30.bin"
    v $? "EXE2 $lab: flow_restart_30.bin byte-identical to EXE's"
    cmp -s "$W/gen/$lab/1_data/domain1_flow_restart_meta_30.dat" "$W/gen2/$lab/1_data/domain1_flow_restart_meta_30.dat"
    v $? "EXE2 $lab: manifest byte-identical to EXE's"
  done
  heated "$EXE2" heated2 mhdles 1 0 0
  heated "$EXE2" heated2 mhdles "$NP4" 2 2
fi

if [ "${LEAN:-0}" = 1 ]; then   # (no -prune: -delete implies -depth, which disables it)
  ( cd "$W" && find . -type f \( -name '*.bin' -o -name '*_meta_*.dat' \) ! -path './fixture/*' ! -path './converted/*' \
      | sort | xargs sha256sum > run_files.sha256 )
  ( cd "$W" && find . -type f -name '*.bin' ! -path './fixture/*' ! -path './converted/*' -delete )
fi
echo "summary: PASS $NP, FAIL $NF" | tee -a "$OUT"
exit $(( NF > 0 ))
