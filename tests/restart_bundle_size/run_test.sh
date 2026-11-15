#!/usr/bin/env bash
# A bundled restart whose .bin does not hold exactly the arrays its metadata lists must stop
# the run before it is read; valid bundles must behave as before.
#
#   tests/restart_bundle_size/run_test.sh [EXE] [WORKDIR] [EXE_REF]
#
# Fixtures are written by the solver itself (bundled, restart_history_mode exact and compact):
#   inout    tests/regression/channel_iso_inout (inlet database, convective x-outlet: the
#            bundles carry the fbcx_q* outlet planes, the exact one also their RHS history)
#   periodic tests/regression/channel_iso_periodic (q, pr only: the GPU qualification layout)
# Each runs 1..4 with checkpoints at 2 and 4, in both modes.
#
# valid   continuation 3..4 from step 2 in the same mode (inout, periodic); heated start
#         (tests/regression/channel_scp_inout_Tw, fresh thermal field) 3..4 from the inout
#         exact and compact precursors; heated continuation 5..6 (flow + thermal bundles).
#         Each must finish; a continuation must equal the uninterrupted run at 4 (exact: the
#         flow bundle, its metadata, checkpoint_meta; compact: the bundle, RK3), the two heated
#         starts must agree at 4. With EXE_REF (the solver without the size check) the whole
#         valid chain is repeated and every restart file must be byte-identical.
# reject  mutated bundles must stop with the given message, without "finished successfully",
#         with a non-zero exit, within TMAX seconds (no hang): truncation inside the velocity
#         data, inside the outlet planes, inside the momentum and the outlet RHS history;
#         padding; the other layout's metadata; wrong precision_bytes; missing .bin; truncated
#         thermal bundle; truncated LES bundle (heated WALE run from
#         tests/functional/LES_channel_scp_inout_Tw, 1..2, continued from 2 with the flow and
#         thermal bundles intact). On NP ranks; a subset also on 1 rank.
# Assumes the default real64 bundles (truncation offsets use 8 bytes per value).
# Environment: NP (default 4), TMAX (default 300 s), KEEP=1 keeps the .bin files.
# Exit 0 if every check passes.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXE=${1:-$ROOT/bin/CHAPSim}
WORK=${2:-$(mktemp -d /tmp/chapsim_bundle_size.XXXXXX)}
EXE_REF=${3:-}
NP=${NP:-4}; TMAX=${TMAX:-300}; KEEP=${KEEP:-0}
REG=$ROOT/tests/regression
mkdir -p "$WORK"; WORK=$(cd "$WORK" && pwd)
NPASS=0; NFAIL=0
v() { if [ "$1" = 0 ]; then NPASS=$((NPASS + 1)); echo "PASS  $2"; else NFAIL=$((NFAIL + 1)); echo "FAIL  $2"; fi; }

mk() {  # dir template history first last ckpt [from [heated-start|heated-cont]]
  local d=$1 tmpl=$REG/$2 from=${7:-0} kind=${8:-}
  rm -rf "$d"; mkdir -p "$d/1_data"
  cp "$tmpl"/1_data/* "$d/1_data/" 2>/dev/null || true
  cp "$tmpl"/*.DAT "$d/" 2>/dev/null || true
  local s=( -e "s/^\( *niterflowfirst *= *\).*/\1$4/" -e "s/^\( *niterflowlast *= *\).*/\1$5/"
            -e "s/^\( *ckpt_nfre *= *\).*/\1$6/" -e "s/^\( *visu_nfre *= *\).*/\1100000/"
            -e "s/^\( *stat_istart *= *\).*/\1100000/"
            -e "s/^\( *is_record_xoutlet_read_xinlet *= *\)[^,]*,/\1.false.,/"
            -e "s/^\( *restart_data_layout *= *.*\)/\1\nrestart_history_mode= $3/" )
  [ "$from" -gt 0 ] && s+=( -e "s/^\( *initfl *= *\).*/\1restart/" -e "s/^\( *irestartfrom *= *\).*/\1$from/" )
  case "$kind" in
    heated-start) s+=( -e "s/^\( *inittm *= *\).*/\1const/" ) ;;
    heated-cont)  s+=( -e "s/^\( *inittm *= *\).*/\1restart/" ) ;;
  esac
  [ -n "$kind" ] && s+=( -e "s/^\( *niterthermofirst *= *\).*/\1$4/" -e "s/^\( *niterthermolast *= *\).*/\1$5/" )
  sed "${s[@]}" "$tmpl/input_chapsim.ini" > "$d/input_chapsim.ini"
  [ "$(grep -c "^restart_history_mode= $3\$" "$d/input_chapsim.ini")" = 1 ] || { echo "cannot set restart_history_mode in $d"; exit 2; }
}
run() {  # dir [np] [exe]
  ( cd "$1" && timeout "$TMAX" mpirun -np "${2:-$NP}" "${3:-$EXE}" > run.log 2>&1 < /dev/null ); echo $? > "$1/exit_code"
  grep -q 'finished successfully' "$1/run.log"; }
seed() { cp "$1"/1_data/domain1_*_"$3".* "$2/1_data/"; }  # src dst iter
offset() {  # metadata k: bytes of the first k arrays it lists
  awk -v k="$2" '$1 == "field_shapes" { n = split($2, e, ";"); s = 0
    for(i = 1; i <= k && i <= n; i++) { split(e[i], a, "="); split(a[2], d, ","); s += d[1] * d[2] * d[3] * 8 }
    print s }' "$1"; }

########## valid chain: fixtures, continuations, heated start and continuation
chain() {  # root exe
  local R=$1 X=$2 c h d
  for c in inout periodic; do
    for h in exact compact; do
      mk "$R/$c/$h/full" channel_iso_$c $h 1 4 2; run "$R/$c/$h/full" "$NP" "$X"; v $? "valid: $c $h 1..4 finishes [$R]"
      d=$R/$c/$h/cont; mk "$d" channel_iso_$c $h 3 4 2 2; seed "$R/$c/$h/full" "$d" 2
      run "$d" "$NP" "$X"; v $? "valid: $c $h continuation 3..4 finishes [$R]"
    done
  done
  for h in exact compact; do
    d=$R/inout/heated_from_$h; mk "$d" channel_scp_inout_Tw exact 3 4 2 2 heated-start; seed "$R/inout/$h/full" "$d" 2
    run "$d" "$NP" "$X"; v $? "valid: heated start from the inout $h precursor finishes [$R]"
  done
  d=$R/inout/heated_cont; mk "$d" channel_scp_inout_Tw exact 5 6 2 4 heated-cont; seed "$R/inout/heated_from_exact" "$d" 4
  run "$d" "$NP" "$X"; v $? "valid: heated continuation 5..6 finishes [$R]"
}
restart_files() { ls "$1"/1_data | grep -E '^domain1_(flow_restart|flow_restart_meta|thermo_restart|thermo_restart_meta|thermo_interp_meta|les_restart|les_restart_meta|checkpoint_meta)_[0-9]+\.' ; }

A=$WORK/run; chain "$A" "$EXE"
for c in inout periodic; do
  for f in domain1_flow_restart_4.bin domain1_flow_restart_meta_4.dat domain1_checkpoint_meta_4.dat; do
    cmp -s "$A/$c/exact/full/1_data/$f" "$A/$c/exact/cont/1_data/$f"; v $? "valid: $c exact continuation = uninterrupted at 4: $f"
  done
  cmp -s "$A/$c/compact/full/1_data/domain1_flow_restart_4.bin" "$A/$c/compact/cont/1_data/domain1_flow_restart_4.bin"
  v $? "valid: $c compact continuation = uninterrupted at 4 (RK3)"
done
n=0; bad=0
for f in $(restart_files "$A/inout/heated_from_exact" | grep '_4\.'); do
  n=$((n + 1)); cmp -s "$A/inout/heated_from_exact/1_data/$f" "$A/inout/heated_from_compact/1_data/$f" || bad=$((bad + 1))
done
[ $n -ge 4 ] && [ $bad = 0 ]; v $? "valid: heated start from compact = from exact precursor at 4 ($n files, $bad differ)"
if [ -n "$EXE_REF" ]; then
  Z=$WORK/ref; chain "$Z" "$EXE_REF"
  for d in $(cd "$A" && find . -name run.log -printf '%h\n' | sort); do
    n=0; bad=0
    for f in $(restart_files "$A/$d"); do n=$((n + 1)); cmp -s "$A/$d/1_data/$f" "$Z/$d/1_data/$f" || bad=$((bad + 1)); done
    [ $n -gt 0 ] && [ $bad = 0 ]; v $? "neutral: ${d#./} identical to EXE_REF ($n restart files, $bad differ)"
  done
fi

########## reject
rej() {  # label case fixture-mode kind np message-regex mutation
  local lab=$1 c=$2 h=$3 kind=$4 np=$5 re=$6 mut=$7 d=$WORK/reject/$1
  case "$kind" in
    cont)   mk "$d" channel_iso_$c $h 3 3 100 2; seed "$A/$c/$h/full" "$d" 2 ;;
    xcont)  mk "$d" channel_iso_$c exact 3 3 100 2; seed "$A/$c/$h/full" "$d" 2 ;;
    heated) mk "$d" channel_scp_inout_Tw exact 3 3 100 2 heated-start; seed "$A/$c/$h/full" "$d" 2 ;;
    hcont)  mk "$d" channel_scp_inout_Tw exact 5 5 100 4 heated-cont; seed "$A/inout/heated_from_exact" "$d" 4 ;;
    lcont)  mk "$d" ../functional/LES_channel_scp_inout_Tw exact 3 3 100 2 heated-cont; seed "$A/les/full" "$d" 2 ;;
  esac
  ( cd "$d" && eval "$mut" )
  run "$d" "$np"; local fin=$? code; code=$(cat "$d/exit_code")
  [ $fin != 0 ] && [ "$code" != 0 ] && [ "$code" != 124 ] && grep -q -E "$re" "$d/run.log"
  v $? "reject $lab (np $np, exit $code): $(grep -m1 -o -E "$re[^>]{0,60}" "$d/run.log" || echo 'NO MESSAGE')"
  [ "$KEEP" = 1 ] || rm -f "$d"/1_data/*.bin
}
B=1_data/domain1_flow_restart_2.bin; M=1_data/domain1_flow_restart_meta_2.dat
SHORT='Bundle size mismatch \(file too short\)'; LONG='Bundle size mismatch \(file too long\)'
EB=$A/inout/exact/full/$B; CB=$A/inout/compact/full/$B
PM=$A/periodic/exact/full/$M
q2=$(offset "$PM" 2); q3=$(offset "$PM" 3); r5=$(offset "$PM" 5); r6=$(offset "$PM" 6)
for np in $NP 1; do
  rej c_trunc8_np$np    inout compact cont  $np "$SHORT" "truncate -s -8 $B"        # last array: fbcx_qz
  rej x_trunc8_np$np    inout exact   xcont $np "$SHORT" "truncate -s -8 $B"        # last: outlet RHS history
  rej h_c_meta_x_bin_np$np inout compact heated $np "$LONG" "cp $EB $B"
done
rej c_trunc_in_qz     periodic compact cont  $NP "$SHORT" "truncate -s $(( (q2 + q3) / 2 )) $B"
rej c_trunc_outlet    inout    compact cont  $NP "$SHORT" "truncate -s -1000 $B"
rej x_trunc_in_my_rhs periodic exact   xcont $NP "$SHORT" "truncate -s $(( (r5 + r6) / 2 )) $B"
rej x_trunc_to_q_pr   periodic exact   xcont $NP "$SHORT" "truncate -s $(offset "$PM" 4) $B"
rej c_pad8            inout    compact cont  $NP "$LONG"  "truncate -s +8 $B"
rej c_meta_x_bin      inout    compact cont  $NP "$LONG"  "cp $EB $B"
rej x_meta_c_bin      inout    exact   xcont $NP "$SHORT" "cp $CB $B"
rej precision4        inout    compact cont  $NP 'Bundle precision mismatch' "sed -i 's/^precision_bytes .*/precision_bytes 4/' $M"
rej missing_bin       inout    compact cont  $NP 'does not exist' "rm $B"
rej h_c_trunc8        inout    compact heated $NP "$SHORT" "truncate -s -8 $B"
rej h_x_trunc8        inout    exact   heated $NP "$SHORT" "truncate -s -8 $B"
rej h_x_meta_c_bin    inout    exact   heated $NP "$SHORT" "cp $CB $B"
rej hc_thermo_trunc8  inout    exact   hcont  $NP "$SHORT" "truncate -s -8 1_data/domain1_thermo_restart_4.bin"
# heated LES fixture: the LES bundle (tvisc, prsgs) is read only on a full heated continuation
mk "$A/les/full" ../functional/LES_channel_scp_inout_Tw exact 1 2 2 0 heated-start
run "$A/les/full"; [ $? = 0 ] && [ -f "$A/les/full/1_data/domain1_les_restart_2.bin" ]
v $? "valid: heated LES 1..2 finishes and writes the LES bundle at 2"
rej hl_les_trunc8     les      exact   lcont  $NP "$SHORT" "truncate -s -8 1_data/domain1_les_restart_2.bin"

[ "$KEEP" = 1 ] || find "$WORK" -path '*/1_data/*' -name 'domain1_*.bin' ! -name 'domain1_outlet*' -delete
echo "$NPASS passed, $NFAIL failed ($WORK)"
[ $NFAIL = 0 ]
