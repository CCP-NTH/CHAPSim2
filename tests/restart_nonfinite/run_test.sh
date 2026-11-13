#!/usr/bin/env bash
# A NaN or infinity in the solution must stop the run on every rank through the continuity check
# (abort_if_nonfinite_continuity, eq_continuity.f90), with a diagnostic, a failure status, bounded time
# and no surviving rank. Production path, no test hook: one value of a restart bundle is overwritten
# (tests/tools/plant_nonfinite.py) and the run restarts from it, so the first continuity check to see it
# is the pre-loop 'initial' diagnostic, which must stop on non-finite values at any iteration.
#
#   tests/restart_nonfinite/run_test.sh EXE [WORKDIR]
#   env: NP (2), P_ROW / P_COL (1 2), LAUNCH ("mpirun -np $NP"), TMO (300 s per run),
#        EXPECT (abort: the continuity message is required; nonzero: any bounded failure is accepted,
#        e.g. a CPU build with -ffpe-trap whose earlier comparisons trap first)
#
# Cases: channel_iso_inout (isothermal: qx) and channel_scp_inout_Tw (heated: gx and qx), both with
# an inlet database and a convective outlet. The value sits at global (33, 60, 48), owned by rank 1
# in 1x2 and in 2x1, so the failure originates on a non-root rank. A restart without the planted
# value must finish (control). Exit 0 if every check passes.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXE=$(realpath "${1:?executable}")
WORK=${2:-$(mktemp -d /tmp/chapsim_restart_nonfinite.XXXXXX)}
NP=${NP:-2}; P_ROW=${P_ROW:-1}; P_COL=${P_COL:-2}; LAUNCH=${LAUNCH:-"mpirun -np $NP"}; TMO=${TMO:-300}
EXPECT=${EXPECT:-abort}
MK="python3 -I $ROOT/tests/tools/mkcase.py"; PL="python3 -I $ROOT/tests/tools/plant_nonfinite.py"
COM=(io.visu_nfre=100000 io.stat_istart=100000 io.ckpt_nfre=20 decomposition.p_row=$P_ROW decomposition.p_col=$P_COL)
mkdir -p "$WORK"; status=0
echo "restart_nonfinite: exe $(sha256sum "$EXE" | cut -c1-16), NP $NP, p_row $P_ROW, p_col $P_COL, expect $EXPECT"
run() { ( cd "$1" && timeout "$TMO" $LAUNCH "$EXE" > run.log 2>&1 < /dev/null ); echo $? > "$1/rc.txt"; }
for c in channel_iso_inout channel_scp_inout_Tw; do
  T=$ROOT/tests/regression/$c; TH=(); grep -qiE '^ *ithermo *= *\.true\.' "$T/input_chapsim.ini" && TH=(simcontrol.niterthermofirst=1 simcontrol.niterthermolast=20)
  $MK "$T" "$WORK/$c/base" "${COM[@]}" simcontrol.niterflowlast=20 "${TH[@]}" > /dev/null || exit 2
  run "$WORK/$c/base"; grep -q 'finished successfully' "$WORK/$c/base/run.log" || { echo "$c base run FAILED"; exit 2; }
  RS=(flow.initfl=restart flow.irestartfrom=20 simcontrol.niterflowfirst=21 simcontrol.niterflowlast=24)
  [ ${#TH[@]} -gt 0 ] && RS+=(thermo.inittm=restart thermo.irestartfrom=20 simcontrol.niterthermofirst=21 simcontrol.niterthermolast=24)
  for v in control nan inf; do
    d=$WORK/$c/$v
    $MK "$T" "$d" "${COM[@]}" "${RS[@]}" --copy-data "$WORK/$c/base:*_20.*" > /dev/null || exit 2
    if [ $v != control ]; then
      $PL "$d/1_data" flow 20 qx 33 60 48 $v | sed 's/^/    /'
      [ ${#TH[@]} -gt 0 ] && $PL "$d/1_data" flow 20 gx 33 60 48 $v | sed 's/^/    /'
    fi
    t0=$(date +%s); run "$d"; rc=$(cat "$d/rc.txt"); el=$(( $(date +%s) - t0 ))
    # solver processes by process name (pgrep -f would also match this script, whose command line holds EXE);
    # run the test with no other run of the same executable name by this user on the node
    sleep 2; left=$(pgrep -u "$(id -u)" -x "$(basename "$EXE" | cut -c1-15)" | wc -l)
    fin=$(grep -c 'finished successfully' "$d/run.log")
    msg=$(grep -h -m1 'NaN or infinity in the continuity check' "$d/run.log" | tr -s ' ')
    rmsg=$(grep -h -m1 -E '>> !ERROR! << rank [0-9]+: .*non-finite' "$d/run.log" | tr -s ' ' | cut -c1-170)
    if [ $v = control ]; then
      ok=1; [ "$rc" = 0 ] && [ "$fin" = 1 ] || ok=0
      echo "$c control: exit $rc, ${el} s, finished $fin -> $([ $ok = 1 ] && echo PASS || echo FAIL)"
    else
      ok=1; { [ "$rc" != 0 ] && [ "$rc" != 124 ] && [ "$fin" = 0 ] && [ "$left" = 0 ]; } || ok=0
      [ "$EXPECT" = abort ] && { [ -n "$msg" ] && [ -n "$rmsg" ] || ok=0; }
      echo "$c $v: exit $rc$([ "$rc" = 124 ] && echo ' (TIMEOUT)'), ${el} s, finished $fin, surviving $left -> $([ $ok = 1 ] && echo PASS || echo FAIL)"
      echo "    rank:    ${rmsg:-(none)}"
      echo "    summary: ${msg:-(none)}"
      [ -z "$msg" ] && echo "    other:   $(grep -h -m1 -E 'SIGFPE|Floating-point|exited on signal|ERROR' "$d/run.log" | tr -s ' ' | cut -c1-150)"
    fi
    [ $ok = 1 ] || status=1
  done
done
echo "restart_nonfinite: $([ $status = 0 ] && echo ALL PASS || echo FAIL)"
exit $status
