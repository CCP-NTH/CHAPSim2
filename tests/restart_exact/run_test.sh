#!/usr/bin/env bash
# A heated inlet/outlet run restarted from its own checkpoint must continue bit for bit.
#
#   tests/restart_exact/run_test.sh [EXE] [WORKDIR]      (FLUIDS="water sodium water_perfield les_heated
#                                                          water_delayed water_convT", NP=4)
#
# Built from tests/regression/channel_scp_inout_Tw (inlet database, convective
# x-outlet, Neumann temperature outlet, Tw walls with a 2.0 unheated buffer, CD2, default
# rhoh_recovery = transported), thermal field from step 1:
#   water : scp_water with ref_t0 = 640.0 K, so that h /= 0 on the x boundaries. With
#           ref_t0 = tini the boundary h is exactly 0 for these short runs, and a boundary
#           state re-derived from the stored (rho, rho*h) always round-trips: the case
#           would not test the outlet-state restore of read_flow_restart_xoutlet_state.
#   sodium: liquid sodium (correlation property path), ref_t0 = tini = inlet T = 600 K,
#           walls 580/640 K: the Dirichlet inlet state, re-derived from (rho, rho*h)
#           through the tabulated inverse, is not the state the correlations give.
#   water_perfield: water with restart_data_layout = per_field (writing and reading): the
#           convective-outlet state must be stored in that layout too.
#   les_heated: tests/functional/LES_channel_scp_inout_Tw (WALE, scp_water): the lagged
#           subgrid viscosity and Prandtl number of a heated LES run must be checkpointed.
#   water_convT: water with a convective thermal x-outlet (ifbcx_t = 4,7), outlet_mass_correction =
#           split, thermal_outlet_pair = zero_gradient and enthalpy_advection = koren (the options of the
#           Nemati production): the outlet keeps its advected rho*h, which a restart must restore.
#   water_delayed: water with the energy equation from step 11 (niterthermofirst > niterflowfirst).
#           The thermal field keeps the run clock, so its checkpoint 20 is written at flow step 20
#           (record flow_iteration 20) and the continuation from 20 must be exact. (Builds before
#           2026-10-30 labelled it by the thermal step count: thermal checkpoint 20 then held flow
#           step 30 and the continuation silently coupled two different steps.)
# Each fluid: continuous run 1..30 (checkpoints 10, 20, 30) and a continuation from the step-20
# checkpoint; every step-30 checkpoint file (bundles, or the per-field files) must be
# identical (cmp).
# Exit 0 if every fluid continues bit for bit, 1 otherwise.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXE=${1:-$ROOT/bin/CHAPSim}
WORK=${2:-$(mktemp -d /tmp/chapsim_heated_inout_restart.XXXXXX)}
NP=${NP:-4}
FLUIDS=${FLUIDS:-"water sodium water_perfield les_heated water_delayed water_convT"}

make_case() {  # dir fluid restart_from(0 = fresh)
  local d=$1 fluid=$2 from=$3 first TMPL="$ROOT/tests/regression/channel_scp_inout_Tw"
  [ "$fluid" = les_heated ] && TMPL="$ROOT/tests/functional/LES_channel_scp_inout_Tw"
  mkdir -p "$d/1_data"
  cp "$TMPL"/1_data/* "$d/1_data/"; cp "$TMPL"/*.DAT "$d/"
  first=$((from + 1))
  local thfirst=$first
  [ "$fluid" = water_delayed ] && [ "$from" = 0 ] && thfirst=11
  local sed_args=(
    -e "s/^\( *niterflowfirst *= *\).*/\1$first/" -e "s/^\( *niterflowlast *= *\).*/\130/"
    -e "s/^\( *niterthermofirst *= *\).*/\1$thfirst/" -e "s/^\( *niterthermolast *= *\).*/\130/"
    -e "s/^\( *ckpt_nfre *= *\).*/\110/" -e "s/^\( *visu_nfre *= *\).*/\1100000/"
    -e "s/^\( *stat_istart *= *\).*/\1100000/" )
  case "$fluid" in
    water)  sed_args+=( -e "s/^\( *ref_t0 *= *\).*/\1640.0/" ) ;;
    water_perfield) sed_args+=( -e "s/^\( *ref_t0 *= *\).*/\1640.0/"
                        -e "s/^\( *restart_data_layout *= *\).*/\1per_field/" ) ;;
    sodium) sed_args+=( -e "s/^\( *ifluid *= *\).*/\1sodium/" -e "s/^\( *ref_t0 *= *\).*/\1600.0/"
                        -e "s/^\( *tini *= *\).*/\1600.0/" -e "s/^\( *ifbcx_t *= *\).*/\14,5,600.0,0.0/"
                        -e "s/^\( *ifbcy_t *= *\).*/\14,4,580.0,640.0/" ) ;;
    les_heated) ;;
    water_delayed) sed_args+=( -e "s/^\( *ref_t0 *= *\).*/\1640.0/" ) ;;
    water_convT) sed_args+=( -e "s/^\( *ref_t0 *= *\).*/\1640.0/" -e "s/^\( *ifbcx_t *= *\).*/\14,7,645.15,0.0/"
                       -e "s/^\( *qw_ramp *=.*\)/\1\nenthalpy_advection= koren\nthermal_outlet_pair= zero_gradient/"
                       -e "s/^\( *drivenfc *=.*\)/\1\noutlet_mass_correction= split/" ) ;;
    *) echo "unknown fluid $fluid"; exit 2 ;;
  esac
  if [ "$from" -gt 0 ]; then
    sed_args+=( -e "s/^\( *initfl *= *\).*/\1restart/" -e "s/^\( *inittm *= *\).*/\1restart/"
                -e "s/^\( *irestartfrom *= *\).*/\1$from/" )
  fi
  sed "${sed_args[@]}" "$TMPL/input_chapsim.ini" > "$d/input_chapsim.ini"
}

run() { ( cd "$1" && mpirun -np "$NP" "$EXE" > run.log 2>&1 < /dev/null ); grep -q 'finished successfully' "$1/run.log"; }

status=0
for fluid in $FLUIDS; do
  F="$WORK/$fluid"; rm -rf "$F"
  make_case "$F/full" "$fluid" 0
  run "$F/full" || { echo "$fluid: continuous run FAILED ($F/full/run.log)"; status=1; continue; }
  make_case "$F/split" "$fluid" 20
  cp "$F"/full/1_data/*_20.* "$F/split/1_data/"
  if [ "$fluid" = water_delayed ] && ! grep -q '^flow_iteration 20$' "$F/full/1_data/domain1_thermo_restart_meta_20.dat"; then
    echo "$fluid: thermal checkpoint 20 not written at flow step 20"; status=1; continue
  fi
  run "$F/split" || { echo "$fluid: continuation FAILED ($F/split/run.log)"; status=1; continue; }
  same=1; n=0
  for p in "$F"/full/1_data/domain1_*_30.bin; do
    b=$(basename "$p")
    case "$b" in *outlet1_*|*outlet2_*|*xoutlet_database*) continue ;; esac  # inlet database input
    n=$((n + 1))
    cmp -s "$p" "$F/split/1_data/$b" || same=0
  done
  [ $n -ge 2 ] || same=0
  if [ $same = 1 ]; then echo "$fluid: EXACT (continuation from 20 equals the continuous run at 30, $n files)"
  else echo "$fluid: NOT EXACT (step-30 checkpoint files differ)"; status=1; fi
done
exit $status
