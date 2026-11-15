#!/usr/bin/env bash
# A remapped thermal field (mesh-mapping prerun) must be stored on the equation of state.
#
#   tests/restart_remap_thermo/run_test.sh [EXE] [WORKDIR]      (NP=4; BASE_EXE=<binary> also
#                                                                prints the old remap's error)
#
# Source: tests/regression/pipe_scp_inout_qw coarsened 2x in every direction (40x24x32),
# scp_water, tini = ref_t0 = 650 K (2.5 K below T_pc), q_w = 2e5 W/m2, constant initial
# temperature, thermal field from step 1, dt = 1e-3, 300 steps: a heated near-wall layer with
# a steep density gradient forms. The step-300 checkpoint is remapped (is_prerun) onto the
# template mesh 80x48x64. build_up_interp_target_field_thermo rebuilds the state from the
# interpolated (rho*h, rho) and output_interp_target_field sets rho_hat = rho_EOS(h); the
# stored rho*h must then equal rho_hat h, else the first recovery h = rho*h/rho_hat of the
# remapped run jumps by h (rho_i - rho_EOS)/rho_EOS wherever the interpolation cuts across the
# density gradient. Gate: remap_thermo_consistency.py --remap (recovery <= 1e-14, rho_hat == rho).
# Exit 0 if the gate passes, 1 if it fails, 2 if a run fails.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXE=$(realpath "${1:-$ROOT/bin/CHAPSim}")
WORK=${2:-$(mktemp -d /tmp/chapsim_remap_thermo.XXXXXX)}
NP=${NP:-4}
BASE_EXE=${BASE_EXE:-}
TMPL=$ROOT/tests/regression/pipe_scp_inout_qw
CHK=$ROOT/tests/tools/remap_thermo_consistency.py
mkdir -p "$WORK"

SRC=$WORK/source
if [ ! -f "$SRC/1_data/domain1_thermo_restart_300.bin" ] || [ "$(cat "$SRC/exe" 2>/dev/null)" != "$EXE" ]; then
  rm -rf "$SRC"; mkdir -p "$SRC/1_data"
  cp "$TMPL"/1_data/* "$SRC/1_data/"; cp "$TMPL"/*.DAT "$TMPL"/input_chapsim.ini "$SRC/"
  sed -i -e "s/^\( *ref_t0 *= *\).*/\1650.0/" -e "s/^\( *tini *= *\).*/\1650.0/" \
         -e "/^ *qw_ramp *=/d" \
         -e "s/^\( *ifbcx_t *= *\).*/\14,5,650.0,0.0/" -e "s/^\( *ifbcy_t *= *\).*/\10,5,0.0,2.0e5/" \
         -e "s/^\( *ncx *= *\).*/\140/" -e "s/^\( *ncy *= *\).*/\124/" -e "s/^\( *ncz *= *\).*/\132/" \
         -e "s/^\( *dt *= *\).*/\11e-3/" \
         -e "s/^\( *niterthermofirst *= *\).*/\11/" \
         -e "s/^\( *niterflowlast *= *\).*/\1300/" -e "s/^\( *niterthermolast *= *\).*/\1300/" \
         -e "s/^\( *ckpt_nfre *= *\).*/\1300/" \
         -e "s/^\( *visu_nfre *= *\).*/\1100000/" -e "s/^\( *stat_istart *= *\).*/\1100000/" \
         "$SRC/input_chapsim.ini"
  ( cd "$SRC" && mpirun -np "$NP" "$EXE" > run.log 2>&1 < /dev/null )
  grep -q 'finished successfully' "$SRC/run.log" || { echo "source run FAILED ($SRC/run.log)"; exit 2; }
  echo "$EXE" > "$SRC/exe"
fi

remap() {  # DIR EXE : prerun from the source step-300 checkpoint onto the template mesh
  local d=$1 exe=$2
  rm -rf "$d"; mkdir -p "$d/1_data"
  cp "$SRC"/input_chapsim.ini "$SRC"/*.DAT "$d/"
  cp "$SRC"/1_data/*_300.* "$d/1_data/"
  sed -i -e "s/^\( *is_prerun *= *\).*/\1.true./" \
         -e "/^\[flow\]/,/^\[/ s/^\( *initfl *= *\).*/\1restart/" -e "s/^\( *inittm *= *\).*/\1restart/" \
         -e "s/^\( *irestartfrom *= *\).*/\1300/" \
         -e "s/^\( *niterflowfirst *= *\).*/\1301/" -e "s/^\( *niterthermofirst *= *\).*/\1301/" \
         -e "s/^\( *is_record_xoutlet_read_xinlet *= *\).*/\1.false.,.false./" "$d/input_chapsim.ini"
  awk '/^\[domain\]/{f=1} /^\[flow\]/{f=0} f' "$TMPL/input_chapsim.ini" > "$d/input_chapsim_tgt.ini"
  awk '/^\[mesh\]/{f=1} /^\[bc\]/{f=0} f' "$TMPL/input_chapsim.ini" >> "$d/input_chapsim_tgt.ini"
  ( cd "$d" && mpirun -np 2 "$exe" > run.log 2>&1 < /dev/null )
  [ -f "$d/1_data/domain1_thermo_restart_0.bin" ] || { echo "remap FAILED ($d/run.log)"; exit 2; }
}

if [ -n "$BASE_EXE" ]; then
  remap "$WORK/map_base" "$(realpath "$BASE_EXE")"
  echo "old remap ($BASE_EXE):"; python3 -I "$CHK" "$WORK/map_base/1_data" 0 1e-14 --remap | sed 's/^/  /'
fi
remap "$WORK/map" "$EXE"
echo "remap ($EXE):"
python3 -I "$CHK" "$WORK/map/1_data" 0 1e-14 --remap | sed 's/^/  /'
status=${PIPESTATUS[0]}
[ "$status" = 0 ] && echo "PASS: remapped thermal state on the equation of state" || echo "FAIL"
exit "$status"
