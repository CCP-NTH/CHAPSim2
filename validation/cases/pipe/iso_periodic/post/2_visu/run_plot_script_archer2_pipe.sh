#!/usr/bin/env bash
set -euo pipefail
##
PY=/opt/cray/pe/python/3.10.10/bin/python3
CHAP=/work/c01/c01/wwangdl/CHAPSim2
CASE=/work/c01/c01/wwangdl/CHAPSim_Production/pipe_iso_periodic/Ret180/run4_tripping
##
$PY $CHAP/validation/cases/pipe/iso_periodic/post/2_visu/plot_pipe_velo_stress_v2.py \
  --dns-time 1090000 \
  --re 2650 \
  --input-dir $CASE/2_visu/data \
  --output-dir $CASE/2_visu/plots \
  --ref-dir $CHAP/validation/references/pipe/tdl/retau180
