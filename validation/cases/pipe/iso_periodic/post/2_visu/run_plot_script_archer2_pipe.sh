#!/usr/bin/env bash
set -euo pipefail
##
PY=/opt/cray/pe/python/3.10.10/bin/python3
## Set CHAP to your CHAPSim2 installation and CASE to the run directory,
## either by editing the lines below or by exporting them beforehand.
CHAP=${CHAP:-/path/to/CHAPSim2}
CASE=${CASE:-/path/to/your/case/directory}
##
$PY $CHAP/validation/cases/pipe/iso_periodic/post/2_visu/plot_pipe_velo_stress_v2.py \
  --dns-time 1090000 \
  --re 2650 \
  --input-dir $CASE/2_visu/data \
  --output-dir $CASE/2_visu/plots \
  --ref-dir $CHAP/validation/references/pipe/tdl/retau180
