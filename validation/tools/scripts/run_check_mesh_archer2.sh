#!/usr/bin/env bash
set -euo pipefail
##
PY=/opt/cray/pe/python/3.10.10/bin/python3
## Set CHAP to your CHAPSim2 installation and CASE to the run directory,
## either by editing the lines below or by exporting them beforehand.
CHAP=${CHAP:-/path/to/CHAPSim2}
CASE=${CASE:-/path/to/your/case/directory}
##
$PY $CHAP/validation/tools/scripts/plot_check_mesh.py \
  --case-dir $CASE \
  --output-dir $CASE/4_check/plots
