#!/usr/bin/env bash
set -euo pipefail
##
PY=/opt/cray/pe/python/3.10.10/bin/python3
## Set CHAP to your CHAPSim2 installation and CASE to the run directory,
## either by editing the lines below or by exporting them beforehand.
CHAP=${CHAP:-/path/to/CHAPSim2}
CASE=${CASE:-/path/to/your/case/directory}
DOMAIN_ID=1
NUM_POINTS=1
STRIDE=20
##
$PY $CHAP/validation/tools/scripts/plot_monitor_points_seperate.py \
  --case-dir $CASE \
  --domain-id $DOMAIN_ID \
  --num-points $NUM_POINTS \
  --stride $STRIDE \
  --output-dir $CASE/3_monitor/plots
