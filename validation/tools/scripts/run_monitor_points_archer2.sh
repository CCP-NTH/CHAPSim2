#!/usr/bin/env bash
set -euo pipefail
##
PY=/opt/cray/pe/python/3.10.10/bin/python3
CHAP=/work/c01/c01/wwangdl/CHAPSim2
CASE=/work/c01/c01/wwangdl/CHAPSim_Production/channel_iso_periodic/Ret180/run3_mesh256_more
DOMAIN_ID=1
NUM_POINTS=1
STRIDE=20
##
$PY $CHAP/validation/tools/scripts/plot_monitor_points.py \
  --case-dir $CASE \
  --domain-id $DOMAIN_ID \
  --num-points $NUM_POINTS \
  --stride $STRIDE \
  --output-dir $CASE/3_monitor/plots
