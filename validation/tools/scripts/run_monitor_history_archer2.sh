#!/usr/bin/env bash
set -euo pipefail
##
PY=/opt/cray/pe/python/3.10.10/bin/python3
CHAP=/work/c01/c01/wwangdl/CHAPSim2
CASE=/work/c01/c01/wwangdl/CHAPSim_Production/channel_iso_periodic/Ret180/run3_mesh256_more
##
$PY $CHAP/validation/tools/scripts/plot_monitor_bulk_change_history.py \
  --case-dir $CASE \
  --output-dir $CASE/3_monitor/plots
