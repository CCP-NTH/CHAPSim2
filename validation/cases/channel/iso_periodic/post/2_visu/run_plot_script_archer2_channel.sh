#!/usr/bin/env bash
set -euo pipefail
##
PY=/opt/cray/pe/python/3.10.10/bin/python3
CHAP=/work/c01/c01/wwangdl/CHAPSim2
CASE=/work/c01/c01/wwangdl/CHAPSim_Production/channel_iso_periodic/Ret180/run3_mesh256_more/
##
$PY $CHAP/validation/cases/channel/iso_periodic/post/2_visu/plot_channel_velo_stress.py \
  --dns-time 600000 \
  --re 2800 \
  --input-dir $CASE/2_visu/data \
  --output-dir $CASE/2_visu/plots \
  --ref-dir $CHAP/validation/references/channel/mkm
