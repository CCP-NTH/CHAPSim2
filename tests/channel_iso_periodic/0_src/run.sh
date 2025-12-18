#!/usr/bin/env bash
set -e

rm -f *.log *.dat fort* *.err *.out regression_test_metrics.json
rm -rf 2_*/ 3_*/ 4_*/
find 1_data -type f ! -name "*outlet*" -exec rm -f {} \;

EXEC=../../bin/CHAPSim
NP=4

timestamp=$(date +'%Y-%m-%d_%H.%M')
OUTPUT="output_chapsim2_${timestamp}.log"

nohup mpirun -np ${NP} ${EXEC} > "${OUTPUT}" 2>&1 &
