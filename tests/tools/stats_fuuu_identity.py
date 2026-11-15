#!/usr/bin/env python3
"""One-sample identity of the Favre triple moment (tests/stats_fuuu).

    stats_fuuu_identity.py DATA_DIR ITER [TOL]

Reads domain1_flow_stats_<ITER>.bin with its metadata (bundled layout). With exactly one sample
every running average equals its sample, so t_avg_fuuu_ijk * t_avg_f = t_avg_fuu_ij * t_avg_fu_k
to round-off when t_avg_fuuu holds rho u_i u_j u_k. With the former weight rho*T the two sides
differ by the factor T. Prints max |lhs - rhs| / max |rhs| over the ten components; exit 0 when
it is <= TOL (default 1e-12), the metadata says nsamples 1 and fuuu_definition rho.
"""
import sys
from pathlib import Path

import numpy as np

data, it = sys.argv[1], sys.argv[2]
tol = float(sys.argv[3]) if len(sys.argv) > 3 else 1e-12
meta = Path(data) / f"domain1_flow_stats_meta_{it}.dat"
names, keys = [], {}
for line in meta.read_text().splitlines():
    k, _, v = line.partition(' ')
    if k == 'fields':
        names = [f.split(':')[0] for f in v.split(',')]
    else:
        keys[k] = v.strip()
raw = np.fromfile(Path(data) / f"domain1_flow_stats_{it}.bin", dtype='<f8')
if not names or raw.size % len(names):
    sys.exit(f"{meta}: field list does not divide the bundle")
n = raw.size // len(names)
F = {name: raw[i * n:(i + 1) * n] for i, name in enumerate(names)}
worst = 0.0
for ijk in ('111', '112', '113', '122', '123', '133', '222', '223', '233', '333'):
    lhs = F['t_avg_fuuu' + ijk] * F['t_avg_f']
    rhs = F['t_avg_fuu' + ijk[:2]] * F['t_avg_fu' + ijk[2]]
    worst = max(worst, np.max(np.abs(lhs - rhs)) / max(np.max(np.abs(rhs)), 1e-300))
print(f"nsamples {keys.get('nsamples')}, fuuu_definition {keys.get('fuuu_definition', '(absent)')}, "
      f"max |fuuu f - fuu fu| / max |fuu fu| = {worst:.3e} (tol {tol:.0e})")
ok = worst <= tol and keys.get('nsamples') == '1' and keys.get('fuuu_definition') == 'rho'
sys.exit(0 if ok else 1)
