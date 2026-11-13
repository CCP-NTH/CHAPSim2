#!/usr/bin/env python3
"""Overwrite one value of a field in a bundled restart file with NaN or +Inf (test fixture only).

    plant_nonfinite.py DATA_DIR GROUP ITER FIELD I J K nan|inf

GROUP is flow or thermo (domain1_<GROUP>_restart_<ITER>.bin with its _meta_ file). I, J, K are
1-based global indices of FIELD (shapes from the metadata line field_shapes; Fortran order).
The file is modified in place; its size is unchanged, so the bundle size check still passes.
"""
import struct
import sys
from pathlib import Path

d, group, it, field = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
i, j, k = (int(x) for x in sys.argv[5:8])
kind = sys.argv[8]
meta = Path(d) / f"domain1_{group}_restart_meta_{it}.dat"
binf = Path(d) / f"domain1_{group}_restart_{it}.bin"
keys = {}
for line in meta.read_text().splitlines():
    a, _, b = line.partition(' ')
    keys[a] = b.strip()
assert keys.get('precision_bytes') == '8' and keys.get('layout') == 'bundled', 'bundled double precision only'
names = [f.split(':')[0] for f in keys['fields'].split(',')]
shapes = {}
for item in keys['field_shapes'].split(';'):
    nm, _, sh = item.partition('=')
    shapes[nm] = tuple(int(x) for x in sh.split(','))
offset = 0
for nm in names:
    if nm == field:
        break
    n1, n2, n3 = shapes[nm]
    offset += n1 * n2 * n3
else:
    sys.exit(f"field {field} not in {meta}")
n1, n2, n3 = shapes[field]
assert 1 <= i <= n1 and 1 <= j <= n2 and 1 <= k <= n3, f"index outside {shapes[field]}"
offset += (i - 1) + n1 * ((j - 1) + n2 * (k - 1))
value = float('nan') if kind == 'nan' else float('inf')
size = binf.stat().st_size
with open(binf, 'r+b') as f:
    f.seek(8 * offset)
    old = struct.unpack('<d', f.read(8))[0]
    f.seek(8 * offset)
    f.write(struct.pack('<d', value))
assert binf.stat().st_size == size
print(f"{binf.name}: {field}({i},{j},{k}) {old:.6e} -> {value}")
