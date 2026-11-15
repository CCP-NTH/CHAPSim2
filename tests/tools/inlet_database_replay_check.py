#!/usr/bin/env python3
"""Inlet-database replay check (tests/functional/inlet_database).

    inlet_database_replay_check.py RUN_DATA_DIR DB_DATA_DIR NDBFRE NDBSTART NDBEND OFFSET ITER...

For a run that reads a recorded x-outlet database on the same y-z mesh (is_record_xoutlet_read_xinlet
= .false., .true.), the inlet planes fbcx_q{x,y,z} (slots 1 and 3) of the flow-restart bundle at each
ITER must equal the database planes of the recorded step the
replay maps ITER to: recorded step r = NDBSTART + mod(ITER - 1, NDBEND - NDBSTART + 1), stored in
chunk file NDBFRE * ((r - NDBSTART) // NDBFRE) + OFFSET at record (r - NDBSTART) % NDBFRE.
Slot pairs: qx (x faces) slot 1 <- outlet1_qx, slot 3 <- outlet2_qx; qy, qz (x cell centres) slots
1 and 3 <- outlet1_q*.
The replay always passes the planes through the bilinear y-z interpolation of the inlet database
(interp_xinlet_database_array_yz), also on an identical mesh: qx comes out bit for bit, the staggered
qy and qz to interpolation round-off (observed <= 7e-8 of max|q|). Exit 0 if qx is bitwise and qy, qz
agree within 1e-6 of max|q| (a wrong record differs by O(1)).
"""
import sys
from pathlib import Path

import numpy as np


def bundle(binp):
    binp = Path(binp)
    stem, it = binp.stem.rsplit('_', 1)
    lines = (binp.parent / f"{stem}_meta_{it}.dat").read_text().splitlines()
    fields = [l for l in lines if l.startswith('fields ')][0].split(None, 1)[1]
    shapes = [l for l in lines if l.startswith('field_shapes ')][0].split(None, 1)[1]
    names = [f.split(':')[0] for f in fields.split(';')[-1].split(',')]
    shp = {k: tuple(int(v) for v in s.split(',')) for k, s in (i.split('=') for i in shapes.split(';'))}
    raw = np.fromfile(binp, dtype='<f8')
    out, off = {}, 0
    for n in names:
        k = int(np.prod(shp[n]))
        out[n] = raw[off:off + k].reshape(shp[n], order='F')
        off += k
    if off != raw.size:
        sys.exit(f"{binp}: metadata and size disagree")
    return out


run, db, fre, start, end, offset = sys.argv[1], sys.argv[2], *map(int, sys.argv[3:7])
ok = True
for it in map(int, sys.argv[7:]):
    rec_step = start + (it - 1) % (end - start + 1)
    chunk = fre * ((rec_step - start) // fre) + offset
    rec = (rec_step - start) % fre
    flow = bundle(Path(run) / f"domain1_flow_restart_{it}.bin")
    data = bundle(Path(db) / f"domain1_xoutlet_database_{chunk}.bin")
    for comp in ('qx', 'qy', 'qz'):
        pairs = ((0, 'outlet1_'), (2, 'outlet2_')) if comp == 'qx' else ((0, 'outlet1_'), (2, 'outlet1_'))
        for slot, plane in pairs:
            a = flow['fbcx_' + comp][slot]
            b = data[plane + comp][rec]
            same = a.shape == b.shape and np.array_equal(a.view(np.int64), b.view(np.int64))
            rel = np.max(np.abs(a - b)) / max(np.max(np.abs(b)), 1e-300) if a.shape == b.shape else np.inf
            ok &= same if comp == 'qx' else rel <= 1e-6
            print(f"step {it}: fbcx_{comp} slot {slot + 1} vs chunk {chunk} {plane}{comp} record {rec} "
                  f"(recorded step {rec_step}): {'bitwise' if same else f'rel {rel:.1e}'}")
sys.exit(0 if ok else 1)
