#!/usr/bin/env python3
"""Checks for the temporal inlet-interpolation test (run_test.sh). Reads the inlet planes fbcx_q*
(layers 1 and 3) from the flow-restart checkpoints written at every step and compares them with the
database samples. Step n advances t_{n-1} -> t_n and uses the inflow at t_{n-1}: m = n - 1,
sample q = m div R, alpha = mod(m, R)/R; across the replay wrap (q = last sample) no blend.
L1 ndb_sampling_dt = dt (R = 1) and no key: identical checkpoints (bitwise), every step
L2 R = 1: inlet plane at step n = sample n-1 (bitwise)
A  constant-in-time qy, R = 3: exact (bitwise) at every step
B  linear-in-time qx, R = 3: exact to round-off (<= 1e-13) at every step
S  R = 3, steps with alpha = 0: the stored sample unchanged (bitwise), all components
C  periodic-in-time qz, R = 3: error <= linear-interpolation bound alpha(1-alpha)/2 max|second difference|
E  identical physical times, R = 1 and R = 3: identical inlet planes (bitwise)
F  bundle boundary (sample 24 -> 25) and the step sequence: no missing or duplicated sample
W  replay wrap: last sample held for its R steps, then sample 0 (no blend)
H  two decompositions (R = 3): identical inlet planes (bitwise), every step
G  restarts (R = 3) at steps 40, 73, 75, 148: inlet planes and all flow fields at every later
   step identical (bitwise) to the uninterrupted run
V  validation: a non-integer ratio and a metadata-dt mismatch stop the run with an error
usage: check_inlet_tinterp.py <scratch dir> <R> <period>
"""
import os
import sys
import numpy as np

NAMES = ('fbcx_qx', 'fbcx_qy', 'fbcx_qz')
ok = True


def rep(name, cond, msg):
    global ok
    ok &= bool(cond)
    print(f'{"PASS" if cond else "FAIL"} {name}: {msg}', flush=True)


def read_meta(path):
    m = {}
    for line in open(path):
        s = line.split(None, 1)
        if len(s) == 2:
            m[s[0]] = s[1].strip()
    return m


def restart(d, it, names=NAMES):
    m = read_meta(f'{d}/1_data/domain1_flow_restart_meta_{it}.dat')
    order = [s.split(':')[0] for s in m['fields'].split(',')]
    shapes = {s.split('=')[0]: tuple(int(v) for v in s.split('=')[1].split(',')) for s in m['field_shapes'].split(';')}
    out, off = {}, 0
    for n in order:
        nx, ny, nz = shapes[n]
        if names is None or n in names:
            out[n] = np.fromfile(f'{d}/1_data/domain1_flow_restart_{it}.bin', dtype='<f8',
                                 count=nx * ny * nz, offset=off).reshape(nz, ny, nx)
        off += 8 * nx * ny * nz
    return out


def database(d):
    files = sorted(int(f.split('_')[-1].split('.')[0]) for f in os.listdir(f'{d}/1_data')
                   if f.startswith('domain1_xoutlet_database_meta_'))
    q = {n: [] for n in NAMES}
    for fn in files:
        m = read_meta(f'{d}/1_data/domain1_xoutlet_database_meta_{fn}.dat')
        nb = int(m['ndbbuf']); ny, nz = (int(v) for v in m['nc'].split()[1:])
        shapes = [(nz, ny, nb), (nz, ny, nb), (nz, ny + 1, nb), (nz, ny + 1, nb), (nz, ny, nb), (nz, ny, nb)]
        arrs, off = [], 0
        for s in shapes:
            arrs.append(np.fromfile(f'{d}/1_data/domain1_xoutlet_database_{fn}.bin', dtype='<f8',
                                    count=s[0] * s[1] * s[2], offset=off).reshape(s))
            off += 8 * s[0] * s[1] * s[2]
        for p in range(nb):
            q['fbcx_qx'].append((arrs[0][:, :, p], arrs[1][:, :, p]))
            q['fbcx_qy'].append((arrs[2][:, :, p], arrs[3][:, :, p]))
            q['fbcx_qz'].append((arrs[4][:, :, p], arrs[5][:, :, p]))
    return q, nb


def expected(db, n, R, ntot):
    m = n - 1
    qs, k = m // R, m % R
    qc = qs % ntot
    if k == 0 or qc == ntot - 1:
        return {c: db[c][qc] for c in NAMES}, qc, 0.0
    a = k / R
    return {c: tuple((1 - a) * db[c][qc][i] + a * db[c][qc + 1][i] for i in (0, 1)) for c in NAMES}, qc, a


def main():
    w, R, period = sys.argv[1], int(sys.argv[2]), float(sys.argv[3])
    db, nb = database(f'{w}/r3_a')
    ntot = len(db['fbcx_qx'])
    # L1, L2
    nsteps1 = len([f for f in os.listdir(f'{w}/ref1/1_data') if f.startswith('domain1_flow_restart_meta_')]) - 0
    same = all(open(f'{w}/ref1/1_data/domain1_flow_restart_{i}.bin', 'rb').read() ==
               open(f'{w}/nokey/1_data/domain1_flow_restart_{i}.bin', 'rb').read() for i in range(1, nsteps1 + 1))
    rep('L1 R = 1 key vs no key', same, f'{nsteps1} checkpoints byte-identical')
    e = 0.0
    for n in range(1, nsteps1 + 1):
        s = restart(f'{w}/ref1', n); qc = (n - 1) % ntot
        e = max(e, max(np.abs(s[c][:, :, i * 2] - db[c][qc][i]).max() for c in NAMES for i in (0, 1)))
    rep('L2 R = 1 plane = sample n-1', e == 0.0, f'max|diff| {e:.2e}')
    # R = 3 checks
    nsteps = len([f for f in os.listdir(f'{w}/r3_a/1_data') if f.startswith('domain1_flow_restart_meta_')])
    eA = eB = eS = 0.0; eC = 0.0; bC = 0.0; seq = []; held = []
    d2 = max(np.abs(db['fbcx_qz'][q + 1][0] - 2 * db['fbcx_qz'][q][0] + db['fbcx_qz'][q - 1][0]).max() for q in range(1, ntot - 1))
    for n in range(1, nsteps + 1):
        s = restart(f'{w}/r3_a', n)
        ex, qc, a = expected(db, n, R, ntot)
        seq.append(qc)
        for i in (0, 1):
            eA = max(eA, np.abs(s['fbcx_qy'][:, :, 2 * i] - ex['fbcx_qy'][i]).max())
            eB = max(eB, np.abs(s['fbcx_qx'][:, :, 2 * i] - ex['fbcx_qx'][i]).max())
            dz = np.abs(s['fbcx_qz'][:, :, 2 * i] - ex['fbcx_qz'][i]).max()
            if a == 0.0:
                eS = max(eS, max(np.abs(s[c][:, :, 2 * i] - db[c][qc][i]).max() for c in NAMES))
            eC = max(eC, dz)
        if a > 0:
            # true value of the smooth signal at the intermediate time vs reconstruction (qz, layer 1)
            th_true = np.cos(2 * np.pi * (qc + a) / period) / np.cos(2 * np.pi * qc / period) if abs(np.cos(2 * np.pi * qc / period)) > 0.2 else None
            if th_true is not None:
                true = db['fbcx_qz'][qc][0] * th_true
                bC = max(bC, np.abs(s['fbcx_qz'][:, :, 0] - true).max())
        if qc == ntot - 1:
            held.append((n, qc, a))
    rep('A constant in time (qy), R = 3', eA == 0.0, f'max|diff| {eA:.2e} (bitwise)')
    rep('B linear in time (qx), R = 3', eB <= 1e-13, f'max|diff| {eB:.2e} (tol 1e-13)')
    rep('S sample times reproduce the stored planes', eS == 0.0, f'max|diff| {eS:.2e} (bitwise)')
    bound = 0.125 * d2 * 1.0001
    rep('C periodic in time (qz): solver = linear interpolation of the samples', eC <= 1e-13, f'max|diff| {eC:.2e}')
    rep('C error vs the smooth signal within the linear-interpolation bound', bC <= bound,
        f'max error {bC:.3e} <= alpha(1-alpha)/2 max|d2| <= {bound:.3e}')
    exp_seq = [((n - 1) // R) % ntot for n in range(1, nsteps + 1)]
    rep('F sample sequence incl. bundle boundary 24 -> 25', seq == exp_seq and 24 in seq and 25 in seq,
        f'{nsteps} steps; samples {seq[0]}..{max(seq)}; each sample used for {R} consecutive steps')
    rep('W replay wrap: last sample held, then sample 0', len(held) == R and all(a == 0.0 for _, _, a in held)
        and nsteps > held[-1][0] and seq[held[-1][0]] == 0, f'held at steps {[h[0] for h in held]}, then sample {seq[held[-1][0]] if nsteps > held[-1][0] else None}')
    # E: identical physical times t = q dt_db: R = 1 step q+1, R = 3 step 3q+1
    eE = 0.0
    for q in range(0, min(nsteps1, (nsteps - 1) // R + 1)):
        a1 = restart(f'{w}/ref1', q + 1); a3 = restart(f'{w}/r3_a', R * q + 1)
        eE = max(eE, max(np.abs(a1[c][:, :, [0, 2]] - a3[c][:, :, [0, 2]]).max() for c in NAMES))
    rep('E same physical time, dt = dt_db vs dt_db/3', eE == 0.0, f'max|diff| {eE:.2e} (bitwise)')
    # H
    eH = 0.0
    for n in range(1, nsteps + 1):
        a = restart(f'{w}/r3_a', n); b = restart(f'{w}/r3_b', n)
        eH = max(eH, max(np.abs(a[c][:, :, [0, 2]] - b[c][:, :, [0, 2]]).max() for c in NAMES))
    rep('H decomposition independence (inlet planes)', eH == 0.0, f'max|diff| {eH:.2e} (bitwise)')
    # G
    for r0 in (40, 73, 75, 148):
        d = f'{w}/rst_{r0}'
        if not os.path.isdir(d):
            rep(f'G restart at {r0}', False, 'missing'); continue
        eI = eF = 0.0
        for n in range(r0 + 1, nsteps + 1):
            a = restart(f'{w}/r3_a', n, None); b = restart(d, n, None)
            eI = max(eI, max(np.abs(a[c][:, :, [0, 2]] - b[c][:, :, [0, 2]]).max() for c in NAMES))
            eF = max(eF, max(np.abs(a[k] - b[k]).max() for k in a))
        m = r0
        rep(f'G restart at step {r0} (m = {m}: sample {m // R}, alpha {m % R}/{R})', eI == 0.0 and eF == 0.0,
            f'inlet max|diff| {eI:.2e}, all flow fields max|diff| {eF:.2e} (bitwise, steps {r0 + 1}..{nsteps})')
    # V
    for name, pat in (('ratio', 'must be an integer'), ('meta', 'differs from the sampling interval')):
        log = open(f'{w}/bad_{name}/run.log', errors='replace').read() if os.path.exists(f'{w}/bad_{name}/run.log') else ''
        rep(f'V validation ({name})', pat in log and 'finished successfully' not in log, f'error message present: {pat in log}')
    print('ALL PASS' if ok else 'SOME CHECKS FAILED')
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
