#!/usr/bin/env python3
"""Checks for the inlet-database interpolation test (run_test.sh). Reads the inlet boundary planes
fbcx_qx / fbcx_qy / fbcx_qz (layer 1 = database outlet1_*, layer 3 = outlet2_*) from the flow-restart
checkpoints the solver wrote at every step, and compares them with the database planes:
I1 periodic-linear theta interpolation (cell centres for qx, qy; faces for qz), radial identity:
   max|solver - reference| <= 1e-12 at every target point, the seam points theta ~ 0 and ~ 2 pi included
I2 theta-mean of qx, qy per radius preserved (cell-centred data, ratio 2): <= 1e-13
I3 the plane used at iteration n is database plane mod(n-1, ndbbuf)
D1 inlet planes identical (bitwise) between the two decompositions of the interpolating case
E1 matching mesh (direct read, no interpolation): inlet planes equal the database planes bitwise
G0-G2 halo/decomposition guard: pass message; valid uneven layout accepted (bitwise planes);
   incompatible layout stopped with a diagnostic before the first step completes
usage: check_inlet_interp.py <scratch dir>
"""
import os
import sys
import numpy as np


def read_meta(path):
    m = {}
    for line in open(path):
        s = line.split(None, 1)
        if len(s) == 2:
            m[s[0]] = s[1].strip()
    return m


def restart_fields(d, it, names):
    m = read_meta(f'{d}/1_data/domain1_flow_restart_meta_{it}.dat')
    order = [s.split(':')[0] for s in m['fields'].split(',')]
    shapes = {s.split('=')[0]: tuple(int(v) for v in s.split('=')[1].split(',')) for s in m['field_shapes'].split(';')}
    out, off = {}, 0
    f = f'{d}/1_data/domain1_flow_restart_{it}.bin'
    for n in order:
        nx, ny, nz = shapes[n]
        if n in names:
            out[n] = np.fromfile(f, dtype='<f8', count=nx * ny * nz, offset=off).reshape(nz, ny, nx)
        off += 8 * nx * ny * nz
    return out, float(m['length'].split()[3])


def db_fields(d, n):
    m = read_meta(f'{d}/1_data/domain1_xoutlet_database_meta_{n}.dat')
    nb = int(m['ndbbuf']); ny, nz = (int(v) for v in m['nc'].split()[1:])
    shapes = [(nz, ny, nb), (nz, ny, nb), (nz, ny + 1, nb), (nz, ny + 1, nb), (nz, ny, nb), (nz, ny, nb)]
    arrs, off = [], 0
    for s in shapes:
        arrs.append(np.fromfile(f'{d}/1_data/domain1_xoutlet_database_{n}.bin', dtype='<f8',
                                count=s[0] * s[1] * s[2], offset=off).reshape(s))
        off += 8 * s[0] * s[1] * s[2]
    return nb, {'fbcx_qx': (arrs[0], arrs[1], 1), 'fbcx_qy': (arrs[2], arrs[3], 1), 'fbcx_qz': (arrs[4], arrs[5], 2)}


def periodic_linear(f, nt_tgt, loc):
    nt = len(f)
    r = nt_tgt / nt
    pos = (np.arange(nt_tgt) + 0.5) / r - 0.5 if loc == 1 else np.arange(nt_tgt) / r
    i0 = np.floor(pos).astype(int)
    w = pos - i0
    return f[i0 % nt] * (1 - w) + f[(i0 + 1) % nt] * w


ok = True


def rep(name, cond, msg):
    global ok
    ok &= bool(cond)
    print(f'{"PASS" if cond else "FAIL"} {name}: {msg}', flush=True)


def main():
    w = sys.argv[1]
    names = ('fbcx_qx', 'fbcx_qy', 'fbcx_qz')
    nb, db = db_fields(f'{w}/interp_a', 0)
    e_i1 = e_i2 = 0.0; plane_ok = True; d1 = 0.0
    for it in range(1, nb + 1):
        sa, _ = restart_fields(f'{w}/interp_a', it, names)
        sb, _ = restart_fields(f'{w}/interp_b', it, names)
        p = (it - 1) % nb
        for n in names:
            l1, l2, loc = db[n]
            ntt = sa[n].shape[0]
            for layer, src in ((0, l1[:, :, p]), (2, l2[:, :, p])):
                ref = np.stack([periodic_linear(src[:, j], ntt, loc) for j in range(src.shape[1])], axis=1)
                e_i1 = max(e_i1, np.abs(sa[n][:, :, layer] - ref).max())
                if loc == 1:
                    e_i2 = max(e_i2, np.abs(sa[n][:, :, layer].mean(axis=0) - src.mean(axis=0)).max())
            d1 = max(d1, np.abs(sa[n] - sb[n])[:, :, [0, 2]].max())
        # plane identification: the solver plane must be closest to database plane p
        errs = [np.abs(sa['fbcx_qx'][:, :, 0] - np.stack([periodic_linear(db['fbcx_qx'][0][:, j, q], sa['fbcx_qx'].shape[0], 1)
                for j in range(db['fbcx_qx'][0].shape[1])], axis=1)).max() for q in range(nb)]
        plane_ok &= int(np.argmin(errs)) == p
    rep('I1 periodic-linear interpolation incl. seam', e_i1 <= 1e-12, f'max|solver - reference| {e_i1:.2e} (tol 1e-12)')
    rep('I2 theta-mean preserved (cell-centred)', e_i2 <= 1e-13, f'max|diff| {e_i2:.2e} (tol 1e-13)')
    rep('I3 plane mod(n-1, ndbbuf) used', plane_ok, f'{nb} iterations checked')
    rep('D1 decomposition independence (inlet planes)', d1 == 0.0, f'max|diff| {d1:.2e} (bitwise required)')
    e1 = 0.0
    for it in range(1, nb + 1):
        s, _ = restart_fields(f'{w}/identity', it, names)
        p = (it - 1) % nb
        for n in names:
            l1, l2, _ = db[n]
            e1 = max(e1, np.abs(s[n][:, :, 0] - l1[:, :, p]).max(), np.abs(s[n][:, :, 2] - l2[:, :, p]).max())
    rep('E1 matching mesh: direct read', e1 == 0.0, f'max|solver - database| {e1:.2e} (bitwise required)')
    # decomposition guard of the interpolation halo
    log_a = open(f'{w}/interp_a/run.log', errors='replace').read()
    rep('G0 guard reports a compatible layout', 'halo check passed' in log_a, 'pass message present in interp_a')
    log_ok = open(f'{w}/guard_ok/run.log', errors='replace').read()
    need3 = 'z-, z+): 0 0 1 3' in log_ok
    g1 = 0.0
    for it in range(1, nb + 1):
        sa, _ = restart_fields(f'{w}/interp_a', it, names)
        so, _ = restart_fields(f'{w}/guard_ok', it, names)
        for n in names:
            g1 = max(g1, np.abs(sa[n][:, :, [0, 2]] - so[n][:, :, [0, 2]]).max())
    rep('G1 uneven 1 x 12 layout (3 halo layers) accepted, planes identical to 1 x 2',
        need3 and g1 == 0.0 and 'finished successfully' in log_ok, f'3 layers reported: {need3}; max|diff| {g1:.2e} (bitwise)')
    log_bad = open(f'{w}/guard_bad/run.log', errors='replace').read()
    stopped = 'halo check failed' in log_bad and 'is incompatible with the source database cross-section 24 x 12' in log_bad
    rep('G2 incompatible 1 x 8 layout stopped with diagnostic', stopped and 'finished successfully' not in log_bad
        and 'Time Step = 2/' not in log_bad, f'diagnostic present: {stopped}')
    print('ALL PASS' if ok else 'SOME CHECKS FAILED')
    sys.exit(0 if ok else 1)


if __name__ == '__main__':
    main()
