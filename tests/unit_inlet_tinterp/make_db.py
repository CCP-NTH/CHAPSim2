#!/usr/bin/env python3
"""Write a synthetic time-dependent inlet database (CHAPSim_bundle_v2, bundled) for the temporal
inlet-interpolation test. Sample q (0-based, database time q*dt_db) of each component:
  qy(j, k, q) = 0.1 (1 + 0.01 j) sin(th_k)                               constant in time   (TEST A)
  qx(j, k, q) = 1 + 0.02 j + 0.05 cos(th_k) + (0.01 + 0.001 j) q          linear in time     (TEST B)
  qz(j, k, q) = 0.3 (1 + 0.01 j) cos(th_k) cos(2 pi q / P)                smooth periodic    (TEST C)
outlet2_* = 0.9 outlet1_*. Mesh: the main mesh (radial index j, theta cell centres / faces).
usage: make_db.py <1_data dir> <ny> <nz> <ndbbuf> <nfiles> <dt_db> <lzz> <period P in samples>
"""
import sys
import numpy as np


def sample(q, ny, nz, lzz, period):
    h = lzz / nz
    thc = (np.arange(nz) + 0.5) * h
    thf = np.arange(nz) * h
    jc = np.arange(ny)[None, :]
    jf = np.arange(ny + 1)[None, :]
    qx = 1 + 0.02 * jc + 0.05 * np.cos(thc[:, None]) + (0.01 + 0.001 * jc) * q
    qy = 0.1 * (1 + 0.01 * jf) * np.sin(thc[:, None]) + 0 * q
    qz = 0.3 * (1 + 0.01 * jc) * np.cos(thf[:, None]) * np.cos(2 * np.pi * q / period)
    return qx, qy, qz                                      # [theta, radial]


def main():
    out, ny, nz, nb, nfiles = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4]), int(sys.argv[5])
    dt_db, lzz, period = float(sys.argv[6]), float(sys.argv[7]), float(sys.argv[8])
    for fidx in range(nfiles):
        n = fidx * nb                                      # file offset 0 in the test input
        qx = np.zeros((nz, ny, nb)); qy = np.zeros((nz, ny + 1, nb)); qz = np.zeros((nz, ny, nb))
        for p in range(nb):
            qx[:, :, p], qy[:, :, p], qz[:, :, p] = sample(n + p, ny, nz, lzz, period)
        with open(f'{out}/domain1_xoutlet_database_{n}.bin', 'wb') as f:
            for arr in (qx, 0.9 * qx, qy, 0.9 * qy, qz, 0.9 * qz):
                np.ascontiguousarray(arr).astype('<f8').tofile(f)
        meta = [
            'CHAPSim_bundle_v2', 'group xoutlet_database', f'iter {n}', f'iter_start {n + 1}', f'iter_end {n + nb}',
            f'time {n * dt_db:.16e}', f'time_end {(n + nb - 1) * dt_db:.16e}', f'dt {dt_db:.16e}', 'idom 1',
            'icase 2', 'coordinate 2', 'layout bundled', 'precision_bytes 8', 'endianness native',
            f'nc 26 {ny} {nz}', f'np 26 {ny + 1} {nz}', f'length 1.0 0.0 1.0 {lzz:.16e}', 'stretch 4 2 0.1',
            'periodic T F T', f'ndbbuf {nb}',
            f'fields ndbbuf={nb};outlet1_qx:dxcc,outlet2_qx:dxcc,outlet1_qy:dxpc,outlet2_qy:dxpc,outlet1_qz:dxcp,outlet2_qz:dxcp',
            f'field_shapes outlet1_qx={nb},{ny},{nz};outlet2_qx={nb},{ny},{nz};outlet1_qy={nb},{ny + 1},{nz};'
            f'outlet2_qy={nb},{ny + 1},{nz};outlet1_qz={nb},{ny},{nz};outlet2_qz={nb},{ny},{nz}']
        open(f'{out}/domain1_xoutlet_database_meta_{n}.dat', 'w').write('\n'.join(meta) + '\n')


if __name__ == '__main__':
    main()
