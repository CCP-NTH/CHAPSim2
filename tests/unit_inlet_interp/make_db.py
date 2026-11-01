#!/usr/bin/env python3
"""Write a synthetic analytic inlet database (CHAPSim_bundle_v2, bundled) for the inlet-interpolation test.

Plane p carries one azimuthal mode m_p (MODES, cycled; m = 0 is constant) on a non-zero mean that
varies with the radial index (the radial mesh of the database equals the main mesh, so the radial
interpolation is the identity and only theta matters):
  qx(j, th) = 1 + 0.02 j + a_p (1 + 0.01 j) cos(m th + phi_p)   theta cell centres th = h (k - 1/2)
  qy(j, th) = 0.2 a_p (0.01 j) sin(m th + phi_p)                theta cell centres, radial faces j = 0..ny
  qz(j, th) = 0.3 a_p (1 + 0.01 j) cos(m th + 2 phi_p)          theta faces th = h (k - 1)
  outlet2_* = 0.9 outlet1_*
usage: make_db.py <1_data dir> <ny> <nz> <ndbbuf> <lzz> <file offsets ...>
"""
import sys
import numpy as np

MODES = [0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 15]


def plane(p, ny, nz, lzz):
    m = MODES[p % len(MODES)]
    a = 0.3 * (1.0 + 0.04 * p)
    phi = 0.37 * p
    h = lzz / nz
    thc = (np.arange(nz) + 0.5) * h
    thf = np.arange(nz) * h
    jc = np.arange(ny)[None, :]
    jf = np.arange(ny + 1)[None, :]
    qx = 1 + 0.02 * jc + a * (1 + 0.01 * jc) * np.cos(m * thc[:, None] + phi)
    qy = 0.2 * a * (0.01 * jf) * np.sin(m * thc[:, None] + phi)
    qz = 0.3 * a * (1 + 0.01 * jc) * np.cos(m * thf[:, None] + 2 * phi)
    return qx, qy, qz                                    # [theta, radial]


def main():
    out, ny, nz, nb, lzz = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4]), float(sys.argv[5])
    for fidx, n in enumerate(int(a) for a in sys.argv[6:]):
        qx = np.zeros((nz, ny, nb)); qy = np.zeros((nz, ny + 1, nb)); qz = np.zeros((nz, ny, nb))
        for p in range(nb):
            qx[:, :, p], qy[:, :, p], qz[:, :, p] = plane(fidx * nb + p, ny, nz, lzz)
        with open(f'{out}/domain1_xoutlet_database_{n}.bin', 'wb') as f:
            for arr in (qx, 0.9 * qx, qy, 0.9 * qy, qz, 0.9 * qz):
                np.ascontiguousarray(arr).astype('<f8').tofile(f)
        meta = [
            'CHAPSim_bundle_v2', 'group xoutlet_database', f'iter {n}', f'iter_start {n + 1}', f'iter_end {n + nb}',
            'time 0.0', 'time_end 0.0', 'dt 5.0e-4', 'idom 1', 'icase 2', 'coordinate 2', 'layout bundled',
            'precision_bytes 8', 'endianness native', f'nc 32 {ny} {nz}', f'np 32 {ny + 1} {nz}',
            f'length 4.0 0.0 1.0 {lzz:.16e}', 'stretch 4 2 0.1', 'periodic T F T', f'ndbbuf {nb}',
            f'fields ndbbuf={nb};outlet1_qx:dxcc,outlet2_qx:dxcc,outlet1_qy:dxpc,outlet2_qy:dxpc,outlet1_qz:dxcp,outlet2_qz:dxcp',
            f'field_shapes outlet1_qx={nb},{ny},{nz};outlet2_qx={nb},{ny},{nz};outlet1_qy={nb},{ny + 1},{nz};'
            f'outlet2_qy={nb},{ny + 1},{nz};outlet1_qz={nb},{ny},{nz};outlet2_qz={nb},{ny},{nz}']
        open(f'{out}/domain1_xoutlet_database_meta_{n}.dat', 'w').write('\n'.join(meta) + '\n')


if __name__ == '__main__':
    main()
