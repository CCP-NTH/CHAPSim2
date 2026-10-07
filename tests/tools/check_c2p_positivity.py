#!/usr/bin/env python3
"""Standalone reproduction of the cell-to-face interpolation used for diffusion
coefficients, to answer two questions without running the solver:

  (a) the wall defect -- what the wall face value of a near-wall-vanishing
      coefficient becomes when the Dirichlet fbc is supplied versus when it is
      omitted and the side is silently demoted to IBC_INTRPL
      (reduce_bc_to_interp, src/basics_operations2.f90);

  (b) positivity -- whether the compact C2P operator itself can return a
      negative face value from a strictly non-negative cell-centred input, and
      how large that undershoot can be relative to the peak of the input.

The operator here is transcribed from src/basics_operations2.f90:
  * interior stencil and the alpha family           ~line 1172 (IACCU_* select)
  * IBC_PERIODIC / IBC_INTERIOR rows                ~line 1205, 1241
  * IBC_INTRPL rows 1,2 (alpha1, alpha2)            ~line 1262, 1330
  * IBC_DIRICHLET rows 1,5 (identity) and 2:4       ~line 1394, 1411
  * RHS assembly and the bc_ghost_cd switches       ~line 2144
with bc_ghost_cd = .true., which is the shipped setting (line 35).

Run: python3 tests/tools/check_c2p_positivity.py
"""

import numpy as np

# ---------------------------------------------------------------------------
# operator coefficients, transcribed from basics_operations2.f90
# ---------------------------------------------------------------------------

def interior_coeffs(acc):
    """alpha, a, b for the C2P interior row (line ~1176)."""
    if acc == "cd2":
        alpha, a, b = 0.0, 1.0, 0.0
    elif acc == "cd4":
        alpha = 0.0
        a = 9.0 / 8.0 + 5.0 / 4.0 * alpha
        b = -1.0 / 8.0 + 3.0 / 4.0 * alpha
    elif acc == "cp4":
        alpha = 1.0 / 6.0
        a = 9.0 / 8.0 + 5.0 / 4.0 * alpha
        b = -1.0 / 8.0 + 3.0 / 4.0 * alpha
    elif acc == "cp6":
        alpha = 3.0 / 10.0
        a = 5.0 * (15.0 + 14.0 * alpha) / 64.0
        b = (-25.0 + 126.0 * alpha) / 128.0
    else:
        raise ValueError(acc)
    return alpha, a, b


def intrpl_row1(acc):
    """alpha1 and the six one-sided rhs weights (line ~1262)."""
    if acc == "cd2":
        alpha1 = 0.0
        w = [(3.0 + alpha1) / 2.0, (-1.0 + alpha1) / 2.0, 0, 0, 0, 0]
    elif acc in ("cd4", "cp4"):
        alpha1 = 0.0 if acc == "cd4" else 5.0
        w = [(35.0 + 5.0 * alpha1) / 16.0,
             (-35.0 + 15.0 * alpha1) / 16.0,
             (21.0 - 5.0 * alpha1) / 16.0,
             (-5.0 + alpha1) / 16.0, 0, 0]
    elif acc == "cp6":
        alpha1 = 9.0
        w = [(693.0 + 63.0 * alpha1) / 256.0,
             (-1155.0 + 315.0 * alpha1) / 256.0,
             (693.0 - 105.0 * alpha1) / 128.0,
             (-495.0 + 63.0 * alpha1) / 128.0,
             (385.0 - 45.0 * alpha1) / 256.0,
             (-63.0 + 7.0 * alpha1) / 256.0]
    else:
        raise ValueError(acc)
    return alpha1, w


def intrpl_row2(acc):
    """alpha2 and the six rhs weights for the second row (line ~1330)."""
    if acc == "cd2":
        alpha2 = 0.0
        w = [(1.0 + 2.0 * alpha2) / 2.0, (1.0 + 2.0 * alpha2) / 2.0, 0, 0, 0, 0]
    elif acc in ("cd4", "cp4"):
        alpha2 = 0.0 if acc == "cd4" else 1.0 / 6.0
        w = [(5.0 + 34.0 * alpha2) / 16.0,
             (15.0 - 26.0 * alpha2) / 16.0,
             (-5.0 + 30.0 * alpha2) / 16.0,
             (1.0 - 6.0 * alpha2) / 16.0, 0, 0]
    elif acc == "cp6":
        alpha2 = 9.0
        w = [(63.0 + 686.0 * alpha2) / 256.0,
             (315.0 - 1050.0 * alpha2) / 256.0,
             (-105.0 + 798.0 * alpha2) / 128.0,
             (63.0 - 530.0 * alpha2) / 128.0,
             (-45.0 + 406.0 * alpha2) / 256.0,
             (7.0 - 66.0 * alpha2) / 256.0]
    else:
        raise ValueError(acc)
    return alpha2, w


def c2p_matrices(nc, acc, bc, fbc=(0.0, 0.0)):
    """Return (A, apply_rhs) for np = nc+1 faces from nc cells.

    bc is 'dirichlet' (fbc supplied) or 'intrpl' (fbc omitted -> demoted).
    Both sides are treated identically, as in the cases of interest.
    """
    npf = nc + 1
    alpha, a, b = interior_coeffs(acc)
    A = np.zeros((npf, npf))

    # interior rows 3 .. np-2 (1-based) -> 2 .. np-3 (0-based)
    for i in range(2, npf - 2):
        A[i, i - 1] = alpha
        A[i, i] = 1.0
        A[i, i + 1] = alpha

    if bc == "dirichlet":
        # rows 1 and np are identity; rows 2 and np-1 take the INTERIOR row,
        # which for a non-corner row equals the PERIODIC row (alpha).
        A[0, 0] = 1.0
        A[npf - 1, npf - 1] = 1.0
        for i in (1, npf - 2):
            A[i, i - 1] = alpha
            A[i, i] = 1.0
            A[i, i + 1] = alpha
    elif bc == "intrpl":
        alpha1, _ = intrpl_row1(acc)
        alpha2, _ = intrpl_row2(acc)
        A[0, 0] = 1.0
        A[0, 1] = alpha1
        A[npf - 1, npf - 1] = 1.0
        A[npf - 1, npf - 2] = alpha1
        A[1, 0] = alpha2
        A[1, 1] = 1.0
        A[1, 2] = alpha2
        A[npf - 2, npf - 1] = alpha2
        A[npf - 2, npf - 2] = 1.0
        A[npf - 2, npf - 3] = alpha2
    else:
        raise ValueError(bc)

    def rhs(f):
        r = np.zeros(npf)
        for i in range(2, npf - 2):          # bulk, 0-based face index i
            r[i] = a / 2 * (f[i] + f[i - 1]) + b / 2 * (f[i + 1] + f[i - 2])
        if bc == "dirichlet":
            r[0] = fbc[0]
            r[npf - 1] = fbc[1]
            # rows 2 and np-1 use ghost cells; for cp4 b = 0 so they drop out,
            # but keep them general: Dirichlet ghost = 2*fbc - f(1).
            g0 = 2 * fbc[0] - f[0]
            g1 = 2 * fbc[1] - f[nc - 1]
            r[1] = a / 2 * (f[1] + f[0]) + b / 2 * (f[2] + g0)
            r[npf - 2] = a / 2 * (f[nc - 1] + f[nc - 2]) + b / 2 * (g1 + f[nc - 3])
        else:
            _, w1 = intrpl_row1(acc)
            _, w2 = intrpl_row2(acc)
            r[0] = sum(w1[k] * f[k] for k in range(6) if w1[k] != 0.0)
            r[npf - 1] = sum(w1[k] * f[nc - 1 - k] for k in range(6) if w1[k] != 0.0)
            r[1] = sum(w2[k] * f[k] for k in range(6) if w2[k] != 0.0)
            r[npf - 2] = sum(w2[k] * f[nc - 1 - k] for k in range(6) if w2[k] != 0.0)
        return r

    return A, rhs


def c2p(f, acc, bc, fbc=(0.0, 0.0)):
    A, rhs = c2p_matrices(len(f), acc, bc, fbc)
    return np.linalg.solve(A, rhs(f))


# ---------------------------------------------------------------------------
# the channel mesh of tests/functional/LES_channel_iso_peridic
# ---------------------------------------------------------------------------

def channel_yc(ncy=80, rstret=3.0, lyb=-1.0, lyt=1.0):
    """'twosides' stretching, mirroring geometry.f90's tanh mapping."""
    npy = ncy + 1
    s = np.linspace(0.0, 1.0, npy)
    yp = np.tanh(rstret * (s - 0.5)) / np.tanh(rstret * 0.5)
    yp = lyb + (yt_span := (lyt - lyb)) * (yp - yp[0]) / (yp[-1] - yp[0])
    yc = 0.5 * (yp[:-1] + yp[1:])
    return yp, yc


def banner(t):
    print()
    print("=" * 78)
    print(t)
    print("=" * 78)


def main():
    ncy = 80
    yp, yc = channel_yc(ncy)

    # a WALE-like coefficient: vanishes as y^3 at both walls, O(1) in the core
    d = 1.0 - np.abs(yc)
    nu = (d ** 3) / (d ** 3).max()

    banner("(a) WALL FACE of a y^3-vanishing coefficient, exact answer = 0")
    print(f"{'scheme':>7} {'with fbc=0 (Dirichlet)':>26} {'fbc omitted (INTRPL)':>24}")
    for acc in ("cd2", "cd4", "cp4", "cp6"):
        fd = c2p(nu, acc, "dirichlet", (0.0, 0.0))
        fi = c2p(nu, acc, "intrpl")
        print(f"{acc:>7} {fd[0]:>26.6e} {fi[0]:>24.6e}")

    banner("(a') the same for the momentum TOTAL viscosity mu = 1 + Re*nu_sgs")
    print("    exact wall answer = 1 (molecular only).  Re*nu_sgs peak = 2.63,")
    print("    chosen to match the measured peak of LES_channel_iso_peridic.")
    mu = 1.0 + 2.63 * nu
    print(f"{'scheme':>7} {'with fbc=1':>18} {'fbc omitted':>18} {'array min':>14} {'at face j':>10}")
    for acc in ("cd2", "cd4", "cp4", "cp6"):
        fd = c2p(mu, acc, "dirichlet", (1.0, 1.0))
        fi = c2p(mu, acc, "intrpl")
        print(f"{acc:>7} {fd[0]:>18.8f} {fi[0]:>18.8f} "
              f"{fi.min():>14.6f} {int(np.argmin(fi)) + 1:>10d}")

    banner("(b) POSITIVITY: worst undershoot over all non-negative inputs")
    print("    row-wise bound: min over f>=0 with max(f)=1 of (K f)_i equals the")
    print("    sum of the negative entries of row i of K = A^-1 R.  This is")
    print("    attained, so it is the exact operator bound, not an estimate.")
    print()
    print(f"{'scheme':>7} {'bc':>11} {'wall face j=1':>15} {'worst interior':>16} "
          f"{'at face j':>10} {'single-spike':>14}")
    for acc in ("cd2", "cd4", "cp4", "cp6"):
        for bc in ("dirichlet", "intrpl"):
            npf = ncy + 1
            A, rhs = c2p_matrices(ncy, acc, bc, (0.0, 0.0))
            K = np.zeros((npf, ncy))
            for j in range(ncy):
                e = np.zeros(ncy)
                e[j] = 1.0
                K[:, j] = np.linalg.solve(A, rhs(e))
            interior = slice(1, npf - 1)
            worst = np.minimum(K, 0.0).sum(axis=1)
            wi = int(np.argmin(worst[interior])) + 1
            # single interior spike, the physically reachable case
            spike = np.zeros(ncy)
            spike[ncy // 2] = 1.0
            sp = c2p(spike, acc, bc, (0.0, 0.0))
            print(f"{acc:>7} {bc:>11} {worst[0]:>15.6f} {worst[interior].min():>16.6f} "
                  f"{wi + 1:>10d} {sp.min():>14.6f}")

    banner("(b') undershoot on the realistic y^3 field, interior faces only")
    print(f"{'scheme':>7} {'bc':>11} {'min interior face':>19} {'at j':>6} "
          f"{'max(input)':>12} {'ratio':>9}")
    for acc in ("cd2", "cd4", "cp4", "cp6"):
        for bc in ("dirichlet", "intrpl"):
            f = c2p(nu, acc, bc, (0.0, 0.0))
            inner = f[1:-1]
            j = int(np.argmin(inner)) + 2
            print(f"{acc:>7} {bc:>11} {inner.min():>19.6e} {j:>6d} "
                  f"{nu.max():>12.4f} {inner.min() / nu.max():>9.5f}")

    banner("(b'') undershoot on a spiky but non-negative turbulence-like field")
    rng = np.random.default_rng(20261002)
    worst = {}
    for acc in ("cd2", "cd4", "cp4", "cp6"):
        for bc in ("dirichlet", "intrpl"):
            m = 0.0
            for _ in range(2000):
                f = np.abs(rng.standard_normal(ncy)) ** 3
                f *= (d ** 3) / (d ** 3).max()          # still vanishes at walls
                g = c2p(f, acc, bc, (0.0, 0.0))
                m = min(m, g[1:-1].min() / max(f.max(), 1e-30))
            worst[(acc, bc)] = m
    print(f"{'scheme':>7} {'bc':>11} {'worst min/max over 2000 draws':>31}")
    for k, v in worst.items():
        print(f"{k[0]:>7} {k[1]:>11} {v:>31.6f}")


if __name__ == "__main__":
    main()
