#!/usr/bin/env python3
"""Does the discrete divergence telescope, i.e. is the scheme exactly conservative?

Companion to check_c2p_positivity.py.  A volume-integrated discrete divergence
should equal its net boundary flux,

    sum_j  w_j * (dF/dy)_j   ==   F(top) - F(bottom)                        (*)

with w_j the cell volume weight.  This script asks for which P2C first-
derivative operators (*) holds *exactly*, and if it does not, what the residual
is caused by and how it behaves under refinement.

It exists because a nonzero residual measured in the solver for cp4 is easy to
misread as a defect in the divergence operator.  The controlled test below
shows it is not: the residual comes entirely from the boundary closure, and it
converges away at the closure's own order.

Model problem.  The derivative is defined implicitly,

    A f' = B F        =>        f' = A^{-1} B F ,

A tridiagonal with interior off-diagonal alpha, B the face-difference stencil.
Two configurations are compared:

  periodic  - the interior stencil is used in every row, no boundary closure.
  bounded   - the two rows next to each boundary drop to the narrow stencil,
              which is what the code does so the stencil never reaches outside
              the face array.

In-solver cross-check.  The same quantity was measured inside the solver on
tests/functional/LES_channel_scp_inout_Tw (64x80x64, NP=4, RK3, 2 iterations),
with a temporary probe on the energy diffusion terms reporting

    I = sum_cells vol * div            div = the RHS contribution, post 1/r
    B = surface integral of F.n over the two faces normal to the direction
    (I - B) / sum_cells vol * |div|

Direction z is periodic here, y is wall bounded, x is inlet/outlet.

    scheme   term    dir              I               B      (I-B)/D
    cd2      mol-y     2   2.895402E-03    2.895402E-03    4.603E-16
    cd2      sgs-y     2  -3.808103E-29    0.000000E+00   -2.048E-17
    cd2      mol-z     3   1.151565E-31    0.000000E+00    8.456E-17
    cp4      mol-z     3  -2.674065E-29    0.000000E+00   -1.850E-19
    cp4      sgs-z     3   1.357365E-30    0.000000E+00    9.607E-17
    cp4      mol-y     2   3.123937E-03    3.136665E-03   -6.542E-04
    cp4      sgs-y     2  -7.525072E-11    0.000000E+00   -1.193E-03

cd2 is exact in every direction.  cp4 is exact to round-off in the periodic
direction and picks up a residual only in the wall-bounded one, at the same
relative size for the molecular and the subgrid term - i.e. the solver
reproduces the pattern below, and the subgrid flux is not anomalous.  B = 0
exactly for sgs-y is the assembled wall subgrid heat flux, which is the
separate wall-consistency result.

Run:  python3 tests/tools/check_p2c_telescoping.py
"""

import numpy as np

# ---------------------------------------------------------------------------
# P2C first-derivative interior coefficients (alpha, a, b), transcribed from
# src/basics_operations2.f90, Prepare_coeffs_for_operations (d1fP2C / d1rP2C):
#   alpha f'_{j-1} + f'_j + alpha f'_{j+1}
#       = a (F_{j+1/2} - F_{j-1/2})/h  +  b (F_{j+3/2} - F_{j-3/2})/(3h)
# ---------------------------------------------------------------------------
INTERIOR = {
    "cd2": (0.0, 1.0, 0.0),
    "cd4": (0.0, 9.0 / 8.0, -1.0 / 8.0),
    "cp4": (1.0 / 22.0, 12.0 / 11.0, 0.0),
    "cp6": (9.0 / 62.0, 63.0 / 62.0, 17.0 / 62.0),
}


def operators(nc, h, acc, periodic):
    """Return (A, K) with f' = K F; F has nc faces if periodic, nc+1 if bounded."""
    alpha, a, b = INTERIOR[acc]
    nf = nc if periodic else nc + 1
    A = np.eye(nc)
    B = np.zeros((nc, nf))

    def face(j):
        return j % nc if periodic else j

    for j in range(nc):
        narrow = (not periodic) and (j < 1 or j > nc - 2)
        if narrow:
            # boundary closure: narrow stencil, no coupling to the neighbouring
            # derivative, so this row is explicit
            B[j, j + 1] += 1.0 / h
            B[j, j] -= 1.0 / h
            continue
        A[j, face(j - 1)] += alpha
        A[j, face(j + 1)] += alpha
        B[j, face(j + 1)] += a / h
        B[j, face(j)] -= a / h
        if b != 0.0:
            B[j, face(j + 2)] += b / (3.0 * h)
            B[j, face(j - 1)] -= b / (3.0 * h)
    return A, np.linalg.solve(A, B)


def residual(nc, acc, periodic):
    h = 1.0 / nc
    A, K = operators(nc, h, acc, periodic)
    w = np.full(nc, h)
    y = np.arange(nc + 1) * h if not periodic else np.arange(nc) * h
    F = np.cos(3.0 * np.pi * y) + 0.3 * np.sin(5.0 * np.pi * y)
    net = 0.0 if periodic else F[-1] - F[0]
    d = K @ F
    scale = max(abs(net), np.sum(w * np.abs(d)))
    return (w @ d - net) / scale


print(__doc__.split("Run:")[0])

for periodic in (True, False):
    name = "periodic (no boundary closure)" if periodic else "bounded (code's narrow closure)"
    print(f"--- {name} ---")
    print(f"{'scheme':>7}" + "".join(f"{n:>13}" for n in (16, 32, 64, 128, 256)) + f"{'order':>8}")
    for acc in INTERIOR:
        r = [residual(n, acc, periodic) for n in (16, 32, 64, 128, 256)]
        if abs(r[-1]) < 1e-13 or abs(r[-2]) < 1e-13:
            order = "exact"
        else:
            order = f"{np.log2(abs(r[-2] / r[-1])):.2f}"
        print(f"{acc:>7}" + "".join(f"{v:13.3e}" for v in r) + f"{order:>8}")
    print()

print("Reading:")
print("  Periodic: every scheme telescopes to round-off.  With no boundary row,")
print("  A has constant row sums and B is a pure face difference, so the identity")
print("  (*) is exact for cd2, cd4, cp4 and cp6 alike.  The compact inverse does")
print("  not break conservation by itself.")
print()
print("  Bounded: cd2 alone is still exact - its interior stencil IS the")
print("  telescoping difference and A = I, so the narrow boundary row is the same")
print("  row.  cd4/cp4/cp6 pick up a residual purely from the narrow closure, and")
print("  it converges at second order, the order of that closure.  It is a")
print("  truncation error, not a conservation defect, and it is not grounds for")
print("  changing the divergence operator.")
