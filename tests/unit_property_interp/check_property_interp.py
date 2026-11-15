#!/usr/bin/env python3
"""Checks for the property-interpolation unit driver output (run in the directory holding
nodes_/dense_/trip_<fluid>_<mode>.dat). Prints measured quantities and the criterion used for each:
A  node reproduction (exact) and agreement with an independent PCHIP (scipy) within an evaluation
   round-off bound 64*eps*(|y1|+|dh m1|+|y2|+|dh m2|);
B  derivative continuity at nodes: cp = 1/(dT/dh) and one-sided finite-difference slopes of rho on both
   sides of every node at two offsets (C1 => jump shrinks with the offset; C0 => it does not);
C  monotonicity (T increasing, rho decreasing in h), positivity (mu, k, sigma_e, cp) and boundedness
   (each value within its interval's node values);
D  end nodes reproduced, cp finite and positive there;
F  round trips h->T->h and T->h->T, and the Bc path from_DH(rho_hat h, rho_hat) = from_H(h);
G  cp = 1/(dT/dh) vs the tabulated cp column, compared with the table's own internal consistency
   (secant dh/dT of the h,T columns vs the cp column), by temperature band;
H  conditioning near the pseudo-critical point (min dT/dh, max cp, cp continuity).
usage: check_property_interp.py <fluid> <T0_K>"""
import sys, numpy as np
from scipy.interpolate import PchipInterpolator
fl, T0 = sys.argv[1], float(sys.argv[2]); eps = np.finfo(float).eps
C = ['h', 'T', 'rho', 'mu', 'k', 'sig', 'beta', 'cp']
nod = {m: np.loadtxt(f'nodes_{fl}_{m}.dat') for m in ('linear', 'pchip')}
den = {m: np.loadtxt(f'dense_{fl}_{m}.dat') for m in ('linear', 'pchip')}
trp = {m: np.loadtxt(f'trip_{fl}_{m}.dat') for m in ('linear', 'pchip')}
N = nod['pchip']; h = N[:, 0]; n = len(h)
ok = True
def rep(name, cond, msg):
    global ok; ok &= bool(cond); print(f'{"PASS" if cond else "FAIL"} {name}: {msg}')
print(f'# {fl}: {n} nodes, h {h[0]:.4f}..{h[-1]:.4f}, T {N[0,1]*T0:.2f}..{N[-1,1]*T0:.2f} K')
# dense layout: per interval i: 64 regular points (s = j/64), then for i>1 the 4 node-offset points
reg, off = [], []
r = 0
for i in range(n - 1):
    reg.append(r + np.arange(64)); r += 64
    if i > 0: off.append((i, r + np.arange(4))); r += 4
reg = np.concatenate(reg)
for m in ('linear', 'pchip'):
    D = den[m]; Dr = D[reg]
    s0 = Dr[::64]                                       # s = 0 rows = nodes 1..n-1
    exact = np.all(s0[:, 1:7] == N[:-1, 1:7]) and np.all(D[-1, 1:7] == N[-1, 1:7])
    rep(f'A1[{m}] node reproduction', exact, 'T, rho, mu, k, sigma_e, beta at all nodes bit-identical to the table')
# A2: independent PCHIP
P = den['pchip'][reg]; ii = np.repeat(np.arange(n - 1), 64)
worst = 0.0
for c in range(1, 7):
    ref = PchipInterpolator(h, N[:, c])(P[:, 0]); m_ = PchipInterpolator(h, N[:, c]).derivative()(h)
    dh = np.diff(h)[ii]
    bound = 64 * eps * (abs(N[ii, c]) + abs(dh * m_[ii]) + abs(N[ii + 1, c]) + abs(dh * m_[ii + 1])) + 1e-300
    worst = max(worst, np.max(abs(P[:, c] - ref) / bound))
rep('A2[pchip] agreement with scipy PchipInterpolator', worst <= 1.0, f'max |diff|/bound = {worst:.3f} (bound = 64 eps x sum of Hermite terms)')
# B: derivative continuity. One-sided FD slopes at offset d (fraction of the two-interval width) on both
# sides of every node. C0 (slope jump J at the node): FD jump -> J, independent of d. C1: FD jump = O(d * |y''|)
# (+ round-off O(eps/d), negligible at these offsets), so J(1e-5)/J(1e-3) -> 1e-2. Criterion: median ratio
# <= 0.05 (allows a 5x round-off/curvature-variation contribution) and cp (analytic 1/T_h) continuous to O(d).
for m in ('linear', 'pchip'):
    D = den[m]; jr = {1e-5: [], 1e-3: []}; cpj = []
    for i, rows in off:
        hp5, hp3, hm5, hm3 = D[rows]
        node = N[i]
        sec = abs(N[i + 1, 2] - N[i - 1, 2]) / (h[i + 1] - h[i - 1])
        for dlt, a, b in ((1e-5, hp5, hm5), (1e-3, hp3, hm3)):
            dR = (a[2] - node[2]) / (a[0] - node[0]); dL = (node[2] - b[2]) / (node[0] - b[0])
            jr[dlt].append(abs(dR - dL) / sec)
        cpj.append(abs(hp5[7] - hm5[7]) / abs(node[7]))
    j5, j3 = np.array(jr[1e-5]), np.array(jr[1e-3])
    ratio = np.median(j5 / np.maximum(j3, 1e-300))
    print(f'     B[{m}] rho one-sided slope jump / secant: median {np.median(j5):.2e} (offset 1e-5), {np.median(j3):.2e} (1e-3);'
          f' max {j5.max():.2e} / {j3.max():.2e}; median ratio J(1e-5)/J(1e-3) {ratio:.3f}; cp jump across nodes (offset 1e-5) median {np.median(cpj):.2e}, max {np.max(cpj):.2e}')
    if m == 'pchip':
        rep('B[pchip] C1 in h', ratio <= 0.05, f'rho slope jump scales with the offset (median ratio {ratio:.3f}; C1 theory 0.01, C0 = 1)')
# C: monotonicity, positivity, boundedness
for m in ('linear', 'pchip'):
    D = den[m]; Dr = D[reg]
    mono_T = np.all(np.diff(Dr[:, 1]) > 0); mono_r = np.all(np.diff(Dr[:, 2]) < 0)
    pos = np.all(Dr[:, [3, 4, 5, 7]] > 0)
    lo = np.minimum(N[ii, 1:7], N[ii + 1, 1:7]); hi = np.maximum(N[ii, 1:7], N[ii + 1, 1:7])
    tol = 4 * eps * np.maximum(abs(lo), abs(hi))
    bnd = np.all((Dr[:, 1:7] >= lo - tol) & (Dr[:, 1:7] <= hi + tol))
    rep(f'C[{m}] monotone/positive/bounded', mono_T and mono_r and pos and bnd,
        f'T(h) increasing {mono_T}, rho(h) decreasing {mono_r}, mu,k,sigma_e,cp > 0 {pos}, within interval node values (4 ulp) {bnd}')
# D: endpoints
for m in ('linear', 'pchip'):
    D = den[m]; e1 = D[reg][0]; e2 = D[-1]
    rep(f'D[{m}] end nodes', np.all(e1[1:7] == N[0, 1:7]) and np.all(e2[1:7] == N[-1, 1:7]) and np.all(np.isfinite([e1[7], e2[7]])) and e1[7] > 0 and e2[7] > 0,
        f'exact end values; cp at ends {e1[7]:.5g}, {e2[7]:.5g}')
# F: round trips and Bc path
for m in ('linear', 'pchip'):
    R = trp[m]; hh, Th, hb, cp, Tt, hT, Tb, dh_bc, dp_bc = R.T
    scale = np.abs(hh) + np.abs(Th) * cp                # conditioning: dh = cp dT
    e_h = np.max(abs(hb - hh) / (eps * scale)); e_T = np.max(abs(Tb - Tt) / (eps * abs(Tt)))
    print(f'     F[{m}] max |h(T(h))-h| = {np.max(abs(hb-hh)):.2e} = {e_h:.1f} eps*(|h|+|T| cp); max |T(h(T))-T| = {e_T:.1f} eps|T|;'
          f' Bc path: max |dh| {dh_bc.max():.1e}, max |d prop| {dp_bc.max():.1e}')
    if m == 'pchip':
        rep('F[pchip] inverse consistency', e_h <= 16 and e_T <= 16,
            'both round trips within 16 eps of the conditioning-scaled magnitude (a few rounded operations, amplified by dh/dT = cp)')
# G: cp consistency
Ttab = N[:, 1] * T0; cpt = N[:, 7]; sec = np.diff(h) / np.diff(N[:, 1])   # table secant dh/dT (non-dim) per interval
Dp = den['pchip'][reg]; Tp = Dp[:, 1] * T0; cpp = Dp[:, 7]
cpcol_lin = np.interp(Dp[:, 1], N[:, 1], cpt)                              # tabulated cp at the same states (linear)
print('     G  band(K)        | cp_pchip vs cp column: max, rms rel | table-internal: secant dh/dT vs mean of cp column at ends: max, rms rel')
for lo_, hi_ in [(Ttab[0], 300), (300, 304), (304, 306.5), (306.5, 309.5), (309.5, 320), (320, Ttab[-1])] if fl == 'co2' else \
               [(Ttab[0], 640), (640, 650), (650, 656), (656, 665), (665, Ttab[-1])]:
    w = (Tp >= lo_) & (Tp < hi_); wi = (Ttab[:-1] >= lo_) & (Ttab[:-1] < hi_)
    rp = cpp[w] / cpcol_lin[w] - 1; rt = sec[wi] / (0.5 * (cpt[:-1][wi] + cpt[1:][wi])) - 1
    print(f'     G  {lo_:7.2f}-{hi_:7.2f} | {np.max(abs(rp)):.3e} {np.sqrt(np.mean(rp**2)):.2e}            | {np.max(abs(rt)):.3e} {np.sqrt(np.mean(rt**2)):.2e}')
# H: conditioning
dTdh_min = 1 / cpp.max()
print(f'     H  pchip: min dT/dh {dTdh_min:.4e} (cp max {cpp.max():.4f} at T {Tp[np.argmax(cpp)]:.2f} K; tabulated cp max {cpt.max():.4f} at {Ttab[np.argmax(cpt)]:.2f} K); cp min {cpp.min():.4f}')
rep('H[pchip] cp finite/positive everywhere', np.all(np.isfinite(cpp)) and np.all(cpp > 0), 'all dense samples')
print('ALL PASS' if ok else 'SOME CHECKS FAILED')
