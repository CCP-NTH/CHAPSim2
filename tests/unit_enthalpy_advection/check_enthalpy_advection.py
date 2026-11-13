#!/usr/bin/env python3
"""Checks for the enthalpy-advection unit driver (test_enthalpy_advection, built by build_test.sh). Runs the driver
in a scratch directory and prints each measured quantity with its criterion:
F1 face values = independent Koren formula psi(r) = max(0, min(2r, (1+2r)/3, 2)) with the division r = b/a
   (a = h_U - h_UU, b = h_D - h_U; a = 0 -> h_f = h_U, the r -> +-inf limit), upwind by sign(g), ghosts as
   buildup_ghost_cells_C; within 4 eps (|h_U| + |a| + |b|)
F2 every Koren face value lies between h_U and h_D (exact comparison, no tolerance)
F3 a = 0 or b = 0 (plateau, extremum, uniform data) gives exactly h_U; non-interior end faces keep the cd2/b.c. value
F4 comparison with the B5 replica limiter (which guards the division with eps = 1e-30): differences only where
   |a| <= 1e-30
LED semi-discrete local-extremum-diminishing rate dh/dt = (R_H - h R_rho)/rho at strict extrema (random signs of g)
B/G/H/I smooth convergence (periodic uniform; mapped y uniform and tanh-stretched, both flux signs; cylindrical annulus)
C/L/M sharp profiles: stage and full-step extrema against the initial range, CFL sweep (RK3 boundedness: asserted
      for CFL <= 0.5, the bound observed in all cases; larger CFL is reported, not asserted - the solver's RK3 is not SSP)
A/D/E constant h under variable density, conservation of rho*h, bounded variable-density step
usage: check_enthalpy_advection.py <driver> [scratch_dir]"""
import sys, os, subprocess, numpy as np
drv = os.path.abspath(sys.argv[1]); wd = sys.argv[2] if len(sys.argv) > 2 else '/tmp/b6_unit'
os.makedirs(wd, exist_ok=True); eps = np.finfo(float).eps
ok = True
def rep(name, cond, msg):
    global ok; ok &= bool(cond); print(f'{"PASS" if cond else "FAIL"} {name}: {msg}', flush=True)
def drive(*a):
    r = subprocess.run(['mpirun', '-np', '1', drv, *map(str, a)], cwd=wd, capture_output=True, text=True)
    if r.returncode != 0: raise RuntimeError(r.stderr[-2000:])
    return r.stdout
def run(scheme, case, n, cfl, tend, beta=0.0):
    out = [l for l in drive('run', scheme, case, n, cfl, tend, beta).splitlines() if l.startswith('RUN')][0].split()
    v = [float(x) for x in out[6:]]
    return dict(nst=int(out[5]), e1=v[0], e2=v[1], einf=v[2], smin=v[3], smax=v[4], fmin=v[5], fmax=v[6], cons=v[7], hcon=v[8])
IBC_PER, IBC_DIR, IBC_INT = 1, 4, 0
# ---------------------------------------------------------------- faces
drive('faces')
L = open(os.path.join(wd, 'faces.dat')).read().split('\n')
nrec = n_f1 = n_f2 = n_f3 = n_keep = n_rep = n_rep_eps = 0; worst = 0.0
def koren_psi(r): return max(0.0, min(2 * r, (1 + 2 * r) / 3, 2.0))
def koren_b5(r): return np.maximum(0.0, np.minimum(np.minimum(2 * r, (1 + 2 * r) / 3), 2.0))
i = 0
while i + 5 < len(L) and L[i].strip():
    ib1, ib2, npf = map(int, L[i].split()); fi = np.array(L[i+1].split(), float); fbc = np.array(L[i+2].split(), float)
    g = np.array(L[i+3].split(), float); fcd = np.array(L[i+4].split(), float); fo = np.array(L[i+5].split(), float); i += 6
    nc = fi.size; nrec += 1
    if ib1 == IBC_PER: gl = [fi[-2], fi[-1]]; gr = [fi[0], fi[1]]
    elif ib1 == IBC_DIR: gl = [2 * fbc[0] - fi[1], 2 * fbc[0] - fi[0]]; gr = [2 * fbc[1] - fi[-1], 2 * fbc[1] - fi[-2]]
    else: gl = [fbc[2], fbc[0]]; gr = [fbc[1], fbc[3]]
    e = np.concatenate([gl, fi, gr])               # e[k + 1] = cell k, k = -1 .. nc + 2
    E = lambda k: e[k + 1]
    for f in range(1, npf + 1):
        if ib1 != IBC_PER and ((f == 1 and ib1 != IBC_INT) or (f == npf and ib2 != IBC_INT)):
            n_keep += 1
            if fo[f-1] != fcd[f-1]: n_f3 += 1
            continue
        if g[f-1] >= 0: iu, iuu, idn = f - 1, f - 2, f
        else: iu, iuu, idn = f, f + 1, f - 1
        hu, huu, hd = E(iu), E(iuu), E(idn); a = hu - huu; b = hd - hu
        ref = hu if a == 0 else hu + 0.5 * koren_psi(b / a) * a
        tol = 4 * eps * (abs(hu) + abs(a) + abs(b))
        d = abs(fo[f-1] - ref); worst = max(worst, d / max(tol, 1e-320))
        if d > tol: n_f1 += 1
        if not (min(hu, hd) <= fo[f-1] <= max(hu, hd)): n_f2 += 1
        if (a == 0 or b == 0) and fo[f-1] != hu: n_f3 += 1
        rb5 = (hd - hu) / (a if abs(a) > 1e-30 else 1e-30)
        fb5 = hu + 0.5 * float(koren_b5(rb5)) * a       # as b5_replica.face_1d
        if abs(fb5 - fo[f-1]) > tol:
            n_rep += 1
            if abs(a) <= 1e-30: n_rep_eps += 1
rep('F1 formula', n_f1 == 0, f'{nrec} lines, max |diff|/tol = {worst:.3g}, mismatches {n_f1}')
rep('F2 boundedness', n_f2 == 0, f'face values outside [h_U, h_D]: {n_f2}')
rep('F3 plateau/extremum/kept faces', n_f3 == 0, f'violations {n_f3} ({n_keep} kept Dirichlet end faces checked)')
rep('F4 B5 replica', n_rep == n_rep_eps, f'differences {n_rep}, all with |a| <= 1e-30 (replica eps guard): {n_rep_eps}')
# ---------------------------------------------------------------- LED
drive('led')
D = np.loadtxt(os.path.join(wd, 'led.dat'))
hl, h, hr, r = D.T
mx = (h > hl) & (h > hr); mn = (h < hl) & (h < hr)
scale = np.abs(h).max()
vmax = max(r[mx].max(initial=-np.inf), (-r[mn]).max(initial=-np.inf))
rep('LED', vmax <= 1e-12 * scale * 64, f'{mx.sum()} strict maxima, {mn.sum()} minima; max wrong-sign rate {vmax:.3g}')
# ---------------------------------------------------------------- smooth convergence
def order(case, tend, beta, scheme, Ns=(32, 64, 128, 256, 512), cfl=0.4):
    R = [run(scheme, case, n, cfl, tend, beta) for n in Ns]
    o1 = [np.log2(R[k]['e1'] / R[k+1]['e1']) for k in range(len(R) - 1)]
    oi = [np.log2(R[k]['einf'] / R[k+1]['einf']) for k in range(len(R) - 1)]
    return R, o1, oi
print('# smooth convergence (CFL 0.4): N, L1, Linf; observed orders')
for case, tend, beta, lab in [('psine', 1.0, 0.0, 'G periodic uniform'), ('ysine', 0.3, 0.0, 'mapped y uniform m>0'),
                              ('ysine', 0.3, 2.0, 'H tanh beta=2 m>0'), ('ysineneg', 0.3, 2.0, 'F/H tanh beta=2 m<0'),
                              ('ysine', 0.3, 3.0, 'H tanh beta=3 m>0'), ('rsine', 0.3, 0.0, 'I cylindrical uniform'),
                              ('rsine', 0.3, 2.0, 'I cylindrical tanh beta=2')]:
    for scheme in ('cd2', 'koren'):
        R, o1, oi = order(case, tend, beta, scheme)
        print(f'  {lab:28s} {scheme:5s} L1 ' + ' '.join(f'{x["e1"]:.3e}' for x in R) + ' | orders L1 ' +
              ' '.join(f'{x:.2f}' for x in o1) + ' Linf ' + ' '.join(f'{x:.2f}' for x in oi))
        if scheme == 'koren':
            rep(f'B {lab} koren L1 order', o1[-1] >= 1.8, f'finest-pair L1 order {o1[-1]:.2f} (>= 1.8)')
            rep(f'D {lab} koren conservation', max(abs(x['cons']) for x in R) < 1e-12,
                f'max |rho*h drift incl. end fluxes| {max(abs(x["cons"]) for x in R):.2e}')
# ---------------------------------------------------------------- sharp profiles, CFL sweep, stage extrema
print('# sharp profiles: stage / full-step extrema vs initial range [0, 1], N = 128')
for case, tend, beta in [('pstep', 1.0, 0.0), ('ystep', 0.3, 2.0), ('ystepneg', 0.3, 2.0), ('rstep', 0.3, 2.0),
                         ('pvdstep', 1.0, 0.0)]:
    for cfl in (0.1, 0.4, 0.5, 0.6, 0.8, 1.0, 1.2, 1.4):
        line = []
        for scheme in ('cd2', 'koren'):
            x = run(scheme, case, 128, cfl, tend, beta)
            line.append(f'{scheme} stage [{x["smin"]:+.3e}, {x["smax"]-1:+.3e}] step [{x["fmin"]:+.3e}, {x["fmax"]-1:+.3e}]')
            if scheme == 'koren' and cfl <= 0.5:
                rep(f'C/L {case} CFL {cfl} koren stage extrema', x['smin'] >= -1e-14 and x['smax'] <= 1 + 1e-14,
                    f'undershoot {min(x["smin"], 0):.2e}, overshoot {max(x["smax"] - 1, 0):.2e}')
        print(f'  {case:9s} CFL {cfl:3.1f}: ' + ' | '.join(line))
print('# timestep sensitivity (M): koren psine N = 128, L1 error vs CFL')
for cfl in (0.05, 0.1, 0.2, 0.4, 0.8):
    x = run('koren', 'psine', 128, cfl, 1.0); print(f'  CFL {cfl:4.2f}: L1 {x["e1"]:.4e} Linf {x["einf"]:.4e}')
# ---------------------------------------------------------------- variable density
print('# variable density (E): g(x,t) = 1 + 0.5 sin(2 pi x) cos(2 pi t), rho_hat transported with the same g')
for scheme in ('cd2', 'koren'):
    c = run(scheme, 'pvdcon', 128, 0.4, 1.0)
    rep(f'A/E constant h ({scheme})', c['hcon'] <= 4 * eps, f'max |h - 1| over all stages {c["hcon"]:.2e}')
    s = run(scheme, 'pvdsin', 128, 0.4, 1.0)
    rep(f'D/E conservation pvdsin ({scheme})', abs(s['cons']) < 1e-13, f'rho*h drift {s["cons"]:.2e}; '
        f'stage range [{s["smin"]:+.6f}, {s["smax"]:+.6f}]')
    st = run(scheme, 'pvdstep', 128, 0.4, 1.0)
    print(f'  pvdstep {scheme}: stage range [{st["smin"]:+.3e}, {st["smax"]:.6f}], drift {st["cons"]:.2e}')
print('ALL PASS' if ok else 'SOME CHECKS FAILED')
sys.exit(0 if ok else 1)
