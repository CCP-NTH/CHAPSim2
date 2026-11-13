#!/usr/bin/env python3
"""Read-only cross-check of a statistical window on large production bundles.

For data too large to copy or to post-process locally (e.g. a 512x384x384
channel, ~32 GB of statistics per snapshot), this checks a window (a, b] of
the bundled statistics directly where they live, streaming z-slabs:

  1. manifests at a and b: format, group, iteration, identical signature and
     field list, explicit nsamples / stat_istart, N_b - N_a = b - a, and the
     bundle size equal to n_fields * nc1*nc2*nc3 * 8;
  2. for the selected fields, the window field
         A_ab = A_b + N_a/(N_b - N_a) (A_b - A_a)
     pointwise (the formula of statistics_mod / stats_window_mean), its x-z
     plane average, and - for diagonal second moments - the realisability of
     <ff> - <f>^2 with the solver's tolerance 16 kappa N_b eps <ff>;
  3. the plane-averaged window field against the window rebuilt from the
     solver's own y-profiles (2_visu/data/domain1_tsp_avg_<group>_yprofile_<it>.dat)
     at a and b, relative to max|A_ab| of the 3-D field.

Nothing is written. Usage (2-periodic x-z channel, bundled layout):
    stat_window_profile_crosscheck.py <case_dir> <a> <b> --group flow_stats \
        --fields t_avg_u1,t_avg_uu11 [--var t_avg_uu11:t_avg_u1] [--nz-chunk 16]
"""

import argparse
import os
import sys

import numpy as np


def manifest(path):
    meta = {}
    with open(path) as fh:
        for line in fh:
            parts = line.strip().split(None, 1)
            if parts:
                meta[parts[0]] = parts[1] if len(parts) > 1 else ""
    return meta


def read_profile(path):
    names = None
    with open(path) as fh:
        for line in fh:
            if line.startswith("# columns:"):
                names = line.split(":", 1)[1].split()
    return names, np.atleast_2d(np.loadtxt(path, comments="#"))


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("case_dir")
    p.add_argument("a", type=int)
    p.add_argument("b", type=int)
    p.add_argument("--group", default="flow_stats")
    p.add_argument("--fields", required=True)
    p.add_argument("--var", default="", help="comma list ff:f of diagonal second moments to check")
    p.add_argument("--nz-chunk", type=int, default=16)
    p.add_argument("--domain", type=int, default=1)
    args = p.parse_args()

    data = os.path.join(args.case_dir, "1_data")
    stem = os.path.join(data, "domain%d_%s" % (args.domain, args.group))
    ma, mb = manifest("%s_meta_%d.dat" % (stem, args.a)), manifest("%s_meta_%d.dat" % (stem, args.b))
    fr = manifest(os.path.join(data, "domain%d_flow_restart_meta_%d.dat" % (args.domain, args.b)))
    nx, ny, nz = (int(v) for v in fr["nc"].split())
    ok = True
    for key in ("signature", "fields", "stat_istart"):
        if ma.get(key) != mb.get(key):
            print("MISMATCH %s between a and b" % key)
            ok = False
    if "nsamples" not in ma or "nsamples" not in mb:
        print("NO SAMPLE COUNT")
        return 1
    na, nb, istart = int(ma["nsamples"]), int(mb["nsamples"]), int(mb["stat_istart"])
    if nb - na != args.b - args.a:
        print("COUNT GAP: N_b - N_a = %d, b - a = %d" % (nb - na, args.b - args.a))
        ok = False
    names = [e.split(":", 1)[0] for e in mb["fields"].split(",") if e]
    ncell = nx * ny * nz
    for it in (args.a, args.b):
        size = os.path.getsize("%s_%d.bin" % (stem, it))
        if size != len(names) * ncell * 8:
            print("SIZE MISMATCH at %d: %d bytes, expected %d" % (it, size, len(names) * ncell * 8))
            ok = False
    kappa = (na + nb) / (nb - na)
    print("%s window (%d,%d]: stat_istart %d, N_a %d, N_b %d, N_window %d, kappa %.4g, %d fields, mesh %dx%dx%d, "
          "manifests consistent: %s" % (args.group, args.a, args.b, istart, na, nb, nb - na, kappa,
                                       len(names), nx, ny, nz, ok))

    mapa = np.memmap("%s_%d.bin" % (stem, args.a), dtype="<f8", mode="r")
    mapb = np.memmap("%s_%d.bin" % (stem, args.b), dtype="<f8", mode="r")
    sel = args.fields.split(",")
    var_pairs = [tuple(v.split(":")) for v in args.var.split(",") if v]
    need = sorted(set(sel) | {f for pair in var_pairs for f in pair})
    for f in need:
        if f not in names:
            print("FIELD %s not in the bundle" % f)
            return 1
    w = na / (nb - na)
    prof = {f: np.zeros(ny) for f in need}
    fmax = {f: 0.0 for f in need}
    tol_fac = 16.0 * kappa * nb * np.finfo(np.float64).eps
    vstat = {pair: [0, 0, np.inf] for pair in var_pairs}   # round-off negatives, invalid, min var/<ff>
    for k0 in range(0, nz, args.nz_chunk):
        k1 = min(nz, k0 + args.nz_chunk)
        win = {}
        for f in need:
            off = names.index(f) * ncell
            sl = slice(off + k0 * nx * ny, off + k1 * nx * ny)
            A_a = np.asarray(mapa[sl]).reshape(k1 - k0, ny, nx)
            A_b = np.asarray(mapb[sl]).reshape(k1 - k0, ny, nx)
            win[f] = A_b + w * (A_b - A_a)
            prof[f] += win[f].sum(axis=(0, 2))
            fmax[f] = max(fmax[f], float(np.max(np.abs(win[f]))))
        for (ff, f1) in var_pairs:
            var = win[ff] - win[f1] ** 2
            tau = tol_fac * np.abs(win[ff])
            vstat[(ff, f1)][0] += int(np.count_nonzero((var < 0) & (var >= -tau)))
            vstat[(ff, f1)][1] += int(np.count_nonzero(var < -tau))
            vstat[(ff, f1)][2] = min(vstat[(ff, f1)][2], float(np.min(var / np.maximum(np.abs(win[ff]), 1e-300))))
    for f in need:
        prof[f] /= nx * nz

    tsp = os.path.join(args.case_dir, "2_visu", "data", "domain%d_tsp_avg_%s_yprofile_%%d.dat"
                       % (args.domain, args.group.replace("_stats", "")))
    pn, pa = read_profile(tsp % args.a)
    _, pb = read_profile(tsp % args.b)
    for f in sel:
        col = pn.index(f.replace("t_avg_", "tsp_avg_", 1))
        ref = pb[:, col] + w * (pb[:, col] - pa[:, col])
        err = float(np.max(np.abs(prof[f] - ref)) / max(fmax[f], 1e-300))
        good = err <= 1e-10
        ok &= good
        print("  %-14s plane-averaged 3-D window vs window of the solver's y-profiles: rel %.3e  %s"
              % (f, err, "ok" if good else "FAIL"))
    for (ff, f1), (nr, ninv, vmin) in vstat.items():
        good = ninv == 0
        ok &= good
        print("  variance %s - %s^2: %d points, negative within round-off %d, unrealisable %d, min var/<ff> %.3e  %s"
              % (ff, f1, ncell, nr, ninv, vmin, "ok" if good else "FAIL"))
    print("RESULT %s" % ("PASS" if ok else "FAIL"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
