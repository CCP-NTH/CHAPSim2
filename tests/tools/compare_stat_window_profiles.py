#!/usr/bin/env python3
"""Compare two CHAPSim2 time-and-space averaged profile tables column by column.

Used by tests/functional/stat_window to check the y-profiles written by the
statistical-window post-processing mode against a reference: the profile of a
run accumulated directly over the same window, or the profile rebuilt by
validation/tools/scripts/rebuild_tavg_from_new_start.py.

Both files are ``CHAPSim_profile_ascii_v1`` tables (``#`` header lines, then
index, coordinate and one column per field).  The coordinate must agree to
round-off; every field column is compared as max|a-b| / scale.

The scale is max|ref column|, or - with --bundle-dir - the larger of that and
max|A| of the same field in the 3-D statistics bundle the profile was
averaged from (tsp_avg_X <-> t_avg_X).  The second is needed for fields
whose plane average vanishes by symmetry (e.g. <v>, <du/dx> in a periodic
channel): their profile is round-off noise, and a difference relative to
that noise is meaningless.  Relative to the 3-D field magnitude it is the
same criterion compare_stats_restart.py applies to the bundle; a plane
average cannot amplify an error measured against it.

Usage:
    compare_stat_window_profiles.py <reference.dat> <test.dat> [--rtol R]
        [--bundle-dir <1_data> --iteration N --group flow_stats]
"""

import os

import argparse
import sys

import numpy as np

MAGNITUDE_FLOOR = 1.0e-30


def read_table(path):
    names = None
    with open(path, "r") as fh:
        for line in fh:
            if line.startswith("# columns:"):
                names = line.split(":", 1)[1].split()
    data = np.atleast_2d(np.loadtxt(path, comments="#"))
    return names, data


def bundle_scales(bundle_dir, group, iteration, domain=1):
    """max|A| per field of a bundled statistics file, keyed by field name."""
    stem = os.path.join(bundle_dir, "domain%d_%s" % (domain, group))
    fields = None
    with open("%s_meta_%d.dat" % (stem, iteration)) as fh:
        for line in fh:
            if line.startswith("fields "):
                fields = [e.split(":", 1)[0] for e in line.split(None, 1)[1].strip().split(",") if e]
    data = np.fromfile("%s_%d.bin" % (stem, iteration), dtype=np.float64)
    ncell = data.size // len(fields)
    return {name: float(np.max(np.abs(data[n * ncell:(n + 1) * ncell])))
            for n, name in enumerate(fields)}


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("reference")
    p.add_argument("test")
    p.add_argument("--rtol", type=float, default=1.0e-10)
    p.add_argument("--bundle-dir", help="1_data holding the 3-D bundle of the reference")
    p.add_argument("--iteration", type=int)
    p.add_argument("--group", default="flow_stats")
    args = p.parse_args()

    scales = {}
    if args.bundle_dir:
        scales = bundle_scales(args.bundle_dir, args.group, args.iteration)

    rnames, ref = read_table(args.reference)
    _, tst = read_table(args.test)
    if ref.shape != tst.shape:
        print("  SHAPE differs: %s vs %s" % (ref.shape, tst.shape))
        return 1
    if not np.allclose(ref[:, 1], tst[:, 1], rtol=1e-12, atol=1e-14):
        print("  COORDINATE column differs")
        return 1

    ok = True
    worst_name, worst = "none", 0.0
    for c in range(2, ref.shape[1]):
        name = rnames[c] if rnames and c < len(rnames) else "col%d" % (c + 1)
        a, b = ref[:, c], tst[:, c]
        if not (np.all(np.isfinite(a)) and np.all(np.isfinite(b))):
            print("  NON-FINITE values in %s" % name)
            ok = False
            continue
        scale = max(np.max(np.abs(a)), MAGNITUDE_FLOOR)
        if scales:
            key = name.replace("tsp_avg_", "t_avg_", 1)
            if key not in scales:
                print("  no 3-D field %s in the bundle" % key)
                ok = False
                continue
            scale = max(scale, scales[key])
        err = float(np.max(np.abs(a - b)) / scale)
        if err > worst:
            worst_name, worst = name, err
        if err > args.rtol:
            print("  FAIL %-20s rel %.3e > %.3e" % (name, err, args.rtol))
            ok = False
    print("  %d field columns, %d points, worst %s rel %.3e (tol %.3e)"
          % (ref.shape[1] - 2, ref.shape[0], worst_name, worst, args.rtol))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
