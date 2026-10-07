#!/usr/bin/env python3
"""Compare the time-averaged statistics of a restarted run against one
uninterrupted run.

The restart equivalence runner already compares the instantaneous metrics of the
two runs.  Those say nothing about the statistics accumulators, which are
reloaded from the checkpoint and carry a sample count of their own; a weighting
error there is invisible to every metric in regression_test_metrics.json.

The bundled statistics file is a plain stream of float64 written by
2decomp&FFT's MPI-IO, one global array per field, in the order listed on the
``fields`` line of the companion ``*_meta_*.dat`` manifest.  That lets each
field be compared on its own, which matters because a single file-wide
normalisation would let a large field mask a wrong small one.

Agreement is array-relative:  max|a-b| / max(max|ref|, floor).  Element-wise
relative error is meaningless here - a Taylor-Green field has exact zeros by
symmetry, and the restart reproduces the flow itself only to round-off, not
bitwise, so the statistics inherit that.

Usage:
    check_stats_restart.py <continuous_1_data> <restart_1_data> <iter>
                           [--rtol R] [--groups flow_stats,thermo_stats]
"""

import argparse
import os
import sys

import numpy as np

# Floor on the normalising magnitude, so a field that is identically zero in
# both runs compares equal instead of dividing by zero.
MAGNITUDE_FLOOR = 1.0e-30


def read_manifest(path):
    """Return the manifest as a dict of first-token -> rest-of-line."""
    meta = {}
    with open(path, "r") as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            parts = line.split(None, 1)
            meta[parts[0]] = parts[1] if len(parts) > 1 else ""
    return meta


def field_names(meta):
    """Field names from the manifest, stripped of their ':decomp' suffix."""
    if "fields" not in meta:
        raise ValueError("manifest has no 'fields' line")
    return [entry.split(":", 1)[0] for entry in meta["fields"].split(",") if entry]


def compare_group(cont_dir, rest_dir, group, iteration, rtol, domain):
    stem = "domain%d_%s" % (domain, group)
    cont_bin = os.path.join(cont_dir, "%s_%d.bin" % (stem, iteration))
    rest_bin = os.path.join(rest_dir, "%s_%d.bin" % (stem, iteration))
    cont_meta = os.path.join(cont_dir, "%s_meta_%d.dat" % (stem, iteration))
    rest_meta = os.path.join(rest_dir, "%s_meta_%d.dat" % (stem, iteration))

    for path in (cont_bin, rest_bin, cont_meta, rest_meta):
        if not os.path.isfile(path):
            print("  MISSING %s" % path)
            return False

    cmeta = read_manifest(cont_meta)
    rmeta = read_manifest(rest_meta)

    ok = True

    # The signature pins stat_level and is_thermo, so a mismatch means the two
    # runs did not even accumulate the same set of fields.
    if cmeta.get("signature") != rmeta.get("signature"):
        print("  SIGNATURE differs: %r vs %r"
              % (cmeta.get("signature"), rmeta.get("signature")))
        return False

    # The sample count is the quantity the restart has to carry across, so
    # check it before the arrays - it localises the failure.
    csamp, rsamp = cmeta.get("nsamples"), rmeta.get("nsamples")
    if csamp is None or rsamp is None:
        print("  no 'nsamples' key in the manifest (checkpoint predates it?)")
        ok = False
    elif csamp != rsamp:
        print("  SAMPLE COUNT differs: continuous %s, restart %s" % (csamp, rsamp))
        ok = False
    else:
        print("  samples: %s (both runs)" % csamp)

    names = field_names(cmeta)
    a = np.fromfile(cont_bin, dtype=np.float64)
    b = np.fromfile(rest_bin, dtype=np.float64)

    if a.size != b.size:
        print("  SIZE differs: %d vs %d float64" % (a.size, b.size))
        return False
    if a.size % len(names) != 0:
        print("  %d values do not divide into %d fields" % (a.size, len(names)))
        return False

    ncell = a.size // len(names)
    worst_name, worst = "none", 0.0
    for n, name in enumerate(names):
        fa = a[n * ncell:(n + 1) * ncell]
        fb = b[n * ncell:(n + 1) * ncell]
        if not (np.all(np.isfinite(fa)) and np.all(np.isfinite(fb))):
            print("  NON-FINITE values in %s" % name)
            ok = False
            continue
        scale = max(np.max(np.abs(fa)), MAGNITUDE_FLOOR)
        err = float(np.max(np.abs(fa - fb)) / scale)
        if err > worst:
            worst_name, worst = name, err
        if err > rtol:
            print("  FAIL %-20s rel %.3e > %.3e" % (name, err, rtol))
            ok = False

    print("  %d fields, %d cells each, worst %s rel %.3e (tol %.3e)"
          % (len(names), ncell, worst_name, worst, rtol))
    return ok


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("continuous_dir", help="1_data of the uninterrupted run")
    p.add_argument("restart_dir", help="1_data of the restarted run")
    p.add_argument("iteration", type=int, help="iteration both runs end at")
    p.add_argument("--rtol", type=float, default=1.0e-10,
                   help="array-relative tolerance (default 1e-10, matching the "
                        "instantaneous restart tolerances)")
    p.add_argument("--groups", default="flow_stats",
                   help="comma-separated statistics groups to compare")
    p.add_argument("--domain", type=int, default=1, help="domain index")
    args = p.parse_args()

    all_ok = True
    for group in args.groups.split(","):
        group = group.strip()
        if not group:
            continue
        print("Statistics group '%s' at iteration %d:" % (group, args.iteration))
        if not compare_group(args.continuous_dir, args.restart_dir, group,
                             args.iteration, args.rtol, args.domain):
            all_ok = False

    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main())
