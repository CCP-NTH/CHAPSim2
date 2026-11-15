#!/usr/bin/env python3
"""Check, or derive, an isothermal compact-history flow restart bundle.

The format is the one written by write_flow_restart_bundle_compact and read by
read_flow_restart_bundle_compact / read_flow_restart_bundle_initial
(src/io_restart.f90):

  domain<N>_flow_restart_<ITER>.bin       raw native-endian reals, no headers or
                                          padding; the arrays one after another, each
                                          the whole global array in Fortran order
                                          (x fastest), as 2DECOMP MPI-IO writes it
  domain<N>_flow_restart_meta_<ITER>.dat  CHAPSim_bundle_v2 text metadata; 'fields'
                                          and 'field_shapes' list the arrays in file
                                          order with their global sizes

An isothermal exact bundle holds qx, qy, qz, pr, mx_rhs0, my_rhs0, mz_rhs0 and, with a
convective x-outlet, the outlet planes and their RHS history after them. The compact
bundle holds qx, qy, qz, pr and, with a convective x-outlet, fbcx_qx, fbcx_qy, fbcx_qz.
With a convective x-outlet the compact arrays are therefore NOT a prefix of the exact
file, and a thermal exact bundle starts with gx, gy, gz: 'convert' refuses both.

The solver checks only the metadata (field list, shapes, nc, np, group, iteration,
layout); it does not compare the size of the .bin with the metadata, so a bundle whose
.bin holds more or fewer arrays than its metadata lists is read without an error.
'check' closes that gap for test inputs.

  compact_flow_bundle.py check   DATA_DIR ITER [--domain N] [--exact EXACT_DIR]
  compact_flow_bundle.py convert EXACT_DIR ITER OUT_DIR [--domain N]

'check' verifies a compact bundle: metadata header and keys in the order the solver
reads them, the compact field list (non-convective outlet), .bin size equal to the sum
of the listed arrays. With --exact it also verifies that the compact .bin equals the
leading bytes of the exact .bin and that the two metadata files differ only in
'fields' and 'field_shapes'.

'convert' writes into OUT_DIR (which must not already hold the bundle) the compact
bundle a native compact writer would have written at the same checkpoint, from an
isothermal exact bundle without a convective outlet. Only the two bundle files are
written; copy domain<N>_checkpoint_meta_<ITER>.dat (time and dt of the restart) and any
other file the run needs separately.
"""

import argparse
import os
import sys
from pathlib import Path

META_KEYS = [
    "group", "iter", "iter_start", "iter_end", "time", "time_end", "dt", "idom",
    "icase", "coordinate", "layout", "precision_bytes", "endianness", "nc", "np",
    "length", "stretch", "periodic", "ndbbuf", "fields", "field_shapes",
]
ISO_EXACT = "qx:dpcc,qy:dcpc,qz:dccp,pr:dccc,mx_rhs0:dpcc,my_rhs0:dcpc,mz_rhs0:dccp"
ISO_COMPACT = "qx:dpcc,qy:dcpc,qz:dccp,pr:dccc"
NCOMPACT = 4


def fail(msg):
    sys.exit(f"FAIL: {msg}")


def paths(data_dir, idom, it):
    d = Path(data_dir)
    return (d / f"domain{idom}_flow_restart_{it}.bin",
            d / f"domain{idom}_flow_restart_meta_{it}.dat")


def read_meta(meta_file):
    lines = meta_file.read_text().splitlines()
    if not lines or lines[0].strip() != "CHAPSim_bundle_v2":
        fail(f"{meta_file}: first line is not CHAPSim_bundle_v2")
    if len(lines) - 1 < len(META_KEYS):
        fail(f"{meta_file}: {len(lines) - 1} keys, expected {len(META_KEYS)}")
    meta = {}
    for key, line in zip(META_KEYS, lines[1:]):
        k, _, v = line.partition(" ")
        if k != key:
            fail(f"{meta_file}: expected key {key}, got {k} (the solver reads the keys in order)")
        meta[k] = v.strip()
    return lines, meta


def shapes(meta, meta_file):
    out = []
    for entry in meta["field_shapes"].split(";"):
        name, _, dims = entry.partition("=")
        n = [int(x) for x in dims.split(",")]
        if len(n) != 3:
            fail(f"{meta_file}: malformed field_shapes entry {entry}")
        out.append((name, n[0] * n[1] * n[2]))
    names = [f.split(":")[0] for f in meta["fields"].split(",")]
    if names != [s[0] for s in out]:
        fail(f"{meta_file}: 'fields' and 'field_shapes' list different arrays")
    return out


def bundle(data_dir, idom, it):
    bin_file, meta_file = paths(data_dir, idom, it)
    for f in (bin_file, meta_file):
        if not f.is_file():
            fail(f"{f} does not exist")
    lines, meta = read_meta(meta_file)
    if meta["group"] != "flow_restart" or int(meta["iter"]) != it or meta["layout"] != "bundled":
        fail(f"{meta_file}: group/iter/layout is not flow_restart/{it}/bundled")
    if meta["endianness"] != "native":
        fail(f"{meta_file}: endianness {meta['endianness']}")
    nbytes = int(meta["precision_bytes"])
    shp = shapes(meta, meta_file)
    expected = nbytes * sum(n for _, n in shp)
    actual = bin_file.stat().st_size
    if actual != expected:
        fail(f"{bin_file}: {actual} bytes, the metadata lists {expected} "
             f"({len(shp)} arrays of {nbytes} bytes per value)")
    return bin_file, meta_file, lines, meta, shp, nbytes


def check(args):
    bin_file, meta_file, lines, meta, shp, nbytes = bundle(args.data_dir, args.domain, args.iter)
    if meta["fields"] != ISO_COMPACT:
        fail(f"{meta_file}: fields '{meta['fields']}' is not the isothermal compact list "
             f"'{ISO_COMPACT}' (a convective-outlet compact bundle is outside this check)")
    print(f"OK  compact bundle {bin_file.name}: {len(shp)} arrays, {bin_file.stat().st_size} bytes")
    if args.exact:
        ebin, emeta, elines, emeta_d, eshp, _ = bundle(args.exact, args.domain, args.iter)
        if emeta_d["fields"] != ISO_EXACT:
            fail(f"{emeta}: not an isothermal exact bundle without convective outlet")
        if eshp[:NCOMPACT] != shp:
            fail("the compact arrays are not the leading arrays of the exact bundle")
        n = bin_file.stat().st_size
        with open(bin_file, "rb") as a, open(ebin, "rb") as b:
            if a.read() != b.read(n):
                fail(f"{bin_file} differs from the first {n} bytes of {ebin}")
        diff = [k for k, x, y in zip(["header"] + META_KEYS, elines, lines) if x != y]
        if len(elines) != len(lines) or diff != ["fields", "field_shapes"]:
            fail(f"metadata differ in {diff} (expected only fields, field_shapes)")
        print(f"OK  equals the leading {n} bytes of {ebin.name}; metadata differ only in fields, field_shapes")


def convert(args):
    ebin, emeta, elines, meta, shp, nbytes = bundle(args.exact_dir, args.domain, args.iter)
    if meta["fields"] != ISO_EXACT:
        fail(f"{emeta}: fields '{meta['fields']}' is not an isothermal exact bundle without a "
             "convective outlet; its compact form is not a prefix of it (use the native compact writer)")
    out_bin, out_meta = paths(args.out_dir, args.domain, args.iter)
    if out_bin.exists() or out_meta.exists():
        fail(f"{args.out_dir} already holds a flow restart bundle for iteration {args.iter}")
    if os.path.samefile(args.exact_dir, args.out_dir):
        fail("OUT_DIR must differ from EXACT_DIR")
    n = nbytes * sum(m for _, m in shp[:NCOMPACT])
    with open(ebin, "rb") as f:
        data = f.read(n)
    out_bin.write_bytes(data)
    cshp = ";".join(e for e in meta["field_shapes"].split(";")[:NCOMPACT])
    lines = list(elines)
    lines[1 + META_KEYS.index("fields")] = f"fields {ISO_COMPACT}"
    lines[1 + META_KEYS.index("field_shapes")] = f"field_shapes {cshp}"
    out_meta.write_text("\n".join(lines) + "\n")
    print(f"wrote {out_bin} ({n} bytes) and {out_meta.name}")


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("check")
    c.add_argument("data_dir")
    c.add_argument("iter", type=int)
    c.add_argument("--domain", type=int, default=1)
    c.add_argument("--exact")
    v = sub.add_parser("convert")
    v.add_argument("exact_dir")
    v.add_argument("iter", type=int)
    v.add_argument("out_dir")
    v.add_argument("--domain", type=int, default=1)
    args = p.parse_args()
    check(args) if args.cmd == "check" else convert(args)


if __name__ == "__main__":
    main()
