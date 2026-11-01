#!/usr/bin/env python3
"""Inspect, check, compare and convert CHAPSim flow restart bundles (CHAPSim_bundle_v2).

A bundle is <dir>/domain<d>_<group>_<iter>.bin plus its manifest
<dir>/domain<d>_<group>_meta_<iter>.dat. The .bin holds the listed fields one after
another, each a global array of float64 (precision_bytes 8, native byte order) in
Fortran order, with no record markers or padding (2DECOMP&FFT MPI-IO,
decomp_2d_write_var). Field i has the global size given in field_shapes
(name=n1,n2,n3), so the byte offset of a field is the sum of the sizes before it.

  restart_bundle_tool.py inspect  BIN
  restart_bundle_tool.py check    BIN [--outlet]      native compact flow_restart contract (exit 1 if not met)
  restart_bundle_tool.py compare  BIN_A BIN_B [FIELD ...]
                          bitwise identity of the named fields (default: the fields both
                          bundles share, by name); exit 1 on any difference
  restart_bundle_tool.py convert  EXACT_BIN OUT_DIR   exact isothermal x-periodic flow_restart bundle ->
                          compact bundle (first four arrays, manifest fields/field_shapes cut);
                          OUT_DIR must not contain the output files
Restart history modes: docs/guidance/docs/restart-io.md.
"""
import hashlib
import os
import re
import sys

import numpy as np

KEYS = ["CHAPSim_bundle_v2", "group", "iter", "iter_start", "iter_end", "time", "time_end", "dt",
        "idom", "icase", "coordinate", "layout", "precision_bytes", "endianness", "nc", "np",
        "length", "stretch", "periodic", "ndbbuf", "fields", "field_shapes"]
# io_restart.f90 flow_restart_fields_compact / flow_restart_fields_exact (isothermal)
COMPACT_FIELDS = "qx:dpcc,qy:dcpc,qz:dccp,pr:dccc"
COMPACT_FIELDS_OUTLET = COMPACT_FIELDS + ",fbcx_qx:d4cc,fbcx_qy:d4pc,fbcx_qz:d4cp"
EXACT_ISO_FIELDS = COMPACT_FIELDS + ",mx_rhs0:dpcc,my_rhs0:dcpc,mz_rhs0:dccp"


def meta_path(bin_path):
    d, b = os.path.split(bin_path)
    m = re.fullmatch(r"(domain\d+)_(.+)_(\d+)\.bin", b)
    if not m:
        raise SystemExit(f"not a bundle file name: {b}")
    return os.path.join(d, f"{m.group(1)}_{m.group(2)}_meta_{m.group(3)}.dat"), m.group(2), int(m.group(3))


def read_meta(bin_path):
    mp, group, it = meta_path(bin_path)
    with open(mp) as f:
        lines = f.read().splitlines()
    meta, order = {}, []
    for ln in lines:
        k, _, v = ln.partition(" ")
        meta[k] = v.strip()
        order.append(k)
    return mp, group, it, meta, order, lines


def layout(meta):
    """[(name, location, (n1, n2, n3), offset_bytes, nbytes)] from fields/field_shapes."""
    names = [f.split(":") for f in meta["fields"].split(",")]
    shapes = {}
    # field_shapes is 'name=n1,n2,n3;name=n1,n2,n3;...' (';' between fields)
    for m in re.finditer(r"([A-Za-z0-9_]+)=(\d+),(\d+),(\d+)", meta["field_shapes"]):
        shapes[m.group(1)] = (int(m.group(2)), int(m.group(3)), int(m.group(4)))
    pb = int(meta.get("precision_bytes", "8"))
    out, off = [], 0
    for name, loc in names:
        if name not in shapes:
            raise SystemExit(f"field {name} has no shape in field_shapes")
        n = shapes[name]
        nb = n[0] * n[1] * n[2] * pb
        out.append((name, loc, n, off, nb))
        off += nb
    return out, off


def load(bin_path, name, meta):
    for nm, _, n, off, nb in layout(meta)[0]:
        if nm == name:
            a = np.fromfile(bin_path, dtype=np.float64, count=nb // 8, offset=off)
            return a.reshape(n, order="F")
    raise KeyError(name)


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for blk in iter(lambda: f.read(1 << 20), b""):
            h.update(blk)
    return h.hexdigest()


def cmd_inspect(binp):
    mp, group, it, meta, order, _ = read_meta(binp)
    lay, total = layout(meta)
    size = os.path.getsize(binp)
    print(f"bundle   {binp}\nmanifest {mp}\ngroup {group}  iter {it}  time {meta.get('time')}  dt {meta.get('dt')}")
    print(f"nc {meta.get('nc')}  np {meta.get('np')}  precision_bytes {meta.get('precision_bytes')}  "
          f"endianness {meta.get('endianness')}")
    print(f"file bytes {size}, sum of field sizes {total}: {'MATCH' if size == total else 'MISMATCH'}")
    for nm, loc, n, off, nb in lay:
        a = np.fromfile(binp, dtype=np.float64, count=nb // 8, offset=off)
        ok = a.size == nb // 8
        print(f"  {nm:16s} {loc:5s} {n[0]:4d}x{n[1]:4d}x{n[2]:4d} offset {off:10d} bytes {nb:9d}"
              + (f"  min {a.min(): .6e} max {a.max(): .6e} finite {bool(np.isfinite(a).all())}" if ok and a.size
                 else "  TRUNCATED"))
    print(f"sha256 bin  {sha256(binp)}\nsha256 meta {sha256(mp)}")
    return 0 if size == total else 1


def cmd_check(binp, outlet):
    """The native compact flow_restart contract (what read_flow_restart_bundle_compact and the
    compact branch of read_flow_restart_bundle_initial accept), plus the file length, which the
    solver does not check."""
    errs = []
    mp, group, it, meta, order, lines = read_meta(binp)
    if lines[0] != "CHAPSim_bundle_v2":
        errs.append(f"header {lines[0]!r}")
    if order != KEYS:
        errs.append(f"key order {order} differs from {KEYS}")
    if meta.get("group") != "flow_restart" or group != "flow_restart":
        errs.append(f"group {meta.get('group')}")
    if meta.get("iter") != str(it):
        errs.append(f"iter {meta.get('iter')} vs file name {it}")
    if meta.get("layout") != "bundled":
        errs.append(f"layout {meta.get('layout')}")
    want = COMPACT_FIELDS_OUTLET if outlet else COMPACT_FIELDS
    if meta.get("fields") != want:
        errs.append(f"fields {meta.get('fields')!r}, native compact is {want!r}")
    if meta.get("precision_bytes") != "8" or meta.get("endianness") != "native":
        errs.append("precision_bytes/endianness")
    nc = [int(x) for x in meta["nc"].split()]
    # global sizes: a staggered direction has np points (np = nc in a periodic direction)
    npt = [int(x) for x in meta["np"].split()]
    exp = {"dpcc": (npt[0], nc[1], nc[2]), "dcpc": (nc[0], npt[1], nc[2]),
           "dccp": (nc[0], nc[1], npt[2]), "dccc": (nc[0], nc[1], nc[2])}
    lay, total = layout(meta)
    for nm, loc, n, _, _ in lay:
        if loc in exp and n != exp[loc]:
            errs.append(f"shape of {nm} {n}, expected {exp[loc]} for {loc}")
    # the solver compares the whole field_shapes string (validate_bundle_metadata)
    want_shapes = ";".join(f"{nm}={n[0]},{n[1]},{n[2]}" for nm, _, n, _, _ in lay)
    if meta.get("field_shapes") != want_shapes:
        errs.append(f"field_shapes {meta.get('field_shapes')!r}, the solver writes {want_shapes!r}")
    size = os.path.getsize(binp)
    if size != total:
        errs.append(f"file bytes {size} != sum of field sizes {total}")
    for e in errs:
        print("FAIL", e)
    print("compact contract:", "OK" if not errs else f"{len(errs)} violation(s)")
    return 0 if not errs else 1


def cmd_compare(a, b, names):
    _, _, _, ma, _, _ = read_meta(a)
    _, _, _, mb, _, _ = read_meta(b)
    la = [x[0] for x in layout(ma)[0]]
    lb = [x[0] for x in layout(mb)[0]]
    if not names:
        names = [n for n in la if n in lb]
    bad = 0
    for n in names:
        if n not in la or n not in lb:
            print(f"MISSING {n}")
            bad += 1
            continue
        x, y = load(a, n, ma), load(b, n, mb)
        if x.shape != y.shape:
            print(f"DIFF    {n}: shape {x.shape} vs {y.shape}")
            bad += 1
        elif x.tobytes() == y.tobytes():
            print(f"IDENT   {n}")
        else:
            d = np.abs(x - y)
            print(f"DIFF    {n}: {int((x != y).sum())} values differ, max |diff| {d.max():.3e}, "
                  f"max |a| {np.abs(x).max():.3e}")
            bad += 1
    print("RESULT", "BITWISE IDENTICAL" if bad == 0 else f"{bad} field(s) differ", f"({len(names)} compared)")
    return 0 if bad == 0 else 1


def cmd_convert(exact_bin, out_dir):
    mp, group, it, meta, order, lines = read_meta(exact_bin)
    if group != "flow_restart" or meta.get("fields") != EXACT_ISO_FIELDS:
        raise SystemExit("convert: only an isothermal x-periodic exact flow_restart bundle "
                         f"(fields {EXACT_ISO_FIELDS}) has the compact fields as a prefix; got {meta.get('fields')}")
    if order != KEYS:
        raise SystemExit("convert: manifest key order is not the native one")
    lay, total = layout(meta)
    if os.path.getsize(exact_bin) != total:
        raise SystemExit("convert: exact bundle length does not match its manifest")
    keep = lay[:4]
    nbytes = sum(x[4] for x in keep)
    ob = os.path.join(out_dir, os.path.basename(exact_bin))
    om = os.path.join(out_dir, os.path.basename(mp))
    for p in (ob, om):
        if os.path.exists(p):
            raise SystemExit(f"convert: {p} exists")
    with open(exact_bin, "rb") as f:
        data = f.read(nbytes)
    with open(ob, "wb") as f:
        f.write(data)
    shapes = ";".join(f"{n}={s[0]},{s[1]},{s[2]}" for n, _, s, _, _ in keep)
    out = []
    for ln in lines:
        k = ln.partition(" ")[0]
        out.append(f"fields {COMPACT_FIELDS}" if k == "fields" else
                   f"field_shapes {shapes}" if k == "field_shapes" else ln)
    with open(om, "w") as f:
        f.write("\n".join(out) + "\n")
    print(f"wrote {ob} ({nbytes} bytes) and {om}")
    return 0


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    c = argv[1]
    if c == "inspect":
        return cmd_inspect(argv[2])
    if c == "check":
        return cmd_check(argv[2], "--outlet" in argv[3:])
    if c == "compare":
        return cmd_compare(argv[2], argv[3], argv[4:])
    if c == "convert":
        return cmd_convert(argv[2], argv[3])
    print(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
