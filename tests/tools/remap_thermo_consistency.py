#!/usr/bin/env python3
"""Thermodynamic consistency of a thermal restart bundle (tests/restart_remap_thermo).

    remap_thermo_consistency.py DATA_DIR ITER [TOL] [--remap]

Reads domain1_thermo_restart_<ITER>.bin with its metadata and prints
  recovery: max|rhoh - dens_hat*henth| / max|rhoh|
      the first recovery of the time loop takes h = rho*h / rho_hat; a stored state with
      rhoh /= rho_hat h makes that h jump from the stored henth (rhoh_recovery = transported);
  eos:      max|dens_hat - dens| / max|dens|
      zero for a remapped state, which sets rho_hat = rho_EOS(h); a time-loop state may drift.
Exit 0 when recovery <= TOL (default 1e-14) and, with --remap, eos == 0; 1 otherwise; 2 on a
missing or malformed bundle (or no dens_hat, i.e. not a transported-density checkpoint).
"""
import sys
from pathlib import Path

import numpy as np


def read_bundle(data, it):
    meta = Path(data) / f"domain1_thermo_restart_meta_{it}.dat"
    shapes = None
    for line in meta.read_text().splitlines():
        if line.startswith("field_shapes "):
            shapes = line.split(None, 1)[1].strip()
    if shapes is None:
        raise ValueError(f"{meta}: no field_shapes")
    raw = np.fromfile(Path(data) / f"domain1_thermo_restart_{it}.bin", dtype="<f8")
    out, pos = {}, 0
    for item in shapes.split(";"):
        name, dims = item.split("=")
        n = int(np.prod([int(v) for v in dims.split(",")]))
        out[name] = raw[pos:pos + n]
        pos += n
    if pos != raw.size:
        raise ValueError(f"{meta}: metadata and file size disagree")
    return out


def main():
    args = [a for a in sys.argv[1:] if a != "--remap"]
    remap = "--remap" in sys.argv[1:]
    if len(args) < 2:
        sys.exit(__doc__)
    data, it = args[0], args[1]
    tol = float(args[2]) if len(args) > 2 else 1e-14
    try:
        f = read_bundle(data, it)
    except (OSError, ValueError) as exc:
        print(exc)
        sys.exit(2)
    for name in ("rhoh", "dens", "henth", "dens_hat"):
        if name not in f:
            print(f"field {name} not in the bundle")
            sys.exit(2)
    rec = np.max(np.abs(f["rhoh"] - f["dens_hat"] * f["henth"])) / np.max(np.abs(f["rhoh"]))
    eos = np.max(np.abs(f["dens_hat"] - f["dens"])) / np.max(np.abs(f["dens"]))
    print(f"recovery max|rhoh - dens_hat*henth|/max|rhoh| = {rec:.3e} (tol {tol:.0e})")
    print(f"eos      max|dens_hat - dens|/max|dens|       = {eos:.3e}" + (" (gate: 0)" if remap else ""))
    sys.exit(0 if (rec <= tol and (eos == 0.0 or not remap)) else 1)


if __name__ == "__main__":
    main()
