#!/usr/bin/env python3
"""Generate a CHAPSim2 isobaric property table using CoolProp.

CHAPSim2 reads a tabulated isobar for its supercritical-property (`scp`) cases.
The tables shipped historically are NIST Standard Reference Data, whose
redistribution terms are restrictive - see README.md in this directory. This
script produces an equivalent table from CoolProp (MIT licence), which
implements the same equations of state, so the result carries no redistribution
question.

The thermodynamic columns agree with the NIST tables to round-off for water and
to the NIST file's own 5-significant-figure truncation for CO2. The transport
columns differ by up to 5% because CoolProp carries newer correlations. A
generated table is therefore NOT a bit-identical substitute: swapping it in
moves the thermal baselines and needs an approved reference refresh.

    pip install CoolProp
    python3 generate_property_table.py --fluid Water --pressure 23.5e6 \
        --segments 573.15:623.15:0.1,623.15:673.15:0.02,673.15:873.15:0.1 \
        --output NIST_WATER_23.5MP.DAT

--segments gives a piecewise-uniform grid (start:end:step, joined without
duplicating the shared end points); --tmin/--tmax/--tstep give a uniform grid.
--header-note appends a provenance note to the header line, which the solver
skips.
"""

from __future__ import annotations

import argparse
import sys


HEADER = "#P(Mpa) H(J/KG) T(K)    D(KG/M3)  M(PA-S)       K(W/M-K) CP(J/KG-K)   Beta(1/K)"

# Column order the solver's reader expects. CoolProp's key for each is given
# alongside; 'P' and 'T' are the independent variables and are echoed back.
COOLPROP_KEYS = (
    "H",                                # specific enthalpy, J/kg
    "D",                                # density, kg/m3
    "V",                                # dynamic viscosity, Pa s
    "L",                                # thermal conductivity, W/m K
    "C",                                # isobaric specific heat, J/kg K
    "isobaric_expansion_coefficient",   # beta, 1/K
)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--fluid", required=True,
                        help="CoolProp fluid name, e.g. Water or CO2")
    parser.add_argument("--pressure", type=float, required=True,
                        help="pressure in Pa (the table is a single isobar)")
    parser.add_argument("--tmin", type=float, help="lowest temperature, K (uniform grid)")
    parser.add_argument("--tmax", type=float, help="highest temperature, K (uniform grid)")
    parser.add_argument("--tstep", type=float, help="temperature step, K (uniform grid)")
    parser.add_argument("--segments",
                        help="piecewise-uniform grid 'start:end:step[,start:end:step...]', K")
    parser.add_argument("--header-note", default="",
                        help="text appended to the header line (provenance)")
    parser.add_argument("--output", required=True, help="output file path")
    parser.add_argument("--digits", type=int, default=11,
                        help="significant digits in the output (default 11)")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)

    try:
        from CoolProp.CoolProp import PropsSI
    except ImportError:
        print("error: CoolProp is not installed. Run 'pip install CoolProp'.", file=sys.stderr)
        return 1

    # Build each segment from an integer count so that accumulated floating-point
    # error cannot drop or duplicate an end point.
    if args.segments:
        spans = []
        for item in args.segments.split(","):
            try:
                a, b, h = (float(v) for v in item.split(":"))
            except ValueError:
                print(f"error: bad segment '{item}' (expected start:end:step).", file=sys.stderr)
                return 1
            spans.append((a, b, h))
    elif None not in (args.tmin, args.tmax, args.tstep):
        spans = [(args.tmin, args.tmax, args.tstep)]
    else:
        print("error: give --segments or all of --tmin, --tmax, --tstep.", file=sys.stderr)
        return 1
    temps = []
    for a, b, h in spans:
        if h <= 0 or b < a:
            print("error: need step > 0 and end >= start in every segment.", file=sys.stderr)
            return 1
        n = int(round((b - a) / h))
        for i in range(n + 1):
            t = a + i * h
            if temps and abs(t - temps[-1]) < 0.5 * h:
                continue                    # shared end point of two segments
            temps.append(t)
    nrow = len(temps)
    fmt = f"%.{args.digits}E"
    p_mpa = args.pressure / 1.0e6

    with open(args.output, "w") as out:
        out.write(HEADER + (("  " + args.header_note) if args.header_note else "") + "\n")
        for t in temps:
            try:
                values = [PropsSI(key, "T", t, "P", args.pressure, args.fluid)
                          for key in COOLPROP_KEYS]
            except ValueError as exc:
                # Most often a two-phase state: the table must be a single
                # phase, so say which point failed rather than writing a gap.
                print(f"error: CoolProp failed for {args.fluid} at T = {t} K, "
                      f"P = {p_mpa} MPa: {exc}", file=sys.stderr)
                return 1
            enthalpy, density, viscosity, conductivity, cp, beta = values
            row = (p_mpa, enthalpy, t, density, viscosity, conductivity, cp, beta)
            out.write("\t".join(fmt % v for v in row) + "\n")

    print(f"wrote {nrow} rows for {args.fluid} at {p_mpa} MPa to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
