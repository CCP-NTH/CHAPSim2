"""
README
------
Post-process CHAPSim channel statistics into wall units and write them
to a single ASCII table.

This script reads 1D time/space-averaged statistics files of the form
`domain1_tsp_avg_<name>_<iter>.dat`, computes:
- friction velocity `u_tau`
- friction Reynolds number `Re_tau`
- `y+`
- 3 mean velocity in wall units
- 1 mean pressure in wall units
- 6 Reynolds stresses in wall units
- 3 mean vorticity components in wall units

Because these are 1D channel statistics, the mean fields vary only in `y`.
Therefore the mean vorticity components are evaluated as:
- `omega1 = d(u3)/dy`
- `omega2 = 0`
- `omega3 = -d(u1)/dy`

The CHAPSim `duduij` outputs are not raw velocity gradients, so they are not
used here to build vorticity.

Example
-------
python3 postprocess_channel_wall_units.py --dns-time 95000 --re 2800
"""

from __future__ import annotations

import argparse
import math
from pathlib import Path

import numpy as np


VELOCITY_COMPONENTS = ("u1", "u2", "u3")
REYNOLDS_COMPONENTS = ("uu11", "uu12", "uu13", "uu22", "uu23", "uu33")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Post-process CHAPSim channel statistics into wall units."
    )
    parser.add_argument(
        "--dns-time",
        required=True,
        help="Statistics iteration label, e.g. 95000.",
    )
    parser.add_argument(
        "--re",
        type=float,
        required=True,
        help="Channel Reynolds number used in the simulation, e.g. 2800.",
    )
    parser.add_argument(
        "--input-dir",
        default="../1_data",
        help="Directory containing domain1_tsp_avg_*.dat files.",
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Output file name. Default: channel_wall_units_<dns-time>.dat",
    )
    return parser.parse_args()


def read_profile(input_dir: Path, dns_time: str, name: str) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    path = input_dir / f"domain1_tsp_avg_{name}_{dns_time}.dat"
    if not path.exists():
        raise FileNotFoundError(f"Missing required file: {path}")

    data = np.loadtxt(path)
    data = np.atleast_2d(data)
    if data.shape[1] < 3:
        raise ValueError(f"{path} must contain at least 3 columns: index, y, value")

    return data[:, 0], data[:, 1], data[:, 2]


def check_same_grid(reference_y: np.ndarray, y: np.ndarray, name: str) -> None:
    if reference_y.shape != y.shape or not np.allclose(reference_y, y, rtol=0.0, atol=1.0e-12):
        raise ValueError(f"Profile {name} is not on the same wall-normal grid.")


def wall_distance(y: np.ndarray) -> np.ndarray:
    y_bottom = -1.0
    y_top = 1.0
    return np.minimum(y - y_bottom, y_top - y)


def compute_wall_units(y: np.ndarray, u1: np.ndarray, re_bulk: float) -> tuple[float, float, float, np.ndarray]:
    y_bottom = -1.0
    y1 = y[0] - y_bottom
    y2 = y[1] - y_bottom
    u_1 = u1[0]
    u_2 = u1[1]
    if y1 <= 0.0 or y2 <= 0.0 or abs(y2 - y1) <= 1.0e-14:
        raise ValueError(
            "Invalid first two wall-normal points for wall-shear evaluation: "
            f"y1={y1:.12e}, y2={y2:.12e}"
        )

    # Second-order one-sided wall gradient on a possibly non-uniform grid.
    dudy_wall = (u_1 * y2 * y2 - u_2 * y1 * y1) / (y1 * y2 * (y2 - y1))
    tauw = dudy_wall / re_bulk
    utau = math.sqrt(abs(tauw))
    retau = re_bulk * utau
    yplus = retau * wall_distance(y)

    return tauw, utau, retau, yplus


def reynolds_stress_plus(
    uu: np.ndarray, mean_a: np.ndarray, mean_b: np.ndarray, utau: float
) -> np.ndarray:
    return (uu - mean_a * mean_b) / (utau * utau)


def derivative_y(values: np.ndarray, y: np.ndarray) -> np.ndarray:
    edge_order = 2 if len(y) >= 3 else 1
    return np.gradient(values, y, edge_order=edge_order)


def vorticity_plus(omega: np.ndarray, re_bulk: float, utau: float) -> np.ndarray:
    return omega / (re_bulk * utau * utau)


def main() -> None:
    args = parse_args()

    input_dir = Path(args.input_dir)
    output = (
        Path(args.output)
        if args.output is not None
        else Path(f"channel_wall_units_{args.dns_time}.dat")
    )

    index, y, u1 = read_profile(input_dir, args.dns_time, "u1")
    _, y_u2, u2 = read_profile(input_dir, args.dns_time, "u2")
    _, y_u3, u3 = read_profile(input_dir, args.dns_time, "u3")
    _, y_pr, pr = read_profile(input_dir, args.dns_time, "pr")

    check_same_grid(y, y_u2, "u2")
    check_same_grid(y, y_u3, "u3")
    check_same_grid(y, y_pr, "pr")

    tauw, utau, retau, yplus = compute_wall_units(y, u1, args.re)

    reynolds_raw: dict[str, np.ndarray] = {}

    for name in REYNOLDS_COMPONENTS:
        _, y_comp, values = read_profile(input_dir, args.dns_time, name)
        check_same_grid(y, y_comp, name)
        reynolds_raw[name] = values

    u1_plus = u1 / utau
    u2_plus = u2 / utau
    u3_plus = u3 / utau

    pr_plus = pr / tauw
    pr_plus = pr_plus - pr_plus[0]

    reynolds_plus = {
        "uu11+": reynolds_stress_plus(reynolds_raw["uu11"], u1, u1, utau),
        "uu12+": reynolds_stress_plus(reynolds_raw["uu12"], u1, u2, utau),
        "uu13+": reynolds_stress_plus(reynolds_raw["uu13"], u1, u3, utau),
        "uu22+": reynolds_stress_plus(reynolds_raw["uu22"], u2, u2, utau),
        "uu23+": reynolds_stress_plus(reynolds_raw["uu23"], u2, u3, utau),
        "uu33+": reynolds_stress_plus(reynolds_raw["uu33"], u3, u3, utau),
    }

    du1_dy = derivative_y(u1, y)
    du3_dy = derivative_y(u3, y)
    omega1_plus = vorticity_plus(du3_dy, args.re, utau)
    omega2_plus = np.zeros_like(y)
    omega3_plus = vorticity_plus(-du1_dy, args.re, utau)

    output_data = np.column_stack(
        [
            index,
            y,
            yplus,
            u1_plus,
            u2_plus,
            u3_plus,
            pr_plus,
            reynolds_plus["uu11+"],
            reynolds_plus["uu12+"],
            reynolds_plus["uu13+"],
            reynolds_plus["uu22+"],
            reynolds_plus["uu23+"],
            reynolds_plus["uu33+"],
            omega1_plus,
            omega2_plus,
            omega3_plus,
        ]
    )

    header_lines = [
        f"Re = {args.re:.12e}",
        f"tauw = {tauw:.12e}",
        f"utau = {utau:.12e}",
        f"Retau = {retau:.12e}",
        "columns: index y y+ u1+ u2+ u3+ p+ "
        "uu11+ uu12+ uu13+ uu22+ uu23+ uu33+ "
        "omega1+ omega2+ omega3+",
        "notes: p_plus is shifted so that the first point equals zero",
        "notes: for 1D channel statistics, mean omega1 = d(u3)/dy, omega2 = 0, omega3 = -d(u1)/dy",
        "notes: omega+ uses nu*omega/utau^2 = omega / (Re*utau^2)",
    ]

    np.savetxt(
        output,
        output_data,
        fmt="%.10e",
        header="\n".join(header_lines),
        comments="# ",
    )

    print(f"u_tau  = {utau:.8e}")
    print(f"Re_tau = {retau:.8e}")
    print(f"Saved wall-unit profiles to {output.resolve()}")


if __name__ == "__main__":
    main()
