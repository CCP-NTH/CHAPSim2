#!/usr/bin/env python3
"""Check shared validation scripts can run from outside case directories."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import os
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
VALIDATION_TOOLS = REPO_ROOT / "validation" / "tools" / "scripts"
PYTHON = sys.executable


NSAMPLES = 60
NNODES = 33


def run(command: list[str], cache_dir: Path) -> None:
    env = os.environ.copy()
    # Keep matplotlib headless and self-contained. The previous values were
    # macOS /private/tmp paths, which do not exist on Linux or on a runner.
    env["MPLBACKEND"] = "Agg"
    env["MPLCONFIGDIR"] = str(cache_dir)
    env["XDG_CACHE_HOME"] = str(cache_dir)
    result = subprocess.run(command, cwd=REPO_ROOT, text=True, capture_output=True, env=env)
    if result.returncode != 0:
        print(result.stdout)
        print(result.stderr)
        raise AssertionError(f"command failed: {' '.join(command)}")


def require_file(path: Path) -> None:
    if not path.is_file():
        raise AssertionError(f"expected output file was not created: {path}")


def write_synthetic_case(case_dir: Path) -> None:
    """Build the smallest case layout the four shared scripts read.

    The scripts are checked for path portability, not for numerics, so the
    values only have to be well formed. Generating them here keeps the check
    independent of run output, which no clean checkout contains.
    """
    monitor = case_dir / "3_monitor"
    check = case_dir / "4_check"
    monitor.mkdir(parents=True)
    check.mkdir(parents=True)

    # domain1_monitor_pt1_flow.dat: 3 header lines, then iteration, t, u, v, w, p, phi.
    with (monitor / "domain1_monitor_pt1_flow.dat").open("w") as fh:
        fh.write(" # domain-id :            1 pt-id :            1\n")
        fh.write(" # probe pts location    0.0    0.0    0.0\n")
        fh.write(" # iteration, t, u, v, w, p, phi\n")
        for i in range(1, NSAMPLES + 1):
            t = i * 1.0e-3
            fh.write(
                f"{i:12d}{t:14.5E}{1.0 + t:13.5E}{t:13.5E}{-t:13.5E}{t:13.5E}{t:13.5E}\n"
            )

    # Bulk-history logs. read_monitor_file() requires at least 11 and 15 columns.
    for name, ncol in (
        ("domain1_monitor_metrics_history.log", 11),
        ("domain1_monitor_change_history.log", 15),
    ):
        with (monitor / name).open("w") as fh:
            fh.write(" # domain-id :            1 pt-id :            0\n")
            fh.write(" # columns description:\n")
            for i in range(1, NSAMPLES + 1):
                t = i * 1.0e-3
                fh.write(" ".join(f"{t * (c + 1):13.5E}" for c in range(ncol)) + "\n")

    # check_mesh_yp.dat: index, yp, rp, rpi.   check_mesh_yc.dat: index, yc, growth, rc, rci.
    with (check / "check_mesh_yp.dat").open("w") as fh:
        fh.write(" index, yp, rp, rpi\n")
        for i in range(1, NNODES + 1):
            yp = -1.0 + 2.0 * (i - 1) / (NNODES - 1)
            fh.write(f"{i:12d}{yp:24.16f}{1.0:24.16f}{1.0:24.16f}\n")
    with (check / "check_mesh_yc.dat").open("w") as fh:
        fh.write(" index, yc, growth, rc, rci\n")
        for i in range(1, NNODES):
            yc = -1.0 + 2.0 * (i - 0.5) / (NNODES - 1)
            fh.write(f"{i:12d}{yc:24.16f}{1.0:24.16E}{1.0:24.16f}{1.0:24.16f}\n")


def main() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        out = Path(tmp) / "out"
        out.mkdir()
        cache = Path(tmp) / "cache"
        cache.mkdir()
        case_dir = Path(tmp) / "case"
        write_synthetic_case(case_dir)
        run(
            [
                PYTHON,
                str(VALIDATION_TOOLS / "plot_monitor_points.py"),
                "--case-dir",
                str(case_dir),
                "--num-points",
                "1",
                "--stride",
                "20",
                "--output-dir",
                str(out),
            ],
            cache,
        )
        require_file(out / "monitor_points_plot.png")

        run(
            [
                PYTHON,
                str(VALIDATION_TOOLS / "plot_monitor_points_seperate.py"),
                "--case-dir",
                str(case_dir),
                "--num-points",
                "1",
                "--stride",
                "20",
                "--output-dir",
                str(out),
            ],
            cache,
        )
        require_file(out / "monitor_point_1_plot.png")

        run(
            [
                PYTHON,
                str(VALIDATION_TOOLS / "plot_check_mesh.py"),
                "--case-dir",
                str(case_dir),
                "--output-dir",
                str(out),
            ],
            cache,
        )
        require_file(out / "mesh_check_plots.png")

        run(
            [
                PYTHON,
                str(VALIDATION_TOOLS / "plot_monitor_bulk_change_history.py"),
                "--case-dir",
                str(case_dir),
                "--output-dir",
                str(out),
            ],
            cache,
        )
        require_file(out / "monitor_history.png")

    print("Shared validation CLI checks OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
