#!/usr/bin/env python3
"""Check validation post-processing compatibility with current CHAPSim2 outputs."""

from __future__ import annotations

import re
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
VALIDATION_ROOT = REPO_ROOT / "validation"
sys.path.insert(0, str(VALIDATION_ROOT / "tools"))

from chapsim_validation.profile import load_profile_column


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def check_bundled_profile_reader() -> None:
    for case_name in ("channel_iso_periodic", "pipe_iso_periodic"):
        data_dir = REPO_ROOT / "tests" / case_name / "2_visu" / "data"
        index, y, u1 = load_profile_column(data_dir, "60", "u1")
        require(index.ndim == y.ndim == u1.ndim == 1, "profile arrays must be 1D")
        require(index.size == y.size == u1.size, "profile arrays must have matching length")
        require(index.size > 10, f"bundled profile reader returned too few rows for {case_name}")


def check_split_profile_reader() -> None:
    with tempfile.TemporaryDirectory() as tmp:
        tmpdir = Path(tmp)
        (tmpdir / "domain1_tsp_avg_u1_60.dat").write_text(
            "1 -1.0 0.1\n2 0.0 0.2\n3 1.0 0.3\n",
            encoding="utf-8",
        )
        index, y, u1 = load_profile_column(tmpdir, "60", "u1")
        require(index.tolist() == [1.0, 2.0, 3.0], "split-file index column mismatch")
        require(y.tolist() == [-1.0, 0.0, 1.0], "split-file coordinate column mismatch")
        require(u1.tolist() == [0.1, 0.2, 0.3], "split-file value column mismatch")


def check_monitor_change_indexes() -> None:
    text = (VALIDATION_ROOT / "tools" / "scripts" / "plot_monitor_bulk_change_history.py").read_text()
    values = {
        name: int(match.group(1))
        for name in ("MIN_CHANGE_COLUMNS", "IDX_CHANGE_DMDT", "IDX_CHANGE_DKEDT")
        if (match := re.search(rf"^{name}\s*=\s*(\d+)", text, flags=re.MULTILINE))
    }
    require(values.get("MIN_CHANGE_COLUMNS", 0) >= 15, "change-history parser must accept current 15-column files")
    require(values.get("IDX_CHANGE_DMDT") == 7, "global mass imbalance is column 8 in current files")
    require(values.get("IDX_CHANGE_DKEDT") == 14, "kinetic-energy change rate is column 15 in current files")


def main() -> int:
    check_bundled_profile_reader()
    check_split_profile_reader()
    check_monitor_change_indexes()
    print("Validation profile compatibility checks OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
