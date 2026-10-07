#!/usr/bin/env python3
"""Check validation post-processing compatibility with current CHAPSim2 outputs."""

from __future__ import annotations

import re
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
VALIDATION_ROOT = REPO_ROOT / "validation"
WRITER_SOURCE = REPO_ROOT / "src" / "post_statistics.f90"
sys.path.insert(0, str(VALIDATION_ROOT / "tools"))

from chapsim_validation.profile import load_profile_column


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def writer_source() -> str:
    require(WRITER_SOURCE.is_file(), f"{WRITER_SOURCE} not found")
    return WRITER_SOURCE.read_text(encoding="utf-8")


def writer_header_literals() -> dict[str, str]:
    """The header lines write_visu_profile_bundle_ascii actually emits.

    Read out of the Fortran rather than restated here. A fixture built from
    the reader's own assumptions would make this check vacuous: it has to
    fail when the writer drifts, which is the only way the readers under
    validation/tools can be caught lagging behind the solver.
    """
    body = re.search(
        r"subroutine write_visu_profile_bundle_ascii\b(.*?)"
        r"end subroutine write_visu_profile_bundle_ascii",
        writer_source(),
        flags=re.DOTALL,
    )
    require(body is not None, "write_visu_profile_bundle_ascii not found in the writer source")

    literals = re.findall(r"'(#[^']*)'", body.group(1))
    found = {}
    for key in ("# format_version:", "# columns:"):
        match = next((lit for lit in literals if lit.startswith(key)), None)
        require(match is not None, f"writer no longer emits a '{key}' header line")
        found[key] = match
    return found


def coordinate_name_for_y() -> str:
    """profile_coordinate_name(YDIR) - it names both a column and the file."""
    body = re.search(
        r"function profile_coordinate_name\b(.*?)end function profile_coordinate_name",
        writer_source(),
        flags=re.DOTALL,
    )
    require(body is not None, "profile_coordinate_name not found")
    match = re.search(r"case\(YDIR\);\s*name\s*=\s*'([^']+)'", body.group(1))
    require(match is not None, "profile_coordinate_name has no YDIR case")
    return match.group(1)


def check_writer_reader_contract() -> None:
    """What load_profile_column needs the writer to keep doing."""
    headers = writer_header_literals()
    require(
        "CHAPSim_profile_ascii_v1" in headers["# format_version:"],
        "writer emits a format version the validation readers do not know: "
        f"{headers['# format_version:']!r}",
    )
    # _read_profile_columns anchors on this prefix and splits the rest on
    # whitespace, so the first two names have to stay index and the coordinate.
    require(
        headers["# columns:"].startswith("# columns: index "),
        f"'# columns:' header changed shape: {headers['# columns:']!r}",
    )
    # _candidate_bundle_paths globs domain<n>_tsp_avg_*_yprofile_<iter>.dat,
    # which only matches while profile_bundle_stem builds <name>_<dir>profile.
    require(
        "profile_direction_name(dir)//'profile'" in writer_source(),
        "profile_bundle_stem no longer builds '<name>_<dir>profile'; the "
        "reader's *_yprofile_* glob will stop matching",
    )


def write_bundle_fixture(path: Path, npoints: int, fields: list[str]) -> None:
    """A file in the format write_visu_profile_bundle_ascii emits."""
    coord = coordinate_name_for_y()
    lines = [
        "# CHAPSim2 time-and-space averaged profile",
        "# format_version: CHAPSim_profile_ascii_v1",
        "# direction: y",
        f"# coordinate: {coord}",
        f"# npoints: {npoints}",
        "# columns: index " + coord + " " + " ".join(fields),
    ]
    for j in range(1, npoints + 1):
        row = [f"{j:8d}", f"{-1.0 + 2.0 * (j - 0.5) / npoints:24.16E}"]
        row.extend(f"{0.1 * j * (n + 1):24.16E}" for n in range(len(fields)))
        lines.append(" ".join(row))
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def check_bundled_profile_reader() -> None:
    npoints = 16
    fields = ["tsp_avg_u1", "tsp_avg_u2", "tsp_avg_u3"]
    with tempfile.TemporaryDirectory() as tmp:
        tmpdir = Path(tmp)
        write_bundle_fixture(tmpdir / "domain1_tsp_avg_flow_yprofile_60.dat", npoints, fields)

        index, y, u1 = load_profile_column(tmpdir, "60", "u1")
        require(index.ndim == y.ndim == u1.ndim == 1, "profile arrays must be 1D")
        require(index.size == y.size == u1.size, "profile arrays must have matching length")
        require(index.size == npoints, "bundled profile reader returned the wrong row count")
        require(index[0] == 1.0 and index[-1] == float(npoints), "index column mismatch")

        # Columns must be selected by name, not position: u3 has to skip u1
        # and u2 rather than returning the first data column.
        _, _, u3 = load_profile_column(tmpdir, "60", "u3")
        require(
            abs(u1[0] - 0.1) < 1.0e-12 and abs(u3[0] - 0.3) < 1.0e-12,
            "bundled reader selected the wrong column for its field name",
        )

        # The qualified name must resolve to the same column as the bare one.
        _, _, u1_qualified = load_profile_column(tmpdir, "60", "tsp_avg_u1")
        require(bool((u1_qualified == u1).all()), "qualified and bare field names disagree")

        # A field the bundle does not carry must raise, not return a neighbour.
        try:
            load_profile_column(tmpdir, "60", "u9")
        except FileNotFoundError:
            pass
        else:
            raise AssertionError("bundled reader accepted a field that is not in the file")


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
    check_writer_reader_contract()
    check_bundled_profile_reader()
    check_split_profile_reader()
    check_monitor_change_indexes()
    print("Validation profile compatibility checks OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
