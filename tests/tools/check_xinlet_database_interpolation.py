#!/usr/bin/env python3
"""Check inlet database cross-section interpolation wiring."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "src" / "io_restart.f90"


def require(text: str, needle: str, description: str) -> None:
    if needle not in text:
        raise SystemExit(f"Missing {description}: {needle}")


def forbid_in_subroutine(text: str, subroutine: str, needle: str, description: str) -> None:
    start_token = f"subroutine {subroutine}"
    end_token = f"end subroutine {subroutine}"
    start = text.find(start_token)
    if start < 0:
        raise SystemExit(f"Missing subroutine: {subroutine}")
    end = text.find(end_token, start)
    if end < 0:
        raise SystemExit(f"Missing end subroutine: {subroutine}")
    body = text[start:end]
    if needle in body:
        raise SystemExit(f"Forbidden {description} in {subroutine}: {needle}")


def main() -> None:
    text = SRC.read_text()

    require(
        text,
        "read_xoutlet_database_per_field_interp",
        "interpolation-aware per-field inlet database read helper",
    )
    require(
        text,
        "infer_xoutlet_database_mesh",
        "source cross-section mesh inference before inlet database read",
    )
    require(
        text,
        "same mesh is recommended",
        "rank-0 warning for interpolated inlet database replay",
    )
    require(
        text,
        "interp_xinlet_database_array_yz",
        "cross-section interpolation routine for each buffered inlet plane",
    )
    require(
        text,
        "read_xoutlet_database_per_field_interp(dm, xoutlet_database_file_iter(dm, niter))",
        "read_instantaneous_xinlet call to interpolation-aware per-field path",
    )
    require(
        text,
        "read_xoutlet_database_bundle_interp",
        "interpolation-aware bundled inlet database read helper",
    )
    require(
        text,
        "infer_xoutlet_database_mesh_from_bundle",
        "bundled source cross-section mesh inference before inlet database read",
    )
    require(
        text,
        "iter_start",
        "bundle metadata database iteration start",
    )
    require(
        text,
        "time_end",
        "bundle metadata database time end",
    )
    require(
        text,
        "read_xoutlet_database_bundle_interp(dm, xoutlet_database_file_iter(dm, niter))",
        "read_instantaneous_xinlet call to interpolation-aware bundled path",
    )
    forbid_in_subroutine(
        text,
        "read_xoutlet_database_bundle",
        "validate_bundle_metadata",
        "full-domain restart metadata validation for xoutlet database",
    )

    print("Inlet database interpolation wiring checks OK")


if __name__ == "__main__":
    main()
