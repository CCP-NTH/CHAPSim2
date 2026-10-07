#!/usr/bin/env python3
"""Check MPI-enabled mesh-restart interpolation wiring."""

from __future__ import annotations

from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]


def read(path: str) -> str:
    return (REPO_ROOT / path).read_text()


def require(text: str, pattern: str, description: str) -> None:
    if pattern not in text:
        raise AssertionError(f"missing {description}: {pattern}")


def reject(text: str, pattern: str, description: str) -> None:
    if pattern in text:
        raise AssertionError(f"unexpected {description}: {pattern}")


def main() -> int:
    chapsim = read("src/chapsim.f90")
    decomposition = read("src/domain_decomposition.f90")
    interp = read("src/io_restart.f90")
    docs = read("docs/guidance/docs/mesh-restart.md")

    reject(chapsim, "nrank == 0 .and. is_prerun", "rank-0-only interpolation prerun")
    require(chapsim, "if(is_prerun) then", "collective interpolation prerun guard")

    require(decomposition, "periodic_bc=domain(1)%is_periodic", "periodic halo initialization")

    reject(interp, "Field interpolation and io are in serial mode only", "serial-only interpolation abort")
    require(interp, "use m_halo, only: update_halo", "2decomp halo API import")
    require(interp, "compute_interp_halo_level", "computed interpolation halo depth")
    require(interp, "trilinear_interp_point_halo", "halo-indexed interpolation point routine")
    require(interp, "opt_global=.true.", "global-indexed halo arrays")
    require(interp, "opt_pencil=IPENCIL(1)", "explicit x-pencil halo update")

    reject(docs, "run the source case in serial", "serial-only user guidance")
    require(docs, "can run the source case with MPI", "MPI interpolation guidance")

    print("MPI interpolation prerun checks OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
