#!/usr/bin/env python3
"""Check exact/compact restart-history mode wiring."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
MODULES = ROOT / "src" / "modules.f90"
INPUT_GENERAL = ROOT / "src" / "input_general.f90"
IO_RESTART = ROOT / "src" / "io_restart.f90"


def require(text: str, needle: str, description: str) -> None:
    if needle not in text:
        raise SystemExit(f"Missing {description}: {needle}")


def main() -> None:
    modules = MODULES.read_text()
    input_general = INPUT_GENERAL.read_text()
    io_restart = IO_RESTART.read_text()

    require(modules, "RESTART_HISTORY_EXACT", "exact restart-history enum")
    require(modules, "RESTART_HISTORY_COMPACT", "compact restart-history enum")
    require(modules, "restart_history_mode", "domain restart-history field")

    require(input_general, "parse_restart_history_mode", "restart-history parser")
    require(input_general, "'restart_history_mode'", "optional input key")
    require(
        input_general,
        "domain(1)%restart_history_mode = RESTART_HISTORY_EXACT",
        "exact default restart-history mode",
    )
    require(
        input_general,
        "domain(:)%restart_history_mode = domain(1)%restart_history_mode",
        "domain-wide restart-history propagation",
    )

    require(
        input_general,
        "restart_history_mode=compact is not supported for a thermal flow",
        "thermal compact restart rejection",
    )

    require(io_restart, "is_restart_history_exact", "restart-history exact predicate")
    require(io_restart, "flow_restart_fields_compact", "compact flow restart field list")
    require(io_restart, "read_flow_restart_bundle_compact", "compact flow restart reader")
    require(io_restart, "write_flow_restart_bundle_compact", "compact flow restart writer")
    require(
        io_restart,
        "call rebuild_compact_flow_restart(fl, dm)",
        "compact primitive/conservative flow reconstruction",
    )
    require(
        io_restart,
        "restart_history_mode=compact",
        "compact restart warning",
    )

    # Compact history is isothermal only; the thermal side must be gone.
    for gone in (
        "thermo_restart_fields_compact",
        "thermo_restart_shapes_compact",
        "read_thermo_restart_bundle_compact",
        "write_thermo_restart_bundle_compact",
        "rebuild_compact_thermo_restart",
    ):
        if gone in io_restart:
            raise SystemExit(f"Compact thermo restart leftover: {gone}")

    print("Restart history mode wiring checks OK")


if __name__ == "__main__":
    main()
