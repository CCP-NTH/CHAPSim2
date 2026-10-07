#!/usr/bin/env python3
"""Validate restart-equivalence functional test inputs."""

from __future__ import annotations

import configparser
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
RESTART_ROOT = ROOT / "functional" / "restart"


def read_ini(path: Path) -> configparser.ConfigParser:
    parser = configparser.ConfigParser()
    parser.optionxform = str.lower
    with path.open() as fh:
        parser.read_file(fh)
    return parser


def get_bool(parser: configparser.ConfigParser, section: str, key: str) -> bool:
    return parser.get(section, key).strip().lower() in {".true.", "true", "t", "1", "yes"}


def get_int(parser: configparser.ConfigParser, section: str, key: str) -> int:
    return int(parser.get(section, key).strip())


def main() -> int:
    errors: list[str] = []

    for case_dir in sorted(p for p in RESTART_ROOT.iterdir() if p.is_dir()):
        cont_path = case_dir / "run_continuous" / "input_chapsim.ini"
        rest_path = case_dir / "run_restart" / "input_chapsim.ini"

        if not cont_path.exists() or not rest_path.exists():
            continue

        cont = read_ini(cont_path)
        rest = read_ini(rest_path)

        case = case_dir.name
        restart_from = get_int(rest, "flow", "irestartfrom")
        expected_start = restart_from + 1

        if rest.get("flow", "initfl").strip().lower() != "restart":
            errors.append(f"{case}: run_restart [flow] initfl must be restart")

        if get_int(rest, "simcontrol", "niterflowfirst") != expected_start:
            errors.append(
                f"{case}: run_restart niterflowfirst must be {expected_start} "
                f"for irestartfrom={restart_from}"
            )

        # Two legitimate statistics settings, and nothing between them.
        #   stat_istart >= irestartfrom - the accumulators start empty, and the
        #     case covers the fresh-statistics path.
        #   stat_istart == the continuous run's - the stored averages and their
        #     sample count are reloaded and continued, so the case can compare
        #     t_avg_* against the uninterrupted run.
        # Any other value asks the solver to re-weight stored averages onto a
        # different window, which it refuses at runtime because the samples
        # already folded in cannot be unfolded.
        stat_istart = get_int(rest, "io", "stat_istart")
        cont_stat_istart = get_int(cont, "io", "stat_istart")
        if stat_istart < restart_from and stat_istart != cont_stat_istart:
            errors.append(
                f"{case}: run_restart stat_istart {stat_istart} must either be "
                f">= irestartfrom {restart_from} (start statistics afresh) or "
                f"equal the continuous run's {cont_stat_istart} (continue them)"
            )

        cont_flow_end = get_int(cont, "simcontrol", "niterflowlast")
        rest_flow_end = get_int(rest, "simcontrol", "niterflowlast")
        if rest_flow_end != cont_flow_end:
            errors.append(
                f"{case}: run_restart niterflowlast {rest_flow_end} must match "
                f"continuous niterflowlast {cont_flow_end}"
            )

        cont_has_thermo = cont.has_section("thermo") and get_bool(cont, "thermo", "ithermo")
        if cont_has_thermo:
            if rest.get("thermo", "inittm").strip().lower() != "restart":
                errors.append(f"{case}: run_restart [thermo] inittm must be restart")

            thermo_restart_from = get_int(rest, "thermo", "irestartfrom")
            if thermo_restart_from != restart_from:
                errors.append(
                    f"{case}: run_restart thermo irestartfrom {thermo_restart_from} "
                    f"must match flow irestartfrom {restart_from}"
                )

            thermo_start = get_int(rest, "simcontrol", "niterthermofirst")
            if thermo_start != expected_start:
                errors.append(
                    f"{case}: run_restart niterthermofirst must be {expected_start} "
                    f"for irestartfrom={restart_from}"
                )

            cont_thermo_end = get_int(cont, "simcontrol", "niterthermolast")
            rest_thermo_end = get_int(rest, "simcontrol", "niterthermolast")
            if rest_thermo_end != cont_thermo_end:
                errors.append(
                    f"{case}: run_restart niterthermolast {rest_thermo_end} must match "
                    f"continuous niterthermolast {cont_thermo_end}"
                )

    if errors:
        print("Restart functional input errors:")
        for error in errors:
            print(f"  - {error}")
        return 1

    print("Restart functional inputs OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
