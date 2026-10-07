#!/usr/bin/env python3
"""Check that the validation framework layout has the expected assets."""

from __future__ import annotations

from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
VALIDATION_ROOT = REPO_ROOT / "validation"


REQUIRED_PATHS = (
    "README.md",
    "cases/channel/iso_periodic/README.md",
    "cases/channel/iso_periodic/case.yaml",
    "cases/channel/iso_periodic/post/1_data/postprocess_channel_wall_units.py",
    "cases/channel/iso_periodic/post/2_visu/plot_channel_velo_stress.py",
    "cases/channel/iso_periodic/post/2_visu/postprocess_channel_wall_units.py",
    "cases/pipe/iso_periodic/README.md",
    "cases/pipe/iso_periodic/case.yaml",
    "cases/pipe/iso_periodic/post/2_visu/plot_pipe_velo_stress.py",
    "cases/pipe/iso_periodic/post/2_visu/plot_pipe_velo_stress_v2.py",
    "references/channel/mkm/retau180/chan180.means",
    "references/channel/mkm/retau180/chan180.reystress",
    "references/channel/mkm/retau395/chan395.means",
    "references/channel/mkm/retau395/chan395.reystress",
    "references/pipe/tdl/retau180/dataverse_files.zip",
    "references/pipe/tdl/retau550/dataverse_files.zip",
    "references/pipe/eggels/reb5300/dnsEggels5300.asc",
    "tools/scripts/plot_monitor_bulk_change_history.py",
    "tools/scripts/plot_monitor_points.py",
    "tools/scripts/plot_monitor_points_seperate.py",
    "tools/scripts/plot_check_mesh.py",
    "tools/scripts/rebuild_tavg_from_new_start.py",
    "suites/smoke.yaml",
    "suites/regression_standard.yaml",
    "suites/regression_extended.yaml",
)


def main() -> int:
    missing = [path for path in REQUIRED_PATHS if not (VALIDATION_ROOT / path).exists()]

    if missing:
        print("Missing validation framework paths:")
        for path in missing:
            print(f"  - validation/{path}")
        return 1

    print(f"Validation framework layout OK ({len(REQUIRED_PATHS)} paths checked)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
