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
    "references/pipe/tdl/retau180/dataverse_files.zip",
    "references/pipe/tdl/retau550/dataverse_files.zip",
    "tools/scripts/plot_monitor_bulk_change_history.py",
    "tools/scripts/plot_monitor_points.py",
    "tools/scripts/plot_monitor_points_seperate.py",
    "tools/scripts/plot_check_mesh.py",
    "tools/scripts/rebuild_tavg_from_new_start.py",
    "suites/smoke.yaml",
    "suites/regression_standard.yaml",
    "suites/regression_extended.yaml",
)


# Datasets deliberately not shipped: their redistribution terms could not be
# established, so users fetch them themselves. Their presence is not required
# and must not be asserted — a user who followed the download instructions has
# them, a fresh clone does not, and both are correct. What is asserted is that
# the packaging decision stays documented and that an accidental `git add` of a
# local download cannot redistribute them.
WITHHELD_DATASETS = (
    "channel/mkm/",
    "pipe/eggels/",
)


def check_withheld_datasets() -> list[str]:
    problems = []

    readme = (VALIDATION_ROOT / "references" / "README.md").read_text()
    ignored = (REPO_ROOT / ".gitignore").read_text().splitlines()

    for dataset in WITHHELD_DATASETS:
        if dataset not in readme:
            problems.append(
                f"validation/references/README.md no longer documents how to "
                f"obtain, or why CHAPSim2 omits, references/{dataset}"
            )
        rule = f"validation/references/{dataset}"
        if rule not in ignored:
            problems.append(
                f".gitignore no longer carries '{rule}', so a local download "
                f"of data CHAPSim2 may not redistribute could be committed"
            )

    return problems


def main() -> int:
    missing = [path for path in REQUIRED_PATHS if not (VALIDATION_ROOT / path).exists()]

    if missing:
        print("Missing validation framework paths:")
        for path in missing:
            print(f"  - validation/{path}")
        return 1

    problems = check_withheld_datasets()
    if problems:
        print("Withheld reference dataset handling is inconsistent:")
        for problem in problems:
            print(f"  - {problem}")
        return 1

    print(
        f"Validation framework layout OK ({len(REQUIRED_PATHS)} paths checked, "
        f"{len(WITHHELD_DATASETS)} withheld datasets documented and ignored)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
