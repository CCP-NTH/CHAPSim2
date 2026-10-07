#!/usr/bin/env python3
"""Check that the validation framework layout has the expected assets."""

from __future__ import annotations

import subprocess
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
    "thermal_properties/README.md",
    "thermal_properties/generate_property_table.py",
    "tools/scripts/plot_monitor_bulk_change_history.py",
    "tools/scripts/plot_monitor_points.py",
    "tools/scripts/plot_monitor_points_seperate.py",
    "tools/scripts/plot_check_mesh.py",
    "tools/scripts/rebuild_tavg_from_new_start.py",
    "suites/smoke.yaml",
    "suites/regression_standard.yaml",
    "suites/regression_extended.yaml",
)


# Data deliberately not shipped: redistribution permission could not be
# established, so users fetch or generate it themselves. Its presence is not
# required and must not be asserted — a user who followed the instructions has
# it, a fresh clone does not, and both are correct. What is asserted is that
# the packaging decision stays documented and that an accidental `git add` of a
# local copy cannot redistribute it.
#
# Each entry is (path under validation/, README that must explain it).
WITHHELD_DATA = (
    ("references/channel/mkm/", "references/README.md"),
    ("references/pipe/eggels/", "references/README.md"),
    ("thermal_properties/NIST_CO2_8MP.DAT", "thermal_properties/README.md"),
)


def tracked_paths() -> set[str]:
    """Paths git tracks under validation/, or an empty set outside a checkout."""
    try:
        listing = subprocess.run(
            ["git", "-C", str(REPO_ROOT), "ls-files", "--", "validation"],
            capture_output=True,
            text=True,
            check=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return set()
    return set(listing.stdout.split())


def check_withheld_data() -> list[str]:
    problems = []

    ignored = (REPO_ROOT / ".gitignore").read_text().splitlines()
    tracked = tracked_paths()

    for path, readme_path in WITHHELD_DATA:
        # The README is named relative to validation/, so strip that prefix
        # before looking for the mention it is required to carry.
        mention = path[len(readme_path.rsplit("/", 1)[0]) + 1:]
        readme = (VALIDATION_ROOT / readme_path).read_text()
        if mention not in readme:
            problems.append(
                f"validation/{readme_path} no longer documents how to obtain, "
                f"or why CHAPSim2 omits, {mention}"
            )
        rule = f"validation/{path}"
        if rule not in ignored:
            problems.append(
                f".gitignore no longer carries '{rule}', so a local copy "
                f"of data CHAPSim2 may not redistribute could be committed"
            )
        # A .gitignore rule does not cover a path that is already tracked, so
        # the rule alone cannot prove the data is out of the repository.
        if rule in tracked:
            problems.append(
                f"validation/{path} is tracked by git despite being withheld "
                f"— .gitignore does not apply to an already-tracked path"
            )

    return problems


def main() -> int:
    missing = [path for path in REQUIRED_PATHS if not (VALIDATION_ROOT / path).exists()]

    if missing:
        print("Missing validation framework paths:")
        for path in missing:
            print(f"  - validation/{path}")
        return 1

    problems = check_withheld_data()
    if problems:
        print("Withheld data handling is inconsistent:")
        for problem in problems:
            print(f"  - {problem}")
        return 1

    print(
        f"Validation framework layout OK ({len(REQUIRED_PATHS)} paths checked, "
        f"{len(WITHHELD_DATA)} withheld datasets documented and ignored)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
