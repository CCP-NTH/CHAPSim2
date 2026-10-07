#!/usr/bin/env python3
"""Validate the tests/ runner layout after splitting suites into subfolders."""

from __future__ import annotations

import os
import re
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
TESTS_ROOT = REPO_ROOT / "tests"
REGRESSION_ROOT = TESTS_ROOT / "regression"
FUNCTIONAL_ROOT = TESTS_ROOT / "functional"


def require_path(path: Path, description: str) -> None:
    if not path.exists():
        raise SystemExit(f"Missing {description}: {path.relative_to(REPO_ROOT)}")


def require_executable(path: Path, description: str) -> None:
    require_path(path, description)
    if not os.access(path, os.X_OK):
        raise SystemExit(f"Not executable {description}: {path.relative_to(REPO_ROOT)}")


def shell_array(script: Path, name: str) -> list[str]:
    text = script.read_text()
    match = re.search(rf"^{name}=\(\n(?P<body>.*?)\n\)", text, re.MULTILINE | re.DOTALL)
    if not match:
        raise SystemExit(f"Missing shell array {name} in {script.relative_to(REPO_ROOT)}")

    cases: list[str] = []
    for raw_line in match.group("body").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        cases.append(line)
    return cases


def check_shared_case_lists(case_lists: Path) -> None:
    """Every runner must source case_lists.sh rather than carry its own copy.

    The ARCHER2 driver used to hardcode a second list, which drifted to 17 cases
    against the local 32; a runner that re-defines one of the arrays would
    silently shadow the shared definition in exactly the same way.
    """
    for runner in ("run_regression.sh", "run_regression_tests_archer2.sh"):
        script = REGRESSION_ROOT / runner
        require_path(script, "regression runner")
        text = script.read_text()
        if f"/{case_lists.name}" not in text:
            raise SystemExit(
                f"{script.relative_to(REPO_ROOT)} does not source {case_lists.name}"
            )
        for array_name in ("STANDARD_CASES", "EXTENDED_CASES", "FUNCTIONAL_CASES"):
            if re.search(rf"^{array_name}=\(", text, re.MULTILINE):
                raise SystemExit(
                    f"{script.relative_to(REPO_ROOT)} re-defines {array_name}; it "
                    f"must come from {case_lists.name} only"
                )


def check_run_chapsim_exec_paths() -> None:
    for script in sorted(TESTS_ROOT.rglob("run_chapsim.sh")):
        if any(part in {"1_data", "2_visu", "3_monitor", "4_check"} for part in script.parts):
            continue
        text = script.read_text()
        match = re.search(r'^EXEC="(?P<exec>[^"]+)"', text, re.MULTILINE)
        if not match:
            continue
        # A script under common/ is a template, copied verbatim into each
        # <case>/<step>/ run directory one level deeper. Its relative path has to
        # be correct where it is deployed, not where it rests.
        base = script.parent
        if base.name == "common":
            base = base / "step"
        exe_path = (base / match.group("exec")).resolve()
        expected = (REPO_ROOT / "bin" / "CHAPSim").resolve()
        if exe_path != expected:
            raise SystemExit(
                "Wrong CHAPSim executable path in "
                f"{script.relative_to(REPO_ROOT)}: {match.group('exec')} -> {exe_path}"
            )


def main() -> int:
    require_path(REGRESSION_ROOT, "regression test directory")
    require_path(FUNCTIONAL_ROOT, "functional test directory")
    require_executable(TESTS_ROOT / "run_regression.sh", "top-level regression runner")
    require_executable(TESTS_ROOT / "run_smoke.sh", "top-level smoke runner")
    require_executable(TESTS_ROOT / "run_functional.sh", "top-level functional runner")
    require_executable(REGRESSION_ROOT / "run_regression.sh", "regression suite runner")
    require_executable(REGRESSION_ROOT / "run_smoke.sh", "smoke suite runner")
    require_executable(FUNCTIONAL_ROOT / "restart" / "run_functional.sh", "restart functional runner")
    require_path(TESTS_ROOT / "tools" / "check_metrics.py", "metrics checker")
    require_path(TESTS_ROOT / "tools" / "tolerances.json", "regression tolerances")

    # The three case arrays live in case_lists.sh, the single definition that every
    # runner sources (run_regression.sh locally, the ARCHER2 driver on HPC). Reading
    # them from a runner would validate only that runner's copy.
    case_lists = REGRESSION_ROOT / "case_lists.sh"
    require_path(case_lists, "regression case list")
    check_shared_case_lists(case_lists)

    for array_name in ("STANDARD_CASES", "EXTENDED_CASES"):
        for case in shell_array(case_lists, array_name):
            require_path(REGRESSION_ROOT / case, f"{array_name} case")

    # Paths in FUNCTIONAL_CASES are relative to tests/regression/, not to tests/.
    for case in shell_array(case_lists, "FUNCTIONAL_CASES"):
        require_path(REGRESSION_ROOT / case, "optional functional case")

    check_run_chapsim_exec_paths()

    print("Tests runner layout OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
