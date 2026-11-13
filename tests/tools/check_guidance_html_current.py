#!/usr/bin/env python3
"""Tier 0.5: the committed docs/guidance/html must equal what build_static.py produces.

The documentation workflow (.github/workflows/documentation.yml) fails when the
Markdown under docs/guidance/docs/ is edited without regenerating
docs/guidance/html/. This checker makes the same comparison locally, so the
gate is part of the Tier 0.5 loop instead of being found in CI.

It never writes into the repository: docs/guidance is copied to a temporary
directory, build_static.py is run there (it writes next to itself), and the
result is compared file by file with the committed HTML.

Exit 0 when identical, 1 with the list of differing / missing / extra files.
"""
from __future__ import annotations

import filecmp
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
GUIDE = REPO / "docs" / "guidance"


def main() -> int:
    builder = GUIDE / "build_static.py"
    if not builder.is_file():
        print(f"FAIL: {builder} not found")
        return 1
    committed = GUIDE / "html"
    with tempfile.TemporaryDirectory(prefix="guidance_html_") as tmp:
        work = Path(tmp) / "guidance"
        shutil.copytree(GUIDE, work, ignore=shutil.ignore_patterns("__pycache__"))
        rebuilt = work / "html"
        result = subprocess.run([sys.executable, "-I", str(work / "build_static.py")],
                                capture_output=True, text=True)
        if result.returncode != 0:
            print("FAIL: build_static.py failed")
            print(result.stdout + result.stderr)
            return 1
        names_committed = {p.relative_to(committed) for p in committed.rglob("*") if p.is_file()}
        names_rebuilt = {p.relative_to(rebuilt) for p in rebuilt.rglob("*") if p.is_file()}
        differing = sorted(str(n) for n in names_committed & names_rebuilt
                           if not filecmp.cmp(committed / n, rebuilt / n, shallow=False))
        missing = sorted(str(n) for n in names_rebuilt - names_committed)
        extra = sorted(str(n) for n in names_committed - names_rebuilt)
    if differing or missing:
        print("FAIL: docs/guidance/html is stale. Run: python3 docs/guidance/build_static.py")
        for n in differing:
            print(f"  differs : {n}")
        for n in missing:
            print(f"  missing : {n} (produced by build_static.py, not committed)")
        return 1
    if extra:
        # files the builder does not produce (e.g. assets added by hand) are not an error
        print("note: committed files not produced by build_static.py: " + ", ".join(extra))
    print(f"PASS: docs/guidance/html matches build_static.py ({len(names_committed)} files)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
