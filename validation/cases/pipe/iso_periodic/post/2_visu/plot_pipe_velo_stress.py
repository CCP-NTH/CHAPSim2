#!/usr/bin/env python3
"""Compatibility wrapper for the current pipe validation plotter."""

from __future__ import annotations

import runpy
from pathlib import Path


def main() -> None:
    runpy.run_path(str(Path(__file__).with_name("plot_pipe_velo_stress_v2.py")), run_name="__main__")


if __name__ == "__main__":
    main()
