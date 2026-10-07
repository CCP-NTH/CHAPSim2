#!/usr/bin/env python3
"""Check CHAPSim2 optional visualised-statistics output controls."""

from __future__ import annotations

from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]


def read(path: str) -> str:
    return (REPO_ROOT / path).read_text()


def require(text: str, pattern: str, description: str) -> None:
    if pattern not in text:
        raise AssertionError(f"missing {description}: {pattern}")


def main() -> int:
    modules = read("src/modules.f90")
    input_general = read("src/input_general.f90")
    chapsim = read("src/chapsim.f90")
    post_statistics = read("src/post_statistics.f90")
    template = read("prepost/input_generator/input_chapsim_complete.ini")
    docs = read("docs/guidance/docs/input-file.md")

    require(modules, "STAT_VISU_MODE_ALL", "all-mode enum")
    require(modules, "STAT_VISU_MODE_TSP_ONLY", "tsp-only mode enum")
    require(modules, "integer :: stat_visu_nfre", "stat_visu_nfre domain field")
    require(modules, "integer :: stat_visu_mode", "stat_visu_mode domain field")

    require(input_general, "parse_stat_visu_mode", "stat_visu_mode parser")
    require(input_general, "stat_visu_nfre", "stat_visu_nfre parser/default")
    require(input_general, "domain(1)%stat_visu_nfre = domain(1)%visu_nfre", "backward-compatible frequency default")
    require(input_general, "stat_visu_mode", "stat_visu_mode optional input")
    require(input_general, "count(domain(i)%is_periodic(1:3))", "tsp_only periodicity validation")

    require(chapsim, "domain(i)%stat_visu_nfre", "separate visualised-statistics schedule")
    require(chapsim, "write_visu_stats_flow", "flow visualised-statistics call")
    require(chapsim, "write_visu_stats_thermo", "thermo visualised-statistics call")
    require(chapsim, "write_visu_stats_mhd", "mhd visualised-statistics call")

    require(post_statistics, "dm%stat_visu_mode /= STAT_VISU_MODE_TSP_ONLY", "t_avg visualisation guard")
    require(post_statistics, "tsp_avg_flow", "flow tsp_avg output remains")
    require(post_statistics, "tsp_avg_thermo", "thermo tsp_avg output remains")
    require(post_statistics, "tsp_avg_mhd", "MHD tsp_avg output remains")

    require(template, "stat_visu_nfre", "template stat_visu_nfre")
    require(template, "stat_visu_mode", "template stat_visu_mode")
    require(docs, "stat_visu_nfre", "documented stat_visu_nfre")
    require(docs, "tsp_only", "documented tsp_only mode")

    print("Visualised-statistics control checks OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
