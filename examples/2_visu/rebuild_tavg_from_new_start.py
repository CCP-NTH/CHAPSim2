#!/usr/bin/env python3

"""
Reconstruct CHAPSim2 time-averaged files with a new stat_istart.

--------------------------------------------------------------------------------
BACKGROUND
--------------------------------------------------------------------------------
CHAPSim computes running time averages as:

    A_N = ( (N-1)/N ) * A_{N-1} + (1/N) * a_N

which corresponds to:

    A_old(N) = sum(old_start+1 ... N) / (N - old_start)

If you want to change the averaging start from:

    old_start  →  new_start

you can reconstruct:

    A_new(N) = sum(new_start+1 ... N) / (N - new_start)

using:

    A_new(N) =
        [ (N - old_start) * A_old(N)
        - (new_start - old_start) * A_old(new_start) ]
        / (N - new_start)

--------------------------------------------------------------------------------
REQUIREMENTS
--------------------------------------------------------------------------------
You MUST have:
    - averaged file at new_start:
        domain1_tsp_avg_XXX_<new_start>.dat

    - averaged file(s) at later iteration(s):
        domain1_tsp_avg_XXX_<N>.dat

--------------------------------------------------------------------------------
USAGE
--------------------------------------------------------------------------------

Basic usage:

    python3 rebuild_tavg_from_new_start.py \
        --input-dir . \
        --output-dir corrected_avg \
        --old-start 100000 \
        --new-start 150000

Process specific iteration:

    python3 rebuild_tavg_from_new_start.py \
        --input-dir . \
        --output-dir corrected_avg \
        --old-start 100000 \
        --new-start 150000 \
        --target-iter 190000

Overwrite existing outputs:

    python3 rebuild_tavg_from_new_start.py \
        --input-dir . \
        --output-dir corrected_avg \
        --old-start 100000 \
        --new-start 150000 \
        --overwrite

--------------------------------------------------------------------------------
OUTPUT
--------------------------------------------------------------------------------
- Files are written to: ./corrected_avg/
- Only column 3 (variable) is modified
- Columns (time, y) are preserved

--------------------------------------------------------------------------------
NOTES
--------------------------------------------------------------------------------
- new_start must be > old_start
- target iteration must be > new_start
- If baseline file at new_start is missing → file is skipped
--------------------------------------------------------------------------------
"""

import argparse
import re
from pathlib import Path

import numpy as np


PATTERN = re.compile(r"^(domain\d+_tsp_avg_.+?)_(\d+)\.dat$")


def parse_args():
    parser = argparse.ArgumentParser(
        description="Reconstruct CHAPSim2 time-averaged files using a later stat_istart."
    )
    parser.add_argument("--input-dir", default=".", help="Directory containing old averaged files")
    parser.add_argument("--output-dir", default="new_tavg", help="Directory for corrected files")
    parser.add_argument("--old-start", type=int, required=True, help="Original stat_istart")
    parser.add_argument("--new-start", type=int, required=True, help="New stat_istart")
    parser.add_argument(
        "--target-iter",
        type=int,
        nargs="*",
        default=None,
        help="Target iterations to correct. If omitted, all iterations > new-start are processed.",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Allow overwriting files in output directory.",
    )
    return parser.parse_args()


def read_file(path):
    data = np.loadtxt(path)
    data = np.atleast_2d(data)

    if data.shape[1] < 3:
        raise ValueError(f"{path} must have at least 3 columns")

    return data


def write_file(path, data, overwrite=False):
    if path.exists() and not overwrite:
        raise FileExistsError(f"Output file already exists: {path}")

    np.savetxt(
        path,
        data,
        fmt=["%16.8E", "%16.8E", "%16.8E"],
        delimiter="  ",
    )


def collect_files(input_dir):
    files = {}

    for path in input_dir.glob("domain*_tsp_avg_*_*.dat"):
        match = PATTERN.match(path.name)
        if not match:
            continue

        stem = match.group(1)
        iteration = int(match.group(2))

        files.setdefault(stem, {})[iteration] = path

    return files


def correct_average(avg_old_target, avg_old_newstart, old_start, new_start, target_iter):
    n_old_target = target_iter - old_start
    n_old_newstart = new_start - old_start
    n_new_target = target_iter - new_start

    if n_new_target <= 0:
        raise ValueError("target_iter must be greater than new_start")

    return (
        n_old_target * avg_old_target
        - n_old_newstart * avg_old_newstart
    ) / n_new_target


def main():
    args = parse_args()

    input_dir = Path(args.input_dir).resolve()
    output_dir = Path(args.output_dir).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    if args.new_start <= args.old_start:
        raise ValueError("new-start must be greater than old-start")

    files = collect_files(input_dir)

    if not files:
        raise FileNotFoundError(f"No matching files found in {input_dir}")

    print(f"Input directory : {input_dir}")
    print(f"Output directory: {output_dir}")
    print(f"Old stat_istart : {args.old_start}")
    print(f"New stat_istart : {args.new_start}")
    print("")

    n_done = 0
    n_skip = 0

    for stem, iter_map in sorted(files.items()):
        if args.new_start not in iter_map:
            print(f"[SKIP] {stem}: missing baseline file at new_start={args.new_start}")
            n_skip += 1
            continue

        baseline_path = iter_map[args.new_start]
        baseline_data = read_file(baseline_path)
        baseline_var = baseline_data[:, 2]

        if args.target_iter is None:
            target_iters = sorted(i for i in iter_map if i > args.new_start)
        else:
            target_iters = [i for i in args.target_iter if i in iter_map and i > args.new_start]

        if not target_iters:
            print(f"[SKIP] {stem}: no target iterations found")
            n_skip += 1
            continue

        for target_iter in target_iters:
            target_path = iter_map[target_iter]
            target_data = read_file(target_path)

            if target_data.shape != baseline_data.shape:
                raise ValueError(
                    f"Shape mismatch:\n"
                    f"  baseline: {baseline_path} {baseline_data.shape}\n"
                    f"  target  : {target_path} {target_data.shape}"
                )

            corrected = target_data.copy()
            corrected[:, 2] = correct_average(
                avg_old_target=target_data[:, 2],
                avg_old_newstart=baseline_var,
                old_start=args.old_start,
                new_start=args.new_start,
                target_iter=target_iter,
            )

            output_path = output_dir / target_path.name
            write_file(output_path, corrected, overwrite=args.overwrite)

            print(f"[OK] {target_path.name} -> {output_path}")
            n_done += 1

    print("")
    print("=" * 60)
    print(f"Corrected files: {n_done}")
    print(f"Skipped groups : {n_skip}")
    print("=" * 60)


if __name__ == "__main__":
    main()