#!/usr/bin/env python3
"""
Plot CHAPSim2 monitor point data.

Creates one figure per monitor point. Each figure contains subplots for:
u, v, w, p, phi.

Each subplot shows:
- instantaneous/sampled value
- running average value
"""

import glob
import os

import matplotlib.pyplot as plt
import numpy as np


def read_monitor_data(filename, skip_rows=3):
    """Read monitor point data file."""
    if not os.path.exists(filename):
        print(f"File {filename} not found")
        return None

    try:
        data = np.genfromtxt(filename, skip_header=skip_rows, usecols=range(6))
        data = np.atleast_2d(data)

        if data.size == 0 or data.shape[1] < 6:
            print(f"No valid data read from {filename}")
            return None

        return data

    except Exception as e:
        print(f"Error reading {filename}: {e}")
        return None


def running_average(values):
    """Return cumulative running average."""
    return np.cumsum(values) / np.arange(1, len(values) + 1)


def plot_single_monitor_point(point_num, M, R):
    """Plot all variables for one monitor point."""
    filename = f"domain1_monitor_pt{point_num}_flow.dat"
    data = read_monitor_data(filename, skip_rows=R)

    if data is None:
        print(f"Cannot plot monitor point {point_num} - no valid data")
        return False

    print(f"Plotting monitor point {point_num}...")

    variables = ["u", "v", "w", "p", "phi"]

    time = data[::M, 0]

    fig, axes = plt.subplots(len(variables), 1, figsize=(12, 15), sharex=True)
    fig.suptitle(
        f"Monitor Point {point_num} - Instantaneous and Running Average",
        fontsize=16,
        fontweight="bold",
    )

    for j, var in enumerate(variables):
        variable_data = data[::M, j + 1]
        variable_avg = running_average(variable_data)

        axes[j].plot(
            time,
            variable_data,
            linestyle="-",
            linewidth=1.0,
            color=f"C{j}",
            alpha=0.35,
            label=f"{var} instant.",
        )

        axes[j].plot(
            time,
            variable_avg,
            linestyle="--",
            linewidth=2.0,
            color="black",
            alpha=0.9,
            label=f"{var} running avg.",
        )

        mean_val = np.mean(variable_data)
        std_val = np.std(variable_data)
        final_avg = variable_avg[-1]

        axes[j].set_ylabel(var, fontsize=12, fontweight="bold")
        axes[j].grid(True, alpha=0.3)
        axes[j].legend(fontsize=9, loc="best")
        axes[j].tick_params(axis="both", which="major", labelsize=10)

        axes[j].text(
            0.02,
            0.98,
            f"Mean: {mean_val:.4e}\nStd: {std_val:.4e}\nFinal avg: {final_avg:.4e}",
            transform=axes[j].transAxes,
            verticalalignment="top",
            bbox=dict(boxstyle="round", facecolor="white", alpha=0.8),
            fontsize=9,
        )

    axes[-1].set_xlabel("Time", fontsize=12, fontweight="bold")

    plt.tight_layout()

    output_filename = f"monitor_point_{point_num}_plot.png"
    plt.savefig(output_filename, dpi=300, bbox_inches="tight")
    plt.close()

    print(f"  - Saved as '{output_filename}'")
    return True


def plot_individual_monitor_points(N, M, R):
    """Plot each monitor point in a separate figure."""
    print(f"\nStarting to plot {N} monitor points...")
    print(f"Skipping {R} header/data rows.")
    print(f"Sampling interval: every {M} points\n")

    successful_plots = 0
    created_files = []

    for i in range(1, N + 1):
        success = plot_single_monitor_point(i, M, R)
        if success:
            successful_plots += 1
            created_files.append(f"monitor_point_{i}_plot.png")

    print("\n" + "=" * 60)
    print("PLOTTING COMPLETE")
    print("=" * 60)
    print(f"Successfully plotted: {successful_plots}/{N} monitor points")

    if created_files:
        print("\nCreated files:")
        for file in created_files:
            print(f"  - {file}")
    else:
        print("\nNo plots were created. Please check your data files.")

    return successful_plots


def list_available_files():
    """List available monitor point files in current directory."""
    pattern = "domain1_monitor_pt*_flow.dat"
    files = glob.glob(pattern)

    if not files:
        print("No monitor point files found matching pattern:")
        print(f"  {pattern}")
        return 0

    print(f"Found {len(files)} monitor point files:")
    for file in sorted(files):
        print(f"  - {file}")

    point_numbers = []
    for file in files:
        try:
            point_num = int(file.split("_pt")[1].split("_")[0])
            point_numbers.append(point_num)
        except Exception:
            continue

    if not point_numbers:
        print("Could not extract monitor point numbers from filenames.")
        return 0

    print(f"\nPoint number range: {min(point_numbers)} to {max(point_numbers)}")
    return max(point_numbers)


if __name__ == "__main__":
    print("Individual Monitor Point Plotter")
    print("=" * 40)
    print("Creates separate figures for each monitor point.")
    print("Each figure contains u, v, w, p, phi with running averages.\n")

    max_available = list_available_files()

    if max_available == 0:
        print("\nNo monitor point files found. Please check file names and location.")
        raise SystemExit(1)

    print("\n" + "-" * 40)

    while True:
        try:
            N = int(input(f"Enter the number of monitor points to plot (1-{max_available}): "))
            if 1 <= N <= max_available:
                break
            print(f"Please enter a number between 1 and {max_available}")
        except ValueError:
            print("Please enter a valid integer")

    while True:
        try:
            R = int(input("Enter number of lines to skip at the top of the files [3]: ") or 3)
            if R >= 0:
                break
            print("Please enter zero or a positive integer")
        except ValueError:
            print("Please enter a valid integer")

    while True:
        try:
            M = int(input("Enter sampling interval M [1]: ") or 1)
            if M > 0:
                break
            print("Please enter a positive integer")
        except ValueError:
            print("Please enter a valid integer")

    successful = plot_individual_monitor_points(N, M, R)

    if successful == 0:
        print("\nNo plots were created. Please check your input files.")
    else:
        print("\nPlotting completed successfully.")