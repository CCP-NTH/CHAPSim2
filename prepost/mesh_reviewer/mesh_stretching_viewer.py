#!/usr/bin/env python3
"""
Interactive 1D mesh viewer for the y-direction stretching laws in geometry.f90.

This script mirrors the three CHAPSim mappings:
  - Buildup_grid_mapping_1D_3fmd
  - Buildup_grid_mapping_1D_tanh
  - Buildup_grid_mapping_1D_powerlaw

It focuses on the physical y-coordinate distribution only, which is usually the
first thing we want to inspect while tuning the stretching factor.
"""

from __future__ import annotations

import argparse
import math
import os
from dataclasses import dataclass

import numpy as np
os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")
import matplotlib.pyplot as plt
from matplotlib.widgets import RadioButtons, Slider, TextBox


METHODS = ("3fmd", "tanh", "powerlaw")
LOCATIONS = ("centre", "two-sides", "bottom", "top")
RSTRET_MIN = 1.0e-3
RSTRET_MAX = 1.0
N_MIN = 5
N_MAX = 216


@dataclass(frozen=True)
class DomainConfig:
    n: int = 65
    lyb: float = 0.0
    lyt: float = 1.0
    rstret: float = 0.1
    method: str = "3fmd"
    location: str = "centre"


def build_eta(n: int, grid_kind: str = "nd") -> np.ndarray:
    if grid_kind == "nd":
        return np.linspace(0.0, 1.0, n)
    if grid_kind == "cl":
        return (np.arange(n, dtype=float) + 0.5) / float(n)
    raise ValueError(f"Unknown grid kind: {grid_kind}")


def heaviside_step(x: float) -> float:
    return 0.0 if x < 0.0 else 1.0


def map_3fmd(eta: np.ndarray, rstret: float, lyb: float, lyt: float, location: str) -> np.ndarray:
    if location == "centre":
        raise ValueError("3fmd centre mode is not supported in the viewer because it produces a non-monotone mapping.")
    elif location == "two-sides":
        gamma = 1.0
        delta = 0.5
    elif location == "bottom":
        gamma = 0.5
        delta = 0.5
    elif location == "top":
        gamma = 0.5
        delta = 0.0
    else:
        raise ValueError(f"Unsupported 3fmd location: {location}")

    beta = rstret
    if beta <= 0.0:
        raise ValueError("3fmd requires rstret > 0.")

    alpha = 0.5 * (-1.0 + math.sqrt(1.0 + 4.0 * math.pi * math.pi * beta * beta)) / beta
    cc = math.sqrt(alpha * beta + 1.0) / math.sqrt(beta)
    dd = cc / math.sqrt(alpha)
    ee = cc * math.sqrt(alpha)

    st1 = (1.0 - 2.0 * delta) / gamma * 0.5
    st2 = (3.0 - 2.0 * delta) / gamma * 0.5

    y = np.empty_like(eta)
    for j, eta_j in enumerate(eta):
        mm = math.pi * (gamma * eta_j + delta)
        val = (
            math.atan(dd * math.tan(mm))
            - math.atan(dd * math.tan(math.pi * delta))
            + math.pi * (heaviside_step(eta_j - st1) + heaviside_step(eta_j - st2))
        )
        y[j] = val / (gamma * ee)

    y[0] = 0.0
    y[-1] = 1.0
    return y * (lyt - lyb) + lyb


def map_tanh(eta: np.ndarray, rstret: float, lyb: float, lyt: float, location: str) -> np.ndarray:
    if location == "centre":
        raise ValueError("tanh centre mode is not supported by the current geometry.f90.")
    beta = rstret * 20.0
    if location == "two-sides":
        mm = math.tanh(beta * 0.5)
        y_hat = 0.5 * (1.0 + np.tanh(beta * (eta - 0.5)) / mm)
    elif location == "bottom":
        mm = math.tanh(beta)
        y_hat = 1.0 - np.tanh(beta * (1.0 - eta)) / mm
    elif location == "top":
        mm = math.tanh(beta)
        y_hat = np.tanh(beta * eta) / mm
    else:
        raise ValueError(f"Unsupported tanh location: {location}")
    return y_hat * (lyt - lyb) + lyb


def map_powerlaw(eta: np.ndarray, rstret: float, lyb: float, lyt: float, location: str) -> np.ndarray:
    # Keep the UI range in [0, 1]: smaller values mean stronger clustering,
    # while rstret=1 gives a nearly uniform power-law mesh.
    exponent = 1.0 / max(rstret, RSTRET_MIN)

    if location == "centre":
        raise ValueError("powerlaw centre mode is not supported in the viewer.")
    if location == "two-sides":
        base = np.where(
            eta <= 0.5,
            0.5 * (2.0 * eta) ** exponent,
            1.0 - 0.5 * (2.0 * (1.0 - eta)) ** exponent,
        )
    elif location == "bottom":
        base = eta ** exponent
    elif location == "top":
        base = 1.0 - (1.0 - eta) ** exponent
    else:
        raise ValueError(f"Unsupported powerlaw location: {location}")

    return base * (lyt - lyb) + lyb


def compute_y(cfg: DomainConfig) -> np.ndarray:
    eta = build_eta(cfg.n, "nd")
    if cfg.method == "3fmd":
        return map_3fmd(eta, cfg.rstret, cfg.lyb, cfg.lyt, cfg.location)
    if cfg.method == "tanh":
        return map_tanh(eta, cfg.rstret, cfg.lyb, cfg.lyt, cfg.location)
    if cfg.method == "powerlaw":
        return map_powerlaw(eta, cfg.rstret, cfg.lyb, cfg.lyt, cfg.location)
    raise ValueError(f"Unsupported method: {cfg.method}")


def allowed_location(method: str, location: str) -> str:
    return location


def draw_mesh(ax: plt.Axes, y: np.ndarray, title: str) -> None:
    ax.clear()
    ax.hlines(0.0, y[0], y[-1], color="0.85", linewidth=2.0)
    ax.plot(y, np.zeros_like(y), "|", color="tab:blue", markersize=18, markeredgewidth=1.5)
    ax.set_ylim(-0.3, 0.3)
    ax.set_yticks([])
    ax.set_xlabel("y")
    ax.set_title(title)
    ax.grid(True, axis="x", linestyle="--", alpha=0.3)


def draw_mapping(ax: plt.Axes, eta: np.ndarray, y: np.ndarray) -> None:
    ax.clear()
    ax.plot(eta, y, color="tab:red", linewidth=2.0)
    ax.scatter(eta, y, color="tab:red", s=16)
    ax.set_xlabel("Computational coordinate eta")
    ax.set_ylabel("Physical coordinate y")
    ax.set_title("Mapping y(eta)")
    ax.grid(True, linestyle="--", alpha=0.3)


def draw_spacing(ax: plt.Axes, y: np.ndarray) -> None:
    ax.clear()
    dy = np.diff(y)
    idx = np.arange(1, len(y))
    ax.plot(idx, dy, color="tab:green", linewidth=2.0)
    ax.scatter(idx, dy, color="tab:green", s=16)
    ax.set_xlabel("Cell interval index")
    ax.set_ylabel("Delta y")
    ax.set_title("Mesh spacing")
    ax.grid(True, linestyle="--", alpha=0.3)


def make_plot(initial: DomainConfig) -> None:
    fig = plt.figure(figsize=(13, 8))
    fig.suptitle("CHAPSim2 Y-Direction Mesh Evaluation", fontsize=16, fontweight="bold", y=0.985)
    gs = fig.add_gridspec(
        3,
        3,
        width_ratios=[1.2, 3.0, 3.0],
        height_ratios=[1.0, 1.0, 0.25],
        left=0.06,
        right=0.98,
        bottom=0.18,
        top=0.91,
        wspace=0.35,
        hspace=0.45,
    )

    ax_method = fig.add_subplot(gs[0, 0])
    ax_location = fig.add_subplot(gs[1, 0])
    ax_mesh = fig.add_subplot(gs[0, 1:])
    ax_mapping = fig.add_subplot(gs[1, 1])
    ax_spacing = fig.add_subplot(gs[1, 2])

    radio_method = RadioButtons(ax_method, METHODS, active=METHODS.index(initial.method))
    radio_location = RadioButtons(ax_location, LOCATIONS, active=LOCATIONS.index(initial.location))

    ax_slider_rstret = fig.add_axes([0.17, 0.09, 0.50, 0.03])
    ax_box_rstret = fig.add_axes([0.77, 0.085, 0.10, 0.045])
    ax_slider_n = fig.add_axes([0.17, 0.04, 0.50, 0.03])
    ax_box_n = fig.add_axes([0.77, 0.035, 0.10, 0.045])
    slider_rstret = Slider(
        ax_slider_rstret,
        "stretch factor",
        RSTRET_MIN,
        RSTRET_MAX,
        valinit=min(max(initial.rstret, RSTRET_MIN), RSTRET_MAX),
        valstep=0.001,
    )
    slider_n = Slider(
        ax_slider_n,
        "n points",
        N_MIN,
        N_MAX,
        valinit=min(max(initial.n, N_MIN), N_MAX),
        valstep=1,
    )
    box_rstret = TextBox(ax_box_rstret, "value", initial=f"{slider_rstret.val:.3f}")
    box_n = TextBox(ax_box_n, "value", initial=f"{int(slider_n.val)}")

    note = fig.text(
        0.17,
        0.135,
        "",
        fontsize=10,
        color="0.25",
    )
    sync_state = {"active": False}

    def set_rstret_value(value: float) -> None:
        clipped = min(max(value, RSTRET_MIN), RSTRET_MAX)
        sync_state["active"] = True
        slider_rstret.set_val(clipped)
        box_rstret.set_val(f"{clipped:.3f}")
        sync_state["active"] = False

    def set_n_value(value: int) -> None:
        clipped = min(max(int(value), N_MIN), N_MAX)
        sync_state["active"] = True
        slider_n.set_val(clipped)
        box_n.set_val(f"{clipped}")
        sync_state["active"] = False

    def refresh(_: object | None = None) -> None:
        if sync_state["active"]:
            return
        method = radio_method.value_selected
        location = allowed_location(method, radio_location.value_selected)
        n = int(round(slider_n.val))
        rstret = float(slider_rstret.val)
        cfg = DomainConfig(
            n=n,
            lyb=initial.lyb,
            lyt=initial.lyt,
            rstret=rstret,
            method=method,
            location=location,
        )

        eta = build_eta(cfg.n, "nd")
        try:
            y = compute_y(cfg)
        except ValueError as exc:
            ax_mesh.clear()
            ax_mapping.clear()
            ax_spacing.clear()
            note.set_text(str(exc))
            fig.canvas.draw_idle()
            return
        draw_mesh(ax_mesh, y, f"{method} mesh, location={location}, n={cfg.n}, stretch={cfg.rstret:.2f}")
        draw_mapping(ax_mapping, eta, y)
        draw_spacing(ax_spacing, y)

        if np.any(np.diff(y) <= 0.0):
            note.set_text("Warning: the mapped y array is not strictly monotone for this setup, matching the current geometry.f90 behaviour.")
        elif method == "3fmd" and location == "centre":
            note.set_text("3fmd centre is disabled because it causes a jump/non-monotone mapping.")
        elif method == "tanh" and location == "centre":
            note.set_text("tanh centre is not supported in the current geometry.f90.")
        elif method == "powerlaw" and location == "centre":
            note.set_text("powerlaw centre is not supported in the viewer.")
        elif method == "powerlaw":
            note.set_text(f"powerlaw uses effective exponent = {1.0 / max(cfg.rstret, RSTRET_MIN):.3f}.")
        else:
            note.set_text("")

        fig.canvas.draw_idle()

    def submit_rstret(text: str) -> None:
        try:
            value = float(text.strip())
        except ValueError:
            box_rstret.set_val(f"{slider_rstret.val:.3f}")
            return
        set_rstret_value(value)
        refresh()

    def submit_n(text: str) -> None:
        try:
            value = int(float(text.strip()))
        except ValueError:
            box_n.set_val(f"{int(round(slider_n.val))}")
            return
        set_n_value(value)
        refresh()

    radio_method.on_clicked(refresh)
    radio_location.on_clicked(refresh)
    slider_rstret.on_changed(refresh)
    slider_n.on_changed(refresh)
    box_rstret.on_submit(submit_rstret)
    box_n.on_submit(submit_n)
    set_rstret_value(slider_rstret.val)
    set_n_value(int(round(slider_n.val)))
    refresh()
    plt.show()


def parse_args() -> DomainConfig:
    parser = argparse.ArgumentParser(description="Visualize CHAPSim 1D y-mesh stretching.")
    parser.add_argument("--method", choices=METHODS, default="3fmd")
    parser.add_argument("--location", choices=LOCATIONS, default="centre")
    parser.add_argument("--n", type=int, default=65, help="Number of node points.")
    parser.add_argument("--rstret", type=float, default=0.1, help="Stretching factor.")
    parser.add_argument("--lyb", type=float, default=0.0, help="Lower y bound.")
    parser.add_argument("--lyt", type=float, default=1.0, help="Upper y bound.")
    args = parser.parse_args()

    if args.n < N_MIN or args.n > N_MAX:
        raise ValueError(f"n must be between {N_MIN} and {N_MAX}.")
    if args.lyt <= args.lyb:
        raise ValueError("lyt must be greater than lyb.")
    if args.rstret < RSTRET_MIN or args.rstret > RSTRET_MAX:
        args.rstret = min(max(args.rstret, RSTRET_MIN), RSTRET_MAX)

    return DomainConfig(
        n=args.n,
        lyb=args.lyb,
        lyt=args.lyt,
        rstret=args.rstret,
        method=args.method,
        location=allowed_location(args.method, args.location),
    )


if __name__ == "__main__":
    make_plot(parse_args())
