"""
README
------
Plot CHAPSim2 pipe statistics against TDL pipe reference data.

Supported plot groups:
- velocity: ``ux``, ``uy``, ``uz``
- pressure: ``pr``
- stress: ``uu``, ``uv``, ``uw``, ``vv``, ``vw``, ``ww``
- vorticity: ``omega_r``, ``omega_theta``, ``omega_z``

CHAPSim pipe variable mapping used here:
- ``u1`` = axial velocity ``u_z``
- ``u2`` = stored radial momentum-like variable, usually ``r * u_r``
- ``u3`` = azimuthal velocity ``u_theta``

Mean vorticity is built from the mean velocity profiles using cylindrical relations:
- ``omega_r = 0`` for fully developed axisymmetric mean pipe flow
- ``omega_theta = -d(u_z)/dr``
- ``omega_z = (1/r) d(r u_theta)/dr``

The TDL file ``PIPE_Re180_VORT_PRES_FLUC.dat`` contains fluctuation-vorticity data,
not mean vorticity. This script therefore supports two reference modes for the
vorticity group:
- ``mean``: compare against mean vorticity derived from ``PIPE_Re*_MEAN.dat``
- ``fluctuation``: compare against RMS reference curves from
  ``PIPE_Re*_VORT_PRES_FLUC.dat``; in this mode the CHAPSim curve is still the
  mean vorticity, because the present ``domain1_tsp_avg_*`` files do not contain
  enough information to reconstruct vorticity RMS consistently.

Examples
--------
python3 plot_pipe_velo_stress_v2.py --dns-time 95000 --re 2650
python3 plot_pipe_velo_stress_v2.py --dns-time 95000 --re 2650 --groups velocity stress
python3 plot_pipe_velo_stress_v2.py --dns-time 95000 --re 2650 --groups vorticity --vorticity-ref-mode fluctuation
"""

from __future__ import annotations

import argparse
import io
import math
import shutil
import tempfile
import zipfile
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from pylab import rcParams


FILEMAP_MEAN = {"ux": "u1", "uy": "u2", "uz": "u3", "pr": "pr"}
FILEMAP_REY = {
    "uu": "uu11",
    "uv": "uu12",
    "uw": "uu13",
    "vv": "uu22",
    "vw": "uu23",
    "ww": "uu33",
}

PLOT_GROUPS = {
    "velocity": ("ux", "uy", "uz"),
    "pressure": ("pr",),
    "stress": ("uu", "uv", "uw", "vv", "vw", "ww"),
    "vorticity": ("omega_r", "omega_theta", "omega_z"),
}

PARAMS = {
    "ux": {
        "group": "velocity",
        "ref_kind": "mean",
        "ref_key": "Uz",
        "ylabel": r"$u_z^+$",
        "title": "Mean Axial Velocity",
    },
    "uy": {
        "group": "velocity",
        "ref_kind": "mean",
        "ref_key": "Ur",
        "ylabel": r"$u_r^+$",
        "title": "Mean Radial Velocity",
    },
    "uz": {
        "group": "velocity",
        "ref_kind": "mean",
        "ref_key": "Ut",
        "ylabel": r"$u_\theta^+$",
        "title": "Mean Azimuthal Velocity",
    },
    "pr": {
        "group": "pressure",
        "ref_kind": "mean",
        "ref_key": "P",
        "ylabel": r"$p^+$",
        "title": "Mean Pressure",
    },
    "uu": {
        "group": "stress",
        "ref_kind": "rms",
        "ref_key": "uzuz",
        "ylabel": r"$\overline{u_z^\prime u_z^\prime}^+$",
        "title": "Reynolds Stress $u_zu_z$",
    },
    "uv": {
        "group": "stress",
        "ref_kind": "rms",
        "ref_key": "uruz",
        "ylabel": r"$\overline{u_z^\prime u_r^\prime}^+$",
        "title": "Reynolds Stress $u_zu_r$",
    },
    "uw": {
        "group": "stress",
        "ref_kind": "rms",
        "ref_key": "utuz",
        "ylabel": r"$\overline{u_z^\prime u_\theta^\prime}^+$",
        "title": "Reynolds Stress $u_zu_\\theta$",
    },
    "vv": {
        "group": "stress",
        "ref_kind": "rms",
        "ref_key": "urur",
        "ylabel": r"$\overline{u_r^\prime u_r^\prime}^+$",
        "title": "Reynolds Stress $u_ru_r$",
    },
    "vw": {
        "group": "stress",
        "ref_kind": "rms",
        "ref_key": "urut",
        "ylabel": r"$\overline{u_r^\prime u_\theta^\prime}^+$",
        "title": "Reynolds Stress $u_ru_\\theta$",
    },
    "ww": {
        "group": "stress",
        "ref_kind": "rms",
        "ref_key": "utut",
        "ylabel": r"$\overline{u_\theta^\prime u_\theta^\prime}^+$",
        "title": "Reynolds Stress $u_\\theta u_\\theta$",
    },
    "omega_r": {
        "group": "vorticity",
        "ylabel": r"$\omega_r^+$",
        "title": "Vorticity $\\omega_r$",
    },
    "omega_theta": {
        "group": "vorticity",
        "ylabel": r"$\omega_\theta^+$",
        "title": "Vorticity $\\omega_\\theta$",
    },
    "omega_z": {
        "group": "vorticity",
        "ylabel": r"$\omega_z^+$",
        "title": "Vorticity $\\omega_z$",
    },
}

MEAN_COLS = ["r", "one_minus_r", "yplus", "Ur", "Ut", "Uz", "dUz_dr_plus", "P"]
RMS_COLS = ["r", "one_minus_r", "yplus", "urur", "utut", "uzuz", "urut", "uruz", "utuz"]
VORT_FLUC_COLS = [
    "r",
    "one_minus_r",
    "yplus",
    "oror",
    "otot",
    "ozoz",
    "orot",
    "oroz",
    "otoz",
    "pp",
]

cbrg = plt.get_cmap("brg")
mlst = ["o", "<", "*", "v", "^", ">", "1", "2", "3", "4", "x", "s", "8", "+"]
plt.rc("figure", facecolor="white")
plt.rc("legend", fontsize=13)
rcParams["legend.loc"] = "best"

FIGSIZE = (9, 6)
DPI = 350
REF_SKIPROWS = 8


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Plot CHAPSim2 pipe velocity, pressure, stress, and vorticity profiles."
    )
    parser.add_argument("--dns-time", required=True, help="Statistics iteration label, e.g. 95000.")
    parser.add_argument(
        "--re",
        type=float,
        default=2650.0,
        help="Bulk Reynolds number used in the simulation, e.g. 2650.",
    )
    parser.add_argument(
        "--ref-retau",
        type=int,
        default=None,
        help="Reference friction Reynolds number family, e.g. 180.",
    )
    parser.add_argument(
        "--groups",
        nargs="+",
        choices=("all", "velocity", "pressure", "stress", "vorticity"),
        default=("all",),
        help="Plot groups to generate. Default: all.",
    )
    parser.add_argument(
        "--input-dir",
        default="../1_data",
        help="Directory containing domain1_tsp_avg_*.dat files.",
    )
    parser.add_argument(
        "--ref-dir",
        default="../../../TDL180",
        help="Directory containing extracted TDL files or the TDL zip archive.",
    )
    parser.add_argument(
        "--output-dir",
        default=".",
        help="Directory for generated figures.",
    )
    parser.add_argument(
        "--radial-stored-as-r-times-qr",
        action="store_true",
        default=True,
        help="Interpret u2/uu22/uu12/uu23 as stored with one factor of r. Default: true.",
    )
    parser.add_argument(
        "--radial-stored-directly",
        action="store_true",
        help="Interpret u2/uu22/uu12/uu23 as stored directly as u_r-based quantities.",
    )
    parser.add_argument(
        "--vorticity-ref-mode",
        choices=("mean", "fluctuation"),
        default="mean",
        help="Reference source for vorticity plots. Default: mean.",
    )
    return parser.parse_args()


def safe_divide(num: np.ndarray, den: np.ndarray, eps: float = 1.0e-14) -> np.ndarray:
    out = np.zeros_like(num, dtype=float)
    mask = np.abs(den) > eps
    out[mask] = num[mask] / den[mask]
    return out


def derivative(values: np.ndarray, x: np.ndarray) -> np.ndarray:
    edge_order = 2 if len(x) >= 3 else 1
    return np.gradient(values, x, edge_order=edge_order)


class PipeReferenceStore:
    def __init__(self, ref_dir: Path):
        self.ref_dir = ref_dir
        self.zip_path = self._find_zip()

    def _find_zip(self) -> Path | None:
        zips = sorted(self.ref_dir.glob("*.zip"))
        return zips[0] if zips else None

    def available_retaus(self) -> list[int]:
        names: list[str] = []
        if self.zip_path is not None:
            with zipfile.ZipFile(self.zip_path) as zf:
                names = zf.namelist()
        else:
            names = [path.name for path in self.ref_dir.glob("PIPE_Re*_MEAN.dat")]

        retaus = set()
        for name in names:
            if "PIPE_Re" not in name or "_MEAN.dat" not in name:
                continue
            start = name.index("PIPE_Re") + len("PIPE_Re")
            end = name.index("_MEAN.dat")
            retau_str = name[start:end]
            if retau_str.isdigit():
                retaus.add(int(retau_str))
        return sorted(retaus)

    def _read_text(self, filename: str) -> str:
        direct = self.ref_dir / filename
        if direct.exists():
            return direct.read_text()

        if self.zip_path is None:
            raise FileNotFoundError(f"Reference file not found: {direct}")

        with zipfile.ZipFile(self.zip_path) as zf:
            try:
                return zf.read(filename).decode("utf-8")
            except KeyError as exc:
                raise FileNotFoundError(f"{filename} not found in {self.zip_path}") from exc

    def load_table(self, filename: str, names: list[str]) -> np.ndarray:
        text = self._read_text(filename)
        return np.genfromtxt(io.StringIO(text), skip_header=REF_SKIPROWS, names=names)


class PipeFlowPlotter:
    def __init__(self, args: argparse.Namespace):
        self.dns_time = args.dns_time
        self.re = args.re
        self.input_dir = (Path(__file__).resolve().parent / args.input_dir).resolve()
        self.output_dir = (Path(__file__).resolve().parent / args.output_dir).resolve()
        self.fallback_output_dir = Path(tempfile.gettempdir()) / "chapsim2_plots"
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.reference_store = PipeReferenceStore((Path(__file__).resolve().parent / args.ref_dir).resolve())
        self.radial_stored_as_r_times_qr = (
            args.radial_stored_as_r_times_qr and not args.radial_stored_directly
        )
        self.vorticity_ref_mode = args.vorticity_ref_mode

        self.means_cache: dict[str, np.ndarray] = {}
        self.rey_cache: dict[str, np.ndarray] = {}
        self.ref_cache: dict[str, np.ndarray] = {}

        self._load_wall_units()
        self.ref_retau = self._select_reference_retau(args.ref_retau)

    def _save_figure(self, fig, outfile: Path) -> Path:
        tmp_file = tempfile.NamedTemporaryFile(
            delete=False,
            suffix=outfile.suffix,
            prefix=f"{outfile.stem}_",
        )
        tmp_path = Path(tmp_file.name)
        tmp_file.close()

        try:
            fig.savefig(tmp_path, bbox_inches="tight")
        except OSError:
            tmp_path.unlink(missing_ok=True)
            raise

        try:
            outfile.parent.mkdir(parents=True, exist_ok=True)
            shutil.move(str(tmp_path), outfile)
            print(f"Saved {outfile}", flush=True)
            return outfile
        except OSError as exc:
            self.fallback_output_dir.mkdir(parents=True, exist_ok=True)
            fallback = self.fallback_output_dir / outfile.name

            if tmp_path.exists():
                shutil.move(str(tmp_path), fallback)
                print(
                    f"Warning: could not write {outfile} ({exc}); "
                    f"saving this and remaining plots to {self.fallback_output_dir}",
                    flush=True,
                )
                self.output_dir = self.fallback_output_dir
                print(f"Saved {fallback}", flush=True)
                return fallback
            raise

    def _load_ascii_column(self, stem: str) -> np.ndarray:
        path = self.input_dir / f"domain1_tsp_avg_{stem}_{self.dns_time}.dat"
        if not path.exists():
            raise FileNotFoundError(f"Missing required file: {path}")

        data = np.loadtxt(path)
        data = np.atleast_2d(data)
        if data.shape[1] < 3:
            raise ValueError(f"{path} must contain at least 3 columns: index, r, value")
        return data

    def _load_mean_profile(self, name: str) -> np.ndarray:
        if name not in self.means_cache:
            self.means_cache[name] = self._load_ascii_column(FILEMAP_MEAN[name])
        return self.means_cache[name]

    def _load_reynolds_profile(self, name: str) -> np.ndarray:
        if name not in self.rey_cache:
            self.rey_cache[name] = self._load_ascii_column(FILEMAP_REY[name])
        return self.rey_cache[name]

    def _load_wall_units(self) -> None:
        uz = self._load_mean_profile("ux")
        self.grid_index = uz[:, 0]
        self.grid_r = uz[:, 1]
        self.grid_wall_dist = 1.0 - self.grid_r

        y1 = self.grid_wall_dist[-1]
        y2 = self.grid_wall_dist[-2]
        u1 = uz[-1, 2]
        u2 = uz[-2, 2]
        dudy_wall = (u1 * y2 * y2 - u2 * y1 * y1) / (y1 * y2 * (y2 - y1))

        self.tauw = dudy_wall / self.re
        self.utau = math.sqrt(abs(self.tauw))
        self.retau = self.re * self.utau
        self.grid_yplus = self.retau * self.grid_wall_dist

        print(f"Computed utau = {self.utau:.6f}, Re_tau = {self.retau:.2f}")

    def _select_reference_retau(self, requested_retau: int | None) -> int:
        available = self.reference_store.available_retaus()
        if not available:
            raise FileNotFoundError(
                f"No reference PIPE_Re*_MEAN.dat dataset found under {self.reference_store.ref_dir}"
            )

        if requested_retau is not None:
            if requested_retau not in available:
                raise FileNotFoundError(
                    f"Requested reference Re_tau={requested_retau} is not available. Found: {available}"
                )
            selected = requested_retau
        else:
            selected = min(available, key=lambda retau: abs(retau - self.retau))

        print(f"Using reference Re_tau = {selected} (simulation Re_tau = {self.retau:.2f})")
        return selected

    def _ref_filename(self, suffix: str) -> str:
        return f"PIPE_Re{self.ref_retau}_{suffix}.dat"

    def _load_reference(self, kind: str) -> np.ndarray:
        if kind in self.ref_cache:
            return self.ref_cache[kind]

        if kind == "mean":
            table = self.reference_store.load_table(self._ref_filename("MEAN"), MEAN_COLS)
        elif kind == "rms":
            table = self.reference_store.load_table(self._ref_filename("RMS"), RMS_COLS)
        elif kind == "vort_fluc":
            table = self.reference_store.load_table(self._ref_filename("VORT_PRES_FLUC"), VORT_FLUC_COLS)
        else:
            raise ValueError(f"Unsupported reference kind: {kind}")

        self.ref_cache[kind] = table
        return table

    def _radial_mean(self, stored: np.ndarray, r: np.ndarray) -> np.ndarray:
        if self.radial_stored_as_r_times_qr:
            return safe_divide(stored, r)
        return stored

    def _radial_second_moment(self, stored: np.ndarray, r: np.ndarray) -> np.ndarray:
        if self.radial_stored_as_r_times_qr:
            return safe_divide(stored, r * r)
        return stored

    def _radial_cross_moment(self, stored: np.ndarray, r: np.ndarray) -> np.ndarray:
        if self.radial_stored_as_r_times_qr:
            return safe_divide(stored, r)
        return stored

    def _sorted_xy(self, x: np.ndarray, y: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
        order = np.argsort(x)
        return x[order], y[order]

    def _dns_velocity_profiles(self) -> dict[str, np.ndarray]:
        uz = self._load_mean_profile("ux")[:, 2]
        ur = self._radial_mean(self._load_mean_profile("uy")[:, 2], self.grid_r)
        ut = self._load_mean_profile("uz")[:, 2]
        return {"ux": uz, "uy": ur, "uz": ut}

    def _dns_vorticity_profiles_plus(self) -> dict[str, np.ndarray]:
        mean = self._dns_velocity_profiles()
        uz_plus = mean["ux"] / self.utau
        ut_plus = mean["uz"] / self.utau

        omega_r_plus = np.zeros_like(self.grid_r)
        omega_theta_plus = -(1.0 / self.retau) * derivative(uz_plus, self.grid_r)

        r_ut = self.grid_r * ut_plus
        dr_ut_dr = derivative(r_ut, self.grid_r)
        omega_z_plus = np.zeros_like(self.grid_r)
        mask = np.abs(self.grid_r) > 1.0e-12
        omega_z_plus[mask] = dr_ut_dr[mask] / self.grid_r[mask] / self.retau
        if np.abs(self.grid_r[0]) <= 1.0e-12:
            omega_z_plus[0] = 2.0 * derivative(ut_plus, self.grid_r)[0] / self.retau

        return {
            "omega_r": omega_r_plus,
            "omega_theta": omega_theta_plus,
            "omega_z": omega_z_plus,
        }

    def _dns_series(self, name: str) -> tuple[np.ndarray, np.ndarray]:
        param = PARAMS[name]

        if param["group"] == "velocity":
            mean = self._dns_velocity_profiles()
            dns_val = mean[name] / self.utau
            return self._sorted_xy(self.grid_yplus, dns_val)

        if param["group"] == "pressure":
            pr = self._load_mean_profile("pr")[:, 2] / self.tauw
            pr = pr - pr[-1]
            return self._sorted_xy(self.grid_yplus, pr)

        if param["group"] == "stress":
            mean = self._dns_velocity_profiles()
            stress_raw = self._load_reynolds_profile(name)[:, 2]

            if name == "uu":
                dns_val = (stress_raw - mean["ux"] * mean["ux"]) / (self.utau * self.utau)
            elif name == "vv":
                rr = self._radial_second_moment(stress_raw, self.grid_r)
                dns_val = (rr - mean["uy"] * mean["uy"]) / (self.utau * self.utau)
            elif name == "ww":
                dns_val = (stress_raw - mean["uz"] * mean["uz"]) / (self.utau * self.utau)
            elif name == "uv":
                cross = self._radial_cross_moment(stress_raw, self.grid_r)
                dns_val = (cross - mean["ux"] * mean["uy"]) / (self.utau * self.utau)
            elif name == "uw":
                dns_val = (stress_raw - mean["ux"] * mean["uz"]) / (self.utau * self.utau)
            elif name == "vw":
                cross = self._radial_cross_moment(stress_raw, self.grid_r)
                dns_val = (cross - mean["uy"] * mean["uz"]) / (self.utau * self.utau)
            else:
                raise ValueError(name)

            return self._sorted_xy(self.grid_yplus, dns_val)

        if param["group"] == "vorticity":
            vort = self._dns_vorticity_profiles_plus()
            return self._sorted_xy(self.grid_yplus, vort[name])

        raise ValueError(name)

    def _reference_mean_vorticity_plus(self) -> dict[str, tuple[np.ndarray, np.ndarray]]:
        ref = self._load_reference("mean")
        yplus = ref["yplus"]
        r = ref["r"]
        uz_plus = ref["Uz"]
        ut_plus = ref["Ut"]

        omega_r = np.zeros_like(yplus)
        omega_theta = derivative(uz_plus, yplus)

        r_ut = r * ut_plus
        drut_dr = derivative(r_ut, r)
        omega_z = np.zeros_like(r)
        mask = np.abs(r) > 1.0e-12
        omega_z[mask] = drut_dr[mask] / r[mask] / self.ref_retau
        if np.abs(r[-1]) <= 1.0e-12:
            # Reference tables are ordered wall -> centreline, so the axis is last.
            omega_z[-1] = 2.0 * derivative(ut_plus, r)[-1] / self.ref_retau

        return {
            "omega_r": self._sorted_xy(yplus, omega_r),
            "omega_theta": self._sorted_xy(yplus, omega_theta),
            "omega_z": self._sorted_xy(yplus, omega_z),
        }

    def _reference_series(self, name: str) -> tuple[np.ndarray, np.ndarray]:
        param = PARAMS[name]

        if param["group"] == "velocity":
            ref = self._load_reference("mean")
            return ref["yplus"], ref[param["ref_key"]]

        if param["group"] == "pressure":
            ref = self._load_reference("mean")
            ref_val = ref["P"] - ref["P"][0]
            return ref["yplus"], ref_val

        if param["group"] == "stress":
            ref = self._load_reference("rms")
            return ref["yplus"], ref[param["ref_key"]]

        if param["group"] == "vorticity":
            if self.vorticity_ref_mode == "mean":
                return self._reference_mean_vorticity_plus()[name]

            ref = self._load_reference("vort_fluc")
            key = {
                "omega_r": "oror",
                "omega_theta": "otot",
                "omega_z": "ozoz",
            }[name]
            return ref["yplus"], np.sqrt(np.maximum(ref[key], 0.0))

        raise ValueError(name)

    def plot_quantity(self, name: str) -> None:
        param = PARAMS[name]
        fig, ax = plt.subplots(figsize=FIGSIZE, dpi=DPI)
        ax.set_xlabel(r"$y^+$", fontsize=18)
        ax.set_ylabel(param["ylabel"], fontsize=18)
        ax.set_title(param["title"], fontsize=16)
        ax.set_xscale("log")

        ref_x, ref_y = self._reference_series(name)
        ref_label = f"TDL{self.ref_retau}"
        if param["group"] == "vorticity" and self.vorticity_ref_mode == "fluctuation":
            ref_label = f"TDL{self.ref_retau} RMS"
        ax.plot(
            ref_x,
            ref_y,
            marker=mlst[1],
            mfc="none",
            ms=4,
            color=cbrg(0.0),
            linestyle="None",
            label=ref_label,
        )

        dns_x, dns_y = self._dns_series(name)
        dns_label = "CHAPSim2"
        if param["group"] == "vorticity" and self.vorticity_ref_mode == "fluctuation":
            dns_label = "CHAPSim2 mean"
        ax.plot(
            dns_x,
            dns_y,
            linestyle="--",
            color=cbrg(0.55),
            linewidth=1.6,
            label=dns_label,
        )

        ax.grid(True, which="both", ls="-", alpha=0.2)
        ax.legend()
        ax.set_xlim(0.1, max(500.0, float(np.nanmax(dns_x))))

        outfile = self.output_dir / f"pipe_{name}_{self.dns_time}.png"
        self._save_figure(fig, outfile)
        plt.close(fig)


def expand_groups(groups: tuple[str, ...] | list[str]) -> list[str]:
    ordered_groups = ("velocity", "pressure", "stress", "vorticity") if "all" in groups else groups
    names: list[str] = []
    for group in ordered_groups:
        names.extend(PLOT_GROUPS[group])
    return names


def main() -> None:
    args = parse_args()
    names = expand_groups(args.groups)

    print(f"\nUsing DNS_TIME = {args.dns_time}")
    print(f"Input directory  = {Path(args.input_dir)}")
    print(f"Reference dir    = {Path(args.ref_dir)}")
    print(f"Output directory = {Path(args.output_dir)}")
    print(f"Vorticity ref    = {args.vorticity_ref_mode}\n")

    plotter = PipeFlowPlotter(args)

    for name in names:
        print(f"=== Processing {name} ===")
        plotter.plot_quantity(name)

    print("\nAll requested plots completed.\n")


if __name__ == "__main__":
    main()
