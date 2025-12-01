"""
README
-------
This script performs post-processing and visualization of channel flow simulation data from CHAPSim2.

Features:
- Plots mean velocity profiles (ux, uy, uz)
- Plots pressure profiles
- Plots Reynolds stress components (uu, vv, ww, uv)
- Generates combined RMS velocity plots
- Compares results with MKM180 reference data

Requirements:
- Python 3.x
- NumPy
- Matplotlib

Usage:
1. Ensure the following data structure:
   - ../1_data/: Contains CHAPSim2 simulation results
   - ../../MKM180_profiles/: Contains reference data files (chan180.means, chan180.reystress)

2. Configure the DNS_TIME variable in the code to match your simulation time step

3. Run the script:
   python channel_postprocess_plot.py --dns-time 200000

Output:
- Individual component plots: channel_[component].png
- Combined RMS plot: channel_all_rms.png

Author: W Wang (STFC)
"""
import os
import argparse
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import math
from pylab import rcParams

# -------------------------------------------------------------
# 1. USER INPUT (DNS_TIME)
# -------------------------------------------------------------
parser = argparse.ArgumentParser(description="CHAPSim2 Channel Flow Post-Processing")
parser.add_argument(
    "--dns-time", type=str, required=True, help="Time stamp for DNS data, e.g. 200000"
)
args = parser.parse_args()

DNS_TIME = args.dns_time
print(f"\nUsing DNS_TIME = {DNS_TIME}\n")

# -------------------------------------------------------------
# 2. File-name mapping
# -------------------------------------------------------------
FILEMAP_MEAN = {"ux": "u1", "uy": "u2", "uz": "u3", "pr": "pr"}

FILEMAP_REY = {
    "uu": "uu11",
    "uv": "uu12",
    "uw": "uu13",
    "vv": "uu22",
    "vw": "uu23",
    "ww": "uu33",
}

# -------------------------------------------------------------
# 3. Plot settings & parameters
# -------------------------------------------------------------
cbrg = cm.get_cmap("brg")
mlst = ["o", "<", "*", "v", "^", ">", "1", "2", "3", "4", "x", "s", "8", "+"]
plt.rc("figure", facecolor="white")
plt.rc("legend", fontsize=15)
rcParams["legend.loc"] = "best"

figsize = (9, 6)
dpi = 500

# -------------------------------------------------------------
# 4. Parameter metadata
# -------------------------------------------------------------
PARAMS = {
    "ux": {
        "ref_file": "chan180.means",
        "ref_key": "umean",
        "scaling": "mean",
        "ylabel": r"$u_x^+$",
    },
    "uy": {
        "ref_file": "chan180.means",
        "ref_key": "umean",
        "scaling": "mean",
        "ylabel": r"$u_y^+$",
    },
    "uz": {
        "ref_file": "chan180.means",
        "ref_key": "wmean",
        "scaling": "mean",
        "ylabel": r"$u_z^+$",
    },
    "pr": {
        "ref_file": "chan180.means",
        "ref_key": "pmean",
        "scaling": "mean",
        "ylabel": r"$p^+$",
    },
    "uu": {
        "ref_file": "chan180.reystress",
        "ref_key": "uu",
        "scaling": "rms",
        "ylabel": r"$u_{rms}^+$",
    },
    "vv": {
        "ref_file": "chan180.reystress",
        "ref_key": "vv",
        "scaling": "rms",
        "ylabel": r"$v_{rms}^+$",
    },
    "ww": {
        "ref_file": "chan180.reystress",
        "ref_key": "ww",
        "scaling": "rms",
        "ylabel": r"$w_{rms}^+$",
    },
    "uv": {
        "ref_file": "chan180.reystress",
        "ref_key": "uv",
        "scaling": "reynolds",
        "ylabel": r"$\overline{u^\prime v^\prime}^+$",
    },
}
# -------------------------------------------------------------
# 5. Analyzer class
# -------------------------------------------------------------
class ChannelFlowAnalyzer:
    def __init__(self, name):
        if name not in PARAMS:
            raise ValueError(name)

        self.name = name
        self.param = PARAMS[name]

        self.Re = 2800
        self.Re0 = 2784.0
        self.Ret = 178.12
        self.utau0 = self.Ret / self.Re0

        # wall quantities
        self.load_wall_shear()
        self.load_ref()
        self.load_dns()

    # ---------------------------------------------------------
    def load_wall_shear(self):
        """Compute u_tau using ux profile"""
        fname = os.path.join(
            "../1_data", f"domain1_tsp_avg_{FILEMAP_MEAN['ux']}_{DNS_TIME}.dat"
        )
        ux = np.genfromtxt(fname, names=["j", "y", "ux"])

        dudy = ux["ux"][0] / (1.0 + ux["y"][0])
        self.tauw = dudy / self.Re
        self.utau = math.sqrt(abs(self.tauw))

        print(f"Computed utau = {self.utau:.6f}")

    # ---------------------------------------------------------
    def load_ref(self):
        """Load MKM180 reference"""
        folder = "../../MKM180_profiles"
        ref_file = os.path.join(folder, self.param["ref_file"])

        if self.param["ref_file"] == "chan180.means":
            names = ["y", "yplus", "umean", "umeandy", "wmean", "wmeandy", "pmean"]
        else:
            names = ["y", "yplus", "uu", "vv", "ww", "uv", "uw", "vw"]

        self.ref = np.genfromtxt(ref_file, skip_header=25, names=names)

    # ---------------------------------------------------------
    def load_dns(self):
        """Load CHAPSim DNS data"""

        # mean-field file name
        if self.name in FILEMAP_MEAN:
            fname = f"domain1_tsp_avg_{FILEMAP_MEAN[self.name]}_{DNS_TIME}.dat"
        else:
            fname = f"domain1_tsp_avg_{FILEMAP_REY[self.name]}_{DNS_TIME}.dat"

        fpath = os.path.join("../1_data", fname)
        data = np.genfromtxt(fpath, names=["j", "y", self.name])

        # map y
        y = data["y"]
        y_new = np.where(y < 0, y + 1, 1 - y)
        yplus = self.Re * self.utau * y_new

        self.dns_yplus = yplus
        self.dns_raw = data[self.name]

        # scaling
        if self.param["scaling"] == "mean":
            self.dns_val = self.dns_raw / self.utau

        elif self.param["scaling"] == "reynolds":
            self.dns_val = self.dns_raw / (self.utau * self.utau)

        elif self.param["scaling"] == "rms":
            # load u1, u2, u3 as needed
            if self.name == "uu":
                meanfile = f"domain1_tsp_avg_u1_{DNS_TIME}.dat"
                meancol = "ux"
            elif self.name == "vv":
                meanfile = f"domain1_tsp_avg_u2_{DNS_TIME}.dat"
                meancol = "uy"
            else:  # ww
                meanfile = f"domain1_tsp_avg_u3_{DNS_TIME}.dat"
                meancol = "uz"

            mf = np.genfromtxt(
                os.path.join("../1_data", meanfile), names=["j", "y", meancol]
            )

            mean = mf[meancol]
            self.dns_val = np.sqrt(self.dns_raw - mean * mean) / self.utau

    # ---------------------------------------------------------
    def plot(self):
        """Plot a single component"""

        fig, ax = plt.subplots(figsize=figsize, dpi=dpi)
        ax.set_xlabel(r"$y^+$", fontsize=20)
        ax.set_ylabel(self.param["ylabel"], fontsize=20)
        ax.set_xscale("log")

        # ref
        refval = self.ref[self.param["ref_key"]]
        if self.param["scaling"] == "rms":
            refval = np.sqrt(refval)
        if self.param["scaling"] == "reynolds":
            refval = np.abs(refval)

        ax.plot(
            self.ref["yplus"],
            refval,
            marker=mlst[1],
            mfc="none",
            ms=4,
            color=cbrg(0.0),
            linestyle="None",
            label="MKM180",
        )

        # dns
        ax.plot(
            self.dns_yplus,
            self.dns_val,
            linestyle="--",
            color=cbrg(0.5),
            label="CHAPSim2",
        )

        ax.grid(True, which="both", ls="-", alpha=0.2)
        ax.legend()
        ax.set_xlim(0.1, 500)

        fig.savefig(f"channel_{self.name}.png")
        plt.close()
        print(f"Saved channel_{self.name}.png")


# -------------------------------------------------------------
# 6. MAIN
# -------------------------------------------------------------
def main():
    # loop all components
    for name in PARAMS.keys():
        print(f"\n=== Processing {name} ===")
        ChannelFlowAnalyzer(name).plot()

    print("\nAll plots completed.\n")


if __name__ == "__main__":
    main()










