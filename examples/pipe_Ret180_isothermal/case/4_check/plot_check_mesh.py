"""
README
-------
## Overview
This script visualizes mesh data from two input files (check_mesh_yp.dat and check_mesh_yc.dat),
creating a four-panel figure that shows the relationship between grid indices and y-coordinates,
as well as the grid change rate.

## Input Files
- check_mesh_yp.dat: Contains grid point indices and yp coordinates
- check_mesh_yc.dat: Contains grid point indices, yc coordinates, and growth rates

## Output
- mesh_check_plots.png: A 2×2 grid of plots showing:
  - Full range plot of yp and yc vs grid index
  - Zoomed plot of yp and yc vs grid index (first 15 points)
  - Full range plot of change rate vs grid index
  - Zoomed plot of change rate vs grid index (first 15 points)

## Requirements
- NumPy
- Matplotlib

## Usage
Simply run the script in a directory containing the required data files:
```
python mesh_visualization.py
```

Author: W Wang (STFC)
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

# --- Utility function to draw horizontal projection lines ---
def draw_horizontal_projections(ax, x_vals, y_vals, color='gray', linestyle='-', alpha=0.5):
    for x, y in zip(x_vals, y_vals):
        ax.plot([x, 0], [y, y], linestyle=linestyle, color=color, alpha=alpha)

# --- Load data ---
data_yp = np.loadtxt('check_mesh_yp.dat', skiprows=1)
data_yc = np.loadtxt('check_mesh_yc.dat', skiprows=1)

# --- Extract columns ---
index_yp, yp = data_yp[:, 0], data_yp[:, 1]
index_yc, yc, growth_rate = data_yc[:, 0], data_yc[:, 1], data_yc[:, 2]

n_points = len(index_yp)
zoom_start = max(n_points - 20, 0)  # ensure not negative

# --- Create subplots ---
fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(12, 9))

# === 1. Full Range Plot of yp and yc ===
ax1.plot(index_yp, yp, '+-', label='yp', markersize=4)
ax1.plot(index_yc, yc, 'o-', label='yc', markersize=4)
draw_horizontal_projections(ax1, index_yp, yp)
ax1.set_title('Full Range Plot of yp and yc')
ax1.set_xlabel('Grid Index')
ax1.set_ylabel('y')
ax1.grid(False)
ax1.xaxis.set_major_locator(MaxNLocator(integer=True))
ax1.legend()

# === 2. Zoomed Plot of yp and yc ===
ax2.plot(index_yp, yp, '+-', label='yp', markersize=4)
ax2.plot(index_yc, yc, 'o-', label='yc', markersize=4)
draw_horizontal_projections(ax2, index_yp[zoom_start:], yp[zoom_start:])
ax2.set_title('Zoomed Plot of yp and yc (Last 20 Points)')
ax2.set_xlabel('Grid Index')
ax2.set_ylabel('y')
ax2.grid(False)
ax2.xaxis.set_major_locator(MaxNLocator(integer=True))
ax2.set_xlim(index_yp[zoom_start], index_yp[-1])
ax2.set_ylim(np.min(yp[zoom_start:]), np.max(yp[zoom_start:]))
ax2.legend()

# === 3. Full Range Plot of Change Rate ===
ax3.plot(index_yc, growth_rate * 100, 'o-', markersize=4)
ax3.set_title('Full Range Plot of Change Rate')
ax3.set_xlabel('Grid Index')
ax3.set_ylabel('Change Rate (%)')
ax3.grid(True)
ax3.xaxis.set_major_locator(MaxNLocator(integer=True))

# === 4. Zoomed Plot of Change Rate ===
ax4.plot(index_yc, growth_rate * 100, 'o-', markersize=4)
ax4.set_title('Zoomed Plot of Change Rate (Last 20 Points)')
ax4.set_xlabel('Grid Index')
ax4.set_ylabel('Change Rate (%)')
ax4.grid(True)
ax4.xaxis.set_major_locator(MaxNLocator(integer=True))
ax4.set_xlim(index_yc[zoom_start], index_yc[-1])
ax4.set_ylim(np.min(growth_rate[zoom_start:] * 100), np.max(growth_rate[zoom_start:] * 100))

# --- Final touches ---
plt.tight_layout()
plt.savefig('mesh_check_plots.png', dpi=300, bbox_inches='tight')
plt.show()

