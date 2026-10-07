# Mesh Stretching Reviewer

CHAPSim2 includes an interactive visualization tool for examining one-dimensional y-direction mesh stretching before case execution:

```text
prepost/mesh_reviewer/mesh_stretching_viewer.py
```

This tool implements the stretching mappings used by `geometry.f90` and enables inspection of physical node locations, the computational-to-physical coordinate transformation, and local mesh spacing distributions.

## Run the Viewer

From the repository root:

```bash
python3 prepost/mesh_reviewer/mesh_stretching_viewer.py
```

With explicit settings:

```bash
python3 prepost/mesh_reviewer/mesh_stretching_viewer.py \
  --method 3fmd \
  --location two-sides \
  --n 129 \
  --rstret 0.08 \
  --lyb -1.0 \
  --lyt 1.0
```

The page cannot directly start the Python program from the browser. Browsers
block local command execution for security. The command above is the recommended
safe route.

## Command-Line Options

| Option | Accepted values | Definition |
| --- | --- | --- |
| `--method` | `3fmd`, `tanh`, `powerlaw` | Stretching law (3-point finite-difference, hyperbolic tangent, or power-law) |
| `--location` | `centre`, `two-sides`, `bottom`, `top` | Point clustering location |
| `--n` | integer, 5 to 216 | Number of grid points in visualization |
| `--rstret` | real, 0.001 to 1.0 | Stretching parameter (smaller values increase clustering strength) |
| `--lyb` | real | Physical lower y boundary |
| `--lyt` | real | Physical upper y boundary (must exceed `lyb`) |

## Notes on Supported Modes

| Method | Unsupported or risky location | Reason |
| --- | --- | --- |
| `3fmd` | `centre` | Can produce a jump or non-monotone mapping. |
| `tanh` | `centre` | Not supported by the current geometry implementation. |
| `powerlaw` | `centre` | Not supported by the viewer. |

If the plotted spacing is non-monotone or unexpectedly sharp, adjust `rstret`,
point count, or clustering location before launching a production run.

## Suggested Use in Case Setup

1. Choose the geometry and y-domain limits for the target case.
2. Open the viewer with candidate `n`, `rstret`, method, and location.
3. Inspect the spacing curve and near-wall clustering.
4. Transfer the accepted method and `rstret` into the `[mesh]` section of
   `input_chapsim.ini`.
5. Run the case mesh-check plotting script before starting a long simulation.
