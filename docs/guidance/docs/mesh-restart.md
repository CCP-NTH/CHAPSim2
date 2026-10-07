# Mesh-Restart Interpolation Workflow

This procedure enables simulation continuation from an existing converged solution while employing a different mesh or domain size with identical topology. Typical applications include grid refinement, grid coarsening, or domain-size modification while preserving the original flow configuration.

The helper scripts are:

| Script | Execution location | Purpose |
| --- | --- | --- |
| `prepost/input_generator/setup_chapsim_interp_step1.sh` | Source case directory | Prepare the source case for interpolation, generate interpolation output, and create `input_chapsim_tgt.ini` |
| `prepost/input_generator/setup_chapsim_interp_step2.sh` | Target case directory | Transfer interpolation files, rename for target execution, and update target `input_chapsim.ini` |

## When to Use This Workflow

Use this workflow when:

- Source and target share identical geometry family and topology (e.g., channel-to-channel or pipe-to-pipe)
- Target mesh differs in resolution (`ncx`, `ncy`, `ncz`), stretching parameters (`istret`, `rstret`), or domain extents (`lxx`, `lyt`, `lyb`, `lzz`)
- Target initialization requires a physically developed source solution rather than synthetic initial conditions

Avoid this workflow for conversions between unrelated topologies (e.g., channel-to-pipe) or incompatible physics selections without careful validation of interpolated fields.

## Step 1: Prepare the Source Case

From the source case directory, run:

```bash
bash /path/to/CHAPSim2/prepost/input_generator/setup_chapsim_interp_step1.sh
```

The script requires `input_chapsim.ini` in the current directory. It creates a
backup named `input_chapsim.ini.bak`, then updates the source input file:

| Section | Variable | New value | Meaning |
| --- | --- | --- | --- |
| `[process]` | `is_prerun` | `.true.` | Enable the interpolation/prerun preparation stage. |
| `[flow]` | `initfl` | `0` | Restart flow from existing data. |
| `[flow]` | `irestartfrom` | user selected | Restart index to read from the source case. |

The script also creates `input_chapsim_tgt.ini`. This file stores only the
target `[domain]` and `[mesh]` settings:

```ini
[domain]
icase= ...
lxx= ...
lyt= ...
lyb= ...
lzz= ...

[mesh]
ncx= ...
ncy= ...
ncz= ...
istret= ...
rstret= ...
```

After script execution, you can run the source case with MPI. The interpolation
pre-run uses the same 2-D pencil decomposition as the solver, exchanges source
field halos between neighbouring ranks, and writes distributed target restart
files. Upon successful completion, the source case should generate `domain0_*`
files in `1_data/`.

For first use on a new topology or mesh-ratio change, compare a small serial
pre-run and MPI pre-run before launching a production interpolation.

## Step 2: Prepare the Target Case

Create or enter the target case directory, then run:

```bash
bash /path/to/CHAPSim2/prepost/input_generator/setup_chapsim_interp_step2.sh
```

When prompted, provide the path to the completed source case from Step 1. The script validates that the source directory contains required files:

- `1_data/`
- `input_chapsim.ini`
- `input_chapsim_tgt.ini`
- one or more `1_data/domain0_*` files

It then prepares the target case by:

- creating `1_data/` if needed;
- copying each source `domain0_*` file into the target `1_data/`;
- renaming those files to `domain1_*`;
- copying `input_chapsim.ini` and `input_chapsim_tgt.ini`;
- setting `[process] is_prerun = .false.`;
- setting `[flow] irestartfrom = 0`;
- replacing the target `[domain]` and `[mesh]` values using
  `input_chapsim_tgt.ini`.

After Step 2, run the target case normally. The runtime mesh should match the
values in `input_chapsim_tgt.ini`.

## Practical Checks

Before running the target case:

- Compare `input_chapsim.ini.bak`, `input_chapsim.ini`, and
  `input_chapsim_tgt.ini` if the source setup looks unexpected.
- Confirm that `1_data/domain1_*` exists in the target case.
- Confirm that `icase` is unchanged unless you have intentionally prepared a
  compatible case conversion.
- Check that the target `ncx`, `ncy`, `ncz`, stretching method, and domain
  lengths are the values you intended.
- Start with a short run and check CFL, mass conservation, and early monitor
  history before launching a long production run.

## Common Problems

| Symptom | Likely cause | Action |
| --- | --- | --- |
| `input_chapsim.ini not found` | Script was run from the wrong directory. | Run Step 1 inside the source case directory. |
| `No domain0_* files found` | Source prerun did not complete or wrote output elsewhere. | Re-run the source pre-run with the intended MPI configuration and inspect the solver log and `1_data/`. |
| Target starts from the wrong mesh | `input_chapsim_tgt.ini` does not match the intended target case. | Edit or regenerate `input_chapsim_tgt.ini`, then rerun Step 2. |
| Target run becomes unstable immediately | Interpolated field and target setup are inconsistent. | Check topology, physics options, wall units, CFL, and boundary conditions. |
