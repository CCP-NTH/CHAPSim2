# Postprocessing and Output Data

CHAPSim2 produces several classes of output. The postprocessing approach depends on whether restart data, flow-field visualization, monitor histories, or statistical profiles are required.

## Output Folders

| Folder | Main content | Typical use |
| --- | --- | --- |
| `1_data/` | Restart/checkpoint data and domain files | Restart initialization, mesh interpolation, wall-unit post-processing, and low-level field validation |
| `2_visu/` | Visualization files (typically XDMF/HDF5 format) | ParaView or VisIt inspection, field slices, and instantaneous flow snapshots |
| `3_monitor/` | Monitor history files and plotting scripts | Bulk quantities, point probes, convergence verification, and transient diagnostics |
| `4_check/` | Mesh and setup validation outputs | Grid quality assessment, spatial distribution verification, and early validation |

Not all simulations produce every folder. Output availability depends on the switches and frequencies defined in `[io]`, `[statistics]`, and related input-file sections.

For the exact restart files written under each restart layout and history mode,
see [Restart I/O Modes](restart-io.md).

## Visualisation Data

Visualization is controlled primarily by `visu_idim`, `visu_nfre`, and `visu_nskip` in the input file. Full-domain and plane outputs can be visualized in ParaView or VisIt when XDMF format files are present.

Common example scripts:

| Script | Purpose |
| --- | --- |
| `validation/cases/channel/iso_periodic/post/2_visu/plot_channel_velo_stress.py` | Plot channel mean velocity and Reynolds-stress profiles. |
| `validation/cases/channel/iso_periodic/post/2_visu/postprocess_channel_wall_units.py` | Convert channel output to wall-unit profiles. |
| `validation/cases/pipe/iso_periodic/post/2_visu/plot_pipe_velo_stress_v2.py` | Plot pipe velocity and stress statistics with robust figure saving. |
| `validation/cases/pipe/iso_periodic/post/2_visu/plot_pipe_velo_stress.py` | Earlier pipe profile plotting script. |

For production postprocessing, copy the closest example script into the case
directory, update the run index, mesh/domain assumptions, and file paths, then
run it from the folder containing the output files.

## Monitor Data

Monitor output is useful for assessing run health before expensive statistics accumulation is performed. Reference examples include:

| Script | Purpose |
| --- | --- |
| `validation/tools/scripts/plot_monitor_bulk_change_history.py` | Plot bulk-value evolution. |
| `validation/tools/scripts/plot_monitor_points.py` | Plot monitor-point histories. |
| `validation/tools/scripts/plot_monitor_points_seperate.py` | Plot monitor points in separate figures. |

Recommended checks:

- CFL remains within the intended range.
- Flow rate, pressure gradient, or driving force reaches the expected regime.
- Monitor points do not show sudden jumps after restart.
- Statistics are not started until the transient period has passed.

## Statistics Levels

The input variable `stat_level` controls the extent of statistical information collected. Select the minimum level that contains required quantities, as higher levels increase memory, I/O, and post-processing costs.

| `stat_level` | Content | Typical use |
| --- | --- | --- |
| `0` | Statistics disabled | Initial setup verification, mesh validation, and short test runs |
| `1` | Basic mean quantities | Mean-flow development and preliminary monitoring |
| `2` | Second-order statistics | Reynolds stresses and wall-bounded turbulence statistics |
| `3` | Extended statistics (where supported) | Detailed budgets and advanced diagnostics |

Control sampling behavior with `stat_istart`, `stat_nskip`, and related parameters to ensure statistics are collected only after the transient initial condition phase.

## Statistics Across a Restart

The time-averaged fields (`t_avg_*`) are checkpointed together with the
instantaneous fields and are reloaded on restart, so a run split into several
segments gives the same averages as one uninterrupted run. The reload happens
when all of the following hold:

- the field is initialised with `initfl= restart` (and `inittm= restart` for the
  thermal field);
- `irestartfrom` is greater than `stat_istart`;
- `restart_clock= continue` — with `restart_clock= reset` the timeline starts at
  zero, so the stored averages belong to a stretch of time the new run does not
  continue and the accumulators deliberately start empty.

The number of samples already folded in is stored in the statistics manifest
(`domain<N>_<group>_stats_meta_<iter>.dat`, keys `stat_istart` and `nsamples`)
and restored with it, so the averaging weight stays correct even when the
sample stream has a gap — for example when the flow restarts but the thermal
field is initialised afresh, or when a frozen-field window
(`niterflowfirst`/`niterthermofirst`) suppresses sampling. `stat_istart` is
validated against the value in the manifest and the run stops if it has changed,
because the stored average cannot be re-weighted onto a different window.
Checkpoints written before these keys existed still load; the sample count then
falls back to `iter - stat_istart` and a warning is printed.

**The `Euu` spectra are the exception.** They are not checkpointed, so after a
restart they average only the post-restart samples while the `t_avg_*` fields
average the whole run. Both windows are stated in the spectrum output header
(`# samples` and `# window_first_iter`); read them before comparing a spectrum
with a time-averaged profile from the same file set.

## Statistical-Window Reconstruction

Every stored statistic is a **cumulative running mean** of one equally weighted
sample per iteration, from `stat_istart + 1` up to the iteration it was written
at; the statistics files hold means, not sums, and the sample count `N` of each
group (flow, thermo, MHD) in its manifest. Statistics therefore never form a
fixed or rolling window by themselves. The post-processing mode rebuilds the
average over any interval between two statistics checkpoints `a < b` without
repeating the simulation:

    A(a:b) = (N_b A_b - N_a A_a) / (N_b - N_a)

applied to every stored raw moment (`<u_i>`, `<u_i u_j>`, `<rho u_i u_j>`,
`<j_i u_j>`, ...). The result is exactly what a run with `stat_istart = a`
would have written at `b`. Rms values, Reynolds stresses, turbulent kinetic
energy and Favre fluxes are **not** differenced: form them from the
reconstructed raw moments with the usual post-processing scripts, e.g.
`<u'v'> = <uv> - <u><v>`.

**Running it.** Use the case's own `input_chapsim.ini` — the input of the job
that wrote snapshot `b` — in the case directory, with two keys in `[process]`:

```ini
[process]
is_prerun= .false.
is_postprocess= .true.
pp_stat_window= 68000, 88000
```

and launch the same executable as for the simulation (any rank count; the
statistics files are decomposition-independent). `stat_istart`, `stat_level`,
`restart_data_layout`, the mesh and the thermal/MHD switches must be those of
the run that wrote the statistics. The snapshots are read from `1_data`; both
`a` and `b` must be statistics checkpoints (multiples of `ckpt_nfre` or the
last iteration of a job), except `a = stat_istart`, the empty origin.

**What it does and does not do.** The mode reads the input and the mesh, checks
the snapshots, reads them, rebuilds the window and writes it, then stops. It
never initialises a field, solves an equation, takes a time step, writes a
checkpoint or touches anything in `1_data`, `2_visu`, `3_monitor` or
`4_check`. A missing or invalid key stops the run; it never falls back to a
simulation. Everything is written below the fixed directory
`5_postprocess/window_<a>_<b>/` (one subdirectory per window; the location is
not an input):

| Path | Content |
|---|---|
| `1_data/domain1_<group>_stats_<b>.bin`, `..._meta_<b>.dat` | window averages in the layout they were read in; manifest with `stat_istart a`, `nsamples N_b-N_a` and the appended keys `derived stat_window`, `window_start`, `window_end`, `source_stat_istart`, `source_nsamples`, `kappa` |
| `2_visu/...` | the usual `tsp_avg_*` profiles / planes (and per-field 3-D `t_avg_*` visualisation), same names and formats |
| `4_check/` | mesh checks of this run |
| `stat_window_provenance.dat` | window, origin, counts, times, every check made, precision diagnostics and limitations |

An existing `window_<a>_<b>` directory stops the run: results are never
overwritten. The output is **derived data**: do not put it back into `1_data`
or use it to continue an accumulation.

**Checks.** Before anything is written the run stops, with a diagnostic, if:

- a snapshot or its manifest is missing, or its format, group, signature
  (`stat_level`, thermal switch, experimental pressure statistics) or field list
  differs from the input;
- a manifest has no sample count (`nsamples`, `stat_istart`) — files written
  before 2026-10-06 (`bb0c2f48`); counts are never inferred from iteration
  numbers;
- its `stat_istart` differs from the input (a restart that started a fresh
  window);
- `N_b - N_a` differs from `b - a` for any group: the group was not sampled at
  every iteration of the window (a fresh thermal field at a restart, a frozen
  field, a reset that kept `stat_istart`); continuous sampling is required;
- an intermediate statistics checkpoint (multiples of `ckpt_nfre`) breaks the
  continuity;
- a bundled file does not hold exactly the listed fields on the input mesh
  (truncated file or different mesh), or the mesh recorded in
  `flow_restart_meta` at `a` or `b` differs from the input;
- the flow and thermal statistics are known to cover different solver steps
  (see below).

After the snapshots are read, and before any statistics are written, it also
stops if a reconstructed variance (`<u_i u_i> - <u_i>^2`, its Favre form,
`<T T> - <T>^2`, `<j_i j_i> - <j_i>^2`, ...) is negative beyond round-off; the
window directory then holds only the mesh files and must be removed before a
retry.

**Flow and thermal statistics of one window.** Flow and thermal statistics are
labelled by the run iteration: the thermal clock advances with the flow also
before `niterthermofirst`, and thermal statistics files record this as
`label_clock run`. If both thermal snapshots carry the key, the joint interval
is recorded as **consistent**; if only snapshot b does, the earlier end was
written by an older build and alignment there is recorded as **not
established**.

Builds before 2026-10-30 labelled thermal files with a counter that advanced
only while the energy equation was solved, and did not record the offset. For
such files the mode derives it from the input and the restart convention: a run
segment starts both counters at its start iteration `s` (`irestartfrom`, or 0),
and the flow / energy equation are first advanced at `max(s+1, niterflowfirst)`
/ `max(s+1, niterthermofirst)`. The mode then

- **stops** if these differ and `b > s`: the thermal labels trail the flow labels
  inside the segment, so the two groups do not describe the same interval;
- records the joint interval as **consistent** if they agree and the window
  lies in the segment (`a >= s`);
- records it as **not established** if the window reaches before `s`: earlier
  segments are not described by the input or the files. The reconstruction is
  written, but compatibility is not claimed.

The MHD statistics share the flow counter and are always consistent with the
flow statistics. The status is the `joint_interval` entry of
`stat_window_provenance.dat`. A run that has only one statistics group is not
affected.

**Precision.** Differencing two cumulative means amplifies their rounding by
`kappa = (N_a + N_b)/(N_b - N_a)`, reported per group (a warning above 1e3).
Negative variances within `16 kappa N_b eps <ff>` are counted as round-off and
left unclipped in the raw moments; clip them to zero when forming rms values.

**Interpretation and limitations.**

- The averages are **iteration-weighted**, as the stored statistics are; they
  equal the physical-time average only for a constant time step. A warning is
  printed if `dt` differs between the two snapshots.
- The statistics files record no code version, Reynolds/Prandtl/Hartmann
  number, magnetic field, boundary conditions or `dt` history. A physics or
  scheme change at a restart inside the window, or a field whose definition
  changed between solver versions, cannot be detected; the mesh can be checked
  only where `flow_restart_meta` was kept.
- Flow/thermal interval alignment rests on the input of the segment that wrote
  `b` (above); for windows reaching into earlier segments it is reported as not
  established.
- The reconstruction rebuilds averages over a chosen interval; it does not by
  itself establish that the flow was statistically stationary over it, or that
  the averages have converged.
- Not reconstructable, because the moments are not accumulated: density
  variance (`<rho rho>`), enthalpy variance (`<h h>`, `<rho h h>`), Favre
  temperature variance (`<rho T T>`), pressure rms (`<p p>`, except the
  experimental full-step pressure statistics), electric-potential rms
  (`<phi phi>`), Favre triple moments, and the `Euu` spectra (not
  checkpointed). `t_avg_fuuu` holds `<rho T u_i u_j u_k>` as accumulated, not
  the Favre triple moment its name suggests (open question, see the backlog).
- Memory: two snapshots of a group are held at once, comparable to the
  statistics part of the simulation itself; run production-size cases on at
  least as many ranks as the simulation.

For 2-periodic cases the profile script
`validation/tools/scripts/rebuild_tavg_from_new_start.py` applies the same
formula to the `tsp_avg` profiles, but derives the counts from iteration
numbers and checks nothing; prefer this mode where the statistics checkpoints
exist.

## Mesh and Setup Checks

Before trusting a new case, inspect the mesh:

```bash
python3 validation/tools/scripts/plot_check_mesh.py
```

Check near-wall spacing, stretching smoothness, and whether the physical domain
matches the intended geometry.

## Recommended Workflow

1. Execute a short test case and review log diagnostics
2. Plot monitor histories from `3_monitor/`
3. Inspect mesh and initial condition outputs from `4_check/`
4. Open XDMF visualization outputs from `2_visu/`
5. Enable statistics collection only after flow development is complete
6. Use the closest validation plotting script as a template for profile analysis

**Note on mounted filesystems:** Figure saving can occasionally fail with I/O errors on remote or mounted file systems. A robust approach is to save figures to a local temporary directory first, verify completion, and copy to the case directory when the file system is stable.

For interactive tuning of the y-direction stretching before a run, see the
[Mesh Stretching Reviewer](mesh-reviewer.md).
