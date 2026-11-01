# Restart I/O Modes

This page documents the restart input switches and the restart files written
under the supported combinations of layout, physics, and history mode.

## Input Controls

Restarting requires the normal initialisation switches plus the `[io]` restart
layout and history controls.

| Section | Variable | Values | Meaning |
| --- | --- | --- | --- |
| `[flow]` | `initfl` | `0` | Read the flow field from restart files. |
| `[flow]` | `irestartfrom` | integer | Restart iteration to read for the flow field. |
| `[thermal]` | `inittm` | `0` | Read the thermal field from restart files. Required only for thermal cases. |
| `[thermal]` | `irestartfrom` | integer | Restart iteration to read for the thermal field. |
| `[io]` | `restart_data_layout` | `per_field`, `bundled` | Alias that sets both read and write layouts. Default: `per_field`. |
| `[io]` | `restart_data_layout_read` | `per_field`, `bundled` | Layout used when reading existing restart files. |
| `[io]` | `restart_data_layout_write` | `per_field`, `bundled` | Layout used when writing new restart files. |
| `[io]` | `restart_history_mode` | `exact`, `compact` | Restart history policy. Default: `exact`. `compact` is isothermal only. |
| `[io]` | `reset_unit_massflux` | `.true.`, `.false.` | Optional rescaling of restored streamwise bulk velocity. Default: `.false.`. |

`restart_data_layout_read` and `restart_data_layout_write` can be different.
Use this when converting older per-field restart data to bundled output, or when
writing bundled files from a case that was restarted from per-field files.

## History Modes

| Mode | Files contain | Restart behavior | Typical use |
| --- | --- | --- | --- |
| `exact` | Primary fields, pressure, RHS history, and thermal derived-property fields where applicable. | Designed to reproduce a continuous run after restart. | Regression tests, production checkpoints, debugging. |
| `compact` | Primary flow fields and required boundary state only. RHS history is not stored. | The first restarted AB2 step uses startup history. RK3 can rebuild substep history during the restarted step. Results are not expected to be bitwise identical to a continuous run. | Smaller checkpoints for isothermal runs when exact restart equivalence is not required. |

The default is `exact`. Use `compact` only when smaller checkpoints are more
important than exact continuous-run equivalence.

**Exact restart is not supported under `rhoh_recovery = lagged_legacy`** (deprecated, kept for
regression comparison only). With imposed-heat-flux (Neumann) walls, a continuation departs from
the continuous run: on that path the restart rebuilds the wall states from an interior re-derived
through the property inverse, which does not reproduce the stored state bit for bit (observed:
`channel_scp_periodic_qw` with `iaccuracy = cd4`, restarts at 20 and 40; a configuration that
happens to restart exactly is not a guarantee). The default
`rhoh_recovery = transported` restarts exactly.

**`compact` is rejected for thermal / variable-property runs.** A thermal
checkpoint would have to rebuild `rho`, `mu`, `T`, `h`, `k` and `sigma` from
`rhoh` through the property table, and that inverse lookup is not
bit-reproducible, so the restarted run would not follow the same trajectory.
The input reader stops with an error if `restart_history_mode = compact` is
combined with `is_thermo`, at write time as well as read time, so no unusable
compact thermal checkpoint can be produced. Thermal runs always store the full
history.

## Bundled Restart Output

Bundled mode writes one binary bundle per field group plus a metadata file that
records the field order and expected shapes.

### Isothermal Flow

| History mode | Files | Field order in `domainN_flow_restart_ITER.bin` |
| --- | --- | --- |
| `exact` | `domainN_flow_restart_ITER.bin`, `domainN_flow_restart_meta_ITER.dat`, `domainN_checkpoint_meta_ITER.dat` | `qx`, `qy`, `qz`, `pr`, `mx_rhs0`, `my_rhs0`, `mz_rhs0` |
| `compact` | `domainN_flow_restart_ITER.bin`, `domainN_flow_restart_meta_ITER.dat`, `domainN_checkpoint_meta_ITER.dat` | `qx`, `qy`, `qz`, `pr` |

### Thermal / Variable-Property Flow

Thermal flow has both a flow bundle and a thermal bundle. Only `exact` is
available.

| History mode | Flow bundle field order | Thermal bundle field order |
| --- | --- | --- |
| `exact` | `gx`, `gy`, `gz`, `qx`, `qy`, `qz`, `pr`, `mx_rhs0`, `my_rhs0`, `mz_rhs0` | `rhoh`, `temp`, `ene_rhs0`, `dens`, `visc`, `henth`, `kcond`, `econd` |

### Convective X-Outlet State

When the x direction uses a convective outlet, bundled restart files also carry
the outlet-boundary state required by the boundary condition.

| Physics | History mode | Extra flow-bundle fields |
| --- | --- | --- |
| Isothermal | `exact` | `fbcx_qx`, `fbcx_qy`, `fbcx_qz`, `fbcx_a0cc_rhs0`, `fbcx_a0pc_rhs0`, `fbcx_a0cp_rhs0` |
| Isothermal | `compact` | `fbcx_qx`, `fbcx_qy`, `fbcx_qz` |
| Thermal | `exact` | `fbcx_gx`, `fbcx_gy`, `fbcx_gz`, `fbcx_qx`, `fbcx_qy`, `fbcx_qz`, `fbcx_a0cc_rhs0`, `fbcx_a0pc_rhs0`, `fbcx_a0cp_rhs0`, `fbcx_ftp_d`, `fbcx_ftp_rhoh` |

Convective outlet restart is currently supported for the x direction only.

## Per-Field Restart Output

Per-field mode writes one binary file per field. The filenames follow the
standard pattern:

```text
1_data/domainN_FIELD_ITER.bin
```

For example:

```text
1_data/domain1_qx_40.bin
1_data/domain1_pr_40.bin
```

### Isothermal Flow

| History mode | Required/read fields |
| --- | --- |
| `exact` | `qx`, `qy`, `qz`, `pr`, `mx_rhs0`, `my_rhs0`, `mz_rhs0` |
| `compact` | `qx`, `qy`, `qz`, `pr` |

### Thermal / Variable-Property Flow

| History mode | Required/read flow fields | Required/read thermal fields |
| --- | --- | --- |
| `exact` | `gx`, `gy`, `gz`, `qx`, `qy`, `qz`, `pr`, `mx_rhs0`, `my_rhs0`, `mz_rhs0` | `rhoh`, `temp`, `ene_rhs0`, `dens`, `visc`, `henth`, `kcond`, `econd` |

### Convective X-Outlet State (Per-Field)

With a convective x-outlet, per-field mode also stores the outlet-boundary
state, so that a per-field restart continues exactly as a bundled one:

| File | Written when | Content |
| --- | --- | --- |
| `domainN_flow_xoutlet_ITER.bin` | convective x-outlet | the extra flow-bundle fields of the table above (same order, same writer as the bundle tail) |
| `domainN_fbcx_rhoh_rhs0_ITER.bin` | thermal run with convective x-outlet | outlet rho*h RHS history |
| `domainN_fbcx_dhat_ITER.bin`, `domainN_fbcx_dhat_rhs0_ITER.bin` | as above, `rhoh_recovery = transported` | outlet transported density and its RHS history |
| `domainN_flow_xoutlet_meta_ITER.dat` | thermal run with convective x-outlet | one line, `fbcx_thermal_state rebuild|initial` (see *Provenance of the x-boundary thermal state*) |
| `domainN_thermo_clock_ITER.dat` | every thermal checkpoint | one line, `flow_iteration <n>` (see *Flow step of a thermal checkpoint*) |

Per-field restart directories written before these files existed are still
read: a missing file is reported with a warning ("... the continuation is NOT
exact") and the outlet then restarts from its initial state, as before. Such a
restart is not a bit-for-bit continuation. The `fbcx_*` thermal files are only
used with a convective thermal outlet (`ifbcx_t = ...,7`); with a Neumann
thermal outlet their absence is still warned about but does not change the
result.

### Heated LES State

A heated LES run (`[les]` with a thermal field) also stores the lagged subgrid
state that the first energy substep after a restart reads: the WALE eddy
viscosity `tvisc` and the subgrid Prandtl number `prsgs`. They are written with
the thermal checkpoint:

| Layout | Files |
| --- | --- |
| bundled | `domainN_les_restart_ITER.bin` and `domainN_les_restart_meta_ITER.dat` (fields `tvisc`, `prsgs`) |
| per-field | `domainN_tvisc_ITER.bin`, `domainN_prsgs_ITER.bin` |

They are read on a flow-and-thermal continuation, after the initial WALE call,
but not with `restart_clock = reset` (the checkpoint is then an initial
condition). A checkpoint without them (older, or from field interpolation) is
read with a warning: the subgrid viscosity is recomputed from the restart field,
Pr_sgs starts at its high-Peclet limit, and the continuation is not
bit-continuous. A per-field checkpoint with only one of the two files, or a
bundle metadata file without its bundle, stops the run. Isothermal LES needs
neither file.

Older *isothermal* per-field restart directories that do not contain RHS-history
files should be read with:

```ini
restart_history_mode = compact
```

## Metadata Files

Bundled restart uses metadata for validation.

| File | Purpose |
| --- | --- |
| `domainN_checkpoint_meta_ITER.dat` | User-facing checkpoint manifest: iteration, time, `dt`, and checkpoint-level information. |
| `domainN_flow_restart_meta_ITER.dat` | Field order and shapes for `domainN_flow_restart_ITER.bin`. |
| `domainN_thermo_restart_meta_ITER.dat` | Field order and shapes for `domainN_thermo_restart_ITER.bin`. |
| `domainN_thermo_interp_meta_ITER.dat` | Thermal numerical-mode record: `property_interp`, and, when relevant, `enthalpy_advection` and the latest permitted scheme switch. Copy it with the thermal checkpoint. |
| `domainN_outlet_split_ITER.dat` | Only with `outlet_mass_correction = split` (EXPERIMENTAL): the one state value of the split outlet correction. Copy it with the checkpoint; without it the restart starts with `S = 0` for one step and is no longer bit-for-bit continuous. |

If bundled metadata exists but does not match the requested restart mode, field
order, or mesh shape, the reader stops rather than reading incompatible binary
data silently. It also stops if a bundle `.bin` is shorter or longer than the
arrays its metadata lists (for example a partly copied file), or if
`precision_bytes` differs from the build, naming the file and both sizes.

### Thermal Mode Record and Restart Policy

A thermal restart compares the record `domainN_thermo_interp_meta_ITER.dat` with
the run's input:

| Record vs run | Result |
| --- | --- |
| `property_interp` differs (linear/pchip) | stop (always; not overridable) |
| no record, run uses `pchip` | stop |
| `enthalpy_advection` the same | continue |
| `enthalpy_advection` differs, `enthalpy_advection_restart = strict` (default) | stop |
| `enthalpy_advection` differs, `allow_switch`, statistics restart at or after the restart iteration | continue with a warning; later records carry `enthalpy_advection_switch <from>-><to> <iteration>` |
| `enthalpy_advection` differs, `allow_switch`, statistics would continue (`stat_istart` < restart iteration) | stop |
| record unreadable, unrecognised line, or another domain/iteration | stop |

A record without an `enthalpy_advection` line means `cd2`. Binary checkpoint
formats are not affected. See `enthalpy_advection` in the input-file guide.

### Provenance of the X-Boundary Thermal State

A heated run with a convective x-outlet stores the x-boundary thermal states as
(rho, rho*h). On a restart under `rhoh_recovery = transported`, the states the
time loop builds from (rho*h, rho_hat) are rebuilt and kept, because
re-deriving them from the stored pair does not return the same state. The
checkpoint records where its states came from:
`fbcx_thermal_state rebuild` (written after an energy step) or `initial` (an
initial-state checkpoint, e.g. a mesh-mapped one). The line follows
`field_shapes` in `domainN_flow_restart_meta_ITER.dat` (bundled) or is the
content of `domainN_flow_xoutlet_meta_ITER.dat` (per-field); readers that
validate the fixed keys ignore it.

| Record | Restart |
| --- | --- |
| `rebuild` | keeps the rebuilt state, whichever build or compiler wrote the checkpoint |
| `initial` | re-derives the state from the stored pair |
| absent (checkpoint written before the record existed) | keeps the rebuilt state where it reproduces the stored pair bit for bit, otherwise re-derives it; on the correlation property path (liquid metals) a state that is not reproduced stops the run (see below) |

A checkpoint without the record whose correlation-path state the rebuild does
not reproduce (in practice: written by another build, compiler or flag set)
cannot be classified. The run stops on every rank and names the number of
points. Choose with the environment variable `CHAPSIM_LEGACY_FBCX_STATE`:
`keep` for a checkpoint written by the time loop (any checkpoint after the first
energy step), `rederive` for an initial-state checkpoint (the behaviour before
the record existed). Table fluids (supercritical water, CO2) are not affected.

### Flow Step of a Thermal Checkpoint

Thermal checkpoints and statistics are labelled by the run iteration, like the flow's: the
thermal field keeps the run clock also before `niterthermofirst`, when its equation is not yet
solved. A run whose energy equation starts after the flow therefore writes thermal checkpoints
at the same labels as the flow (from `niterthermofirst` on) and can be continued jointly.
Every thermal checkpoint records `flow_iteration <n>`, after `field_shapes` in
`domainN_thermo_restart_meta_ITER.dat` (bundled) or in `domainN_thermo_clock_ITER.dat`
(per-field), and the statistics metadata carry `label_clock run`.

Builds before 2026-10-30 advanced the thermal counter only while the energy equation was solved:
with `niterthermofirst > niterflowfirst` their thermal labels trailed the flow's, a thermal
checkpoint could carry the label (and the stored time) of an earlier flow checkpoint, and
`qw_ramp` ran on that trailing counter. A restart whose thermal checkpoint records a different
flow step than its label stops; a checkpoint without the record is read with a warning.

## Recommended Setups

### Exact Restart, Bundled Output

Use this for production checkpoints and restart-equivalence tests:

```ini
restart_data_layout = bundled
restart_history_mode = exact
```

### Compact Restart, Bundled Output

Isothermal runs only. Use this when checkpoint size matters more than exact
restart equivalence:

```ini
restart_data_layout = bundled
restart_history_mode = compact
```

### Read Per-Field, Write Bundled

Use this for converting an older restart directory:

```ini
restart_data_layout_read = per_field
restart_data_layout_write = bundled
restart_history_mode = compact
```

Use `exact` instead of `compact` only if the per-field directory includes the
RHS-history and thermal derived-property files listed above.

## Practical Checks

After a restarted run begins, inspect the startup log for:

- `restart input layout`
- `restart output layout`
- `restart history mode`
- restored bulk velocity or mass flux
- warnings about compact restart startup history
- metadata mismatch errors for bundled files

For exact restart validation, compare a continuous run with a stopped-and-restarted
run using the restart functional tests:

```bash
cd tests
./run_functional.sh
```
