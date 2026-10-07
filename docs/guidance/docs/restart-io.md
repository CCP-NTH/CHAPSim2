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

If bundled metadata exists but does not match the requested restart mode, field
order, or mesh shape, the reader stops rather than reading incompatible binary
data silently.

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
