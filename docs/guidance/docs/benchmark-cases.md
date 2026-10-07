# Benchmark and Validation Cases

The recommended approach for creating a new CHAPSim2 configuration is to locate the most similar existing case and duplicate it, then edit `input_chapsim.ini` according to your requirements. Runnable cases live in two places: `tests/regression/` holds the metric-gated cases that form the smoke and regression suites, and `tests/functional/` holds feature cases. The `validation/` directory contains case-specific post-processing assets, shared validation tools, suite manifests, and reference databases for selected canonical cases.

## Case Families

| Family | Representative test cases | Primary use |
| --- | --- | --- |
| Taylor-Green vortex | `tgv_iso`, `tgv_scp` | Periodic validation for core numerics, scalar/thermal coupling, and regression metrics |
| Channel flow | `channel_iso_periodic`, `channel_iso_inout`, `channel_scp_*` | Wall-bounded Cartesian configurations (periodic or inlet/outlet, isothermal or thermal) |
| Pipe flow | `pipe_iso_periodic`, `pipe_iso_inout`, `pipe_scp_*` | Cylindrical wall-bounded cases with radial/azimuthal constraints |
| Annular flow | `annular_iso_periodic`, `annular_iso_inout`, `annular_scp_*` | Cylindrical annular geometries with inner and outer wall treatment |

Suffixes in case names are used consistently:

| Suffix | Definition |
| --- | --- |
| `iso` | Isothermal flow configuration |
| `scp` | Scalar/thermal property case |
| `periodic` | Periodic streamwise direction (typically pressure-gradient or flow-rate controlled) |
| `inout` | Inlet/outlet boundary condition |
| `Tw` | Wall-temperature thermal boundary condition |
| `qw` | Wall-heat-flux thermal boundary condition |
| `cd4`, `cp4`, `cp6` | Non-default spatial scheme; every other case runs `cd2` |

## Functional Cases

`tests/functional/` covers features that the regression suite does not exercise. Each
family is a useful starting point when configuring that feature:

| Family | Covers | How it runs |
| --- | --- | --- |
| `LES_*` | WALE subgrid model in channel, pipe, annular and duct geometries, isothermal and thermal | Metric-gated, with the extended regression suite |
| `MHD_*` | Magnetohydrodynamics, including wall-normal and tilted (`_Btilt`) applied fields | Metric-gated, with the extended regression suite |
| `restart/` | Restart write/read across layouts and history modes | `tests/run_functional.sh` |
| `mesh_mapping/` | Restarting a developed field onto a different mesh | `tests/run_functional.sh` |
| `inlet_database/` | Stored inlet/outlet field data consumed by the inlet-database cases | Fixture, not run directly |

The `LES_*` and `MHD_*` cases are reached through the extended regression suite with
functional tests included; `tests/run_functional.sh` runs only the behavioural `restart/`
and `mesh_mapping/` sub-suites. See [Regression and Smoke Tests](testing.md).

## Use Cases and Starting Configurations

| Objective | Recommended starting case |
| --- | --- |
| Initial solver validation | `tests/regression/tgv_iso` |
| Periodic channel Direct Numerical Simulation | `tests/regression/channel_iso_periodic` |
| Channel with inlet/outlet conditions | `tests/regression/channel_iso_inout` |
| Periodic pipe Direct Numerical Simulation | `tests/regression/pipe_iso_periodic` |
| Pipe with inlet/outlet conditions | `tests/regression/pipe_iso_inout` |
| Annular periodic configuration | `tests/regression/annular_iso_periodic` |
| Thermal case with wall-temperature | Closest `*_Tw` case |
| Thermal case with wall-heat-flux | Closest `*_qw` case |

After copying a case, verify the following parameters:

1. `[domain] icase` and domain spatial extent
2. `[mesh] ncx`, `ncy`, `ncz`, `istret`, and `rstret`
3. `[flow] initfl`, `ren`, `idriven`, and boundary-condition selections
4. `[thermo]` only when thermal or scalar physics is enabled
5. `[io]` and `[statistics]` output frequencies before executing production simulations

## Validation Assets

| Path | Content |
| --- | --- |
| `validation/cases/channel/iso_periodic/post/` | Channel velocity/stress plotting and wall-unit postprocessing scripts. |
| `validation/references/channel/mkm/retau180/` | Reference profile data for channel comparison. |
| `validation/references/channel/mkm/retau395/` | Higher-Re channel reference profiles. |
| `validation/cases/pipe/iso_periodic/post/` | Pipe velocity/stress plotting scripts. |
| `validation/references/pipe/tdl/retau180/` | Pipe reference data at friction Reynolds number near 180. |
| `validation/references/pipe/tdl/retau550/` | Pipe reference data at higher friction Reynolds number. |
| `validation/tools/scripts/` | Shared monitor, mesh-check, and rebuild scripts. |
| `validation/suites/` | Smoke/regression/extended suite manifests. |

## How Benchmarks Fit the Workflow

Use benchmark cases in three sequential stages:

1. **Build confidence**: Run a small smoke test or Taylor-Green vortex case immediately after compilation
2. **Create new configuration**: Copy the most similar case and adapt the input file to your requirements
3. **Validate results**: Compare monitor histories, mean profiles, stresses, and available reference data

For automated checking, see [Regression and Smoke Tests](testing.md). For
profile plotting and visualisation, see [Postprocessing and Output Data](postprocessing.md).
