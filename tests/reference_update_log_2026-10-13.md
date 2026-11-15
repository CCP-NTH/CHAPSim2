# Reference update log — 2026-10-13

## Solver change
Optional `[thermo] property_interp = linear | pchip` (default linear) with a per-checkpoint interpolation record
(integration branch `integration/property-interp`, from investigation commit e0bf3c9). Linear mode is unchanged:
all existing outputs of the extended + functional suites, the restart suite and the mesh-mapping suite are
byte-identical to the d600227 baseline (only the new `thermo_interp_meta_<iter>.dat` sidecars are added; the
non-solution files `decomp_2d_setup.log` (2decomp version string) and appended monitor histories differ only in
build metadata and in the number of appended runs).

## Baselines
* **New:** `tests/regression/pipe_scp_inout_qw_pchip/reference.json` — `pipe_scp_inout_qw` with
  `property_interp= pchip` (the only input difference), generated with the integration build at NP 4 with
  `CHAPSIM_B3_PBAR=1` (as `run_regression.sh` sets). Against its linear twin: global pressure drop −0.36 %, full-step
  pressure drop −0.39 %, bulk enthalpy −3.7 %, mass-conservation residuals at round-off. Checking the linear twin's
  metrics against this reference fails on the full-step pressure keys (rel 3.9e-3 > 1e-3), so the case detects a
  silent fallback to linear interpolation.
* **Unchanged:** every existing `reference.json` and `tests/tools/tolerances.json`.
