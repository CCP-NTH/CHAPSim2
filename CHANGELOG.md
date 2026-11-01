# Changelog

## v2.3.0 - 2026-11-01

CPU release supporting the CHAPSim2 CPC paper. It collects the development
since v2.2.0: the transported-density (Bc) recovery of the thermodynamic
state, now the default; optional split outlet mass correction and
zero-gradient thermal outlet pair for convective x-outlets; optional Koren
enthalpy advection and PCHIP property interpolation; the statistical-window
post-processing mode; restart integrity (exact heated, LES, per-field and
mesh-mapped restarts, checkpoint provenance records, bundle size checks);
correctness fixes found while porting the solver to GPUs; and the structured
restart, unit and functional tests that verify them. Options marked
EXPERIMENTAL are verified only for the configurations stated in their entries
and in the user guide. Regression baselines moved where a fix changed results;
each move is explained in `tests/reference_update_log_*.md`.

### Added

- Added the optional `[bc]` key `outlet_mass_correction` (EXPERIMENTAL, after
  `drivenfc`) for convective x-outlets. `proportional` (default, unchanged)
  scales the outlet face mass flux by `s = 1 + R/mdot_out` at every RK stage;
  `uniform_flux` adds the same mass-flux increment `R/A_out` to every outlet
  face instead (`enforce_domain_mass_balance_dyn_fbc`); `split` applies the
  slow part `S_k = alpha_k * sum(R over the previous step)` proportionally and
  the stage-periodic remainder `R_k - S_k` uniformly, with its one state value
  written to `1_data/domain<id>_outlet_split_<iter>.dat` at each checkpoint
  (`write_outlet_split_state`). All variants close the stage balance
  `int drho/dt dV + mdot_out - mdot_in = 0` exactly; only the profile of the
  correction differs. `split` is the setting used for the Nemati/Bae case B
  supercritical CO2 pipe DNS; it is verified for that configuration only.

- Added the optional `[thermo]` key `thermal_outlet_pair` (EXPERIMENTAL;
  `convective`, default and unchanged; `zero_gradient`) for a convective
  thermal x-outlet under `rhoh_recovery = transported`. With `zero_gradient`,
  `update_fbcx_convective_outlet_thermo` sets the outlet face (rho*h, rho_hat)
  to the last interior cell's pair, so the outflow enthalpy flux is the upwind
  g_b h(nc) and no separately time-integrated face state exists; the
  convective face pair let rho_hat drift from rho(h) in the outlet cells.
  Outlet backflow faces are counted and reported. The key must directly follow
  `enthalpy_advection_restart` (or its position) and is rejected for other
  outlet types, `lagged_legacy` and convective z-outlets.

- Added the statistical-window post-processing mode. `is_postprocess = .true.`
  in `[process]`, which was read but had no effect, now runs post-processing
  only: with `pp_stat_window = a, b` it rebuilds the statistics averaged over
  iterations a+1 ... b from the cumulative statistics checkpoints at a and b,
  `A = (N_b A_b - N_a A_a)/(N_b - N_a)` per stored raw moment, using the
  sample counts of each group (flow, thermo, MHD) from their manifests. No
  field is initialised and no time step is taken; nothing in the case's
  `1_data`, `2_visu`, `3_monitor` or `4_check` is written. Output goes to the
  fixed directory `5_postprocess/window_<a>_<b>/` (an existing window
  directory stops the run) in the existing statistics and profile formats,
  with a provenance record `stat_window_provenance.dat`. Snapshots without a
  sample count (written before 2026-10-06), a changed `stat_istart`,
  signature, field list or mesh, a truncated bundle, `N_b - N_a /= b - a`, and
  flow and thermal statistics known to cover different solver steps are
  rejected before any output is created; reconstructed variances negative
  beyond round-off stop the run before the statistics are written. New
  routines `check_stats_window`, `read_stats_window`, `write_stats_window`
  (`post_statistics.f90`), `Prepare_postprocess_output`, `Postprocess_chapsim`
  (`chapsim.f90`). Normal runs are unchanged. Tests:
  `tests/unit_stat_window/`, `tests/functional/stat_window/`.

- Added the EXPERIMENTAL optional `[thermo]` key `enthalpy_advection` (`cd2`,
  default and unchanged; `koren`): Koren-limited MUSCL face value of h in the
  convective flux g*h_f of the rho*h equation, upwinded on the existing face
  mass flux, with the conservative P2C divergence, cylindrical factors, RK3,
  projection, diffusion and boundary conditions unchanged
  (`Get_[xyz]_midp_C2P_koren_3D`, `Compute_energy_rhs`). Requires
  `iaccuracy = cd2`; independent of `property_interp`; the key follows
  `property_interp` (or takes its position), and a misplaced or repeated line
  stops the run. No clipping. Inputs without the key are unchanged bit for bit.

- Added the optional lines `enthalpy_advection <cd2|koren>` and
  `enthalpy_advection_switch <from>-><to> <iteration>` to the thermal-checkpoint
  record `domain<d>_thermo_interp_meta_<iter>.dat` (written by koren runs and by
  runs carrying a permitted switch; cd2 records otherwise unchanged; binary
  checkpoints unchanged).

- Added the optional `[thermo]` key `enthalpy_advection_restart` (`strict`,
  default; `allow_switch`), read directly after `enthalpy_advection`:
  `check_thermo_interp_metadata` stops a thermal restart whose checkpoint was
  advanced with the other enthalpy-advection scheme unless `allow_switch` is
  given, and then still stops if the statistics averaging window would
  continue across the change; a permitted change is warned about and recorded
  in later checkpoint records. Records without the line count as cd2; the
  property_interp restart rule is unchanged and not overridable.

- Added the optional `[io]` key `ndb_sampling_dt` (directly after
  `ndb_file_offset`): the physical time between stored inlet-database planes.
  With `ndb_sampling_dt = R dt` (R an integer, validated, and equal to the
  bundle-metadata dt) the inlet is interpolated linearly in time between
  samples (`read_instantaneous_xinlet_tinterp`): database time is the run
  time, the stored planes are used unchanged at their sample times, the replay
  does not interpolate across its wrap, and a restart reads the same inflow as
  an uninterrupted run. Requires a constant dt. Inputs without the key, and
  R = 1, keep the step-indexed replay bit for bit.

- Added the EXPERIMENTAL optional `[thermo]` key `property_interp` (`linear`,
  default and unchanged; `pchip`): monotone piecewise-cubic Hermite
  interpolation of the property table in enthalpy (T, rho, mu, k, sigma_e,
  beta), C1 in h, with cp = 1/(dT/dh) of the interpolant and inverse
  temperature lookups by inversion of the same T(h). Table fluids with
  `rhoh_recovery = transported` only; the key follows `rhoh_recovery`
  (or `qw_ramp`, or `inout_buffer`) in the `[thermo]` block, and a
  `property_interp` line anywhere else, or given twice, stops the run with a
  message. Inputs without the key are unchanged. Removes the pressure impulses
  caused by d(rho)/dh jumps at table nodes; not yet approved for production use.

- Added the thermal-checkpoint record `domain<d>_thermo_interp_meta_<iter>.dat`
  (`write_thermo_interp_metadata`, both restart layouts) holding the property
  interpolation of the checkpoint; `check_thermo_interp_metadata` stops a
  thermal restart whose recorded mode differs from `property_interp`, or whose
  record is unreadable or belongs to another iteration, and continues
  checkpoints without a record (older versions) in linear mode only. Existing
  binary and metadata formats are unchanged.

- Added the full-step effective pressure `pr_fullstep` = sum_k alpha_k p_k
  (RK3 stages) as visualisation field with metadata sidecar, monitor metrics
  and probe companion files (env `CHAPSIM_B3_PBAR=1`; output only). The
  internal projection pressure `pr` is unchanged.

- Added opt-in mass and rho*h budget diagnostics `3_monitor/domain1_budget.log`
  (env `CHAPSIM_B3_BUDGET=1`; output only).

- Added an opt-in fixed-dt safety mode in `Check_cfl_convection` (env
  `CHAPSIM_B3_FIXED_DT=1`, limit `CHAPSIM_B3_FIXED_DT_CFLMAX`): dt is never
  changed and all ranks stop if the CFL exceeds the limit; needed with the
  iteration-indexed inlet database.

- Added experimental, opt-in pressure statistics paired with the completed
  step (env `CHAPSIM_B3_PSTAT=1`); not a production default.

- Added `known_limitation` support in `tests/tools/check_metrics.py`.

- Added partial restarts of a thermal run as initialisations. When only one of
  the flow and thermal fields is restarted, the restarted field is treated as
  an initial condition rather than a continuation:
  - flow restart, thermal field initialised afresh: the flow bundle is
    accepted in any layout (thermal exact, isothermal exact, isothermal
    compact; per-field layout reads `qx`, `qy`, `qz`, `pr`). Only `q`, `pr`
    and the outlet `fbcx_q*` planes are taken; `g = rho*u` is rebuilt from
    the new density, the stored `g*`, `fbcx_g*` and `fbcx_ftp_*` are
    discarded, and the momentum and outlet RHS histories are dropped
    (`read_flow_restart_bundle_initial`, `initialise_flow_from_restart_q`).
    Previously an isothermal bundle stopped the run with "Bundle field list
    mismatch", and a thermal bundle reused the stored `g` and outlet thermal
    state against a different thermal field.
  - thermal restart, flow field initialised afresh: `ene_rhs0` and
    `fbcx_rhoh_rhs0` are dropped.
  A full restart (isothermal, or thermal with both fields restarted) still
  requires the exact layout.

- Added `stat_istart` and `nsamples` to the statistics manifest
  `domain<N>_<group>_stats_meta_<iter>.dat`, so the number of samples behind a
  stored set of averages travels with them instead of being recomputed from the
  iteration number. They are appended after the keys the bundle validator reads
  by position, so new files still load in an older build and older files still
  load here - a checkpoint without them falls back to the derived weight and
  warns. `stat_istart` is validated on read and a run stops if it has changed
  since the checkpoint, because a stored average cannot be re-weighted onto a
  different window. The per-field restart layout, which previously wrote no
  manifest at all, now writes this one.

- Added `# samples` and `# window_first_iter` to the header of both the bundled
  and the per-field `Euu` spectrum output. The spectra are not checkpointed, so
  after a restart they average a shorter window than the `t_avg_*` fields
  alongside them; the headers now state which samples each file covers, and a
  restart that is accumulating spectra says so in the log.

- Added a positivity check on the property table built from polynomial
  correlations: a run stops at startup, naming the fluid, the property and the
  temperature, if density, dynamic viscosity, thermal conductivity or heat
  capacity is not positive anywhere in the tabulated range.

- Added `TP0min`/`TP0max` to the fluid parameters: the temperature interval the
  property table spans, kept distinct from the melting and boiling points. It
  starts as the liquid range and is narrowed by the validity range of each
  correlation that holds over less than it, and the property binding each end is
  recorded and named in the log and in every out-of-range diagnostic. A run
  whose `ref_t0` or `tini` falls outside the interval now stops at startup
  instead of extrapolating. PbLi is the only fluid narrowed at present, to
  521-625 K by its viscosity correlation; that interval is the overlap of two
  disagreeing source ranges and is an implementation policy, not an established
  physical validity range.

- Added the molar gas constant `RU_GAS` to `parameters_constant_mod`, so an
  activation energy can be written in its published units.

- Added `tests/tools/run_fluid_property_tests.py`, which runs the solver once
  per fluid and checks the emitted property table for `dH/dT = cp`, positive
  properties, falling density, LBE's density at 600 K, Li's thermal expansion
  and its analytic value at 600 K, and PbLi's KfK-4144 viscosity at 550 K and
  600 K and its tabulated range. Where the solver prints a directly evaluated
  reference-state property it is checked against that, at 1e-8 relative, rather
  than against the tabulated diagnostic, which is written with six significant
  figures. For sodium and LBE it also checks the solver's
  `cp` against the enthalpy polynomial differentiated independently in the test.
  It further checks that `water`, an unknown fluid name and a PbLi run outside
  521-625 K are all rejected, and that a lithium run above 3500 K stops on the
  range diagnostic rather than on the floating-point trap in its density
  correlation. It runs in the CI build job, after the build and before the smoke
  tests.

- Added `validation/thermal_properties/README.md`, recording what the two
  supercritical property tables actually are. Both were shown by reproduction
  to be generated output of NIST Standard Reference Data - the NIST Chemistry
  WebBook isobaric generator reproduces the water densities to 11 significant
  figures and the CO2 thermodynamic columns exactly. NIST Standard Reference
  Data is copyrighted under 15 U.S.C. 290e and is excluded from the general
  NIST redistribution grant. The README keeps three questions apart - evidence
  of provenance, evidence of an applicable restriction, and whether permission
  was found in this repository's records - and states that none was found,
  which is an absence of evidence rather than proof that none exists.

- Added `validation/thermal_properties/generate_property_table.py`, which
  builds a table in the solver's column format from CoolProp (MIT licence).
  CoolProp uses the same equations of state, so the thermodynamic columns match
  the shipped tables to round-off for water and to the file's own five
  significant figures for CO2; the transport columns differ by up to 5% because
  the correlations have since been revised. Replacing a shipped table with a
  generated one would therefore move the thermal baselines and is not a silent
  substitution.

### Changed

- Changed the thermal clock of a run whose energy equation starts after the
  flow (`niterthermofirst > niterflowfirst`): the thermal iteration and time
  now advance with every step up to `niterthermolast`, also before
  `niterthermofirst` (`chapsim.f90`); the energy equation is still solved from
  `niterthermofirst`. Thermal checkpoints and statistics therefore carry the
  run iteration of the flow step that wrote them, and `qw_ramp =
  .true., start, end` ramps over run iterations `start..end`, as documented;
  before, it ran on the trailing thermal count (e.g. thermal from 21, ramp
  21..30: wall heat flux zero until 40, ramp 41..50). Statistics metadata gain
  `label_clock run`; the statistical window treats such thermal snapshots as
  aligned and keeps the input-based derivation for older files. Runs with the
  thermal start at the flow start are unchanged.

- Changed `t_avg_fuuu` to accumulate the Favre triple moment rho u_i u_j u_k,
  as its declaration and the other f-statistics; it was weighted by rho*T
  (copied from `t_avg_tfuu`). Flow statistics metadata gain
  `fuuu_definition rho`; a statistics restart (`stat_level` 3, heated) from
  flow statistics without the key stops, and a statistical window whose ends
  differ in it stops (`check_stats_fuuu_definition`, `post_statistics.f90`). Test: `tests/stats_fuuu/run_test.sh`.

- Changed a fresh thermal start to build the boundary thermal states
  (imposed-heat-flux walls, Neumann outlets) from the initial interior field,
  as a restart from the same state does (`Rebuild_thermal_boundary_states`,
  `eq_energy.f90`, called by `initialise_thermo_fields` and
  `restore_thermo_variables_from_restart`). They stayed at the uniform initial
  state until the first energy step, so with temperature noise and a delayed
  energy start the momentum equation used, during the delay, wall states that
  a restart from step 0 did not reproduce (velocity ~1e-7, pressure ~5e-4
  relative after 60 steps); fresh and restarted runs now agree bit for bit.
  Fresh-start trajectories of heated runs with a non-uniform initial field and
  Neumann wall or outlet states change.

- Changed the restart of a checkpoint written before `fbcx_thermal_state`
  existed (below), on the correlation property path only (liquid metals): if
  the rebuilt x-boundary thermal state does not reproduce the stored one bit
  for bit, the run now stops on every rank with the number of points, instead
  of re-deriving the state from the stored (rho, rho*h) without notice. The
  environment variable `CHAPSIM_LEGACY_FBCX_STATE=keep` (a time-loop
  checkpoint) or `=rederive` (an initial-state checkpoint; the former
  behaviour) chooses. Same-build checkpoints, table fluids and checkpoints
  with the record are unaffected (`check_legacy_fbcx_state`, `io_restart.f90`).

- Changed the recovery of the thermodynamic state from the advanced rho*h to
  the transported density rho_hat (Bc): rho_hat is advanced with the energy
  RK3 weights and the lagged physical density rate, h = (rho h)/rho_hat,
  rho = rho_EOS(h). Input key `rhoh_recovery` (`transported`, default;
  `lagged_legacy`, deprecated, comparison only). The previous one-pass
  recovery was unstable independently of dt when |(h-h0) beta/cp| exceeded
  about 0.5 and its pressure contained a component proportional to 1/dt.
  Thermal regression baselines moved (tests/reference_update_log_2026-10-10.md).

- Changed property-range violations to a collective, informative stop of all
  ranks without clamping or extrapolation.

- Changed the per-step MHD log output. `max-abs-elementary |div(j_vec)|` and
  `global electric current imbalance` are constraint residuals and are now
  printed under `Numerical Info`, next to the mass residuals, instead of under
  `Field Info`. `Field Info` gains the extrema of the current density
  (`jx`, `jy`, `jz`) and the Lorentz force (`lrfx`, `lrfy`, `lrfz`), each with
  its per-mille change from the previous step, to follow the development of
  the MHD field; both are from the start of the last substep. The JSON metrics
  are unchanged.

- Changed the source cited for the PbLi dynamic viscosity from the 1991 journal
  paper to the primary measurement report KfK-4144 (1986), Part II section 4.3,
  which has been read directly and prints the identical expression. The
  out-of-range diagnostic consequently names `dynamic viscosity (KfK-4144)`
  rather than `dynamic viscosity (Schulz 1991)`. The supported interval is
  unchanged at 521-625 K: the report states no range beside the equation, so
  that interval remains an implementation policy. The same report's heat-capacity
  range, 508-800 K, is now declared alongside it; it binds nothing today, because
  the melting point already sets 508 K and the viscosity cuts the top to 625 K.

- Removed `validation/thermal_properties/NIST_CO2_8MP.DAT`. It is generated
  output of NIST Standard Reference Data 69, which is copyrighted under
  15 U.S.C. 290e and may not be redistributed without prior permission; no such
  permission is recorded. `ifluid= scp_co2` is unaffected as a solver option -
  supply the table in the working directory and the case runs, or generate one
  under the MIT licence with `generate_property_table.py`. Nothing in `tests/`
  selects `scp_co2`, so no case or baseline changes. The path is now in
  `.gitignore` and `check_validation_layout.py` asserts both that guard and
  that the file is untracked. `NIST_WATER_23.5MP.DAT` is unchanged and still
  shipped; its redistribution status is unresolved on the same evidence and is
  recorded as such in `validation/thermal_properties/README.md`.

- Changed `prepost/job_submission/run_local.sh` to take the CHAPSim2 directory
  from `CHAPSIM_DIR`, falling back to a `/path/to/CHAPSim2` placeholder and
  stopping with a message when that directory does not exist. It previously
  hardcoded one developer's absolute path, so an unedited run failed later and
  less clearly. The other job-submission helpers already had this treatment;
  this script was missed.

- Regenerated the FORD reference under `docs/code_structure/` from the current
  `src/`. The published bundle predated the statistics sample-count work, so it
  omitted `read_stats_sample_count` and `restore_stats_sample_count` and
  carried an older copy of several source files. Comparing file lists, as the
  previous check did, cannot detect that.

- Removed the bundled Moser-Kim-Mansour channel profiles
  (`validation/references/channel/mkm/`) and the unattributed
  `dnsEggels5300.asc` pipe profile (`validation/references/pipe/eggels/`) from
  the distribution: neither carries a licence or an explicit redistribution
  permission, and public availability is not permission.
  `validation/references/README.md` now gives the authoritative source, the
  required citation and the exact download commands for the MKM database, and
  `plot_channel_velo_stress.py` says so when `--ref-dir` is empty. Both paths
  are in `.gitignore` so a local download cannot be committed back. The Texas
  Data Repository pipe datasets are unaffected; they are CC0 and still shipped.
  No numerical test reads any of this data.

### Fixed

- Fixed the singular line of the y-direction tridiagonal Poisson solve with z
  walls (`poisson_11x_yskip`): the real and imaginary parts of a z-wall mode
  hold different cosine modes, and the combined zero-mode test never flagged
  the (0,0) line, which was solved without its compatibility projection
  (line residual 2.2e-3 of the source). The two parts now take their own
  flags; periodic-z runs are unchanged bit for bit. Check:
  `tests/tools/check_yskip_zero_mode_zwalls.sh`.

- Fixed the 1D profile inlet (`IBC_PROFILE1D`, BC id 9) being accepted
  although the path is unfinished for every variable: the streamwise velocity
  profile is never read, the profile interpolation passes the values as
  abscissae, the cylindrical radius scaling is applied to Cartesian cases, and
  the thermal profile is never converted and then reset to `tini`. The input
  check now rejects BC id 9 on the x inlet with a message.

- Fixed the initial temperature noise depending on the compiler: the hash index
  passed to `generate_random11_mixhash` in `initialise_thermal_properties` was
  unassigned. It is now 0, the value gfortran builds used, so their results are
  unchanged.

- Fixed `build_chapsim.sh` deleting the vendored `lib/2decomp-fft` (which has no
  `.git` of its own) in a fresh clone and cloning an unpinned upstream copy,
  which fails without network access; the vendored copy is accepted when its
  `CMakeLists.txt` is present.

- Fixed `inversion5_v2` reusing the previous line's multiplier on a zero pivot,
  as `inversion5_v1` already did; reached only by matrices with a zero pivot,
  which current inputs do not produce (static check
  `tests/tools/check_inversion5_zero_pivot.py`).

- Fixed the drho/dt damping towards a z convective outlet having no effect:
  `damping_drhodt` damped a z-pencil copy that was never written back. Reached
  only once the projected mass residual exceeds 1 (static
  check `tests/tools/check_damping_drhodt_z_writeback.py`).

- Fixed the misleading time-scheme and viscous-term options: `itimescheme =
  rk3_cn` runs the explicit RK3 scheme and `iviscous = semi_implicit` has no
  effect (no Crank-Nicolson or implicit diffusion is implemented). Both are
  still accepted, now with a warning, and the diffusion-number warnings no
  longer advise implicit diffusion.

- Fixed the first header line of the monitor history logs
  (`domainN_monitor_change_history.log`, `domainN_monitor_metrics_history.log`)
  printing an unassigned integer as `pt-id`; it is now 0, the value gfortran
  builds printed.

- Fixed the restart of the x-boundary thermal state from a checkpoint written
  by another build, compiler or flag set (`rhoh_recovery = transported`,
  convective x-outlet). The per-point bit test that kept the state rebuilt from
  (rho*h, rho_hat) fails when the property evaluation differs in the last bit,
  and the stored-pair re-derivation then moved the outlet rho*h (3.9e-11
  relative after one step for sodium from an FMA build; ~5e-8 reported for
  PbLi MHD across CPU and GPU builds). Flow checkpoints now record
  `fbcx_thermal_state rebuild|initial` (`fl%is_fbcx_thermal_rebuilt`, set by
  each energy step), after `field_shapes` in `domainN_flow_restart_meta_ITER.dat`
  or in the per-field sidecar `domainN_flow_xoutlet_meta_ITER.dat`; `rebuild`
  keeps the rebuilt state on Neumann- and Dirichlet-temperature sides,
  `initial` re-derives it, an absent record keeps the bit test. A convective
  thermal outlet (`ifbcx_t = ..., 7`) always keeps its stored advected rho*h.
  Same-build continuations and fresh runs are unchanged bit for bit; older
  executables read the new files. Test:
  `tests/restart_exact/run_test.sh` (`water_convT`).

- Fixed a silent mismatch when a run whose energy equation starts after the
  flow is restarted from checkpoints of builds before this one, whose thermal
  labels trailed the flow step (see the thermal clock above). Thermal
  checkpoints now record `flow_iteration <n>` (after `field_shapes` in
  `domainN_thermo_restart_meta_ITER.dat`, or in the per-field sidecar
  `domainN_thermo_clock_ITER.dat`), and `check_thermo_flow_iteration` stops a
  restart from a mismatched pair; a checkpoint without the record is read
  with a warning. The stop messages for a missing thermal checkpoint or
  thermal statistics file name the label offset of older builds. Test:
  `tests/restart_exact/run_test.sh` (`water_delayed`).

- Fixed the thermal state of a mesh-mapped (prerun) checkpoint lying off the
  equation of state: `build_up_interp_target_field_thermo` kept the
  interpolated rho*h while replacing rho by rho_EOS(h), and rho_hat is set to
  rho_EOS, so the first recovery h = rho*h/rho_hat of the remapped run jumped
  (max|rhoh - rho_hat h|/max|rhoh| 4.1e-4 on a coarse heated pipe remapped
  2x finer). rho*h is now stored from the rebuilt state, rho_EOS(h) h. This is
  thermodynamic consistency, not conservation. Test:
  `tests/restart_remap_thermo/run_test.sh`.

- Fixed bundled restart files being read without a size check
  (`restart_data_layout = bundled`; flow, thermal and LES bundles, including
  the compact-history flow bundle and a heated start from an isothermal
  precursor). A `.bin` shorter than its metadata lists was read past its end
  without an error (missing values left as zero, e.g. a truncated velocity
  component), and a longer one was read as its leading part.
  `validate_bundle_metadata` now stops on every rank, before any data are
  read, unless the file size equals the arrays listed in `field_shapes`, and
  unless `precision_bytes` matches the build's real kind; the message names
  the file and both sizes. Valid bundles and results are unchanged. Test:
  `tests/restart_bundle_size/run_test.sh`.

- Fixed the mass-conservation safety stop in `Check_element_mass_conservation`
  terminating heated restarts beyond iteration 10000 during initialisation: the
  pre-loop diagnostics (`initial`, `init-div-free`) evaluate div(g) + drho/dt
  before drho/dt has been computed, so they report max|drho/dt| of the restored
  state. The stop now applies only to the per-step check; the diagnostics are
  still printed and the per-step threshold is unchanged.

- Fixed inexact restarts of heated inlet/outlet runs with a Neumann
  temperature x-boundary under `rhoh_recovery = transported`:
  `read_flow_restart_xoutlet_state` re-derived the boundary states from the
  stored (rho, rho*h) with h = rho*h/rho after
  `restore_thermo_variables_from_restart` had rebuilt them exactly from
  (rho*h, rho_hat), and the re-derivation does not round-trip. The rebuilt
  state is now kept at every point where it reproduces the stored pair bit for
  bit; elsewhere (e.g. a mesh-mapped iteration-0 state) the stored pair is used
  as before. Fresh runs, `lagged_legacy` and the checkpoint format are
  unchanged; restarted runs with h /= 0 on that boundary now continue bit for
  bit. Test: `tests/restart_exact/run_test.sh`.

- Fixed inexact inlet/outlet restarts on the correlation property path
  (`IPROPERTY_FUNCS`: sodium, lead, bismuth, LBE, lithium, FLiBe, PbLi) under
  `rhoh_recovery = transported`: re-deriving a Dirichlet-T x-boundary state
  from the stored (rho, rho*h) through the tabulated h(T) inverse does not
  return the state the correlations gave for the prescribed temperature.
  `read_flow_restart_xoutlet_state` now applies the same per-point rule to
  Dirichlet-T sides as to Neumann-T sides (convective-T side unchanged). Table
  fluids are unaffected (the re-derivation is exact there); fresh runs and the
  checkpoint format are unchanged. Test: `tests/restart_exact/run_test.sh`
  (sodium).

- Fixed per-field restarts (`restart_data_layout = per_field`, the default) of
  runs with a convective x-outlet: the per-field writers stored no outlet
  state, so the outlet restarted from its initial state (isothermal channel:
  1.3e-3 in qx at the outlet ten steps later). Per-field checkpoints now also
  write `domainN_flow_xoutlet_ITER.bin` (the flow-bundle outlet fields, same
  routines as the bundle) and, for thermal runs, `fbcx_rhoh_rhs0` and (under
  `transported`) `fbcx_dhat`, `fbcx_dhat_rhs0`. Older per-field checkpoints
  are read as before with a warning that the continuation is not exact.
  Bundled checkpoints and fresh runs are unchanged. Test:
  `tests/restart_exact/run_test.sh` (water_perfield).

- Fixed inexact restarts of heated LES runs: the first energy substep after a
  restart reads `fl%tVisc` (WALE of the previous momentum substep) and
  `tm%prSgs` (refreshed by the previous energy substep), neither of which was
  checkpointed or can be rebuilt. `write_instantaneous_thermo` now also writes
  them for a heated LES run (bundled: `domainN_les_restart_ITER.bin` + meta;
  per-field: `tvisc`, `prsgs`), and `read_les_restart_state` restores them
  after the initial WALE call of a continuation (not for `restart_clock =
  reset`). Checkpoints without them are read as before with a warning that
  the continuation is not bit-continuous. Isothermal LES and fresh runs are
  unchanged; the LES model is unchanged. Test: `tests/restart_exact/run_test.sh` (les_heated).

- Fixed silent corruption of interpolated inlet-database planes on processor
  grids whose split of the database cross-section is misaligned with the main
  grid (e.g. p_col = 48 for a 128 -> 256 theta interpolation): the halo depth
  then exceeded the adjacent ranks' source blocks and update_halo returned
  unfilled values. `check_xinlet_database_halo_layout` now compares, on every
  rank and side, the source layers the interpolation reads with the actual
  neighbouring block extents and stops with a diagnostic before any
  interpolated value is used; compatible grids are unchanged bit for bit and
  the processor grid (including automatic p_row = p_col = 0) is never changed.

- Fixed the periodic seam of the inlet-database interpolation in z when the
  database cross-section mesh differs from the main domain
  (`interp_xinlet_database_array_yz`, `bilinear_interp_xinlet_point_halo`,
  `compute_xinlet_database_halo_level`, new `build_xinlet_z_search_coords`):
  in a periodic z direction, targets between the last source point and the
  period end (or below the first point) were given the end value instead of
  being interpolated across the seam. They now use the periodic neighbour
  through the existing global halo; interior targets, radial interpolation,
  non-periodic z and the matching-mesh direct read are unchanged bit for bit.

- Fixed exact restart of heated inlet/outlet runs (outlet boundary states and
  histories) and of the Bc state (rho_hat, density-rate history, convective
  thermal outlet).

- Fixed the convective thermal x-outlet to use the transported density.

- Fixed the default x-inlet temperature (`ifbcx_t` third field) never being
  applied when `[thermo]` precedes `[bc]`: a Dirichlet inlet given as `0.0`
  ran with an inlet at 0 K. A temperature BC value may now be the token
  `tini` (read by `read_temperature_bc_line`, resolved after all sections in
  `apply_inlet_temperature_default`); numeric values are explicit and never
  replaced, so `0.0` or a negative Dirichlet temperature is now an error.
  Fixed the crash of isothermal inputs with `[bc]` before `[thermo]`
  (unallocated `thermo`).

- Added `check_dirichlet_bc_temperatures`: Dirichlet boundary temperatures
  outside the property range (table rows or correlation interval) now stop
  the run before the simulation with the boundary named.

- Fixed the per-mille change printed with `|div(j_vec)|`, which was always
  `0.00‰` because `check_current_conservation` zeroed its tracker on every
  call; the previous-step value is now kept by the caller. Fixed the
  previous-step trackers in `Solve_eqs_iteration` (`maxmin_ep`, `maxmin_qx`,
  ...) being read uninitialised on the first step.

- Fixed the per-mille change printed with the mass residuals in
  `Check_element_mass_conservation`. Its tracker was zeroed on every call and
  shared between the inlet, outlet and bulk lines, so periodic cases always
  printed `0.00‰` and inlet/outlet cases compared each region with the one
  printed before it. Each region is now compared with its own value from the
  previous call (`fl%mcon`, `fl%mcon_projected`).

- Fixed the averaging weight used by the time-averaged statistics, which was
  derived from the iteration number as `iter - stat_istart` instead of counting
  the samples actually folded in. The two agree only while the sample stream is
  unbroken, so a run in which the flow restarts but the thermal field is
  initialised afresh, or in which `niterflowfirst`/`niterthermofirst` suppress
  sampling for a window, weighted its averages by more samples than it had and
  biased every `t_avg_*` field towards zero. `run_stats_action` now takes the
  population as `opt_nstat` from a counter on the owning field
  (`nstat_samples`), which `update_stats_flow`, `update_stats_thermo` and
  `update_stats_mhd` increment once per call; a `STATS_TAVG` call without it is
  a fatal error rather than a silent miscount. For an uninterrupted run the
  weight is unchanged.

- Fixed `init_stats_mhd` reloading the MHD statistics regardless of how the
  field was initialised: it tested `iterfrom` and `restart_clock` but not the
  initialisation mode, the test its flow and thermal siblings both apply. It
  now takes the flow's `inittype`, the MHD field having no initialisation mode
  of its own. `t_mhd%iterfrom` also gained a default initialiser and is set
  from the flow in the restart-clock block, so it no longer depends on `[mhd]`
  being parsed before the statistics are initialised.

- Fixed the liquid-sodium and LBE enthalpy coefficients `CoH_Na` and `CoH_LBE`
  in `parameters_constant_mod`. `CoH_Na` was a copy of the LBE row, so sodium's
  enthalpy was built from LBE's heat capacity, and `CoH_LBE(3)` was 100 times
  too large. Measured from the solver's own property table, `dH/dT` disagreed
  with the separately evaluated `cp` by 69 % for sodium at 462 K and by a factor
  of 3.8 for LBE at 576 K. Every `CoH_*` row is now written as the term-by-term
  integral of the corresponding `CoCp_*` row, so the identity `dH/dT = cp`
  cannot drift again; this also removes a rounding of 6.5e-05 relative in
  `CoH_Pb(3)`. The enthalpy datum is unaffected: `HM0` and `CoH(0)` cancel in
  the non-dimensionalisation.

- Fixed the sign of the LBE density slope `CoD_LBE(1)`, which made LBE denser
  as it got hotter. LBE density at 600 K goes from 11840.8 to 10289.2 kg/m3.

- Fixed `ifluid= water` silently running liquid sodium. There is no property
  correlation for ordinary liquid water, but the fluid selector fell through to
  a sodium default while the log still announced "Liquid Water". The input
  parser now rejects `water` and names `scp_water` as the alternative, and the
  fluid selector and both dynamic-viscosity selectors abort on an unknown
  `ifluid` instead of substituting sodium.

- Fixed the thermal-expansion coefficient of liquid lithium. The tabulated
  `1 / (CoB - T)` form is the exact `-(1/rho) drho/dT` only for a density that
  is linear in temperature, which Li's is not; `CoB_Li` overstated beta by
  15 % at 589 K. Li now differentiates its own density correlation. The
  coefficient is reported in the property table and is not used by the
  momentum equation, which carries the full variable density.

- Replaced the PbLi dynamic-viscosity correlation. The cubic that stood in
  `CoM_PbLi` is the fit printed by Martelli, Venturini & Utili (2019); it
  crosses zero at 859.0 K and is negative above it, inside the 508-873 K range
  that source's own table states. It is now the Arrhenius expression measured in
  KfK-4144 (Jauch, Haase & Schulz, Kernforschungszentrum Karlsruhe, 1986),
  Part II section 4.3, `M = 1.87e-4 * exp(11640 / (Ru T))` Pa s with
  `Ru = 8.314 J/mol/K`, as also implemented in INL MOOSE. `CoM_PbLi` changed shape from `(0:3)`
  polynomial coefficients to the `(-1:1)` Arrhenius form already used by Pb, Bi,
  LBE and FLiBe, and both the dimensional and the non-dimensional evaluation
  paths were updated with it. PbLi viscosity at 550 K goes from 1.278e-3 to
  2.384e-3 Pa s, a factor of 1.87.

- Fixed the order of the new startup temperature-range check in
  `buildup_property_relations_from_function`, which ran after the correlations
  had already been evaluated at the offending temperature. Liquid lithium's
  density carries `(1 - T / 3500)**0.467`, so a `ref_t0` or `tini` above 3500 K
  raised a negative base to a fractional power and the debug build's
  `-ffpe-trap=invalid` aborted with a bare SIGFPE before the diagnostic could be
  printed. The check now runs first, so the run stops with the message naming
  the property that sets the violated end of the range.

### Known limitations

- `rhoh_recovery = lagged_legacy` is deprecated and kept for regression
  comparison only; its restarts with imposed-heat-flux walls are not exact
  (documented in `restart-io.md`).
- The EXPERIMENTAL options `outlet_mass_correction = split`,
  `thermal_outlet_pair = zero_gradient`, `enthalpy_advection = koren` and
  `property_interp = pchip` are verified for the configurations stated in
  their entries, not in general.
- The fix to the drho/dt damping towards a z convective outlet is verified
  statically only: no shipped case has a heated z-outlet.
- The singular-line treatment of the y-direction Poisson solve with z walls is
  verified by its compatibility residual and the regression cases, not by a
  manufactured-solution test.
- The 1D profile inlet (BC id 9) is rejected at input; the feature is
  unfinished.
- Results depend on the number of MPI ranks and the decomposition at round-off
  level, through the summation order of global reductions; restarts at a fixed
  decomposition are exact.
- The public distribution does not include the NIST CO2 property table;
  `ifluid= scp_co2` then needs a user-supplied table (see
  `validation/thermal_properties/README.md`).

## v2.2.0 - 2026-10-05

### Added

- Added the build variable `CHAPSIM_FFT` to select the FFT backend used by the
  pressure-Poisson solver. The default, `generic`, is the transform bundled in
  2decomp-fft and needs nothing installed. `CHAPSIM_FFT=fftw` opts in to FFTW
  3, with `FFTW_ROOT` giving the installation prefix (default `/usr/local`;
  `lib`, `lib64` and Debian/Ubuntu multiarch library directories are all
  searched). Requesting FFTW when none can be found is now a fatal build error
  rather than a silent downgrade to the generic backend. Documented in
  `docs/guidance/docs/fft-backend.md`.
- Added `lib/2decomp-fft/build/opt/chapsim_fft_backend.mk`, written by
  `build/build_cmake_2decomp.sh` to record the backend the library was compiled
  with and the matching link flags. `build/Makefile` reads it, so the solver
  link always agrees with the library instead of re-deriving the choice.

- Added `get_ibc_for_sgs_coef_c2p` in `bc_general.f90`, which derives the
  boundary class of the subgrid coefficient from the nominal velocity boundary
  condition rather than reusing the velocity condition directly. A no-slip wall
  becomes a Dirichlet zero for the coefficient, so the subgrid stress and the
  subgrid enthalpy flux vanish on the wall face and all near-wall transport is
  carried by the molecular terms; an inlet or outlet becomes zero-gradient, so
  subgrid transport is retained across the plane. Used by the subgrid assembly
  in `eq_momentum2.f90` and by `add_sgs_enthalpy_flux` in `eq_energy.f90`.
- Added the z-direction property boundary buffers `fbcz_pc4` and `fbcz_cp4` to
  the thermal momentum assembly, so the `Get_z_midp_C2P_3D` calls that place the
  xz and yz shear-face viscosities are supplied with wall values instead of
  being left without an `opt_fbc`. Only reachable with a non-periodic z.
- Added five subgrid diagnostics to the metrics written by `write_metrics_json`
  in `io_monitor.f90`: `min. sgs coefficient on stress/flux faces`,
  `min. total face viscosity (molecular+sgs)`,
  `max. sgs coefficient on physical walls`,
  `max. sgs coefficient at inlet/outlet` and `max. |sgs enthalpy flux|`. They
  record the interpolated coefficient at the faces where it is consumed, so they
  assert positivity of the eddy viscosity, the molecular floor of the total face
  viscosity, and the zero-at-wall / retained-at-inlet-outlet treatment above.
  Written for LES runs only; `max. |sgs enthalpy flux|` for thermal LES only.
- Added LES model selection through integer `LES_model`, with `ILES_NONE` for
  DNS/default operation and `ILES_WALE` for the WALE LES model.
- Added the WALE LES implementation and integrated it into the momentum solver
  for both isothermal and thermal-flow viscosity handling.
- Added a unified pipe-axis halo and centre reconstruction routine,
  `axis_mirror_fbcy`, replacing the older even/odd-only mirroring routines.
- Added axis reconstruction modes for regular cylindrical centreline behaviour:
  `AXIS_RECON_NONE`, `AXIS_RECON_ZERO`, `AXIS_RECON_M0`, `AXIS_RECON_M1`, and
  `AXIS_RECON_M0_M2`.
- Added pipe-centre reconstruction support in momentum, energy, statistics,
  pressure-gradient, and MHD-related halo updates so scalar-like, vector-like,
  and quadratic/tensor-like terms use appropriate centreline regularity.
- Added a random-initialisation envelope that damps perturbations near channel,
  pipe, and annular boundaries instead of applying a flat perturbation level.
- Added an interactive `build/Makefile` default target that can optionally clean
  first and choose between default, GNU, Intel, Cray, and NVHPC CPU build modes.
- Added the optional `[simcontrol]` key `restart_clock`, with values `continue`
  (default) and `reset`, which selects how a restart maps onto the run
  timeline. `continue` adopts the checkpoint iteration and time as the run
  clock and reproduces the previous behaviour of an uninterrupted run. `reset`
  treats the checkpoint as an initial condition only: the clock starts at
  iteration 0 and time 0, the stored momentum and energy RHS history is
  discarded, and the stored statistics are not read, so every
  iteration-numbered input must be expressed on the new clock.

### Changed

- Changed FFT backend selection in `build_chapsim.sh` from automatic to
  explicit. The build previously chose FFTW whenever `FFTW_ROOT` looked
  plausible; it now builds the generic backend unless `CHAPSIM_FFT=fftw` is
  given, and prints which backend it selected. When the requested backend
  differs from the one the existing `libdecomp2d.a` was built with, the
  2decomp-fft library and the solver are both rebuilt from clean, since the
  transform and its module files are compiled into the library.

- Changed a restart that advances only one of the two fields so that both
  fields share a single run clock. Previously `fl%iteration` and
  `tm%iteration` were each set to their own `irestartfrom`, so restarting the
  flow from iteration N while initialising the thermal field fresh left the two
  counters at N and 0; the solver loop then started at the smaller of the two
  and the restarted field stayed frozen behind its gate for N iterations while
  the other one spun up, and the two fields wrote output files under different
  iteration numbers. The run clock is now derived once at input-parsing time
  into `dm%iteration_start` and adopted by the flow, thermal and MHD fields
  alike, so a fresh field is injected at the restart iteration and both advance
  together from the next step. Runs in which both fields restart from the same
  checkpoint, or in which neither restarts, are unaffected.
- Changed the cell-centre-to-face interpolation of the subgrid coefficient, in
  both `eq_momentum2.f90` and `add_sgs_enthalpy_flux`, to use `IACCU_CD2`
  unconditionally instead of the run's `iaccuracy`. CD2 is the only midpoint
  interpolation in the set with two positive weights, so it cannot return a
  negative eddy viscosity; per unit peak, cd4, cp4 and cp6 undershoot by 0.125,
  0.221 and 0.362. CD2 is also the only one whose boundary row reduces to the
  intended zero-gradient at an inlet or outlet. The trade-off is explicit: the
  subgrid term is now second-order accurate even in a cp4 or cp6 run. The
  resolved convective, molecular and pressure terms keep their formal order, and
  the subgrid contribution is a model term, so the formal order of the
  discretisation of the Navier-Stokes terms is unchanged.
- Updated LES turbulent viscosity handling so `tVisc` stores turbulent dynamic
  viscosity and combines consistently with molecular dynamic viscosity in the
  effective momentum viscosity.
- Modernised the LES module structure by removing global scratch arrays,
  using local working fields, adding explicit interfaces/imports, and documenting
  the WALE helper routines.
- Reworked pipe-centre treatment for cylindrical flows to enforce single-valued
  scalar quantities and regular first/second azimuthal-mode behaviour at the
  axis.
- Updated thermal-property interpolation and energy RHS assembly to pass
  boundary-condition halos into midpoint and derivative operations, then
  reconstruct pipe-axis values where required.
- Updated cylindrical `q/r` and radial derivative handling to reconstruct
  centreline values by azimuthal projection rather than by the previous
  lower-order estimates.
- Updated conservative/primary velocity refresh order in the momentum solver so
  thermal cases convert conservative variables back to velocities before
  enforcing velocity boundary conditions.
- Changed cylindrical visualisation binary output to little-endian and updated
  the generated XDMF metadata accordingly.
- Tidied module `use` lists across the Fortran sources: unused modules were
  removed and remaining imports were alphabetically ordered for readability and
  easier review.

### Fixed

- Fixed FFTW never actually being used, in two places that had to be corrected
  together. `build/build_cmake_2decomp.sh` passed `-DFFT_Choice=FFTW_F03`
  where 2decomp-fft matches `fftw_f03` case-sensitively, so CMake accepted the
  value, failed the match and compiled the generic transform while the build
  log reported FFTW; and `build/Makefile` carried no `-lfftw3`, so correcting
  the case alone left the solver link failing on undefined `fftw_*` symbols.
  Every CHAPSim binary produced before this change used the generic backend,
  including those that announced FFTW.

- Fixed an unreachable reset of the flow iteration and time in
  `initialise_thermo_fields`. It was written to zero the clock when the thermal
  field was initialised fresh, but `initialise_thermo_fields` runs before
  `initialise_flow_fields`, so the flow restart read overwrote it every time.
  The clock is now set once, after the last field has been read.
- Fixed `irestartfrom` in `[thermo]` being retained when `inittm` is not
  `restart`. A leftover value in a non-restart `[thermo]` block is now cleared,
  matching the existing treatment of `[flow]`, so it cannot be mistaken for a
  run-clock origin.
- Fixed a restart in which the flow and the thermal field named different
  checkpoints being accepted silently. Because a run advances on one timeline,
  mismatched `irestartfrom` values are now rejected during input parsing with a
  message naming both remedies.
- Fixed four 2decomp transpose calls that were given a decomposition descriptor
  not matching the staggered location of their arrays. In `para_conversion.f90`,
  the density transposed for the `fbcz_qx` scaling omitted `dm%dpcc` and so used
  the default cell-centred descriptor, and the density for the `fbcz_qy` scaling
  called `transpose_y_to_x` where the target was a z-pencil array. In
  `eq_momentum2.f90`, two z-staggered arrays were transposed with `dm%dpcc`
  instead of `dm%dpcp` and `dm%dccp`. All four are invisible under z periodicity,
  where 2decomp sets `np = nc` for a 'p' location and the descriptors share a z
  extent; with a z wall `np = nc+1` and the transpose reads or writes past the
  end of the array on more than one rank.
- Fixed the WALE tensor invariant and eddy-viscosity denominator formulation,
  including zero-denominator protection.
- Fixed inlet database reads for short smoke-test runs so an empty read period
  does not lead to `mod(..., 0)` when smoke iterations end before `ndbstart`.
- Fixed thermal restart handling so restart cases do not require the inlet
  thermal boundary condition checks that apply only to fresh initialisation.
- Fixed statistics restart/read handling by allowing statistics arrays to be
  populated in `STATS_READ` mode, not only accumulated in `STATS_TAVG` mode.
- Fixed the statistics averaging count after restart by using
  `iter - dm%stat_istart` instead of adding one extra sample.
- Fixed `is_IO_off` behaviour so initial visualisation, mesh/check-file output,
  monitor/probe history files, outlet-record output, folder creation, and
  initial thermo/flow visualisation are skipped when I/O is disabled.
- Fixed the Makefile object list by removing stale merge-conflict markers around
  `eq_continuity.o`.
