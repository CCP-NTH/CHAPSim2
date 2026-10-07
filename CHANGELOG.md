# Changelog

## Unreleased - 2026-10-06

### Fixed

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

### Changed

- Changed the per-step MHD log output. `max-abs-elementary |div(j_vec)|` and
  `global electric current imbalance` are constraint residuals and are now
  printed under `Numerical Info`, next to the mass residuals, instead of under
  `Field Info`. `Field Info` gains the extrema of the current density
  (`jx`, `jy`, `jz`) and the Lorentz force (`lrfx`, `lrfy`, `lrfz`), each with
  its per-mille change from the previous step, to follow the development of
  the MHD field; both are from the start of the last substep. The JSON metrics
  are unchanged.

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

- Changed the source cited for the PbLi dynamic viscosity from the 1991 journal
  paper to the primary measurement report KfK-4144 (1986), Part II section 4.3,
  which has been read directly and prints the identical expression. The
  out-of-range diagnostic consequently names `dynamic viscosity (KfK-4144)`
  rather than `dynamic viscosity (Schulz 1991)`. The supported interval is
  unchanged at 521-625 K: the report states no range beside the equation, so
  that interval remains an implementation policy. The same report's heat-capacity
  range, 508-800 K, is now declared alongside it; it binds nothing today, because
  the melting point already sets 508 K and the viscosity cuts the top to 625 K.

### Added

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
