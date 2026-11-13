# CHAPSim Input File Guide

CHAPSim2 reads simulation parameters from an INI-style configuration file, typically named `input_chapsim.ini`. Input generation tools may produce files named `input_chapsim_auto.ini` or `input_chapsim_gui.ini`; before solver execution, ensure the selected file is placed or renamed as `input_chapsim.ini` in the case directory.

The file parser operates on a section-based structure; however, variable order within sections is significant. Maintain variable order as shown in generated templates.

## What This File Controls

The input file establishes the primary interface between user specifications and the Fortran solver. It controls:

- **Physical configuration**: Geometry specification, Reynolds number, thermal/magnetohydrodynamic options, and working-fluid properties
- **Numerical discretization**: Grid resolution, domain extent, mesh stretching, time-stepping, and spatial/temporal discretization schemes
- **Boundary conditions**: Periodic directions, inlet/outlet treatment, wall velocity specifications, wall temperature, and wall heat flux
- **Simulation control**: Iteration ranges for flow and thermal field computation
- **Output specification**: Restart checkpoint frequency, visualization output frequency, statistics accumulation parameters, and database plane I/O
- **Diagnostics**: Probe location definitions and monitoring-output frequency

For new configurations, begin with the most similar existing `tests/regression/*/input_chapsim.ini` template or generate a file using `prepost/input_generator/autoinput_script.py` or `autoinput_gui.py`, then edit accordingly.

## Basic Rules

| Property | Format |
| --- | --- |
| Boolean values | Fortran logical syntax: `.true.` or `.false.` |
| Integer values | Whole-number identifiers or counters |
| Real values | Decimal or scientific notation (e.g., `1e-05`) |
| List values | Comma-separated (e.g., `veloinit= 0.0,0.0,0.0`) |
| Domain lengths | Nondimensional with respect to reference half-height, radius, or equivalent case length |
| Thermal parameters | SI units before solver nondimensionalization |
| Reynolds numbers | Based on channel half-height, pipe radius, or equivalent case reference length |
| Boundary condition rows | Format: `bc_low,bc_high,value_low,value_high` |
| Comments | Lines beginning with `#` or `;`, blank lines, or indented comment lines are ignored |

## `[process]`

Governs high-level execution mode selection.

| Variable | Fortran type | Meaning |
|---|---|---|
| `is_prerun` | `logical` | If `.true.`, run preprocessing/recommendation logic only. |
| `is_postprocess` | `logical` | If `.true.`, run the statistical-window post-processing mode only: no time step is taken. See [Statistical-Window Reconstruction](postprocessing.md#statistical-window-reconstruction). Cannot be combined with `is_prerun`. |
| `pp_stat_window` | `integer(2)` | Required when `is_postprocess = .true.`: `a, b` with `0 <= a < b`. Rebuilds the statistics averaged over iterations `a+1 ... b` from the cumulative statistics written at `a` and `b`. The output always goes to `5_postprocess/window_<a>_<b>/`; the directory is not an input. Ignored otherwise. |

## `[decomposition]`

Controls MPI/domain decomposition.

| Variable | Fortran type | Meaning |
|---|---|---|
| `nxdomain` | `integer` | Number of domains in `x`. Current production inputs should use `1`. |
| `p_row` | `integer` | MPI process grid rows, usually aligned with `y`. Use `0` for automatic decomposition. |
| `p_col` | `integer` | MPI process grid columns, usually aligned with `z`. Use `0` for automatic decomposition. |

`p_row= 0, p_col= 0` is the recommended setting and the only one the test suite
exercises. 2decomp&FFT then chooses the process grid itself, and every regression,
functional and decomposition-independence check in `tests/` has been run that way.
Results should not depend on the process grid — that is a solver invariant — but a
hand-chosen `p_row`/`p_col` is outside the tested matrix, so verify such a run against
the automatic layout before trusting it for production.

## `[domain]`

Defines the physical case and domain extents.

| Variable | Fortran type | Meaning |
|---|---|---|
| `icase` | `integer` | Flow geometry/case ID. |
| `lxx` | `real` | Domain length in `x`. For channel, pipe, and annular cases this is usually streamwise length. |
| `lyt` | `real` | Upper `y`/radial boundary. Some cases reset this internally. |
| `lyb` | `real` | Lower `y`/radial boundary. Some cases reset this internally. |
| `lzz` | `real` | Domain length in `z`. For pipe/annular this is azimuthal length and is reset to `2π`. |

Case IDs:

| ID | Case |
|---:|---|
| 1 | Channel |
| 2 | Pipe |
| 3 | Annular |
| 4 | 3-D Taylor-Green vortex |
| 5 | Duct |

Notes:

- Pipe and annular cases use cylindrical coordinates internally.
- For pipe, `lyb`, `lyt`, and `lzz` are reset to `0`, `1`, and `2π`.
- For annular flow, `lyt` and `lzz` are reset to `1` and `2π`.
- For duct, `x` and `y` are wall-normal directions and `z` is streamwise.

## `[flow]`

Defines flow-field initialisation and Reynolds numbers.

| Variable | Fortran type | Meaning |
|---|---|---|
| `initfl` | `integer` | Flow-field initialisation method ID. |
| `irestartfrom` | `integer` | Restart iteration for flow when `initfl=0`; otherwise reset internally to `0`. |
| `veloinit` | `real(3)` | Constant initial velocity vector used when `initfl=4`. |
| `noiselevel` | `real` | Random perturbation amplitude added during initialisation where applicable. |
| `reni` | `real` | Initial Reynolds number used for ramping/scaling. |
| `nreni` | `integer` | Number of iterations over which the initial Reynolds setting is applied. |
| `ren` | `real` | Target Reynolds number for the run. |

Initialisation IDs:

| ID | Meaning |
|---:|---|
| 0 | Restart from saved fields |
| 2 | Random perturbation |
| 3 | Initialise from inlet data |
| 4 | Given constant values |
| 5 | Poiseuille profile |
| 6 | Analytic function, used for Taylor-Green vortex |
Common choices are `initfl=5` for periodic channel, pipe, and annular cases;
`initfl=3` for inlet/outlet cases; and `initfl=6` for Taylor-Green vortex.

## `[thermo]`

Defines thermal/energy-equation settings. Include this section when solving the
energy equation. Some templates include placeholder thermal values even for
isothermal cases because the input format is shared.

| Variable | Fortran type | Meaning |
|---|---|---|
| `ithermo` | `logical` | Enables thermal/energy equation. |
| `icht` | `logical` | Enables conjugate heat transfer mode. |
| `igravity` | `integer` | Gravity direction ID. |
| `ifluid` | `integer` | Working-fluid property model ID. |
| `ref_l0` | `real` | Dimensional reference length in metres. |
| `ref_t0` | `real` | Reference temperature in Kelvin. |
| `inittm` | `integer` | Thermal-field initialisation method ID: `0` restart, `4` constant, `6` analytic/function, `7` linear interpolation from boundary conditions, or `8` smooth interpolation from boundary conditions. `8` matches `7` when both y sides are Dirichlet, and is quadratic with zero gradient on the lower side otherwise — the regular shape for a pipe axis or an adiabatic wall. Both fall back to `4` for an inlet-outlet configuration, where the inlet plane prescribes the temperature instead. |
| `irestartfrom` | `integer` | Restart iteration for thermal field when `inittm=0`. |
| `tini` | `real` | Initial temperature in Kelvin. |
| `inout_buffer` | `real(2)` | Inlet and outlet thermal buffer lengths as `inlet,outlet`, scaled by `L0`. Over the inlet buffer the y-wall Dirichlet temperature is smoothstepped from the inlet value to the prescribed wall value, so the wall matches the inlet plane exactly at the corner and reaches the wall value with zero slope. This is a *gradually heated* entry, not an unheated one. |
| `qw_ramp` | `logical,integer,integer` | Heat-flux ramp as `enabled,start_iter,end_iter`: a half-cosine ramp of the wall heat flux from 0 at `start_iter` to its full value at `end_iter`, in run iterations (the flow's counter, also when `niterthermofirst > niterflowfirst`). |
| `rhoh_recovery` | `string`, optional | Recovery of the thermodynamic state from the advanced rho*h: `transported` (default) or `lagged_legacy` (deprecated, comparison only; restarts with imposed-heat-flux walls are not exact, see restart-io). |
| `property_interp` | `string`, optional | Property-table interpolation in enthalpy: `linear` (default; unchanged behaviour) or `pchip` (EXPERIMENTAL). See below. |
| `enthalpy_advection` | `string`, optional | Face value of h in the convective flux of rho*h: `cd2` (default; unchanged behaviour) or `koren` (EXPERIMENTAL). See below. |
| `enthalpy_advection_restart` | `string`, optional | Restart policy for a change of `enthalpy_advection`: `strict` (default; a thermal restart from a checkpoint advanced with the other scheme stops) or `allow_switch`. See below. |
| `thermal_outlet_pair` | `string`, optional | Thermal state at a convective x-outlet under `rhoh_recovery = transported`: `convective` (default; unchanged behaviour) or `zero_gradient` (EXPERIMENTAL). See below. |

### `property_interp` (experimental)

Table fluids (`scp_water`, `scp_co2`) look up properties from the enthalpy column
of the table. `linear` interpolates piecewise linearly: d(rho)/dh jumps at every
table node, and when many heated wall cells cross a node together the step in
the domain-integrated d(rho)/dt appears as a short pressure impulse.
`pchip` uses monotone piecewise-cubic Hermite interpolation (Fritsch-Butland
slopes) of T, rho, mu, k and beta in h: C1 in h, monotone, no overshoot between
nodes, and table values reproduced exactly at the nodes. In `pchip` mode
cp = 1/(dT/dh) of the interpolant (consistent with the energy equation; it agrees
with the tabulated cp as closely as the table's own h and cp columns agree), and
a lookup by temperature inverts the same T(h).

- Omit the key, or give `linear`, for the existing behaviour; results are
  unchanged bit for bit.
- `pchip` requires `rhoh_recovery = transported` and a table fluid; liquid-metal
  correlations reject it.
- Position: like every key, `property_interp` is read in order. Put it directly
  after `rhoh_recovery`; if that key is absent, after `qw_ramp`; if both are
  absent, after `inout_buffer`. A `property_interp` line anywhere else stops the
  run with a message.
- LES: cp enters only the subgrid Prandtl number. Verified with the water-table
  LES cases; not yet verified with the CO2 table.
- Restart: every thermal checkpoint is accompanied by
  `1_data/domain<d>_thermo_interp_meta_<iter>.dat`, which records the
  interpolation it was written with. Copy it together with the checkpoint (it
  carries the same iteration number). A thermal restart (`inittm` = restart)
  must use the same mode: linear to linear and pchip to pchip are accepted (and
  reproduce the continuous run bit for bit); linear to pchip and pchip to linear
  stop the run, because the stored density belongs to the other interpolation.
  A checkpoint without the record (written before `property_interp` existed)
  is continued with linear interpolation, as before, and is refused with pchip.
  To change the interpolation, start a fresh thermal field.
  Executables built before this record existed ignore it: they continue any
  checkpoint, including a pchip one, with linear interpolation and cannot detect
  the change of mode. Restart pchip checkpoints only with a current executable.
- Mesh mapping: the mapping utility does not check the interpolation record of
  its source checkpoint; its output checkpoints carry the record of the mapping
  run, not of the source. Use the same `property_interp` in the mapping run as
  in the run that wrote the source checkpoint (this is not enforced).
- Not yet established: performance at production scale (property evaluation is
  about 11 % slower than linear in serial; no difference was resolvable in a
  whole time step of a small case), and a port to the GPU branch (dev-gpu does
  not have `property_interp`).
- Smoother interpolation does not add information to a table of limited
  precision and is not, by itself, more accurate.

### `enthalpy_advection` (experimental)

The energy equation advances rho*h with the convective flux g h_f, where g is the
staggered face mass flux (the same one the continuity equation uses) and h_f the
specific enthalpy at the face. `cd2` takes the midpoint value of the scheme set by
`iaccuracy`. `koren` takes a Koren-limited MUSCL value, upwinded on the sign of g:

    h_f = h_U + 1/2 psi(r) (h_U - h_UU),   r = (h_D - h_U) / (h_U - h_UU)
    psi(r) = max(0, min(2r, (1 + 2r)/3, 2))

with U, UU the upwind and far-upwind cells and D the downwind cell. It is evaluated
without the division and without any tolerance. The face value always lies between
h_U and h_D (exactly, in floating point). Where the data are smooth and monotone the
face formula is the unlimited kappa = 1/3 value, (-h_UU + 5 h_U + 2 h_D)/6, the
third-order upwind-biased face formula on a uniform (computational) grid. At local
extrema and in steep gradients the limiter reduces it towards first-order upwind.

Accuracy of the complete discretisation: the third order belongs to the face formula
(and to constant-coefficient advection on a uniform grid). In CHAPSim2 the face value
multiplies a face mass flux, density and grid metric that vary in space, and the rest
of the solver (mass flux, diffusion, time coupling) is second order, so `koren` is
**not** a third-order solver. The tests support approximately second-order global
accuracy for the tested configurations (L1 order 2.3-2.4 for smooth advection on
uniform, tanh-stretched and cylindrical grids; L-infinity about 1.5, from the limiter
at extrema). No formal order is claimed for arbitrary stretched grids.

- Omit the key, or give `cd2`, for the existing behaviour; results are unchanged
  bit for bit.
- `koren` requires `iaccuracy = cd2`: only there is the divergence of the face flux a
  two-point difference, which the bounded-operator property needs. Other
  `iaccuracy` values stop the run with a message.
- It is independent of `property_interp`; any combination is allowed.
- Position: put it directly after `property_interp`. If that key is absent, put it
  where `property_interp` would go: after `rhoh_recovery`, else `qw_ramp`, else
  `inout_buffer`. A line anywhere else, or given twice, stops the run.
- Grids: the stencil is in cell index, like the `cd2` midpoint. The stretched y
  grid is a mapping y(s), with faces at integer s and centres at the mapped half
  points, so this is the uniform-grid scheme in s, with the metric applied in the
  existing derivative. On the tested smooth tanh mappings the observed accuracy matches
  the uniform grid (see the accuracy paragraph above); errors grow with stretching by a
  similar factor for `cd2` and `koren` (`tests/unit_enthalpy_advection`).
- Boundaries: periodic directions, interior/multi-domain ends and the pipe axis
  (whose halo holds the cells at theta + pi) use the full stencil. Wall and inlet
  faces keep their boundary value. Every documented thermal boundary type gives the
  enthalpy ghost cells: a heat-flux wall and Neumann, convective or database outlets
  are Dirichlet for the enthalpy (with a live, extrapolated or advected boundary
  state), so the full stencil is used at the face next to every boundary. The code
  also contains a first-order-upwind branch for an end type without ghost cells; no
  documented thermal boundary type reaches it (an undocumented boundary code passed
  through unchanged would).
  No extra MPI exchange is needed: each direction is reconstructed in the pencil
  that holds its whole lines.
- Boundedness: the face value is bounded, and so is the semi-discrete transport
  operator (local extrema do not grow) when the face mass flux satisfies discrete
  continuity. The complete time step is not guaranteed bounded, because the
  low-storage RK3 is not strong-stability-preserving. In the unit tests the RK3
  solution of sharp profiles stayed within its initial range at every stage for
  CFL <= 0.5. Overshoots appeared above CFL of about 0.55 to 0.75, and the scheme
  became unstable near CFL 1.4. In the solver the continuity residual and
  diffusion add further terms. No clipping is applied anywhere.
- Dissipation: the limiter adds numerical diffusion at extrema and in steep
  gradients. That removes the grid-scale oscillations of `cd2` but also smears
  sharp thermal layers. It is not a substitute for resolution. It does not make
  wall temperatures DNS-accurate on a mesh that is too coarse for the thermal
  boundary layer.
- Restart: each thermal checkpoint's record
  `1_data/domain<d>_thermo_interp_meta_<iter>.dat` states the scheme it was
  advanced with: a `koren` run writes the line `enthalpy_advection koren`; a
  `cd2` run writes nothing extra, so a checkpoint without the line (including
  every checkpoint written before the key existed) counts as `cd2`. On a thermal
  restart (`inittm` = restart):
  - same scheme as the checkpoint: continues normally;
  - different scheme: **stops** by default (`enthalpy_advection_restart = strict`).
    To change the scheme deliberately, e.g. to continue a `cd2` checkpoint with
    `koren` for a comparison, set `enthalpy_advection_restart = allow_switch`
    (directly after `enthalpy_advection`, or where it would go). The run then
    prints a prominent warning and every later thermal record carries
    `enthalpy_advection cd2|koren` and `enthalpy_advection_switch <from>-><to>
    <iteration>` (the latest permitted change; carried forward on later
    restarts);
  - a permitted change still stops if the statistics would continue across it
    (`stat_level` > 0, restart iteration > `stat_istart`, run clock continued):
    set `stat_istart` to the restart iteration or later, so that the averages
    start afresh with the new scheme;
  - an unrecognised fifth or sixth record line stops;
  - the `property_interp` rule is checked first and is not affected:
    `allow_switch` does not permit a change of property interpolation.
  For reproducible DNS keep `strict`: a run directory's input then fully states
  the scheme of every checkpoint it continues, and no averaging window mixes
  two schemes. The stored state does not depend on the face reconstruction, so a
  permitted switch is numerically admissible; the policy concerns provenance.
  Mesh mapping: the mapped checkpoints carry the record of the mapping run, not
  of the source; use the same `enthalpy_advection` in the mapping run. Executables
  built before these lines existed ignore them.
- Not yet established: production-scale cost and validation against reference
  data (e.g. the Nemati et al. heated-pipe case), and a port to the GPU branch.

Gravity IDs:

| ID | Direction |
|---:|---|
| 0 | No gravity |
| 1 | +x |
| -1 | -x |
| 2 | +y |
| -2 | -y |
| 3 | +z |
| -3 | -z |

Fluid IDs:

| ID | Fluid |
|---:|---|
| 1 | Supercritical water |
| 2 | Supercritical CO2 |
| 3 | Liquid sodium |
| 4 | Liquid lead |
| 5 | Liquid bismuth |
| 6 | Liquid LBE |
| 8 | Liquid lithium |
| 9 | Liquid FLiBe |
| 10 | Liquid PbLi eutectic |

ID 7 is reserved for ordinary liquid water, which has no property correlation in
the code; the solver rejects it. Use ID 1 (`scp_water`) and its NIST table
instead.

For IDs 3-10 the properties come from correlations evaluated over a single
temperature interval, printed in the log at startup. That interval is the liquid
range (melting to boiling) narrowed by the validity range of any correlation
that holds over less than it; the log names whichever property sets each end,
and `ref_t0` or `tini` outside the interval stops the run with that name in the
message.

PbLi is the only fluid currently narrowed this way. Its dynamic viscosity is the
Arrhenius expression measured by Jauch, Haase and Schulz, *Thermophysical
Properties in the System Li-Pb*, report KfK-4144 (Kernforschungszentrum
Karlsruhe, 1986), Part II section 4.3, which prints
`eta = 0.187 * exp(11640 / (R T))` mPa s, i.e.
`mu = 1.87e-4 * exp(11640 / (8.314 T))` Pa s. It is evaluated over
**521-625 K** only. That interval is an implementation policy, not an
established physical validity range: the report prints no range beside the
equation, the INL MOOSE implementation of the same expression restricts it to
melting point-625 K, the liquid-breeder compilation quoting it lists 521-900 K,
and 521-625 K is their overlap. The report's own section 5 discusses
extrapolating its properties to 1250 K, but that is a statement about the whole
property set rather than a viscosity bound, so it is not used to widen the
interval.

The PbLi heat capacity is the same report's `cp = 0.195 - 9.116e-6 T` J/(g K),
stated there for 508-800 K; that range is recorded but binds nothing, because
508 K is already the melting point and the viscosity cuts the top to 625 K. The
PbLi density and thermal conductivity fits used here do not match that report
and carry no other identified source range, so they do not narrow the interval
further.

### `thermal_outlet_pair` (experimental)

Placed directly after `enthalpy_advection_restart` (or where it would go); only for a
convective x-outlet with `rhoh_recovery = transported`.

- `convective` (default): the outlet face state (rho*h, rho_hat) is time-integrated by
  the convective outlet equation.
- `zero_gradient`: the face takes the last interior cell's (rho*h, rho_hat) pair, so the
  outflow enthalpy flux is the upwind `g_b h(nc)` and no separately integrated face state
  exists. The number of reversed (backflow) outlet faces is reported in the log.

Rejected for other outlet types, `lagged_legacy` and convective z-outlets.

## `[mhd]`

Defines magnetohydrodynamics settings. If MHD is disabled, the section may still
contain placeholder values.

| Variable | Fortran type | Meaning |
|---|---|---|
| `imhd` | `logical` | Enables MHD model. |
| `NStuart` | `logical,real` | Pair `enabled,value` for Stuart number. |
| `NHartmn` | `logical,real` | Pair `enabled,value` for Hartmann number. |
| `B_static` | `real(3)` | Static magnetic-field vector `Bx,By,Bz`. |

Exactly one of `NStuart` or `NHartmn` should be enabled for an MHD run.
`B_static` is always interpreted as a global Cartesian vector. For cylindrical
pipe and annular cases, the solver decomposes this vector into local radial and
azimuthal components internally.

The current MHD implementation assumes constant electrical conductivity in the
electric-potential solve. This is appropriate for isothermal cases and is used
as the present approximation for thermal MHD cases. For the liquid-metal
property relation currently under review, a 20% temperature increase of about
114 K gives an estimated electrical-conductivity decrease of about 6.9% through
the corresponding resistivity change. Temperature-dependent conductivity is
therefore a known future extension rather than part of the current MHD model.

## `[mesh]`

Defines grid resolution and wall-normal/radial stretching.
Use the [Mesh Stretching Reviewer](mesh-reviewer.md) to inspect the y-direction
mapping before running an expensive case.

| Variable | Fortran type | Meaning |
|---|---|---|
| `ncx` | `integer` | Number of cells in `x`. |
| `ncy` | `integer` | Number of cells in `y` or radial direction. |
| `ncz` | `integer` | Number of cells in `z` or azimuthal direction. For cylindrical cases, odd values are increased to the next even value. |
| `istret` | `integer` | Mesh stretching type ID. |
| `rstret` | `integer,real` | Stretching method and factor as `method,factor`. |
| `poisson_y_method` | `string` | Optional wall-normal Poisson method: `auto` (default), `fft`, or `tdma`. |

Stretching type IDs for `istret`:

| ID | Meaning |
|---:|---|
| 0 | No stretching |
| 1 | Centre clustering |
| 2 | Two-side clustering |
| 3 | Bottom-side clustering |
| 4 | Top-side clustering |

Stretching method IDs for the first value of `rstret`:

| ID | Meaning |
|---:|---|
| 0 | Uniform mesh; use with `istret=0` and factor `0.0` |
| 1 | Five-mode spectral stretching |
| 2 | Tanh stretching method |
| 3 | Power-law stretching method |

Five-mode spectral stretching uses a smooth analytic stretching function whose
spectral representation contains only five modes, `-2`, `-1`, `0`, `1`, and `2`.
This compact support is useful when the stretching effect is handled as a
convolution in the FFT spectral domain.

Recommended defaults are two-side clustering for channel and annular flow,
top-side clustering for pipe flow, and no stretching for Taylor-Green vortex.

`poisson_y_method=auto` preserves the validated solver selection: it uses the
y-skip TDMA path for non-periodic-y thermal cases and cylindrical cases, and
the full FFT path otherwise. Explicit `fft` is available for Cartesian cases
using a uniform mesh or the spectral stretching method; thermal inlet/outlet use is experimental.
Explicit `tdma` requires a non-periodic y direction.

## `[bc]`

Defines boundary conditions and periodic-flow driving.

Each boundary-condition line has four values:

```ini
ifbcx_u= bc_xlow,bc_xhigh,value_xlow,value_xhigh
```

The prefix gives the direction (`ifbcx`, `ifbcy`, `ifbcz`) and the suffix gives
the variable (`u`, `v`, `w`, `p`, `t`). The first BC/value pair belongs to the
lower/start boundary, and the second belongs to the upper/end boundary.

| Variable | Fortran type | Meaning |
|---|---|---|
| `ifbcx_u`, `ifbcx_v`, `ifbcx_w` | `integer,integer,real,real` | Velocity BCs on x boundaries. |
| `ifbcx_p` | `integer,integer,real,real` | Pressure BC on x boundaries. |
| `ifbcx_t` | `integer,integer,real,real` | Temperature BC on x boundaries. Temperature values are Kelvin for Dirichlet; heat flux values are W/m² before nondimensionalisation for Neumann. Each value field is a number or the token `tini`. For a Dirichlet side a number is an explicit temperature and is never replaced; `tini` means *not specified* and the initial temperature `tini` is used (resolved after all sections are read, independent of section order). For a Neumann side the value is a heat flux (`tini` is an error); for periodic/interior/convective sides the value is unused. An explicit x-inlet temperature different from `tini` is kept with a warning (the uniform initial field then does not match the inlet plane). Every Dirichlet boundary temperature must lie inside the property range (table rows or correlation interval, endpoints included); otherwise - including 0 K or negative values - the run stops before the simulation with the boundary named. |
| `ifbcy_u`, `ifbcy_v`, `ifbcy_w` | `integer,integer,real,real` | Velocity BCs on y/radial boundaries. |
| `ifbcy_p` | `integer,integer,real,real` | Pressure BC on y/radial boundaries. |
| `ifbcy_t` | `integer,integer,real,real` | Temperature BC on y/radial boundaries. |
| `ifbcz_u`, `ifbcz_v`, `ifbcz_w` | `integer,integer,real,real` | Velocity BCs on z/azimuthal boundaries. |
| `ifbcz_p` | `integer,integer,real,real` | Pressure BC on z/azimuthal boundaries. |
| `ifbcz_t` | `integer,integer,real,real` | Temperature BC on z/azimuthal boundaries. |
| `idriven` | `integer` | Flow-driving method ID. |
| `drivenfc` | `real` | Magnitude for wall-shear or pressure-gradient driving. Mass-flux driving normally uses `0.0`. |
| `outlet_mass_correction` | `string`, optional | Profile of the per-stage global mass correction at a convective x-outlet: `proportional` (default; unchanged behaviour), `uniform_flux` or `split` (both EXPERIMENTAL). Directly after `drivenfc`. See below. |

### `outlet_mass_correction` (experimental)

At every RK stage a convective x-outlet closes the global balance
`int drho/dt dV + mdot_out - mdot_in = 0` exactly by correcting the outlet face mass
flux with the stage residual `R`. The key only changes how `R` is distributed over the
outlet plane:

- `proportional` (default): the face mass flux is scaled by `s = 1 + R/mdot_out`.
- `uniform_flux`: the same increment `R/A_out` is added to every outlet face.
- `split`: the slow part `S_k = alpha_k * sum(R over the previous step)` is applied
  proportionally, and the stage-periodic remainder `R_k - S_k` uniformly. Its single
  state value is written to `1_data/domain<id>_outlet_split_<iter>.dat` at each
  checkpoint and read on restart. A checkpoint without that file (older runs,
  mesh-mapped restarts) starts with `S = 0` for one step.

`split` was introduced for heated variable-property inlet/outlet runs, where the
proportional profile admits the uniform pressure-projection increment unevenly and
drove a drift of the stored density in the outlet cells. It is EXPERIMENTAL: it has
been verified for the supercritical CO2 pipe (Nemati/Bae case B) configuration only.

Boundary condition IDs:

| ID | Meaning | Typical use |
|---:|---|---|
| 0 | Interior | Pipe axis or internal boundary |
| 1 | Periodic | Periodic directions |
| 2 | Symmetric | Symmetry plane |
| 3 | Antisymmetric | Antisymmetry plane |
| 4 | Dirichlet | Fixed value |
| 5 | Neumann | Fixed gradient or heat flux |
| 6 | Interpolation | Internal/interpolation use |
| 7 | Convective outlet | Open outlet, only supported in `x` or `z` |
| 9 | Profile inlet | Not supported (rejected at input): the 1-D profile inlet is unfinished |
| 10 | Database inlet | Inlet from stored plane data |
| 11 | Poiseuille | Nominal Poiseuille BC |
| 12 | Other/interpolation | Special cases |

Flow driving IDs for `idriven`:

| ID | Meaning |
|---:|---|
| 0 | No forcing |
| 1 | Constant streamwise mass flux in `x` |
| 2 | Constant wall shear in `x` |
| 3 | Constant pressure gradient in `x` |
| 4 | Constant streamwise mass flux in `z` |
| 5 | Constant wall shear in `z` |
| 6 | Constant pressure gradient in `z` |

Important constraints:

- Boundary rows are given for five variables in this order: `u`, `v`, `w`, `p`,
  and `T`. Even isothermal runs still include the `T` rows.
- Use driving only for periodic wall-bounded cases. In open inlet/outlet cases,
  the solver disables flow driving.
- Convective outlet in `y` is not supported.
- For pipe, the lower radial boundary is treated internally as the axis/interior.
- If any side of a variable is periodic, both sides for that variable are made periodic.
- For database inlet in `x`, the solver applies database treatment to all velocity
  components and Neumann treatment to pressure.

## `[scheme]`

Defines time integration and spatial discretisation.

| Variable | Fortran type | Meaning |
|---|---|---|
| `dt` | `real` | Time-step size. |
| `itimescheme` | `integer` | Time integration method ID. |
| `iaccuracy` | `integer` | Spatial derivative accuracy ID. |
| `iviscous` | `integer` | Viscous-term treatment ID. |
| `out_sponge_l_re` | `real(2)` | Outlet sponge layer as `length,Re_strength`. Use nonzero length for open outlet cases if needed. |

Time scheme IDs for `itimescheme`:

| ID | Meaning |
|---:|---|
| 0 | Euler |
| 1 | Adams-Bashforth 2 |
| 2 | RK3-Crank-Nicolson |
| 3 | RK3 |

Spatial accuracy IDs for `iaccuracy`:

| ID | Meaning |
|---:|---|
| 1 | 2nd-order central difference |
| 2 | 4th-order central difference |
| 3 | 4th-order compact |
| 4 | 6th-order compact |

Viscous treatment IDs for `iviscous`:

| ID | Meaning |
|---:|---|
| 1 | Explicit viscous treatment |
| 2 | Semi-implicit viscous treatment |

For cylindrical coordinates, the solver forces `iaccuracy` to 2nd-order central
difference. For channel cases, compact schemes are reduced to 4th-order central
difference.

## `[simcontrol]`

Defines iteration ranges.

| Variable | Fortran type | Meaning |
|---|---|---|
| `niterflowfirst` | `integer` | First flow iteration to run. |
| `niterflowlast` | `integer` | Last flow iteration to run. |
| `niterthermofirst` | `integer` | First thermal iteration; use `0` when thermal is disabled. |
| `niterthermolast` | `integer` | Last thermal iteration; use `0` when thermal is disabled. |
| `restart_clock` | `string` | Optional. How a restart maps onto the run timeline: `continue` (default) or `reset`. |

All four `niter*` values are **absolute iteration numbers on the run clock**, not
offsets from the restart point.

The environment variable `CHAPSIM_NITER` can override the final iteration for
short smoke tests.

### `restart_clock`

A restart carries two distinct numbers that are easy to confuse. `irestartfrom`
in `[flow]` and `[thermo]` selects **which checkpoint file to read**;
`restart_clock` selects **where the run then sits on the timeline**. They are
the same number under `continue` and deliberately differ under `reset`.

| Value | Run starts at | Stored RHS history | Stored statistics |
|---|---|---|---|
| `continue` (default) | the checkpoint iteration and its time | kept | read, and accumulation continues |
| `reset` | iteration `0`, time `0` | discarded | not read; accumulators start empty |

Use `continue` to extend a simulation — this is the normal case and reproduces
the behaviour of a run that was never interrupted. Use `reset` to treat a stored
field as an initial condition for a new experiment, for example when restarting
from a developed field under a changed Reynolds number, wall temperature, or
body force.

The clock is a property of the **run**, not of one field, so it is set once and
both the flow and the thermal field adopt it. This matters in the common case of
restarting the flow from a developed field while starting the thermal field
fresh:

```ini
[flow]
initfl= restart
irestartfrom= 2000
[thermo]
inittm= const
[simcontrol]
niterflowfirst= 2001
niterthermofirst= 2001
niterflowlast= 5000
niterthermolast= 5000
```

Under `continue` the run clock is 2000, the fresh thermal field is injected at
that same iteration, and both fields advance together from 2001. The flow and
thermal output files therefore carry matching iteration numbers.

Two constraints follow from there being one clock:

- If **both** fields restart, they must restart from the **same**
  `irestartfrom`. A mismatch is rejected at input-parsing time rather than
  producing a run in which one field silently idles while the other catches up.
- Under `reset` the run starts at iteration 0, so `niterflowfirst` and
  `niterthermofirst` must be set to `1`, not to `irestartfrom + 1`. Every other
  iteration-numbered input — `stat_istart`, `ndbstart`, `ndbend`, `initReTo` and
  the wall-heat-flux ramp bounds — is likewise read on the new clock. The solver
  warns if a field's last iteration is at or before the iteration the run starts
  from, since that field would never be advanced.

## `[io]`

Controls monitor, restart, visualisation, statistics, and plane-database I/O.

| Variable | Fortran type | Meaning |
|---|---|---|
| `cpu_nfre` | `integer` | Frequency for CPU/progress output. |
| `ckpt_nfre` | `integer` | Checkpoint/restart write frequency. |
| `visu_idim` | `integer` | Visualisation mode ID. |
| `visu_nfre` | `integer` | Visualisation output frequency. |
| `visu_nskip` | `integer(3)` | Cell skip for visualisation output in `x,y,z`. |
| `stat_istart` | `integer` | Iteration at which statistics begin. |
| `stat_level` | `integer` | Statistics level. |
| `stat_nskip` | `integer(3)` | Optional. Reserved: a statistics cell skip is not implemented. Statistics are accumulated on every cell; a value other than `1,1,1` is ignored with a warning. |
| `stat_visu_nfre` | `integer` | Optional visualised-statistics output frequency. Defaults to `visu_nfre` when absent. |
| `stat_visu_mode` | `string` | Optional visualised-statistics mode: `all` or `tsp_only`. Defaults to `all`. |
| `is_wrt_read_bc` | `logical,logical` | Pair `write_outlet,read_inlet` for plane database files. |
| `wrt_read_nfre` | `integer(3)` | Plane database frequency and range as `frequency,start,end`. |
| `ndb_sampling_dt` | `real` | Optional, directly after `ndb_file_offset`: physical time between two stored inlet-database planes. Absent: one stored plane per solver step (unchanged behaviour). Given: `ndb_sampling_dt/dt` must be an integer `R >= 1` and equal the `dt` recorded in the bundle metadata; for `R > 1` the inlet is interpolated linearly in time between samples. Requires a constant `dt`. |
| `existing_output_policy` | `string` | Action when an output file already exists: `overwrite` (default), `skip`, or `rename_existing`. |
| `restart_data_layout` | `string` | Backward-compatible alias that sets both restart input and output layouts: `per_field` (default) or `bundled`. |
| `restart_data_layout_read` | `string` | Layout used when reading existing restart and inlet-database files. Overrides `restart_data_layout` for input only. |
| `restart_data_layout_write` | `string` | Layout used when writing future restart, outlet-database, statistics, and visualisation bundle-capable outputs. Overrides `restart_data_layout` for output only. |
| `restart_history_mode` | `string` | Restart history policy: `exact` (default) or `compact`. `exact` stores/reads RHS history and derived thermal properties; `compact` reduces restart size and rebuilds history on the first restarted step. `compact` is isothermal only and is rejected for thermal runs. |
| `reset_unit_massflux` | `logical` | Optional restart restore control. Default `false`; when `true`, rescale the restored streamwise velocity component so its volume-average bulk value is 1.0. |

Visualisation mode IDs for `visu_idim`:

| ID | Meaning |
|---:|---|
| 0 | 3-D only |
| 1 | 2-D planes only |
| 2 | Both 3-D and 2-D outputs |

Statistics levels for `stat_level`:

| ID | Meaning |
|---:|---|
| 0 | No statistics |
| 1 | Mean/first moments |
| 2 | Second moments |
| 3 | Extended/turbulent budget statistics where supported |

Visualised-statistics modes for `stat_visu_mode`:

| Mode | Meaning |
|---|---|
| `all` | Write both `t_avg_*` and `tsp_avg_*` visualisation/post-processing statistics. |
| `tsp_only` | Write only `tsp_avg_*` visualisation/post-processing statistics. This requires at least one periodic direction. Checkpoint/restart `t_avg_*` statistics are still written with `ckpt_nfre`. |

Existing-output policies:

| Value | Meaning |
|---|---|
| `overwrite` | Overwrite existing files |
| `skip` | Skip writing when the target file exists |
| `rename_existing` | Rename the existing file before writing |

Restart-data layouts:

| Value | Meaning |
|---|---|
| `per_field` | Store one restart file per field |
| `bundled` | Store one restart bundle per field group |

Restart-history modes:

| Value | Meaning |
|---|---|
| `exact` | Store and read restart history needed for continuous-run restart equivalence. This is the default. |
| `compact` | Store only primary restart fields plus required boundary state. RHS history is rebuilt with startup handling after restart, so exact continuous-run equivalence is not expected. |

For the complete restart file lists under isothermal, thermal,
bundled/per-field, and convective-outlet conditions, see
[Restart I/O Modes](restart-io.md).

For database inlet cases, set `is_wrt_read_bc= .false.,.true.` and make sure
the requested inlet database files exist.

To run with a smaller time step than the one the inlet database was recorded
with, give `ndb_sampling_dt` (the recording interval). Database time is the
run's physical time: the step from `t` to `t + dt` uses the inflow at `t`,
sampled planes are used unchanged at their sample times, and linear
interpolation in time is used between them. The cyclic replay does not
interpolate across its wrap from the last to the first plane (the last plane is
held for its interval, then the inflow jumps to the first plane, as without the
key).

## `[probe]`

Defines point probes for monitor output.

| Variable | Fortran type | Meaning |
|---|---|---|
| `npp` | `integer` | Number of probe points. |
| `pt1`, `pt2`, ... | `real(3)` | Probe coordinates as `x,y,z`. |

Probe coordinates use the same nondimensional coordinate system as the domain.

## Practical Advice

### Recommended Setup Workflow

1. Select the closest existing case under `tests/`.
2. Generate or copy an input file.
3. Check `[domain]`, `[mesh]`, `[flow]`, and `[bc]` first; these define the core
   physics and numerics.
4. Enable `[thermo]` or `[mhd]` only when the case needs those physics.
5. Keep the first run short by reducing `niterflowlast`, or by using the
   `CHAPSIM_NITER` environment override.
6. Increase output frequencies only after the run is stable.

### Case Setup Checks

- Keep `nxdomain= 1` unless the solver is extended to support multiple x-domains.
- Use `p_row= 0` and `p_col= 0`. This is the recommended and the only tested setting;
  a hand-chosen process grid is untested and should be checked against it first.
- For periodic channel, pipe, and annular cases, use periodic streamwise BCs and
  a driving method such as `idriven=1`.
- For open inlet/outlet cases, use `initfl=3`, `idriven=0`, and database reading
  when `ifbcx_u` or `ifbcz_u` uses BC `10`.
- For thermal wall-heating cases, confirm the sign convention of Neumann heat
  flux before launching a long production run.
- For pipe and annular cases, remember that cylindrical-coordinate constraints
  can override some mesh and accuracy choices.

### Runtime Monitoring

During early runs, watch:

- Input-reading messages for unexpected case, mesh, BC, or scheme overrides.
- CFL and time-step diagnostics.
- Mass-conservation checks.
- Wall quantities and driven-flow response.
- Restart, visualisation, statistics, and plane-database output frequencies.

Treat warnings during input reading as setup feedback. They often indicate that
the solver has corrected an unsupported or inconsistent option.
