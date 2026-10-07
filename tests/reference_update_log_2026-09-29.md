# Reference update 2026-09-29 — LES filter width from the local physical cell volume

One baseline moves: `tests/functional/LES_channel_iso_peridic`. This records why,
by how much, and why the other LES case was left alone.

Commit `e49c987` forward-references this file as
`tests/functional/reference_update_log_2026-09-29.md`; it lives here instead,
alongside the 2026-08-13 / 2026-09-21 / 2026-09-22 logs.

## What changed in the solver

`calculate_eddy_viscosity_wale` (`src/eq_les.f90`) used

```fortran
delta = (dm%h(1) * dm%h(2) * dm%h(3))**(1.0_WP/3.0_WP)
```

a single scalar built from the **computational** spacings. `dm%h` is the uniform
step in the mapped coordinate, not a length: on a stretched mesh `h(2)` is not
the cell height, and in cylindrical coordinates `h(3)` is an angle, not an arc.
So the WALE length scale was wrong everywhere except on a uniform Cartesian mesh.

It now uses the cube root of the local **physical** cell volume, the same measure
`geometry.f90:625-630` already uses to build `dm%vol`:

```fortran
dy = dm%h(2) / dm%yMappingcc(jj, 1)   ! when is_stretching(2)
dz = dm%h(3) * dm%rc(jj)              ! when icoordinate == ICYLINDRICAL
delta(j) = (dm%h(1) * dy * dz)**(1.0_WP/3.0_WP)
```

`jj = dm%dccc%xst(2) + j - 1` is the global radial index, so the value is
decomposition-independent.

Since `nu_t ~ delta**2`, a ratio `r = delta_new/delta_old` scales the eddy
viscosity by `r**2`.

### `LES_channel_iso_peridic` — 64x80x64, `lyy = [-1,1]`, `istret= twosides`, `rstret= 3fmd,0.10`

`h = (0.125, 0.025, 0.0625)`, so `delta_old = 0.05802` uniformly.
Physical `dy` read from the case's own `4_check/check_mesh_mapping.dat`.

| j | dy | delta_new | delta_new/delta_old | nu_t factor |
|---|---|---|---|---|
| 2 (wall) | 0.007225 | 0.038360 | 0.661 | **0.437** |
| 5 | 0.007412 | 0.038688 | 0.667 | 0.445 |
| 10 | 0.008201 | 0.040015 | 0.690 | 0.476 |
| 20 | 0.012873 | 0.046503 | 0.802 | 0.642 |
| 40 (centre) | 0.086412 | 0.087724 | 1.512 | **2.286** |
| 60 | 0.013758 | 0.047546 | 0.819 | 0.672 |
| 70 | 0.008446 | 0.040409 | 0.696 | 0.485 |
| 79 (wall) | 0.007225 | 0.038360 | 0.661 | 0.437 |

The physical cell height spans a factor of 12 across the channel; the old scalar
`delta` split the difference, over-damping the near-wall region by ~2.3x and
under-damping the core by ~2.3x. The new profile is the physically intended one:
a smaller filter where the mesh is finer.

### `LES_pipe_iso_periodic` — 80x48x64, `lyy = [0,1]`, `istret= top`, `rstret= tanh,0.1`

`h = (0.1, 0.020833, 0.098175)`, `delta_old = 0.058919`. Here **both**
corrections apply: the radial stretching and the `r*dtheta` arc length.

| j | r_c | dr | r*dtheta | delta_new | ratio | nu_t factor | cell AR (dx / r*dtheta) |
|---|---|---|---|---|---|---|---|
| 1 (axis) | 0.02161 | 0.04320 | 0.00212 | 0.02090 | 0.355 | **0.126** | 47 |
| 2 | 0.06475 | 0.04305 | 0.00636 | 0.03014 | 0.511 | 0.262 | 16 |
| 5 | 0.19225 | 0.04174 | 0.01887 | 0.04287 | 0.728 | 0.529 | 5.3 |
| 10 | 0.39042 | 0.03710 | 0.03833 | 0.05220 | 0.886 | 0.785 | 2.6 |
| 24 (mid) | 0.78079 | 0.01873 | 0.07665 | 0.05237 | 0.889 | 0.790 | 1.3 |
| 36 | 0.93494 | 0.00811 | 0.09179 | 0.04207 | 0.714 | 0.510 | 1.1 |
| 48 (wall) | 0.99844 | 0.00318 | 0.09802 | 0.03147 | 0.534 | 0.285 | 1.0 |

`nu_t` was overestimated by ~8x in the first cell off the axis and ~3.5x at the
wall. No baseline moves here — the case is new.

**Known limitation, not fixed.** `delta` is an isotropic measure. At `j = 1` the
cell aspect ratio is 47:1, where an anisotropy correction (e.g. Scotti et al.
1993) would be more appropriate than the cube root of the volume. Recorded in
the source comment; out of scope.

## Baseline moved: `LES_channel_iso_peridic`

Full 32-case run, `CI=true REGRESSION_SUITE=extended INCLUDE_FUNCTIONAL_TESTS=y`,
4 MPI ranks, 60 steps.

| metric | old reference | new reference | rel | tolerance |
|---|---|---|---|---|
| total kinetic energy | 6.07982195e-01 | 6.07483025e-01 | 8.21e-04 | 1.00e-03 |
| bulk velocity uz | 2.93959942e-04 | 2.93959973e-04 | 1.05e-07 | 1.00e-02 |
| bulk velocity ux | 1.0 | 1.0 | 0 | pinned by constant-massflux forcing |

The case **still passed** at the old reference — KE landed at 82% of the relative
tolerance budget. The baseline is refreshed anyway so that future drift is
measured against the corrected physics rather than against a value the change was
already known to invalidate.

Everything else in the file is roundoff-level motion in metrics that are at
machine zero and stay there, confirming the projection is untouched:

| metric | old | new |
|---|---|---|
| max. mass conservation (interior) | 4.44e-15 | 5.33e-15 |
| physical Poisson compatibility defect | 9.04e-20 | 9.16e-19 |
| Poisson zero-mode projection (scaled solver RHS) | 2.71e-16 | 2.75e-15 |
| mean dpdx | 5.50e-21 | 1.35e-20 |

## `LES_channel_scp_inout_Tw` — diagnosis (resolved in the addendum below)

This case was expected to move too. It barely did — the filter-width change
shifts its kinetic energy by 1.9e-06 (6.104558e-01 -> 6.104539e-01) and its bulk
enthalpy by 1.5e-14. Over 40 steps from a laminar-plus-noise start, its SGS
contribution is negligible.

It **fails**, but for a reason that predates this work and is unrelated to LES:

```text
global pressure drop   new=-1.547167e+02  ref=-9.395868e+03   rel 9.84e-01
mean dpdx              new= 2.417449e+00  ref= 1.468104e+02   rel 9.84e-01
total kinetic energy   new= 6.104539e-01  ref= 6.229343e-01   rel 2.00e-02
bulk velocity ux       new= 1.002513e+00  ref= 1.032217e+00   rel 2.88e-02
```

The identical failure, digit for digit in the reference column and to 6 digits in
the new column, is present in the 2026-09-25 run made before any of this work.
`MHD_channel_scp_inout_Tw` — which contains no LES at all — fails the same way.

Root cause, from `reference_update_log_2026-09-22.md`: that update fixed
`inittm= linear -> const` and `inout_buffer= 0.0,0.0 -> 2.0,0.0` in
`tests/regression/channel_scp_inout_Tw`, but the two functional siblings still
carry the old settings:

```text
tests/regression/channel_scp_inout_Tw       inittm= const    inout_buffer= 2.0, 0.0
tests/functional/LES_channel_scp_inout_Tw   inittm= linear   inout_buffer= 0.0, 0.0
tests/functional/MHD_channel_scp_inout_Tw   inittm= linear   inout_buffer= 0.0, 0.0
```

Refreshing their references against *these* inputs would bless an initial
condition the project has already diagnosed as wrong. The fix is to apply the
same input change to both functional cases and then regenerate — done, see the
addendum at the end of this file.

## Also in this update

`tests/regression/run_regression.sh:117-118` assigned `REGRESSION_SUITE` and
`INCLUDE_FUNCTIONAL` unconditionally, clobbering any exported value before the
`CI=true` branch below could read it back with `${REGRESSION_SUITE:-...}`. Line
116 already did it correctly for `TEST_MODE`. A non-interactive
`REGRESSION_SUITE=extended` therefore silently ran the 12-case standard suite.
Fixed to match line 116.

## Addendum — the two `*_scp_inout_Tw` functional cases, fixed

Resolved rather than left open. Both functional cases now carry the same input
the 2026-09-22 update gave their regression sibling:

```text
inittm=       linear    -> const
inout_buffer= 0.0, 0.0  -> 2.0, 0.0
```

Apart from these two settings and their own `[les]` / `[mhd]` section, both
files are character-for-character identical to
`tests/regression/channel_scp_inout_Tw/input_chapsim.ini`, so this closes the
divergence rather than creating a new configuration.

References regenerated. The pathological pressure drop is gone, and the LES case
now agrees with its regression sibling to every printed digit on the metrics LES
should not affect:

| metric | old reference | new reference | regression sibling |
|---|---|---|---|
| global pressure drop | -9.395868e+03 | **-1.075136e+02** | **-1.075136e+02** |
| mean dpdx | 1.468104e+02 | **1.679900e+00** | **1.679900e+00** |
| bulk velocity ux | 1.032217e+00 | **1.001814e+00** | **1.001814e+00** |
| bulk temperature | 1.000015e+00 | **1.000000e+00** | **1.000000e+00** |
| total kinetic energy | 6.229343e-01 | 6.097462e-01 | 6.097493e-01 |
| bulk enthalpy | 1.493156e-05 | 2.673117e-10 | 2.672864e-10 |

The only two that differ are exactly the two the SGS viscosity should touch, and
they differ by 5e-06 relative.

`MHD_channel_scp_inout_Tw` likewise agrees on `bulk velocity ux`, `bulk massflux
gx` and `bulk temperature`, and differs on pressure drop (1.871823e+01 vs
-1.075136e+02) — that is the Hartmann drag of the `Ha = 10` wall-normal field,
which is the point of the case.

Suite is now **32/32 green**.
