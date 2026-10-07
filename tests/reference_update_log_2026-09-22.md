# Reference update 2026-09-22 — the three `*_scp_inout_Tw` cases

Closes the three cases held back on 2026-09-21. They are red because their
`global pressure drop` was O(1e5); this records why, and what changed.

## What was wrong

Two separate inconsistencies in the *initial and boundary data*, not in the
solver. Both were transmitted into the pressure field by `drhodt`, which is
working exactly as designed.

### 1. `inittm = linear` in an inlet-outlet run (dominant, ~1600x)

`INIT_GVBCLN` fills the interior with a y-linear profile between the two wall
temperatures. That is the right starting field for a streamwise-**periodic**
channel or annulus, where nothing else prescribes the temperature. In an
inlet-outlet run the inlet plane prescribes it, and the two disagree: the
interior runs 640.15 -> 652.15 across y while the inlet is a uniform 645.15.
The first thermal substep therefore sees a density step across the *whole*
inlet plane, and

    drhodt = (dDens - dDens0) / (tAlpha * dt)      eq_energy.f90:73

divides it by `dt = 1e-5` and puts the result into the Poisson right-hand side.

The signature is diagnostic. On `channel_scp_inout_Tw` the pressure drop
**steps** to 1.723e5 at the first thermal substep (t = 0.21e-3) and then stays
flat to 7 digits for the remaining 40 steps. A one-off bad initial condition,
not an instability:

```text
inittm = linear                     inittm = const
 t=0.21e-3  dP= 5.98211e+04          t=0.21e-3  dP= 5.02476e-02
 t=0.31e-3  dP= 1.72314e+05          t=0.31e-3  dP=-2.75960e+01
 t=0.41e-3  dP= 1.72308e+05          t=0.41e-3  dP=-5.52001e+01
 t=0.51e-3  dP= 1.72302e+05          t=0.51e-3  dP=-8.27541e+01
 t=0.60e-3  dP= 1.72304e+05          t=0.60e-3  dP=-1.07514e+02
```

The `const` column grows smoothly from zero: the ordinary transient of an
impulsively started Dirichlet wall, which has an unbounded initial heat flux.

The pipe was never affected, because `input_general.f90:2120` already downgrades
it to `INIT_GVCONST` (one wall, so there is no pair to interpolate between).
That is why its dP was -7.7e2 rather than 4e5.

### 2. `inout_buffer` was a no-op for a Tw wall (secondary, ~2.9x)

`bc_dirichlet.f90` filled the buffer region with `fbcy_const(n,5)` for a
Dirichlet wall, i.e. the same wall temperature as everywhere else. Only a
Neumann wall got a real unheated length (`ZERO` flux). So an unheated entry
length could not be expressed for a Tw case at all, and the inlet corner cell
carried a temperature jump. `input_general.f90:1719` warned against setting a
buffer in this configuration, which is why all three cases shipped with
`inout_buffer = 0.0, 0.0`.

## What changed

| file | change |
|---|---|
| `src/bc_dirichlet.f90` | Dirichlet buffer now blends the wall from the inlet temperature to `Tw` with the smoothstep `3s^2 - 2s^3` (C1 at both ends). Neumann branch split out and left as a step. |
| `src/input_general.f90` | `INIT_GVBCLN` downgraded to `INIT_GVCONST` when the streamwise direction is not periodic, with a warning. Stale "buffer not recommanded" warning removed. Effective initialisation type now reported after the downgrades. |
| three `*_scp_inout_Tw/input_chapsim.ini` | `inittm = const`; `inout_buffer = 2.0, 0.0` (2R for the pipe, 2x half-height for the channel, 2x outer radius for the annulus; uniformly 25% of `lxx = 8.0`). |

## Effect on `global pressure drop`

| case | before | + buffer ramp | + `const` init |
|---|---|---|---|
| `channel_scp_inout_Tw` |  4.9341e+05 |  1.7230e+05 | -1.0751e+02 |
| `annular_scp_inout_Tw` |  4.1910e+05 |  1.5030e+05 | -1.5007e+03 |
| `pipe_scp_inout_Tw`    | -7.7168e+02 | -5.4213e+02 | -5.4213e+02 |

The pipe moves only with the buffer, as expected — it was already `GVCONST`.

## What this says about `drho/dt`

Zeroing `drho/dt` in the Poisson right-hand side also makes the pressure drop
reasonable, but for the wrong reason: it suppresses the symptom of an
inconsistent initial condition. With the initial condition made consistent, the
pressure drop is reasonable **with `drho/dt` intact**. No change to the Poisson
solver is warranted: the constant-coefficient FFT solver with a `drho/dt`
correction is a deliberate design choice, and the `(0,0)` Fourier-mode
compatibility condition it has to satisfy is global mass conservation.

## Verification

- Tier 0: clean build, no new warnings.
- Intermediate run with the code change but `inout_buffer = 0.0`: all 20 cases
  bit-identical (`abs = 0.00e+00` on every check), confirming the Neumann path
  was untouched and the new Dirichlet branch is inert at zero buffer.
- The three `*_scp_inout_qw` cases stayed bit-identical throughout.
- Guard checked directly: a scratch copy with `inittm = linear` restored prints
  the warning, reports `Initialised from given values` as the effective type,
  and reproduces the `const` result exactly (dP = -1.0751e+02).
- The four `*_scp_periodic_Tw`/`_qw` cases still report `GVBCLN` as effective,
  so the periodic use of `inittm = linear` is unchanged.

## Still open

`inout_buffer(2)` (the outlet buffer) for a Dirichlet wall is implemented as the
mirrored ramp, but for a Tw wall that is a *cooling* section, not an unheated
one — the fluid arrives hot. It is commented as such. All shipped cases use
`0.0` there.
