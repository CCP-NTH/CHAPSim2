# Reference Baseline Refresh - 2026-09-21

Partial, deliberate refresh of CHAPSim2 regression reference data. Unlike the
2026-08-13 refresh, this one is **not** a blanket copy: one case was held back and
two of the failing metrics turned out to be defects rather than stale values.

## Starting point

After commits `18f1d6a` and `ebf6702` (cylindrical axis ghosts, cylindrical high
order) the 12-case regression stood at:

```text
MODE   : run
PASSED :   5 /  12
FAILED :   7 /  12
  SOLVER :   0
  METRICS:   7
```

All 7 failures were supercritical (thermal) cases. Every solver execution
completed; none produced NaN or Inf. The failures were **not** uniformly stale
references, so each failing metric was traced back to its definition in `src/`
before anything was overwritten.

## Diagnosis of the 7 failures

The failing metrics fell into four groups.

### 1. Broken diagnostic - fixed in code, not refreshed

`src/io_monitor.f90`, "Mass-flux-weighted enthalpy":

```fortran
call Get_x_1der_C2C_3D(fl%gx, accc1, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
accc2 = accc1 * tm%hEnth
call Get_volumetric_average_3d(dm, dm%dccc, accc2, bulk_h, SPACE_AVERAGE, 'h')
ftp_bulk%h = bulk_h / bulk_g(1)
```

Two defects:

- A bulk enthalpy is `<gx*h> / <gx>`. This formed `<d(gx)/dx * h> / <gx>` - the
  x-derivative of the mass flux, not the mass flux. It also used a **C2C**
  stencil on `fl%gx`, which lives on `dpcc` (P in x), so the wrong quantity was
  also evaluated at the wrong points. Corrected to `Get_x_midp_P2C_3D` with
  `dm%fbcx_gx`.
- The division by `bulk_g(1)` was unguarded. In a zero-net-flow case (`tgv_scp`)
  `<gx>` sits at round-off, ~1e-19, so bulk enthalpy came out **-6.5e+14** and
  bulk temperature **-3.3e+15**, and that value was then pushed through the
  thermal property table. Now uses `safe_divide`, which returns zero below
  `MINP`.

The stored reference was equally meaningless (7.0e+13 / 3.5e+14), so this metric
had never worked. Refreshing it would have cemented garbage.

Effect of the fix, visible across the suite: `bulk temperature` used to read
exactly `1.000000` on nearly every thermal case, because the volume average of a
streamwise derivative is ~0 in a developed flow. It now tracks the heating -
1.001989 in `channel_scp_periodic_Tw`, 1.006114 in `annular_scp_inout_Tw`.

This is a monitor/diagnostic path only. It does not enter the solution, and the
regression confirms it: comparing the runs before and after the fix, the only
metrics that moved anywhere in the suite were `bulk enthalpy`, `bulk temperature`
and the tolerance display for `global mass balance`. Velocities, kinetic energy,
mass conservation and pressure were unchanged.

### 2. Unreachable tolerance - tolerance changed, not the value

`global mass balance` is `fl%tt_mass_change`, the volume integral of `drho/dt`. In
a **periodic thermal** case there is no boundary to carry mass in or out, so as
the fluid heats this integral is genuinely non-zero - 0.282 in
`pipe_scp_periodic_Tw`. The global tolerance asked for `abs 1e-6`, which cannot
be met by construction, and refreshing the number would not have helped because
`rel 1e-5` would fail on the next solver change.

`tests/tools/check_metrics.py` gained support for per-case tolerance overrides,
keyed by case directory name and read from a new `"cases"` block in
`tests/tools/tolerances.json`. The case name is derived from the path of
`reference.json`, which the checker already receives, so no runner script
changed. Every override applied is printed as a `[TOLER]` line, so a loosened
tolerance cannot pass silently.

The eight periodic thermal cases (`tgv_scp`, `*_scp_periodic_*`) now keep the
relative check on `global mass balance` and drop the absolute one. Isothermal and
inlet-outlet cases keep the tight global tolerance, where a non-zero value really
would indicate a defect.

### 3. Genuinely stale - refreshed

`channel_scp_inout_qw`, `annular_scp_inout_qw`, `pipe_scp_inout_qw`: `global
pressure drop` and `mean dpdx` collapsed from O(1e2-1e3) to ~0, with ux/gx
drifting 1e-4..1e-3. The near-zero pressure drops are the more physical values
and are consistent with the T1-T7 thermal corrections in `55103d1`.

### 4. Held back pending physics review

`annular_scp_inout_Tw` was **not** refreshed. It shows:

| metric | new | reference |
|---|---|---|
| bulk massflux gx | 0.908 | 0.999 |
| bulk velocity ux | 1.007 | 1.111 |
| total kinetic energy | 0.550 | 0.648 |
| global pressure drop | 4.19e+05 | -1.76e+04 |

`idriven = none`, so the mass flux is set by the inlet, and a 9% deficit in the
volume-averaged `rho*u` is either a real thermal-expansion transient produced by
the corrected energy equation or a conservation problem. Local divergence is
machine-zero either way, so the metrics cannot settle it. Wei will judge this
case before its reference is touched. Until then the suite reads 11/12 with this
one case deliberately red.

## Result

```text
MODE   : run
PASSED :  11 /  12
FAILED :   1 /  12   (annular_scp_inout_Tw, held back on purpose)
```

The refreshed values reproduced exactly on a clean rerun against the new
references, which confirms the cases are deterministic and the tolerances are
actually meetable.

## Refreshed references

```text
tests/regression/tgv_scp/reference.json
tests/regression/channel_scp_periodic_Tw/reference.json
tests/regression/channel_scp_inout_qw/reference.json
tests/regression/annular_scp_inout_qw/reference.json
tests/regression/pipe_scp_periodic_Tw/reference.json
tests/regression/pipe_scp_inout_qw/reference.json
```

Checked for NaN/Inf in every generated metrics file before copying; none found.

## Caveat

As with 2026-08-13, these references define the current solver output baseline.
They are not an independent physics validation. The one exception worth recording
is `bulk enthalpy` / `bulk temperature`, which are now believed to be *correct*
rather than merely current, since the previous values were produced by a
demonstrably wrong expression.

---

# Extended suite (B7) — same day

The 20-case extended suite was then run for the first time since the T1-T7
thermal corrections. It adds eight cases to the standard twelve:
`channel_scp_periodic_qw`, `channel_scp_inout_Tw`, `annular_iso_inout`,
`annular_scp_periodic_Tw`, `annular_scp_periodic_qw`, `pipe_iso_inout`,
`pipe_scp_periodic_qw`, `pipe_scp_inout_Tw`.

Starting point: **13/20**, zero solver failures, no NaN or Inf. The two extra
isothermal cases (`annular_iso_inout`, `pipe_iso_inout`) passed untouched.

## `global mass balance` in the periodic thermal cases — exactly -3x, and why

`channel_scp_periodic_qw`, `annular_scp_periodic_qw` and `pipe_scp_periodic_qw`
each moved by a ratio of **-3.000** against their stored reference:

| case | new | reference | ratio |
|---|---|---|---|
| channel_scp_periodic_qw | -1.146430e-05 |  3.821421e-06 | -3.000003 |
| annular_scp_periodic_qw | -1.694154e-05 |  5.647165e-06 | -3.000003 |
| pipe_scp_periodic_qw    | -1.126346e-05 |  3.754472e-06 | -3.000012 |

A clean integer ratio in three independent cases is a definition change, not
drift, so it was traced before anything was refreshed. It is the product of two
earlier review fixes:

- **x3** from T1 (`55103d1`): `Calculate_drhodt` divides by `tAlpha(isub)*dt`
  instead of `dt`. The monitor is written after the last RK3 sub-step, where
  `tAlpha(3) = tGamma(3) + tZeta(3) = 3/4 - 5/12 = 1/3`, so the reported
  `drho/dt` is exactly three times the old value.
- **x-1** from S1 (`b1503b9`, "Fix sign of the density-change term in the global
  mass balance"): `mass_imbalance(8) = -intg_m + ...` in
  `tools_solver.f90:1049`. A fully periodic domain has no boundary flux, so the
  metric reduces to `-intg_m` and the sign flip is the whole change.

`annular_scp_periodic_Tw` shows -2.976 rather than -3.000 because it is the one
periodic case with real heating, so a genuine physical component sits on top of
the definitional factor. The `physical Poisson compatibility defect` in the same
files also moved by exactly 3.0, consistent with the same `tAlpha` cause.

Everything else in those four files is unchanged to 8 significant digits
(e.g. channel kinetic energy 6.06497804e-01 vs 6.06497790e-01), so these are
diagnostic-only shifts with an accounted-for cause. Refreshed.

## Refreshed references (extended)

```text
tests/regression/channel_scp_periodic_qw/reference.json
tests/regression/annular_scp_periodic_Tw/reference.json
tests/regression/annular_scp_periodic_qw/reference.json
tests/regression/pipe_scp_periodic_qw/reference.json
```

`annular_scp_periodic_Tw` also picked up the corrected `bulk enthalpy` /
`bulk temperature` (2.86e-03 / 1.002636, previously -6.77e-08 / 1.000000), the
same io_monitor fix described above.

## Held back — all three `*_scp_inout_Tw` cases

The earlier decision to hold `annular_scp_inout_Tw` turns out not to be specific
to the annulus. All three constant-wall-temperature inlet-outlet cases move
together, and their `*_scp_inout_qw` siblings (refreshed above) do not:

| case | bulk massflux gx | ref | bulk velocity ux | ref | global pressure drop | ref |
|---|---|---|---|---|---|---|
| channel_scp_inout_Tw | 0.9678 | 0.9998 | 0.9984 | 1.0322 |  4.934e+05 | -9.396e+03 |
| annular_scp_inout_Tw | 0.9080 | 0.9990 | 1.0071 | 1.1106 |  4.191e+05 | -1.764e+04 |
| pipe_scp_inout_Tw    | 1.0450 | 0.9850 | 1.0450 | 0.9850 | -7.717e+02 | -6.697e+04 |

Two things are unresolved and neither can be settled from metrics alone:

1. **The pressure drop changes sign and reaches O(1e5)** in the channel and the
   annulus. These are 40-step runs (`niterthermofirst = 21`,
   `niterflowlast = 60`, `dt = 1e-5`) with an unramped Dirichlet wall
   temperature and `inout_buffer = 0.0`, so a violent startup transient is
   expected - but a positive 5e+05 where the reference was -9e+03 is not
   self-evidently the right transient.

2. **The pipe does not heat.** Bulk enthalpy across the constant-wall-temperature
   cases:

   | case | bulk enthalpy | bulk temperature |
   |---|---|---|
   | channel_scp_periodic_Tw | 2.110e-03 | 1.001989 |
   | annular_scp_periodic_Tw | 2.860e-03 | 1.002636 |
   | pipe_scp_periodic_Tw    | 1.386e-07 | 1.000000 |
   | channel_scp_inout_Tw    | 2.100e-03 | - |
   | annular_scp_inout_Tw    | 7.610e-03 | - |
   | pipe_scp_inout_Tw       | 1.225e-07 | 1.000000 |

   Both pipe cases sit four orders of magnitude below the channel and the
   annulus despite a comparable wall superheat (650.15 K wall against a 645.15 K
   inlet). The annulus is cylindrical too and does heat, so this is not
   "cylindrical" in general; the pipe differs in having `ifbcy_t = 0,4`, i.e.
   `IBC_INTERIOR` at the axis and the heated Dirichlet wall on the **second** y
   side. That is a hypothesis about where to look, not a diagnosis. It also
   explains the pipe row of the table above: with no heating the density stays
   at one, `ux` and `gx` agree to six digits, and the 6% rise is hydrodynamic.

   Note the `qw` cases are ~1e-08 in every geometry, so they say nothing either
   way - their heat flux ramps over steps 21-30 and deposits almost nothing in
   the window.

Until these are judged, the extended suite reads 17/20 with three cases
deliberately red.

## Result (extended)

```text
MODE   : run
PASSED :  17 /  20
FAILED :   3 /  20   (the three *_scp_inout_Tw cases, held back on purpose)
```

Refreshed values reproduced exactly on a clean rerun.

## Unrelated observation

`tests/regression/pipe_scp_periodic/` has a `reference.json` and a per-case
tolerance override in `tolerances.json`, but appears in neither `STANDARD_CASES`
nor `EXTENDED_CASES` in `run_regression.sh`. It is never executed. Left as-is
pending a decision on whether to wire it in or remove it.
