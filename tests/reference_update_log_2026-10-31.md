# Reference update log — 2026-10-31

Approved by Wei Wang on 2026-10-31 (conditional approval, conditions verified: per-case record, mid-ramp restart check, Nemati outlet check). Applied with `apply.sh` of the staged proposal.

## Solver changes behind the moves
Integration of the CPU correctness fixes found while porting the solver to GPUs (development
branch `integration/cpu-gpu-fixes-20261030` on `d9ecc687`; review items are numbered as in
that review):
* **Thermal clock** (`b247ed68`): the thermal iteration advances with the run also before
  `niterthermofirst`, so `qw_ramp = .true., s, e` ramps over run iterations s..e as documented
  (dev ramped 20 steps late in the delayed-start qw cases).
* **Fresh-start boundary thermal states** (item 11, `e12c2fa8`): built from the
  initial interior as a restart does; with temperature noise the Neumann wall/outlet states (and
  the initial `gx = ρu` at a Neumann outlet) change. Fresh run = restart from its step-0 state,
  bit for bit (dev: velocity 1e-8..1e-7, pressure 2e-5..5e-4 apart).
* **Singular y-TDMA line with z walls** (item 9, `5ebe778d`): compatibility projection of the
  (0,0) line restored (residual 2.2e-3 → 4.8e-14 of the source).
* **Inlet-database functional cases** (item 14, `5688cde7`): `ndb_file_offset = 0`, so the cases
  read the shipped database; their old references came from a run that could not read it.

## Baselines
Each updated reference keeps its existing key set; only values change, copied verbatim from
`regression_test_metrics.json` of the final build (`7995ddca`, gfortran 13.3, NP 4,
`CHAPSIM_B3_PBAR=1`). Per-metric old/new values: `TABLE.md` of the proposal.

| case | cause | largest change (relative) | old reference |
|---|---|---|---|
| channel_scp_inout_qw | clock | full-step Δp 3.3e-5; bulk enthalpy 0.57 | passes |
| channel_scp_inout_qw_cp4 | clock | Δp 2.4e-4; bulk enthalpy 0.57 | passes |
| pipe_scp_inout_qw | clock (2.4e-3) + item 11 (3e-5) | full-step Δp 2.4e-3 | **fails** (full-step keys, rel tol 1e-3) |
| pipe_scp_inout_qw_pchip | clock + item 11 | full-step Δp 4.0e-4 | passes |
| channel_scp_periodic_qw | item 11 | bulk enthalpy 3e-8 | passes |
| annular_scp_periodic_qw | item 11 | bulk enthalpy 2.6e-6 | passes |
| annular_scp_inout_Tw | item 11 | Δp 3.6e-8; bulk enthalpy 5.9e-6 | passes |
| pipe_scp_inout_Tw | item 11 | Δp 9.2e-8 | passes |
| F: LES_duct_scp_zwall_Tw | item 11 (2.7e-4) + item 9 (6e-6) | Δp 1.7e-4; bulk enthalpy 2.7e-3 | passes |
| F: inlet_database/same_yz_mesh | item 14 | old values meaningless (bulk 0.34, Δp −3.4e4) → bulk 0.957, Δp 1.815 | fails |
| F: inlet_database/diff_yz_mesh | item 14 | → bulk 0.956, Δp 0.980 | fails |

The two inlet-database cases are added to `FUNCTIONAL_CASES` (`tests/regression/case_lists.sh`).
Mass-conservation metrics stay at round-off in every moved case. Moved-but-passing references
are updated too, so that the references describe the merged solver (as in the 2026-10-10
update).

## Untouched
The other 27 extended + functional cases (metrics identical to dev), `tests/tools/tolerances.json`,
the functional restart and mesh-mapping references (pass), and every isothermal case.

## Correctness evidence (not only difference)
Item 11: fresh = step-0 restart bitwise (`channel/annular_scp_periodic_qw`, `pipe_scp_inout_qw`;
`LES_duct_scp_zwall_Tw` consistent before and after, change confined to the initial `gx` at the
Neumann outlet). Clock: wall heat input per step follows the half-cosine over run iterations
21..30 (`CHAPSIM_B3_BUDGET`), energy residual at round-off. Item 9: compatibility residual
4.8e-14. Item 14: inlet planes equal the database records (qx bitwise, qy/qz ≤ 7.6e-8), 1 and 4
ranks identical, restarts from 20 and 30 exact.
Clock restart consistency (`channel_scp_inout_qw`, thermal from 21, ramp 21..30, final build,
checkpoints every 5): continuation from 15 and 20 (flow restart, thermal field initialised as in
the input) and from 25 (mid-ramp) and 30 (thermal restart) equals the uninterrupted run bit for
bit at every later checkpoint. Item 9 in the metrics: `LES_duct_scp_zwall_Tw` "Poisson zero-mode
projection" 0 -> -4.5e-11 (the singular line is now projected), interior divergence
3.8e-14 -> 3.4e-15.

## Appendix — per-case values (old reference, new reference)
Physical metrics one by one; mass-conservation residuals and Poisson amplitudes summarised by
their largest magnitude.

### LES_duct_scp_zwall_Tw
`tests/functional/LES_duct_scp_zwall_Tw/reference.json` — cause: item 11 (Neumann outlet state / initial gx) + item 9

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | 8.8817842e-16 | -3.55271368e-15 | 4.441e-15 | 1.25e+00 |
| total mass drift from run start | -4.39687409e-06 | -4.3986725e-06 | 1.798e-09 | 4.09e-04 |
| global pressure drop | 0.701074475 | 0.70119139 | 1.169e-04 | 1.67e-04 |
| mean dpdx | -0.0220921468 | -0.0220961234 | 3.977e-06 | 1.80e-04 |
| total kinetic energy | 0.610160849 | 0.610161666 | 8.170e-07 | 1.34e-06 |
| bulk velocity uz | -0.000572615935 | -0.000572226174 | 3.898e-07 | 6.81e-04 |
| bulk massflux gx | 1.00165585 | 1.00165589 | 4.000e-08 | 3.99e-08 |
| bulk massflux gz | -0.000573409914 | -0.000573018829 | 3.911e-07 | 6.82e-04 |
| bulk enthalpy | 8.57172565e-08 | 8.5483131e-08 | 2.341e-10 | 2.73e-03 |
| max. initial strain rate S_ijS_ij | 717.668303 | 717.668626 | 3.230e-04 | 4.50e-07 |
| max. sgs coefficient at inlet/outlet | 54.2620593 | 54.2620591 | 2.000e-07 | 3.69e-09 |
| max. |sgs enthalpy flux| | 0.00012141884 | 0.000121418847 | 7.000e-12 | 5.77e-08 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 3.84e-14, max |new| 4.46e-11.

### annular_scp_inout_Tw
`tests/regression/annular_scp_inout_Tw/reference.json` — cause: item 11 (Neumann outlet state / initial gx)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| total mass drift from run start | -0.000158626787 | -0.000158626886 | 9.900e-11 | 6.24e-07 |
| global pressure drop | 88.2775094 | 88.2775062 | 3.200e-06 | 3.62e-08 |
| mean dpdx | -4.68326733 | -4.68326716 | 1.700e-07 | 3.63e-08 |
| bulk enthalpy | 1.0411663e-07 | 1.04117243e-07 | 6.130e-13 | 5.89e-06 |
| global pressure drop (full-step pressure, t_mid) | -18.2038388 | -18.2038389 | 1.000e-07 | 5.49e-09 |
| mean dpdx (full-step pressure, t_mid) | 0.965743641 | 0.96574365 | 9.000e-09 | 9.32e-09 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 1.15e-11, max |new| 1.54e-11.

### annular_scp_periodic_qw
`tests/regression/annular_scp_periodic_qw/reference.json` — cause: item 11 (qw wall states from the noisy interior)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | -2.10883179e-05 | -2.10883181e-05 | 2.000e-13 | 9.48e-09 |
| mean dpdx | -4.70428916e-22 | -1.84467817e-21 | 1.374e-21 | 7.45e-01 |
| bulk velocity uz | -7.36847922e-06 | -7.36847896e-06 | 2.600e-13 | 3.53e-08 |
| bulk massflux gz | -7.37165523e-06 | -7.37165497e-06 | 2.600e-13 | 3.53e-08 |
| bulk enthalpy | 4.96592719e-09 | 4.96593994e-09 | 1.275e-14 | 2.57e-06 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 1.12e-06, max |new| 1.12e-06.

### channel_scp_inout_qw
`tests/regression/channel_scp_inout_qw/reference.json` — cause: thermal clock (qw_ramp on run iterations)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | -7.10542736e-15 | -3.55271368e-15 | 3.553e-15 | 5.00e-01 |
| total mass | 63.9999997 | 63.9999993 | 4.000e-07 | 6.25e-09 |
| total mass drift from run start | -2.89353522e-07 | -6.75164586e-07 | 3.858e-07 | 5.71e-01 |
| global pressure drop | 0.0436305859 | 0.0436319457 | 1.360e-06 | 3.12e-05 |
| mean dpdx | -0.000681727905 | -0.000681749152 | 2.125e-08 | 3.12e-05 |
| total kinetic energy | 0.608010627 | 0.608010629 | 2.000e-09 | 3.29e-09 |
| bulk enthalpy | 4.82125752e-12 | 1.12529162e-11 | 6.432e-12 | 5.72e-01 |
| global pressure drop (full-step pressure, t_mid) | 0.043722633 | 0.043721195 | 1.438e-06 | 3.29e-05 |
| mean dpdx (full-step pressure, t_mid) | -0.000683166141 | -0.000683143672 | 2.247e-08 | 3.29e-05 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 1.12e-11, max |new| 2.23e-11.

### channel_scp_inout_qw_cp4
`tests/regression/channel_scp_inout_qw_cp4/reference.json` — cause: thermal clock (qw_ramp on run iterations)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | 5.32907052e-15 | 1.77635684e-15 | 3.553e-15 | 6.67e-01 |
| total mass | 63.9999997 | 63.9999993 | 4.000e-07 | 6.25e-09 |
| total mass drift from run start | -2.88100097e-07 | -6.72249207e-07 | 3.841e-07 | 5.71e-01 |
| global pressure drop | 0.0430121806 | 0.0430016855 | 1.050e-05 | 2.44e-04 |
| mean dpdx | -0.000680724517 | -0.000680559641 | 1.649e-07 | 2.42e-04 |
| total kinetic energy | 0.608977145 | 0.608977148 | 3.000e-09 | 4.93e-09 |
| bulk enthalpy | 3.8321794e-12 | 8.94578017e-12 | 5.114e-12 | 5.72e-01 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 2.52e-11, max |new| 4.17e-11.

### channel_scp_periodic_qw
`tests/regression/channel_scp_periodic_qw/reference.json` — cause: item 11 (qw wall states from the noisy interior)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | -1.42651072e-05 | -1.42651116e-05 | 4.400e-12 | 3.08e-07 |
| total mass drift from run start | 5.75538195e-09 | 5.75537484e-09 | 7.110e-15 | 1.24e-06 |
| mean dpdx | -1.98166625e-21 | -7.10749771e-21 | 5.126e-21 | 7.21e-01 |
| bulk enthalpy | 2.87202546e-08 | 2.87202555e-08 | 9.000e-16 | 3.13e-08 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 2.23e-07, max |new| 2.23e-07.

### diff_yz_mesh
`tests/functional/inlet_database/diff_yz_mesh/reference.json` — cause: item 14 (database read; old reference meaningless)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | 2.66453526e-15 | -1.77635684e-15 | 4.441e-15 | 1.67e+00 |
| total mass | 38.2623818 | 39.4784089 | 1.216e+00 | 3.08e-02 |
| total mass drift from run start | 0.00125115743 | -1.74266644e-05 | 1.269e-03 | 1.01e+00 |
| global pressure drop | -34601.9165 | 0.980231971 | 3.460e+04 | 1.00e+00 |
| mean dpdx | 876.47659 | -0.0248295604 | 8.765e+02 | 1.00e+00 |
| total kinetic energy | 0.153693424 | 0.563251909 | 4.096e-01 | 7.27e-01 |
| bulk velocity ux | 0.341544964 | 0.955723738 | 6.142e-01 | 6.43e-01 |
| bulk velocity uz | 0.00210529586 | 0.00195583905 | 1.495e-04 | 7.10e-02 |
| bulk massflux gx | 0.324216383 | 0.955723731 | 6.315e-01 | 6.61e-01 |
| bulk massflux gz | 0.00205456291 | 0.00195583904 | 9.872e-05 | 4.81e-02 |
| bulk enthalpy | -0.000526322453 | 6.39897092e-10 | 5.263e-04 | 1.00e+00 |
| bulk temperature | 0.999466152 | 1 | 5.338e-04 | 5.34e-04 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 1.22e+04, max |new| 1.01e-11.

### pipe_scp_inout_Tw
`tests/regression/pipe_scp_inout_Tw/reference.json` — cause: item 11 (Neumann outlet state / initial gx)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | -4.4408921e-16 | 4.4408921e-16 | 8.882e-16 | 2.00e+00 |
| total mass drift from run start | -9.93456495e-05 | -9.93456752e-05 | 2.570e-11 | 2.59e-07 |
| global pressure drop | 45.4130224 | 45.4130266 | 4.200e-06 | 9.25e-08 |
| mean dpdx | -1.80660987 | -1.80661004 | 1.700e-07 | 9.41e-08 |
| bulk velocity uz | 9.08454063e-08 | 9.08453985e-08 | 7.800e-15 | 8.59e-08 |
| bulk massflux gz | 9.07697258e-08 | 9.07697182e-08 | 7.600e-15 | 8.37e-08 |
| bulk enthalpy | 7.35878055e-08 | 7.35877916e-08 | 1.390e-14 | 1.89e-07 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 1.27e-12, max |new| 3.71e-12.

### pipe_scp_inout_qw
`tests/regression/pipe_scp_inout_qw/reference.json` — cause: thermal clock + item 11

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | 0 | -1.33226763e-15 | 1.332e-15 | 1.00e+00 |
| total mass | 25.1371261 | 25.1371257 | 4.000e-07 | 1.59e-08 |
| total mass drift from run start | -3.027174e-07 | -7.06535008e-07 | 4.038e-07 | 5.72e-01 |
| global pressure drop | 0.0469158505 | 0.0466033508 | 3.125e-04 | 6.66e-03 |
| mean dpdx | -0.00186639501 | -0.00185396322 | 1.243e-05 | 6.66e-03 |
| total kinetic energy | 0.667008655 | 0.667008662 | 7.000e-09 | 1.05e-08 |
| bulk velocity uz | 9.07760327e-08 | 9.07758964e-08 | 1.363e-13 | 1.50e-06 |
| bulk massflux gx | 1.000299 | 1.00029901 | 1.000e-08 | 1.00e-08 |
| bulk massflux gz | 9.07116245e-08 | 9.07114858e-08 | 1.387e-13 | 1.53e-06 |
| bulk enthalpy | 1.01718064e-07 | 1.01726897e-07 | 8.833e-12 | 8.68e-05 |
| global pressure drop (full-step pressure, t_mid) | 0.0480671785 | 0.0479510126 | 1.162e-04 | 2.42e-03 |
| mean dpdx (full-step pressure, t_mid) | -0.00191219686 | -0.00190757558 | 4.621e-06 | 2.42e-03 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 7.07e-13, max |new| 2.16e-12.

### pipe_scp_inout_qw_pchip
`tests/regression/pipe_scp_inout_qw_pchip/reference.json` — cause: thermal clock + item 11

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | -8.8817842e-16 | 0 | 8.882e-16 | 1.00e+00 |
| total mass | 25.1371275 | 25.1371271 | 4.000e-07 | 1.59e-08 |
| total mass drift from run start | -3.02797819e-07 | -7.066153e-07 | 4.038e-07 | 5.71e-01 |
| global pressure drop | 0.0467459028 | 0.0467285046 | 1.740e-05 | 3.72e-04 |
| mean dpdx | -0.00185963419 | -0.00185894206 | 6.921e-07 | 3.72e-04 |
| global pressure drop (full-step pressure, t_mid) | 0.0478787442 | 0.0478593641 | 1.938e-05 | 4.05e-04 |
| mean dpdx (full-step pressure, t_mid) | -0.00190470062 | -0.00190392964 | 7.710e-07 | 4.05e-04 |
| total kinetic energy | 0.667008677 | 0.667008684 | 7.000e-09 | 1.05e-08 |
| bulk velocity ux | 1.00030025 | 1.00030026 | 1.000e-08 | 1.00e-08 |
| bulk velocity uz | 9.07760313e-08 | 9.07758952e-08 | 1.361e-13 | 1.50e-06 |
| bulk massflux gx | 1.00029904 | 1.00029905 | 1.000e-08 | 1.00e-08 |
| bulk massflux gz | 9.07116204e-08 | 9.07114817e-08 | 1.387e-13 | 1.53e-06 |
| bulk enthalpy | 9.79138378e-08 | 9.79226722e-08 | 8.834e-12 | 9.02e-05 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 1.65e-12, max |new| 2.12e-12.

### same_yz_mesh
`tests/functional/inlet_database/same_yz_mesh/reference.json` — cause: item 14 (database read; old reference meaningless)

| metric | old | new | abs change | rel change |
|---|---|---|---|---|
| global mass balance | -1.77635684e-15 | -3.55271368e-15 | 1.776e-15 | 5.00e-01 |
| total mass | 38.2623809 | 39.4784045 | 1.216e+00 | 3.08e-02 |
| total mass drift from run start | 0.00125140606 | -2.17748788e-05 | 1.273e-03 | 1.02e+00 |
| global pressure drop | -34487.1023 | 1.81489063 | 3.449e+04 | 1.00e+00 |
| mean dpdx | 873.568315 | -0.0459717066 | 8.736e+02 | 1.00e+00 |
| total kinetic energy | 0.158462747 | 0.569033317 | 4.106e-01 | 7.22e-01 |
| bulk velocity ux | 0.344048899 | 0.957049992 | 6.130e-01 | 6.41e-01 |
| bulk velocity uz | 0.00230144148 | 0.00221311333 | 8.833e-05 | 3.84e-02 |
| bulk massflux gx | 0.324813945 | 0.957049984 | 6.322e-01 | 6.61e-01 |
| bulk massflux gz | 0.00224630297 | 0.00221311334 | 3.319e-05 | 1.48e-02 |
| bulk enthalpy | -0.0005434222 | 6.99988301e-10 | 5.434e-04 | 1.00e+00 |
| bulk temperature | 0.999448555 | 1 | 5.514e-04 | 5.51e-04 |

Residual metrics (mass-conservation residuals, Poisson amplitudes): max |old| 1.22e+04, max |new| 3.56e-11.

