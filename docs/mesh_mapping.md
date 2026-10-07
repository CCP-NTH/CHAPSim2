# Mesh remapping (`is_prerun = .true.`)

How to move an existing CHAPSim restart onto a different mesh, what the solver
actually does when you ask for it, and which parts of the state are and are not
carried across.

---

## 1. What it is for

Typical uses:

- a coarse run has reached a statistically steady state and you want to continue
  it on a finer mesh without paying for the transient again;
- you want to lengthen or shorten the streamwise box of a developed field;
- you want to change the wall-normal stretching or the azimuthal resolution of a
  pipe/annulus.

The prerun is a *one-shot conversion*: it reads a restart written on the source
mesh, interpolates every stored field onto the target mesh, writes an
iteration-0 restart for the target mesh, and stops. It never advances the
solution.

## 2. How to run it

Three directories, run in order.

```
step0_source/     # whatever produced the field you want to remap
step1_remap/      # the prerun
step2_new_mesh/   # the continuation on the new mesh
```

**step1** needs two input files:

| file | describes |
|---|---|
| `input_chapsim.ini` | the **source**: geometry, mesh, BCs, thermo, scheme — i.e. a copy of the run that produced the restart, with `is_prerun= .true.` |
| `input_chapsim_tgt.ini` | the **target**: only `[domain]` and `[mesh]` are read |

Everything except `[domain]` and `[mesh]` is inherited from
`input_chapsim.ini`, so the two files must not disagree about the physics.

Then:

```bash
# 1. seed step1 with the source restart
cp step0_source/1_data/*_20.bin step0_source/1_data/*_20.dat step1_remap/1_data/

# 2. convert  (reads *_20 on the source mesh, writes *_0 on the target mesh)
cd step1_remap && ./run_chapsim.sh

# 3. seed step2 with the converted restart and continue
cp step1_remap/1_data/*_0.bin step1_remap/1_data/*_0.dat step2_new_mesh/1_data/
cd ../step2_new_mesh && ./run_chapsim.sh     # initfl= restart, irestartfrom= 0
```

`step2_new_mesh/input_chapsim.ini` must have the **same** `[domain]` and
`[mesh]` as `input_chapsim_tgt.ini`, and `irestartfrom= 0` in both `[flow]` and
`[thermo]`.

## 3. What gets remapped

Interpolation is trilinear in the computational coordinates, done separately for
each staggered location (cell centre or face) in each direction, so every field
is sampled at the points it actually lives on.

**Interior (`build_up_interp_target_field_flow` / `_thermo`)**

- `qx, qy, qz` and, for thermal runs, `gx, gy, gz`
- `pr`
- the AB2 history `mx_rhs0, my_rhs0, mz_rhs0`
- `rhoh`, `dDens`, `ene_rhs0`

After the thermal interpolation, `h, T, k, sigma_e, d, mu` are **rebuilt from
`(rhoh, d)` through the property table** rather than interpolated. This puts the
remapped state back on the equation of state, which is the consistent choice,
but it is not an identity: on an identity remap it moves the density by about
6e-9 relative. Divided by `dt`, that shows up as a one-step `drho/dt` transient
in the pressure that has decayed by the second step. It is expected.

**Streamwise bulk (`preserve_interp_streamwise_bulk`)**

The interior is rescaled so the target carries the same streamwise bulk flux as
the source, absorbing the O(interpolation error) drift that trilinear sampling
would otherwise introduce.

**Inlet/outlet planes (`build_up_interp_target_xoutlet_state`)**

For `is_conv_outlet(1)` configurations the stored `fbcx_*` planes are remapped
*bilinearly in (y, z) from the source's own in-memory planes*:

```
fbcx_qx, fbcx_qy, fbcx_qz            (and fbcx_gx/gy/gz when thermal)
fbcx_a0cc_rhs0, fbcx_a0pc_rhs0, fbcx_a0cp_rhs0    (convective-outlet AB2 history)
fbcx_ftp_d, fbcx_ftp_rhoh                          (then refreshed via the table)
```

This must not be re-derived from the target interior. The four `fbcx` slots are
**not** "the first and last interior layers": slot 1 is the inlet boundary value
(a prescribed profile, or zero transverse flow), slot 2 the outlet, slots 3/4
the second layers, and their content is whatever the BC machinery is holding.
Deriving them from the interior replaces a Poiseuille inlet's zero cross-flow
(±1.8e-5) with the first cell-centre velocity (±0.5) and the hot-inlet density
(0.3398) with the interior value (~1.0), which corrupts the global mass balance
from the first step.

The bulk rescale above is applied to the interior but **not** to these planes,
so they carry a 1 + O(interpolation error) inconsistency that
`enforce_domain_mass_balance_dyn_fbc` absorbs each step.

## 4. Extending or shortening the box

`setup_extension_mapping` compares source and target extents per direction:

| relation | mode |
|---|---|
| target length == source length | 0 — direct mapping |
| target shorter | 1 — clamp: the target reads the first `L_tgt` of the source |
| target longer | 2 — repeat: a trailing chunk of the source is tiled to fill the extra length |

The tiled chunk is `Lbuf = max((L_tgt - L_src)/5, 2*h_tgt)` long, taken from the
end of the source and wrapped with `modulo`. This makes the appended region
periodic with period `Lbuf`, which is a reasonable seed for a developed field
but is not a solution; expect a transient over the joint. It applies
independently in x and z.

## 5. Limitations

- **The recorded x-inlet database is not remapped.** A case using
  `ifbcx_u= 10,...` (`IBC_DATABASE`) with
  `is_record_xoutlet_read_xinlet= .false.,.true.` reads
  `domain1_xoutlet_database_*.bin`, which is written on the *source* (y, z)
  mesh. Changing `ncy` or `ncz` invalidates it. Either keep the cross-section
  fixed, or re-record the database on the new mesh.
- **`nxdomain > 1` (multi-domain) is not supported** anywhere in the code
  today; the prerun inherits that. Use `nxdomain= 1`.
- **`qy` is remapped in its stored flux form** — `qy = r*u_r` is interpolated
  directly rather than being converted to `u_r` first. This is deliberate: the
  radial mass balance is written in `r*u_r`, and `r*u_r` stays regular on the
  axis where `u_r = qy/r` does not. The consequence is that the remap is linear
  in `r*u_r` rather than in `u_r`, so the two differ at the O(dr^2) level of the
  interpolation itself.
- The prerun writes iteration 0 only. Statistics, visualisation output and the
  probe history do not carry across.

## 6. Tests

`tests/functional/mesh_mapping/` — run with

```bash
cd tests && ./run_functional.sh          # both functional suites
# or
cd tests/functional/mesh_mapping && CI=true ./run_functional.sh
```

Two cases, each a linear chain of step directories:

| case | covers |
|---|---|
| `channel_scp_inout` | Cartesian, supercritical water, Poiseuille inlet + convective outlet, wall-normal stretching |
| `pipe_iso_periodic` | cylindrical with the axis, periodic in x and z, constant-massflux driving |

Each chain makes two kinds of assertion:

- **identity** (`step2_identity_remap` → `step3_identity_run` vs
  `step1_control`): the target mesh equals the source mesh, so remap-then-run
  must reproduce a plain restart of the same field. This is the invariant that
  has real teeth — it needs no stored baseline, and it is what exposed the
  `fbcx` defect described in §3. Measured agreement after 20 steps is exact for
  the isothermal pipe and at round-off for the thermal channel.
- **reference** (the genuine mesh changes): the mesh changes, so there is no
  analytic invariant left. These steps assert against a stored `reference.json`
  and act as a smoke test that the remapped state is a valid initial condition —
  finite, divergence-free after projection, mass-consistent.

Tolerances live in `tolerance_identity.json` and `tolerance_reference.json`
alongside the runner, with the reasoning for each class of metric in the
`_comment` block of each file.
