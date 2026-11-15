# Reference update log — 2026-11-14

Approved by Wei Wang on 2026-11-14.

## Change behind the moves
No solver change. The supercritical-water property table used by the tests (`NIST_WATER_23.5MP.DAT`,
36 tracked copies: 28 files and 8 symlinks) was replaced by a table generated with CoolProp 8.0.0
(MIT licence), so that no NIST Standard Reference Data is redistributed:

```bash
python3 validation/thermal_properties/generate_property_table.py --fluid Water --pressure 23.5e6 \
    --segments 573.15:623.15:0.1,623.15:673.15:0.02,673.15:873.15:0.1 --header-note "..." \
    --output NIST_WATER_23.5MP.DAT
```

The grid is exactly that of the former table: 5001 rows, 573.15 - 873.15 K, 0.1 K steps, 0.02 K in
623.15 - 673.15 K. Compared with the former table on that grid, the maximum relative differences are:

- enthalpy 3.7e-8, density 7.0e-10, `cp` and `beta` 1.1e-5;
- viscosity 1.2% (mean 0.26%);
- thermal conductivity 5.3% (mean 1.4%).

All maxima are near the pseudo-critical temperature (about 653 K). Only the transport columns change
materially.

**Expected response.** Cases with an imposed wall temperature (`_Tw`) respond most, because the wall
heat flux follows k dT/dn: bulk enthalpy up to 7.5%, pressure drop up to 17% (full-step value,
`annular_scp_inout_Tw`). Imposed-heat-flux cases (`_qw`) have a fixed heat input: bulk enthalpy
<= 0.24%, pressure drop <= 0.5%. Sealed periodic cases change at the 1e-8 level in their bulk
quantities. Their "fails" below come from the documented-limitation mass metrics (global mass
balance, total mass drift: relative changes 1.4e-2 .. 0.35, gated at rel 1e-3).

## Baselines
Each updated reference keeps its existing key set; only values change, copied verbatim from
`regression_test_metrics.json` of the run with the new table. Executable sha256 `88a5ae65...`
(gfortran 13.3, `cfg=default`, generic FFT), NP 4, `CHAPSIM_B3_PBAR=1`; the source is unchanged
from PR #10 `fc35e98`.

| case | largest change of a physical metric (relative) | old reference with new table |
|---|---|---|
| annular_scp_inout_Tw | global pressure drop (full-step pressure, t_mid) 1.7e-01; mean dpdx (full-step pressure, t_mid) 1.7e-01 | **fails** |
| annular_scp_inout_qw | bulk enthalpy 1.4e-03; global pressure drop 1.7e-04 | passes |
| annular_scp_periodic_Tw | bulk velocity uz 1.4e-07; bulk enthalpy 7.3e-08 | **fails** |
| annular_scp_periodic_qw | bulk enthalpy 2.4e-03; bulk velocity uz 1.4e-08 | **fails** |
| channel_scp_inout_Tw | global pressure drop 1.2e-01; mean dpdx 1.2e-01 | **fails** |
| channel_scp_inout_qw | global pressure drop (full-step pressure, t_mid) 2.0e-04; mean dpdx (full-step pressure, t_mid) 2.0e-04 | passes |
| channel_scp_inout_qw_cp4 | global pressure drop 1.6e-04; mean dpdx 1.6e-04 | passes |
| channel_scp_periodic_Tw | bulk enthalpy 4.7e-09; total kinetic energy 1.6e-09 | **fails** |
| channel_scp_periodic_qw | bulk enthalpy 1.1e-04 | **fails** |
| pipe_scp_inout_Tw | global pressure drop 7.9e-02; mean dpdx 7.9e-02 | **fails** |
| pipe_scp_inout_qw | global pressure drop (full-step pressure, t_mid) 4.6e-03; mean dpdx (full-step pressure, t_mid) 4.6e-03 | **fails** |
| pipe_scp_inout_qw_pchip | mean dpdx 5.2e-04; global pressure drop 5.2e-04 | passes |
| pipe_scp_periodic_Tw | bulk velocity uz 3.0e-08; bulk enthalpy 1.4e-08 | **fails** |
| pipe_scp_periodic_qw | bulk enthalpy 1.9e-04; bulk velocity uz 8.5e-09 | **fails** |
| tgv_scp | round-off metrics only | **fails** |
| F: LES_channel_scp_inout_Tw | max. |sgs enthalpy flux| 1.7e-01; mean dpdx 1.2e-01 | **fails** |
| F: LES_duct_scp_zwall_Tw | global pressure drop 3.6e-02; mean dpdx 3.6e-02 | **fails** |
| F: MHD_channel_scp_inout_Tw | bulk enthalpy 7.5e-02; mean dpdx 1.7e-03 | **fails** |
| F: inlet_database/same_yz_mesh | mean dpdx 4.5e-02; global pressure drop 4.5e-02 | **fails** |
| F: inlet_database/diff_yz_mesh | mean dpdx 4.4e-02; global pressure drop 4.4e-02 | **fails** |
| F: mesh_mapping/channel_scp_inout/step5_clamp_run | global pressure drop 2.4e-02; mean dpdx 2.4e-02 | passes |
| F: mesh_mapping/channel_scp_inout/step7_extend_run | mean dpdx 5.7e-02; global pressure drop 5.7e-02 | **fails** |

## Left unchanged, and why
- **The 14 isothermal references.** These cases do not read the table. Their metrics files are
  byte-identical to the verified run with the former table.
- **`tests/tools/tolerances.json`.** No tolerance was changed.
- **`tests/functional/restart/*/reference.json`.** The restart suite gates on restart-versus-continuous
  equivalence (5/5 PASS with the new table), not on these files, which are already stale.
- **`mesh_mapping` identity check.** It compares two runs and needs no reference; it still passes.
