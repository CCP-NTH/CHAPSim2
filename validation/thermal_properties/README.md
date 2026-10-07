# Thermophysical property tables

Two isobaric property tables are read directly by the solver. `input_thermo.f90`
opens whichever one `ifluid` selects, by the filename constants in
`src/modules.f90`:

| `ifluid` | File | Fluid | Pressure | Temperature range | Rows |
|---|---|---|---|---|---|
| `scp_water` | `NIST_WATER_23.5MP.DAT` | Water | 23.5 MPa | 573.15 – 1073.15 K, step 0.1 K | 5001 |
| `scp_co2` | `NIST_CO2_8MP.DAT` | Carbon dioxide | 8 MPa | 220 – 520 K, step 0.2 K | 1501 |

Both pressures are above the critical point of their fluid, which is the point:
these tables are what makes the supercritical-property (`scp`) cases possible.
The solver needs the table in its **working directory**, which is why every
`*_scp_*` case under `tests/` carries a copy of the water table.

Column order, both files:

```
P (MPa)  H (J/kg)  T (K)  rho (kg/m3)  mu (Pa s)  k (W/m K)  cp (J/kg K)  beta (1/K)
```

`beta` is the isobaric volume expansivity, `-(1/rho) (d rho / dT)_P`. It is a
property of the equation of state, not a finite difference of the density
column — differencing the rounded density reproduces it only to about 2%.

## Provenance

These tables are **generated output of NIST Standard Reference Data**, not
original measurements and not tables authored for CHAPSim2. That was determined
by reproduction, not by the filename:

- The NIST Chemistry WebBook (SRD 69) "Thermophysical Properties of Fluid
  Systems" isobaric generator, queried on 2026-10-07 for water at 23.5 MPa on
  the same temperature grid at 12 significant figures, returns densities
  agreeing with this file to 11 significant figures
  (740.607021522 against 740.607021532 kg/m3 at 573.15 K).
- The same generator at its default 5 significant figures reproduces the CO2
  file's density, enthalpy and `cp` columns **exactly** at 220.0 K and 220.2 K.
- In both files the viscosity and thermal-conductivity columns differ from what
  the generator returns today, by up to 0.6% and 5% for water and 2% and 3% for
  CO2. The thermodynamic columns do not. That pattern is what an older vintage
  of the transport correlations looks like; the equation of state has not
  changed.

Neither the exact product version nor the generation date is recorded anywhere
in this repository, so the table vintage can only be bracketed by the transport
correlations above.

## Terms — read before redistributing

NIST Standard Reference Data is **not** in the public domain. It is one of the
few categories of United States government output that is expressly
copyrighted, under the Standard Reference Data Act, 15 U.S.C. § 290e, which
empowers the Secretary of Commerce to secure copyright in SRD on behalf of the
United States. NIST states this directly:

> Standard Reference Data (SRD) are copyrighted by the U.S. Secretary of
> Commerce on behalf of the United States of America. All rights reserved. None
> of our SRD may be reproduced, stored in a retrieval system or transmitted, in
> any form or by any means, electronic, mechanical, photocopying, recording or
> otherwise, without prior permission.
>
> — <https://www.nist.gov/srd/public-law>, retrieved 2026-10-07

The WebBook's fluid-properties page carries the matching notice: "© 2026 by the
U.S. Secretary of Commerce on behalf of the United States of America. All
rights reserved. Copyright for NIST Standard Reference Data is governed by the
Standard Reference Data Act."

Note the contrast with NIST's general licensing page,
<https://www.nist.gov/open/license>: data created by NIST employees that is
**not** SRD falls under 17 U.S.C. § 105 and carries a broad royalty-free
redistribution grant. SRD is carved out of that grant.

**No permission to redistribute these two files has been obtained or recorded.**
They are present here as a historical artefact of the repository, not as an
assertion that redistribution is permitted. If you are packaging, mirroring or
redistributing CHAPSim2, resolve this before you do.

Cite NIST, not CHAPSim2, for any property values taken from these tables:

> Lemmon, E.W., Bell, I.H., Huber, M.L., McLinden, M.O., "Thermophysical
> Properties of Fluid Systems", in *NIST Chemistry WebBook, NIST Standard
> Reference Database Number 69*, Linstrom, P.J. and Mallard, W.G., eds.,
> National Institute of Standards and Technology, Gaithersburg MD,
> <https://doi.org/10.18434/T4D303>.

## Generating a replacement yourself

CoolProp (<https://github.com/CoolProp/CoolProp>, **MIT licence**) implements
the same underlying equations of state — IAPWS-95 for water, Span-Wagner for
CO2 — and can regenerate both tables in the exact column order above, with no
redistribution question attached to the result.

```bash
pip install CoolProp
python3 validation/thermal_properties/generate_property_table.py \
    --fluid Water --pressure 23.5e6 --tmin 573.15 --tmax 1073.15 --tstep 0.1 \
    --output NIST_WATER_23.5MP.DAT
```

How close the result is, measured against the shipped files on 2026-10-07 with
CoolProp 8.0.0, sampling every 200th row for water and every 50th for CO2:

| Column | Water, max relative difference | CO2, max relative difference |
|---|---|---|
| Enthalpy | 1.2e-09 | 2.8e-05 |
| Density | 3.6e-10 | 4.5e-05 |
| `cp` | 4.9e-09 | 4.3e-05 |
| `beta` | 5.7e-09 | 4.0e-05 |
| Viscosity | 6.0e-03 | 2.3e-02 |
| Thermal conductivity | 5.2e-02 | 3.4e-02 |

The thermodynamic columns agree to the precision each file was written at — for
water that is round-off, confirming both come from IAPWS-95; for CO2 it is the
file's own 5-significant-figure truncation. The transport columns differ by up
to 5%, because CoolProp carries newer correlations than the vintage used here.

That last point is worth stating precisely, because it also dates the shipped
files. CoolProp's water viscosity at 573.15 K and 23.5 MPa is
`9.12499878020E-05` Pa s — **digit for digit** what the NIST WebBook returned
for the same state on 2026-10-07, and not what this directory's file contains
(`9.11809415003E-05`). CoolProp and NIST agree with each other today; the
shipped table agrees with neither, which places it before the current transport
correlations were adopted.

A CoolProp-generated table is therefore a drop-in replacement thermodynamically,
but **it is not numerically identical**: a 5% change in thermal conductivity
moves every supercritical case's thermal field, so swapping the tables requires
an approved baseline refresh, not a silent substitution. See
`tests/reference_update_log_*.md` for how such a refresh is recorded.
