# Thermophysical property tables

CHAPSim2's supercritical-property (`scp`) cases read a tabulated isobar.
`input_thermo.f90` opens whichever file the `ifluid` input selects, by the
filename constants in `src/modules.f90`:

| `ifluid` | File | Fluid | Pressure | Temperature range | Rows | Shipped here |
|---|---|---|---|---|---|---|
| `scp_water` | `NIST_WATER_23.5MP.DAT` | Water | 23.5 MPa | 573.15 – 1073.15 K, step 0.1 K | 5001 | **yes** — see "Unresolved" below |
| `scp_co2` | `NIST_CO2_8MP.DAT` | Carbon dioxide | 8 MPa | 220 – 520 K, step 0.2 K | 1501 | **no — withdrawn**, see below |

Both pressures are above the critical point of their fluid, which is the point:
these tables are what makes the `scp` cases possible. The solver needs the table
in its **working directory**, which is why every `*_scp_*` case under `tests/`
carries a copy of the water table.

`ifluid= scp_co2` remains a supported solver option. Only the data file is
withdrawn — supply your own `NIST_CO2_8MP.DAT` in the working directory and the
case runs. "Generating a table yourself" below produces one under a licence
that permits redistribution.

Column order, both files:

```
P (MPa)  H (J/kg)  T (K)  rho (kg/m3)  mu (Pa s)  k (W/m K)  cp (J/kg K)  beta (1/K)
```

`beta` is the isobaric volume expansivity, `-(1/rho) (d rho / dT)_P`. It is a
property of the equation of state, not a finite difference of the density
column — differencing the rounded density reproduces it only to about 2%.

---

## Rights status — read before redistributing

Three separate questions are kept separate below, because they have different
answers and different kinds of evidence behind them. Conflating them is how a
restriction gets missed.

### 1. Evidence of provenance: where these tables came from

**Conclusion: both files are generated output of NIST Standard Reference
Database 69.** They are not original measurements, and not tables authored for
CHAPSim2.

This was established by **reproducing them**, not inferred from the filename.
Source: the NIST Chemistry WebBook, "Thermophysical Properties of Fluid
Systems" (SRD 69), isobaric generator at
<https://webbook.nist.gov/chemistry/fluid/>, queried 2026-10-07.

| Observation | Result |
|---|---|
| Water, 23.5 MPa, 573.15 K, generator at 12 significant figures | returns `740.607021522` kg/m³; this file holds `740.607021532` — agreement to 11 significant figures |
| CO₂, 8 MPa, 220.0 K and 220.2 K, generator at its default 5 significant figures | reproduces the file's density, enthalpy and `cp` columns **exactly** |
| Viscosity and thermal conductivity, both files | differ from today's generator by up to 0.6% / 5% (water) and 2% / 3% (CO₂) |

The thermodynamic columns match and the transport columns do not. That is the
signature of an older vintage of the transport correlations over an unchanged
equation of state — so these are SRD 69 output, generated at some earlier date.

Limitation, stated rather than papered over: **the exact product version and
generation date are not recorded anywhere in this repository.** The vintage can
only be bracketed by the transport correlations above. The commit that
introduced both files, `00b80c2` "Add validation thermal property tables"
(2026-08-11), carries no provenance note.

### 2. Evidence of applicable redistribution restrictions

**Conclusion: NIST Standard Reference Data is expressly copyrighted and its
published terms prohibit redistribution without prior permission.**

This is not the usual position for United States government output. Most works
of the US federal government are uncopyrightable under 17 U.S.C. § 105. SRD is
a **statutory carve-out**: the Standard Reference Data Act, **15 U.S.C. § 290e**,
empowers the Secretary of Commerce to secure copyright in Standard Reference
Data on behalf of the United States.

Authoritative sources, with the exact applicable terms:

- **<https://www.nist.gov/srd/public-law>** (retrieved 2026-10-07):

  > Standard Reference Data (SRD) are copyrighted by the U.S. Secretary of
  > Commerce on behalf of the United States of America. All rights reserved.
  > None of our SRD may be reproduced, stored in a retrieval system or
  > transmitted, in any form or by any means, electronic, mechanical,
  > photocopying, recording or otherwise, without prior permission.

- **<https://webbook.nist.gov/chemistry/fluid/>** (retrieved 2026-10-07), the
  generator that produced these tables:

  > © 2026 by the U.S. Secretary of Commerce on behalf of the United States of
  > America. All rights reserved. Copyright for NIST Standard Reference Data is
  > governed by the Standard Reference Data Act.

- **<https://www.nist.gov/open/license>** (retrieved 2026-10-07) — the contrast
  that makes the above binding rather than boilerplate. NIST grants a broad,
  royalty-free right to use and redistribute data created by NIST employees,
  and **excludes SRD from that grant**. The NIST name on a file is therefore
  evidence *against* a redistribution right, not for one.

- **15 U.S.C. § 290e**, the enabling statute:
  <https://www.law.cornell.edu/uscode/text/15/290e>.

### 3. Whether permission was found in repository records

**Conclusion: no record of permission was found.**

Searched: the working tree (`LICENSE`, `README.md`, every file under
`validation/thermal_properties/`, the test harness that reads the tables), and
the commit messages of every commit that touches either file — including
`00b80c2`, which added them both and whose message is a single line with no
licence or permission note.

**This is an absence of evidence, not evidence of absence.** Permission may
have been sought and granted off the record — by correspondence, or under an
arrangement not captured in version control. Nothing here establishes that
permission *cannot* exist or was never obtained; only that this repository does
not record it. **If you hold such a record, it resolves this question and
should be added to this file.**

Until then, the two files are treated as "redistribution not established".

---

## What was done, and what is still unresolved

### Withdrawn: `NIST_CO2_8MP.DAT`

No longer shipped. The decision was cheap and the cost measurable: **no test
or case uses it.** No input file anywhere under `tests/` sets
`ifluid= scp_co2`; the only references to the filename are the constant
`INPUT_SCP_CO2` in `src/modules.f90` and the `scp_co2` branch of `parse_ifluid`
in `src/input_general.f90`, both of which still work with a user-supplied file.
`tests/tools/run_fluid_property_tests.py` reads the water table only.

`.gitignore` ignores the path, and `tests/tools/check_validation_layout.py`
asserts both that rule and that the file is untracked — a `.gitignore` entry
does not cover a path that is already tracked, so the rule alone would not be
a guard.

What this does not do: an older copy of the same table, differing only by a
row-count header line and trailing whitespace, is in this repository's
**history** under `CHAPSim2.0_0pre/test_cases/`. It was removed from the tree
long before this change and is in no current branch. History is not rewritten,
so the table is out of the shipped tree but not out of the repository.

### Unresolved: `NIST_WATER_23.5MP.DAT`

**Still shipped, and its redistribution status is unresolved on exactly the
same evidence as the CO₂ table above.** It is not kept because its rights are
better; it is kept because withdrawing it here would achieve nothing while
breaking a great deal:

- 38 test inputs select `ifluid= scp_water`, and 26 case directories under
  `tests/` carry their own copy of the file because the solver reads it from
  the working directory.
- `tests/tools/run_fluid_property_tests.py` fails without it.
- The identical file is **already published** in this repository's history and
  in its current released branches, so deleting it from a new commit does not
  withdraw it from anyone.

Resolving it properly means either producing the permission record, or
switching to a generated table and refreshing the affected numerical baselines
— see below for why that is not a silent substitution. This is tracked as
follow-up work, not closed.

---

## Generating a table yourself

CoolProp (<https://github.com/CoolProp/CoolProp>, **MIT licence**) implements
the same underlying equations of state — IAPWS-95 for water, Span-Wagner for
CO₂ — and can produce both tables in the exact column order above, with no
redistribution question attached to the result.

```bash
pip install CoolProp

# the withdrawn CO2 table
python3 validation/thermal_properties/generate_property_table.py \
    --fluid CO2 --pressure 8.0e6 --tmin 220.0 --tmax 520.0 --tstep 0.2 \
    --output NIST_CO2_8MP.DAT

# the water table
python3 validation/thermal_properties/generate_property_table.py \
    --fluid Water --pressure 23.5e6 --tmin 573.15 --tmax 1073.15 --tstep 0.1 \
    --output NIST_WATER_23.5MP.DAT
```

How close the result is, measured against the files as shipped on 2026-10-07
with CoolProp 8.0.0, sampling every 200th row for water and every 50th for CO₂:

| Column | Water, max relative difference | CO₂, max relative difference |
|---|---|---|
| Enthalpy | 1.2e-09 | 2.8e-05 |
| Density | 3.6e-10 | 4.5e-05 |
| `cp` | 4.9e-09 | 4.3e-05 |
| `beta` | 5.7e-09 | 4.0e-05 |
| Viscosity | 6.0e-03 | 2.3e-02 |
| Thermal conductivity | 5.2e-02 | 3.4e-02 |

The thermodynamic columns agree to the precision each file was written at — for
water that is round-off, confirming both come from IAPWS-95; for CO₂ it is the
file's own 5-significant-figure truncation. The transport columns differ by up
to 5%, because CoolProp carries newer correlations than the vintage used here.

That last point also dates the shipped files. CoolProp's water viscosity at
573.15 K and 23.5 MPa is `9.12499878020E-05` Pa s — **digit for digit** what the
NIST WebBook returns for the same state today, and not what the shipped water
table contains (`9.11809415003E-05`). CoolProp and NIST agree with each other
now; the shipped table agrees with neither, which places it before the current
transport correlations were adopted.

**A generated table is a drop-in replacement thermodynamically, but it is not
numerically identical.** A 5% change in thermal conductivity moves every
supercritical case's thermal field, so swapping the water table requires an
approved baseline refresh, recorded the way `tests/reference_update_log_*.md`
records one — not a silent substitution.

---

## Citation

Cite NIST, not CHAPSim2, for any property values taken from these tables. The
citation is owed whatever the redistribution position turns out to be:

> Lemmon, E.W., Bell, I.H., Huber, M.L., McLinden, M.O., "Thermophysical
> Properties of Fluid Systems", in *NIST Chemistry WebBook, NIST Standard
> Reference Database Number 69*, Linstrom, P.J. and Mallard, W.G., eds.,
> National Institute of Standards and Technology, Gaithersburg MD,
> <https://doi.org/10.18434/T4D303>.

If you use a CoolProp-generated table instead, cite CoolProp as well:

> Bell, I.H., Wronski, J., Quoilin, S., Lemort, V., "Pure and Pseudo-pure Fluid
> Thermophysical Property Evaluation and the Open-Source Thermophysical
> Property Library CoolProp", *Ind. Eng. Chem. Res.* **53**(6) 2498–2508 (2014),
> <https://doi.org/10.1021/ie4033999>.
