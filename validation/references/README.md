# Reference Databases

Third-party DNS data used to validate CHAPSim2. None of it was produced by this
project. Cite the original authors, not CHAPSim2, when you publish a comparison
against any of it.

**Only data whose redistribution terms are explicit is shipped with CHAPSim2.**
Everything else is described below with its authoritative source and the exact
commands to fetch it, so that you obtain it under the terms its own authors set
rather than under ours. Public availability is not permission to redistribute.

## Layout

Reference data is organized by geometry, source, and primary parameter:

```text
references/<geometry>/<source>/<parameter>/
```

Examples:

- `references/pipe/tdl/retau550/` — shipped with the repository
- `references/channel/mkm/retau180/` — you create this directory yourself

Keep reference databases separate from generated CHAPSim2 output. Generated
case output belongs in runtime folders such as `1_data/`, `2_visu/`,
`3_monitor/`, and `4_check/`.

The two directories you populate yourself are listed in `.gitignore`, so a
local download cannot be committed back and redistributed by accident.

## Provenance, citation and redistribution status

| Directory | Dataset | Persistent identifier | Licence | Shipped here |
|---|---|---|---|---|
| `pipe/tdl/retau180/` | Turbulent pipe flow at Re_tau = 180 | `doi:10.18738/T8/HLC3QY` | CC0 1.0 Universal | yes |
| `pipe/tdl/retau550/` | Turbulent pipe flow at Re_tau = 550 | `doi:10.18738/T8/KO64GG` | CC0 1.0 Universal | yes |
| `channel/mkm/retau180/` | Plane channel DNS, Re_tau = 180 | none; served from the authors' file server | none stated | no — download |
| `channel/mkm/retau395/` | Plane channel DNS, Re_tau = 395 | none; served from the authors' file server | none stated | no — download |
| `pipe/eggels/reb5300/` | Pipe DNS profiles, Re_b = 5300 | none established | none stated | no — withdrawn |

## Shipped data

### `pipe/tdl/` — Yao, Texas Data Repository

Mean profiles, r.m.s. profiles, Reynolds-stress transport budgets, vorticity
and pressure fluctuations, and energy spectra from DNS of fully developed
turbulent pipe flow. `tdl` is the Texas Digital Library, which operates the
repository; it is not an author initialism. Each directory holds the
`dataverse_files.zip` produced by the repository's whole-dataset download, its
`MANIFEST.TXT`, and the HDF5 spectra file unpacked alongside it — the `.h5` is
also inside the zip, so it is stored twice.

Cite:

> Yao, Jie, 2020, "Turbulent Pipe flow at Re_tau=180", Texas Data Repository,
> <https://doi.org/10.18738/T8/HLC3QY>

> Yao, Jie, 2022, "Turbulent Pipe Flow at Re_tau=550", Texas Data Repository,
> <https://doi.org/10.18738/T8/KO64GG>

**Redistribution status — licence verified.** Both datasets are released under
[CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/legalcode),
read from their DataCite registrations (`https://api.datacite.org/dois/<doi>`).
Redistribution and modification are permitted without conditions; the citations
above are a scholarly obligation, not a licence term.

**Upstream byte equality — not verified.** The copies here are believed
unmodified, but that has not been checked against the repository: the Texas
Data Repository landing pages were not reachable from the machine where this
file was written, so no upstream download was available to compare against.
Treat "unmodified" as an assertion, not a verified fact — distinct from the
licence above, which was verified. The repository also notes "before
publication, please check here for any updates to the data"; the DataCite
records stood at version 3.0 when this file was written, and the version of the
archives shipped here was not recorded, so resolve the DOI before quoting these
numbers in a paper.

## Datasets you download yourself

### `channel/mkm/` — Moser, Kim & Mansour

Statistics from three spectral DNS of plane channel flow at Re_tau = 180, 395
and 590. CHAPSim2's channel comparison script uses the two lower Reynolds
numbers.

**Why it is not shipped.** The database is publicly downloadable and the site
asks that the paper be cited, but it states no licence and grants no explicit
redistribution permission. Public availability is not permission, so CHAPSim2
does not redistribute it. Download it from the authors' file server, which also
gives you a provenance guarantee this repository could not.

Landing page: <https://turbulence.oden.utexas.edu/MKM_1999.html>

From the repository root:

```bash
base=https://turbulence.oden.utexas.edu/data/MKM
for retau in 180 395; do
  mkdir -p validation/references/channel/mkm/retau${retau}
  for ext in means reystress flat skew velp vortvar; do
    curl -fsSL -o validation/references/channel/mkm/retau${retau}/chan${retau}.${ext} \
      ${base}/chan${retau}/profiles/chan${retau}.${ext}
  done
done
curl -fsSL -o validation/references/channel/mkm/retau180/README \
  ${base}/chan180/profiles/README
```

`chan*.means` and `chan*.reystress` are the only two files
`validation/cases/channel/iso_periodic/post/2_visu/plot_channel_velo_stress.py`
reads; the rest are fetched because they complete the authors' `profiles/`
subset. The script also accepts the authors' own `MKM<retau>_profiles/` folder
naming, so an existing local copy can be pointed at with `--ref-dir` instead.

Cite:

> R. D. Moser, J. Kim and N. N. Mansour, "Direct numerical simulation of
> turbulent channel flow up to Re_tau = 590", *Physics of Fluids* **11**(4),
> 943–945 (1999). doi:10.1063/1.869966

The numerical method is that of Kim, Moin & Moser, *J. Fluid Mech.* **177**,
133–166 (1987); the data files repeat the attribution in their headers.

### `pipe/eggels/reb5300/` — withdrawn, provenance not established

A file `dnsEggels5300.asc` holding 96 radial points of mean velocity, pressure,
r.m.s. velocities, resolved and viscous shear stress, triple products,
turbulent kinetic energy and dissipation was previously carried here. It
carried no attribution beyond its column header, no licence, and no record of
where it was obtained, and no authoritative landing page for it was found. It
has therefore been removed rather than republished under a guess.

Nothing in CHAPSim2 read it. The radial resolution and Reynolds number matched
the pipe DNS of

> J. G. M. Eggels, F. Unger, M. H. Weiss, J. Westerweel, R. J. Adrian,
> R. Friedrich and F. T. M. Nieuwstadt, "Fully developed turbulent pipe flow: a
> comparison between direct numerical simulation and experiment",
> *J. Fluid Mech.* **268**, 175–210 (1994). doi:10.1017/S002211209400131X

but that attribution was inferred from the contents of the file, not read off a
source, and should not be relied on. Obtain pipe reference data from the
`pipe/tdl/` datasets above, or the digitised profiles from the paper itself.
