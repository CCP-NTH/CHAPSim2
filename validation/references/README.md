# Reference Databases

Third-party DNS data used to validate CHAPSim2. None of it was produced by this
project. Cite the original authors, not CHAPSim2, when you publish a comparison
against any of it.

## Layout

Reference data is organized by geometry, source, and primary parameter:

```text
references/<geometry>/<source>/<parameter>/
```

Examples:

- `references/channel/mkm/retau180/`
- `references/pipe/tdl/retau550/`
- `references/pipe/eggels/reb5300/`

Keep reference databases separate from generated CHAPSim2 output. Generated
case output belongs in runtime folders such as `1_data/`, `2_visu/`,
`3_monitor/`, and `4_check/`.

## Provenance, citation and redistribution status

| Directory | Dataset | Persistent identifier | Licence |
|---|---|---|---|
| `channel/mkm/retau180/` | Plane channel DNS, Re_tau = 180 | none; served from the authors' file server | none stated |
| `channel/mkm/retau395/` | Plane channel DNS, Re_tau = 395 | none; served from the authors' file server | none stated |
| `pipe/tdl/retau180/` | Turbulent pipe flow at Re_tau = 180 | `doi:10.18738/T8/HLC3QY` | CC0 1.0 Universal |
| `pipe/tdl/retau550/` | Turbulent pipe flow at Re_tau = 550 | `doi:10.18738/T8/KO64GG` | CC0 1.0 Universal |
| `pipe/eggels/reb5300/` | Pipe DNS profiles, Re_b = 5300 | none established | none stated |

### `channel/mkm/` — Moser, Kim & Mansour

Statistics from three spectral DNS of plane channel flow at Re_tau = 180, 395
and 590. The two lower Reynolds numbers are bundled here. The files are the
`profiles/` subset of the authors' database, unmodified and byte-identical to
the copies served at

- <https://turbulence.oden.utexas.edu/MKM_1999.html> (landing page)
- `https://turbulence.oden.utexas.edu/data/MKM/chan180/profiles/`
- `https://turbulence.oden.utexas.edu/data/MKM/chan395/profiles/`

Cite:

> R. D. Moser, J. Kim and N. N. Mansour, "Direct numerical simulation of
> turbulent channel flow up to Re_tau = 590", *Physics of Fluids* **11**(4),
> 943–945 (1999). doi:10.1063/1.869966

The numerical method is that of Kim, Moin & Moser, *J. Fluid Mech.* **177**,
133–166 (1987). `retau180/README` is the authors' own file, carried over
unchanged; the data files repeat the attribution in their headers.

**Redistribution status.** The database is publicly downloadable and the site
asks that the paper be cited, but it states no licence and grants no explicit
redistribution permission. Public availability is not permission. Treat the
copies here as pending confirmation from the authors, whose contact details are
on the landing page. If you need a provenance guarantee, download from the file
server rather than relying on this copy.

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

**Redistribution status.** Both datasets are released under
[CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/legalcode),
recorded in their DataCite registrations. Redistribution and modification are
permitted without conditions; the citations above are a scholarly obligation,
not a licence term.

The repository notes "before publication, please check here for any updates to
the data". The DataCite records stood at version 3.0 when this file was
written; the archives bundled here were downloaded earlier and their version
was not recorded, so resolve the DOI before quoting these numbers in a paper.

### `pipe/eggels/reb5300/` — provenance not established

`dnsEggels5300.asc` holds 96 radial points of mean velocity, pressure, r.m.s.
velocities, resolved and viscous shear stress, triple products, turbulent
kinetic energy and dissipation. The radial resolution and the Reynolds number
match the pipe DNS of

> J. G. M. Eggels, F. Unger, M. H. Weiss, J. Westerweel, R. J. Adrian,
> R. Friedrich and F. T. M. Nieuwstadt, "Fully developed turbulent pipe flow: a
> comparison between direct numerical simulation and experiment",
> *J. Fluid Mech.* **268**, 175–210 (1994). doi:10.1017/S002211209400131X

**Redistribution status.** The file carries no attribution beyond its column
header, no licence, and no record of where it was obtained; no authoritative
landing page for it has been found. The attribution above is inferred from the
contents of the file, not read off a source. No CHAPSim2 tool reads this file,
so it is a candidate for removal: obtain the data from the authors or from the
paper rather than relying on this copy.
