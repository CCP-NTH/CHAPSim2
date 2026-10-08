# Reference

Reference documentation is organized for quick lookup rather than sequential reading.

## User-Facing Reference

| Page | Use |
| --- | --- |
| [CHAPSim Input File Guide](input-file.md) | Variable meanings, Fortran types, IDs, and common setup rules for `input_chapsim.ini`. |
| [Benchmark and Validation Cases](benchmark-cases.md) | Case naming conventions and recommended starting cases. |
| [Restart I/O Modes](restart-io.md) | Restart field lists, metadata files, exact/compact history behavior, and recommended input combinations. |
| [Postprocessing and Output Data](postprocessing.md) | Output folders, statistics levels, monitor scripts, and visualisation scripts. |

## Repository File Map

| Path | Description |
| --- | --- |
| `src/` | Main Fortran solver source. |
| `bin/` | Compiled solver, `bin/CHAPSim`. |
| `lib/` | Bundled third-party libraries: 2decomp-fft and fishpack. |
| `tests/` | Smoke/regression/functional entry points and shared tools. |
| `tests/regression/` | Metric-gated cases, the case lists, and the reference-update script. |
| `tests/functional/` | Feature cases: LES, MHD, restart, mesh mapping, inlet database. |
| `tests/tools/` | Static checkers, `check_metrics.py`, and `tolerances.json`. |
| `validation/` | Validation cases, reference databases, shared post-processing tools, and suite manifests. |
| `prepost/input_generator/` | Python and shell tools for generating or modifying input files. |
| `prepost/job_submission/` | Local run wrapper and HPC submission scripts. |
| `prepost/mesh_reviewer/` | Interactive mesh-stretching inspection tool. |
| `docs/guidance/docs/` | Markdown source for user guidance. |
| `docs/guidance/html/` | Dependency-free static HTML preview generated from the Markdown source. |
| `docs/diagrams/` | Source diagrams used by documentation and architecture notes. |
| `docs/code_structure/` | Generated FORD code-structure documentation. |

## Code Structure Reference

The generated FORD documentation is separate from the user guide:

```text
docs/code_structure/index.html
```

From the documentation home page, choose **Code Structure** to browse modules,
procedures, derived types, and source files.
