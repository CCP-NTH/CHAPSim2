# CHAPSim2 Validation Framework

This directory contains validation assets that are shared across CHAPSim2
cases. It is separate from `tests/`, which remains the automated regression
entry point.

## Layout

| Path | Purpose |
| --- | --- |
| `cases/` | Case-specific validation metadata and post-processing scripts. |
| `references/` | External/reference databases used for profile and figure comparison. |
| `tools/scripts/` | Framework-wide scripts that should not be copied into every case. |
| `suites/` | Smoke, regression, and extended suite definitions. |

## Relationship to `tests/`

The existing `tests/` directory still owns the runnable regression cases and
the `run_smoke.sh` / `run_regression.sh` entry points. The suite files in this
directory document the same case groups in a data-oriented form so future CI
and validation tooling can consume them without hard-coded shell arrays.

## Case Convention

Each validation case should include:

- `README.md` explaining how to run, post-process, and validate the case.
- `case.yaml` describing geometry, physics, references, regression mapping,
  and expected checks.
- `post/` for case-specific scripts.

Runtime output such as `1_data/`, `2_visu/`, `3_monitor/`, and `4_check/`
should remain generated case output, not reference data.
