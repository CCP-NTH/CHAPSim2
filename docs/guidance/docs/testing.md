# Regression and Smoke Tests

The `tests/` directory contains ready-to-run cases and helper scripts for quick
health checks and numerical regression checks. The `validation/` directory
contains the longer-term validation framework: case metadata, reference
databases, shared post-processing tools, and suite manifests.

Use smoke tests for rapid validation after build or input-generation changes. Use regression tests before merging solver, discretization, I/O, or physics modifications.

## Test Scripts

| Script | Purpose |
| --- | --- |
| `tests/run_smoke.sh` | Execute a 10-iteration subset of representative cases for rapid validation |
| `tests/run_regression.sh` | Run or validate standard/extended regression suites against reference metrics |
| `tests/run_functional.sh` | Run the behavioural `restart/` and `mesh_mapping/` functional sub-suites |
| `tests/regression/case_lists.sh` | The single definition of the standard, extended, and functional case lists, shared by the local and ARCHER2 runners |
| `tests/regression/update_reference.sh` | Replace `reference.json` files with newly generated metrics (use only after validated changes) |
| `tests/tools/check_metrics.py` | Compare `regression_test_metrics.json` against `reference.json` using specified tolerances |
| `tests/tools/tolerances.json` | Metric comparison tolerance specifications |
| `tests/tools/check_*.py` | Standalone static checkers over inputs, layout, and source wiring; take no arguments and need no build |

The top-level `tests/run_smoke.sh` and `tests/run_regression.sh` are thin wrappers that
exec the corresponding script in `tests/regression/`.

The suite manifests in `validation/suites/` document the same smoke,
standard-regression, and extended-regression case groups in a data-oriented
form for future CI and validation tooling. The shell scripts in `tests/`
remain the current execution interface.

## Static Checks

The standalone checkers in `tests/tools/` need no build and no solver run. They inspect
input files, directory layout, and source wiring, and are the cheapest thing to run after
editing inputs or test assets:

```bash
for f in tests/tools/check_*.py; do
  [ "$(basename "$f")" = check_metrics.py ] && continue
  python3 "$f" >/dev/null 2>&1 || echo "FAIL $f"
done
```

`check_metrics.py` is excluded because it is the metric comparator the runners invoke with
file paths, not a standalone check.

## Smoke Tests

From the `tests/` directory:

```bash
cd tests
bash run_smoke.sh
```

The smoke suite is designed to detect build failures, missing files, invalid input generation, and early runtime errors. It is not intended as a substitute for statistically converged production validation.

## Regression Tests

From the `tests/` directory, execute:

```bash
cd tests
bash run_regression.sh
```

The script supports two operational modes:

| Mode | Behavior |
| --- | --- |
| `run` | Execute each case, await `regression_test_metrics.json`, then validate metrics |
| `check` | Skip solver execution and validate existing metrics only |

Two test suites are available:

| Suite | Scope |
| --- | --- |
| `standard` | 12 cases: a compact default set for routine validation |
| `extended` | 23 cases, adding the remaining thermal and inlet/outlet combinations plus three non-default-order cases (`cd4`, `cp4`, `cp6`) |

With the extended suite you are also asked whether to include the functional tests, which
adds the metric-gated `LES_*` and `MHD_*` cases from `tests/functional/`.

Each prompt times out after ten seconds and takes its default. To run without any prompts,
set `CI=true` and select the suite through the environment:

```bash
cd tests
CI=true TEST_MODE=run REGRESSION_SUITE=standard ./run_regression.sh
CI=true TEST_MODE=run REGRESSION_SUITE=extended INCLUDE_FUNCTIONAL_TESTS=y ./run_regression.sh
```

Cases run on four MPI ranks by default; override with `NP`. As a rough guide on a recent
desktop workstation, the standard suite takes on the order of ten minutes and the extended
suite around a quarter of an hour.

## Reference Metrics

Each regression case should contain:

| File | Meaning |
| --- | --- |
| `input_chapsim.ini` | Case setup. |
| `run_chapsim.sh` | Case run wrapper. |
| `reference.json` | Accepted reference metrics. |
| `regression_test_metrics.json` | Metrics produced by the latest run. |

The comparison is performed by:

```bash
python3 tests/tools/check_metrics.py \
  tests/regression/<case>/regression_test_metrics.json \
  tests/regression/<case>/reference.json \
  tests/tools/tolerances.json
```

`tolerances.json` holds a global absolute and relative tolerance per metric key, plus
per-case overrides under `cases`. An override replaces the global entry outright, so
omitting `abs` drops the absolute check and leaves only the relative one. The `_comment_*`
blocks in the file explain why individual keys are gated the way they are; read them before
relying on a bare number.

## Updating References

Update references only after confirming that any numerical changes are intentional and physically validated.

```bash
cd tests/regression
bash update_reference.sh
```

This operation copies every available `regression_test_metrics.json` over its
`reference.json` — all cases at once, not a selected one. Review the resulting
version-control diff before committing.

Record the reasoning in a new `tests/reference_update_log_YYYY-MM-DD.md`, following the
existing files: what changed in the solver, which baselines moved and by how much, and why
any related case was left alone.

## Recommended Testing Policy

- **Input-generation tool modifications**: Execute smoke tests and at least the affected generated cases
- **I/O, restart, mesh, or boundary-condition modifications**: Execute smoke tests plus the standard regression suite
- **Numerical scheme or physics model modifications**: Execute the extended regression suite and inspect representative monitor histories
- **Reference metric updates**: Document the rationale explaining why new metrics are expected
