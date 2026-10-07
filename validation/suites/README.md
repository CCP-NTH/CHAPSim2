# Validation Suites

Suite manifests define named groups of cases for smoke, regression, extended
regression, and future CI workflows.

The current shell entry points remain:

- `tests/run_smoke.sh`
- `tests/run_regression.sh`

The YAML files here mirror those case groups in a data-oriented form so future
tools can consume the same suites without hard-coded shell arrays.
