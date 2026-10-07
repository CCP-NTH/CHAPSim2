# Reference Baseline Refresh - 2026-08-13

This log records a deliberate refresh of CHAPSim2 test reference data.

## Reason

The previous reference data were generated with an old solver version, likely from April 2026. A full extended regression run on 2026-08-13 completed all solver executions successfully, but many cases failed metrics comparison against those old references.

Pre-refresh summary from the extended regression run:

```text
MODE   : run
PASSED :   6 /  24
FAILED :  18 /  24
  SOLVER :   0
  METRICS:  18
  OTHER  :   0
SKIPPED:   0 /  24
```

The failures were therefore treated as stale-reference mismatches rather than solver runtime failures.

## Baseline Source

The new references were copied from the existing generated `regression_test_metrics.json` files after the user manually completed:

1. Running the relevant regression and functional test cases with the current code.
2. Checking that solver execution completed and generated metrics.

Codex started from step 3 only: copying generated metrics into reference files and recording this log.

## Environment

- Date/time: 2026-08-13 16:42:22 BST
- Git commit at refresh time: `2cf9507`
- Repository: `/Users/wei.wang/Work_RSDevelopment/1_CHAPSim/CHAPSim2`

## Commands

The refresh copied every generated metrics file under `tests/regression` and `tests/functional` to the adjacent `reference.json`:

```bash
while IFS= read -r metrics; do
  ref="${metrics%/regression_test_metrics.json}/reference.json"
  cp -f "$metrics" "$ref"
done < <(find tests/regression tests/functional -name regression_test_metrics.json | sort)
```

Validation checks:

```bash
rg -n "NaN|nan|Inf|inf|Infinity|infinity" tests/regression tests/functional -g regression_test_metrics.json
find tests/regression tests/functional -name reference.json | sort
```

## Refresh Scope

- Generated metrics files used: 39
- Reference files after refresh: 39
- Existing reference files updated: 31
- New reference files created: 8

New reference files created:

```text
tests/functional/inlet_database/diff_yz_mesh/reference.json
tests/functional/inlet_database/same_yz_mesh/reference.json
tests/functional/mesh_mapping/channel_geo_clamp/step1_gen_bin_for_new_mesh/reference.json
tests/functional/mesh_mapping/channel_geo_extend/step1_gen_bin_for_new_mesh/reference.json
tests/functional/restart/channel_scp_inout/run_continuous/reference.json
tests/functional/restart/pipe_scp_inout/run_continuous/reference.json
tests/functional/restart/tgv_iso/run_restart/reference.json
tests/regression/pipe_scp_periodic/reference.json
```

## Caveat

These refreshed references define the current solver output baseline. They are not an independent physics validation. Future regression failures should be interpreted relative to this refreshed 2026-08-13 baseline.
