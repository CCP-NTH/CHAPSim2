# Pipe Isothermal Periodic Validation

This case group contains pipe-flow-specific post-processing and reference
comparison assets for isothermal periodic pipe simulations.

## Run

The current runnable regression case remains:

```bash
cd tests/pipe_iso_periodic
bash run_chapsim.sh
```

For the full regression entry point:

```bash
cd tests
bash run_regression.sh
```

## Post-Process

Case-specific scripts are under `post/`:

- `post/2_visu/plot_pipe_velo_stress.py`
- `post/2_visu/plot_pipe_velo_stress_v2.py`

Shared monitor and mesh-check scripts are under `validation/tools/scripts/`.

## References

Pipe reference data is stored under:

- `validation/references/pipe/tdl/retau180/`
- `validation/references/pipe/tdl/retau550/`

Both are CC0 and shipped with the repository; cite the Texas Data Repository
DOIs listed in `validation/references/README.md`.
