# Channel Isothermal Periodic Validation

This case group contains channel-flow-specific post-processing and reference
comparison assets for isothermal periodic channel simulations.

## Run

The current runnable regression case remains:

```bash
cd tests/channel_iso_periodic
bash run_chapsim.sh
```

For the full regression entry point:

```bash
cd tests
bash run_regression.sh
```

## Post-Process

Case-specific scripts are under `post/`:

- `post/1_data/postprocess_channel_wall_units.py`
- `post/2_visu/postprocess_channel_wall_units.py`
- `post/2_visu/plot_channel_velo_stress.py`

Shared monitor and mesh-check scripts are under `validation/tools/scripts/`.

## References

Moser-Kim-Mansour channel reference profiles are stored under:

- `validation/references/channel/mkm/retau180/`
- `validation/references/channel/mkm/retau395/`
