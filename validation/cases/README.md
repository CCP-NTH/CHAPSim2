# Validation Cases

Case directories are organized by geometry and flow class:

```text
cases/<geometry>/<physics_boundary>/
```

Examples:

- `cases/channel/iso_periodic/`
- `cases/pipe/iso_periodic/`

Each case directory should contain:

- `README.md` for run, post-processing, and validation notes.
- `case.yaml` for machine-readable metadata.
- `post/` for scripts that are specific to that case or geometry.

Runnable compact regression cases currently remain in `tests/`; `case.yaml`
links validation metadata back to the corresponding test case.
