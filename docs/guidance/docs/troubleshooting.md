# Troubleshooting

This page provides solutions for common setup, execution, and post-processing issues encountered during CHAPSim2 usage. Consult this section before modifying solver parameters.

## Documentation Opens as Raw Markdown

If Chrome displays raw Markdown text instead of formatted HTML, access the compiled HTML documentation from the repository root:

```bash
google-chrome docs/guidance/html/index.html
```

Then choose **User Guidance**. The browser-facing pages are under:

```text
docs/guidance/html/
```

The Markdown source files are still kept under:

```text
docs/guidance/docs/
```

To regenerate the local HTML preview after editing source Markdown:

```bash
python3 docs/guidance/build_static.py
```

This procedure does not require MkDocs.

## MkDocs Is Not Installed

MkDocs is optional. A dependency-free static HTML builder is available for local browser preview. Employ MkDocs only when the complete Material theme documentation site is required.

## Figure Saving Fails on a Mounted Filesystem

On remote or mounted filesystems, Matplotlib may fail with an error such as:

```text
OSError: [Errno 5] Input/output error
```

A robust workflow is:

- save plots first to a local temporary directory, for example `/tmp`;
- verify the PNG was completed;
- copy the completed file back to the mounted case directory when the filesystem
  is stable.

For long postprocessing batches, this avoids losing all later figures after one
filesystem write failure.

## Restart or Interpolation Case Fails Early

Check these items before changing numerical parameters:

- Source and target cases use the same geometry family and topology.
- `input_chapsim_tgt.ini` contains the intended target mesh and domain values.
- Target `1_data/` contains `domain1_*` files.
- The target `input_chapsim.ini` has `[process] is_prerun = .false.`.
- The initial CFL is conservative for the first target restart.
- Boundary-condition and physics settings are consistent with the source field.

## Visualisation Files Are Missing

Check the input-file output settings:

- `visu_idim` selects full-domain or plane output mode.
- `visu_nfre` controls visualisation write frequency.
- `visu_nskip` controls skipped records or stride behavior.
- The run length is long enough to reach a visualisation write step.

Also inspect the run log for disabled postprocessing or I/O errors.

## Regression Metrics Are Missing

For regression cases, `regression_test_metrics.json` should be produced by the
case run. If it is absent:

- confirm the case was run with the regression wrapper;
- inspect the solver log for early termination;
- check that `run_chapsim.sh` exists and is executable;
- verify the run finished before the metric timeout in `tests/run_regression.sh`.

Use metrics-only mode only when `regression_test_metrics.json` already exists.
