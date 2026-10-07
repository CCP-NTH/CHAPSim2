# CHAPSim2 User Guide

CHAPSim2 is a high-fidelity Direct Numerical Simulation (DNS) solver for incompressible flow and heat transfer in canonical wall-bounded and related configurations. It supports isothermal flow, thermal and variable-property flow, and magnetohydrodynamic (MHD) cases in Cartesian and cylindrical geometries.

The user-facing workflow is:

1. Build the solver.
2. Select or generate an input configuration file using:
   - An existing case from `tests/regression/` or `tests/functional/`, or
   - Python tools in `prepost/input_generator/` for custom configurations.
3. Edit `input_chapsim.ini` to specify geometry, physics, mesh, and output requirements.
4. Execute the solver with MPI parallelization.
5. Monitor diagnostics: logs, CFL/mass conservation metrics, restart files, visualization, and statistics.

## Documentation Structure

- **Getting Started:** [Installation and First Run](installation.md).
- **Benchmark Cases:** [Benchmark and Validation Cases](benchmark-cases.md).
- **User Guide and How-To's:** [User Guide and How-To's](user-guide.md),
  including input files, mesh review, restart I/O, restart/interpolation,
  postprocessing, and tests.
- **Reference:** [Reference](reference.md) and
  [CHAPSim Input File Guide](input-file.md).
- **Methodology:** [Methodology](methodology.md) and the generated FORD
  code-structure reference.
- **Troubleshooting:** [Troubleshooting](troubleshooting.md).

## Recommended Reading Path

New users should:
1. Start with [Installation and First Run](installation.md)
2. Execute or copy a benchmark case
3. Edit `input_chapsim.ini` while referencing the [CHAPSim Input File Guide](input-file.md)
4. Use the [Mesh Stretching Reviewer](mesh-reviewer.md) to validate grid stretching
5. After the first successful run, inspect monitor histories and visualization output before interpreting statistics
6. Use [Restart I/O Modes](restart-io.md) when choosing exact or compact restart files
7. Use the [Mesh-Restart Workflow](mesh-restart.md) when refining or resizing grids with a developed flow field
8. Apply smoke/regression tests when modifying code or input-generation logic

## Repository Layout

- `src/`: Main Fortran solver source code
- `build/`, `obj/`: Build system and intermediate build output
- `bin/`: Compiled solver (`bin/CHAPSim`)
- `lib/`: Bundled third-party libraries (2decomp-fft, fishpack)
- `tests/`: `regression/` metric-gated cases, `functional/` feature cases, and `tools/`
- `validation/`: Validation cases, reference databases, shared post-processing tools, and suite manifests
- `examples/`: Case-specific example inputs
- `prepost/`: Input generation (`input_generator/`), job submission (`job_submission/`), and mesh review (`mesh_reviewer/`)
- `docs/`: User guidance, source diagrams, and generated code-structure documentation

The generated FORD code-structure reference is maintained separately under `docs/code_structure/`.
Source diagrams are maintained under `docs/diagrams/`.
