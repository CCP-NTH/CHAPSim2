# User Guide and How-To Workflows

This section documents practical workflows employed after solver compilation. The content is organized according to the typical case-development lifecycle.

## Case Preparation

- [CHAPSim Input File Guide](input-file.md): Complete reference for variable definitions, data types, identifiers, and validation rules for `input_chapsim.ini`
- [Mesh Stretching Reviewer](mesh-reviewer.md): Interactive visualization tool for y-direction mesh analysis, including stretching parameter optimization and clustering location selection

## Case Execution and Continuation

- [Restart I/O Modes](restart-io.md): Restart input switches, exact versus compact history behavior, and restart files written under isothermal, thermal, bundled, and per-field conditions
- [Mesh-Restart Interpolation](mesh-restart.md): Two-step workflow for field interpolation when restarting from a source case onto a target mesh with equivalent topology
- [Regression and Smoke Tests](testing.md): Rapid validation procedures, comprehensive regression suites, and reference-metric management

## Output and Analysis

- [Postprocessing and Output Data](postprocessing.md): Output folder organization, visualization workflows, monitor history analysis, statistics collection strategies, and reference plotting scripts
- [Troubleshooting](troubleshooting.md): Solutions for common issues including documentation access, missing dependencies, filesystem-related problems, restart configuration, and metric validation

## Recommended Workflow

1. Select a starting configuration from [Benchmark and Validation Cases](benchmark-cases.md)
2. Configure the case using the [CHAPSim Input File Guide](input-file.md)
3. Validate grid stretching with the [Mesh Stretching Reviewer](mesh-reviewer.md) before long simulations
4. Execute a short diagnostic test case and examine monitor diagnostics
5. Enable statistics collection only after flow development is complete
6. Apply regression testing when modifying core solver logic or input-generation procedures
