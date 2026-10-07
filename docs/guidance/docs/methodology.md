# Methodology

This page gives a compact orientation to the numerical and modelling choices
that users need to understand when preparing a case. Detailed implementation
information is available in the generated code-structure documentation.

## Solver Scope

CHAPSim2 targets canonical DNS-style configurations, including:

- Cartesian channel and Taylor-Green vortex cases;
- cylindrical pipe and annular cases;
- isothermal flow;
- scalar/thermal cases with wall-temperature or wall-heat-flux boundary
  conditions;
- selected MHD configurations.

## Geometry and Topology

The primary geometry selector is `[domain] icase`:

| `icase` | Geometry |
| ---: | --- |
| `1` | Channel |
| `2` | Pipe |
| `3` | Annular |
| `4` | Taylor-Green vortex |

Pipe and annular cases use cylindrical-coordinate constraints internally.
Several domain values are reset by the input-reading logic for these cases, so
always check the run log for final interpreted values.

## MHD Model Scope

The MHD model uses the quasi-static approximation with an imposed magnetic
field. The user input `B_static = Bx,By,Bz` is interpreted as a global Cartesian
vector; cylindrical cases decompose that vector into local radial and azimuthal
components before forming the Lorentz force.

Electrical conductivity is currently treated as constant in the MHD
electric-potential solve. For the liquid-metal resistivity relation currently
under review, a 20% temperature increase of about 114 K corresponds to an
estimated electrical-conductivity decrease of about 6.9%. This gives a useful
scale for the constant-conductivity approximation, while temperature-dependent
conductivity remains a planned future extension.

## Mesh and Stretching

Wall-normal or radial resolution is controlled by the `[mesh]` section. Available options include:

- **No stretching**: uniform grid spacing
- **Geometric clustering**: centre, two-side, bottom-side, or top-side clustering
- **Spectral element stretching**: three-point finite-difference (3-point FD), hyperbolic tangent (tanh), or power-law methods

Use the [Mesh Stretching Reviewer](mesh-reviewer.md) to inspect a candidate mesh
before launching production runs.

## Boundary Conditions and Driving

Periodic wall-bounded cases typically employ either pressure-gradient driving or flow-rate control. Inlet/outlet cases should use external boundary conditions (database or prescribed inlet profiles) and avoid periodic driving to maintain consistent external forcing.

The input guide documents the boundary-condition IDs and common combinations:

[CHAPSim Input File Guide](input-file.md)

## Output and Statistics

CHAPSim2 separates restart/checkpoint data, visualisation output, monitor
histories, setup checks, and statistics. Statistics should normally begin only
after the transient has passed. Higher statistics levels are more expensive and
should be enabled only when those quantities are needed.

See [Postprocessing and Output Data](postprocessing.md) for the output workflow.

## Implementation Reference

For module-level details, browse:

```text
docs/code_structure/index.html
```

Important implementation areas include geometry setup, boundary conditions,
momentum and energy equations, restart I/O, monitor I/O, visualisation I/O, and
statistics.
