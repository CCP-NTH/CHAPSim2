# CHAPSim2 FORD Reference Documentation

This directory contains the FORD-generated Fortran code-structure reference and
the configuration used to regenerate it.

## Configuration

Use the single maintained FORD configuration file:

```text
docs/code_structure/ford.md
```

It must stay in Markdown form. FORD 7 reads its settings as python-markdown
metadata — plain `key: value` lines at the top of a Markdown project file,
continuation lines indented, terminated by a blank line. A `.yaml` project file
is parsed as Markdown prose instead, every key in it is silently ignored, and
FORD falls back to its default `src_dir` of `./src`. In this directory that
default is the source copy FORD itself wrote on a previous run, so the generated
reference re-documents its own stale output rather than the solver.

## Generate the Reference

```bash
cd docs/code_structure
ford ford.md
```

FORD empties its output directory on every run, so the configuration sends the
output to `doc/`, which is git-ignored scratch. Review `doc/index.html`, then
copy the generated entries from `doc/` over the published copies in
`docs/code_structure/`.

`docs/code_structure/` is the only maintained location for the published
code-structure reference. Do not keep a root-level `code_structure/` directory.

After regenerating, check that `docs/code_structure/src/` lists the same files
as `src/`. If it does not, FORD read the wrong source directory and the output
must be discarded.

## FORD Comment Style

Use FORD documentation comments directly before the entity they describe:

```fortran
!> Short one-line summary.
!>
!> Longer explanation of purpose, assumptions, and workflow.
!> - dm (in): Domain descriptor.
!> - fl (inout): Flow state.
subroutine example_routine(fl, dm)
```

Recommended practice:

- The maintained marker configuration is `predocmark: ">"` and
  `docmark: "!"`, which means FORD recognises leading `!>` blocks and
  continuation `!!` comments.
- The alternate markers are intentionally empty because CHAPSim2 does not use a
  second documentation-comment style.
- Put a short `!> ...` summary block before each public module, derived type,
  subroutine, and function.
- Use simple Markdown bullets such as `!> - dm (in): Domain descriptor.` for
  important arguments.
- Keep ordinary implementation notes as normal `!` comments inside routines.
- Avoid long banner comments as the only documentation; FORD renders structured
  `!>` blocks much more clearly.

## View the Reference

Open the generated HTML file in a browser, for example:

```bash
google-chrome docs/code_structure/index.html
```
