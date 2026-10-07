project: CHAPSim2
summary: A finite difference-based incompressible DNS solver with heat transfer for fluids with variable properties.
author: Wei Wang
author_description: Senior Computational Scientist, Scientific Computing Department, UKRI-STFC
email: wei.wang@stfc.ac.uk
github: https://github.com/CHAPSim/CHAPSim2
license: BSD-3-Clause
src_dir: ../../src
exclude_dir: ../../build
             ../../tests
             ../../bin
             ../../obj
             ../../prepost
             ../../validation
output_dir: ./doc
predocmark: >
docmark: !
display: public
         protected
         private
source: true
graph: false
search: true
creation_date: true
coloured_edges: true
show_proc_parent: true
warn: true
macro: VERSION=2.2.0

CHAPSim2 is a finite difference-based incompressible DNS solver with heat transfer for
fluids with variable properties. It uses a fully staggered (MAC) grid in Cartesian or
cylindrical coordinates, optional 2nd- to 6th-order compact and explicit schemes, and a
2-D pencil decomposition via 2decomp&FFT.

These pages are generated from the source annotations in `src/`. For installation, the
input-file reference, testing and workflow guidance, see the user guide under
`docs/guidance/`.

## Regenerating

    cd docs/code_structure
    ford ford.md

FORD empties `output_dir` on every run, so the output goes to the git-ignored `doc/`
subdirectory rather than into this directory. Copy `doc/` over the published copy in this
directory when the result is good.

FORD 7 reads these settings as python-markdown metadata: plain `key: value` lines at the
top of a Markdown project file, continuation lines indented, terminated by a blank line.
A bare `.yaml` project file is parsed as Markdown prose and every key in it is silently
ignored, which leaves `src_dir` at its default of `./src` — the stale source copy FORD
itself wrote into this directory on a previous run. That made the generated reference
re-document its own output instead of the solver. Keep this file in Markdown form.
