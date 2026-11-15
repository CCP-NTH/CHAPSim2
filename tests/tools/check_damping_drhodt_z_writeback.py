#!/usr/bin/env python3
"""Reproducer: damping_drhodt must write the z-outlet damping back to its argument.

damping_drhodt (src/tools_solver.f90) damps drho/dt towards a convective outlet. For a z
outlet it transposes the x-pencil argument to a z-pencil work array and multiplies that
work array by (1 - zdamping(k)). Unless the work array is transposed back into the
argument, the damping is lost and the z branch has no effect at all.

This check reads the routine and requires, after the z-pencil damping loop, a
transpose of the z-pencil array back into accc_xpencil. Exit 0 if present.
"""
import os
import re
import sys

src = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "src", "tools_solver.f90")
text = open(src).read()
m = re.search(r"subroutine damping_drhodt\b(.*?)end subroutine", text, re.S | re.I)
if not m:
    print("damping_drhodt not found"); sys.exit(2)
body = m.group(1)
z = body.find("is_conv_outlet(3)")
if z < 0:
    print("no z-outlet branch"); sys.exit(2)
zbranch = body[z:]
damped = re.search(r"accc_zpencil\s*\(.*?\)\s*=\s*accc_zpencil", zbranch, re.S)
back = re.search(r"call\s+transpose_from_z_pencil\s*\(\s*accc_zpencil\s*,\s*accc_xpencil", zbranch, re.I)
if damped and back and back.start() > damped.start():
    print("PASS: the z-outlet damping is transposed back into accc_xpencil")
    sys.exit(0)
print("FAIL: the z branch damps a z-pencil work array that is never written back; "
      "the z-outlet damping has no effect")
sys.exit(1)
