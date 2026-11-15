#!/usr/bin/env python3
"""Reproducer: the pentadiagonal solvers must treat every (i,k) line independently.

inversion5_v1 / inversion5_v2 (src/poisson_1stderivcomp_fft2d.f90) eliminate, for
every spectral line, two independent real pentadiagonal systems (the real and the
imaginary parts). When a pivot is (numerically) zero the elimination multiplier must
be 0: the column below a zero pivot is already zero in the systems these routines
see (the singular zero-wavenumber row is identically zero), and any other multiplier
adds a multiple of the pivot row, whose superdiagonals are non-zero, to the rows
below. A multiplier left over from the previously processed line is never correct:
it couples independent systems and makes the result depend on the loop order and
hence on the MPI decomposition.

This script extracts the actual routine from the source, compiles it with small
stubs (gfortran), and solves two lines at once:
  line 1 - an ordinary diagonally dominant system (sets a non-zero multiplier),
  line 2 - the singular zero-wavenumber structure: zero first pivot, non-zero
           superdiagonals in that row, zero column below it.
For each routine it checks that line 2 solved together with line 1 equals line 2
solved alone, and that every line satisfies the rows it was not told to ignore.
Exit 0 if both routines pass.

usage: check_inversion5_zero_pivot.py [--routine inversion5_v1|inversion5_v2]
"""
import os
import re
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(HERE, "..", "..", "src", "poisson_1stderivcomp_fft2d.f90")

STUBS = r"""
module decomp_2d
  implicit none
  integer, parameter :: mytype = kind(1.0d0)
  type DECOMP_INFO
    integer :: yst(3), yen(3)
  end type
end module decomp_2d
module math_mod
  use decomp_2d, only : mytype
  implicit none
  interface abs_prec
    module procedure abs_dp
  end interface
contains
  elemental function abs_dp(r) result(d)
    real(mytype), intent(in) :: r
    real(mytype) :: d
    d = abs(r)
  end function
  elemental function cx(a, b) result(c)
    real(mytype), intent(in) :: a, b
    complex(mytype) :: c
    c = cmplx(a, b, kind=mytype)
  end function
  elemental function rl(c) result(a)
    complex(mytype), intent(in) :: c
    real(mytype) :: a
    a = real(c, kind=mytype)
  end function
  elemental function iy(c) result(a)
    complex(mytype), intent(in) :: c
    real(mytype) :: a
    a = aimag(c)
  end function
end module math_mod
module fft2decomp_interface_mod
  use decomp_2d, only : mytype
  use math_mod, only : abs_prec
  implicit none
  integer :: ny, nym
  real(mytype), parameter :: zero = 0.0_mytype, one = 1.0_mytype
  real(mytype), parameter :: MINP = 1.0e-16_mytype
end module fft2decomp_interface_mod
"""

DRIVER = r"""
program drive
  use decomp_2d
  use fft2decomp_interface_mod
  implicit none
  integer, parameter :: n = NROWS
  type(DECOMP_INFO) :: sp2, sp1
  complex(mytype) :: a2(1:2, n, 1:1, 5), e2(1:2, n, 1:1), a1(2:2, n, 1:1, 5), e1(2:2, n, 1:1)
  complex(mytype) :: a2keep(1:2, n, 1:1, 5), e2keep(1:2, n, 1:1)
  real(mytype) :: res, diff
  integer :: i, j, m
  ny = NYVAL; nym = n
  sp2%yst = [1, 1, 1]; sp2%yen = [2, n, 1]
  sp1%yst = [2, 1, 1]; sp1%yen = [2, n, 1]
  do j = 1, 2
    do m = 1, n
      a2(j, m, 1, 1) = cmplx(0.3d0 + 0.01d0*m*j, 0.2d0 + 0.02d0*m, kind=mytype)
      a2(j, m, 1, 2) = cmplx(1.1d0 + 0.03d0*m, 0.9d0 + 0.01d0*m*j, kind=mytype)
      a2(j, m, 1, 3) = cmplx(-8.0d0 - 0.1d0*m*j, -7.0d0 - 0.2d0*m, kind=mytype)
      a2(j, m, 1, 4) = cmplx(1.3d0 + 0.02d0*m*j, 1.2d0 + 0.03d0*m, kind=mytype)
      a2(j, m, 1, 5) = cmplx(0.4d0 + 0.01d0*m, 0.5d0 + 0.02d0*m*j, kind=mytype)
      e2(j, m, 1) = cmplx(sin(1.0d0*m + j), cos(0.5d0*m - j), kind=mytype)
    end do
    a2(j, 1, 1, 1:2) = 0; a2(j, 2, 1, 1) = 0
    a2(j, n-1, 1, 5) = 0; a2(j, n, 1, 4:5) = 0
  end do
  ! line 2: structure of the singular zero-wavenumber row of matrice_refinement
  ! (yky(1) = 0): zero diagonal, non-zero superdiagonals and right-hand side, and
  ! a zero column below the diagonal, so x(1) is free and rows 2..n determine
  ! x(2..n); the first row is the (dropped) compatibility equation
  a2(2, 1, 1, 3) = cmplx(0.0d0, 0.0d0, kind=mytype)
  a2(2, 2, 1, 2) = 0; a2(2, 3, 1, 1) = 0
  a2keep = a2; e2keep = e2
  a1(2, :, 1, :) = a2(2, :, 1, :); e1(2, :, 1) = e2(2, :, 1)
  call ROUTINE(a2, e2, sp2)
  call ROUTINE(a1, e1, sp1)
  diff = maxval(abs(e2(2, :, 1) - e1(2, :, 1)))
  ! residual of the original equations (rows 2..n for line 2, all rows for line 1)
  res = 0
  do j = 1, 2
    do m = merge(1, 2, j == 1), n
      call rowres(a2keep(j, m, 1, :), e2keep(j, m, 1), e2(j, :, 1), m)
    end do
  end do
  write(*, '(A,ES10.3,A,ES10.3)') 'line2 together-vs-alone max diff ', diff, '  max residual ', res
  if (diff > 1.0d-13 .or. res > 1.0d-12) then
    write(*, '(A)') 'FAIL'
  else
    write(*, '(A)') 'PASS'
  end if
contains
  subroutine rowres(row, rhs, x, m)
    complex(mytype), intent(in) :: row(5), rhs, x(:)
    integer, intent(in) :: m
    real(mytype) :: sr, si
    integer :: c, col
    sr = 0; si = 0
    do c = 1, 5
      col = m + c - 3
      if (col >= 1 .and. col <= n) then
        sr = sr + real(row(c)) * real(x(col))
        si = si + aimag(row(c)) * aimag(x(col))
      end if
    end do
    res = max(res, abs(sr - real(rhs)), abs(si - aimag(rhs)))
  end subroutine
end program drive
"""


def extract(name):
    lines = open(SRC).read().split("\n")
    s = next(i for i, l in enumerate(lines) if re.match(r"\s*subroutine\s+%s\s*\(" % name, l))
    e = next(i for i in range(s, len(lines)) if re.match(r"\s*end\s+subroutine\s+%s" % name, lines[i]))
    body = "\n".join(lines[s:e + 1])
    # strip OpenACC directives so the CPU path is what is tested
    return "\n".join(l for l in body.split("\n") if "!$acc" not in l)


def run(name, fc):
    n = 10
    # inversion5_v1 sizes its systems ny/2, inversion5_v2 nym
    nyval = 2 * n if name == "inversion5_v1" else n
    tmp = tempfile.mkdtemp(prefix="inv5_")
    try:
        code = STUBS + "\nmodule under_test\ncontains\n" + extract(name) + "\nend module under_test\n"
        drv = DRIVER.replace("NROWS", str(n)).replace("NYVAL", str(nyval)).replace("ROUTINE", name)
        drv = drv.replace("  use fft2decomp_interface_mod\n", "  use fft2decomp_interface_mod\n  use under_test\n", 1)
        with open(os.path.join(tmp, "t.F90"), "w") as fh:
            fh.write(code + "\n" + drv)
        cp = subprocess.run([fc, "-O0", "-cpp", "-DDOUBLE_PREC", "-ffree-line-length-none", "-finit-real=snan",
                             "t.F90", "-o", "t"], cwd=tmp, capture_output=True, text=True)
        if cp.returncode:
            print(cp.stderr[-2000:])
            return False
        out = subprocess.run(["./t"], cwd=tmp, capture_output=True, text=True).stdout
        print("%s: %s" % (name, out.strip().replace("\n", "  ")))
        return out.strip().endswith("PASS")
    finally:
        shutil.rmtree(tmp)


def main():
    fc = shutil.which("gfortran")
    if not fc:
        print("gfortran not found; skipped")
        return 0
    names = ["inversion5_v1", "inversion5_v2"]
    if "--routine" in sys.argv:
        names = [sys.argv[sys.argv.index("--routine") + 1]]
    ok = all([run(n, fc) for n in names])
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
