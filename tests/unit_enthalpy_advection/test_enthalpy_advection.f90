!------------------------------------------------------------------------------
! Unit driver for the Koren face reconstruction of h (enthalpy_advection = koren),
! linked against the solver's own objects. It calls the solver routines
! Get_x_midp_C2P_koren_3D (index lines, uniform spacing) and
! Get_y_midp_C2P_koren_3D (mapped y lines) on single lines.
!
! usage: mpirun -np 1 test_enthalpy_advection faces
!   faces.dat : random lines (periodic, Dirichlet, interior ends; both flux signs,
!               zero flux, plateaus, tiny and huge differences) with the cd2 input
!               and the Koren output face values, for check_enthalpy_advection.py
! usage: mpirun -np 1 test_enthalpy_advection led
!   led.dat   : for random periodic lines and random-sign face mass fluxes, the
!               semi-discrete rate dh/dt = (R_H - h R_rho)/rho of every cell
!               (R from the same face fluxes, i.e. consistent continuity)
! usage: mpirun -np 1 test_enthalpy_advection run <scheme> <case> <N> <CFL> <tend> [beta]
!   1-D transport of rho*h and rho_hat with the solver's low-storage RK3
!   (gamma 8/15, 5/12, 3/4; zeta 0, -17/60, -5/12), h = (rho h)/rho_hat, and the
!   P2C divergence in the solver's form (F_{j+1} - F_j)/h_s * (ds/dy)_c [* 1/r_c].
!   scheme: cd2 | koren
!   case  : psine   periodic x, g = 1, h0 = sin(2 pi x)
!           pstep   periodic x, g = 1, h0 = square wave (0 / 1)
!           pvdsin  periodic x, g(x,t) = 1 + 0.5 sin(2 pi x) cos(2 pi t), rho_hat0 = 1, h0 = sin(2 pi x)
!           pvdstep as pvdsin with the square wave
!           pvdcon  as pvdsin with h0 = 1 (constant h under variable density)
!           ysine / ystep   mapped y in [0,1], m = +1, inflow Dirichlet at y = 0
!                           (exact value), outflow interior end (exact ghosts)
!           ysineneg / ystepneg  same with m = -1 (inflow at y = 1)
!           rsine / rstep   cylindrical annulus r = 1 + y, r g = m = 1 (divergence
!                           free), exact h0(sqrt(r^2 - 2 m t))
!   beta  : tanh stretching parameter of y(s) (0 = uniform), clustered at both ends
!   writes one summary line to stdout and the final profile to prof.dat
!------------------------------------------------------------------------------
module ea_run_mod
  use mpi_mod
  use parameters_constant_mod
  use udf_type_mod
  use operations
  implicit none
  ! state of task_run, shared with its helpers exact and rates
  type(t_domain) :: dm
  character(len = 16) :: ascheme, acase, arg
  integer :: n, np, nst, istep, k, j, u, ibc(2)
  real(WP) :: cfl, tend, beta, dt, t, tk, hs, msign, err1, err2, errinf, ex
  real(WP) :: hmin0, hmax0, smin, smax, fmin, fmax, mass0, mass, bflux, hcon, bst1, bst2
  logical  :: is_koren, is_periodic, is_cyl, is_stepp, is_vd, is_con
  real(WP), allocatable :: QH(:), QR(:), RH1(:), RH2(:), RR1(:), RR2(:), sc(:), sp(:), yc(:), yp(:), rc(:), rp(:), &
                           jac(:), vol(:), g(:), hf(:), fi(:, :, :), fo(:, :, :), g3(:, :, :), fbc(:, :, :), h(:)
  real(WP), parameter :: gam(3) = [8.0_WP / 15.0_WP, 5.0_WP / 12.0_WP, 0.75_WP]
  real(WP), parameter :: zet(3) = [ZERO, -17.0_WP / 60.0_WP, -5.0_WP / 12.0_WP]
  real(WP), parameter :: cst(3) = [ZERO, 8.0_WP / 15.0_WP, 2.0_WP / 3.0_WP]

contains
  !----------------------------------------------------------------------------
  real(WP) function urand()
    call random_number(urand)
  end function urand
  !----------------------------------------------------------------------------
  ! cd2 face values (the solver's cd2 midpoint, alpha = 0, a = 1, b = 0) on a
  ! line with the two ghost layers of buildup_ghost_cells_C, used as the input
  ! that the Koren routine keeps at non-interior ends
  subroutine cd2_faces(fi, fo, ibc, fbc)
    real(WP), intent(in)  :: fi(:)
    real(WP), intent(out) :: fo(:)
    integer,  intent(in)  :: ibc(2)
    real(WP), intent(in)  :: fbc(4)
    integer :: nc, np, i
    real(WP) :: gl, gr
    nc = size(fi); np = size(fo)
    if(ibc(1) == IBC_PERIODIC) then
      gl = fi(nc)
    else if(ibc(1) == IBC_DIRICHLET) then
      gl = TWO * fbc(1) - fi(1)
    else
      gl = fbc(1)
    end if
    if(ibc(2) == IBC_DIRICHLET) then
      gr = TWO * fbc(2) - fi(nc)
    else
      gr = fbc(2)
    end if
    fo(1) = HALF * (gl + fi(1))
    do i = 2, nc
      fo(i) = HALF * (fi(i - 1) + fi(i))
    end do
    if(np == nc + 1) fo(np) = HALF * (fi(nc) + gr)
    if(ibc(1) == IBC_DIRICHLET) fo(1) = fbc(1)
    if(ibc(2) == IBC_DIRICHLET .and. np == nc + 1) fo(np) = fbc(2)
  end subroutine cd2_faces
  !----------------------------------------------------------------------------
  subroutine task_faces()
    type(t_domain) :: dm
    integer, parameter :: nc = 9, ntrial = 6000
    real(WP) :: fi(nc, 1, 1), fbc(4, 1, 1), g(nc + 1, 1, 1), fo(nc + 1, 1, 1), fcd(nc + 1)
    integer :: itr, ibc(2), np, i, u, kind, mag
    real(WP) :: scale, base
    integer, allocatable :: seed(:)
    call random_seed(size = i); allocate(seed(i)); seed = 20261013; call random_seed(put = seed)
    dm%h = ONE
    open(newunit = u, file = 'faces.dat', status = 'replace', action = 'write')
    do itr = 1, ntrial
      kind = mod(itr - 1, 3)             ! 0 periodic, 1 Dirichlet, 2 interior
      select case(kind)
      case(0); ibc = IBC_PERIODIC;  np = nc
      case(1); ibc = IBC_DIRICHLET; np = nc + 1
      case default; ibc = IBC_INTERIOR; np = nc + 1
      end select
      mag = mod((itr - 1) / 3, 6)
      select case(mag)                   ! difference scales, incl. sub-normal-range steps
      case(0); scale = ONE
      case(1); scale = 1.0e-12_WP
      case(2); scale = 1.0e-300_WP
      case(3); scale = 1.0e+150_WP
      case(4); scale = ONE               ! plateaus: values repeated
      case default; scale = 1.0e-8_WP
      end select
      base = 1.0e3_WP * (urand() - HALF)
      if(mag == 2 .or. mag == 3) base = ZERO
      do i = 1, nc
        fi(i, 1, 1) = base + scale * (urand() - HALF)
        if(mag == 4 .and. urand() < 0.5_WP .and. i > 1) fi(i, 1, 1) = fi(i - 1, 1, 1)
      end do
      do i = 1, 4
        fbc(i, 1, 1) = base + scale * (urand() - HALF)
      end do
      do i = 1, np
        g(i, 1, 1) = urand() - HALF
        if(urand() < 0.05_WP) g(i, 1, 1) = ZERO
      end do
      call cd2_faces(fi(:, 1, 1), fcd(1:np), ibc, fbc(:, 1, 1))
      fo(1:np, 1, 1) = fcd(1:np)
      call Get_x_midp_C2P_koren_3D(fi, g(1:np, :, :), fo(1:np, :, :), dm, ibc, fbc)
      write(u, '(3I4)') ibc(1), ibc(2), np
      write(u, '(*(ES26.17E3))') fi(:, 1, 1)
      write(u, '(*(ES26.17E3))') fbc(:, 1, 1)
      write(u, '(*(ES26.17E3))') g(1:np, 1, 1)
      write(u, '(*(ES26.17E3))') fcd(1:np)
      write(u, '(*(ES26.17E3))') fo(1:np, 1, 1)
    end do
    close(u)
    write(*, *) 'faces written', ntrial
  end subroutine task_faces
  !----------------------------------------------------------------------------
  subroutine task_led()
    type(t_domain) :: dm
    integer, parameter :: nc = 64, ntrial = 2000
    real(WP) :: fi(nc, 1, 1), fbc(4, 1, 1), g(nc, 1, 1), fo(nc, 1, 1), rho(nc), rh, rr
    integer :: itr, i, ip, u, ibc(2)
    integer, allocatable :: seed(:)
    call random_seed(size = i); allocate(seed(i)); seed = 7; call random_seed(put = seed)
    dm%h = ONE / real(nc, WP)
    ibc = IBC_PERIODIC
    fbc = ZERO
    open(newunit = u, file = 'led.dat', status = 'replace', action = 'write')
    do itr = 1, ntrial
      do i = 1, nc
        fi(i, 1, 1) = urand()
        if(mod(itr, 2) == 0) fi(i, 1, 1) = sin(TWOPI * real(i, WP) / real(nc, WP) * (1 + mod(itr, 7))) + 0.01_WP * urand()
        g(i, 1, 1) = TWO * urand() - ONE
        rho(i) = HALF + urand()
      end do
      call cd2_faces(fi(:, 1, 1), fo(:, 1, 1), ibc, fbc(:, 1, 1))
      call Get_x_midp_C2P_koren_3D(fi, g, fo, dm, ibc, fbc)
      do i = 1, nc
        ip = mod(i, nc) + 1
        rh = - (g(ip, 1, 1) * fo(ip, 1, 1) - g(i, 1, 1) * fo(i, 1, 1)) / dm%h(1)
        rr = - (g(ip, 1, 1) - g(i, 1, 1)) / dm%h(1)
        ! cell i, its neighbours, and dh/dt for a state with rho*h = rho*fi
        write(u, '(4ES26.17E3)') fi(mod(i + nc - 2, nc) + 1, 1, 1), fi(i, 1, 1), fi(ip, 1, 1), &
          (rh - fi(i, 1, 1) * rr) / rho(i)
      end do
    end do
    close(u)
    write(*, *) 'led written', ntrial * nc
  end subroutine task_led
  !----------------------------------------------------------------------------
  ! mapping y(s), s in [0,1] (extended beyond for ghosts): uniform or tanh at both ends
  real(WP) function ymap(s, beta)
    real(WP), intent(in) :: s, beta
    if(beta <= ZERO) then
      ymap = s
    else
      ymap = HALF * (ONE + tanh(beta * (TWO * s - ONE)) / tanh(beta))
    end if
  end function ymap
  real(WP) function dsdy(s, beta)
    real(WP), intent(in) :: s, beta
    if(beta <= ZERO) then
      dsdy = ONE
    else
      dsdy = tanh(beta) / (beta * (ONE - tanh(beta * (TWO * s - ONE))**2))
    end if
  end function dsdy
  !----------------------------------------------------------------------------
  real(WP) function prof(x, istep)
    real(WP), intent(in) :: x
    logical,  intent(in) :: istep
    real(WP) :: xp
    if(istep) then
      xp = x - floor(x)
      prof = ZERO
      if(xp > 0.25_WP .and. xp <= 0.6_WP) prof = ONE
    else
      prof = sin(TWOPI * x)
    end if
  end function prof
  !----------------------------------------------------------------------------
  subroutine task_run()
    call get_command_argument(2, ascheme); call get_command_argument(3, acase)
    call get_command_argument(4, arg); read(arg, *) n
    call get_command_argument(5, arg); read(arg, *) cfl
    call get_command_argument(6, arg); read(arg, *) tend
    beta = ZERO
    if(command_argument_count() >= 7) then
      call get_command_argument(7, arg); read(arg, *) beta
    end if
    is_koren = (trim(ascheme) == 'koren')
    is_periodic = (acase(1:1) == 'p')
    is_cyl = (acase(1:1) == 'r')
    is_stepp = (index(acase, 'step') > 0)
    is_vd = (acase(1:3) == 'pvd')
    is_con = (trim(acase) == 'pvdcon')
    msign = ONE
    if(index(acase, 'neg') > 0) msign = -ONE

    if(is_periodic) then
      np = n
      ibc = IBC_PERIODIC
    else
      np = n + 1
      if(msign > ZERO) then
        ibc = [IBC_DIRICHLET, IBC_INTERIOR]
      else
        ibc = [IBC_INTERIOR, IBC_DIRICHLET]
      end if
    end if
    allocate(QH(n), QR(n), RH1(n), RH2(n), RR1(n), RR2(n), sc(n), sp(n + 1), yc(n), yp(n + 1), rc(n), rp(n + 1), &
             jac(n), vol(n), g(np), hf(np), h(n))
    hs = ONE / real(n, WP)
    do j = 1, n + 1
      sp(j) = real(j - 1, WP) * hs
      yp(j) = ymap(sp(j), beta)
    end do
    do j = 1, n
      sc(j) = (real(j, WP) - HALF) * hs
      yc(j) = ymap(sc(j), beta)
      jac(j) = dsdy(sc(j), beta)
    end do
    rc = ONE; rp = ONE
    if(is_cyl) then
      rc = ONE + yc; rp = ONE + yp
    end if
    vol = rc * hs / jac            ! cell measure consistent with the divergence form
    dm%h = hs
    dm%nc = n; dm%np = n + 1
    allocate(dm%yc(n), dm%yp(n + 1))
    dm%yc = yc; dm%yp = yp

    ! initial state
    do j = 1, n
      if(is_cyl) then
        h(j) = prof(rc(j), is_stepp)
      else
        h(j) = prof(yc(j), is_stepp)
      end if
      if(is_con) h(j) = ONE
    end do
    QR = ONE
    QH = QR * h
    hmin0 = minval(h); hmax0 = maxval(h)
    mass0 = sum(QH * vol)
    hcon = ZERO
    ! time step from the face velocity bound
    if(is_vd) then
      dt = cfl * hs / 1.5_WP
    else if(is_cyl) then
      dt = cfl * minval(yp(2:n + 1) - yp(1:n)) / ONE
    else
      dt = cfl * minval(yp(2:n + 1) - yp(1:n))
    end if
    nst = max(1, nint(tend / dt))
    dt = tend / real(nst, WP)

    smin = hmin0; smax = hmax0; fmin = hmin0; fmax = hmax0
    bflux = ZERO
    bst1 = ZERO
    RH1 = ZERO; RR1 = ZERO
    t = ZERO
    do istep = 1, nst
      do k = 1, 3
        tk = t + cst(k) * dt
        h = QH / QR
        call rates(tk, h, RH2, RR2, bst2)
        ! low-storage RK3: stage k uses the rate of stage k and of stage k-1
        QH = QH + dt * (gam(k) * RH2 + zet(k) * RH1)
        QR = QR + dt * (gam(k) * RR2 + zet(k) * RR1)
        bflux = bflux + dt * (gam(k) * bst2 + zet(k) * bst1)   ! net inflow of rho*h through the ends
        RH1 = RH2; RR1 = RR2; bst1 = bst2
        h = QH / QR
        smin = min(smin, minval(h)); smax = max(smax, maxval(h))
        if(is_con) hcon = max(hcon, maxval(abs(h - ONE)))
      end do
      t = t + dt
      fmin = min(fmin, minval(h)); fmax = max(fmax, maxval(h))
    end do
    mass = sum(QH * vol)

    ! errors against the exact solution (constant-density cases)
    err1 = ZERO; err2 = ZERO; errinf = ZERO
    do j = 1, n
      ex = exact(yc(j), rc(j), t)
      err1 = err1 + abs(h(j) - ex) * vol(j)
      err2 = err2 + (h(j) - ex)**2 * vol(j)
      errinf = max(errinf, abs(h(j) - ex))
    end do
    err1 = err1 / sum(vol); err2 = sqrt(err2 / sum(vol))
    open(newunit = u, file = 'prof.dat', status = 'replace', action = 'write')
    do j = 1, n
      write(u, '(3ES26.17E3)') yc(j), h(j), exact(yc(j), rc(j), t)
    end do
    close(u)
    write(*, '(A,1X,A,1X,A,I6,1X,ES12.4,I8,9ES24.15)') 'RUN', trim(ascheme), trim(acase), n, cfl, nst, &
      err1, err2, errinf, smin, smax, fmin, fmax, (mass - mass0 - bflux) / max(abs(mass0), ONE), hcon
  end subroutine task_run
    real(WP) function exact(y, r, tt)
      real(WP), intent(in) :: y, r, tt
      real(WP) :: r0
      if(is_vd) then
        exact = ZERO                    ! no closed form; errors not used
      else if(is_cyl) then
        r0 = r * r - TWO * msign * tt
        exact = prof(sqrt(max(r0, ZERO)), is_stepp)   ! inflow data continue the profile
      else
        exact = prof(y - msign * tt, is_stepp)
      end if
    end function exact
    !--------------------------------------------------------------------------
    ! RH = -div(m h_f), RR = -div(m), solver P2C form; ends from exact data
    subroutine rates(tt, hh, rh, rr, bst)
      real(WP), intent(in)  :: tt, hh(:)
      real(WP), intent(out) :: rh(:), rr(:)
      real(WP), intent(out) :: bst
      real(WP) :: fb(4), yg(4), x
      integer :: jj
      ! face mass flux (m = r g in the cylindrical case)
      do jj = 1, np
        if(is_vd) then
          x = real(jj - 1, WP) * hs
          g(jj) = ONE + HALF * sin(TWOPI * x) * cos(TWOPI * tt)
        else
          g(jj) = msign
        end if
      end do
      fb = ZERO
      if(.not. is_periodic) then
        ! inflow end: Dirichlet face value; outflow end: exact values at the two
        ! ghost centres (mapped s = 1 + hs/2, 1 + 3hs/2, or s = -hs/2, -3hs/2)
        if(msign > ZERO) then
          fb(1) = exact(ZERO, rp(1), tt)
          yg(2) = ymap(ONE + HALF * hs, beta); yg(4) = ymap(ONE + 1.5_WP * hs, beta)
          if(is_cyl) then
            fb(2) = exact(yg(2), ONE + yg(2), tt); fb(4) = exact(yg(4), ONE + yg(4), tt)
          else
            fb(2) = exact(yg(2), ONE, tt); fb(4) = exact(yg(4), ONE, tt)
          end if
        else
          fb(2) = exact(ONE, rp(np), tt)
          yg(1) = ymap(-HALF * hs, beta); yg(3) = ymap(-1.5_WP * hs, beta)
          if(is_cyl) then
            fb(1) = exact(yg(1), ONE + yg(1), tt); fb(3) = exact(yg(3), ONE + yg(3), tt)
          else
            fb(1) = exact(yg(1), ONE, tt); fb(3) = exact(yg(3), ONE, tt)
          end if
        end if
      end if
      call cd2_faces(hh, hf, ibc, fb)
      if(is_koren) then
        if(is_periodic) then
          allocate(fi(n, 1, 1), fo(np, 1, 1), g3(np, 1, 1), fbc(4, 1, 1))
          fi(:, 1, 1) = hh; fo(:, 1, 1) = hf; g3(:, 1, 1) = g; fbc(:, 1, 1) = fb
          call Get_x_midp_C2P_koren_3D(fi, g3, fo, dm, ibc, fbc)
          hf = fo(:, 1, 1)
        else
          allocate(fi(1, n, 1), fo(1, np, 1), g3(1, np, 1), fbc(1, 4, 1))
          fi(1, :, 1) = hh; fo(1, :, 1) = hf; g3(1, :, 1) = g; fbc(1, :, 1) = fb
          call Get_y_midp_C2P_koren_3D(fi, g3, fo, dm, ibc, fbc)
          hf = fo(1, :, 1)
        end if
        deallocate(fi, fo, g3, fbc)
      end if
      do jj = 1, n
        if(is_periodic) then
          rh(jj) = - (g(mod(jj, n) + 1) * hf(mod(jj, n) + 1) - g(jj) * hf(jj)) * jac(jj) / hs
          rr(jj) = - (g(mod(jj, n) + 1) - g(jj)) * jac(jj) / hs
        else
          rh(jj) = - (g(jj + 1) * hf(jj + 1) - g(jj) * hf(jj)) * jac(jj) / hs / rc(jj)
          rr(jj) = - (g(jj + 1) - g(jj)) * jac(jj) / hs / rc(jj)
        end if
      end do
      ! net inflow through the two ends (sum of rh * vol telescopes to this)
      bst = ZERO
      if(.not. is_periodic) bst = g(1) * hf(1) - g(np) * hf(np)
    end subroutine rates
end module ea_run_mod
program test_enthalpy_advection
  use mpi_mod
  use ea_run_mod
  implicit none
  character(len = 16) :: atask

  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, nrank, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierror)
  call get_command_argument(1, atask)
  select case(trim(atask))
  case('faces')
    call task_faces()
  case('led')
    call task_led()
  case('run')
    call task_run()
  case default
    write(*, *) 'unknown task ', trim(atask)
  end select
  call MPI_FINALIZE(ierror)
end program test_enthalpy_advection
