!> Unit driver for the statistical-window reconstruction (statistics_mod,
!> stats_window_mean), independent of the DNS solver.
!>
!> A synthetic, slowly drifting, correlated velocity record u_i(n) is folded
!> into running means with exactly the arithmetic of run_stats_action
!> (STATS_TAVG): A_n = ((n-1)/n) A_{n-1} + (1/n) f_n, n counted from
!> stat_istart+1. Snapshots of the running means are reconstructed into
!> interval means and checked against quadruple-precision direct sums of the
!> same samples.
!>
!>   Test A  interval means of u_i and u_i u_j, the Reynolds shear stress and
!>           normal stresses formed from the reconstructed raw moments, and the
!>           equivalence with the raw-sum form (N_b A_b - N_a A_a)/(N_b - N_a)
!>   Test B  window decomposition a < b < c: N_ac A_ac = N_ab A_ab + N_bc A_bc,
!>           and the count-weighted recombination of interval means
!>
!> Tolerance for a reconstructed raw moment (same basis as the solver's
!> realisability check): tol = 16 kappa N_b eps max(|A_a|, |A_b|), with
!> kappa = (N_a + N_b)/(N_b - N_a). Exits non-zero on any failure.
program test_stat_window
  use precision_mod, only : WP
  use statistics_mod, only : stats_window_mean
  implicit none
  integer, parameter :: QP = selected_real_kind(30)
  integer, parameter :: ISTART = 1000          ! stat_istart
  integer, parameter :: NT = 20000             ! last iteration
  integer, parameter :: NSNAP = 6
  integer, parameter :: snap(NSNAP) = [ISTART, 5000, 12000, 19990, 19999, NT]
  ! running means (solver arithmetic) and exact prefix sums, per snapshot:
  ! components 1-3 = u_i, 4-9 = u_i u_j (11, 12, 13, 22, 23, 33)
  real(WP) :: avg(9), avg_snap(9, NSNAP)
  real(QP) :: sum_q(9), sum_snap(9, NSNAP)
  real(WP) :: u(3), f(9), ac, am
  integer  :: n, nstat, k, isnap, nfail
  integer(8) :: seed

  seed = 20261018_8   ! in (0, 2^31 - 1)
  avg = 0.0_WP
  sum_q = 0.0_QP
  avg_snap = 0.0_WP
  sum_snap = 0.0_QP
  nstat = 0
  isnap = 1
  do n = 1, NT
    call sample_velocity(n, seed, u)
    if(n > ISTART) then
      f(1:3) = u(1:3)
      f(4) = u(1) * u(1); f(5) = u(1) * u(2); f(6) = u(1) * u(3)
      f(7) = u(2) * u(2); f(8) = u(2) * u(3); f(9) = u(3) * u(3)
      nstat = nstat + 1
      ! exactly run_stats_action, STATS_TAVG
      ac = 1.0_WP / real(nstat, WP)
      am = real(nstat - 1, WP) / real(nstat, WP)
      avg = am * avg + ac * f
      sum_q = sum_q + real(f, QP)
    end if
    if(isnap <= NSNAP) then
      if(n == snap(isnap)) then
        avg_snap(:, isnap) = avg
        sum_snap(:, isnap) = sum_q
        isnap = isnap + 1
      end if
    end if
  end do

  nfail = 0
  write(*, '(A)') 'Test A: interval means from cumulative running means'
  call check_window(1, 6)     ! a = stat_istart (empty origin) ... NT
  call check_window(2, 6)     ! long window
  call check_window(3, 6)     ! second half
  call check_window(4, 6)     ! short window late in the record (large kappa)
  call check_window(5, 6)     ! a single sample, kappa = 37997
  call check_origin_exact()
  write(*, '(A)') 'Test B: window decomposition a < b < c'
  call check_decomposition(2, 3, 6)
  call check_decomposition(1, 2, 3)
  call check_decomposition(3, 4, 6)

  if(nfail > 0) then
    write(*, '(A,I0,A)') 'test_stat_window: ', nfail, ' check(s) FAILED'
    error stop 1
  end if
  write(*, '(A)') 'test_stat_window: all checks PASSED'

contains

  !> Deterministic record: a mean flow drifting by 5 % over the record (a
  !> developing flow), fluctuations with <u'v'> < 0, all O(0.1) of the mean.
  subroutine sample_velocity(n, seed, u)
    integer,    intent(in)    :: n
    integer(8), intent(inout) :: seed
    real(WP),   intent(out)   :: u(3)
    real(WP) :: r(3), drift
    integer :: i

    do i = 1, 3
      ! Park-Miller minimal standard; the product stays below 2^46, no overflow
      seed = modulo(16807_8 * seed, 2147483647_8)
      r(i) = real(seed, WP) / 2147483647.0_WP - 0.5_WP
    end do
    drift = 0.05_WP * real(n, WP) / real(NT, WP)
    u(1) = 1.15_WP + drift + 0.10_WP * r(1)
    u(2) = 0.05_WP * r(2) - 0.04_WP * r(1)
    u(3) = 0.02_WP + 0.05_WP * r(3)
  end subroutine sample_velocity

  subroutine report(name, err, tol)
    character(*), intent(in) :: name
    real(WP), intent(in) :: err, tol
    character(4) :: verdict

    verdict = 'ok'
    if(.not. (err <= tol)) then
      verdict = 'FAIL'
      nfail = nfail + 1
    end if
    write(*, '(4X,A,T52,A,ES10.3,A,ES10.3,2X,A)') name, 'err ', err, '  tol ', tol, trim(verdict)
  end subroutine report

  subroutine check_window(ia, ib)
    integer, intent(in) :: ia, ib
    integer  :: na, nb, c
    real(WP) :: kappa, tol(9), aab(9), araw(9), err, rs, rs_exact, tol_rs
    real(QP) :: exact(9)
    character(64) :: tag

    na = snap(ia) - ISTART
    nb = snap(ib) - ISTART
    kappa = real(na + nb, WP) / real(nb - na, WP)
    write(tag, '(A,I0,A,I0,A)') '(', snap(ia), ', ', snap(ib), ']'
    write(*, '(2X,A,A,ES10.3)') trim(tag), '  kappa = ', kappa
    exact = (sum_snap(:, ib) - sum_snap(:, ia)) / real(nb - na, QP)
    do c = 1, 9
      aab(c) = stats_window_mean(avg_snap(c, ia), avg_snap(c, ib), na, nb)
      tol(c) = 16.0_WP * kappa * real(nb, WP) * epsilon(1.0_WP) * &
               max(abs(avg_snap(c, ia)), abs(avg_snap(c, ib)))
    end do
    ! raw moments against the direct average of the same samples
    err = maxval(abs(aab - real(exact, WP)) / tol)
    call report('raw moments <u_i>, <u_i u_j>  (max err/tol)', err, 1.0_WP)
    ! the raw-sum form of the formula gives the same window mean
    araw = (real(nb, WP) * avg_snap(:, ib) - real(na, WP) * avg_snap(:, ia)) / real(nb - na, WP)
    err = maxval(abs(aab - araw) / tol)
    call report('equivalent (N_b A_b - N_a A_a)/(N_b - N_a)', err, 1.0_WP)
    ! Reynolds shear stress u'v' = <uv> - <u><v> from the reconstructed moments
    rs = aab(5) - aab(1) * aab(2)
    rs_exact = real(exact(5) - exact(1) * exact(2), WP)
    tol_rs = tol(5) + abs(aab(2)) * tol(1) + abs(aab(1)) * tol(2)
    call report("<u'v'> from reconstructed moments", abs(rs - rs_exact), tol_rs)
    ! normal stresses
    rs = aab(4) - aab(1) * aab(1)
    rs_exact = real(exact(4) - exact(1) * exact(1), WP)
    tol_rs = tol(4) + 2.0_WP * abs(aab(1)) * tol(1)
    call report("<u'u'> from reconstructed moments", abs(rs - rs_exact), tol_rs)
    ! realisability, classified as in check_window_variance: a negative value
    ! within tau = 16 kappa N_b eps <ff> is round-off (a one-sample window has
    ! zero variance exactly, so it lands on either side of zero); beyond tau
    ! it is a failure
    call check_realisable("<u'u'>", aab(4), aab(1), kappa, nb)
    call check_realisable("<v'v'>", aab(7), aab(2), kappa, nb)
    call check_realisable("<w'w'>", aab(9), aab(3), kappa, nb)
  end subroutine check_window

  subroutine check_realisable(name, ff, f, kappa, nb)
    character(*), intent(in) :: name
    real(WP), intent(in) :: ff, f, kappa
    integer,  intent(in) :: nb
    real(WP) :: var, tau

    var = ff - f * f
    tau = 16.0_WP * kappa * real(nb, WP) * epsilon(1.0_WP) * abs(ff)
    if(var >= 0.0_WP) return
    if(var >= -tau) then
      write(*, '(4X,A,T52,A,ES10.3,A,ES10.3,2X,A)') trim(name)//' negative within round-off', &
        'var ', var, '  tau ', tau, 'ok'
    else
      write(*, '(4X,A,T52,A,ES10.3,A,ES10.3,2X,A)') trim(name)//' negative beyond round-off', &
        'var ', var, '  tau ', tau, 'FAIL'
      nfail = nfail + 1
    end if
  end subroutine check_realisable

  !> a = stat_istart: N_a = 0 and the window mean is the stored mean, bitwise.
  subroutine check_origin_exact()
    real(WP) :: aab(9)
    integer :: c

    do c = 1, 9
      aab(c) = stats_window_mean(avg_snap(c, 1), avg_snap(c, NSNAP), 0, NT - ISTART)
    end do
    if(all(aab == avg_snap(:, NSNAP))) then
      write(*, '(4X,A)') 'a = stat_istart returns the stored cumulative mean bitwise: ok'
    else
      write(*, '(4X,A)') 'a = stat_istart returns the stored cumulative mean bitwise: FAIL'
      nfail = nfail + 1
    end if
  end subroutine check_origin_exact

  subroutine check_decomposition(ia, ib, ic)
    integer, intent(in) :: ia, ib, ic
    integer  :: na, nb, nc, c
    real(WP) :: a_ab(9), a_bc(9), a_ac(9), tol(9), kappa_max, err
    character(64) :: tag

    na = snap(ia) - ISTART
    nb = snap(ib) - ISTART
    nc = snap(ic) - ISTART
    write(tag, '(A,I0,A,I0,A,I0)') 'a, b, c = ', snap(ia), ', ', snap(ib), ', ', snap(ic)
    write(*, '(2X,A)') trim(tag)
    kappa_max = max(real(na + nb, WP) / real(nb - na, WP), real(nb + nc, WP) / real(nc - nb, WP), &
                    real(na + nc, WP) / real(nc - na, WP))
    do c = 1, 9
      a_ab(c) = stats_window_mean(avg_snap(c, ia), avg_snap(c, ib), na, nb)
      a_bc(c) = stats_window_mean(avg_snap(c, ib), avg_snap(c, ic), nb, nc)
      a_ac(c) = stats_window_mean(avg_snap(c, ia), avg_snap(c, ic), na, nc)
      tol(c) = 48.0_WP * kappa_max * real(nc, WP) * epsilon(1.0_WP) * &
               maxval(abs(avg_snap(c, [ia, ib, ic])))
    end do
    ! window sums: S_ac = S_ab + S_bc
    err = maxval(abs(real(nc - na, WP) * a_ac - (real(nb - na, WP) * a_ab + real(nc - nb, WP) * a_bc)) &
                 / (real(nc - na, WP) * tol))
    call report('S_ac = S_ab + S_bc  (max err/tol)', err, 1.0_WP)
    ! count-weighted recombination of the interval means
    err = maxval(abs(a_ac - (real(nb - na, WP) * a_ab + real(nc - nb, WP) * a_bc) / real(nc - na, WP)) / tol)
    call report('A_ac = (N_ab A_ab + N_bc A_bc)/N_ac', err, 1.0_WP)
  end subroutine check_decomposition

end program test_stat_window
