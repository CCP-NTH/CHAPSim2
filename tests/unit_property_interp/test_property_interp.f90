!------------------------------------------------------------------------------
! Unit driver for the property-table interpolation (thermo_info_mod), linked
! against the solver's own objects. Loads a table through the solver path
! (Buildup_thermo_mapping_relations) and writes, in the selected mode:
!   nodes_<tag>.dat  : the non-dimensional table nodes (h, T, rho, mu, k, sigma_e, beta, cp)
!   dense_<tag>.dat  : from_H on 64 points per interval plus +-1e-5 and +-1e-3 (of the 2-interval width) of each
!                      node (h, T, rho, mu, k, sigma_e, beta, cp)
!   trip_<tag>.dat   : round trips h -> T(h) -> h(T) and T -> h(T) -> T(h), and the
!                      Bc path from_DH(rho_hat h, rho_hat) vs from_H(h)
! or, with task 'oor', evaluates one state outside the table and calls the
! collective check (expected to stop the run with the existing diagnostic).
! or, with task 'bench', times 2e7 from_DH evaluations at pseudo-random states in the
! table range (property-evaluation cost only; serial).
! usage: mpirun -np 1 test_property_interp <linear|pchip> <co2|water> <dense|oor_h|oor_t|bench>
!------------------------------------------------------------------------------
program test_property_interp
  use mpi_mod
  use parameters_constant_mod
  use udf_type_mod
  use thermo_info_mod
  implicit none
  type(t_thermo) :: tm
  type(t_fluidThermoProperty) :: f, g
  type(t_fluidThermoProperty) :: a3(1, 1, 1)   ! from_T is reached through its public 3-D wrapper
  character(len = 16) :: amode, afluid, atask
  character(len = 64) :: tag
  integer :: n, i, j, u
  real(WP) :: s, hh, rhat, t0w, t1w, acc
  integer(8) :: nb, ib
  real(WP), parameter :: soff(4) = [1.0e-5_WP, 1.0e-3_WP, -1.0e-5_WP, -1.0e-3_WP]

  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, nrank, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierror)
  call get_command_argument(1, amode); call get_command_argument(2, afluid); call get_command_argument(3, atask)
  iprop_interp = IPROP_INTERP_LINEAR
  if(trim(amode) == 'pchip') iprop_interp = IPROP_INTERP_PCHIP
  is_bc_recovery = .true.
  tm%ifluid = ISCP_CO2
  tm%ref_T0 = 301.15_WP; tm%init_T0 = 301.15_WP
  if(trim(afluid) == 'water') then
    tm%ifluid = ISCP_WATER; tm%ref_T0 = 645.15_WP; tm%init_T0 = 645.15_WP
  end if
  call Buildup_thermo_mapping_relations(tm)
  n = fluidparam%nlist
  tag = trim(afluid)//'_'//trim(amode)

  if(trim(atask) == 'oor_h') then
    f%h = ftplist(n)%h + 1.0e-3_WP * abs(ftplist(n)%h - ftplist(n-1)%h); f%d = ONE; f%rhoh = f%h
    call ftp_refresh_thermal_properties_from_DH(f)
    call check_property_range(0, ZERO, 'unit test oor_h')
    write(*, *) 'NOT STOPPED'
    call MPI_FINALIZE(ierror); stop
  end if
  if(trim(atask) == 'oor_t') then
    f%t = ftplist(1)%t - 1.0e-3_WP * abs(ftplist(2)%t - ftplist(1)%t)
    a3(1,1,1) = f; call ftp_refresh_thermal_properties_from_T_undim_3Dftp(a3); f = a3(1,1,1)
    call check_property_range(0, ZERO, 'unit test oor_t')
    write(*, *) 'NOT STOPPED'
    call MPI_FINALIZE(ierror); stop
  end if

  if(trim(atask) == 'bench') then
    ! Bc path as used per cell: h = rhoh/rho_hat, then the table lookup. A fixed
    ! low-discrepancy sequence keeps both modes on identical states.
    nb = 20000000_8; acc = ZERO
    t0w = MPI_WTIME()
    do ib = 1, nb
      s = modulo(real(ib, WP) * 0.6180339887498949_WP, ONE)
      f%d = ONE; f%rhoh = ftplist(1)%h + s * (ftplist(n)%h - ftplist(1)%h)
      call ftp_refresh_thermal_properties_from_DH(f)
      acc = acc + f%d + f%t + f%m + f%k
    end do
    t1w = MPI_WTIME()
    write(*, '(A, A, ES12.4, A, ES14.6)') trim(tag), ': ns per from_DH evaluation ', (t1w - t0w) / real(nb, WP) * 1.0e9_WP, &
      '   checksum ', acc
    call MPI_FINALIZE(ierror); stop
  end if

  open(newunit = u, file = 'nodes_'//trim(tag)//'.dat', status = 'replace')
  do i = 1, n
    write(u, '(8ES26.17)') ftplist(i)%h, ftplist(i)%t, ftplist(i)%d, ftplist(i)%m, ftplist(i)%k, &
                           ftplist(i)%sigma_e, ftplist(i)%b, ftplist(i)%cp
  end do
  close(u)

  open(newunit = u, file = 'dense_'//trim(tag)//'.dat', status = 'replace')
  do i = 1, n - 1
    do j = 0, 63
      s = real(j, WP) / 64.0_WP
      f%h = ftplist(i)%h + s * (ftplist(i+1)%h - ftplist(i)%h)
      call ftp_refresh_thermal_properties_from_H(f)
      write(u, '(8ES26.17)') f%h, f%t, f%d, f%m, f%k, f%sigma_e, f%b, f%cp
    end do
    if(i > 1) then
      do j = 1, 4
        f%h = ftplist(i)%h + soff(j) * (ftplist(i+1)%h - ftplist(i-1)%h)
        call ftp_refresh_thermal_properties_from_H(f)
        write(u, '(8ES26.17)') f%h, f%t, f%d, f%m, f%k, f%sigma_e, f%b, f%cp
      end do
    end if
  end do
  f%h = ftplist(n)%h
  call ftp_refresh_thermal_properties_from_H(f)
  write(u, '(8ES26.17)') f%h, f%t, f%d, f%m, f%k, f%sigma_e, f%b, f%cp
  close(u)

  ! round trips and the Bc from_DH path; rho_hat deliberately != rho_EOS
  open(newunit = u, file = 'trip_'//trim(tag)//'.dat', status = 'replace')
  do i = 1, n - 1
    do j = 0, 15
      s = (real(j, WP) + 0.37_WP) / 16.0_WP
      hh = ftplist(i)%h + s * (ftplist(i+1)%h - ftplist(i)%h)
      f%h = hh; call ftp_refresh_thermal_properties_from_H(f)          ! T(h)
      g%t = f%t; a3(1,1,1) = g; call ftp_refresh_thermal_properties_from_T_undim_3Dftp(a3); g = a3(1,1,1)   ! h(T(h))
      write(u, '(4ES26.17)', advance = 'no') hh, f%t, g%h, f%cp
      f%t = ftplist(i)%t + s * (ftplist(i+1)%t - ftplist(i)%t)
      a3(1,1,1) = f; call ftp_refresh_thermal_properties_from_T_undim_3Dftp(a3); f = a3(1,1,1)   ! h(T)
      g%h = f%h; call ftp_refresh_thermal_properties_from_H(g)         ! T(h(T))
      write(u, '(3ES26.17)', advance = 'no') f%t, f%h, g%t
      rhat = 1.01_WP * f%d                                             ! Bc: h = rho h / rho_hat
      g%h = hh; call ftp_refresh_thermal_properties_from_H(g)
      f%rhoh = rhat * hh; f%d = rhat; call ftp_refresh_thermal_properties_from_DH(f)
      write(u, '(2ES26.17)') abs(f%h - g%h), max(abs(f%d - g%d), abs(f%t - g%t), abs(f%m - g%m), abs(f%k - g%k), abs(f%cp - g%cp))
    end do
  end do
  close(u)
  write(*, *) 'done ', trim(tag), n
  call MPI_FINALIZE(ierror)
end program test_property_interp
