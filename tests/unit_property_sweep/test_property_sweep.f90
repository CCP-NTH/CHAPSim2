!------------------------------------------------------------------------------
! Full-precision property sweep, linked against the solver's own objects, for
! comparing two builds bit for bit (evaluation-order changes claimed neutral).
! For one fluid and interpolation mode it builds the property table through the
! solver path (Buildup_thermo_mapping_relations) and writes, as raw real(WP)
! stream files (h, T, rho, mu, k, sigma_e, cp, beta, rho*h per state):
!   nodes_<tag>.bin : the non-dimensional table nodes
!   fromT_<tag>.bin : from_T on 32 points per table interval
!   fromH_<tag>.bin : from_H on 32 points per interval (table fluids)
!   fromDH_<tag>.bin: from_DH(rho_hat h, rho_hat) with rho_hat = rho (1 + 1e-3), the
!                     transported-density inverse used per cell (both property paths)
! usage: mpirun -np 1 test_property_sweep <linear|pchip> <fluid> <ref_T0>
!   fluid: water co2 sodium lead bismuth lbe lithium flibe pbli
!------------------------------------------------------------------------------
program test_property_sweep
  use mpi_mod
  use parameters_constant_mod
  use udf_type_mod
  use thermo_info_mod
  implicit none
  type(t_thermo) :: tm
  type(t_fluidThermoProperty) :: f
  type(t_fluidThermoProperty) :: a3(1, 1, 1)   ! from_T is reached through its public 3-D wrapper
  character(len = 16) :: amode, afluid, aref
  character(len = 64) :: tag
  integer :: n, i, j, u
  integer, parameter :: nsub = 32
  real(WP) :: s, rhat

  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, nrank, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierror)
  call get_command_argument(1, amode); call get_command_argument(2, afluid); call get_command_argument(3, aref)
  iprop_interp = IPROP_INTERP_LINEAR
  if(trim(amode) == 'pchip') iprop_interp = IPROP_INTERP_PCHIP
  is_bc_recovery = .true.
  select case(trim(afluid))
    case('water');   tm%ifluid = ISCP_WATER
    case('co2');     tm%ifluid = ISCP_CO2
    case('sodium');  tm%ifluid = ILIQUID_SODIUM
    case('lead');    tm%ifluid = ILIQUID_LEAD
    case('bismuth'); tm%ifluid = ILIQUID_BISMUTH
    case('lbe');     tm%ifluid = ILIQUID_LBE
    case('lithium'); tm%ifluid = ILIQUID_LITHIUM
    case('flibe');   tm%ifluid = ILIQUID_FLIBE
    case('pbli');    tm%ifluid = ILIQUID_PBLI
    case default;    write(*, *) 'unknown fluid'; call MPI_ABORT(MPI_COMM_WORLD, 2, ierror)
  end select
  read(aref, *) tm%ref_T0
  tm%init_T0 = tm%ref_T0
  call Buildup_thermo_mapping_relations(tm)
  n = fluidparam%nlist
  tag = trim(afluid)//'_'//trim(amode)

  open(newunit = u, file = 'nodes_'//trim(tag)//'.bin', access = 'stream', form = 'unformatted', status = 'replace')
  do i = 1, n
    call put(u, ftplist(i))
  end do
  close(u)

  open(newunit = u, file = 'fromT_'//trim(tag)//'.bin', access = 'stream', form = 'unformatted', status = 'replace')
  do i = 1, n - 1
    do j = 0, nsub - 1
      s = real(j, WP) / real(nsub, WP)
      f%t = ftplist(i)%t + s * (ftplist(i + 1)%t - ftplist(i)%t)
      a3(1, 1, 1) = f; call ftp_refresh_thermal_properties_from_T_undim_3Dftp(a3); f = a3(1, 1, 1)
      call put(u, f)
    end do
  end do
  close(u)

  if(fluidparam%ipropertyState == IPROPERTY_TABLE) then
    open(newunit = u, file = 'fromH_'//trim(tag)//'.bin', access = 'stream', form = 'unformatted', status = 'replace')
    do i = 1, n - 1
      do j = 0, nsub - 1
        s = real(j, WP) / real(nsub, WP)
        f%h = ftplist(i)%h + s * (ftplist(i + 1)%h - ftplist(i)%h)
        call ftp_refresh_thermal_properties_from_H(f)
        call put(u, f)
      end do
    end do
    close(u)
  end if

  open(newunit = u, file = 'fromDH_'//trim(tag)//'.bin', access = 'stream', form = 'unformatted', status = 'replace')
  do i = 1, n - 1
    do j = 1, nsub - 1
      s = real(j, WP) / real(nsub, WP)
      f%t = ftplist(i)%t + s * (ftplist(i + 1)%t - ftplist(i)%t)
      a3(1, 1, 1) = f; call ftp_refresh_thermal_properties_from_T_undim_3Dftp(a3); f = a3(1, 1, 1)
      rhat = f%d * (ONE + 1.0e-3_WP)
      f%rhoh = rhat * f%h
      f%d = rhat
      call ftp_refresh_thermal_properties_from_DH(f)
      call put(u, f)
    end do
  end do
  close(u)
  write(*, '(A, I0, A)') trim(tag)//': ', n, ' table nodes written'
  call MPI_FINALIZE(ierror)

contains
  subroutine put(unit, p)
    integer, intent(in) :: unit
    type(t_fluidThermoProperty), intent(in) :: p
    write(unit) p%h, p%t, p%d, p%m, p%k, p%sigma_e, p%cp, p%b, p%rhoh
  end subroutine put
end program test_property_sweep
