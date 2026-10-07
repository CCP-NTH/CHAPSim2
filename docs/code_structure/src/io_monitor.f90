!> Regression-test metric container and JSON writer.
!>
!> Provides the compact metric set consumed by the shell regression tests and
!> writes rank-0 JSON files for comparison against reference values.
module regression_test_mod
  use precision_mod, only: WP, MPI_REAL_WP
  implicit none
  private
  !
  !> Scalar diagnostics used by smoke and regression tests.
  type :: t_metrics
    ! mass metrics
    real(wp) :: mass_balance
    real(wp) :: mass_residual(3)
    real(wp) :: projected_mass_residual(3)
    real(wp) :: physical_poisson_compatibility_defect
    real(wp) :: uniform_poisson_source_correction
    real(wp) :: poisson_projected_source_amplitude
    real(wp) :: poisson_zero_mode_rhs_projection
    real(wp) :: total_mass
    real(wp) :: total_mass_drift
    ! flow metrics
    real(wp) :: kinetic_energy
    real(wp) :: bulk_velocity(3)
    !real(wp) :: wall_shear_integral
    real(wp) :: mean_dpdx
    real(wp) :: pressure_drop
    ! thermal metrics
    real(wp) :: bulk_massflux(3)
    real(wp) :: bulk_enthalpy
    real(wp) :: bulk_temperature
    ! mhd metrics
    real(wp) :: max_div_j
    real(wp) :: current_imbalance
    ! les metrics
    real(wp) :: max_strain_rate_mag2_init
    real(wp) :: sgs_coef_face_min
    real(wp) :: sgs_visc_face_min
    real(wp) :: sgs_wall_coef_peak
    real(wp) :: sgs_inout_coef_peak
    real(wp) :: sgs_enthalpy_flux_peak
  end type t_metrics
  public :: t_metrics
  !----------------------------------------------------------------------------
  ! Subgrid-coefficient boundary and positivity diagnostics.
  !
  ! The momentum and energy subgrid blocks push the *production* face arrays
  ! through the recorders below, so these numbers describe the coefficients the
  ! stress and flux terms actually multiplied, not a reimplementation of the
  ! interpolation. They are running extrema over the whole run and are reduced
  ! across ranks once, when the metric file is written, so they do not depend on
  ! the pencil decomposition.
  !
  ! What each one gates:
  !   sgs coefficient face minimum  >= 0  the C2P interpolation of a non-negative
  !                                       coefficient must not undershoot.
  !   face viscosity minimum        >= 1  molecular floor; isothermal flow has
  !                                       mu/mu0 = 1 identically.
  !   wall subgrid coefficient peak == 0  no subgrid momentum stress and no
  !                                       subgrid enthalpy flux through a
  !                                       physical no-slip wall.
  !   inlet/outlet coefficient peak >  0  subgrid transport is retained at a
  !                                       flow-through plane, not switched off.
  !                                       Reported as 0 when the case has no
  !                                       inlet or outlet at all.
  !   subgrid enthalpy flux peak    >  0  the magnitude of the subgrid heat
  !                                       transport the case actually carries,
  !                                       so that a case with a negligible
  !                                       enthalpy gradient cannot read green
  !                                       on the wall check by default.
  !
  ! Non-negativity of the coefficient is a necessary property, not a proof of
  ! stability or of conservation; neither is claimed here.
  !----------------------------------------------------------------------------
  real(WP), save :: sgs_coef_face_min   = huge(1.0_WP)
  real(WP), save :: sgs_visc_face_min   = huge(1.0_WP)
  real(WP), save :: sgs_wall_coef_peak  = 0.0_WP
  real(WP), save :: sgs_inout_coef_peak = 0.0_WP
  real(WP), save :: sgs_enth_flux_peak  = 0.0_WP
  ! Coverage counters. A zero reference is legitimate for several of the gates
  ! above - a periodic case has no wall and no inlet/outlet, and a correct wall
  ! closure reports exactly zero - so a zero value cannot by itself distinguish
  ! "the gate sampled its locations and found zero" from "the gate was never
  ! reached". These count recorder invocations, not values, and make that
  ! distinction explicit: the values stay untouched, and only the absence of
  ! sampling is an error. Reduced with MPI_MAX, so a rank owning no boundary
  ! plane does not look like missing instrumentation.
  integer,  save :: n_sgs_coef_face_calls = 0
  integer,  save :: n_sgs_visc_face_calls = 0
  integer,  save :: n_sgs_coef_bc_calls   = 0
  integer,  save :: n_sgs_enth_flux_calls = 0
  !
  private :: write_json_real
  public  :: reduce_sgs_diagnostics
  public  :: write_metrics_json
  public  :: record_sgs_coef_face
  public  :: record_sgs_visc_face
  public  :: record_sgs_coef_bc
  public  :: record_sgs_enthalpy_flux
contains
  !> Record the peak magnitude of an assembled subgrid enthalpy flux component.
  !> - arr (in): face-located c_sgs * dh/dx_i.
  subroutine record_sgs_enthalpy_flux(arr)
    implicit none
    real(WP), intent(in) :: arr(:, :, :)
    ! max|arr| without abs(arr), which would materialise a temporary of
    ! the whole field on every substep.
    sgs_enth_flux_peak = max(sgs_enth_flux_peak, maxval(arr), -minval(arr))
    n_sgs_enth_flux_calls = n_sgs_enth_flux_calls + 1
    return
  end subroutine record_sgs_enthalpy_flux
!==============================================================================
  !> Record the minimum of an interpolated subgrid coefficient on a face set.
  !> - arr (in): face-located subgrid coefficient.
  subroutine record_sgs_coef_face(arr)
    implicit none
    real(WP), intent(in) :: arr(:, :, :)
    sgs_coef_face_min = min(sgs_coef_face_min, minval(arr))
    n_sgs_coef_face_calls = n_sgs_coef_face_calls + 1
    return
  end subroutine record_sgs_coef_face
!==============================================================================
  !> Record the minimum of a total (molecular + subgrid) face viscosity.
  !> - arr (in): face-located viscosity ratio mu/mu0.
  subroutine record_sgs_visc_face(arr)
    implicit none
    real(WP), intent(in) :: arr(:, :, :)
    sgs_visc_face_min = min(sgs_visc_face_min, minval(arr))
    n_sgs_visc_face_calls = n_sgs_visc_face_calls + 1
    return
  end subroutine record_sgs_visc_face
!==============================================================================
  !> Record the subgrid coefficient on the two physical boundary planes of a
  !> face set, classified by the coefficient boundary codes.
  !>
  !> arr must be staggered in idir AND held in the pencil aligned with idir, so
  !> that planes 1 and size(arr, idir) are the two global boundary planes on
  !> every rank. A wall plane is required to be exactly zero, a flow-through
  !> plane is required to stay positive; both are extrema over all ranks and
  !> substeps.
  !>
  !> - arr (in): face-located subgrid coefficient, aligned pencil.
  !> - idir (in): stagger direction, 1 = x, 2 = y, 3 = z.
  !> - ibc_sgs (in): coefficient boundary codes from get_ibc_for_sgs_coef_c2p.
  subroutine record_sgs_coef_bc(arr, idir, ibc_sgs)
    use parameters_constant_mod, only: IBC_DIRICHLET, IBC_NEUMANN
    implicit none
    real(WP), intent(in) :: arr(:, :, :)
    integer,  intent(in) :: idir
    integer,  intent(in) :: ibc_sgs(2)

    integer  :: side, ip
    real(WP) :: pmax, pmin

    n_sgs_coef_bc_calls = n_sgs_coef_bc_calls + 1

    ! The plane is reduced where it lies. An array section passed to maxval or
    ! minval needs no temporary, whereas returning the plane from a helper, or
    ! taking abs() of it, allocates one on every substep. pmax/pmin hold the
    ! plane extrema so that the wall branch gets max|.| as max(pmax, -pmin)
    ! without ever forming abs() of an array.
    do side = 1, 2
      ip = 1
      if(side == 2) ip = size(arr, idir)
      if(ibc_sgs(side) /= IBC_DIRICHLET .and. ibc_sgs(side) /= IBC_NEUMANN) cycle
      select case(idir)
      case(1)
        pmax = maxval(arr(ip, :, :)); pmin = minval(arr(ip, :, :))
      case(2)
        pmax = maxval(arr(:, ip, :)); pmin = minval(arr(:, ip, :))
      case default
        pmax = maxval(arr(:, :, ip)); pmin = minval(arr(:, :, ip))
      end select
      select case(ibc_sgs(side))
      case(IBC_DIRICHLET)   ! physical wall
        sgs_wall_coef_peak  = max(sgs_wall_coef_peak, pmax, -pmin)
      case(IBC_NEUMANN)     ! inlet or outlet
        sgs_inout_coef_peak = max(sgs_inout_coef_peak, pmax)
      end select
    end do

    return
  end subroutine record_sgs_coef_bc
!==============================================================================
  !> Reduce the subgrid diagnostics across ranks and copy them into the metrics.
  !> - metrics (inout): metric container to fill.
  !> - is_thermo (in): require the enthalpy-flux gate to have sampled as well.
  subroutine reduce_sgs_diagnostics(metrics, is_thermo)
    use mpi_mod
    use print_msg_mod, only : Print_error_msg
    implicit none
    type(t_metrics), intent(inout) :: metrics
    logical,         intent(in)    :: is_thermo
    real(WP) :: sbuf(2), rbuf(2)
    real(WP) :: sbuf3(3), rbuf3(3)
    integer  :: cbuf(4), crbuf(4)

    ! Coverage first: if a gate never sampled, its value below is meaningless
    ! and would otherwise be published as a zero that compares clean against a
    ! zero reference. MPI_MAX, so one rank having sampled is enough.
    cbuf = [n_sgs_coef_face_calls, n_sgs_visc_face_calls, &
            n_sgs_coef_bc_calls,   n_sgs_enth_flux_calls]
    call mpi_allreduce(cbuf, crbuf, 4, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierror)
    if(crbuf(1) == 0) call Print_error_msg('SGS gate coverage: '//&
         'record_sgs_coef_face was never called, so "min. sgs coefficient on '//&
         'stress/flux faces" would be published unsampled.')
    if(crbuf(2) == 0) call Print_error_msg('SGS gate coverage: '//&
         'record_sgs_visc_face was never called, so "min. total face '//&
         'viscosity (molecular+sgs)" would be published unsampled.')
    if(crbuf(3) == 0) call Print_error_msg('SGS gate coverage: '//&
         'record_sgs_coef_bc was never called, so the wall and inlet/outlet '//&
         'coefficient gates would be published unsampled.')
    if(is_thermo .and. crbuf(4) == 0) call Print_error_msg('SGS gate coverage: '//&
         'record_sgs_enthalpy_flux was never called in a thermal LES run, so '//&
         '"max. |sgs enthalpy flux|" would be published unsampled.')

    sbuf = [sgs_coef_face_min, sgs_visc_face_min]
    call mpi_allreduce(sbuf, rbuf, 2, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)
    ! Coverage above guarantees both recorders ran, so the huge() initialiser
    ! cannot survive; the guard stays as a defensive assertion.
    metrics%sgs_coef_face_min = rbuf(1)
    metrics%sgs_visc_face_min = rbuf(2)
    if(rbuf(1) > 0.5_WP * huge(1.0_WP)) call Print_error_msg('SGS gate coverage: '//&
         'sgs_coef_face_min was reduced but never written.')
    if(rbuf(2) > 0.5_WP * huge(1.0_WP)) call Print_error_msg('SGS gate coverage: '//&
         'sgs_visc_face_min was reduced but never written.')

    sbuf3 = [sgs_wall_coef_peak, sgs_inout_coef_peak, sgs_enth_flux_peak]
    call mpi_allreduce(sbuf3, rbuf3, 3, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    metrics%sgs_wall_coef_peak     = rbuf3(1)
    metrics%sgs_inout_coef_peak    = rbuf3(2)
    metrics%sgs_enthalpy_flux_peak = rbuf3(3)

    return
  end subroutine reduce_sgs_diagnostics
!==============================================================================
  !> Write regression metrics to a JSON file on rank 0.
  !> - filename (in): Output JSON file name.
  !> - metrics (in): Metrics to write.
  !> - is_thermo (in): Include thermal metrics when true.
  !> - is_mhd (in): Include MHD charge-conservation metrics when true.
  !> - is_les (in): Include LES metrics when true.
  subroutine write_metrics_json(filename, metrics, is_thermo, is_mhd, is_les)
    use mpi_mod, only: nrank
    implicit none
    !
    character(len=*), intent(in) :: filename
    type(t_metrics),  intent(in) :: metrics
    logical, intent(in) :: is_thermo
    logical, intent(in) :: is_mhd
    logical, intent(in) :: is_les
    !
    integer :: unit
    logical :: exists
    !----------------------------------------------------------
    ! Only rank 0 writes
    !----------------------------------------------------------
    if (nrank /= 0) return
    !
    inquire(file=filename, exist=exists)
    if (exists) open(newunit=unit, file=filename, status='replace', action='write')
    if (.not. exists) open(newunit=unit, file=filename, status='new',     action='write')

    write(unit,'(a)') '{'
    ! mass
    call write_json_real(unit, 'global mass balance',               metrics%mass_balance,        last=.false.)
    ! Preserve the established regression meaning: these keys measure the
    ! continuity residual achieved by the pressure projection.
    call write_json_real(unit, 'max. mass conservation (interior)', metrics%projected_mass_residual(1), last=.false.)
    call write_json_real(unit, 'max. mass conservation (inlet)',    metrics%projected_mass_residual(2), last=.false.)
    call write_json_real(unit, 'max. mass conservation (outlet)',   metrics%projected_mass_residual(3), last=.false.)
    call write_json_real(unit, 'max. physical mass conservation (interior)', metrics%mass_residual(1), last=.false.)
    call write_json_real(unit, 'max. physical mass conservation (inlet)',    metrics%mass_residual(2), last=.false.)
    call write_json_real(unit, 'max. physical mass conservation (outlet)',   metrics%mass_residual(3), last=.false.)
    call write_json_real(unit, 'max. projected mass conservation (interior)', metrics%projected_mass_residual(1), last=.false.)
    call write_json_real(unit, 'max. projected mass conservation (inlet)',    metrics%projected_mass_residual(2), last=.false.)
    call write_json_real(unit, 'max. projected mass conservation (outlet)',   metrics%projected_mass_residual(3), last=.false.)
    call write_json_real(unit, 'physical Poisson compatibility defect', metrics%physical_poisson_compatibility_defect, last=.false.)
    call write_json_real(unit, 'explicit uniform Poisson-source correction', &
                         metrics%uniform_poisson_source_correction, last=.false.)
    call write_json_real(unit, 'Poisson projected-source amplitude', metrics%poisson_projected_source_amplitude, last=.false.)
    call write_json_real(unit, 'Poisson zero-mode projection (scaled solver RHS)', &
                         metrics%poisson_zero_mode_rhs_projection, last=.false.)
    call write_json_real(unit, 'total mass', metrics%total_mass, last=.false.)
    call write_json_real(unit, 'total mass drift from run start', metrics%total_mass_drift, last=.false.)
    call write_json_real(unit, 'global pressure drop',  metrics%pressure_drop,    last=.false.)
    call write_json_real(unit, 'mean dpdx',             metrics%mean_dpdx,        last=.false.)
    ! momentum
    call write_json_real(unit, 'total kinetic energy',  metrics%kinetic_energy,   last=.false.)
    call write_json_real(unit, 'bulk velocity ux',      metrics%bulk_velocity(1), last=.false.)
    !call write_json_real(unit, 'bulk velocity uy',      metrics%bulk_velocity(2), last=.false.)
    ! JSON forbids a trailing comma, so whichever optional block comes last has to
    ! close the object. The order is flow -> thermal -> mhd -> les.
    call write_json_real(unit, 'bulk velocity uz',      metrics%bulk_velocity(3), &
                         last = (.not. is_thermo) .and. (.not. is_mhd) .and. (.not. is_les))
    !call write_json_real(unit, 'wall shear integral',  metrics%wall_shear_integral, last=.false.)

    if(is_thermo) then
      ! mass flux
      call write_json_real(unit, 'bulk massflux gx',      metrics%bulk_massflux(1), last=.false.)
      !call write_json_real(unit, 'bulk massflux gy',      metrics%bulk_massflux(2), last=.false.)
      call write_json_real(unit, 'bulk massflux gz',      metrics%bulk_massflux(3), last=.false.)
      call write_json_real(unit, 'bulk enthalpy',         metrics%bulk_enthalpy,    last=.false.)
      call write_json_real(unit, 'bulk temperature',      metrics%bulk_temperature, &
                           last = (.not. is_mhd) .and. (.not. is_les))
     !call write_json_real(unit, 'wall_heat_flux',                    metrics%wall_heat_flux,      last=.false.)
    end if

    if(is_mhd) then
      ! Charge conservation. j = -grad(ep) + u x B is solenoidal only if the
      ! discrete D.G reproduces the Poisson operator the solver inverts, so these
      ! two numbers are the MHD analogue of the mass-conservation residual above.
      call write_json_real(unit, 'max. |div(current density)|', metrics%max_div_j, last=.false.)
      call write_json_real(unit, 'global electric current imbalance', metrics%current_imbalance, &
                           last = .not. is_les)
    end if

    if(is_les) then
      ! Taken on the initial field, never on an advanced one: a solid-body rotation
      ! has S_ij = 0 exactly, so a cylindrical case initialised that way turns this
      ! key into a machine-zero gate on the velocity-gradient assembly. For any
      ! other initial condition it is simply the peak resolved strain.
      call write_json_real(unit, 'max. initial strain rate S_ijS_ij', &
                           metrics%max_strain_rate_mag2_init, last=.false.)
      ! The four subgrid-coefficient gates; see the block comment on the module
      ! accumulators for what each one has to satisfy.
      call write_json_real(unit, 'min. sgs coefficient on stress/flux faces', &
                           metrics%sgs_coef_face_min,   last=.false.)
      call write_json_real(unit, 'min. total face viscosity (molecular+sgs)', &
                           metrics%sgs_visc_face_min,   last=.false.)
      call write_json_real(unit, 'max. sgs coefficient on physical walls', &
                           metrics%sgs_wall_coef_peak,  last=.false.)
      call write_json_real(unit, 'max. sgs coefficient at inlet/outlet', &
                           metrics%sgs_inout_coef_peak, last=.not. is_thermo)
      if(is_thermo) &
      call write_json_real(unit, 'max. |sgs enthalpy flux|', &
                           metrics%sgs_enthalpy_flux_peak, last=.true.)
    end if
    ! other
    write(unit,'(a)') '}'
    close(unit)
    return
  end subroutine write_metrics_json
!==============================================================================
  subroutine write_json_real(unit, key, value, last)
    implicit none
    integer,          intent(in) :: unit
    character(len=*), intent(in) :: key
    real(WP),         intent(in) :: value
    logical,          intent(in) :: last

    if (last) then
      write(unit,'(a,""": ",es16.8)') '  "'//trim(key), value
    else
      write(unit,'(a,""": ",es16.8,",")') '  "'//trim(key), value
    end if
    return
  end subroutine write_json_real
end module
!==============================================================================
!==============================================================================
!> Monitor-history output for mass, bulk, probe, and regression diagnostics.
!>
!> This module writes the `3_monitor` history files used to track run health and
!> emits regression metrics at configured checkpoints.
module io_monitor_mod
  use precision_mod
  use print_msg_mod
  use regression_test_mod
  implicit none

  private
  !real(WP), save :: bulk_MKE0
  public :: write_monitor_ini
  public :: write_monitor_bulk
  public :: write_monitor_probe

  character(len=120), parameter :: fl_bulk = "monitor_metrics_history"
  character(len=120), parameter :: fl_mass = "monitor_change_history"

  type(t_metrics),save :: metrics, metrics0

contains
  !> Create monitor-history files and headers.
  !> - dm (inout): Domain descriptor containing monitor configuration.
  subroutine write_monitor_ini(dm)
    use io_tools_mod
    use parameters_constant_mod
    use typeconvert_mod
    use udf_type_mod
    use wtformat_mod
    implicit none
    type(t_domain),  intent(inout) :: dm

    integer :: myunit
    integer :: i, j
    logical :: exist
    character(len=120) :: flname, keyword

    integer :: idgb(3)
    integer :: nplc
    logical :: is_y, is_z
    integer, allocatable :: probeid(:, :)

    if(nrank == 0) call Print_debug_start_msg("Writing monitor initial files ...")
!------------------------------------------------------------------------------
! create history file for total variables
!------------------------------------------------------------------------------
    if(nrank == 0 .and. (.not. is_IO_off)) then
      call generate_pathfile_name(flname, dm%idom, trim(fl_bulk), dir_moni, 'log')
      inquire(file = trim(flname), exist = exist)
      if (exist) then
        !open(newunit = myunit, file = trim(flname), status="old", position="append", action="write")
      else
        open(newunit = myunit, file = trim(flname), status="new", action="write")
        write(myunit, *) "# domain-id : ", dm%idom, "pt-id : ", i
        write(myunit, *) "# columns description:"
        write(myunit, *) "# column  1 : time"
        write(myunit, *) "# column  2 : global mass balance"
        write(myunit, *) "# column  3 : max. mass conservation (interior)"
        write(myunit, *) "# column  4 : max. mass conservation (inlet)"
        write(myunit, *) "# column  5 : max. mass conservation (outlet)"
        write(myunit, *) "# column  6 : total kinetic energy"
        !write(myunit, *) "# column 10 : wall shear integral"
        write(myunit, *) "# column  7 : mean dpdx"
        write(myunit, *) "# column  8 : global pressure drop"
        write(myunit, *) "# column  9 : bulk velocity qx"
        write(myunit, *) "# column 10 : bulk velocity qy"
        write(myunit, *) "# column 11 : bulk velocity qz"
        if(dm%is_thermo) then
          write(myunit, *) "# column 12 : bulk mass flux gx"
          write(myunit, *) "# column 13 : bulk mass flux gy"
          write(myunit, *) "# column 14 : bulk mass flux gz"
          write(myunit, *) "# column 15 : bulk enthalpy"
          write(myunit, *) "# column 16 : bulk temperature"
          !write(myunit, *) "# column 17 : wall heat flux"
        end if
        close(myunit)
      end if

      call generate_pathfile_name(flname, dm%idom, trim(fl_mass), dir_moni, 'log')
      inquire(file = trim(flname), exist = exist)
      if (exist) then
        !open(newunit = myunit, file = trim(flname), status="old", position="append", action="write")
      else
        open(newunit = myunit, file = trim(flname), status="new", action="write")
        write(myunit, *) "# domain-id : ", dm%idom, "pt-id : ", i
        write(myunit, *) "# columns: time; physical mass residual at bulk, inlet, outlet;"
        write(myunit, *) "#          projected mass residual at bulk, inlet, outlet; global mass flux imbalance;"
        write(myunit, *) "#          physical Poisson compatibility defect; explicit uniform Poisson-source correction;"
        write(myunit, *) "#          Poisson projected-source amplitude C;"
        write(myunit, *) "#          projected correction is C in Cartesian coordinates and C/r^2 in cylindrical coordinates;"
        write(myunit, *) "#          Poisson zero-mode projection in scaled solver-RHS units;"
        write(myunit, *) "#          total mass; total mass drift from run start; kinetic energy change rate"
        close(myunit)
      end if
    end if
!------------------------------------------------------------------------------
    if(dm%proben <= 0) return

    if(nrank == 0) then
      call Print_debug_inline_msg("  Probed points for monitoring ...")
    end if
!------------------------------------------------------------------------------
    allocate( dm%probe_is_in(dm%proben) )
    dm%probe_is_in(:) = .false.

    allocate( probeid(3, dm%proben) )
    nplc = 0
    do i = 1, dm%proben
!------------------------------------------------------------------------------
! probe points find the nearest cell centre, global index info, then convert to local index in x-pencil
!------------------------------------------------------------------------------
      idgb(1:3) = 0

      idgb(1) = ceiling ( dm%probexyz(1, i) / dm%h(1) )
      idgb(3) = ceiling ( dm%probexyz(3, i) / dm%h(3) )
      do j = 1, dm%np(2) - 1
        if (dm%probexyz(2, i) >= dm%yp(j) .and. &
            dm%probexyz(2, i) < dm%yp(j+1)) then
          idgb(2) = j
        end if
      end do
      if( dm%probexyz(2, i) >= dm%yp(dm%np(2)) .and. dm%probexyz(2, i) < dm%lyt) then
        idgb(2) = dm%nc(2)
      end if
!------------------------------------------------------------------------------
! convert global id to local, based on x-pencil
!------------------------------------------------------------------------------
      is_y = .false.
      is_z = .false.
      if( idgb(2) >= dm%dccc%xst(2) .and. idgb(2) <= dm%dccc%xen(2) ) is_y = .true.
      if( idgb(3) >= dm%dccc%xst(3) .and. idgb(3) <= dm%dccc%xen(3) ) is_z = .true.
      if(is_y .and. is_z) then
        dm%probe_is_in(i) = .true.
        nplc = nplc + 1
        probeid(1, nplc) = idgb(1)
        probeid(2, nplc) = idgb(2) - dm%dccc%xst(2) + 1
        probeid(3, nplc) = idgb(3) - dm%dccc%xst(3) + 1
        !write(*,*) 'test', i, nrank, nplc, probeid(1:3, nplc)
      end if
    end do

    if(nplc > 0) allocate(dm%probexid(3, nplc))

    do i = 1, nplc
      dm%probexid(1:3, i) = probeid(1:3, i)
    end do

    deallocate (probeid)
!------------------------------------------------------------------------------
! create probe history file for flow
!------------------------------------------------------------------------------
    nplc = 0
    do i = 1, dm%proben
      if(dm%probe_is_in(i)) then
        nplc = nplc + 1
        write (*, '(A, I1, A, I1, A, 3F5.2, A, 3I6)') &
            '  pt global id =', i, ', at nrank =', nrank, ', location xyz=', dm%probexyz(1:3, i), &
            ', local id = ', dm%probexid(1:3, nplc)
      end if
    end do
    call mpi_barrier(MPI_COMM_WORLD, ierror)
!------------------------------------------------------------------------------
! create probe history file for flow
!------------------------------------------------------------------------------
    if (.not. is_IO_off) then
    do i = 1, dm%proben
      if(.not. dm%probe_is_in(i)) cycle

      keyword = "monitor_pt"//trim(int2str(i))//"_flow"
      call generate_pathfile_name(flname, dm%idom, keyword, dir_moni, 'dat')

      inquire(file = trim(flname), exist = exist)
      if (exist) then
        !open(newunit = myunit, file = trim(flname), status="old", position="append", action="write")
      else
        open(newunit = myunit, file = trim(flname), status="new", action="write")
        write(myunit, *) "# domain-id : ", dm%idom, "pt-id : ", i
        write(myunit, *) "# probe pts location ",  dm%probexyz(1:3, i)
        if(dm%is_thermo) then
          write(myunit, *) "# iteration, t, u, v, w, p, phi, T" ! to add more instantanous or statistics
        else
          write(myunit, *) "# iteration, t, u, v, w, p, phi" ! to add more instantanous or statistics
        end if
        close(myunit)
      end if
    end do
    end if
    call mpi_barrier(MPI_COMM_WORLD, ierror)

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==============================================================================
  !> Append bulk-flow, mass, pressure, and optional thermal monitor values.
  !> - fl (in): Flow state.
  !> - dm (inout): Domain descriptor.
  !> - tm (in): Thermal state.
  subroutine write_monitor_bulk(fl, dm, tm)
    use bc_dirichlet_mod
    use cylindrical_rn_mod
    use find_max_min_ave_mod
    use io_files_mod
    use io_tools_mod
    use math_mod, only : safe_divide
    use operations
    use parameters_constant_mod
    use regression_test_mod
    use solver_tools_mod
    use thermo_info_mod
    use typeconvert_mod
    use udf_type_mod
    use wtformat_mod
    implicit none

    type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    type(t_thermo), optional, intent(in) :: tm

    type(t_metrics) :: metrics
    character(len=120) :: flname
    character(len=120) :: keyword
    character(200) :: iotxt
    integer :: ioerr, myunit

    real(WP) :: bulk_MKE, bulk_q(3), bulk_g(3), bulk_m, bulk_h, mean_dpdx, pressure_drop
    real(WP) :: bulk_fbcx(2), bulk_fbcy(2), bulk_fbcz(2)
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc1
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc2
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc3
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: fenergy
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil, qy_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil, qz_zpencil
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx
    real(WP), dimension(dm%dccc%ysz(1), 4, dm%dccc%ysz(3)) :: fbcy
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), 4) :: fbcz
    real(WP), dimension(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) :: fbcy_c4c
    real(WP) :: dMKEdt
    type(t_fluidThermoProperty) :: ftp_bulk

!------------------------------------------------------------------------------
!   kinetic energy = 1/2*rho * (uu+vv+ww)
!------------------------------------------------------------------------------
    ! ux
    call Get_x_midp_P2C_3D(fl%qx, accc1, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    ! uy = qy/r
    call transpose_x_to_y(fl%qy, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy(:), dm%fbcy_qy)
    if(dm%icoordinate == ICYLINDRICAL)&
    call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
    call transpose_y_to_x(accc_ypencil, accc2, dm%dccc)
    ! qz = uz
    call transpose_x_to_y(fl%qz, accp_ypencil, dm%dccp)
    call transpose_y_to_z(accp_ypencil, accp_zpencil, dm%dccp)
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz(:), dm%fbcz_qz)
    call transpose_z_to_y(accc_zpencil, accc_ypencil, dm%dccc)
    call transpose_y_to_x(accc_ypencil, accc3, dm%dccc)
    !volumetric averaged kinetic energy
    fenergy = HALF * (accc1 * accc1 + accc2 * accc2 + accc3 * accc3)
    if(dm%is_thermo) then
      fenergy = fenergy * fl%dDens
    end if
    call Get_volumetric_average_3d(dm, dm%dccc, fenergy, bulk_MKE, SPACE_AVERAGE, 'MKE')
    dMKEdt = (bulk_MKE - fl%tt_kinetic_energy)/dm%dt
    fl%tt_kinetic_energy = bulk_MKE
!------------------------------------------------------------------------------
!   mass balance = density change + net mass flux through boundaries
!------------------------------------------------------------------------------
    if(dm%is_thermo) then
      call Get_volumetric_average_3d(dm, dm%dccc, fl%dDens, fl%total_mass, SPACE_INTEGRAL, 'total mass')
    else
      fl%total_mass = dm%vol
    end if
    fl%total_mass_drift = fl%total_mass - fl%total_mass_reference
!------------------------------------------------------------------------------
!   Bulk quantities
!------------------------------------------------------------------------------
    ! mean dp/dx pressure gradient
    call Get_x_1der_C2C_3D(fl%pres, accc1, dm, dm%iAccuracy, dm%ibcx_pr(:), dm%fbcx_pr)
    call Get_volumetric_average_3d(dm, dm%dccc, accc1, mean_dpdx,  SPACE_AVERAGE, 'dpdx')
    !
    ! global pressure drop
    if(dm%ibcx_pr(1)/=IBC_PERIODIC) then
    call Get_area_average_2d_for_fbcx(dm, dm%dccc, fl%pres, bulk_fbcx, SPACE_INTEGRAL, 'varx')
    pressure_drop = bulk_fbcx(1) - bulk_fbcx(2)
    else
    pressure_drop = ZERO
    end if
    !
    ! bulk streamwise velocity
    bulk_q = ZERO
    call Get_volumetric_average_3d(dm, dm%dpcc, fl%qx, bulk_q(1), SPACE_AVERAGE, 'ux')
    call Get_volumetric_average_3d(dm, dm%dccp, fl%qz, bulk_q(3), SPACE_AVERAGE, 'uz')
    !
    ! thermal flow quantities
    if(dm%is_thermo .and. present(tm)) then
      ! bulk momentum
      bulk_g = ZERO
      call Get_volumetric_average_3d(dm, dm%dpcc, fl%gx, bulk_g(1), SPACE_AVERAGE, 'rho*ux')
      call Get_volumetric_average_3d(dm, dm%dccp, fl%gz, bulk_g(3), SPACE_AVERAGE, 'rho*uz')
      ! Mass-flux-weighted (bulk) enthalpy, h_b = <gx*h> / <gx>. gx lives on dpcc,
      ! so it has to be *interpolated* onto the cell centres where hEnth lives -
      ! P2C, not a derivative and not C2C. This used to call Get_x_1der_C2C_3D,
      ! which formed <d(gx)/dx * h> with a C2C stencil on a P-located array: the
      ! wrong quantity evaluated at the wrong points.
      call Get_x_midp_P2C_3D(fl%gx, accc1, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_gx)
      accc2 = accc1 * tm%hEnth
      call Get_volumetric_average_3d(dm, dm%dccc, accc2, bulk_h,  SPACE_AVERAGE, 'h')
      ! A bulk enthalpy is only meaningful where there is a net mass flux to
      ! weight by. In a zero-net-flow case (TGV) <gx> is at round-off, ~1e-19, and
      ! an unguarded divide turned this diagnostic into ~1e15 and then fed that
      ! through the property table. Report zero instead.
      ftp_bulk%h = safe_divide(bulk_h, bulk_g(1))
      call ftp_refresh_thermal_properties_from_H(ftp_bulk)
    end if
!------------------------------------------------------------------------------
!   save regression test metrics at the end of flow simulation
!------------------------------------------------------------------------------
    if(fl%iteration == fl%nIterFlowEnd) then
      ! universal matrics
      metrics%mass_balance     = fl%tt_mass_change
      metrics%mass_residual(1:3) = fl%mcon(1:3) !
      metrics%projected_mass_residual(1:3) = fl%mcon_projected(1:3)
      metrics%physical_poisson_compatibility_defect = fl%physical_poisson_compatibility_defect
      metrics%uniform_poisson_source_correction = fl%uniform_poisson_source_correction
      metrics%poisson_projected_source_amplitude = fl%poisson_projected_source_amplitude
      metrics%poisson_zero_mode_rhs_projection = fl%poisson_zero_mode_rhs_projection
      metrics%total_mass = fl%total_mass
      metrics%total_mass_drift = fl%total_mass_drift
      metrics%kinetic_energy   = fl%tt_kinetic_energy
      metrics%bulk_velocity(:) = bulk_q(:)
      metrics%pressure_drop    = pressure_drop
      metrics%mean_dpdx        = mean_dpdx
      if(dm%is_thermo .and. present(tm)) then
        metrics%bulk_massflux(:) = bulk_g(:)
        metrics%bulk_enthalpy    = ftp_bulk%h
        metrics%bulk_temperature = ftp_bulk%t
      end if
      ! The charge-conservation diagnostics are carried on t_flow rather than t_mhd
      ! because mhd(:) is only allocated for an MHD run, while this monitor is called
      ! for every case; check_current_conservation refreshes them each step.
      metrics%max_div_j         = ZERO
      metrics%current_imbalance = ZERO
      if(dm%is_mhd) then
        metrics%max_div_j         = fl%max_div_j
        metrics%current_imbalance = fl%current_imbalance
      end if
      ! Recorded once by initialise_flow_fields and never overwritten, so this is
      ! the strain of the initial field however many steps the case has run.
      metrics%max_strain_rate_mag2_init = ZERO
      metrics%sgs_coef_face_min   = ZERO
      metrics%sgs_visc_face_min   = ZERO
      metrics%sgs_wall_coef_peak  = ZERO
      metrics%sgs_inout_coef_peak = ZERO
      metrics%sgs_enthalpy_flux_peak = ZERO
      if(dm%LES_model /= ILES_NONE) then
        metrics%max_strain_rate_mag2_init = fl%max_strain_rate_mag2_init
        ! collective: every rank must reach this, so it sits outside the nrank==0
        ! guard of write_metrics_json.
        call reduce_sgs_diagnostics(metrics, dm%is_thermo)
      end if
      !
      call write_metrics_json(trim('regression_test_metrics.json'), metrics, &
                              dm%is_thermo, dm%is_mhd, dm%LES_model /= ILES_NONE)
    end if
!------------------------------------------------------------------------------
! open file
!------------------------------------------------------------------------------
    if(nrank == 0) then
      ! write out history of key conservative variables
      call generate_pathfile_name(flname, dm%idom, trim(fl_mass), dir_moni, 'log')
      open(newunit = myunit, file = trim(flname), status = "old", action = "write", position = "append", &
          iostat = ioerr, iomsg = iotxt)
      if(ioerr /= 0) then
        call Print_error_msg('Problem openning conservation file')
      end if
      write(myunit, '(15ES16.8)') fl%time, fl%mcon(1:3), fl%mcon_projected(1:3), fl%tt_mass_change, &
                                  fl%physical_poisson_compatibility_defect, fl%uniform_poisson_source_correction, &
                                  fl%poisson_projected_source_amplitude, fl%poisson_zero_mode_rhs_projection, &
                                  fl%total_mass, fl%total_mass_drift, dMKEdt
      close(myunit)
      ! write out history of bulk variables
      call generate_pathfile_name(flname, dm%idom, trim(fl_bulk), dir_moni, 'log')
      open(newunit = myunit, file = trim(flname), status = "old", action = "write", position = "append", &
          iostat = ioerr, iomsg = iotxt)
      if(ioerr /= 0) then
        call Print_error_msg('Problem openning bulk file')
      end if
      if(dm%is_thermo .and. present(tm)) then
        write(myunit, '(1E13.5, 15ES16.8)') fl%time, fl%tt_mass_change, fl%mcon(1:3), &
          bulk_MKE, mean_dpdx, pressure_drop, bulk_q(1:3), &
          bulk_g(1:3), ftp_bulk%h, ftp_bulk%t
      else
        write(myunit, '(1E13.5, 10ES16.8)') fl%time, fl%tt_mass_change, fl%mcon(1:3), &
          bulk_MKE, mean_dpdx, pressure_drop, bulk_q(1:3)
      end if
      close(myunit)
    end if

    return
  end subroutine

!==============================================================================
  !> Write configured point-probe histories.
  !> - fl (in): Flow state.
  !> - dm (in): Domain descriptor containing probe locations.
  !> - tm (in): Thermal state.
  subroutine write_monitor_probe(fl, dm, tm)
    use io_files_mod
    use io_tools_mod
    use parameters_constant_mod
    use typeconvert_mod
    use udf_type_mod
    use wtformat_mod
    implicit none

    type(t_domain),  intent(in) :: dm
    type(t_flow), intent(in) :: fl
    type(t_thermo), optional, intent(in) :: tm

    character(len=120) :: flname
    character(len=120) :: keyword
    character(200) :: iotxt
    integer :: ioerr, myunit
    integer :: ix, iy, iz
    integer :: i, nplc

    if(dm%proben <= 0) return
!------------------------------------------------------------------------------
! based on x-pencil
!------------------------------------------------------------------------------
    nplc = 0
    do i = 1, dm%proben
      if( .not. dm%probe_is_in(i) ) cycle
      nplc = nplc + 1
!------------------------------------------------------------------------------
! open file
!------------------------------------------------------------------------------
        keyword = "monitor_pt"//trim(int2str(i))//"_flow"
        call generate_pathfile_name(flname, dm%idom, keyword, dir_moni, 'dat')
        open(newunit = myunit, file = trim(flname), status = "old", action = "write", position = "append", &
            iostat = ioerr, iomsg = iotxt)
        if(ioerr /= 0) then
          !write (*, *) 'Problem openning probing file'
          !write (*, *) 'Message: ', trim (iotxt)
          call Print_error_msg('Problem opening probing file')
        end if
!------------------------------------------------------------------------------
! write out local data
!------------------------------------------------------------------------------
        ix = dm%probexid(1, nplc)
        iy = dm%probexid(2, nplc)
        iz = dm%probexid(3, nplc)
        !write(*,*) 'probe pts:', nrank, nplc, ix, iy, iz
        if(dm%is_thermo .and. present(tm)) then
          write(myunit, '(I12, 1X, 7ES13.5)') fl%iteration, fl%time, fl%qx(ix, iy, iz), fl%qy(ix, iy, iz), &
            fl%qz(ix, iy, iz), fl%pres(ix, iy, iz), fl%pcor(ix, iy, iz), tm%tTemp(ix, iy, iz)
        else
          write(myunit, '(I12, 1X, 6ES13.5)') fl%iteration, fl%time, fl%qx(ix, iy, iz), fl%qy(ix, iy, iz), &
            fl%qz(ix, iy, iz), fl%pres(ix, iy, iz), fl%pcor(ix, iy, iz)
        end if
        close(myunit)
    end do

    return
  end subroutine
!==============================================================================
end module

!==============================================================================
!==============================================================================
