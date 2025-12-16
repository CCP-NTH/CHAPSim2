module regression_test_mod 
  use precision_mod, only: WP
  implicit none
  private
  !
  type :: t_metrics
    ! mass metrics
    real(wp) :: mass_balance
    real(wp) :: mass_residual(3)
    ! flow metrics
    real(wp) :: kinetic_energy
    real(wp) :: bulk_velocity(3)
    !real(wp) :: wall_shear_integral
    real(wp) :: mean_dpdx
    real(wp) :: pressure_drop
    ! thermal metrics
    real(wp) :: bulk_massflux(3)
    real(wp) :: bulk_enthalpy
    real(wp) :: energy_balance
    !real(wp) :: wall_heat_flux
  end type t_metrics
  public :: t_metrics
  !
  private :: write_json_real
  public :: write_metrics_json
contains
  subroutine write_metrics_json(filename, metrics, is_thermo)
    use mpi_mod, only: nrank
    implicit none
    !
    character(len=*), intent(in) :: filename
    type(t_metrics),  intent(in) :: metrics
    logical, intent(in) :: is_thermo
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
    call write_json_real(unit, 'max. mass conservation (interior)', metrics%mass_residual(1),    last=.false.)
    call write_json_real(unit, 'max. mass conservation (inlet)',    metrics%mass_residual(2),    last=.false.)
    call write_json_real(unit, 'max. mass conservation (outlet)',   metrics%mass_residual(3),    last=.false.)
    ! momentum
    call write_json_real(unit, 'total kinetic energy',  metrics%kinetic_energy,   last=.false.)
    call write_json_real(unit, 'bulk velocity ux',      metrics%bulk_velocity(1), last=.false.)
    call write_json_real(unit, 'bulk velocity uy',      metrics%bulk_velocity(2), last=.false.)
    call write_json_real(unit, 'bulk velocity uz',      metrics%bulk_velocity(3), last=.false.)
    !call write_json_real(unit, 'wall shear integral',  metrics%wall_shear_integral, last=.false.)
    call write_json_real(unit, 'mean dpdx',             metrics%mean_dpdx,        last=.false.)
    if(is_thermo) then
    call write_json_real(unit, 'global energy balance', metrics%energy_balance,   last=.false.)
    else
    call write_json_real(unit, 'global pressure drop',  metrics%pressure_drop,    last=.true.)
    end if
    if(is_thermo) then
      ! mass flux
      call write_json_real(unit, 'bulk massflux gx',      metrics%bulk_massflux(1), last=.false.)
      call write_json_real(unit, 'bulk massflux gy',      metrics%bulk_massflux(2), last=.false.)
      call write_json_real(unit, 'bulk massflux gz',      metrics%bulk_massflux(3), last=.false.)
      call write_json_real(unit, 'bulk enthalpy',         metrics%bulk_enthalpy,    last=.false.)
      call write_json_real(unit, 'global energy balance', metrics%energy_balance,   last=.true.)
     !call write_json_real(unit, 'wall_heat_flux',                    metrics%wall_heat_flux,      last=.false.)
    end if  
    ! other
    write(unit,'(a)') '}'
    close(unit)
    return
  end subroutine write_metrics_json
!==========================================================================================================
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
!==========================================================================================================
!========================================================================================================== 
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
  subroutine write_monitor_ini(dm)
    use typeconvert_mod
    use wtformat_mod
    use udf_type_mod
    use io_files_mod
    use io_tools_mod
    use parameters_constant_mod
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
!----------------------------------------------------------------------------------------------------------
! create history file for total variables
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then
      call generate_pathfile_name(flname, dm%idom, trim(fl_bulk), dir_moni, 'log')
      inquire(file = trim(flname), exist = exist)
      if (exist) then
        !open(newunit = myunit, file = trim(flname), status="old", position="append", action="write")
      else
        open(newunit = myunit, file = trim(flname), status="new", action="write")
        write(myunit, *) "# domain-id : ", dm%idom, "pt-id : ", i
        write(myunit, *) "# columns description:"
        write(myunit, *) "# column 1 : time"
        write(myunit, *) "# column 2 : global mass balance"
        write(myunit, *) "# column 3 : max. mass conservation (interior)"
        write(myunit, *) "# column 4 : max. mass conservation (inlet)"  
        write(myunit, *) "# column 5 : max. mass conservation (outlet)"
        write(myunit, *) "# column 6 : total kinetic energy"
        write(myunit, *) "# column 7 : bulk velocity qx"
        write(myunit, *) "# column 8 : bulk velocity qy"
        write(myunit, *) "# column 9 : bulk velocity qz"
        !write(myunit, *) "# column 10 : wall shear integral"
        write(myunit, *) "# column 10 : mean dpdx"
        write(myunit, *) "# column 11 : global pressure drop"
        if(dm%is_thermo) then
          write(myunit, *) "# column 12 : bulk mass flux gx"
          write(myunit, *) "# column 13 : bulk mass flux gy"
          write(myunit, *) "# column 14 : bulk mass flux gz"
          write(myunit, *) "# column 15 : bulk enthalpy"
          write(myunit, *) "# column 16 : global enthalpy balance"
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
        write(myunit, *) "# time, mass drift, mass error at bulk, inlet, outlet, total mass change rate, kinetic energy change rate" ! to add more instantanous or statistics
        close(myunit)
      end if
    end if
!----------------------------------------------------------------------------------------------------------
    if(dm%proben <= 0) return

    if(nrank == 0) then
      call Print_debug_inline_msg("  Probed points for monitoring ...")
    end if
!----------------------------------------------------------------------------------------------------------    
    allocate( dm%probe_is_in(dm%proben) )
    dm%probe_is_in(:) = .false.

    allocate( probeid(3, dm%proben) )
    nplc = 0
    do i = 1, dm%proben
!----------------------------------------------------------------------------------------------------------
! probe points find the nearest cell centre, global index info, then convert to local index in x-pencil
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
! convert global id to local, based on x-pencil
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
! create probe history file for flow
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
! create probe history file for flow
!----------------------------------------------------------------------------------------------------------
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
          write(myunit, *) "# t, u, v, w, p, phi, T" ! to add more instantanous or statistics
        else
          write(myunit, *) "# t, u, v, w, p, phi" ! to add more instantanous or statistics
        end if
        close(myunit)
      end if
    end do
    call mpi_barrier(MPI_COMM_WORLD, ierror)

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==========================================================================================================
  subroutine write_monitor_bulk(fl, dm, tm)
    use typeconvert_mod
    use parameters_constant_mod
    use wtformat_mod
    use udf_type_mod
    use io_files_mod
    use io_tools_mod
    use solver_tools_mod
    use operations
    use find_max_min_ave_mod
    use cylindrical_rn_mod
    use regression_test_mod
    use bc_dirichlet_mod
    implicit none 

    type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    type(t_thermo), optional, intent(in) :: tm

    type(t_metrics) :: metrics
    character(len=120) :: flname
    character(len=120) :: keyword
    character(200) :: iotxt
    integer :: ioerr, myunit

    real(WP) :: bulk_MKE, bulk_q(3), bulk_g(3), bulk_T, bulk_m, bulk_h, bulk_rhoh, mean_dpdx, pressure_drop, &
                mass_balance
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

!----------------------------------------------------------------------------------------------------------
!   kinetic energy = 1/2*rho * (uu+vv+ww)
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
!   mass balance = density change + net mass flux through boundaries
!----------------------------------------------------------------------------------------------------------
    !density change introduced mass change 
    if(dm%is_thermo) then
      call Get_volumetric_average_3d(dm, dm%dccc, fl%drhodt, bulk_m, SPACE_INTEGRAL, 'drhodt')
    else
      bulk_m = ZERO
    end if
    !area averaged mass flux - x - boundary: rho*uy
    if(dm%ibcx_qx(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
        fbcx = dm%fbcx_gx
      else
        fbcx = dm%fbcx_qx
      end if
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, fbcx, bulk_fbcx, SPACE_INTEGRAL, 'fbcx')
    else
      bulk_fbcx = ZERO
    end if
    !area averaged mass flux - y - boundary: rho*uy
    if(dm%ibcy_qy(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
          call transpose_x_to_y(fl%gy, acpc_ypencil, dm%dcpc)
          call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
          fbcy = fbcy_c4c
        else
          fbcy = dm%fbcy_qyr
        end if
      call Get_area_average_2d_for_fbcy(dm, dm%dcpc, fbcy, bulk_fbcy, SPACE_INTEGRAL, 'fbcy', rdxdz=1)
    else
      bulk_fbcy = ZERO
    end if
    !area averaged mass flux - z - boundary: rho*uz
    if(dm%ibcz_qz(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
          fbcz = dm%fbcy_gz
        else
          fbcz = dm%fbcy_qz
        end if
      call Get_area_average_2d_for_fbcz(dm, dm%dccp, fbcz, bulk_fbcz, SPACE_INTEGRAL, 'fbcz')
    else
      bulk_fbcz = ZERO
    end if
    ! mass change rate, kg/s
    fl%tt_mass_change = bulk_m + &
                        bulk_fbcx(1) - bulk_fbcx(2) + &
                        bulk_fbcy(1) - bulk_fbcy(2) +&
                        bulk_fbcz(1) - bulk_fbcz(2)
!----------------------------------------------------------------------------------------------------------
!   Bulk quantities
!----------------------------------------------------------------------------------------------------------
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
    if(dm%icoordinate == ICYLINDRICAL) then
      acpc = fl%qy
      call multiple_cylindrical_rn(acpc, dm%dcpc, dm%rpi, 1, IPENCIL(1))
    else
      acpc = fl%qy
    end if
    call Get_volumetric_average_3d(dm, dm%dcpc, acpc,  bulk_q(2), SPACE_AVERAGE, 'uy')
    call Get_volumetric_average_3d(dm, dm%dpcc, fl%qx, bulk_q(1), SPACE_AVERAGE, 'ux')
    call Get_volumetric_average_3d(dm, dm%dccp, fl%qz, bulk_q(3), SPACE_AVERAGE, 'uz')
    !
    ! thermal flow quantities
    if(dm%is_thermo .and. present(tm)) then
      ! bulk momentum
      if(dm%icoordinate == ICYLINDRICAL) then
        acpc = fl%gy
        call multiple_cylindrical_rn(acpc, dm%dcpc, dm%rpi, 1, IPENCIL(1))
      else
        acpc = fl%gy
      end if
      call Get_volumetric_average_3d(dm, dm%dcpc, acpc,  bulk_q(2), SPACE_AVERAGE, 'rho*uy')
      call Get_volumetric_average_3d(dm, dm%dpcc, fl%gx, bulk_q(1), SPACE_AVERAGE, 'rho*ux')
      call Get_volumetric_average_3d(dm, dm%dccp, fl%gz, bulk_q(3), SPACE_AVERAGE, 'rho*uz')
      !
      ! bulk temperature
      call Get_volumetric_average_3d(dm, dm%dccc, tm%tTemp, bulk_T,  SPACE_AVERAGE, 'T')
      !
      ! bulk enthalpy
      call Get_volumetric_average_3d(dm, dm%dccc, tm%hEnth, bulk_h,  SPACE_AVERAGE, 'h')
      !
      ! enthalpy balance
      accc1 = tm%hEnth * fl%dDens
      call Get_volumetric_average_3d(dm, dm%dccc, accc1, bulk_rhoh,  SPACE_AVERAGE, 'rhoh')
    end if
!----------------------------------------------------------------------------------------------------------
!   save regression test metrics at the end of flow simulation
!----------------------------------------------------------------------------------------------------------
    if(fl%iteration == fl%nIterFlowEnd) then
      ! universal matrics
      metrics%mass_balance     = fl%tt_mass_change
      metrics%mass_residual(:) = fl%mcon(:) !
      metrics%kinetic_energy   = fl%tt_kinetic_energy
      metrics%bulk_velocity(:) = bulk_q(:)
      metrics%pressure_drop    = pressure_drop
      metrics%mean_dpdx        = mean_dpdx
      if(dm%is_thermo .and. present(tm)) then
        metrics%energy_balance   = bulk_rhoh
        metrics%bulk_massflux(:) = bulk_g(:)
        metrics%bulk_enthalpy    = bulk_h
      end if
      !
      call write_metrics_json(trim('regression_test_metrics.json'), metrics, dm%is_thermo)
    end if
!----------------------------------------------------------------------------------------------------------
! open file
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then
      ! write out history of key conservative variables
      call generate_pathfile_name(flname, dm%idom, trim(fl_mass), dir_moni, 'log')
      open(newunit = myunit, file = trim(flname), status = "old", action = "write", position = "append", &
          iostat = ioerr, iomsg = iotxt)
      if(ioerr /= 0) then
        call Print_error_msg('Problem openning conservation file')
      end if 
      write(myunit, '(7ES16.8)') fl%time, fl%mcon(:), fl%tt_mass_change, dMKEdt
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
          bulk_MKE, bulk_q(1:3), mean_dpdx, pressure_drop, &
          bulk_rhoh, bulk_g(1:3), bulk_h
      else
        write(myunit, '(1E13.5, 10ES16.8)') fl%time, fl%tt_mass_change, fl%mcon(1:3), &
          bulk_MKE, bulk_q(1:3), mean_dpdx, pressure_drop
      end if
      close(myunit)
    end if     

    return
  end subroutine

!==========================================================================================================
  subroutine write_monitor_probe(fl, dm, tm)
    use typeconvert_mod
    use parameters_constant_mod
    use wtformat_mod
    use udf_type_mod
    use io_files_mod
    use io_tools_mod
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
!----------------------------------------------------------------------------------------------------------
! based on x-pencil
!----------------------------------------------------------------------------------------------------------
    nplc = 0
    do i = 1, dm%proben
      if( .not. dm%probe_is_in(i) ) cycle
      nplc = nplc + 1
!----------------------------------------------------------------------------------------------------------
! open file
!----------------------------------------------------------------------------------------------------------
        keyword = "monitor_pt"//trim(int2str(i))//"_flow"
        call generate_pathfile_name(flname, dm%idom, keyword, dir_moni, 'dat')
        open(newunit = myunit, file = trim(flname), status = "old", action = "write", position = "append", &
            iostat = ioerr, iomsg = iotxt)
        if(ioerr /= 0) then
          !write (*, *) 'Problem openning probing file'
          !write (*, *) 'Message: ', trim (iotxt)
          call Print_error_msg('Problem opening probing file')
        end if
!----------------------------------------------------------------------------------------------------------
! write out local data
!----------------------------------------------------------------------------------------------------------
        ix = dm%probexid(1, nplc)
        iy = dm%probexid(2, nplc)
        iz = dm%probexid(3, nplc)
        !write(*,*) 'probe pts:', nrank, nplc, ix, iy, iz
        if(dm%is_thermo .and. present(tm)) then
          write(myunit, '(7ES13.5)') fl%time, fl%qx(ix, iy, iz), fl%qy(ix, iy, iz), fl%qz(ix, iy, iz), &
            fl%pres(ix, iy, iz), fl%pcor(ix, iy, iz), tm%tTemp(ix, iy, iz)
        else
          write(myunit, '(6ES13.5)') fl%time, fl%qx(ix, iy, iz), fl%qy(ix, iy, iz), fl%qz(ix, iy, iz), &
            fl%pres(ix, iy, iz), fl%pcor(ix, iy, iz)
        end if
        close(myunit)
    end do

    return
  end subroutine 
!==========================================================================================================
end module

!==========================================================================================================
!==========================================================================================================
