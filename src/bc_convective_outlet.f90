
module bc_convective_outlet_mod
  use parameters_constant_mod
  use decomp_2d
  use udf_type_mod
  implicit none 

  private :: get_convective_outlet_ux
  private :: calculate_fbcx_convective_outlet
  !public  :: update_flow_from_dyn_fbcx

  private  :: enforce_domain_mass_balance_dyn_fbc
  !private :: enforce_domain_energy_balance_dyn_fbc

  !public  :: update_dyn_fbcx_from_flow
  public  :: update_fbcx_convective_outlet_flow
  public  :: update_fbcx_convective_outlet_thermo

  contains

!==========================================================================================================
  subroutine get_convective_outlet_ux(fl, dm, uxdx)
    use wtformat_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow), intent(in) :: fl
    real(WP), intent(out) :: uxdx
    logical :: flg_bc_conv

    real(WP) :: uxmax, uxmin, uxmax_work, uxmin_work, uintf
    integer :: nn, k, j
    
    if(.not. dm%is_conv_outlet) return

    uxmax = MINN
    uxmin = MAXP
    nn = dm%dpcc%xsz(1) -  1
    do k = 1, dm%dpcc%xsz(3)
      do j = 1, dm%dpcc%xsz(2)
        uintf = fl%qx(nn, j, k) !( fl%qx(nn, j, k) + dm%fbcx_qx(2, j, k) ) * HALF ! at i=nc
        if(fl%qx(nn, j, k) > uxmax) uxmax = uintf
        if(fl%qx(nn, j, k) < uxmin) uxmin = uintf
      end do
    end do

    !write(*,*) 'outlet bc', fl%qx(nn, :, 2)

    call MPI_ALLREDUCE(uxmax, uxmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    call MPI_ALLREDUCE(uxmin, uxmin_work, 1, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)

    uxdx = HALF * (uxmax_work + uxmin_work)
    uxdx = uxdx * dm%h1r(1)
#ifdef DEBUG_STEPS 
    if(nrank == 0) write(*, '(10X, A, 3ES13.5, A, 1I5.1)') 'convective outlet uxmax, min, ave = ', &
      uxmax_work, uxmin_work, HALF * (uxmax_work + uxmin_work), ' at iter(real) =', fl%iteration
#endif

    return
  end subroutine
!==========================================================================================================
  subroutine calculate_fbcx_convective_outlet(fbcx_var, uxdx, fbc_rhs0, var, dtmp, dm, isub)

    type(DECOMP_INFO), intent(in) :: dtmp 
    type(t_domain), intent(in) :: dm
    real(WP), dimension(4,           dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: fbcx_var
    real(WP), dimension(             dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: fbc_rhs0
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: var
    real(WP), intent(in) :: uxdx
    integer, intent(in) :: isub

    integer :: j, k, nn
    logical :: is_x
    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    
    if(.not. dm%is_conv_outlet) return

    ! all based on x pencil
    ! dphi/dt + ux * dphi/dx = 0
    ! data storage:
    ! ux = cell centre
    ! 
    if(dtmp%xsz(1) == dm%dpcc%xsz(1)) then
      ! qx,  -----|-----||
      !          qx     bc2/bc4 
      is_x = .true.
      nn = dm%dpcc%xsz(1) - 1
    else
      ! any vars else, like v, w, phi, T, etc 
      ! qy,  --x--|--x--||--x--|
      !       qy    qy  bc2 bc4
      is_x = .false.
      nn = dm%dccc%xsz(1)
    end if

    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
      ! add explicit terms : convection rhs
        rhs_explicit_current = fbcx_var(4, j, k) - var(nn, j, k) ! at cell centre for ux, and bc point for otherse
        rhs_explicit_current = - rhs_explicit_current * uxdx
        rhs_explicit_last    = fbc_rhs0(j, k)
        rhs_total = ( dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last ) * dm%dt
        fbc_rhs0(j, k) = rhs_explicit_current
      ! calculate updated b.c. values
        fbcx_var(4, j, k) = fbcx_var(4, j, k) + rhs_total
      end do
    end do

    if(is_x) then
      ! ux, fbc = var(last point)
      do k = 1, dtmp%xsz(3)
        do j = 1, dtmp%xsz(2)
          fbcx_var(2, j, k) = fbcx_var(4, j, k)
          var(dm%dpcc%xsz(1), j, k) = fbcx_var(2, j, k)
        end do
      end do
    else
      do k = 1, dtmp%xsz(3)
        do j = 1, dtmp%xsz(2)
          fbcx_var(2, j, k) = (fbcx_var(4, j, k) + var(nn, j, k)) * HALF
        end do
      end do
    end if

  end subroutine 

!==========================================================================================================
  ! subroutine update_dyn_fbcx_from_flow(dm, ux, uy, uz, fbcx1, fbcx2, fbcx3)
  !   use print_msg_mod
  !   implicit none 
  !   type(t_domain), intent(in) :: dm
  !   real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in) :: ux
  !   real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in) :: uy
  !   real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in) :: uz
  !   real(WP), dimension(4,              dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (inout) :: fbcx1
  !   real(WP), dimension(4,              dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (inout) :: fbcx2
  !   real(WP), dimension(4,              dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (inout) :: fbcx3

  !   if( .not. dm%is_conv_outlet) return

  !   ! x - pencil 
  !   if(dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
  !     !fbcx1(2, :, :) = ux(dm%dpcc%xsz(1), :, :)
  !     fbcx1(4, :, :) = fbcx1(2, :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 2) == IBC_CONVECTIVE) then
  !     fbcx2(4, :, :) = TWO * fbcx2(2, :, :) - uy(dm%dcpc%xsz(1), :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 3) == IBC_CONVECTIVE) then
  !     fbcx3(4, :, :) = TWO * fbcx3(2, :, :) - uz(dm%dccp%xsz(1), :, :)
  !   end if

  !   return
  ! end subroutine


!==========================================================================================================
  ! subroutine update_flow_from_dyn_fbcx(dm, ux, uy, uz, fbcx1, fbcx2, fbcx3)
  !   use udf_type_mod
  !   use parameters_constant_mod
  !   use print_msg_mod
  !   implicit none 
  !   type(t_domain), intent(in) :: dm
  !   real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (inout) :: ux
  !   real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (inout) :: uy
  !   real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (inout) :: uz
  !   real(WP), dimension(4,              dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in)    :: fbcx1
  !   real(WP), dimension(4,              dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in)    :: fbcx2
  !   real(WP), dimension(4,              dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in)    :: fbcx3

  !   if( .not. dm%is_conv_outlet) return

  !   ! x - pencil 
  !   if(dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
  !     ux(dm%dpcc%xsz(1), :, :) = fbcx1(2, :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 2) == IBC_CONVECTIVE) then
  !     uy(dm%dcpc%xsz(1), :, :) = TWO * fbcx2(2, :, :) - fbcx2(4, :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 3) == IBC_CONVECTIVE) then
  !     uz(dm%dccp%xsz(1), :, :) =  TWO * fbcx3(2, :, :) - fbcx3(4, :, :)
  !   end if
    

  !   return
  ! end subroutine
  !==========================================================================================================
  subroutine enforce_domain_mass_balance_dyn_fbc(fl, dm)
    use wtformat_mod
    use bc_dirichlet_mod
    use find_max_min_ave_mod
    use cylindrical_rn_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    type(DECOMP_INFO) :: dtmp
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx
    real(WP), dimension(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) :: fbcy
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) :: fbcz
    real(WP) :: scale
    real(WP) :: fbcm_x(2), fbcm_y(2), fbcm_z(2)
    real(WP) :: bulkm
    logical :: iconv(3)
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    
    ! only 1 direction could be convective outlet
    if (.not. dm%is_conv_outlet) return

    if(dm%is_thermo) then
      call Get_volumetric_average_3d_for_var_xcx(dm, dm%dccc, fl%drhodt, bulkm, SPACE_INTEGRAL, 'drhodt')
    else
      bulkm = ZERO
    end if

    iconv = .false.
    fbcm_x = ZERO
    fbcm_y = ZERO
    fbcm_z = ZERO
!----------------------------------------------------------------------------------------------------------
! x - inlet/outlet, ux = qx
!----------------------------------------------------------------------------------------------------------
    if(dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
      iconv(1) = .true.
      if(dm%is_thermo) then
        fbcx = dm%fbcx_gx
      else
        fbcx = dm%fbcx_qx
      end if
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, fbcx, fbcm_x, SPACE_INTEGRAL, 'fbcx')
    else if(dm%ibcy_nominal(2, 2) == IBC_CONVECTIVE) then
!----------------------------------------------------------------------------------------------------------
! y - inlet/outlet - uy = qr / r
!----------------------------------------------------------------------------------------------------------
      iconv(2) = .true.
      ! if(dm%icase == ICASE_PIPE) then
      !   if(dm%is_thermo) then
      !     acpc_xpencil = fl%gy
      !   else
      !     acpc_xpencil = fl%qy
      !   end if
      !   call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)
      !   call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rpi, 1, IPENCIL(2)) ! qr/r
      !   call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(2), is_reversed = .true.)
      !   call extract_dirichlet_fbcy(fbcy, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
      ! else 
      !   if(dm%is_thermo) then
      !     fbcy = dm%fbcy_gy
      !   else
      !     fbcy = dm%fbcy_qy
      !   end if
      !   if(dm%icoordinate == ICYLINDRICAL) then
      !     call multiple_cylindrical_rn_x4x(fbcy, dm%dcpc, dm%rpi, 1, IPENCIL(2))
      !   end if
      ! end if
      if(dm%icoordinate = ICYLINDRICAL) then
        fbcy = dm%fbcy_qyr
      else
        fbcy = dm%fbcy_qy
      end if
      call Get_area_average_2d_for_fbcy(dm, dm%dcpc, fbcy, fbcm_y, SPACE_INTEGRAL, 'fbcy')
    else if (dm%ibcz_nominal(2, 3) == IBC_CONVECTIVE) then
      iconv(3) = .true.
!----------------------------------------------------------------------------------------------------------
! z - inlet/outlet - qz = u_theta
!----------------------------------------------------------------------------------------------------------
      if(dm%is_thermo) then
        fbcz = dm%fbcz_gz
      else
        fbcz = dm%fbcz_qz
      end if
      ! if(dm%icoordinate == ICYLINDRICAL) then
      !   call multiple_cylindrical_rn_xx4(fbcz, dm%dccp, dm%rci, 1, IPENCIL(3))
      ! end if
      call Get_area_average_2d_for_fbcz(dm, dm%dccp, fbcz, fbcm_z, SPACE_INTEGRAL, 'fbcz')
    end if
!----------------------------------------------------------------------------------------------------------
! scaling factor for a mass conservation
! fbcm_x(1) - scaling * fbcm_x(2) + bulkm = 0
! scale the dynamic bc
!----------------------------------------------------------------------------------------------------------
    if(iconv(1)) then
      scale = ( fbcm_x(1) + bulkm ) / fbcm_x(2)
      fbcx(2, :, :) = fbcx(2, :, :) * scale
      fbcx(4, :, :) = fbcx(2, :, :)
    else if(iconv(2)) then
      scale = ( fbcm_y(1) + bulkm ) / fbcm_y(2)
      fbcy(:, 2, :) = fbcy(:, 2, :) * scale
      fbcy(:, 4, :) = fbcy(:, 2, :)
    else if(iconv(3)) then
      scale = ( fbcm_z(1) + bulkm ) / fbcm_z(2)
      fbcz(:, :, 2) = fbcz(:, :, 2) * scale
      fbcz(:, :, 4) = fbcz(:, :, 2)
    else
    end if
!----------------------------------------------------------------------------------------------------------
! back to real fbc
!----------------------------------------------------------------------------------------------------------
    if(dm%is_thermo) then
      if( dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) dm%fbcx_gx(2, :, :) = fbcx(2, :, :) 
      if( dm%ibcy_nominal(2, 2) == IBC_CONVECTIVE) dm%fbcy_gy(2, :, :) = fbcy(2, :, :) 
      if( dm%ibcz_nominal(2, 3) == IBC_CONVECTIVE) dm%fbcz_gz(2, :, :) = fbcz(2, :, :) 
      if( dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) dm%fbcx_qx(2, :, :) = dm%fbcx_gx(2, :, :) / dm%fbcx_ftp(2, :, :)%d
      if( dm%ibcy_nominal(2, 2) == IBC_CONVECTIVE) dm%fbcy_qy(2, :, :) = dm%fbcy_gy(2, :, :) / dm%fbcy_ftp(2, :, :)%d
      if( dm%ibcz_nominal(2, 3) == IBC_CONVECTIVE) dm%fbcz_qz(2, :, :) = dm%fbcz_gz(2, :, :) / dm%fbcz_ftp(2, :, :)%d
      !call update_flow_from_dyn_fbcx(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcx_gy, dm%fbcx_gz)
    else
      if( dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) dm%fbcx_qx(2, :, :) = fbcx(2, :, :)
      if( dm%ibcy_nominal(2, 2) == IBC_CONVECTIVE) dm%fbcy_qy(2, :, :) = fbcy(2, :, :)
      if( dm%ibcz_nominal(2, 3) == IBC_CONVECTIVE) dm%fbcz_qz(2, :, :) = fbcz(2, :, :)
    end if
    !call update_flow_from_dyn_fbcx(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcx_qy, dm%fbcx_qz)
#ifdef DEBUG_STEPS 
    if(nrank == 0) then 
      write(*, *) "m_in, m_out, m_bulk, m_net, scale"
      if(iconv(1)) &
      write (*, '(10X, A, 4ES13.5, 1F16.13)') 'x: ', fbcm_x(1), fbcm_x(2), bulkm, fbcm_x(1)-fbcm_x(2)+bulkm, scale
      if(iconv(2)) &
      write (*, '(10X, A, 4ES13.5, 1F16.13)') 'y: ', fbcm_y(1), fbcm_y(2), bulkm, fbcm_y(1)-fbcm_y(2)+bulkm, scale
      if(iconv(3)) &
      write (*, '(10X, A, 4ES13.5, 1F16.13)') 'z: ', fbcm_z(1), fbcm_z(2), bulkm, fbcm_z(1)-fbcm_z(2)+bulkm, scale
    end if
#endif
    return
  end subroutine enforce_domain_mass_balance_dyn_fbc

!==========================================================================================================
  subroutine update_fbcx_convective_outlet_flow(fl, dm, isub)
    use bc_dirichlet_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    
    real(WP) :: uxdx
    integer :: i

    if(.not. dm%is_conv_outlet) return
#ifdef DEBUG_STEPS
    if(nrank == 0) call Print_debug_inline_msg("Calculate convective outlet for flow ...")
#endif
    ! work on fbcx, not fl directly
    call get_convective_outlet_ux(fl, dm, uxdx)
    if ( .not. dm%is_thermo) then
      
      if(dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
        call calculate_fbcx_convective_outlet(dm%fbcx_qx(:, :, :), uxdx, fl%fbcx_qx_rhs0(:, :), fl%qx, dm%dpcc, dm, isub)
      end if
      if(dm%ibcx_nominal(2, 2) == IBC_CONVECTIVE) then
        call calculate_fbcx_convective_outlet(dm%fbcx_qy(:, :, :), uxdx, fl%fbcx_qy_rhs0(:, :), fl%qy, dm%dcpc, dm, isub)
      end if 
      if(dm%ibcx_nominal(2, 3) == IBC_CONVECTIVE) then
        call calculate_fbcx_convective_outlet(dm%fbcx_qz(:, :, :), uxdx, fl%fbcx_qz_rhs0(:, :), fl%qz, dm%dccp, dm, isub)
      end if
    else
      ! check , whether it is better to use qx = gx / density ?
      if(dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
        call calculate_fbcx_convective_outlet(dm%fbcx_qx(:, :, :), uxdx, fl%fbcx_qx_rhs0(:, :), fl%qx, dm%dpcc, dm, isub)
        call calculate_fbcx_convective_outlet(dm%fbcx_gx(:, :, :), uxdx, fl%fbcx_gx_rhs0(:, :), fl%gx, dm%dpcc, dm, isub)
      end if
      if(dm%ibcx_nominal(2, 2) == IBC_CONVECTIVE) then
        call calculate_fbcx_convective_outlet(dm%fbcx_qy(:, :, :), uxdx, fl%fbcx_qy_rhs0(:, :), fl%qy, dm%dcpc, dm, isub)
        call calculate_fbcx_convective_outlet(dm%fbcx_gy(:, :, :), uxdx, fl%fbcx_gy_rhs0(:, :), fl%gy, dm%dcpc, dm, isub)
      end if
      if(dm%ibcx_nominal(2, 3) == IBC_CONVECTIVE) then
        call calculate_fbcx_convective_outlet(dm%fbcx_qz(:, :, :), uxdx, fl%fbcx_qz_rhs0(:, :), fl%qz, dm%dccp, dm, isub)
        call calculate_fbcx_convective_outlet(dm%fbcx_gz(:, :, :), uxdx, fl%fbcx_gz_rhs0(:, :), fl%gz, dm%dccp, dm, isub)
      end if

    end if

    call enforce_domain_mass_balance_dyn_fbc(fl, dm)

    if ( .not. dm%is_thermo) then
      call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
    else
      ! check , whether it is better to use qx = gx / density ?
      call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
      call enforce_velo_from_fbc(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcy_gy, dm%fbcz_gz)
    end if

    return
  end subroutine

!==========================================================================================================
  subroutine update_fbcx_convective_outlet_thermo(fl, tm, dm, isub)
    use thermo_info_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    
    real(WP) :: uxdx
    integer :: j, k
    real(WP), dimension(4, dm%dccc%xsz(2), dm%dccc%xsz(3)) :: a4cc_xpencil

    if ( .not. dm%is_thermo) return
    if ( .not. dm%is_conv_outlet) return

#ifdef DEBUG_STEPS
    if(nrank == 0) call Print_debug_inline_msg("Calculate convective outlet for thermo ...")
#endif
    call get_convective_outlet_ux(fl, dm, uxdx)

    if(dm%ibcx_nominal(2, 5) == IBC_CONVECTIVE) then
      a4cc_xpencil = dm%fbcx_ftp(:, :, :)%rhoh
      call calculate_fbcx_convective_outlet(a4cc_xpencil, uxdx, tm%fbcx_rhoh_rhs0(:, :), tm%rhoh, dm%dccc, dm, isub)
      dm%fbcx_ftp(:, :, :)%rhoh = a4cc_xpencil
      do j = 1, size(dm%fbcx_ftp, 2)
        do k = 1, size(dm%fbcx_ftp, 3)
          call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(2, j, k))
          call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(4, j, k))
        end do
      end do
    end if

    !call enforce_domain_energy_balance_dyn_fbc(fl, dm) ! to check necessary? 

    return
  end subroutine

end module
