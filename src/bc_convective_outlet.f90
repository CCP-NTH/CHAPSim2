
module bc_convective_outlet_mod
  use parameters_constant_mod
  use decomp_2d
  use udf_type_mod
  implicit none 

  private :: get_convective_outlet_velocity
  private :: calculate_fbcx_convective_outlet
  private :: calculate_fbcz_convective_outlet
  !public  :: update_flow_from_dyn_fbcx

  private  :: enforce_domain_mass_balance_dyn_fbc
  !private :: enforce_domain_energy_balance_dyn_fbc


  public :: update_fbcx_convective_outlet_flow
  public :: update_fbcz_convective_outlet_flow

  public :: update_fbcx_convective_outlet_thermo
  public :: update_fbcz_convective_outlet_thermo

  contains

!==========================================================================================================
  subroutine get_convective_outlet_velocity(dm, uxdx)
    use wtformat_mod
    use math_mod
    use print_msg_mod
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: uxdx
    ! local variables
    real(WP) :: uxmax, uxmin, uxmax_work, uxmin_work, uintf, dx
    integer :: i, j, k, nsz(3)
    ! conditions
    if(.not. dm%is_conv_outlet(1) .and. &
       .not. dm%is_conv_outlet(3)) return
    if(dm%is_conv_outlet(1) .and. &
       dm%is_conv_outlet(3)) call Print_error_msg("Not supported.")

    uxmax = MINP
    uxmin = MAXP
    !
    if(dm%is_conv_outlet(1)) then 
      ! x - convective velocity
      do k = 1, size(dm%fbcx_qx, 3)
        do j = 1, size(dm%fbcx_qx, 2)
          uintf = dm%fbcx_qx(2, j, k)
          uintf = abs_wp(uintf)
          if(uintf > uxmax) uxmax = uintf
          if(uintf < uxmin) uxmin = uintf
        end do
      end do
      dx = dm%h1r(1)
    else if(dm%is_conv_outlet(3)) then
      ! y - convective velocity
      do j = 1, size(dm%fbcz_qz, 2)
        do i = 1, size(dm%fbcz_qz, 1)
          uintf = dm%fbcz_qz(i, j, 2)
          uintf = abs_wp(uintf)
          if(uintf > uxmax) uxmax = uintf
          if(uintf < uxmin) uxmin = uintf
        end do
      end do
      dx = dm%h1r(3)
    else
      call Print_error_msg("Not supported.")
    end if
    ! find the global max/min
    call MPI_ALLREDUCE(uxmax, uxmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    call MPI_ALLREDUCE(uxmin, uxmin_work, 1, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)
    ! calc convective velocity
    uxdx = HALF * (uxmax_work + uxmin_work)
    uxdx = uxdx * dx
#ifdef DEBUG_STEPS 
    if(nrank == 0) write(*, '(10X, A, 3ES13.5)') 'convective outlet velocity Max., Min., Ave = ', &
      uxmax_work, uxmin_work, HALF * (uxmax_work + uxmin_work)
#endif

    return
  end subroutine
!==========================================================================================================
  subroutine calculate_fbcx_convective_outlet(fbcx_var, uxdx, fbc_rhs0, var_xpencil, dm, isub)
    ! all based on x pencil
    ! dphi/dt + ux * dphi/dx = 0
    ! data storage:
    ! qx,  -----|-----||-----|
    !          qx     bc2   bc4 
    ! qy,  --x--|--x--||--x--|
    !       qy    qy  bc2 bc4
    ! 
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    real(WP), dimension(:, :, :), intent(inout), contiguous :: fbcx_var
    real(WP), dimension(:, :),    intent(inout), contiguous :: fbc_rhs0
    real(WP), dimension(:, :),    intent(in),    contiguous :: var_xpencil
    real(WP), intent(in) :: uxdx
    integer, intent(in)  :: isub
    ! local variables
    integer :: j, k
    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    !
    if(.not. dm%is_conv_outlet(1)) return
    !
    do k = 1, size(fbcx_var, 3)
      do j = 1, size(fbcx_var, 2)
      ! add explicit terms : convection rhs
        rhs_explicit_current = fbcx_var(4, j, k) - var_xpencil(j, k) ! at cell centre for ux, and bc point for otherse
        rhs_explicit_current = - rhs_explicit_current * uxdx
        rhs_explicit_last    = fbc_rhs0(j, k)
        rhs_total = ( dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last ) * dm%dt
        fbc_rhs0(j, k) = rhs_explicit_current
      ! calculate updated b.c. values
        fbcx_var(2, j, k) = fbcx_var(2, j, k) + rhs_total
        fbcx_var(4, j, k) = TWO * fbcx_var(2, j, k) - var_xpencil(j, k)
      end do
    end do
    return
  end subroutine 
!==========================================================================================================
  subroutine calculate_fbcz_convective_outlet(fbcz_var, uzdz, fbc_rhs0, var2d_zpencil, dm, isub)
    ! all based on z pencil
    ! dphi/dt + ux * dphi/dx = 0
    ! data storage:
    ! qz,  -----|-----||-----|
    !          qz     bc2   bc4 
    ! qy,  --x--|--x--||--x--|
    !       qy    qy  bc2 bc4
    ! 
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    real(WP), dimension(:, :, :), intent(inout), contiguous :: fbcz_var
    real(WP), dimension(:, :),    intent(inout), contiguous :: fbc_rhs0
    real(WP), dimension(:, :),    intent(in),    contiguous :: var2d_zpencil
    real(WP), intent(in) :: uzdz
    integer, intent(in)  :: isub
    ! local variables
    integer :: i, j
    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    !
    if(.not. dm%is_conv_outlet(3)) return
    !
    do j = 1, size(fbcz_var, 2)
      do i = 1, size(fbcz_var, 1)
      ! add explicit terms : convection rhs
        rhs_explicit_current = fbcz_var(i, j, 4) - var2d_zpencil(i, j) ! at cell centre for ux, and bc point for otherse
        rhs_explicit_current = - rhs_explicit_current * uzdz
        rhs_explicit_last    = fbc_rhs0(i, j)
        rhs_total = ( dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last ) * dm%dt
        fbc_rhs0(i, j) = rhs_explicit_current
      ! calculate updated b.c. values
        fbcz_var(i, j, 2) = fbcz_var(i, j, 2) + rhs_total
        fbcz_var(i, j, 4) = TWO * fbcz_var(i, j, 2) - var2d_zpencil(i, j)
      end do
    end do
    return
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
  !   if(dm%is_conv_outlet(1)) then
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
  !   if(dm%is_conv_outlet(1)) then
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
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: drhodt
    
    ! only 1 direction could be convective outlet
    if(.not. dm%is_conv_outlet(1) .and. &
       .not. dm%is_conv_outlet(3)) return

    if(dm%is_thermo) then
      drhodt = fl%drhodt
      call Get_volumetric_average_3d(dm, dm%dccc, drhodt, bulkm, SPACE_INTEGRAL, 'drhodt')
    else
      bulkm = ZERO
    end if

    fbcm_x = ZERO
    fbcm_y = ZERO
    fbcm_z = ZERO
!----------------------------------------------------------------------------------------------------------
! x - inlet/outlet, ux = qx
!----------------------------------------------------------------------------------------------------------
    if(dm%is_conv_outlet(1)) then
      if(dm%is_thermo) then
        fbcx = dm%fbcx_gx
      else
        fbcx = dm%fbcx_qx
      end if
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, fbcx, fbcm_x, SPACE_INTEGRAL, 'fbcx')
    else if(dm%is_conv_outlet(2)) then
!----------------------------------------------------------------------------------------------------------
! y - inlet/outlet - uy = qr / r
!----------------------------------------------------------------------------------------------------------
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
      if(dm%is_thermo) then
        fbcy = dm%fbcy_gy
      else
        fbcy = dm%fbcy_qy
      end if
      call Get_area_average_2d_for_fbcy(dm, dm%dcpc, fbcy, fbcm_y, SPACE_INTEGRAL, 'fbcy', rdxdz=1)
    else if (dm%is_conv_outlet(3)) then
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
    if(dm%is_conv_outlet(1)) then
      scale = ( fbcm_x(1) + bulkm ) / fbcm_x(2)
      fbcx(2, :, :) = fbcx(2, :, :) * scale
      fbcx(4, :, :) = fbcx(2, :, :)
    else if(dm%is_conv_outlet(2)) then
      scale = ( fbcm_y(1) + bulkm ) / fbcm_y(2)
      fbcy(:, 2, :) = fbcy(:, 2, :) * scale
      fbcy(:, 4, :) = fbcy(:, 2, :)
    else if(dm%is_conv_outlet(3)) then
      scale = ( fbcm_z(1) + bulkm ) / fbcm_z(2)
      fbcz(:, :, 2) = fbcz(:, :, 2) * scale
      fbcz(:, :, 4) = fbcz(:, :, 2)
    else
    end if
!----------------------------------------------------------------------------------------------------------
! back to real fbc
!----------------------------------------------------------------------------------------------------------
    if(dm%is_thermo) then
      if( dm%is_conv_outlet(1)) then
        dm%fbcx_gx(2, :, :) = fbcx(2, :, :) 
        dm%fbcx_qx(2, :, :) = dm%fbcx_gx(2, :, :) / dm%fbcx_ftp(2, :, :)%d
      else if( dm%is_conv_outlet(2)) then
        dm%fbcy_gy(2, :, :) = fbcy(2, :, :) 
        dm%fbcy_qy(2, :, :) = dm%fbcy_gy(2, :, :) / dm%fbcy_ftp(2, :, :)%d
      else if( dm%is_conv_outlet(3)) then
        dm%fbcz_gz(2, :, :) = fbcz(2, :, :) 
        dm%fbcz_qz(2, :, :) = dm%fbcz_gz(2, :, :) / dm%fbcz_ftp(2, :, :)%d
      else
      end if
      !call update_flow_from_dyn_fbcx(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcx_gy, dm%fbcx_gz)
    else
      if( dm%is_conv_outlet(1)) dm%fbcx_qx(2, :, :) = fbcx(2, :, :)
      if( dm%is_conv_outlet(2)) dm%fbcy_qy(2, :, :) = fbcy(2, :, :)
      if( dm%is_conv_outlet(3)) dm%fbcz_qz(2, :, :) = fbcz(2, :, :)
    end if
    !call update_flow_from_dyn_fbcx(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcx_qy, dm%fbcx_qz)
#ifdef DEBUG_STEPS 
    if(nrank == 0) then 
      write(*, *) "m_in, m_out, m_bulk, m_net, scale"
      if(dm%is_conv_outlet(1)) &
      write (*, '(10X, A, 4ES13.5, 1F16.11)') 'x: ', fbcm_x(1), fbcm_x(2), bulkm, fbcm_x(1)-fbcm_x(2)+bulkm, scale
      if(dm%is_conv_outlet(2)) &
      write (*, '(10X, A, 4ES13.5, 1F16.11)') 'y: ', fbcm_y(1), fbcm_y(2), bulkm, fbcm_y(1)-fbcm_y(2)+bulkm, scale
      if(dm%is_conv_outlet(3)) &
      write (*, '(10X, A, 4ES13.5, 1F16.11)') 'z: ', fbcm_z(1), fbcm_z(2), bulkm, fbcm_z(1)-fbcm_z(2)+bulkm, scale
    end if
#endif
    return
  end subroutine enforce_domain_mass_balance_dyn_fbc

!==========================================================================================================
  subroutine update_fbcx_convective_outlet_flow(fl, dm, isub)
    use bc_dirichlet_mod
    use convert_primary_conservative_mod
    implicit none
    ! arguments
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP), dimension(dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a0cc
    real(WP), dimension(dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a0pc
    real(WP), dimension(dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a0cp
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a4cc
    real(WP), dimension(4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a4pc
    real(WP), dimension(4, dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a4cp
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2)) :: acc0
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2)) :: apc0
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2)) :: acp0
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2), 4) :: acc4
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4) :: apc4
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) :: acp4
    real(WP) :: uxdx
    integer :: i
    ! condition + default x-pencil
    if(.not. dm%is_conv_outlet(1)) return
    ! assign proper variables to calculate convective bc.
    if ( .not. dm%is_thermo) then
      a0cc = fl%qx(dm%dpcc%xsz(1)-1, :, :)
      a0pc = fl%qy(dm%dcpc%xsz(1),   :, :)
      a0cp = fl%qz(dm%dccp%xsz(1),   :, :)
      a4cc = dm%fbcx_qx
      a4pc = dm%fbcx_qy
      a4cp = dm%fbcx_qz
    else
      a0cc = fl%gx(dm%dpcc%xsz(1)-1, :, :)
      a0pc = fl%gy(dm%dcpc%xsz(1),   :, :)
      a0cp = fl%gz(dm%dccp%xsz(1),   :, :)
      a4cc = dm%fbcx_gx
      a4pc = dm%fbcx_gy
      a4cp = dm%fbcx_gz
    end if
    ! get u_c/dx
    call get_convective_outlet_velocity(dm, uxdx)
    ! update b.c.
    call calculate_fbcx_convective_outlet(a4cc, uxdx*TWO, fl%fbcx_a0cc_rhs0, a0cc, dm, isub)
    call calculate_fbcx_convective_outlet(a4pc, uxdx,     fl%fbcx_a0pc_rhs0, a0pc, dm, isub)
    call calculate_fbcx_convective_outlet(a4cp, uxdx,     fl%fbcx_a0cp_rhs0, a0cp, dm, isub)
    ! fbc data back
    if ( .not. dm%is_thermo) then
      dm%fbcx_qx = a4cc
      dm%fbcx_qy = a4pc
      dm%fbcx_qz = a4cp
      fl%qx(dm%dpcc%xsz(1), :, :) = dm%fbcx_qx(2, :, :)
    else
      dm%fbcx_gx = a4cc
      dm%fbcx_gy = a4pc
      dm%fbcx_gz = a4cp
      fl%gx(dm%dpcc%xsz(1), :, :) = dm%fbcx_gx(2, :, :)
    end if
    call enforce_domain_mass_balance_dyn_fbc(fl, dm)

    if ( .not. dm%is_thermo) then
      call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
    else
      ! todo-check , whether it is better to use qx = gx / density ?
      !call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
      call enforce_velo_from_fbc(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcy_gy, dm%fbcz_gz)
      call convert_primary_conservative(dm, fl%dDens, IG2Q, IBND)
    end if

    return
  end subroutine
!==========================================================================================================
  subroutine update_fbcz_convective_outlet_flow(fl, dm, isub)
    use bc_dirichlet_mod
    use convert_primary_conservative_mod
    use transpose_extended_mod
    implicit none
    ! arguments
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2))    :: acc0
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2))    :: apc0
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2))    :: acp0
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2), 4) :: acc4
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4) :: apc4
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) :: acp4
    real(WP), dimension(dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a0cc
    real(WP), dimension(dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a0pc
    real(WP), dimension(dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a0cp
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a4cc
    real(WP), dimension(4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a4pc
    real(WP), dimension(4, dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a4cp
    real(WP) :: uzdz
    integer :: i
    ! condition
    if(.not. dm%is_conv_outlet(3)) return
    ! assign proper variables to calculate convective bc.
    if ( .not. dm%is_thermo) then
      call transpose_to_z_pencil(fl%qx, apcc_zpencil, dm%dpcc, IPENCIL(1))
      call transpose_to_z_pencil(fl%qy, acpc_zpencil, dm%dcpc, IPENCIL(1))
      call transpose_to_z_pencil(fl%qz, accp_zpencil, dm%dccp, IPENCIL(1))
      apc0 = apcc_zpencil(:, :, dm%dpcc%zsz(3)  )
      acp0 = acpc_zpencil(:, :, dm%dcpc%zsz(3)  )
      acc0 = accp_zpencil(:, :, dm%dccp%zsz(3)-1)
      apc4 = dm%fbcz_qx
      acp4 = dm%fbcz_qy
      acc4 = dm%fbcz_qz
    else
      call transpose_to_z_pencil(fl%gx, apcc_zpencil, dm%dpcc, IPENCIL(1))
      call transpose_to_z_pencil(fl%gy, acpc_zpencil, dm%dcpc, IPENCIL(1))
      call transpose_to_z_pencil(fl%gz, accp_zpencil, dm%dccp, IPENCIL(1))
      apc0 = apcc_zpencil(:, :, dm%dpcc%zsz(3)  )
      acp0 = acpc_zpencil(:, :, dm%dcpc%zsz(3)  )
      acc0 = accp_zpencil(:, :, dm%dccp%zsz(3)-1)
      apc4 = dm%fbcz_gx
      acp4 = dm%fbcz_gy
      acc4 = dm%fbcz_gz
    end if
    ! get u_c/dz
    call get_convective_outlet_velocity(dm, uzdz)
    ! update b.c.
    call calculate_fbcz_convective_outlet(apc4, uzdz,     fl%fbcz_apc0_rhs0, apc0, dm, isub)
    call calculate_fbcz_convective_outlet(acp4, uzdz,     fl%fbcz_acp0_rhs0, acp0, dm, isub)
    call calculate_fbcz_convective_outlet(acc4, uzdz*TWO, fl%fbcz_acc0_rhs0, acc0, dm, isub)
    call enforce_domain_mass_balance_dyn_fbc(fl, dm)

    if ( .not. dm%is_thermo) then
      call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
    else
      ! todo-check , whether it is better to use qx = gx / density ?
      !call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
      call enforce_velo_from_fbc(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcy_gy, dm%fbcz_gz)
      call convert_primary_conservative(dm, fl%dDens, IG2Q, IBND)
    end if

    return
  end subroutine
!==========================================================================================================
  subroutine update_fbcx_convective_outlet_thermo(ux, tm, dm, isub)
    use thermo_info_mod
    implicit none
    ! arguments
    real(WP), intent(in) :: ux(:, :, :)
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP) :: uxdx
    integer :: j, k
    real(WP), dimension(4, dm%dccc%xsz(2), dm%dccc%xsz(3)) :: a4cc_xpencil
    real(WP), dimension(   dm%dccc%xsz(2), dm%dccc%xsz(3)) :: a0cc_xpencil
    ! conditions
    if ( .not. dm%is_thermo) return
    if ( .not. dm%is_conv_outlet(1)) return
    if ( dm%ibcx_nominal(2, 5) /= IBC_CONVECTIVE) return
    ! get u_c/dx
    call get_convective_outlet_velocity(dm, uxdx)
    ! assign proper variables to calculate convective bc.
    a4cc_xpencil = dm%fbcx_ftp(:, :, :)%rhoh
    a0cc_xpencil = tm%rhoh(dm%dccc%xsz(1), :, :)
    ! update b.c.
    call calculate_fbcx_convective_outlet(a4cc_xpencil, uxdx, tm%fbcx_rhoh_rhs0, a0cc_xpencil, dm, isub)
    !call enforce_domain_energy_balance_dyn_fbc(fl, dm) ! todo-check necessary? 
    ! update other properties 
    do j = 1, size(dm%fbcx_ftp, 2)
      do k = 1, size(dm%fbcx_ftp, 3)
        call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(2, j, k))
        call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(4, j, k))
      end do
    end do

    return
  end subroutine

  !==========================================================================================================
  subroutine update_fbcz_convective_outlet_thermo(uz, tm, dm, isub)
    use thermo_info_mod
    use transpose_extended_mod
    implicit none
    ! arguments
    real(WP), intent(in) :: uz(:, :, :)
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP) :: uzdz
    integer :: i, j
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), 4)              :: acc4_zpencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: accc_zpencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2))                 :: acc0_zpencil
    ! condition
    if ( .not. dm%is_thermo) return
    if ( .not. dm%is_conv_outlet(3)) return
    if ( dm%ibcz_nominal(2, 5) /= IBC_CONVECTIVE) return
    ! get u_c/dz or u_c/d_theta
    call get_convective_outlet_velocity(dm, uzdz)
    ! assign proper variables to calculate convective bc.
    call transpose_to_z_pencil(tm%rhoh, accc_zpencil, dm%dccc, IPENCIL(1))
    acc0_zpencil = accc_zpencil(:, :, dm%dccc%zsz(3))
    acc4_zpencil = dm%fbcz_ftp(:, :, :)%rhoh
    ! update b.c.
    call calculate_fbcz_convective_outlet(acc4_zpencil, uzdz, tm%fbcz_rhoh_rhs0, acc0_zpencil, dm, isub)
    !!call enforce_domain_energy_balance_dyn_fbc(fl, dm) ! todo-check necessary? 
    ! update other properties
    do j = 1, size(dm%fbcz_ftp, 2)
      do i = 1, size(dm%fbcz_ftp, 1)
        call ftp_refresh_thermal_properties_from_DH(dm%fbcz_ftp(i, j, 2))
        call ftp_refresh_thermal_properties_from_DH(dm%fbcz_ftp(i, j, 4))
      end do
    end do
    
    return
  end subroutine

end module
