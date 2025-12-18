module eq_scalar_transport
  use operations
  use decomp_2d
  use wrt_debug_field_mod
  implicit none

  private :: Compute_scalar_rhs
  private :: Calculate_energy_fractional_step
  public  :: Update_thermal_properties
  public  :: Solve_energy_eq
contains
!==========================================================================================================
  subroutine Calculate_scalar_fractional_step(rhs0, rhs1, dtmp, dm, isub)
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: rhs0, rhs1
    integer,  intent(in) :: isub
    
    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    integer :: i, j, k 

    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        do i = 1, dtmp%xsz(1)

      ! add explicit terms : convection+viscous rhs
          rhs_explicit_current = rhs1(i, j, k) ! not (*dt)
          rhs_explicit_last    = rhs0(i, j, k) ! not (*dt)
          rhs_total = dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last
          rhs0(i, j, k) = rhs_explicit_current
      ! times the time step 
          rhs1(i, j, k) = dm%dt * rhs_total ! * dt
        end do
      end do
    end do

    return
  end subroutine
!==========================================================================================================
  subroutine Compute_scalar_rhs(ux, uy, uz, tm, dm, isub)
    use operations
    use udf_type_mod
    use thermo_info_mod
    use boundary_conditions_mod
    use wrt_debug_field_mod
    use cylindrical_rn_mod
    use wrt_debug_field_mod
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    integer,        intent(in) :: isub    
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ), intent(in) :: ux
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ), intent(in) :: uy
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ), intent(in) :: uz
    ! local variables
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: gz_ccp_zpencil 

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dppc%xsz(3) ) :: Ttemp_pcc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: Ttemp_cpc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: Ttemp_ccp_zpencil

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: Ttemp_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: Ttemp_ccc_zpencil

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dppc%xsz(3) ) :: kCond_pcc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: kCond_cpc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: kCond_ccp_zpencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: kCond_ccc_zpencil
    
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: ene_rhs_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: ene_rhs_ccc_zpencil
    
    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc 
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4
    
    integer  :: n, i, j, k
    integer  :: mbc(1:2, 1:3)
!----------------------------------------------------------------------------------------------------------
!    T --> T_pcc
!      --> T_ypencil --> T_cpc_ypencil
!                    --> T_zpencil --> T_ccp_zpencil
!----------------------------------------------------------------------------------------------------------
    fbcx_4cc = MAXP
    fbcy_c4c = MAXP
    fbcz_cc4 = MAXP
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%t
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%t
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%t
    call Get_x_midp_C2P_3D(tm%tTemp, tTemp_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp(:), fbcx_4cc ) ! for d(g_x h_pcc))/dy
    call transpose_x_to_y (tm%tTemp, Ttemp_ccc_ypencil, dm%dccc)                     !accc_ypencil = hEnth_ypencil
    call Get_y_midp_C2P_3D(Ttemp_ccc_ypencil, tTemp_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp(:), fbcy_c4c)! for d(g_y h_cpc)/dy
    call axis_estimating_radial_xpx(tTemp_cpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(1))
    call transpose_y_to_z (Ttemp_ccc_ypencil, Ttemp_ccc_zpencil, dm%dccc) !ccc_zpencil = hEnth_zpencil
    call Get_z_midp_C2P_3D(Ttemp_ccc_zpencil, tTemp_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp(:), fbcz_cc4) ! for d(g_z h_ccp)/dz
!----------------------------------------------------------------------------------------------------------
!    k --> k_pcc
!      --> k_ypencil --> k_cpc_ypencil
!                    --> k_zpencil --> k_ccp_zpencil              
!----------------------------------------------------------------------------------------------------------
    fbcx_4cc = MAXP
    fbcy_c4c = MAXP
    fbcz_cc4 = MAXP
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%k
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%k
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%k
    call Get_x_midp_C2P_3D(tm%kCond, kCond_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp(:), fbcx_4cc) ! for d(k_pcc * (dT/dx) )/dx
    call transpose_x_to_y (tm%kCond, accc_ypencil, dm%dccc)  ! for k d2(T)/dy^2
    call Get_y_midp_C2P_3D(accc_ypencil,  kCond_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp(:), fbcy_c4c)
    call axis_estimating_radial_xpx(kCond_cpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(1))
    call transpose_y_to_z (accc_ypencil,  kCond_ccc_zpencil, dm%dccc) 
    call Get_z_midp_C2P_3D(kCond_ccc_zpencil, kCond_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp(:), fbcz_cc4)
!----------------------------------------------------------------------------------------------------------
!    T --> T_ypencil --> T_zpencil
!----------------------------------------------------------------------------------------------------------
    call transpose_x_to_y (tm%Ttemp,      Ttemp_ccc_ypencil, dm%dccc)   ! for k d2(T)/dy^2
    call transpose_y_to_z (Ttemp_ccc_ypencil, Ttemp_ccc_zpencil, dm%dccc)   ! for k d2(T)/dz^2
!==========================================================================================================
! the RHS of energy equation : convection terms
!==========================================================================================================
    sc%ene_rhs          = ZERO
    ene_rhs_ccc_ypencil = ZERO
    ene_rhs_ccc_zpencil = ZERO
!----------------------------------------------------------------------------------------------------------
! conv-x-e, x-pencil : d (ux * T_pcc) / dx 
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    apcc_xpencil = - ux * T_pcc_xpencil
    !------b.c.------
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcx(fbcx_4cc, apcc_xpencil, dm%dpcc)
    else
      fbcx_4cc = MAXP
    end if
    !------PDE------
    call Get_x_1der_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, ebcx_conv, fbcx_4cc) 
    tm%ene_rhs = tm%ene_rhs + accc_xpencil

#ifdef DEBUG_STEPS
    write(*,*) 'conx-e', accc_xpencil(4, 1:4, 4)
#endif
!----------------------------------------------------------------------------------------------------------
! conv-y-e, y-pencil : d (gy * h_cpc) / dy  * (1/r)
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    call transpose_x_to_y(gy, acpc_ypencil,   dm%dcpc)   ! for d(g_y h)/dy
    acpc_ypencil = - acpc_ypencil * hEnth_cpc_ypencil
    !------b.c.------
    if(is_fbcy_velo_required) then
      call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
    else
      fbcy_c4c = MAXP
    end if
    !------PDE------
    call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, ebcy_conv, fbcy_c4c)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
    ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil

#ifdef DEBUG_STEPS
    write(*,*) 'cony-e', accc_ypencil(4, 1:4, 4)
#endif
!----------------------------------------------------------------------------------------------------------
! conv-z-e, z-pencil : d (gz * h_ccp) / dz   * (1/r)
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    call transpose_x_to_y(gz,           accp_ypencil,     dm%dccp)   ! intermediate, accp_ypencil = gz_ypencil
    call transpose_y_to_z(accp_ypencil, gz_ccp_zpencil,   dm%dccp)   ! for d(g_z h)/dz
    accp_zpencil = - gz_ccp_zpencil * hEnth_ccp_zpencil
    ! if(dm%icoordinate == ICYLINDRICAL) &
    ! call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    if(is_fbcz_velo_required) then
      call extract_dirichlet_fbcz(fbcz_cc4, accp_zpencil, dm%dccp)
    else
      fbcz_cc4 = MAXP
    end if
    !------PDE------
    call Get_z_1der_P2C_3D( accp_zpencil, accc_zpencil, dm, dm%iAccuracy, ebcz_conv, fbcz_cc4)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_zpencil, dm%dccc, dm%rci, 1, IPENCIL(3))
    ene_rhs_ccc_zpencil = ene_rhs_ccc_zpencil + accc_zpencil

#ifdef DEBUG_STEPS
    write(*,*) 'conz-e', accc_zpencil(4, 1:4, 4)
#endif
!==========================================================================================================
! the RHS of energy equation : diffusion terms
!==========================================================================================================
!----------------------------------------------------------------------------------------------------------
! diff-x-e, d ( k_pcc * d (T) / dx ) dx
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    call get_fbcx_iTh(dm%ibcx_Tm, dm, fbcx_4cc)
    call Get_x_1der_C2P_3D(tm%tTemp, apcc_xpencil, dm, dm%iAccuracy, dm%ibcx_Tm, fbcx_4cc )
    apcc_xpencil = apcc_xpencil * kCond_pcc_xpencil
    !------B.C.------
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcx(fbcx_4cc, apcc_xpencil, dm%dpcc)
    else
      fbcx_4cc = MAXP
    end if  
    !------PDE------f
    call Get_x_1der_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, ebcx_difu, fbcx_4cc)
    tm%ene_rhs = tm%ene_rhs + accc_xpencil * tm%rPrRen
#ifdef DEBUG_STEPS
    write(*,*) 'difx-e', accc_xpencil(4, 1:4, 4)
#endif
!----------------------------------------------------------------------------------------------------------
! diff-y-e, d ( r * k_cpc * d (T) / dy ) dy * 1/r
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    call get_fbcy_iTh(dm%ibcy_Tm, dm, fbcy_c4c)
    call Get_y_1der_C2P_3D(tTemp_ccc_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_Tm, fbcy_c4c)
    acpc_ypencil = acpc_ypencil * kCond_cpc_ypencil
#ifdef DEBUG_STEPS
    write(*,*) 'diy-dT', acpc_ypencil(4, 1:4, 4)
    write(*,*) 'dify-k', kCond_cpc_ypencil(4, 1:4, 4)
#endif
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rp, 1, IPENCIL(2))
    !------B.C.------
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
    else
      fbcy_c4c = MAXP
    end if  
    !------PDE------
    call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, ebcy_difu, fbcy_c4c) ! check, dirichlet, r treatment
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
    ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil * tm%rPrRen
    
#ifdef DEBUG_STEPS
    write(*,*) 'dify-e', accc_ypencil(4, 1:4, 4)
#endif
!----------------------------------------------------------------------------------------------------------
! diff-z-e, d (1/r* k_ccp * d (T) / dz ) / dz * 1/r
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    call get_fbcz_iTh(dm%ibcz_Tm, dm, fbcz_cc4)
    call Get_z_1der_C2P_3D(tTemp_ccc_zpencil, accp_zpencil, dm, dm%iAccuracy, dm%ibcz_Tm, fbcz_cc4 )
    accp_zpencil = accp_zpencil * kCond_ccp_zpencil
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    if(is_fbcz_velo_required) then
      call extract_dirichlet_fbcz(fbcz_cc4, accp_zpencil, dm%dccp)
    else
      fbcz_cc4 = MAXP
    end if  
    !------PDE------
    call Get_z_1der_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, ebcz_difu, fbcz_cc4)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_zpencil, dm%dccc, dm%rci, 1, IPENCIL(3))
    ene_rhs_ccc_zpencil = ene_rhs_ccc_zpencil + accc_zpencil * tm%rPrRen
    
#ifdef DEBUG_STEPS
    write(*,*) 'difz-e', accc_zpencil(4, 1:4, 4)
#endif
!==========================================================================================================
! all convert into x-pencil
!==========================================================================================================
    call transpose_z_to_y(ene_rhs_ccc_zpencil, accc_ypencil, dm%dccc)
    ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil
    call transpose_y_to_x(ene_rhs_ccc_ypencil, accc_xpencil, dm%dccc)
    tm%ene_rhs = tm%ene_rhs + accc_xpencil
!==========================================================================================================
! time approaching
!==========================================================================================================
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(tm%tTemp,   dm%dccc, tm%iteration, isub, 'T@bf stepping') ! debug_ww
    call wrt_3d_pt_debug(tm%ene_rhs, dm%dccc, tm%iteration, isub, 'energy_rhs@bf stepping') ! debug_ww
    write(*,*) 'rhs-e', tm%ene_rhs(1, 1:4, 1)
#endif
    call Calculate_energy_fractional_step(tm%ene_rhs0, tm%ene_rhs, dm%dccc, dm, isub)
    return
  end subroutine Compute_energy_rhs

!==========================================================================================================
!==========================================================================================================
  subroutine Solve_energy_eq(fl, tm, dm, isub)
    use udf_type_mod
    use thermo_info_mod 
    use solver_tools_mod
    use boundary_conditions_mod
    use bc_convective_outlet_mod
    use convert_primary_conservative_mod
    implicit none
    ! arguments
    type(t_domain), intent(inout)    :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP) :: uxdx
    integer :: j, k
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: gx, ux
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: gy, uy
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: gz, uz
    !
    ! set up flow info based on different time stepping
    gx = fl%gx
    gy = fl%gy
    gz = fl%gz
    if (dm%is_conv_outlet(1)) ux = fl%qx
    if (dm%is_conv_outlet(3)) uz = fl%qz
    ! backup density and viscosity 
    if (isub == 1) then
      fl%dDens0 = fl%dDens
    end if
    ! compute b.c. info from convective b.c. if specified.
    if (dm%is_conv_outlet(1)) call update_fbcx_convective_outlet_thermo(ux, tm, dm, isub)
    if (dm%is_conv_outlet(3)) call update_fbcz_convective_outlet_thermo(uz, tm, dm, isub)
    ! calculate rhs of energy equation
    call Compute_energy_rhs(gx, gy, gz, tm, dm, isub)
    !  update rho * h
    tm%rhoh = tm%rhoh + tm%ene_rhs
    !  update other properties from rho * h for domain + b.c.
    call Update_thermal_properties(fl%dDens, fl%mVisc, tm, dm)
    if (dm%icase == ICASE_PIPE) call update_fbcy_cc_thermo_halo(tm, dm)

  return
  end subroutine

end module eq_energy_mod
