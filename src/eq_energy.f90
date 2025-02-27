module eq_energy_mod
  use operations
  use decomp_2d
  use wrt_debug_field_mod
  implicit none

  private :: Compute_energy_rhs
  private :: Calculate_energy_fractional_step

  public  :: Update_thermal_properties
  public  :: Solve_energy_eq
contains
!==========================================================================================================
  subroutine Update_thermal_properties(fl, tm, dm)
    use parameters_constant_mod
    use udf_type_mod
    use operations
    use thermo_info_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: dh_pcc
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: dh_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: dh_cpc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: dh_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: dh_ccp_zpencil
    
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    integer :: i, j, k
    type(t_fluidThermoProperty) :: ftp
!----------------------------------------------------------------------------------------------------------
!   main field
!----------------------------------------------------------------------------------------------------------
    do k = 1, dm%dccc%xsz(3)
      do j = 1, dm%dccc%xsz(2)
        do i = 1, dm%dccc%xsz(1)
          ftp%rhoh = tm%rhoh(i, j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          tm%hEnth(i, j, k) = ftp%h
          tm%tTemp(i, j, k) = ftp%T
          tm%kCond(i, j, k) = ftp%k
          fl%dDens(i, j, k) = ftp%d
          fl%mVisc(i, j, k) = ftp%m
        end do
      end do
    end do
    fl%dDensm2 = fl%dDensm1
    fl%dDensm1 = fl%dDens

!----------------------------------------------------------------------------------------------------------
!  BC - x
!----------------------------------------------------------------------------------------------------------
  if( dm%ibcx_Tm(1) == IBC_NEUMANN .or. &
      dm%ibcx_Tm(2) == IBC_NEUMANN) then
    call Get_x_midp_C2P_3D(tm%rhoh, dh_pcc, dm, dm%iAccuracy, dm%ibcx_ftp) ! exterpolation, check
    if(dm%ibcx_Tm(1) == IBC_NEUMANN .and. &
       dm%dpcc%xst(1) == 1) then 
      do j = 1, size(dm%fbcx_ftp, 2)
        do k = 1, size(dm%fbcx_ftp, 3)
          ftp%rhoh = dh_pcc(1, j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcx_ftp(1, j, k) = ftp
          dm%fbcx_ftp(3, j, k) = ftp
        end do
      end do 
    end if
    if(dm%ibcx_Tm(2) == IBC_NEUMANN .and. &
       dm%dpcc%xen(1) == dm%np(1)) then 
      do j = 1, size(dm%fbcx_ftp, 2)
        do k = 1, size(dm%fbcx_ftp, 3)
          ftp%rhoh = dh_pcc(dm%np(1), j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcx_ftp(2, j, k) = ftp
          dm%fbcx_ftp(4, j, k) = ftp
        end do
      end do 
    end if

  end if
!----------------------------------------------------------------------------------------------------------
!  BC - y
!----------------------------------------------------------------------------------------------------------
  if( dm%ibcy_Tm(1) == IBC_NEUMANN .or. &
      dm%ibcy_Tm(2) == IBC_NEUMANN) then
    call transpose_x_to_y(tm%rhoh, dh_ypencil, dm%dccc)
    call Get_y_midp_C2P_3D(dh_ypencil, dh_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp) ! exterpolation, check
    
    if(dm%ibcy_Tm(1) == IBC_NEUMANN .and. &
       dm%dcpc%yst(2) == 1) then 
      do i = 1, size(dm%fbcy_ftp, 1)
        do k = 1, size(dm%fbcy_ftp, 3)
          ftp%rhoh = dh_cpc_ypencil(i, 1, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcy_ftp(i, 1, k) = ftp
          dm%fbcy_ftp(i, 3, k) = ftp
        end do
      end do
    end if
    if(dm%ibcy_Tm(2) == IBC_NEUMANN .and. &
       dm%dcpc%yen(2) == dm%np(2)) then 
      do i = 1, size(dm%fbcy_ftp, 1)
        do k = 1, size(dm%fbcy_ftp, 3)
          ftp%rhoh = dh_cpc_ypencil(i, dm%np(2), k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcy_ftp(i, 2, k) = ftp
          dm%fbcy_ftp(i, 4, k) = ftp
        end do
      end do
    end if
  end if
!----------------------------------------------------------------------------------------------------------
!  BC - z
!----------------------------------------------------------------------------------------------------------
  if( dm%ibcz_Tm(1) == IBC_NEUMANN .or. &
      dm%ibcz_Tm(2) == IBC_NEUMANN) then
    call transpose_x_to_y(tm%rhoh, dh_ypencil, dm%dccc)
    call transpose_y_to_z(dh_ypencil, dh_zpencil, dm%dccc)
    call Get_z_midp_C2P_3D(dh_zpencil, dh_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp) ! exterpolation, check
    
    if(dm%ibcz_Tm(1) == IBC_NEUMANN .and. &
       dm%dccp%zst(1) == 1) then 
      do j = 1, size(dm%fbcz_ftp, 2)
        do i = 1, size(dm%fbcz_ftp, 1)
          ftp%rhoh = dh_ccp_zpencil(i, j, 1)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcz_ftp(i, j, 1) = ftp
          dm%fbcz_ftp(i, j, 3) = ftp
        end do
      end do
    end if
    if(dm%ibcz_Tm(2) == IBC_NEUMANN .and. &
       dm%dccp%zen(1) == dm%np(3)) then 
      do j = 1, size(dm%fbcz_ftp, 2)
        do i = 1, size(dm%fbcz_ftp, 1)
          ftp%rhoh = dh_ccp_zpencil(i, j, dm%np(3))
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcz_ftp(i, j, 2) = ftp
          dm%fbcz_ftp(i, j, 4) = ftp
        end do
      end do
    end if
  end if

  return
  end subroutine Update_thermal_properties
!==========================================================================================================
  subroutine Calculate_energy_fractional_step(rhs0, rhs1, dtmp, dm, isub)
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
  subroutine Compute_energy_rhs(fl, tm, dm, isub)
    use operations
    use udf_type_mod
    use thermo_info_mod
    use boundary_conditions_mod
    use wrt_debug_field_mod
    use cylindrical_rn_mod
    use wrt_debug_field_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl
    type(t_thermo), intent(inout) :: tm
    integer,        intent(in) :: isub    

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: gy_cpc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: gz_ccp_zpencil 

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dppc%xsz(3) ) :: hEnth_pcc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: hEnth_cpc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: hEnth_ccp_zpencil

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
!==========================================================================================================
!   preparation
!==========================================================================================================
    call transpose_x_to_y(fl%gy,        gy_cpc_ypencil,   dm%dcpc)   ! for d(g_y h)/dy
    call transpose_x_to_y(fl%gz,        accp_ypencil,     dm%dccp)   ! intermediate, accp_ypencil = gz_ypencil
    call transpose_y_to_z(accp_ypencil, gz_ccp_zpencil,   dm%dccp)   ! for d(g_z h)/dz
!----------------------------------------------------------------------------------------------------------
!    h --> h_pcc
!      --> h_ypencil --> h_cpc_ypencil
!                    --> h_zpencil --> h_ccp_zpencil
!----------------------------------------------------------------------------------------------------------
    fbcx_4cc = MAXP
    fbcy_c4c = MAXP
    fbcz_cc4 = MAXP
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%h
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%h
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%h
    call Get_x_midp_C2P_3D(tm%hEnth, hEnth_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp(:), fbcx_4cc ) ! for d(g_x h_pcc))/dy
    call transpose_x_to_y (tm%hEnth, accc_ypencil, dm%dccc)                     !accc_ypencil = hEnth_ypencil
    call Get_y_midp_C2P_3D(accc_ypencil, hEnth_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp(:), fbcy_c4c)! for d(g_y h_cpc)/dy
    call transpose_y_to_z (accc_ypencil, accc_zpencil, dm%dccc) !ccc_zpencil = hEnth_zpencil
    call Get_z_midp_C2P_3D(accc_zpencil, hEnth_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp(:), fbcz_cc4) ! for d(g_z h_ccp)/dz
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
    tm%ene_rhs      = ZERO
    ene_rhs_ccc_ypencil = ZERO
    ene_rhs_ccc_zpencil = ZERO
!----------------------------------------------------------------------------------------------------------
! conv-x-e, x-pencil : d (gx * h_pcc) / dx 
!----------------------------------------------------------------------------------------------------------
    ! !------b.c.------
    ! if(is_fbcx_velo_required) then
    !   fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%h
    !   fbcx_4cc = - fbcx_4cc * dm%fbcx_gx
    ! else
    !   fbcx_4cc = MAXP
    ! end if
    !------bulk------
    apcc_xpencil = - fl%gx * hEnth_pcc_xpencil
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
    acpc_ypencil = - gy_cpc_ypencil * hEnth_cpc_ypencil
    !------b.c.------
    if(is_fbcy_velo_required) then
      call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm)
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
    !------PDE------
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
      call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm)
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
    call wrt_3d_pt_debug(tm%tTemp,   dm%dccc, fl%iteration, isub, 'T@bf stepping') ! debug_ww
    call wrt_3d_pt_debug(tm%ene_rhs, dm%dccc, fl%iteration, isub, 'energy_rhs@bf stepping') ! debug_ww
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
    implicit none
    type(t_domain), intent(inout)    :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    integer,        intent(in)    :: isub
    real(WP) :: uxdx
    integer :: j, k

    if(isub==3) then
      fl%dDensm2(:, :, :) = fl%dDensm1(:, :, :)
      fl%dDensm1(:, :, :) = fl%dDens(:, :, :)
    end if
!----------------------------------------------------------------------------------------------------------
! to set up halo b.c. for cylindrical pipe
!----------------------------------------------------------------------------------------------------------
    call update_fbcy_cc_thermo_halo(fl, tm, dm)
!----------------------------------------------------------------------------------------------------------
! to set up convective outlet b.c. assume x direction
!----------------------------------------------------------------------------------------------------------
    call update_fbcx_convective_outlet_thermo(fl, tm, dm, isub)
!----------------------------------------------------------------------------------------------------------
!   calculate rhs of energy equation
!----------------------------------------------------------------------------------------------------------
    call Compute_energy_rhs(fl, tm, dm, isub)
!----------------------------------------------------------------------------------------------------------
!   update rho * h
!----------------------------------------------------------------------------------------------------------
#ifdef DEBUG_STEPS
    write(*,*) 'rhoh-e-bf', tm%rhoh(1, 1:4, 1)
#endif
    tm%rhoh = tm%rhoh + tm%ene_rhs
#ifdef DEBUG_STEPS
    write(*,*) 'rhoh-e-af', tm%rhoh(1, 1:4, 1)
    call wrt_3d_pt_debug(tm%rhoh, dm%dccc, fl%iteration, isub, 'rhoh@af stepping') ! debug_ww
#endif
!----------------------------------------------------------------------------------------------------------
!   update other properties from rho * h
!----------------------------------------------------------------------------------------------------------
    call Update_thermal_properties(fl, tm, dm)
!----------------------------------------------------------------------------------------------------------
!   No Need to apply b.c.
!----------------------------------------------------------------------------------------------------------
#ifdef DEBUG_STEPS
    write(*,*) 'T-e', tm%tTemp(1, 1:4, 1)
    call wrt_3d_pt_debug(tm%tTemp,   dm%dccc, fl%iteration, isub, 'T@af stepping') ! debug_ww
#endif

  return
  end subroutine

end module eq_energy_mod
