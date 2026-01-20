module eq_momentum_mod
  use operations
  use precision_mod
  use decomp_2d
  use print_msg_mod
  use wrt_debug_field_mod
  implicit none

  private :: Calculate_momentum_fractional_step
  private :: Compute_momentum_rhs
  private :: Correct_massflux
  private :: solve_pressure_poisson
  private :: gravity_decomposition_to_rz
  !private :: solve_poisson_x2z
  
  public  :: Solve_momentum_eq
  

contains
!==========================================================================================================
  subroutine gravity_decomposition_to_rz(dens, iforce, fgravity, fr_cpc_ypencil, ft_ccp_zpencil, dm)
    use math_mod
    use udf_type_mod
    use cylindrical_rn_mod
    implicit none
    type(t_domain), intent(in) :: dm
    INTEGER, intent(in) :: iforce
    real(WP), intent(in) :: fgravity
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent(in) :: dens
    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)), intent(out) :: fr_cpc_ypencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)), intent(out) :: ft_ccp_zpencil

    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: accc_ypencil
    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: acpc_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: accp_ypencil
    real(WP), dimension(dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3)) :: acpp_ypencil
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3)) :: acpc_zpencil
    real(WP), dimension(dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3)) :: acpp_zpencil,   &
                                                                           fr_cpp_zpencil, &
                                                                           ft_cpp_zpencil
    real(WP), dimension(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3))              :: fbcy_c4c
    integer :: k, j
    real(WP) :: theta

    if(dm%icoordinate /= ICYLINDRICAL) return
!----------------------------------------------------------------------------------------------------------
!   density interpolation from ccc to cpp
!----------------------------------------------------------------------------------------------------------
    call transpose_x_to_y(dens, accc_ypencil, dm%dccc)
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%d
    call Get_y_midp_C2P_3D(accc_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_c4c)
    !call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(1))
#ifdef DEBUG_STEPS  
    if(dm%icase == ICASE_PIPE) then
      write(*,*) 'density', acpc_ypencil(4, 1, 4),  acpc_ypencil(4, 1, dm%knc_sym(4)) , &
                  acpc_ypencil(4, 1, 4)-acpc_ypencil(4, 1, dm%knc_sym(4))
    end if
#endif
    call transpose_y_to_z(acpc_ypencil, acpc_zpencil, dm%dcpc)
    call Get_z_midp_C2P_3D(acpc_zpencil, acpp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp) ! assume z is periodic
!----------------------------------------------------------------------------------------------------------
!   force: calculate in z-pencil
!----------------------------------------------------------------------------------------------------------
    if(iforce == 2 .or. iforce == -2) then
      acpp_zpencil = acpp_zpencil * fgravity

      do k = 1, dm%dcpp%zsz(3)
        theta = dm%h(3) * REAL(k, WP)
        do j = 1, dm%dccc%zsz(2)
          fr_cpp_zpencil(:, j, k) =  acpp_zpencil(:, j, k) * sin_wp(theta)
          ft_cpp_zpencil(:, j, k) = -acpp_zpencil(:, j, k) * cos_wp(theta)
        end do
      end do

    else if(iforce == 3 .or. iforce == -3) then
      acpp_zpencil = acpp_zpencil * fgravity

      do k = 1, dm%dcpp%zsz(3)
        theta = dm%h(3) * REAL(k, WP)
        do j = 1, dm%dcpp%zsz(2)
          fr_cpp_zpencil(:, j, k) =  acpp_zpencil(:, j, k) * cos_wp(theta)
          ft_cpp_zpencil(:, j, k) =  acpp_zpencil(:, j, k) * sin_wp(theta)
        end do
      end do
    else
      call Print_error_msg('gravity direction is not correct for cylindrical coordinates')
    end if
!----------------------------------------------------------------------------------------------------------
!   back to momentum eq.
!----------------------------------------------------------------------------------------------------------
    call Get_z_midp_P2C_3D(fr_cpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp)
    call transpose_z_to_y(acpc_zpencil, fr_cpc_ypencil, dm%dcpc)
    call multiple_cylindrical_rn(fr_cpc_ypencil, dm%dcpc, dm%rp, 1, IPENCIL(2)) ! fr * r


    call transpose_z_to_y(ft_cpp_zpencil, acpp_ypencil, dm%dcpp)
    call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp)
    call transpose_y_to_z(accp_ypencil, ft_ccp_zpencil, dm%dccp)

    return

  end subroutine
!==========================================================================================================
!==========================================================================================================
!> \brief To calcuate the convection and diffusion terms in rhs of momentum eq.
!>
!> This subroutine is called everytime when calcuting the rhs of momentum eqs.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                         !
!______________________________________________________________________________!
!> \param[inout]  rhs0          the last iteration rhs
!> \param[inout]  rhs1          the current iteration rhs
!> \param[in]     rhs1_semi     the semi-implicit term
!> \param[in]     isub          the RK iteration to get correct Coefficient 
!_______________________________________________________________________________
  subroutine Calculate_momentum_fractional_step(rhs0, rhs1, rhs1_pfc, dtmp, dm, isub)
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(inout)    :: rhs1_pfc
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: rhs0, rhs1
    integer,  intent(in) :: isub
    
    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    integer :: i, j, k 

    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        do i = 1, dtmp%xsz(1)

      ! add explicit terms : convection+viscous rhs
          rhs_explicit_current = rhs1(i, j, k) ! not (* dt)
          rhs_explicit_last    = rhs0(i, j, k) ! not (* dt)
          rhs_total = dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last
          rhs0(i, j, k) = rhs_explicit_current ! not (* dt)

      ! add pressure gradient
          rhs_total = rhs_total + &
                      dm%tAlpha(isub) * rhs1_pfc(i, j, k)

      ! times the time step 
          rhs1(i, j, k) = dm%dt * rhs_total ! * dt 

        end do
      end do
    end do

    return
  end subroutine
!==========================================================================================================
!> \brief To calcuate all rhs of momentum eq.
!==========================================================================================================
  subroutine Compute_momentum_rhs(fl, dm, isub, opt_dens, opt_visc)
    use parameters_constant_mod
    use udf_type_mod
    use operations
    use solver_tools_mod
    use typeconvert_mod
    use boundary_conditions_mod
    use wrt_debug_field_mod
    use find_max_min_ave_mod
    use cylindrical_rn_mod
    implicit none
    ! arguments
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,     intent(in ) :: isub
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent(in), optional :: opt_dens, opt_visc
!----------------------------------------------------------------------------------------------------------
! result variables
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3) ) :: mx_rhs_ypencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: mx_rhs_zpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: mx_rhs_pfc_xpencil

    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: my_rhs_ypencil
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: my_rhs_zpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: my_rhs_pfc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: my_rhs_pfc_ypencil

    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: mz_rhs_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: mz_rhs_zpencil
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: mz_rhs_pfc_xpencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: mz_rhs_pfc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: mz_rhs_pfc_zpencil
!----------------------------------------------------------------------------------------------------------
! intermediate variables
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: pres_ypencil     ! p-m2, common
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: pres_zpencil     ! p-m3, common

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: div_ccc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: div_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: div_ccc_zpencil  
!----------------------------------------------------------------------------------------------------------
! qx, gx
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: qxix_ccc_xpencil ! conv-x-m1, common
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: qxiy_ppc_ypencil ! conv-y-m1, common
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: qxiy_ppc_xpencil ! conv-x-m2, no-thermo
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: qxiz_pcp_zpencil ! conv-z-m1, common
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: qxiz_pcp_xpencil ! conv-x-m3, no-thermo

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: gxix_ccc_xpencil ! conv-x-m1, thermo
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: gxiy_ppc_xpencil ! conv-x-m2, <=> qxiy_ppc_xpencil
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: gxiz_pcp_xpencil ! conv-x-m3, <=> qxiz_pcp_xpencil
    
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: qxdx_pcc_xpencil ! for div b.c., common
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: qxdx_cpc_ypencil ! for div b.c., common
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: qxdx_ccp_zpencil ! for div b.c., dommon
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: qxdx_ccc_xpencil ! diff-x-m1, common
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: qxdy_ppc_xpencil ! diff-x-m2, common
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: qxdy_ppc_ypencil ! diff-y-m1, common
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: qxdz_pcp_xpencil ! diff-x-m3, common
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: qxdz_pcp_zpencil ! diff-z-m1, common
!----------------------------------------------------------------------------------------------------------
! qy, gy
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: qyix_ppc_xpencil  ! conv-x-m2, common
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: qyix_ppc_ypencil  ! conv-y-m1, no-thermo
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: qyiy_ccc_ypencil  ! conv-y-m2, common
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qyiz_cpp_ypencil  ! conv-y-m3, no-thermo
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qyiz_cpp_zpencil  ! conv-z-m2, no-cly1

    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: gyix_ppc_ypencil  ! conv-y-m1, <=> qyix_ppc_ypencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: gyiy_ccc_ypencil  ! conv-y-m2, <=> qyiy_ccc_ypencil
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: gyiz_cpp_ypencil  ! conv-y-m3, <=> qyiz_cpp_ypencil
    !real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: gyriz_cpp_ypencil ! conv-r-m3, <=> qyriz_cpp_ypencil
    
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: qyr_ypencil, qyr2_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: qyriy_ccc_zpencil ! diff-z-m3, cly
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: qyriy_ccc_ypencil ! conv-y-m2, cly
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qyriz_cpp_ypencil ! conv-r-m3, no-thermal
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qyriz_cpp_zpencil ! conv-z-m2, cly1
    
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: qydx_ppc_xpencil  ! diff-x-m2, common
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: qydx_ppc_ypencil  ! diff-y-m1, common
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qydz_cpp_ypencil  ! diff-r-m3
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qydz_cpp_zpencil  ! diff-y-m3, no-cly2
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: qydy_ccc_ypencil  ! diff-y-m2, no-cly1, div
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: qydy_pcc_xpencil  ! for div b.c., common
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: qydy_cpc_ypencil  ! for div b.c., common
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: qydy_ccp_zpencil  ! for div b.c., common

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: qyrdy_ccc_ypencil ! diff-y-m2, cly1
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qyr2dz_cpp_zpencil ! diff-z-m2, cly2
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qyrdz_cpp_ypencil, qyr2dz_cpp_ypencil ! diff-y-m3, cly3
!----------------------------------------------------------------------------------------------------------
! qz, gz
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: qz_zpencil
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: qzix_pcp_xpencil  ! conv-x-m3, common
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: qzix_pcp_zpencil  ! conv-z-m1, no-thermo, common
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qziy_cpp_ypencil  ! conv-y-m3, common
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qziy_cpp_zpencil  ! conv-z-m2, no-thermo, no-cly
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: qziz_ccc_zpencil  ! conv-z-m3, common
    
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: gzix_pcp_zpencil  ! conv-z-m1, <=> qzix_pcp_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: gziy_cpp_zpencil  ! conv-z-m2
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: gziy_cpp_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: gziz_ccc_zpencil  ! conv-z-m3, <=> qziz_ccc_zpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: qziz_ccc_ypencil ! conv-r-m2, cly

    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qzriy_cpp_ypencil ! conv-y-m3, conv-r-m3, diff-y-m3, diff-r-m3, common
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qzriy_cpp_zpencil ! diff-z-m2, cly
    
    
    ! real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: gzriy_cpp_zpencil ! conv-z-m2, 
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: gziz_ccc_ypencil ! conv-r-m2, thermo+cly, 
    
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: qzdx_pcp_xpencil  ! diff-x-m3, common
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: qzdx_pcp_zpencil  ! diff-z-m1, common
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qzdy_cpp_ypencil  ! diff-y-m3, no-cly
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qzdy_cpp_zpencil  ! diff-z-m2, no-cly
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: qzdz_ccc_ypencil  ! diff-r-m2, cly
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: qzdz_ccc_zpencil  ! diff-z-m3, common
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: qzdz_pcc_xpencil  ! for div b.c.
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: qzdz_cpc_ypencil  ! for div b.c.
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: qzdz_ccp_zpencil  ! for div b.c.

    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: qzrdz_cpc_ypencil
    ! real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: qzrdy_cpp_ypencil ! diff-y-m3, diff-r-m3
    ! real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: qzrdy_cpp_zpencil ! diff-z-m2, cly
!----------------------------------------------------------------------------------------------------------
!   intermediate variables : if dm%is_thermo = true
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: d_ccc_xpencil
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: mu_ccc_xpencil    ! x-v1, thermal only
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: mu_ccc_ypencil    ! y-v2, thermal only
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: mu_ccc_zpencil    ! z-v3, thermal only
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: muiz_ccp_zpencil
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: muixy_ppc_xpencil ! diff-x-m2, thermal only
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: muixy_ppc_ypencil ! diff-y-m1, thermal only
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: muixz_pcp_xpencil ! diff-x-m3, thermal only
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: muixz_pcp_zpencil ! diff-z-m1, thermal only
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: muiyz_cpp_ypencil ! z-v2, z-v4, thermal only
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: muiyz_cpp_zpencil ! diff-z-m2, thermal only
!----------------------------------------------------------------------------------------------------------
!   intermediate variables : if fl%igravity == i
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: dDens_ypencil  ! d 
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: dDens_zpencil  ! d  
!----------------------------------------------------------------------------------------------------------
! temporary variables
!----------------------------------------------------------------------------------------------------------
#ifdef DEBUG_STEPS  
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_test
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_test
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_test
#endif 
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil, &
                                                                             apcc_xpencil1, &
                                                                             apcc_xpencil2, &
                                                                             apcc_xpencil3
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: appc_xpencil, appc_xpencil1
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: apcp_xpencil
    
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: appc_ypencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil, accc_ypencil1
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: acpp_ypencil, acpp_ypencil1
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil, acpc_ypencil1
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3) ) :: apcc_ypencil
    real(WP), dimension( dm%dpcp%ysz(1), dm%dpcp%ysz(2), dm%dpcp%ysz(3) ) :: apcp_ypencil
    
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: apcp_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: acpp_zpencil, acpp_zpencil1
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil, accc_zpencil1
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil, &
                                                                             accp_zpencil1, &
                                                                             accp_zpencil2, &
                                                                             accp_zpencil3
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil, acpc_zpencil1
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil

    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil

!----------------------------------------------------------------------------------------------------------
! bc variables
!----------------------------------------------------------------------------------------------------------
    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc 
    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_div_4cc  ! common
    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_mu_4cc   ! thermo
    !real(WP), dimension( 4, dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: fbcx_mu_4pc

    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c1
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_div_c4c, fbcy_div_c4c1
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_mu_c4c

    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc41, fbcz_cc42
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_div_cc4
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_mu_cc4

    real(WP), dimension( 4, dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: fbcx_4pc 
    real(WP), dimension( 4, dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: fbcx_4cp
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), 4 ) :: fbcz_pc4 
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), 4 ) :: fbcz_cp4
    real(WP), dimension( dm%dppc%ysz(1), 4, dm%dppc%ysz(3) ) :: fbcy_p4c
    real(WP), dimension( dm%dcpp%ysz(1), 4, dm%dcpp%ysz(3) ) :: fbcy_c4p, fbcy_c4p1
    
!----------------------------------------------------------------------------------------------------------
! others
!----------------------------------------------------------------------------------------------------------
    integer  :: i, k
    real(WP) :: rhsx_bulk, rhsz_bulk
    integer :: ibcy(2), ibcz(2)

!==========================================================================================================
! preparation of constants to be used.
!==========================================================================================================
#ifdef DEBUG_STEPS  
    if(nrank == 0) &
    call Print_debug_start_msg("Compute_momentum_rhs at isub = "//trim(int2str(isub)))
#endif
!==========================================================================================================
! preparation of intermediate variables to be used - common
! Note: 
!      Due to bc treatment, please do interp first, then derivative to minumize numerical 
!      errors for Dirichlet B.C.
!==========================================================================================================
!----------------------------------------------------------------------------------------------------------
!    p --> p_ypencil --> p_zpencil 
!----------------------------------------------------------------------------------------------------------
    call transpose_x_to_y (fl%pres,      pres_ypencil, dm%dccc)
    call transpose_y_to_z (pres_ypencil, pres_zpencil, dm%dccc)
!----------------------------------------------------------------------------------------------------------
!   d --> d_ypencil --> d_zpencil
!----------------------------------------------------------------------------------------------------------    
    if(dm%is_thermo .and. fl%igravity /= 0) then
      d_ccc_xpencil = opt_dens
      call transpose_x_to_y (d_ccc_xpencil,      dDens_ypencil, dm%dccc)
      call transpose_y_to_z (dDens_ypencil, dDens_zpencil, dm%dccc)
    end if
!----------------------------------------------------------------------------------------------------------
!    qx 
!    | -[ipx]-> qxix_ccc_xpencil(common)
!    | -[1dx]-> qxdx_ccc_xpencil(common)
!    | -[x2y]-> qx_ypencil(temp) 
!               | -[ipy]-> qxiy_ppc_ypencil(common) -[y2x]-> qxiy_ppc_xpencil(no thermal)-[1dx]-> qxdx_cpc_xpencil(temp) -[x2y]-> qxdx_cpc_ypencil (for div bc)
!               | -[1dy]-> qxdy_ppc_ypencil(common) -[y2x]-> qxdy_ppc_xpencil(common)
!               | -[y2z]-> qx_zpencil(temp) 
!                          | -[ipz]-> qxiz_pcp_zpencil(common) -[z2x]-> qxiz_pcp_xpencil(no-thermal) 
!                                                            | -[1dx]-> qxdx_ccp_xpencil(temp) -[x2z]-> qxdx_ccp_zpencil (for div bc)
!                          | -[1dz]-> qxdz_pcp_zpencil(common) -[z2y]-> qxdz_pcp_ypencil(temp) -[y2x]-> qxdz_pcp_xpencil(common)
!----------------------------------------------------------------------------------------------------------
    call Get_x_midp_P2C_3D(fl%qx, qxix_ccc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_qx)
    call Get_x_1der_P2C_3D(fl%qx, qxdx_ccc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_qx)
    !
    call transpose_x_to_y (fl%qx, apcc_ypencil, dm%dpcc) !apcc_ypencil = qx_ypencil
    call Get_y_1der_C2P_3D(apcc_ypencil, qxdy_ppc_ypencil, dm, dm%iAccuracy, dm%ibcy_qx, dm%fbcy_qx)
    !
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qxdy_ppc_ypencil', qxdy_ppc_ypencil(4, 1, 4),  qxdy_ppc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qxdy_ppc_ypencil(4, 1, 4)+qxdy_ppc_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
    !
    call transpose_y_to_x(qxdy_ppc_ypencil, qxdy_ppc_xpencil, dm%dppc)
    !
    call Get_y_midp_C2P_3D(apcc_ypencil, qxiy_ppc_ypencil, dm, dm%iAccuracy, dm%ibcy_qx, dm%fbcy_qx) ! for x-mom
    !call axis_estimating_radial_xpx(qxiy_ppc_ypencil, dm%dppc, IPENCIL(2), dm, IDIM(1))
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qxiy_ppc_ypencil', qxiy_ppc_ypencil(4, 1, 4),  qxiy_ppc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qxiy_ppc_ypencil(4, 1, 4)-qxiy_ppc_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
    !
    if(.not. dm%is_thermo) then
      call transpose_y_to_x (qxiy_ppc_ypencil, qxiy_ppc_xpencil, dm%dppc)
    end if
    !
    call transpose_y_to_z (apcc_ypencil, apcc_zpencil, dm%dpcc)!apcc_zpencil = qx_zpencil
    call Get_z_midp_C2P_3D(apcc_zpencil, qxiz_pcp_zpencil, dm, dm%iAccuracy, dm%ibcz_qx, dm%fbcz_qx)
    !
    if(.not. dm%is_thermo) then
      call transpose_z_to_y (qxiz_pcp_zpencil, apcp_ypencil, dm%dpcc)! apcp_ypencil = qxiz_pcp_ypencil
      call transpose_y_to_x (apcp_ypencil, qxiz_pcp_xpencil, dm%dpcp)
    end if
    !
    call Get_z_1der_C2P_3D(apcc_zpencil, qxdz_pcp_zpencil, dm, dm%iAccuracy, dm%ibcz_qx, dm%fbcz_qx)
    !
    call transpose_z_to_y(qxdz_pcp_zpencil, apcp_ypencil,     dm%dpcp) !qxdz_pcp_ypencil
    call transpose_y_to_x(apcp_ypencil,     qxdz_pcp_xpencil, dm%dpcp)
!----------------------------------------------------------------------------------------------------------
!   qx related b.c.
!----------------------------------------------------------------------------------------------------------
    if(is_fbcy_velo_required) then
      ! qxiy_ppc_ypencil --> qxdx_cpc_ypencil (for div. b.c.)
      call transpose_y_to_x (qxiy_ppc_ypencil, appc_xpencil, dm%dppc)
      if(is_fbcx_velo_required) then
        call extract_dirichlet_fbcx(fbcx_4pc, appc_xpencil, dm%dppc)
      else
        fbcx_4pc = MAXP
      end if
      !
      call Get_x_1der_P2C_3D(appc_xpencil, acpc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, fbcx_4pc)
      call transpose_x_to_y(acpc_xpencil, qxdx_cpc_ypencil, dm%dcpc)
    end if

    if(is_fbcz_velo_required) then
      ! qxiz_pcp_zpencil --> qxdx_ccp_zpencil (for div. b.c.)
      call transpose_z_to_y(qxiz_pcp_zpencil, apcp_ypencil, dm%dpcp)
      call transpose_y_to_x(apcp_ypencil,     apcp_xpencil, dm%dpcp)
      if(is_fbcx_velo_required) then
        call extract_dirichlet_fbcx(fbcx_4cp, apcp_xpencil, dm%dpcp)
      else
        fbcx_4cp = MAXP
      end if
      call Get_x_1der_P2C_3D(apcp_xpencil, accp_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, fbcx_4cp)
      call transpose_x_to_y(accp_xpencil, accp_ypencil,     dm%dccp)
      call transpose_y_to_z(accp_ypencil, qxdx_ccp_zpencil, dm%dccp) 
    end if
#ifdef DEBUG_STEPS
    call Print_debug_mid_msg('qx preparation ... done.')
#endif
!----------------------------------------------------------------------------------------------------------
!    qy
!    | -[ipx]-> qyix_ppc_xpencil(common) -[x2y]-> qyix_ppc_ypencil(no thermal) 
!    | -[1dx]-> qydx_ppc_xpencil(common) -[x2y]-> qydx_ppc_ypencil(common)
!    | -[x2y]-> qy_ypencil(temp) 
!               | -[ipy]-> qyiy_ccc_ypencil
!               | -[1dy]-> qydy_ccc_ypencil(no-cly, div) 
!               | -[y2z]-> qy_zpencil(temp) 
!                          | -[ipz]-> qyiz_cpp_zpencil(no-cly) -[z2y]-> qyiz_cpp_ypencil(no-thermal, cly)    
!                          | -[1dz]-> qydz_cpp_zpencil(no-cly) -[z2y]-> qydz_cpp_ypencil
!----------------------------------------------------------------------------------------------------------
    call Get_x_1der_C2P_3D(fl%qy, qydx_ppc_xpencil, dm, dm%iAccuracy, dm%ibcx_qy, dm%fbcx_qy)
    !
    call transpose_x_to_y(qydx_ppc_xpencil, qydx_ppc_ypencil, dm%dppc)
    !
    call Get_x_midp_C2P_3D(fl%qy, qyix_ppc_xpencil, dm, dm%iAccuracy, dm%ibcx_qy, dm%fbcx_qy)
    !
    if(.not. dm%is_thermo) then
      call transpose_x_to_y (qyix_ppc_xpencil, qyix_ppc_ypencil, dm%dppc)
    end if
    !
    call transpose_x_to_y (fl%qy, acpc_ypencil, dm%dcpc) !acpc_ypencil = qy_ypencil
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qy_ypencil', acpc_ypencil(4, 1, 4),  acpc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   acpc_ypencil(4, 1, 4)-acpc_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
    call Get_y_1der_P2C_3D(acpc_ypencil, qydy_ccc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
    !
    call Get_y_midp_P2C_3D(acpc_ypencil, qyiy_ccc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
    !
    call transpose_y_to_z(acpc_ypencil, acpc_zpencil, dm%dcpc)!acpc_zpencil = qy_zpencil
    call Get_z_1der_C2P_3D(acpc_zpencil, acpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_qy) ! acpp_zpencil = qydz
    call transpose_z_to_y(acpp_zpencil, qydz_cpp_ypencil, dm%dcpp)
    !
    if(dm%icoordinate == ICARTESIAN) then
      qydz_cpp_zpencil = acpp_zpencil
    end if
    call Get_z_midp_C2P_3D(acpc_zpencil, acpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_qy) ! acpp_zpencil = qyiz
    !
    if(dm%icoordinate == ICARTESIAN) then
      qyiz_cpp_zpencil = acpp_zpencil
    end if
    if(.not. dm%is_thermo) then
      call transpose_z_to_y(acpp_zpencil, qyiz_cpp_ypencil, dm%dcpp)
    end if
!----------------------------------------------------------------------------------------------------------
!   qy related b.c.
!----------------------------------------------------------------------------------------------------------
    if(is_fbcx_velo_required) then
      !qyix_ppc_xpencil --> qydy_pcc_xpencil  for div b.c.
      call transpose_x_to_y (qyix_ppc_xpencil, appc_ypencil, dm%dppc)
      if(is_fbcy_velo_required) then
        call extract_dirichlet_fbcy(fbcy_p4c, appc_ypencil, dm%dppc, dm, is_reversed = .true.)
      else
        fbcy_p4c = MAXP
      end if
      call Get_y_1der_P2C_3D(appc_ypencil, apcc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, fbcy_p4c)
      call transpose_y_to_x(apcc_ypencil, qydy_pcc_xpencil, dm%dpcc) 
    end if

    if(is_fbcy_velo_required) then
      !qydy_cpc_ypencil for div b.c.
      !acpc_ypencil = qy_ypencil
      call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil,     dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
      call Get_y_1der_C2P_3D(accc_ypencil, qydy_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
!write(*,*) 'qydy_cpc_ypencil', qydy_cpc_ypencil(4, 1, 1), qydy_cpc_ypencil(4, 1, dm%knc_sym(1)) ! not symmetric, correct
    end if
    if(is_fbcz_velo_required) then
      !qyiz_cpp_zpencil --> qydy_ccp_zpencil  for div b.c.
      !acpp_zpencil = qyiz_cpp_zpencil
      call transpose_z_to_y(acpp_zpencil, acpp_ypencil, dm%dcpp)
      if(is_fbcy_velo_required) then
        call extract_dirichlet_fbcy(fbcy_c4p, acpp_ypencil, dm%dcpp, dm, is_reversed = .true.)
      else
        fbcy_c4p = MAXP
      end if
      call Get_y_1der_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, fbcy_c4p)
      call transpose_y_to_z(accp_ypencil, qydy_ccp_zpencil, dm%dpcc) 
    end if
#ifdef DEBUG_STEPS
     call Print_debug_mid_msg('qy preparation ... done.')
#endif
!----------------------------------------------------------------------------------------------------------
!    qz 
!    | -[1dx]-> qzdx_pcp_xpencil(common) -[x2y]-> qzdx_pcp_ypencil(temp) -[y2z]-> qzdx_pcp_zpencil(common)
!    | -[ipx]-> qzix_pcp_xpencil(common) -[x2y]-> qzix_pcp_ypencil(temp) -[y2z]-> qzix_pcp_zpencil(no-thermal)
!    | -[x2y]-> qz_ypencil(temp) 
!               | -[1dy]-> qzdy_cpp_ypencil(no-cly) -[y2z]-> qzdy_cpp_zpencil
!               | -[ipy]-> qziy_cpp_ypencil(no-cly) -[y2z]-> qziy_cpp_zpencil(no thermal)
!               | -[y2z]-> qz_zpencil(temp) 
!                          | -[ipz]-> qziz_ccc_zpencil(common)
!                          | -[1dz]-> qzdz_ccc_zpencil(common) -[y2z]-> qzdz_ccc_ypencil(cly)
!----------------------------------------------------------------------------------------------------------
    call Get_x_1der_C2P_3D(fl%qz, qzdx_pcp_xpencil, dm, dm%iAccuracy, dm%ibcx_qz, dm%fbcx_qz)
    call transpose_x_to_y(qzdx_pcp_xpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_z(apcp_ypencil, qzdx_pcp_zpencil, dm%dpcp)

    call Get_x_midp_C2P_3D(fl%qz, qzix_pcp_xpencil, dm, dm%iAccuracy, dm%ibcx_qz, dm%fbcx_qz)
    !if(.not. dm%is_thermo) then
      call transpose_x_to_y(qzix_pcp_xpencil, apcp_ypencil, dm%dpcp) !qzix_pcp_ypencil
      call transpose_y_to_z(apcp_ypencil, qzix_pcp_zpencil, dm%dpcp)
    !end if
    call transpose_x_to_y(fl%qz, accp_ypencil, dm%dccp) ! qz_ypencil
    call Get_y_midp_C2P_3D(accp_ypencil, qziy_cpp_ypencil, dm, dm%iAccuracy, dm%ibcy_qz, dm%fbcy_qz) ! z-mom
    !call axis_estimating_radial_xpx(qziy_cpp_ypencil, dm%dcpp, IPENCIL(2), dm, IDIM(3))
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qziy_cpp_ypencil', qziy_cpp_ypencil(4, 1, 4),  qziy_cpp_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qziy_cpp_ypencil(4, 1, 4)+qziy_cpp_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
    !
    call Get_y_1der_C2P_3D(accp_ypencil, qzdy_cpp_ypencil, dm, dm%iAccuracy, dm%ibcy_qz, dm%fbcy_qz) ! check !
    !if(dm%icase == ICASE_PIPE) qzdy_cpp_ypencil(:, 1, :) = ZERO ! check , test
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qzdy_cpp_ypencil', qzdy_cpp_ypencil(4, 1, 4),  qzdy_cpp_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qzdy_cpp_ypencil(4, 1, 4)-qzdy_cpp_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
!qzdy_cpp_ypencil(:, 1, :) = ZERO ! check , test
!write(*,*) 'qzdy_cpp_ypencil', qzdy_cpp_ypencil(4, 1, 1), qzdy_cpp_ypencil(4, 1, dm%knc_sym(1)) ! correct, symmetric
    !
    call transpose_y_to_z(qzdy_cpp_ypencil, qzdy_cpp_zpencil, dm%dcpp)
    !
    if((.not. dm%is_thermo) .or. (dm%icoordinate == ICYLINDRICAL)) then
        call transpose_y_to_z(qziy_cpp_ypencil, qziy_cpp_zpencil, dm%dcpp)
    end if
    !
    call transpose_y_to_z(accp_ypencil, qz_zpencil, dm%dccp) ! qz_zpencil
    call Get_z_midp_P2C_3D(qz_zpencil, qziz_ccc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    call Get_z_1der_P2C_3D(qz_zpencil, qzdz_ccc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    !
    if(dm%icoordinate == ICYLINDRICAL) then
      call transpose_z_to_y(qziz_ccc_zpencil, qziz_ccc_ypencil, dm%dccc)
      call transpose_z_to_y(qzdz_ccc_zpencil, qzdz_ccc_ypencil, dm%dccc)
    end if
!----------------------------------------------------------------------------------------------------------
!   qz related b.c.
!----------------------------------------------------------------------------------------------------------
    if(is_fbcx_velo_required) then
      !qzdz_pcc_xpencil for div b.c.
      if(is_fbcz_velo_required) then
        call extract_dirichlet_fbcz(fbcz_pc4, qzix_pcp_zpencil, dm%dpcp)
      else
        fbcz_pc4 = MAXP
      end if
      call Get_z_1der_P2C_3D(qzix_pcp_zpencil, apcc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, fbcz_pc4)
      call transpose_z_to_y(apcc_zpencil, apcc_ypencil, dm%dpcc )
      call transpose_y_to_x(apcc_ypencil, qzdz_pcc_xpencil, dm%dpcc)
    end if

    if(is_fbcy_velo_required) then
      ! qzdz_cpc_ypencil for div b.c.
      call transpose_y_to_z(qziy_cpp_ypencil, acpp_zpencil, dm%dcpp)
      if(is_fbcz_velo_required) then
        call extract_dirichlet_fbcz(fbcz_cp4, acpp_zpencil, dm%dcpp)
      else
        fbcz_cp4 = MAXP
      end if
      call Get_z_1der_P2C_3D(acpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, fbcz_cp4) !acpc_zpencil = qzdz
      call transpose_z_to_y(acpc_zpencil, qzdz_cpc_ypencil, dm%dcpc)
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qzdz_cpc_ypencil', qzdz_cpc_ypencil(4, 1, 4),  qzdz_cpc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qzdz_cpc_ypencil(4, 1, 4)+qzdz_cpc_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
    end if

    if(is_fbcz_velo_required) then
      ! qzdz_ccp_zpencil for div b.c.
      call Get_z_1der_C2P_3D(qziz_ccc_zpencil, qzdz_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    end if
#ifdef DEBUG_STEPS
     call Print_debug_mid_msg('qz preparation ... done.')
#endif
!==========================================================================================================
! preparation of intermediate variables to be used - thermal only
!==========================================================================================================
    ! if(dm%is_thermo) then
         mu_ccc_xpencil = ONE ! x-v1, default for flow only
         mu_ccc_ypencil = ONE ! y-v2, default for flow only
         mu_ccc_zpencil = ONE ! z-v3, default for flow only
       muiz_ccp_zpencil = ONE
      muixy_ppc_xpencil = ONE ! y-v1, default for flow only
      muixy_ppc_ypencil = ONE ! x-v2, default for flow only
      muixz_pcp_xpencil = ONE ! z-v1, default for flow only
      muixz_pcp_zpencil = ONE ! x-v3, default for flow only
      muiyz_cpp_ypencil = ONE ! z-v2, default for flow only
      muiyz_cpp_zpencil = ONE ! y-v3, default for flow only
            fbcx_mu_4cc = ONE
            fbcy_mu_c4c = ONE
            fbcz_mu_cc4 = ONE
    ! end if
    if(dm%is_thermo) then
!----------------------------------------------------------------------------------------------------------
!    gx 
!    | -[ipx]-> gxix_ccc_xpencil
!    | -[x2y]-> gx_ypencil(temp) 
!               | -[ipy]-> gxiy_ppc_ypencil(temp) -[y2x]-> gxiy_ppc_xpencil
!               | -[y2z]-> gx_zpencil(temp) 
!                          | -[ipz]-> gxiz_pcp_zpencil(temp) -[z2y]-> gxiz_pcp_ypencil(temp) -[y2x]-> gxiz_pcp_xpencil
!----------------------------------------------------------------------------------------------------------
      call Get_x_midp_P2C_3D(fl%gx, gxix_ccc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_gx)
      !
      call transpose_x_to_y (fl%gx, apcc_ypencil, dm%dpcc) !gx_ypencil
      call Get_y_midp_C2P_3D(apcc_ypencil, appc_ypencil, dm, dm%iAccuracy, dm%ibcy_qx, dm%fbcy_gx)
      !call axis_estimating_radial_xpx(appc_ypencil, dm%dppc, IPENCIL(2), dm, IDIM(1))
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'gxiy_ppc_ypencil', appc_ypencil(4, 1, 4),  appc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   appc_ypencil(4, 1, 4)-appc_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
      call transpose_y_to_x (appc_ypencil, gxiy_ppc_xpencil, dm%dppc)
      !
      call transpose_y_to_z (apcc_ypencil, apcc_zpencil, dm%dpcc)!gx_zpencil
      call Get_z_midp_C2P_3D(apcc_zpencil, apcp_zpencil, dm, dm%iAccuracy, dm%ibcz_qx, dm%fbcz_gx)
      call transpose_z_to_y (apcp_zpencil, apcp_ypencil, dm%dpcc)!qxiz_pcp_ypencil
      call transpose_y_to_x (apcp_ypencil, gxiz_pcp_xpencil, dm%dpcp)
#ifdef DEBUG_STEPS
      call Print_debug_mid_msg('gx preparation ... done.')
#endif
!----------------------------------------------------------------------------------------------------------
!    gy
!    | -[ipx]-> gyix_ppc_xpencil(temp) -[x2y]-> gyix_ppc_ypencil
!    | -[x2y]-> gy_ypencil(temp) 
!               | -[ipy]-> gyiy_ccc_ypencil
!               | -[y2z]-> gy_zpencil(temp) 
!                          | -[ipz]-> gyiz_cpp_zpencil(temp) -[z2y]-> gyiz_cpp_ypencil          
!----------------------------------------------------------------------------------------------------------
      call Get_x_midp_C2P_3D(fl%gy, appc_xpencil, dm, dm%iAccuracy, dm%ibcx_qy, dm%fbcx_gy)
      call transpose_x_to_y (appc_xpencil, gyix_ppc_ypencil, dm%dppc)
!write(*,*) 'gyix_ppc_ypencil', gyix_ppc_ypencil(4, 1, 1:4)

      call transpose_x_to_y (fl%gy, acpc_ypencil, dm%dcpc) !acpc_ypencil = gy_ypencil
      call Get_y_midp_P2C_3D(acpc_ypencil, gyiy_ccc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_gy)

      call transpose_y_to_z(acpc_ypencil, acpc_zpencil, dm%dcpc)!qy_zpencil
      call Get_z_midp_C2P_3D(acpc_zpencil, acpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_gy)
      call transpose_z_to_y(acpp_zpencil, gyiz_cpp_ypencil, dm%dcpp)
!write(*,*) 'gyiz_cpp_ypencil', gyiz_cpp_ypencil(4, 1, 1:4)
#ifdef DEBUG_STEPS
      call Print_debug_mid_msg('gy preparation ... done.')
#endif
!----------------------------------------------------------------------------------------------------------
!    gz 
!    | -[ipx]-> gzix_pcp_xpencil(temp) -[x2y]-> gzix_pcp_ypencil(temp) -[y2z]-> gzix_pcp_zpencil
!    | -[x2y]-> gz_ypencil(temp) 
!               | -[ipy]-> gziy_cpp_ypencil(temp) -[y2z]-> gziy_cpp_zpencil
!               | -[y2z]-> gz_zpencil(temp) 
!                          | -[ipz]-> gziz_ccc_zpencil
!----------------------------------------------------------------------------------------------------------
      call Get_x_midp_C2P_3D(fl%gz, apcp_xpencil, dm, dm%iAccuracy, dm%ibcx_qz, dm%fbcx_gz)
      !
      call transpose_x_to_y(apcp_xpencil, apcp_ypencil, dm%dpcp)
      call transpose_y_to_z(apcp_ypencil, gzix_pcp_zpencil, dm%dpcp)
      !
      call transpose_x_to_y(fl%gz, accp_ypencil, dm%dccp) ! gz_ypencil
      call Get_y_midp_C2P_3D(accp_ypencil, gziy_cpp_ypencil, dm, dm%iAccuracy, dm%ibcy_qz, dm%fbcy_gz)
      !call axis_estimating_radial_xpx(acpp_ypencil, dm%dcpp, IPENCIL(2), dm, IDIM(3))
! #ifdef DEBUG_STEPS 
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'gziy_cpp_ypencil', gziy_cpp_ypencil(4, 1, 4),  gziy_cpp_ypencil(4, 1, dm%knc_sym(4)) , &
!                   gziy_cpp_ypencil(4, 1, 4)-gziy_cpp_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
      call transpose_y_to_z(gziy_cpp_ypencil, gziy_cpp_zpencil, dm%dcpp)
      !
      call transpose_y_to_z(accp_ypencil, accp_zpencil, dm%dccp) ! gz_zpencil
      call Get_z_midp_P2C_3D(accp_zpencil, gziz_ccc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_gz)
      if(dm%icoordinate == ICYLINDRICAL) then
        call transpose_z_to_y(gziz_ccc_zpencil, gziz_ccc_ypencil, dm%dccc)
      end if
#ifdef DEBUG_STEPS
      call Print_debug_mid_msg('gz preparation ... done.')
#endif   
!----------------------------------------------------------------------------------------------------------
!    m = mu_ccc_xpencil <need BCx>
!             BCx:|-->mu_4cc_xpencil
!    | -[ipx]-> muix_pcc_xpencil(temp) -[x2y]-> muix_pcc_ypencil(temp)
!                                               | -[ipy]-> muixy_ppc_ypencil -[y2z]-> muixy_ppc_xpencil
!                                               | -[y2z]-> muix_pcc_zpencil(temp) -[ipz]-> muixz_pcp_zpencil -[z2y]-> muixz_pcp_ypencil (temp) -[y2z]-> muixz_pcp_xpencil
!    | -[x2y]-> mu_ccc_ypencil <need BCy> -[y2z]-> mu_ccc_zpencil <need BCz>
!                                             BCz:|-[ipz]->mu_ccp_zpencil -> mu_cc4_zpencil
!              | -[ipy]-> muiy_cpc_ypencil(temp) -[y2z]-> muiy_cpc_zpencil(temp) -[ipz]-> muiyz_cpp_zpencil -[z2y]-> muiyz_cpp_ypencil
!                     BCy:|->mu_c4c_ypencil                                           
!----------------------------------------------------------------------------------------------------------
! fbcy_c4c               ! fbcy_p4c
! ^z_________________    ! ^z_________________
! |   |   |   |   |      ! |   |   |   |   | 
! | O | O | O | O |      ! O   O   O   O   O
! |___|___|___|___|__    ! |___|___|___|___|__  
! |   |   |   |   |      ! |   |   |   |   |    
! | O | O | O | O |      ! O   O   O   O   O
! |___|___|___|___|__>x  ! |___|___|___|___|__>x
! the interpolation for constant value is exact same. ignore the warning message if pops up.
!----------------------------------------------------------------------------------------------------------
      fbcx_4cc = MAXP
      fbcy_c4c = MAXP 
      fbcz_cc4 = MAXP 
      fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%m
      fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%m
      fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%m
#ifdef DEBUG_STEPS
      write(*,*) 'fbcy_m', fbcy_c4c(1, 1:4, 1)
#endif
      mu_ccc_xpencil = opt_visc
      call transpose_x_to_y(mu_ccc_xpencil, mu_ccc_ypencil, dm%dccc)
      call transpose_y_to_z(mu_ccc_ypencil, mu_ccc_zpencil, dm%dccc)
      !
      call Get_y_midp_C2P_3D(mu_ccc_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_c4c)
      !call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(1))
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'mu_cpc_ypencil', acpc_ypencil(4, 1, 4),  acpc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   acpc_ypencil(4, 1, 4)-acpc_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
      call transpose_y_to_x(acpc_ypencil, acpc_xpencil, dm%dcpc) !acpc_xpencil = muiy_cpc_xpencil
      if(is_fbcx_velo_required) call get_fbcx_ftp_4pc(fbcx_4cc, fbcx_4pc, dm)
      call Get_x_midp_C2P_3D(acpc_xpencil, muixy_ppc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp, fbcx_4pc)

      !
      call transpose_x_to_y(muixy_ppc_xpencil, muixy_ppc_ypencil, dm%dppc)
      !
      call Get_x_midp_C2P_3D(mu_ccc_xpencil, apcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp, fbcx_4cc)
      call transpose_x_to_y(apcc_xpencil, apcc_ypencil, dm%dpcc)
      call transpose_y_to_z(apcc_ypencil, apcc_zpencil, dm%dpcc)
      call Get_z_midp_C2P_3D(apcc_zpencil, muixz_pcp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp)!, fbcz_pc4)
      !
      call transpose_z_to_y(muixz_pcp_zpencil, apcp_ypencil, dm%dpcp)
      call transpose_y_to_x(apcp_ypencil, muixz_pcp_xpencil, dm%dpcp)
      !
      call transpose_y_to_z(acpc_ypencil, acpc_zpencil, dm%dcpc) !muiy_cpc_zpencil
      call Get_z_midp_C2P_3D(acpc_zpencil, muiyz_cpp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp)!, fbcz_cp4)
      !
      call transpose_z_to_y(muiyz_cpp_zpencil, muiyz_cpp_ypencil, dm%dcpp)
      !
      if(is_fbcx_velo_required) then
        call extract_dirichlet_fbcx(fbcx_mu_4cc, apcc_xpencil, dm%dpcc)
      end if

      if(is_fbcy_velo_required) then
        call extract_dirichlet_fbcy(fbcy_mu_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .false.)
      end if

      if(is_fbcz_velo_required) then
        call Get_z_midp_C2P_3D(mu_ccc_zpencil, muiz_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp, fbcz_cc4) ! accp_zpencil = muiz_ccp_zpencil
        call extract_dirichlet_fbcz(fbcz_mu_cc4, accp_zpencil, dm%dccp)
      end if
#ifdef DEBUG_STEPS
      call Print_debug_mid_msg('mu preparation ... done.')
#endif
    end if

!==========================================================================================================
! preparation of intermediate variables to be used - cylindrical only
!==========================================================================================================
    if(dm%icoordinate == ICYLINDRICAL) then
!----------------------------------------------------------------------------------------------------------
!    qy/r=qyr
!    | -[x2y]-> qyr_ypencil(temp) 
!               | -[ipy]-> qyriy_ccc_ypencil-[y2z]->qyriy_ccc_zpencil
!               | -[1dy]-> qyrdy_ccc_ypencil
!               | -[y2z]-> qyr_zpencil(temp) 
!                          | -[ipz]-> qyriz_cpp_zpencil -[z2y]-> qyriz_cpp_ypencil (no-thermal)        
!                          | -[1dz]-> qyrdz_cpp_zpencil -[z2y]-> qyrdz_cpp_ypencil
!     qy/r in r=0 is not zero, but qy is zero! qy/r = 0.5 * (qy/r + qy/r_sym)
!----------------------------------------------------------------------------------------------------------
      acpc_xpencil = fl%qy
      call multiple_cylindrical_rn(acpc_xpencil, dm%dcpc, dm%rpi, 1, IPENCIL(1)) ! qr/r
      call transpose_x_to_y(acpc_xpencil, qyr_ypencil, dm%dcpc) ! acpc_ypencil = qyr_ypencil
      !qyr_ypencil(:, 1, :) = dm%fbcy_qyr(:, 1, :)
      call axis_estimating_radial_xpx(qyr_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(2), is_reversed = .true.)
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qyr_ypencil', qyr_ypencil(4, 1, 4),  qyr_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qyr_ypencil(4, 1, 4)-qyr_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
      !
      call Get_y_midp_P2C_3D(qyr_ypencil, qyriy_ccc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qyr)
      !
      call transpose_y_to_z(qyriy_ccc_ypencil, qyriy_ccc_zpencil, dm%dccc)
      !
      call Get_y_1der_P2C_3D(qyr_ypencil, qyrdy_ccc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qyr) ! check r treatment
      !
      call transpose_y_to_z(qyr_ypencil, acpc_zpencil, dm%dcpc)
      call Get_z_midp_C2P_3D(acpc_zpencil, qyriz_cpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_qyr)
      !
      if(.not. dm%is_thermo) call transpose_z_to_y(qyriz_cpp_zpencil, qyriz_cpp_ypencil, dm%dcpp)
      !
      call Get_z_1der_C2P_3D(acpc_zpencil, acpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_qyr)
      call transpose_z_to_y(acpp_zpencil, qyrdz_cpp_ypencil, dm%dcpp)
!write(*,*) 'qyrdz_cpp_ypencil', qyrdz_cpp_ypencil(4, 1, 1:4) ! all difference, check!
      !
      acpc_xpencil = fl%qy
      call multiple_cylindrical_rn(acpc_xpencil, dm%dcpc, dm%rpi, 2, IPENCIL(1)) ! qr/r^2
      call transpose_x_to_y(acpc_xpencil, qyr2_ypencil, dm%dcpc) ! acpc_ypencil = qr/r^2_ypencil
      call axis_estimating_radial_xpx(qyr2_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(2), is_reversed = .true.)
!write(*,*) 'qyr2_ypencil', qyr2_ypencil(4, 1, 1:4)
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qyr2_ypencil', qyr2_ypencil(4, 1, 4),  qyr2_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qyr2_ypencil(4, 1, 4)+qyr2_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
      !
      call transpose_y_to_z(qyr2_ypencil, acpc_zpencil, dm%dcpc)
      call Get_z_1der_C2P_3D(acpc_zpencil, qyr2dz_cpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_qyr) ! to check, this bc is not used for peridoic z
      !
      call transpose_z_to_y(qyr2dz_cpp_zpencil, qyr2dz_cpp_ypencil, dm%dcpp)
!write(*,*) 'qyr2dz_cpp_ypencil', qyr2dz_cpp_ypencil(4, 1, 1:4)
#ifdef DEBUG_STEPS
      call Print_debug_mid_msg('qyr preparation ... done.')
#endif
!----------------------------------------------------------------------------------------------------------
!    qz/r=qzr
!    | -[x2y]-> qzr_ypencil(temp) 
!               | -[ipy]-> qzriy_cpp_ypencil -[y2z]-> qzriy_cpp_zpencil
!               | -[1dy]-> qzrdy_cpp_ypencil -[y2z]-> qzrdy_cpp_zpencil
!               | -[y2z]-> qzr_zpencil(temp) -[ipz]-> qzriz_ccc_zpencil(temp) -[z2y]- qzriz_ccc_ypencil
!----------------------------------------------------------------------------------------------------------
       accp_xpencil = fl%qz
       call multiple_cylindrical_rn(accp_xpencil, dm%dccp, dm%rci, 1, IPENCIL(1)) ! qz/r
       call transpose_x_to_y(accp_xpencil, accp_ypencil, dm%dccp)
       call Get_y_midp_C2P_3D(accp_ypencil, qzriy_cpp_ypencil, dm, dm%iAccuracy, dm%ibcy_qz, dm%fbcy_qzr)
       !call axis_estimating_radial_xpx(qzriy_cpp_ypencil, dm%dcpp, IPENCIL(2), dm, IDIM(0))
! #ifdef DEBUG_STEPS  
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qzriy_cpp_ypencil', qzriy_cpp_ypencil(4, 1, 4),  qzriy_cpp_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qzriy_cpp_ypencil(4, 1, 4)+qzriy_cpp_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
       call transpose_y_to_z(qzriy_cpp_ypencil, qzriy_cpp_zpencil, dm%dcpp)
      !
      if(is_fbcy_velo_required) then
        call Get_z_1der_P2C_3D(qzriy_cpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz) ! accc_zpencil = d(qz/r)/dz
        call transpose_z_to_y(acpc_zpencil, qzrdz_cpc_ypencil, dm%dcpc)
      end if
! #ifdef DEBUG_STEPS
!     ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) 'qzrdz_cpc_ypencil', qzrdz_cpc_ypencil(4, 1, 4),  qzrdz_cpc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   qzrdz_cpc_ypencil(4, 1, 4)+qzrdz_cpc_ypencil(4, 1, dm%knc_sym(4))
!     end if
!       call Print_debug_mid_msg('qzr preparation ... done.')
! #endif
    end if
!==========================================================================================================
! preparation of div
! div = qxdx + 1/r * qydy + 1/r qzdz
! r * div = r * qxdx + qydy + qzdz
!==========================================================================================================
    ! r * div_tmp = r * qxdx
    div_ccc_xpencil  = ZERO 
    accc_xpencil = qxdx_ccc_xpencil
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_xpencil, dm%dccc, dm%rc, 1, IPENCIL(1))
    div_ccc_xpencil = div_ccc_xpencil + accc_xpencil !

    ! r * div_tmp = r * qxdx + qydy
    call transpose_y_to_x (qydy_ccc_ypencil, accc_xpencil, dm%dccc)   
    div_ccc_xpencil = div_ccc_xpencil + accc_xpencil

    ! r * div_tmp = r * qxdx + qydy + qzdz
    call transpose_z_to_y (qzdz_ccc_zpencil, accc_ypencil, dm%dccc)   
    call transpose_y_to_x (accc_ypencil, accc_xpencil, dm%dccc)   
    div_ccc_xpencil = div_ccc_xpencil + accc_xpencil

    ! div_tmp = (r * div_tmp) / r
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div_ccc_xpencil, dm%dccc, dm%rci, 1, IPENCIL(1))
    call transpose_x_to_y (div_ccc_xpencil, div_ccc_ypencil, dm%dccc)
    call transpose_y_to_z (div_ccc_ypencil, div_ccc_zpencil, dm%dccc)
    
    ! fbcx_div_4cc, qxdx + 1/r * qydy + 1/r qzdz
    if(is_fbcx_velo_required) then
      call Get_x_1der_P2P_3D(fl%qx, qxdx_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_qx)
      apcc_xpencil1 = qxdx_pcc_xpencil
      if(dm%icoordinate == ICYLINDRICAL) &
      call multiple_cylindrical_rn(apcc_xpencil1, dm%dpcc, dm%rc, 1, IPENCIL(1))
      apcc_xpencil2 = qydy_pcc_xpencil
      apcc_xpencil3 = qzdz_pcc_xpencil
      apcc_xpencil = apcc_xpencil1 + apcc_xpencil2 + apcc_xpencil3
      if(dm%icoordinate == ICYLINDRICAL) &
      call multiple_cylindrical_rn(apcc_xpencil, dm%dpcc, dm%rci, 1, IPENCIL(1))
      call extract_dirichlet_fbcx(fbcx_div_4cc, apcc_xpencil, dm%dpcc)
    end if

    ! fbcy_div_c4c, qxdx + 1/r * qydy + 1/r qzdz
    if(is_fbcy_velo_required) then
      call extract_dirichlet_fbcy(fbcy_div_c4c1, qxdx_cpc_ypencil, dm%dcpc, dm, is_reversed = .false.)
      fbcy_div_c4c = fbcy_div_c4c1

      acpc_ypencil = qydy_cpc_ypencil
      if(dm%icoordinate == ICYLINDRICAL) &
      call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rpi, 1, IPENCIL(2)) ! (qydy)/r_cpc_ypencil
      call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(2), is_reversed = .true.)
! #ifdef DEBUG_STEPS  
!       ! serial only
!     if(dm%icase == ICASE_PIPE) then
!       write(*,*) '(qydy)/r_cpc_ypencil', acpc_ypencil(4, 1, 4),  acpc_ypencil(4, 1, dm%knc_sym(4)) , &
!                   acpc_ypencil(4, 1, 4)+acpc_ypencil(4, 1, dm%knc_sym(4))
!     end if
! #endif
      call extract_dirichlet_fbcy(fbcy_div_c4c1, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
      fbcy_div_c4c = fbcy_div_c4c + fbcy_div_c4c1

      
      if(dm%icoordinate == ICYLINDRICAL) then
        acpc_ypencil = qzrdz_cpc_ypencil
      else
        acpc_ypencil = qzdz_cpc_ypencil
      end if
      call extract_dirichlet_fbcy(fbcy_div_c4c1, acpc_ypencil, dm%dcpc, dm, is_reversed = .false.)
      fbcy_div_c4c = fbcy_div_c4c + fbcy_div_c4c1
    end if

    ! fbcz_div_cc4
    if(is_fbcz_velo_required) then

      accp_zpencil1 = qxdx_ccp_zpencil
      if(dm%icoordinate == ICYLINDRICAL) &
      call multiple_cylindrical_rn(accp_zpencil1, dm%dccp, dm%rc, 1, IPENCIL(3))
      accp_zpencil2 = qydy_ccp_zpencil
      accp_zpencil3 = qzdz_ccp_zpencil
      accp_zpencil  = accp_zpencil1 + accp_zpencil2 + accp_zpencil3
      if(dm%icoordinate == ICYLINDRICAL) &
      call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
      call extract_dirichlet_fbcz(fbcz_div_cc4, accp_zpencil, dm%dccp)
    end if
#ifdef DEBUG_STEPS
     call Print_debug_mid_msg('div preparation ... done.')
#endif
!==========================================================================================================
! The governing equation x is 
! d(gx)/dt = -        d(gxix * qxix)/dx                               ! conv-x-m1         
!            - 1/r  * d(gyix * qxiy)/dy                               ! conv-y-m1       
!            - 1/r  * d(gzix * qxiz)/dz                               ! conv-z-m1        
!            - dpdx                                                   ! p-m1
!            + d[ 2 * mu * (qxdx - 1/3 * div)]/dx                     ! diff-x-m1
!            + 1/r  * d[muixy * (qydx + r * qxdy)]/dy                  ! diff-y-m1
!            + 1/r  * d[muixz * (qzdx + 1/r*qxdz)]/dz                  ! diff-z-m1
!==========================================================================================================
    i = 1
    fl%mx_rhs      = ZERO
    mx_rhs_ypencil = ZERO
    mx_rhs_zpencil = ZERO
    mx_rhs_pfc_xpencil = ZERO
!----------------------------------------------------------------------------------------------------------
! X-mom convection term 1/3 at (i', j, k): 
! conv-x-m1 = - d(gxix * qxix)/dx
!----------------------------------------------------------------------------------------------------------  
    !------b.c.------
    if(is_fbcx_velo_required) then
      if ( .not. dm%is_thermo) then
        fbcx_4cc = dm%fbcx_qx
      else
        fbcx_4cc = dm%fbcx_gx
      end if
      fbcx_4cc = - fbcx_4cc * dm%fbcx_qx
    else
      fbcx_4cc = MAXP
    end if
    !------bulk------
    if ( .not. dm%is_thermo) then
      accc_xpencil = qxix_ccc_xpencil
    else
      accc_xpencil = gxix_ccc_xpencil
    end if
    accc_xpencil = - accc_xpencil * qxix_ccc_xpencil
    !------PDE------
    call Get_x_1der_C2P_3D(accc_xpencil, apcc_xpencil, dm, dm%iAccuracy, mbcx_cov1, fbcx_4cc)
    fl%mx_rhs = fl%mx_rhs + apcc_xpencil

#ifdef DEBUG_STEPS
    write(*,*) 'conx-11', apcc_xpencil(1, 1:4, 1)
    call is_valid_number_3D(apcc_xpencil, 'conx-11')
#endif
!----------------------------------------------------------------------------------------------------------
! X-mom convection term 2/3 at (i', j, k): 
! conv-y-m1 = - 1/r  * d(gyix * qxiy)/dy
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    if ( .not. dm%is_thermo) then
      appc_ypencil = qyix_ppc_ypencil
    else
      appc_ypencil = gyix_ppc_ypencil
    end if
    appc_ypencil = -appc_ypencil * qxiy_ppc_ypencil  
    !------b.c.------
    if(is_fbcy_velo_required) then
      call extract_dirichlet_fbcy(fbcy_p4c, appc_ypencil, dm%dppc, dm, is_reversed = .true.)
    else
      fbcy_p4c = MAXP
    end if
    !------PDE------
    call Get_y_1der_P2C_3D(appc_ypencil, apcc_ypencil, dm, dm%iAccuracy, mbcy_cov1, fbcy_p4c)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(apcc_ypencil, dm%dpcc, dm%rci, 1, IPENCIL(2))
    mx_rhs_ypencil = mx_rhs_ypencil + apcc_ypencil
    
#ifdef DEBUG_STEPS
    write(*,*) 'conx-12', apcc_ypencil(1, 1:4, 1)
    call is_valid_number_3D(appc_ypencil, 'conx-12')
#endif
!----------------------------------------------------------------------------------------------------------
! X-mom convection term 3/3 at (i', j, k) : 
! conv-z-m1 = - 1/r * d(gzix * qxiz)/dz
!----------------------------------------------------------------------------------------------------------
    !------bulk------    
    if ( .not. dm%is_thermo) then
      apcp_zpencil = qzix_pcp_zpencil
    else
      apcp_zpencil = gzix_pcp_zpencil
    end if
    apcp_zpencil = -apcp_zpencil * qxiz_pcp_zpencil   
    !------b.c.------
    if(is_fbcz_velo_required) then
      call extract_dirichlet_fbcz(fbcz_pc4, apcp_zpencil, dm%dpcp)
    else
      fbcz_pc4 = MAXP
    end if
    !------PDE------
    call Get_z_1der_P2C_3D(apcp_zpencil, apcc_zpencil, dm, dm%iAccuracy, mbcz_cov1, fbcz_pc4)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(apcc_zpencil, dm%dpcc, dm%rci, 1, IPENCIL(3))
    mx_rhs_zpencil = mx_rhs_zpencil + apcc_zpencil

#ifdef DEBUG_STEPS
    write(*,*) 'conx-13', apcc_zpencil(1, 1:4, 1)
    call is_valid_number_3D(apcc_zpencil, 'conx-13')
#endif
#ifdef DEBUG_STEPS
    apcc_test = fl%mx_rhs
    call transpose_y_to_x (mx_rhs_ypencil, apcc_xpencil, dm%dpcc)
    apcc_test =  apcc_test + apcc_xpencil
    call transpose_z_to_y (mx_rhs_zpencil, apcc_ypencil, dm%dpcc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    apcc_test =  apcc_test + apcc_xpencil 
    call wrt_3d_pt_debug(apcc_test,  dm%dpcc, fl%iteration, isub, 'ConX@bf st') ! debug_ww
#endif
!----------------------------------------------------------------------------------------------------------
! X-mom pressure gradient: 
! p-m1 = - dpdx
!----------------------------------------------------------------------------------------------------------
    call Get_x_1der_C2P_3D( -fl%pres * dm%sigma1p, apcc_xpencil, dm, dm%iAccuracy, dm%ibcx_pr, -dm%fbcx_pr)
    mx_rhs_pfc_xpencil=  mx_rhs_pfc_xpencil + apcc_xpencil
#ifdef DEBUG_STEPS
    write(*,*) 'px-11', apcc_xpencil(1, 1:4, 1)
    call is_valid_number_3D(apcc_xpencil, 'px-11')
#endif
!----------------------------------------------------------------------------------------------------------
! X-mom gravity in x direction, X-pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%is_thermo .and. (fl%igravity == i .or. fl%igravity == -i) )  then
      fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%d
      call Get_x_midp_C2P_3D(d_ccc_xpencil, apcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp, fbcx_4cc )
      mx_rhs_pfc_xpencil =  mx_rhs_pfc_xpencil + fl%fgravity(i) * apcc_xpencil
    end if
!----------------------------------------------------------------------------------------------------------
! X-mom Lorentz Force in x direction, x-pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%is_mhd) then
      mx_rhs_pfc_xpencil = mx_rhs_pfc_xpencil + fl%lrfx
    end if
!----------------------------------------------------------------------------------------------------------
! X-mom diffusion term 1/3 at (i', j, k)
! diff-x-m1 = d[ 2 * mu * (qxdx - 1/3 * div)]/dx
!----------------------------------------------------------------------------------------------------------
    !------b.c.------
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcx(fbcx_4cc, qxdx_pcc_xpencil, dm%dpcc)
      fbcx_4cc = TWO * fbcx_mu_4cc * ( fbcx_4cc - ONE_THIRD * fbcx_div_4cc)
    else
      fbcx_4cc = MAXP
    end if
    !------bulk------  
    accc_xpencil = TWO * mu_ccc_xpencil * ( qxdx_ccc_xpencil - ONE_THIRD * div_ccc_xpencil)
    !------PDE------
    call Get_x_1der_C2P_3D(accc_xpencil,  apcc_xpencil, dm, dm%iAccuracy, mbcx_tau1, fbcx_4cc) 
    fl%mx_rhs = fl%mx_rhs + apcc_xpencil * fl%rre

#ifdef DEBUG_STEPS
    write(*,*) 'visx-11', apcc_xpencil(1, 1:4, 1)
    call is_valid_number_3D(apcc_xpencil, 'visx-11')
#endif
!----------------------------------------------------------------------------------------------------------
! X-mom diffusion term 2/3 at (i', j, k)
! diff-y-m1 = 1/r  * d[muixy * (qydx + r * qxdy)]/dy
!----------------------------------------------------------------------------------------------------------
    !------bulk------      
    appc_ypencil = qxdy_ppc_ypencil
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(appc_ypencil, dm%dppc, dm%rp, 1, IPENCIL(2))
    appc_ypencil = ( appc_ypencil + qydx_ppc_ypencil) * muixy_ppc_ypencil
    !------b.c.------
    if(is_fbcy_velo_required) then
      call extract_dirichlet_fbcy(fbcy_p4c, appc_ypencil, dm%dppc, dm, is_reversed = .true.)
    else
      fbcy_p4c = MAXP
    end if
    !------PDE------
    call Get_y_1der_P2C_3D(appc_ypencil, apcc_ypencil, dm, dm%iAccuracy, mbcy_tau1, fbcy_p4c)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(apcc_ypencil, dm%dpcc, dm%rci, 1, IPENCIL(2))
    mx_rhs_ypencil =  mx_rhs_ypencil + apcc_ypencil * fl%rre

#ifdef DEBUG_STEPS
    write(*,*) 'visx-12', apcc_ypencil(1, 1:4, 1)
    call is_valid_number_3D(apcc_ypencil, 'visx-12')
#endif
!----------------------------------------------------------------------------------------------------------
! X-mom diffusion term 3/3 at (i', j, k)
! diff-z-m1 = 1/r * d[muixz * (qzdx + 1/r * qxdz)]/dz
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    apcp_zpencil = qxdz_pcp_zpencil
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(apcp_zpencil, dm%dpcp, dm%rci, 1, IPENCIL(3))
    apcp_zpencil = (apcp_zpencil + qzdx_pcp_zpencil) * muixz_pcp_zpencil
    !------b.c.------
    if(is_fbcz_velo_required) then
      call extract_dirichlet_fbcz(fbcz_pc4, apcp_zpencil, dm%dpcp)
    else
      fbcz_pc4 = MAXP
    end if
    !------PDE------
    call Get_z_1der_P2C_3D(apcp_zpencil, apcc_zpencil, dm, dm%iAccuracy, mbcz_tau1, fbcz_pc4)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(apcc_zpencil, dm%dpcc, dm%rci, 1, IPENCIL(3))
    mx_rhs_zpencil =  mx_rhs_zpencil + apcc_zpencil * fl%rre
#ifdef DEBUG_STEPS
    write(*,*) 'visx-13', apcc_zpencil(1, 1:4, 1)
    call is_valid_number_3D(apcc_zpencil, 'visx-13')
#endif
!==========================================================================================================
! the RHS of y-momentum equation
! d(gy)/dt = -        d(gxiy * qyix)/dx                             ! conv-x-m2         
!            -        d(gyiy * qyriy)/dy                            ! conv-y-m2       
!            -        d(gziy * qyriz)/dz                            ! conv-z-m2  
!            + (gziz * qziz)^y                                      ! conv-r-m2      
!            - r * dpdy                                                 ! p-m2
!            + d[muixy * (qydx + r * qxdy)]/dx                      ! diff-x-m2
!            + d[r * 2 * mu * (qyrdy - 1/3 * div)]/dy               ! diff-y-m2
!            + d[muiyz * (qzdy + 1/r * qyrdz - qzriy]/dz            ! diff-z-m2
!            - [2 mu * (1/r * qzdz - 1/3 * div + 1/r * qyriy)]^y    ! diff-r-m2
!==========================================================================================================
    i = 2
    fl%my_rhs          = ZERO
    my_rhs_ypencil     = ZERO
    my_rhs_zpencil     = ZERO
    my_rhs_pfc_xpencil = ZERO
    my_rhs_pfc_ypencil = ZERO
    mz_rhs_pfc_zpencil = ZERO
!----------------------------------------------------------------------------------------------------------
! Y-mom convection term 1/4 at (i, j', k)
! conv-x-m2 = - d(gxiy * qyix)/dx
!----------------------------------------------------------------------------------------------------------  
    !------bulk------
    if ( .not. dm%is_thermo) then
      appc_xpencil = qxiy_ppc_xpencil
    else
      appc_xpencil = gxiy_ppc_xpencil
    end if
    appc_xpencil = - appc_xpencil * qyix_ppc_xpencil
    !------b.c.------
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcx(fbcx_4pc, appc_xpencil, dm%dppc)
    else
      fbcx_4pc = MAXP
    end if
    !------PDE------
    call Get_x_1der_P2C_3D( appc_xpencil, acpc_xpencil, dm, dm%iAccuracy, mbcx_cov2, fbcx_4pc)
    fl%my_rhs = fl%my_rhs + acpc_xpencil

#ifdef DEBUG_STEPS
    write(*,*) 'cony-21', acpc_xpencil(1, 1:4, 1)
    call is_valid_number_3D(acpc_xpencil, 'cony-21')
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom convection term 2/4 at (i, j', k)
! conv-y-m2 = - d(gyiy * qyriy)/dy
!----------------------------------------------------------------------------------------------------------
    !------b.c.------
    if(is_fbcy_velo_required) then  
      if(dm%icoordinate == ICYLINDRICAL) then
        fbcy_c4c1 = dm%fbcy_qyr
      else
        fbcy_c4c1 = dm%fbcy_qy
      end if
      if ( .not. dm%is_thermo) then
        fbcy_c4c = dm%fbcy_qy
      else
        fbcy_c4c = dm%fbcy_gy
      end if
      fbcy_c4c =  - fbcy_c4c1 * fbcy_c4c
    else
      fbcy_c4c = MAXP
    end if
    !------bulk-----
    if(dm%icoordinate == ICYLINDRICAL) then
      accc_ypencil1 = qyriy_ccc_ypencil
    else
      accc_ypencil1 = qyiy_ccc_ypencil
    end if
    if ( .not. dm%is_thermo) then
      accc_ypencil = qyiy_ccc_ypencil
    else
      accc_ypencil = gyiy_ccc_ypencil
    end if
    accc_ypencil = - accc_ypencil1 * accc_ypencil
    !------PDE----- calculation
    call Get_y_1der_C2P_3D( accc_ypencil, acpc_ypencil, dm, dm%iAccuracy, mbcy_cov2, fbcy_c4c)
    my_rhs_ypencil = my_rhs_ypencil + acpc_ypencil
#ifdef DEBUG_STEPS
    !write(*,*) 'cony-pp1', accc_ypencil1(1, 1:4, 1)
    !write(*,*) 'cony-pp2', accc_ypencil(1, 1:4, 1)
    write(*,*) 'cony-22', acpc_ypencil(1, 1:4, 1)
    call is_valid_number_3D(acpc_ypencil, 'cony-22')
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom convection term 3/4 at (i, j', k)
! conv-z-m2 = - d(gziy * qyriz)/dz
!----------------------------------------------------------------------------------------------------------
    !------bulk-----
    if ( .not. dm%is_thermo) then
      acpp_zpencil = qziy_cpp_zpencil
    else
      acpp_zpencil = gziy_cpp_zpencil
    end if
    if(dm%icoordinate == ICYLINDRICAL) then
      acpp_zpencil1 = qyriz_cpp_zpencil
    else
      acpp_zpencil1 = qyiz_cpp_zpencil
    end if
    acpp_zpencil = - acpp_zpencil1 * acpp_zpencil
    !------b.c.-----
    if(is_fbcz_velo_required) then
      call extract_dirichlet_fbcz(fbcz_cp4, acpp_zpencil, dm%dcpp)
    else
      fbcz_cp4 = MAXP
    end if
    !------PDE-----
    call Get_z_1der_P2C_3D( acpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, mbcz_cov2, fbcz_cp4)
    my_rhs_zpencil = my_rhs_zpencil + acpc_zpencil
#ifdef DEBUG_STEPS
    write(*,*) 'cony-23', acpc_zpencil(1, 1:4, 1)
    call is_valid_number_3D(acpc_zpencil, 'cony-23')
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom convection term 4/4 at (i, j', k)
! conv-r-m2 = (gziz * qziz)^y
!----------------------------------------------------------------------------------------------------------
    if(dm%icoordinate == ICYLINDRICAL) then
      ! ------b.c.------
      if(is_fbcy_velo_required) then
        !call Get_y_midp_C2P_3D(qziz_ccc_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_qz, dm%fbcy_qz)
        !call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(3))
        call Get_z_midp_P2C_3D(qziy_cpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz)
        call transpose_z_to_y(acpc_zpencil, acpc_ypencil, dm%dcpc)
        call extract_dirichlet_fbcy(fbcy_c4c1, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.) !  check how to do this for interior b.c.
! #ifdef DEBUG_STEPS  
!         ! serial only
!     if(dm%icase == ICASE_PIPE) then
!         write(*,*) 'qziz_cpc_ypencil', acpc_ypencil(4, 1, 4),  acpc_ypencil(4, 1, dm%knc_sym(4)) , &
!                     acpc_ypencil(4, 1, 4)+acpc_ypencil(4, 1, dm%knc_sym(4))
!         write(*,*) 'fbcy_c4c1', fbcy_c4c1(4, 1, 4),  fbcy_c4c1(4, 1, dm%knc_sym(4)) , &
!                     fbcy_c4c1(4, 1, 4)+fbcy_c4c1(4, 1, dm%knc_sym(4))
!     end if
! #endif
        if ( .not. dm%is_thermo) then
          fbcy_c4c = fbcy_c4c1
        else
          call Get_z_midp_P2C_3D(gziy_cpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz)
          call transpose_z_to_y(acpc_zpencil, acpc_ypencil, dm%dcpc)
          !call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(3))
          call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
        end if
        fbcy_c4c = fbcy_c4c * fbcy_c4c1
      else 
        fbcy_c4c = MAXP
      end if
! #ifdef DEBUG_STEPS  
!         ! serial only
!     if(dm%icase == ICASE_PIPE) then
!         write(*,*) 'fbcy_c4c', fbcy_c4c(4, 1, 4),  fbcy_c4c(4, 1, dm%knc_sym(4)) , &
!                     fbcy_c4c(4, 1, 4)+fbcy_c4c(4, 1, dm%knc_sym(4))
!     end if
! #endif
      !------bulk-----
      if ( .not. dm%is_thermo) then
        accc_ypencil = qziz_ccc_ypencil
      else
        accc_ypencil = gziz_ccc_ypencil
      end if
      accc_ypencil = accc_ypencil * qziz_ccc_ypencil 
      !------PDE-----
      call Get_y_midp_C2P_3D(accc_ypencil, acpc_ypencil, dm, dm%iAccuracy, mbcr_cov2, fbcy_c4c)
      call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(2), is_reversed = .true.)
          
      my_rhs_ypencil = my_rhs_ypencil + acpc_ypencil
#ifdef DEBUG_STEPS
      write(*,*) 'cony-24', acpc_ypencil(1, 1:4, 1)
      call is_valid_number_3D(acpc_ypencil, 'cony-24')
#endif
    end if
#ifdef DEBUG_STEPS
    acpc_test = fl%my_rhs
    call transpose_y_to_x (my_rhs_ypencil, acpc_xpencil, dm%dcpc)
    acpc_test =  acpc_test + acpc_xpencil
    call transpose_z_to_y (my_rhs_zpencil, acpc_ypencil, dm%dcpc)
    call transpose_y_to_x (acpc_ypencil, acpc_xpencil, dm%dcpc)
    acpc_test =  acpc_test + acpc_xpencil 
    call wrt_3d_pt_debug(acpc_test,  dm%dcpc, fl%iteration, isub, 'ConY@bf st') ! debug_ww
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom pressure gradient in y direction, Y-pencil, d(sigma_1 p)
! p-m2 = - r * dpdy
!----------------------------------------------------------------------------------------------------------
    call Get_y_1der_C2P_3D( -pres_ypencil * dm%sigma1p, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_pr, -dm%fbcy_pr)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rp, 1, IPENCIL(2))
    my_rhs_pfc_ypencil =  my_rhs_pfc_ypencil + acpc_ypencil
#ifdef DEBUG_STEPS
    write(*,*) 'py-21', acpc_ypencil(1, 1:4, 1)
    call is_valid_number_3D(acpc_ypencil, 'py-21')
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom gravity in y direction ( not r direction ), Y-pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%is_thermo .and. (fl%igravity == i .or. fl%igravity == -i) )  then
      if(dm%icoordinate == ICYLINDRICAL) then
        call gravity_decomposition_to_rz(d_ccc_xpencil, fl%igravity, fl%fgravity(i), acpc_ypencil, accp_zpencil, dm)
        mz_rhs_pfc_zpencil = mz_rhs_pfc_zpencil + accp_zpencil
      else
        fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%d
        call Get_y_midp_C2P_3D(dDens_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_c4c )
        !call axis_estimating_radial_xpx(acpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(1))
        acpc_ypencil = fl%fgravity(i) * acpc_ypencil
      end if
      my_rhs_pfc_ypencil =  my_rhs_pfc_ypencil + acpc_ypencil
    end if
!----------------------------------------------------------------------------------------------------------
! Y-mom Lorentz Force in z direction, x-pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%is_mhd) then
      my_rhs_pfc_xpencil = my_rhs_pfc_xpencil + fl%lrfy
      if(dm%icoordinate == ICYLINDRICAL) then
      ! force cannot be defined in radial direction, it should be decomposed into r and z direction
! To do!!!!!!!!
      end if
    end if
!----------------------------------------------------------------------------------------------------------
! Y-mom diffusion term 1/4 at (i, j', k)
! diff-x-m2 = d[muixy * (qydx + r * qxdy)]/dx
!----------------------------------------------------------------------------------------------------------
    !------bulk-----
    appc_xpencil1 = qxdy_ppc_xpencil
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(appc_xpencil1, dm%dppc, dm%rp, 1, IPENCIL(1))
    appc_xpencil = ( appc_xpencil1 + qydx_ppc_xpencil) * muixy_ppc_xpencil
    !------b.c.-----
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcx(fbcx_4pc, appc_xpencil, dm%dppc)
    else
      fbcx_4pc = MAXP
    end if
    !------PDE-----
    call Get_x_1der_P2C_3D(appc_xpencil, acpc_xpencil, dm, dm%iAccuracy, mbcx_tau2, fbcx_4pc)
    fl%my_rhs = fl%my_rhs + acpc_xpencil * fl%rre
#ifdef DEBUG_STEPS
    write(*,*) 'visy-21', acpc_xpencil(1, 1:4, 1)
    call is_valid_number_3D(acpc_xpencil, 'visy-21')
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom diffusion term 2/4 at (i, j', k)
! diff-y-m2 = d[r * 2 * mu * (qyrdy - 1/3 * div)]/dy                 
!----------------------------------------------------------------------------------------------------------
    ! ------b.c.------
    if(is_fbcy_velo_required) then
      if(dm%icoordinate == ICYLINDRICAL) then
        call Get_y_midp_P2C_3D(qyr_ypencil,  accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qyr)
        call Get_y_1der_C2P_3D(accc_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qyr) ! acpc_ypencil = d(qyr)/dy
        call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.) ! fbcy_c4c = fbcy_dqyrdy_c4c
      else if(dm%icoordinate == ICARTESIAN) then
        call extract_dirichlet_fbcy(fbcy_c4c, qydy_cpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
      else
      end if
      fbcy_c4c = ( fbcy_c4c - ONE_THIRD * fbcy_div_c4c) * TWO * fbcy_mu_c4c
      if(dm%icoordinate == ICYLINDRICAL) then
        call multiple_cylindrical_rn_x4x(fbcy_c4c, dm%dcpc, dm%rp, 1, IPENCIL(2))
      end if
    else
      fbcy_c4c = MAXP
    end if
    ! ------bulk-----
    if(dm%icoordinate == ICYLINDRICAL) then
      accc_ypencil1 = qyrdy_ccc_ypencil
    else if(dm%icoordinate == ICARTESIAN) then
      accc_ypencil1 = qydy_ccc_ypencil
    else
    end if
    accc_ypencil = ( accc_ypencil1 - ONE_THIRD * div_ccc_ypencil) * TWO * mu_ccc_ypencil    
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rc, 1, IPENCIL(2))
    ! ------PDE-----
    call Get_y_1der_C2P_3D(accc_ypencil,  acpc_ypencil, dm, dm%iAccuracy, mbcy_tau2, fbcy_c4c) 
    my_rhs_ypencil = my_rhs_ypencil + acpc_ypencil * fl%rre
#ifdef DEBUG_STEPS
    write(*,*) 'visy-pp', fbcy_mu_c4c(1, 1:4, 1)
    write(*,*) 'visy-22', acpc_ypencil(1, 1:4, 1)
    call is_valid_number_3D(acpc_ypencil, 'visy-22')
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom diffusion term 3/4 at (i, j', k)
! diff-z-m2 = d[muiyz * (qzdy + qyr2dz - qzriy]/dz       
!----------------------------------------------------------------------------------------------------------
    !------bulk-----
    if(dm%icoordinate == ICYLINDRICAL) then
      ! acpp_zpencil = qyrdz_cpp_zpencil ! acpp_zpencil = d(qr/r)/dz
      ! call multiple_cylindrical_rn(acpp_zpencil, dm%dcpp, dm%rpi, 1, IPENCIL(3)) ! acpp_zpencil = 1/r * d(qr/r)/dz
      ! call axis_estimating_radial_xpx(acpp_zpencil1, dm%dcpp, IPENCIL(3), dm)    ! 
      ! acpp_zpencil1 = qziy_cpp_zpencil ! acpp_zpencil1 = (qz)^r
      ! call multiple_cylindrical_rn(acpp_zpencil1, dm%dcpp, dm%rpi, 1, IPENCIL(3)) ! acpp_zpencil1 = 1/r * (dz)^r
      ! call estimate_azimuthal_xpx_on_axis(acpp_zpencil1, dm%dcpp, IPENCIL(3), dm)  
      acpp_zpencil = qzdy_cpp_zpencil + qyr2dz_cpp_zpencil - qzriy_cpp_zpencil
      
    else
      acpp_zpencil = qzdy_cpp_zpencil + qydz_cpp_zpencil - ZERO
    end if
    acpp_zpencil = acpp_zpencil * muiyz_cpp_zpencil
    !------b.c.-----
    if(is_fbcz_velo_required) then
      call extract_dirichlet_fbcz(fbcz_cp4, acpp_zpencil, dm%dcpp)
    else
      fbcz_cp4 = MAXP
    end if
    !------PDE-----
    call Get_z_1der_P2C_3D(acpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, mbcz_tau2, fbcz_cp4)
    my_rhs_zpencil =  my_rhs_zpencil + acpc_zpencil * fl%rre
#ifdef DEBUG_STEPS
    write(*,*) 'visy-23', acpc_zpencil(1, 1:4, 1)
    call is_valid_number_3D(acpc_zpencil, 'visy-23')
#endif
!----------------------------------------------------------------------------------------------------------
! Y-mom diffusion term 4/4 at (i, j', k)
! diff-r-m2 = - [2 mu * (1/r * qzdz - 1/3 * div + 1/r * qyriy)]^y     
!----------------------------------------------------------------------------------------------------------
    if(dm%icoordinate == ICYLINDRICAL) then
      !------b.c.------
      if(is_fbcy_velo_required) then
        ! acpc_ypencil = qzdz_cpc_ypencil
        ! call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rpi, 1, IPENCIL(2))
        ! call estimate_azimuthal_xpx_on_axis(acpc_ypencil, dm%dcpc, IPENCIL(2), dm) 
        call extract_dirichlet_fbcy(fbcy_c4c, qzrdz_cpc_ypencil, dm%dcpc, dm, is_reversed = .false.) ! fbcy_c4c = (1/r * qzdz)_c4c
        call extract_dirichlet_fbcy(fbcy_c4c1, qyr2_ypencil, dm%dcpc, dm, is_reversed = .true.) ! fbcy_c4c1 = (1/r * qyriy)_c4c
        fbcy_c4c = (fbcy_c4c - ONE_THIRD * fbcy_div_c4c + fbcy_c4c1) * TWO * fbcy_mu_c4c
      else
        fbcy_c4c = MAXP
      end if
      !------bulk------
      accc_ypencil = qzdz_ccc_ypencil
      call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2)) ! accc_ypencil = 1/r * d(qz)/dz
      accc_ypencil1 = qyriy_ccc_ypencil
      call multiple_cylindrical_rn(accc_ypencil1, dm%dccc, dm%rci, 1, IPENCIL(2)) !accc_ypencil1 = 1/r * (qr/r)^r
      accc_ypencil = (accc_ypencil - ONE_THIRD * div_ccc_ypencil + accc_ypencil1) * TWO * mu_ccc_ypencil
      !------PDE------
      call Get_y_midp_C2P_3D(accc_ypencil, acpc_ypencil, dm, dm%iAccuracy, mbcr_tau2, fbcy_c4c)
      !call axis_estimating_radial_xpx ! not needed as b.c. for uy is specified.
      my_rhs_ypencil =  my_rhs_ypencil - acpc_ypencil * fl%rre
#ifdef DEBUG_STEPS
      write(*,*) 'visy-24', acpc_ypencil(1, 1:4, 1)
      call is_valid_number_3D(acpc_ypencil, 'visy-24')
#endif
    end if
!==========================================================================================================
! the RHS of z-momentum equation
! d(gz)/dt = -        d(gxiz * qzix)/dx                               ! conv-x-m3         
!            - 1/r^2  d(r * gyiz * qziy)/dy                           ! conv-y-m3       
!            - 1/r  * d(gziz * qziz)/dz                               ! conv-z-m3     
!            - 1/r * dpdz                                             ! p-m3
!            +         d[muixz * (qzdx + 1/r * qxdz)]/dx              ! diff-x-m3
!            + 1/r   * d[muiyz * (r * qzdy + qyrdz - qziy]/dy         ! diff-y-m3
!            + 1/r^2 * d[2 * mu * (qzdz - r/3 * div + qyriy)]/dz      ! diff-z-m3
!            + 1/r   * [muiyz * (qzdy + 1/r2 * qydz - 1/r * qziy)]^y  ! diff-r-m3
!conv-y-m3  + conv-r-m3 = conv-y-m2_new
! 1/r    d(gyiz * qziy)/dy + 1/r (gyriz * qziy)^y = 1/r^2 * d( r * qziy * gyiz)/dy
!==========================================================================================================
    i = 3
    fl%mz_rhs          = ZERO
    mz_rhs_ypencil     = ZERO
    mz_rhs_zpencil     = ZERO
    mz_rhs_pfc_xpencil = ZERO
    mz_rhs_pfc_ypencil = ZERO
    !mz_rhs_pfc_zpencil = ZERO ! could get value from gravity_decomposition_to_rz
!----------------------------------------------------------------------------------------------------------
! Z-mom convection term 1/3 at (i, j, k')
! conv-x-m3 = - d(gxiz * qzix)/dx                               
!----------------------------------------------------------------------------------------------------------  
    !------bulk------
    if ( .not. dm%is_thermo) then
      apcp_xpencil = qxiz_pcp_xpencil
    else
      apcp_xpencil = gxiz_pcp_xpencil
    end if
    apcp_xpencil = - apcp_xpencil * qzix_pcp_xpencil
    !------b.c.-----
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcx(fbcx_4cp, apcp_xpencil, dm%dpcp)
    else
      fbcx_4cp = MAXP
    end if
    !------PDE------
    call Get_x_1der_P2C_3D(apcp_xpencil, accp_xpencil, dm, dm%iAccuracy, mbcx_cov3, fbcx_4cp)
    fl%mz_rhs = fl%mz_rhs + accp_xpencil
#ifdef DEBUG_STEPS
    write(*,*) 'conz-31', accp_xpencil(1, 1:4, 1)
    call is_valid_number_3D(accp_xpencil, 'conz-31')
#endif
!----------------------------------------------------------------------------------------------------------
! Z-mom convection term 2/3 at (i, j, k')
! conv-y-m3 = - 1/r^2 * d( r * qziy * gyiz)/dy                            
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    acpp_ypencil1 = qziy_cpp_ypencil
    if ( .not. dm%is_thermo) then
      acpp_ypencil = qyiz_cpp_ypencil
    else
      acpp_ypencil = gyiz_cpp_ypencil
    end if
    acpp_ypencil = - acpp_ypencil * acpp_ypencil1
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(acpp_ypencil, dm%dcpp, dm%rp, 1, IPENCIL(2)) 
    !------b.c.-----
    if(is_fbcy_velo_required) then
      call extract_dirichlet_fbcy(fbcy_c4p, acpp_ypencil, dm%dcpp, dm, is_reversed = .true.)
    else
      fbcy_c4p = MAXP
    end if
    !------PDE------
    call Get_y_1der_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, mbcy_cov3, fbcy_c4p)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_ypencil, dm%dccp, dm%rci, 2, IPENCIL(2)) 
    mz_rhs_ypencil = mz_rhs_ypencil + accp_ypencil
#ifdef DEBUG_STEPS
    write(*,*) 'conz-32', accp_ypencil(1, 1:4, 1)
    call is_valid_number_3D(accp_ypencil, 'conz-32')
#endif
!----------------------------------------------------------------------------------------------------------
! Z-mom convection term 3/3 at (i, j, k')
! conv-z-m3 = 1/r  * d(gziz * qziz)/dz                            
!----------------------------------------------------------------------------------------------------------
    !------b.c.------
    if(is_fbcz_velo_required) then
      if ( .not. dm%is_thermo) then
        fbcz_cc4 = dm%fbcz_qz
      else
        fbcz_cc4 = dm%fbcz_gz
      end if
      fbcz_cc4 = -fbcz_cc4 * dm%fbcz_qz
    else
      fbcz_cc4 = MAXP
    end if
    !------bulk------
    if ( .not. dm%is_thermo) then
      accc_zpencil = qziz_ccc_zpencil
    else
      accc_zpencil = gziz_ccc_zpencil
    end if
    accc_zpencil = -accc_zpencil * qziz_ccc_zpencil
    !------PDE------
    call Get_z_1der_C2P_3D( accc_zpencil, accp_zpencil, dm, dm%iAccuracy, mbcz_cov3, fbcz_cc4 )
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    mz_rhs_zpencil = mz_rhs_zpencil + accp_zpencil
#ifdef DEBUG_STEPS
    write(*,*) 'conz-33', accp_zpencil(1, 1:4, 1)
    call is_valid_number_3D(accp_zpencil, 'conz-33')
#endif
! !----------------------------------------------------------------------------------------------------------
! ! Z-mom convection term 4/4 at (i, j, k')
! ! conv-r-m3 = - (gyriz * qzriy)^y                                      
! !----------------------------------------------------------------------------------------------------------
!     if(dm%icoordinate == ICYLINDRICAL) then
!       !------bulk------
!       if ( .not. dm%is_thermo) then
!         acpp_ypencil = qyriz_cpp_ypencil
!       else
!         acpp_ypencil = gyriz_cpp_ypencil
!       end if
!       acpp_ypencil = - acpp_ypencil * qzriy_cpp_ypencil
!       !------b.c.-----
!       if(is_fbcy_velo_required) then
!         call extract_dirichlet_fbcy(fbcy_c4p, acpp_ypencil, dm%dcpp, dm)
!       else
!         fbcy_c4p = MAXP
!       end if
!       !------PDE------
!       call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, mbcr_cov3, fbcy_c4p)
!       mz_rhs_ypencil = mz_rhs_ypencil + accp_ypencil

! #ifdef DEBUG_STEPS
!       write(*,*) 'conz-34', accp_ypencil(1, 1:4, 1)
!       call is_valid_number_3D(accp_ypencil, 'conz-34')
! #endif
!     end if
#ifdef DEBUG_STEPS
    accp_test = fl%mz_rhs
    call transpose_y_to_x (mz_rhs_ypencil, accp_xpencil, dm%dccp)
    accp_test =  accp_test + accp_xpencil
    call transpose_z_to_y (mz_rhs_zpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_x (accp_ypencil, accp_xpencil, dm%dccp)
    accp_test =  accp_test + accp_xpencil 
    call wrt_3d_pt_debug(accp_test,  dm%dccp, fl%iteration, isub, 'ConZ@bf st') ! debug_ww
#endif
!----------------------------------------------------------------------------------------------------------
! Z-mom pressure gradient in z direction, Z-pencil, d(sigma_1 p)
! p-m3 = -1/r * dpdz
!----------------------------------------------------------------------------------------------------------
    call Get_z_1der_C2P_3D( -pres_zpencil * dm%sigma1p, accp_zpencil, dm, dm%iAccuracy, dm%ibcz_pr, -dm%fbcz_pr)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    mz_rhs_pfc_zpencil =  mz_rhs_pfc_zpencil + accp_zpencil
#ifdef DEBUG_STEPS
      write(*,*) 'pz-31', mz_rhs_pfc_zpencil(1, 1:4, 1)
      call is_valid_number_3D(mz_rhs_pfc_zpencil, 'pz-31')
#endif
!----------------------------------------------------------------------------------------------------------
! Z-mom gravity in z direction, Z-pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%is_thermo .and. (fl%igravity == i .or. fl%igravity == -i) )  then
      if(dm%icoordinate == ICYLINDRICAL) then
        call gravity_decomposition_to_rz(d_ccc_xpencil, fl%igravity, fl%fgravity(i), acpc_ypencil, accp_zpencil, dm)
        my_rhs_pfc_ypencil = my_rhs_pfc_ypencil + accp_zpencil
      else
        fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%d
        call Get_z_midp_C2P_3D(dDens_zpencil, accp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp, fbcz_cc4 )
        accp_zpencil = fl%fgravity(i) * accp_zpencil
      end if
      mz_rhs_pfc_zpencil =  mz_rhs_pfc_zpencil + accp_zpencil
#ifdef DEBUG_STEPS
      write(*,*) 'pz-32', mz_rhs_pfc_zpencil(1, 1:4, 1)
      call is_valid_number_3D(mz_rhs_pfc_zpencil, 'pz-32')
#endif
    end if
!----------------------------------------------------------------------------------------------------------
! Z-mom Lorentz Force in z direction, x-pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%is_mhd) then
      mz_rhs_pfc_xpencil = mz_rhs_pfc_xpencil + fl%lrfz
#ifdef DEBUG_STEPS
      write(*,*) 'pz-33', mz_rhs_pfc_xpencil(1, 1:4, 1)
      call is_valid_number_3D(mz_rhs_pfc_xpencil, 'pz-33')
#endif
    end if
!----------------------------------------------------------------------------------------------------------
! Z-mom diffusion term 1/4  at (i, j, k')
! diff-x-m3 = d[muixz * (qzdx + 1/r * qxdz)]/dx                            
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    apcp_xpencil = qxdz_pcp_xpencil
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(apcp_xpencil, dm%dpcp, dm%rci, 1, IPENCIL(1))
    apcp_xpencil = ( qzdx_pcp_xpencil + apcp_xpencil) * muixz_pcp_xpencil
    !------b.c.-----
    if(is_fbcx_velo_required) then
      call extract_dirichlet_fbcx(fbcx_4cp, apcp_xpencil, dm%dpcp)
    else
      fbcx_4cp = MAXP
    end if
    !------PDE------
    call Get_x_1der_P2C_3D(apcp_xpencil, accp_xpencil, dm, dm%iAccuracy, mbcx_tau3, fbcx_4cp)
    fl%mz_rhs = fl%mz_rhs + accp_xpencil * fl%rre
#ifdef DEBUG_STEPS
      write(*,*) 'visz-31', accp_xpencil(1, 1:4, 1)
      call is_valid_number_3D(accp_xpencil, 'visz-31')
#endif
!----------------------------------------------------------------------------------------------------------
! Z-mom diffusion term 2/4 at (i, j, k')
! diff-y-m3 = 1/r * d[muiyz * (r * qzdy + qyrdz - qziy]/dy               
!----------------------------------------------------------------------------------------------------------
    !------bulk------
    acpp_ypencil = qzdy_cpp_ypencil
    if(dm%icoordinate == ICYLINDRICAL) then
      call multiple_cylindrical_rn(acpp_ypencil, dm%dcpp, dm%rp, 1, IPENCIL(2))
      acpp_ypencil = acpp_ypencil + qyrdz_cpp_ypencil - qziy_cpp_ypencil
    else
      acpp_ypencil = acpp_ypencil + qydz_cpp_ypencil
    end if
    acpp_ypencil = acpp_ypencil * muiyz_cpp_ypencil
    !------b.c.-----
    if(is_fbcy_velo_required) then
      call extract_dirichlet_fbcy(fbcy_c4p, acpp_ypencil, dm%dcpp, dm, is_reversed = .true.)
    else
      fbcy_c4p = MAXP
    end if
#ifdef DEBUG_STEPS
    if(dm%icoordinate == ICYLINDRICAL) then
      write(*,*) 'visz-32-interior', qzdy_cpp_ypencil(1, 1:2, 1), &
        qyrdz_cpp_ypencil(1, 1:2, 1), qziy_cpp_ypencil(1, 1:2, 1)
      write(*,*) 'visz-32-fbc', fbcy_c4p(1, 1, 1)
    end if
#endif
    !------PDE------
    call Get_y_1der_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, mbcy_tau3, fbcy_c4p)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_ypencil, dm%dccp, dm%rci, 1, IPENCIL(2))
    mz_rhs_ypencil =  mz_rhs_ypencil + accp_ypencil * fl%rre
#ifdef DEBUG_STEPS
    write(*,*) 'visz-32', accp_ypencil(1, 1:4, 1)
    call is_valid_number_3D(accp_ypencil, 'visz-32')
#endif
!----------------------------------------------------------------------------------------------------------
! Z-mom diffusion term 3/4 at (i, j, k')
! diff-z-m3 = 1/r2 * d[2 * mu * (qzdz - r/3 * div + qyriy)]/dz 
!----------------------------------------------------------------------------------------------------------
    !------b.c.------
    if(is_fbcz_velo_required) then
      call extract_dirichlet_fbcz(fbcz_cc4, qzdz_ccp_zpencil, dm%dccp)
      if(dm%icoordinate == ICYLINDRICAL) then
        !call multiple_cylindrical_rn_xx4(fbcz_cc4, dm%dccp, dm%rci, 2, IPENCIL(3))
        fbcz_cc41 = dm%fbcz_qyr
        fbcz_cc42 = fbcz_div_cc4
        call multiple_cylindrical_rn_xx4(fbcz_cc42, dm%dccp, dm%rc, 1, IPENCIL(3))
      else
        fbcz_cc42 = fbcz_div_cc4
        fbcz_cc41 = ZERO
      end if
      fbcz_cc4 = ( fbcz_cc4 - ONE_THIRD * fbcz_cc42 + fbcz_cc41) * TWO * fbcz_mu_cc4
    else
      fbcz_cc4 = MAXP
    end if
    !------bulk------
    if(dm%icoordinate == ICYLINDRICAL) then
      accc_zpencil = div_ccc_zpencil
      call multiple_cylindrical_rn(accc_zpencil, dm%dccc, dm%rc, 1, IPENCIL(3))
      accc_zpencil1 = qyriy_ccc_zpencil
    else
      accc_zpencil  = div_ccc_zpencil
      accc_zpencil1 = ZERO
    end if
    accc_zpencil = ( qzdz_ccc_zpencil - ONE_THIRD * accc_zpencil + accc_zpencil1) * TWO * mu_ccc_zpencil
    !------PDE------
    call Get_z_1der_C2P_3D(accc_zpencil, accp_zpencil, dm, dm%iAccuracy, mbcz_tau3, fbcz_cc4) 
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 2, IPENCIL(3))
    mz_rhs_zpencil = mz_rhs_zpencil + accp_zpencil * fl%rre
#ifdef DEBUG_STEPS
    write(*,*) 'visz-33', accp_zpencil(1, 1:4, 1)
    call is_valid_number_3D(accp_zpencil, 'visz-33')
#endif
!----------------------------------------------------------------------------------------------------------
! Z-mom diffusion term 4/4 at (i, j, k')
! diff-r-m3 = 1/r * [muiyz * (qzdy + 1/r2 * qydz - qzriy)]^y  (method 1)
!           = 1/r * [muiyz * (qzdy + 1/r2 * qydz)]^y - 1/r^2 * muiz * qz (method 2)
!---------------------------------------------------------------------------------------------------------
    if(dm%icoordinate == ICYLINDRICAL) then
    ! method 1
      !------bulk1------
      acpp_ypencil = (qzdy_cpp_ypencil + qyr2dz_cpp_ypencil - qzriy_cpp_ypencil) * muiyz_cpp_ypencil
      !------b.c.-----
      if(is_fbcy_velo_required) then
        call extract_dirichlet_fbcy(fbcy_c4p, acpp_ypencil, dm%dcpp, dm, is_reversed = .true.)
      else
        fbcy_c4p = MAXP
      end if
      !------PDE------
      call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, mbcr_tau3, fbcy_c4p)
      if(dm%icoordinate == ICYLINDRICAL) &
      call multiple_cylindrical_rn(accp_ypencil, dm%dccp, dm%rci, 1, IPENCIL(2))
      mz_rhs_ypencil =  mz_rhs_ypencil + accp_ypencil * fl%rre

! method 2
      ! !------bulk1------
      ! acpp_ypencil = (qzdy_cpp_ypencil + qyr2dz_cpp_ypencil) * muiyz_cpp_ypencil
      ! !------b.c.1-----
      ! if(is_fbcy_velo_required) then
      !   call extract_dirichlet_fbcy(fbcy_c4p, acpp_ypencil, dm%dcpp, dm, is_reversed = .true.)
      ! else
      !   fbcy_c4p = MAXP
      ! end if
      ! !------PDE1------
      ! call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, mbcr_tau3, fbcy_c4p)
      ! call multiple_cylindrical_rn(accp_ypencil, dm%dccp, dm%rci, 1, IPENCIL(2))
      ! mz_rhs_ypencil =  mz_rhs_ypencil + accp_ypencil * fl%rre

      ! !------bulk2------
      ! accp_zpencil = muiz_ccp_zpencil * qz_zpencil
      ! call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 2, IPENCIL(3))
      ! mz_rhs_zpencil =  mz_rhs_zpencil + accp_zpencil * fl%rre

#ifdef DEBUG_STEPS
      write(*,*) 'visz-34', accp_ypencil(1, 1:4, 1)
      call is_valid_number_3D(accp_ypencil, 'visz-34')
#endif
    end if
!----------------------------------------------------------------------------------------------------------
! x-mom: convert all terms to rhs
!----------------------------------------------------------------------------------------------------------
    call transpose_y_to_x (mx_rhs_ypencil, apcc_xpencil, dm%dpcc)
    fl%mx_rhs =  fl%mx_rhs + apcc_xpencil
    call transpose_z_to_y (mx_rhs_zpencil, apcc_ypencil, dm%dpcc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    fl%mx_rhs =  fl%mx_rhs + apcc_xpencil
    !mx_rhs_pfc_xpencil, no need to transpose
!----------------------------------------------------------------------------------------------------------
! Y-mom: convert all terms to rhs
!----------------------------------------------------------------------------------------------------------
    call transpose_y_to_x (my_rhs_ypencil, acpc_xpencil, dm%dcpc)
    fl%my_rhs =  fl%my_rhs + acpc_xpencil

    call transpose_z_to_y (my_rhs_zpencil, acpc_ypencil, dm%dcpc)
    call transpose_y_to_x (acpc_ypencil, acpc_xpencil, dm%dcpc)
    fl%my_rhs =  fl%my_rhs + acpc_xpencil

    call transpose_y_to_x (my_rhs_pfc_ypencil,  acpc_xpencil,  dm%dcpc)
    my_rhs_pfc_xpencil = my_rhs_pfc_xpencil + acpc_xpencil
!----------------------------------------------------------------------------------------------------------
! Z-mom: convert all terms to rhs
!----------------------------------------------------------------------------------------------------------
    call transpose_y_to_x (mz_rhs_ypencil, accp_xpencil, dm%dccp)
    fl%mz_rhs =  fl%mz_rhs + accp_xpencil

    call transpose_z_to_y (mz_rhs_zpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_x (accp_ypencil, accp_xpencil, dm%dccp)
    fl%mz_rhs =  fl%mz_rhs + accp_xpencil

    call transpose_z_to_y (mz_rhs_pfc_zpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_x (accp_ypencil, accp_xpencil, dm%dccp)
    mz_rhs_pfc_xpencil = mz_rhs_pfc_xpencil + accp_xpencil

!==========================================================================================================
! x-pencil : to build up rhs in total, in all directions
!==========================================================================================================
!----------------------------------------------------------------------------------------------------------
! x-pencil : x-momentum
!----------------------------------------------------------------------------------------------------------
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%mx_rhs, dm%dpcc, fl%iteration, isub, 'ConVisX@bf stepping') ! debug_ww

#endif
    call Calculate_momentum_fractional_step(fl%mx_rhs0, fl%mx_rhs, mx_rhs_pfc_xpencil, dm%dpcc, dm, isub)  
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%mx_rhs, dm%dpcc, fl%iteration, isub, 'ConVisPX@af stepping') ! debug_ww
#endif
!----------------------------------------------------------------------------------------------------------
! x-pencil : y-momentum
!----------------------------------------------------------------------------------------------------------
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%my_rhs, dm%dcpc, fl%iteration, isub, 'ConVisY@bf stepping') ! debug_ww
#endif
    call Calculate_momentum_fractional_step(fl%my_rhs0, fl%my_rhs, my_rhs_pfc_xpencil, dm%dcpc, dm, isub)
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%my_rhs, dm%dcpc, fl%iteration, isub, 'ConVisPY@af stepping') ! debug_ww
#endif
!----------------------------------------------------------------------------------------------------------
! x-pencil : z-momentum
!----------------------------------------------------------------------------------------------------------
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%mz_rhs, dm%dccp, fl%iteration, isub, 'ConVisZ@bf stepping') ! debug_ww
#endif
    call Calculate_momentum_fractional_step(fl%mz_rhs0, fl%mz_rhs, mz_rhs_pfc_xpencil, dm%dccp, dm, isub)
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%mz_rhs, dm%dccp, fl%iteration, isub, 'ConVisZ@af stepping') ! debug_ww
#endif
!==========================================================================================================
! x-pencil : flow drive terms (source terms) in periodic Streamwise flow
!==========================================================================================================
    if (fl%idriven == IDRVF_X_MASSFLUX) then
      call Get_volumetric_average_3d(dm, dm%dpcc, fl%mx_rhs, rhsx_bulk, SPACE_AVERAGE, "mx_rhs")
      fl%mx_rhs = fl%mx_rhs - rhsx_bulk
    else if (fl%idriven == IDRVF_X_DPDX) then
      rhsx_bulk = - HALF * fl%drvfc * dm%tAlpha(isub) * dm%dt
      fl%mx_rhs = fl%mx_rhs - rhsx_bulk
    else if (fl%idriven == IDRVF_X_TAUW) then
      ! to add on wall-cells only
      if(nrank == 0) call Print_error_msg('Function to be added soon.')
    else if (fl%idriven == IDRVF_Z_MASSFLUX) then
      call Get_volumetric_average_3d(dm, dm%dccp, fl%mz_rhs, rhsz_bulk, SPACE_AVERAGE, "mz_rhs")
      fl%mz_rhs = fl%mz_rhs - rhsz_bulk
    else if (fl%idriven == IDRVF_Z_DPDZ) then
      rhsz_bulk = - HALF * fl%drvfc * dm%tAlpha(isub) * dm%dt
      fl%mz_rhs = fl%mz_rhs - rhsz_bulk
    else if (fl%idriven == IDRVF_Z_TAUW) then
      ! to add on wall-cells only
      if(nrank == 0) call Print_error_msg('Function to be added soon.')
    else
    end if
    
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%mx_rhs, dm%dpcc, fl%iteration, isub, 'RHSX@total') ! debug_ww
    call wrt_3d_pt_debug(fl%my_rhs, dm%dpcc, fl%iteration, isub, 'RHSY@total') ! debug_ww
    call wrt_3d_pt_debug(fl%mz_rhs, dm%dccp, fl%iteration, isub, 'RHSZ@total') ! debug_ww
#endif
 
    return
  end subroutine Compute_momentum_rhs
!==========================================================================================================
!==========================================================================================================

!==========================================================================================================
!==========================================================================================================
  subroutine Correct_massflux(fl, phi_ccc, dm, isub)
    use udf_type_mod
    use input_general_mod
    use operations
    use parameters_constant_mod
    use cylindrical_rn_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: isub
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ), intent(in ) :: phi_ccc

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: ux
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: uy
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: uz

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: dphidx_pcc
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: dphidy_cpc
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: dphidz_ccp

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: phi_ccc_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: dphidy_cpc_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: dphidz_ccp_ypencil
    
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: pphi_ccc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: dphidz_ccp_zpencil
    
#ifdef DEBUG_STEPS  
    if(nrank == 0) &
    call Print_debug_inline_msg("Updating the velocity/mass flux ...")
#endif

    if(dm%is_thermo) then
      ux = fl%gx
      uy = fl%gy
      uz = fl%gz
    else 
      ux = fl%qx
      uy = fl%qy
      uz = fl%qz
    end if
!----------------------------------------------------------------------------------------------------------
!   x-pencil, qx = qx - dt * alpha * d(phi_ccc)/dx
!----------------------------------------------------------------------------------------------------------
    dphidx_pcc = ZERO
    call Get_x_1der_C2P_3D(phi_ccc,  dphidx_pcc, dm, dm%iAccuracy, dm%ibcx_pr, dm%fbcx_pr )
    ux = ux - dm%dt * dm%tAlpha(isub) * dm%sigma2p * dphidx_pcc
!----------------------------------------------------------------------------------------------------------
!   y-pencil, qy = qy - dt * alpha * r * d(phi_ccc)/dy
!----------------------------------------------------------------------------------------------------------
    phi_ccc_ypencil = ZERO
    dphidy_cpc_ypencil = ZERO
    dphidy_cpc = ZERO
    call transpose_x_to_y (phi_ccc, phi_ccc_ypencil, dm%dccc)
    call Get_y_1der_C2P_3D(phi_ccc_ypencil, dphidy_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_pr, dm%fbcy_pr)
    !call axis_estimating_radial_xpx(dphidy_cpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(1))
    call transpose_y_to_x (dphidy_cpc_ypencil, dphidy_cpc, dm%dcpc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(dphidy_cpc, dm%dcpc, dm%rp, 1, IPENCIL(1))
    uy = uy - dm%dt * dm%tAlpha(isub) * dm%sigma2p * dphidy_cpc
!----------------------------------------------------------------------------------------------------------
!   z-pencil, qz = qz - dt * alpha * 1/r * d(phi_ccc)/dz (if qz = uz)
!----------------------------------------------------------------------------------------------------------
    pphi_ccc_zpencil =  ZERO
    dphidz_ccp_zpencil = ZERO
    dphidz_ccp_ypencil = ZERO
    dphidz_ccp = ZERO
    call transpose_y_to_z (phi_ccc_ypencil, pphi_ccc_zpencil, dm%dccc)
    call Get_z_1der_C2P_3D(pphi_ccc_zpencil, dphidz_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_pr, dm%fbcz_pr )
    call transpose_z_to_y (dphidz_ccp_zpencil, dphidz_ccp_ypencil, dm%dccp)
    call transpose_y_to_x (dphidz_ccp_ypencil, dphidz_ccp,         dm%dccp)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(dphidz_ccp, dm%dccp, dm%rci, 1, IPENCIL(1))
    uz = uz - dm%dt * dm%tAlpha(isub) * dm%sigma2p * dphidz_ccp

    if(dm%is_thermo) then
      fl%gx = ux
      fl%gy = uy
      fl%gz = uz
    else 
      fl%qx = ux
      fl%qy = uy
      fl%qz = uz
    end if

    return
  end subroutine Correct_massflux

!==========================================================================================================
  subroutine solve_pressure_poisson(fl, dm, isub)
    use udf_type_mod
    use parameters_constant_mod
    use poisson_interface_mod
    use continuity_eq_mod
    use cylindrical_rn_mod
    use find_max_min_ave_mod
    use typeconvert_mod
    use io_visualisation_mod
    use solver_tools_mod
    implicit none
    !------------------------------------------------------------------
    ! Arguments
    !------------------------------------------------------------------
    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl
    integer,        intent(in)    :: isub
    !------------------------------------------------------------------
    ! Local variables
    !------------------------------------------------------------------
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: &
        drhodt, div
    real(WP) :: coeff, pres_bulk, mass_imbalance(8)
    !
#ifdef DEBUG_STEPS
    if (nrank == 0) then
      call Print_debug_inline_msg("Calculating the RHS of Poisson equation ...")
    end if
#endif
    !------------------------------------------------------------------
    ! 1. Build RHS components of Poisson equation
    !------------------------------------------------------------------
    ! RHS = d(rho)/dt + div(rho u)
    ! For cylindrical coordinate:
    ! Poisson eq is: r^2 * d2/dx2 + r * d(r * d/dy)/dy + d2/dz2  = &
    !                r^2 * div + r^2 * d(rho)/dt
    !------------------------------------------------------------------
    ! time scaling coefficient
    coeff = ONE / (dm%tAlpha(isub) * dm%sigma2p * dm%dt)
    ! RHS-part1
    if (dm%is_thermo) then
      drhodt = fl%drhodt
    else
      drhodt = ZERO
    end if
    ! RHS-part2
    div = ZERO
    call Get_divergence_flow(fl, div, dm)
    ! final RHS
    fl%pcor = drhodt + div
    !
    
    !write(*,*) 'mass_imbalance before fft1', mass_imbalance
    if(is_global_mass_correction) then
      call check_global_mass_balance(mass_imbalance, fl%drhodt, dm)
      fl%pcor = fl%pcor - mass_imbalance(8)/dm%vol
      !write(*,*) 'mass_imbalance before fft', mass_imbalance
    end if
    !call check_global_mass_balance(mass_imbalance, fl%drhodt- mass_imbalance(8)/dm%vol, dm)
    !write(*,*) 'mass_imbalance before fftX', mass_imbalance
    !
    if (dm%icoordinate == ICYLINDRICAL) then
      call multiple_cylindrical_rn(fl%pcor, dm%dccc, dm%rc, 2, IPENCIL(1))
    end if
    fl%pcor = fl%pcor * coeff
    !------------------------------------------------------------------
    ! 2. Solve Poisson equation
    !------------------------------------------------------------------
    call solve_fft_poisson(fl%pcor, dm)
    ! remove drift
    call Get_volumetric_average_3d(dm, dm%dccc, fl%pcor, pres_bulk, SPACE_AVERAGE, "phi")
    !if(nrank==0) write(*,*) 'shifted phi:', pres_bulk
    fl%pcor = fl%pcor - pres_bulk
    !------------------------------------------------------------------
    ! 3. Pressure correction
    !------------------------------------------------------------------
    fl%pres = fl%pres + fl%pcor
    ! Remove pressure drift (zero-mean constraint)
    call Get_volumetric_average_3d(dm, dm%dccc, fl%pres, pres_bulk, SPACE_AVERAGE, "pressure")
    !if(nrank==0) write(*,*) 'shifted pres:', pres_bulk
    fl%pres = fl%pres - pres_bulk
    !
#ifdef DEBUG_STEPS
    ! call wrt_3d_pt_debug(fl%pres, dm%dccc, fl%iteration, isub, 'pr_updated')
    write(*,*) 'solved_phi:', fl%pcor(2, 1:4, 2)
#endif
    return
  end subroutine solve_pressure_poisson
! !==========================================================================================================
!   subroutine solve_poisson_x2z(fl, dm, isub)
!     use udf_type_mod
!     use parameters_constant_mod
!     use decomp_2d_poisson
!     use transpose_extended_mod
!     use continuity_eq_mod

!     implicit none
!     type(t_domain), intent( in    ) :: dm
!     type(t_flow),   intent( inout ) :: fl                  
!     integer,        intent( in    ) :: isub

!     real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: rhs
!     real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: rhs_ypencil
!     real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: rhs_zpencil

  

!     real(WP), dimension( dm%dccc%zst(1) : dm%dccc%zen(1), &
!                          dm%dccc%zst(2) : dm%dccc%zen(2), &
!                          dm%dccc%zst(3) : dm%dccc%zen(3) ) :: rhs_zpencil_ggg
!     !integer :: i, j, k, jj, ii

! !==========================================================================================================
! ! RHS of Poisson Eq.
! !==========================================================================================================
!     fl%pcor_zpencil_ggg = ZERO
! !----------------------------------------------------------------------------------------------------------
! ! $d\rho / dt$ at cell centre
! !----------------------------------------------------------------------------------------------------------
!     if (dm%is_thermo) then
!       rhs = ZERO
!       rhs_ypencil = ZERO
!       rhs_zpencil = ZERO
!       rhs_zpencil_ggg = ZERO
!       call Calculate_drhodt(dm, tm%dDens, tm%dDens0, tm%dDensm2, rhs)
!       call transpose_x_to_y(rhs,         rhs_ypencil)
!       call transpose_y_to_z(rhs_ypencil, rhs_zpencil)
!       call zpencil_index_llg2ggg(rhs_zpencil, rhs_zpencil_ggg, dm%dccc)
!       fl%pcor_zpencil_ggg = fl%pcor_zpencil_ggg + rhs_zpencil_ggg
!     end if
! !----------------------------------------------------------------------------------------------------------
! ! $d(\rho u_i)) / dx_i $ at cell centre
! !----------------------------------------------------------------------------------------------------------
!     rhs_zpencil_ggg  = ZERO
!     if (dm%is_thermo) then
!       call Get_divergence_x2z(fl%gx, fl%gy, fl%gz, rhs_zpencil_ggg, dm)
!     else
!       call Get_divergence_x2z(fl%qx, fl%qy, fl%qz, rhs_zpencil_ggg, dm)
!     end if
!     fl%pcor_zpencil_ggg = fl%pcor_zpencil_ggg + rhs_zpencil_ggg
!     fl%pcor_zpencil_ggg = fl%pcor_zpencil_ggg / (dm%tAlpha(isub) * dm%sigma2p * dm%dt)
! !==========================================================================================================
! !   solve Poisson
! !==========================================================================================================
!     call poisson(fl%pcor_zpencil_ggg)
! !==========================================================================================================
! !   convert back RHS from zpencil ggg to xpencil gll
! !==========================================================================================================
!     call zpencil_index_ggg2llg(fl%pcor_zpencil_ggg, rhs_zpencil, dm%dccc)
!     call transpose_z_to_y (rhs_zpencil, rhs_ypencil, dm%dccc)
!     call transpose_y_to_x (rhs_ypencil, fl%pcor,     dm%dccc)

!     return
!   end subroutine
!==========================================================================================================
!> \brief To update the provisional u or rho u.
!>
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                         !
!______________________________________________________________________________!
!> \param[inout]  fl            flow field
!> \param[inout]  dm            domain
!> \param[in]     isub         RK sub-iteration
!==========================================================================================================
  subroutine Solve_momentum_eq(fl, dm, isub)
    use bc_convective_outlet_mod
    use boundary_conditions_mod
    use continuity_eq_mod
    use convert_primary_conservative_mod
    use eq_energy_mod
    use find_max_min_ave_mod
    use io_restart_mod
    use mpi_mod
    use parameters_constant_mod
    use solver_tools_mod
    use typeconvert_mod
    use udf_type_mod
#ifdef DEBUG_STEPS
    use io_tools_mod
    use typeconvert_mod
    use wtformat_mod
#endif
    implicit none
    ! arguments
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    logical :: flg_bc_conv
    integer :: i
    real(WP) :: pres_bulk, mass_imbalance(8)
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: dens, visc
    !
    ! set up thermo info based on different time stepping
    ! drho/dt is updated here as it is used in balancing mass in convective outlet
    if(dm%is_thermo) then
      dens = fl%dDens
      visc = fl%mVisc
      call Calculate_drhodt(fl, dm, isub)
    end if
    ! to set up convective outlet b.c.
    call update_convective_outlet_flow(fl, dm, isub)
    ! Main Momentum RHS
    if ( .not. dm%is_thermo) then 
      ! to calculate the rhs of the momenturn equation in stepping method
      call Compute_momentum_rhs(fl, dm, isub)
      ! to update intermediate (\hat{q}) or (\hat{g})
      fl%qx = fl%qx + fl%mx_rhs
      fl%qy = fl%qy + fl%my_rhs
      fl%qz = fl%qz + fl%mz_rhs
      call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
    else if ( dm%is_thermo) then 
      ! to calculate the rhs of the momenturn equation in stepping method
      call Compute_momentum_rhs(fl, dm, isub, dens, visc)
      ! to update intermediate (\hat{q}) or (\hat{g})
      fl%gx = fl%gx + fl%mx_rhs
      fl%gy = fl%gy + fl%my_rhs
      fl%gz = fl%gz + fl%mz_rhs
      call enforce_velo_from_fbc(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcy_gy, dm%fbcz_gz)
    else
      call Print_error_msg("Error in velocity updating")
    end if
    if(dm%icase == ICASE_PIPE) call update_fbcy_cc_flow_halo(fl, dm)
    !----------------------------------------------------------------
    ! Poisson eq and velocity correction
    !----------------------------------------------------------------
    if(is_RK_proj(isub)) then
      ! to solve Poisson equation
      !if(nrank == 0) call Print_debug_inline_msg("  Solving Poisson Equation ...") 
      !call solve_poisson_x2z(fl, dm, isub) !   
      call solve_pressure_poisson(fl, dm, isub) ! test show above two methods gave the same results. 
      !
      ! to update velocity/massflux correction
      !if(nrank == 0) call Print_debug_inline_msg("  Updating velocity/mass flux ...")
      call Correct_massflux(fl, fl%pcor, dm, isub)
      if ( .not. dm%is_thermo) then
        call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
      else
        call enforce_velo_from_fbc(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcy_gy, dm%fbcz_gz)
      end if
      if(dm%icase == ICASE_PIPE) call update_fbcy_cc_flow_halo(fl, dm)
#ifdef DEBUG_STEPS
      call check_global_mass_balance(mass_imbalance, fl%drhodt, dm)
      write(*,*) 'mass_imbalance after correction', mass_imbalance
#endif
#ifdef DEBUG_STEPS
      if(dm%is_thermo) then
      call wrt_3d_pt_debug(fl%gx, dm%dpcc,   fl%iteration, isub, 'gx_updated') ! debug_ww
      call wrt_3d_pt_debug(fl%gy, dm%dcpc,   fl%iteration, isub, 'gy_updated') ! debug_ww
      call wrt_3d_pt_debug(fl%gz, dm%dccp,   fl%iteration, isub, 'gz_updated') ! debug_ww
      end if
      call Check_element_mass_conservation(fl, dm, opt_isub=isub) 
#endif
    end if
    !----------------------------------------------------------------
    ! to update velocity from gx gy gz 
    !----------------------------------------------------------------
    if(dm%is_thermo) then
      call convert_primary_conservative(dm, fl%dDens, IG2Q, IALL, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)
    end if

#ifdef DEBUG_STEPS
  call wrt_3d_pt_debug(fl%qx, dm%dpcc,   fl%iteration, isub, 'qx_updated') ! debug_ww
  call wrt_3d_pt_debug(fl%qy, dm%dcpc,   fl%iteration, isub, 'qy_updated') ! debug_ww
  call wrt_3d_pt_debug(fl%qz, dm%dccp,   fl%iteration, isub, 'qz_updated') ! debug_ww
  if(nrank == 0) then
    call Print_debug_inline_msg("Conservative parameters have been updated.")
    ! write(*,*) 'updated qx', fl%qx(1:4, 1, 1)
    ! write(*,*) 'updated qx', fl%qx(1, 1:4, 1)
  end if
#endif

    return
  end subroutine Solve_momentum_eq

end module eq_momentum_mod
