module continuity_eq_mod
  use operations
  use decomp_2d

  public :: Calculate_drhodt
  public :: Get_divergence_vector
  public :: Get_divergence_flow
  public :: Get_divergence_vel_x2z
  public :: Check_element_mass_conservation
contains
!==========================================================================================================
!==========================================================================================================
!> \brief To calculate d(\rho)/dt in the continuity eq.
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]  dDens            density at the current time step
!> \param[in]  dDensm1          density at the t-1 time step
!> \param[in]  dDensm2          density at the t-2 time step
!> \param[out] drhodt           d(rho)/dt
!> \param[in]  itime            the sub-step in RK3
!_______________________________________________________________________________
  subroutine Calculate_drhodt(fl, dm)
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: div
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: div0


    if( .not. dm%is_thermo) return

    ! method 1:
    if(dm%iTimeScheme == ITIME_AB2) then
      fl%pcor(:, :, :) = HALF * fl%dDens  (:, :, :) - &
                         TWO  * fl%dDensm1(:, :, :) + &
                         HALF * fl%dDensm2(:, :, :)
      fl%pcor(:, :, :) = fl%pcor(:, :, :) / dm%dt

    else if (dm%iTimeScheme == ITIME_RK3 .or. dm%iTimeScheme == ITIME_RK3_CN) then
      ! ! to check this part, is iteration necessary?
      ! drhodt(:, :, :) = fl%dDens  (:, :, :)
      ! do i = 1, dm%nsubitr
      !   drhodt(:, :, :) = drhodt(:, :, :) + dm%tAlpha(i) * &
      !                     (fl%dDensm1(:, :, :) - fl%dDensm2(:, :, :))  / dm%dt
      ! end do

      !fl%pcor(:, :, :) = HALF * fl%dDens  (:, :, :) - &
       !                  TWO  * fl%dDensm1(:, :, :) + &
      !                   HALF * fl%dDensm2(:, :, :)
      fl%pcor(:, :, :) = fl%dDens(:, :, :) - fl%dDensm1(:, :, :)

      fl%pcor(:, :, :) = fl%pcor(:, :, :) / dm%dt
    else  
      ! default, Euler 1st order 
      fl%pcor(:, :, :) = fl%dDens(:, :, :) - fl%dDensm1(:, :, :)
      fl%pcor(:, :, :) = fl%pcor(:, :, :) / dm%dt
    end if

    !fl%pcor = ZERO 

    fl%drhodt = fl%pcor

    

!  method 2
    

    !call Get_divergence_vector(fl%gx, fl%gy, fl%gz, div, dm)
    !call Get_divergence_vector(fl%gx0, fl%gy0, fl%gz0, div0, dm)

    ! fl%pcor=  ( dm%tAlpha(isub) * div + dm%tZeta(isub) * div0 ) &
    !           / (dm%tGamma(isub) - TWO)
    !fl%pcor =  -dm%tGamma(isub) * div - dm%tZeta(isub) * div0


    return
  end subroutine Calculate_drhodt

  !==========================================================================================================
!==========================================================================================================
!> \brief To calculate divergence of (rho * u) or divergence of (u)
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[out]    div          div(q) or div(g)
!> \param[in]     d            domain
!_______________________________________________________________________________
  subroutine Get_divergence_flow(fl, div, dm)
    use parameters_constant_mod
    use udf_type_mod
    use solver_tools_mod
    use cylindrical_rn_mod
    implicit none

    type(t_domain), intent(in) :: dm
    type(t_flow),    intent(in) :: fl
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent (out) :: div

    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)):: qx
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)):: qy
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)):: qz

    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div0
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: div0_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: div0_zpencil

    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: qy_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: qz_ypencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: qz_zpencil

    if(dm%is_thermo) then
      qx = fl%gx
      qy = fl%gy
      qz = fl%gz
    else 
      qx = fl%qx
      qy = fl%qy
      qz = fl%qz
    end if

    div = ZERO
!----------------------------------------------------------------------------------------------------------
! operation in x pencil, dqx/dx
!----------------------------------------------------------------------------------------------------------
    div0 = ZERO
    call Get_x_1der_P2C_3D(qx, div0, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
!----------------------------------------------------------------------------------------------------------
! operation in y pencil, dqy/dy * (1/r)
!----------------------------------------------------------------------------------------------------------
    qy_ypencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(qy, qy_ypencil, dm%dcpc)
    call Get_y_1der_P2C_3D(qy_ypencil, div0_ypencil, dm, dm%iAccuracy, dm%ibcy_qy(:), dm%fbcy_qy)
    call transpose_y_to_x(div0_ypencil, div0, dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
!----------------------------------------------------------------------------------------------------------
! operation in z pencil, dw/dz * (1/r)
!----------------------------------------------------------------------------------------------------------
    qz_ypencil = ZERO
    qz_zpencil = ZERO
    div0_zpencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(qz,         qz_ypencil, dm%dccp)
    call transpose_y_to_z(qz_ypencil, qz_zpencil, dm%dccp)
    call Get_z_1der_P2C_3D(qz_zpencil, div0_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    call transpose_z_to_y(div0_zpencil, div0_ypencil, dm%dccc)
    call transpose_y_to_x(div0_ypencil, div0,         dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
    
    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
!> \brief To calculate divergence of (rho * u) or divergence of (u)
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ux           ux or gx
!> \param[in]     uy           uy or gy
!> \param[in]     uz           uz or gz
!> \param[out]    div          div(u) or div(g)
!> \param[in]     d            domain
!_______________________________________________________________________________
  subroutine Get_divergence_vector(ux, uy, uz, div, dm)
    use parameters_constant_mod
    use udf_type_mod
    use cylindrical_rn_mod
    implicit none

    type(t_domain), intent (in) :: dm
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in ) :: ux
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in ) :: uy
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in ) :: uz
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent (out) :: div

    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div0
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: div0_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: div0_zpencil

    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: uy_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: uz_ypencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: uz_zpencil

    div = ZERO
!----------------------------------------------------------------------------------------------------------
! operation in x pencil, du/dx
!----------------------------------------------------------------------------------------------------------
    div0 = ZERO
    call Get_x_1der_P2C_3D(ux, div0, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
    !write(*,*) 'div, x', div0(1, 1, 1), div0(2, 2, 2), div0(8, 8, 8)!, div0(16, 8, 8), div0(32, 8, 8)
!----------------------------------------------------------------------------------------------------------
! operation in y pencil, dqy/dy * (1/r)
!----------------------------------------------------------------------------------------------------------
    uy_ypencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(uy, uy_ypencil, dm%dcpc)
    call Get_y_1der_P2C_3D(uy_ypencil, div0_ypencil, dm, dm%iAccuracy, dm%ibcy_qy(:), dm%fbcy_qy)
    call transpose_y_to_x(div0_ypencil, div0, dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
    !write(*,*) 'div, y', div0(1, 1, 1), div0(2, 2, 2), div0(8, 8, 8)!, div0(16, 8, 8), div0(32, 8, 8)
!----------------------------------------------------------------------------------------------------------
! operation in z pencil, dw/dz * (1/r)
!----------------------------------------------------------------------------------------------------------
    uz_ypencil = ZERO
    uz_zpencil = ZERO
    div0_zpencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(uz,         uz_ypencil, dm%dccp)
    call transpose_y_to_z(uz_ypencil, uz_zpencil, dm%dccp)
    call Get_z_1der_P2C_3D(uz_zpencil, div0_zpencil, dm, dm%iAccuracy, dm%ibcz_qz(:), dm%fbcz_qz)
    call transpose_z_to_y(div0_zpencil, div0_ypencil, dm%dccc)
    call transpose_y_to_x(div0_ypencil, div0,         dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
    !write(*,*) 'div, z', div0(1, 1, 1), div0(2, 2, 2), div0(8, 8, 8)!, div0(16, 8, 8), div0(32, 8, 8)
    !write(*,*) 'divall', div0(1, 1, 1), div(8, 8, 8)
    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
!> \brief To calculate divergence of (rho * u) or divergence of (u)
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ux           ux or gx
!> \param[in]     uy           uy or gy
!> \param[in]     uz           uz or gz
!> \param[out]    div          div(u) or div(g)
!> \param[in]     d            domain
!_______________________________________________________________________________
  subroutine Get_divergence_vel_x2z(ux, uy, uz, div_zpencil_ggg, dm)
    use parameters_constant_mod
    use udf_type_mod
    use decomp_extended_mod
    use cylindrical_rn_mod
    implicit none

    type(t_domain), intent (in) :: dm
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in) :: ux
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in) :: uy
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in) :: uz
    real(WP), dimension(dm%dccc%zst(1) : dm%dccc%zen(1), &
                        dm%dccc%zst(2) : dm%dccc%zen(2), &
                        dm%dccc%zst(3) : dm%dccc%zen(3)), intent (out) :: div_zpencil_ggg

    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div0
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: div0_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: div0_zpencil
    real(WP), dimension(dm%dccc%yst(1) : dm%dccc%yen(1), &
                        dm%dccc%yst(2) : dm%dccc%yen(2), &
                        dm%dccc%ysz(3))                  :: div0_ypencil_ggl
    real(WP), dimension(dm%dccc%yst(1) : dm%dccc%yen(1), &
                        dm%dccc%yst(2) : dm%dccc%yen(2), &
                        dm%dccc%ysz(3))                  :: div_ypencil_ggl
    real(WP), dimension(dm%dccc%zst(1) : dm%dccc%zen(1), &
                        dm%dccc%zst(2) : dm%dccc%zen(2), &
                        dm%dccc%zst(3) : dm%dccc%zen(3)) :: div0_zpencil_ggg

    real(WP), dimension(dm%dcpc%ysz(1),                  dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: uy_ypencil
    !real(WP), dimension(dm%dccp%yst(1) : dm%dccp%yen(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: uz_ypencil_ggl

    real(WP), dimension(dm%dccp%ysz(1),                  dm%dccp%ysz(2),                  dm%dccp%ysz(3)) :: uz_ypencil
    real(WP), dimension(dm%dccp%zsz(1),                  dm%dccp%zsz(2),                  dm%dccp%zsz(3)) :: uz_zpencil

!----------------------------------------------------------------------------------------------------------
! operation in x pencil, du/dx
!----------------------------------------------------------------------------------------------------------
    div0 = ZERO
    div0_ypencil_ggl = ZERO
    div_ypencil_ggl = ZERO
    call Get_x_1der_P2C_3D(ux, div0, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    call transpose_x_to_y(div0, div0_ypencil_ggl, dm%dccc)
    div_ypencil_ggl = div0_ypencil_ggl
!----------------------------------------------------------------------------------------------------------
! operation in y pencil, dv/dy * (1/r)
!----------------------------------------------------------------------------------------------------------
    uy_ypencil = ZERO
    div0_ypencil = ZERO
    div0_ypencil_ggl = ZERO
    call transpose_x_to_y(uy, uy_ypencil, dm%dcpc)
    call Get_y_1der_P2C_3D(uy_ypencil, div0_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
    call ypencil_index_lgl2ggl(div0_ypencil, div0_ypencil_ggl, dm%dccc)
    div_ypencil_ggl = div_ypencil_ggl + div0_ypencil_ggl
    call transpose_y_to_z(div_ypencil_ggl, div_zpencil_ggg, dm%dccc)
!----------------------------------------------------------------------------------------------------------
! operation in z pencil, dw/dz * (1/r)
!----------------------------------------------------------------------------------------------------------
    uz_ypencil = ZERO
    uz_zpencil = ZERO
    div0_zpencil = ZERO
    div0_zpencil_ggg = ZERO
    call transpose_x_to_y(uz,         uz_ypencil, dm%dccp)
    call transpose_y_to_z(uz_ypencil, uz_zpencil, dm%dccp)
    call Get_z_1der_P2C_3D(uz_zpencil, div0_zpencil, dm, dm%iAccuracy, dm%ibcz_qz(:), dm%fbcz_qz)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0_zpencil, dm%dccc, dm%rci, 1, IPENCIL(3))
    call zpencil_index_llg2ggg(div0_zpencil, div0_zpencil_ggg, dm%dccc)
    div_zpencil_ggg = div_zpencil_ggg + div0_zpencil_ggg

    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
!> \brief To calculate divergence of (rho * u) or divergence of (u)
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ux           ux or gx
!> \param[in]     uy           uy or gy
!> \param[in]     uz           uz or gz
!> \param[out]    div          div(u) or div(g)
!> \param[in]     d            domain
!_______________________________________________________________________________
  subroutine Check_element_mass_conservation(fl, dm, iter, opt_isub, opt_str)
    use precision_mod
    use udf_type_mod
    use input_general_mod    
    use parameters_constant_mod
    use math_mod                
    use mpi_mod
    use solver_tools_mod
    use wtformat_mod
    use io_visualisation_mod
    use find_max_min_ave_mod
    use typeconvert_mod
    implicit none

    type(t_domain), intent( in) :: dm
    type(t_flow),   intent( inout) :: fl  
    integer, intent(in) :: iter
    integer, intent(in), optional :: opt_isub
    character(*), intent(in), optional :: opt_str                

    character(32) :: str
    integer :: n, nlayer, isub

    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div, drhodt

    if(present(opt_str)) then
      str = trim(opt_str)//'_iter_'//int2str(iter)
    else
      str = 'at iter = '//int2str(iter)
    end if

    if(present(opt_isub)) then
      isub = opt_isub
    else
      isub = 0
    end if

    !fl%pcor = ZERO
    div(:, :, :)  = ZERO
!----------------------------------------------------------------------------------------------------------
! $d\rho / dt$ at cell centre
!----------------------------------------------------------------------------------------------------------
    if (dm%is_thermo) then
      drhodt = fl%drhodt
    else
      drhodt = ZERO
    end if
!----------------------------------------------------------------------------------------------------------
! $d(\rho u_i)) / dx_i $ at cell centre
!----------------------------------------------------------------------------------------------------------
    call Get_divergence_flow(fl, div, dm)

write(*,*) 'test, dddt, div, plus', drhodt(4, 4, 4), div(4, 4, 4),  drhodt(4, 4, 4)+div(4, 4, 4)
    div = div + drhodt


#ifdef DEBUG_STEPS
    if(MOD(iter, dm%visu_nfre) == 0) &
    call write_visu_any3darray(div, 'divU', 'debug'//trim(str), dm%dccc, dm, fl%iteration)
#endif

    n = dm%dccc%xsz(1)
    nlayer = 4
    call Find_maximum_absvar3d(div(1         : nlayer,   :, :), fl%mcon(1), dm%dccc, "Mass Consv. (inlet  4) =", 1  )
    call Find_maximum_absvar3d(div(nlayer+1  : n-nlayer, :, :), fl%mcon(2), dm%dccc, "Mass Consv. (bulk    ) =", nlayer+1  )
    call Find_maximum_absvar3d(div(n-nlayer+1: n,        :, :), fl%mcon(3), dm%dccc, "Mass Consv. (outlet 4) =", n-nlayer+1)
    
    if(nrank == 0) then
      if(fl%mcon(2) > 5.0E-13_WP .and. fl%iteration > 100 ) &
      call Print_error_msg("Mass conservation is not strictly satisfied at the machine precision level.")
    end if

    ! if(nrank == 0) then
    !   write (*, wrtfmt1e) "  Check Mass Conservation:", divmax
    ! end if

    return
  end subroutine Check_element_mass_conservation 

end module continuity_eq_mod
