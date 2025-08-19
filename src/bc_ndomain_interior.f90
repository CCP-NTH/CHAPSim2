module bc_ndomain_interior_mod ! not used anymore
  use parameters_constant_mod
  use udf_type_mod
  use decomp_2d
  use print_msg_mod
  private
  integer, parameter :: IFBC(1:2) = (/1, 2/)

  private :: apply_fbcx_2dm_halo
  private :: apply_fbcy_2dm_halo
  private :: apply_fbcz_2dm_halo
  private :: apply_fbc_2dm_flow_halo
  public  :: update_fbc_2dm_flow_halo   ! for multiple domains only, update every NS 
  public  :: update_fbc_2dm_thermo_halo ! for multiple domains only, update every NS

contains
!==========================================================================================================
  subroutine apply_fbcx_2dm_halo(fbcx, var, iside, dtmp)
    use print_msg_mod
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in)     :: iside
    real(WP), intent(in)    :: var(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3))
    real(WP), intent(inout) :: fbcx(:, :, :)
    
    if(iside == IFBC(1)) then !122
      fbcx(1, :, :) = var(dtmp%xsz(1),     :, :) ! interior there is a repeated shared points.
      fbcx(3, :, :) = var(dtmp%xsz(1) - 1, :, :)
    else if(iside == IFBC(2)) then !221
      fbcx(2, :, :) = var(1, :, :)
      fbcx(4, :, :) = var(2, :, :)
    else
      call Print_error_msg('Error input for apply_fbcx_2dm_halo')
    end if

    return
  end subroutine
!==========================================================================================================
  subroutine apply_fbcy_2dm_halo(fbcy, var, iside, dtmp)
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in)     :: iside
    real(WP), intent(in)    :: var(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3))
    real(WP), intent(inout) :: fbcy(:, :, :)

    real(WP), dimension( dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3) ) :: var_ypencil

    call transpose_x_to_y(var, var_ypencil, dtmp)
    if(iside == IFBC(1)) then
      fbcy(:, 1, :) = var_ypencil(:, dtmp%ysz(2),     :) ! interior there is a repeated shared points.
      fbcy(:, 3, :) = var_ypencil(:, dtmp%ysz(2) - 1, :)
    else if(iside == IFBC(2)) then
      fbcy(:, 2, :) = var_ypencil(:, 1, :)
      fbcy(:, 4, :) = var_ypencil(:, 2, :)
    else
      call Print_error_msg('Error input for apply_fbcy_2dm_halo')
    end if

    return
  end subroutine
!==========================================================================================================
  subroutine apply_fbcz_2dm_halo(fbcz, var, iside, dtmp)
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in)     :: iside
    real(WP), intent(in)    :: var(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3))
    real(WP), intent(inout) :: fbcz(:, :, :)

    real(WP), dimension( dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3) ) :: var_ypencil
    real(WP), dimension( dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3) ) :: var_zpencil

    call transpose_x_to_y(var,         var_ypencil, dtmp)
    call transpose_y_to_z(var_ypencil, var_zpencil, dtmp)
    if(iside == IFBC(1)) then
      fbcz(:, :, 1) = var_zpencil(:, :, dtmp%ysz(2)    ) ! interior there is a repeated shared points.
      fbcz(:, :, 3) = var_zpencil(:, :, dtmp%ysz(2) - 1)
    else if(iside == IFBC(2)) then
      fbcz(:, 2, :) = var_zpencil(:, :, 1)
      fbcz(:, 4, :) = var_zpencil(:, :, 2)
    else
      call Print_error_msg('Error input for apply_fbcz_2dm_halo')
    end if

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine apply_fbc_2dm_flow_halo(dm, fl, iside, bc_type)
    use cylindrical_rn_mod
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(in)    :: fl
    integer, intent(in)           :: iside
    character(len=*), intent(in)  :: bc_type

    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: ac4c_ypencil
    real(WP), dimension( dm%dcpp%ysz(1), 4, dm%dcpp%ysz(3) ) :: ac4p_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: acc4_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), 4 ) :: acp4_zpencil
!----------------------------------------------------------------------------------------------------------
!   one repeat on nodes: 1'-2'-3'-4'-5'          no repeat of cells 1--2--3--4--5-
!                                    1'-2'-3'-4'-5'                               -1--2--3--4--5     
!                                 f2 f1 f2                                   f2 f1 f1 f2
!   iside = 122, 1-->2  : |-domain-1----3-1|-domain-2-----|
!   iside = 221, 1<--2  : |-domain-1---|-2-4-domain-2-----|
!----------------------------------------------------------------------------------------------------------
    select case (bc_type)
    case ('x')
      if (dm%ibcx_qx(iside) == IBC_INTERIOR) then
        call apply_fbcx_2dm_halo(dm%fbcx_qx, fl%qx, iside, dm%dpcc)
        if (dm%is_thermo) &
        call apply_fbcx_2dm_halo(dm%fbcx_gx, fl%gx, iside, dm%dpcc)
      end if
      if (dm%ibcx_qy(iside) == IBC_INTERIOR) then
        call apply_fbcx_2dm_halo(dm%fbcx_qy, fl%qy, iside, dm%dcpc)
        if (dm%is_thermo) &
        call apply_fbcx_2dm_halo(dm%fbcx_gy, fl%gy, iside, dm%dcpc)
      end if
      if (dm%ibcx_qz(iside) == IBC_INTERIOR) then
        call apply_fbcx_2dm_halo(dm%fbcx_qz, fl%qz, iside, dm%dccp)
        if (dm%is_thermo) &
        call apply_fbcx_2dm_halo(dm%fbcx_gz, fl%gz, iside, dm%dccp)
      end if
      if (dm%ibcx_pr(iside) == IBC_INTERIOR) then
        call apply_fbcx_2dm_halo(dm%fbcx_pr, fl%pres, iside, dm%dccc)
      end if
    case ('y')
      if (dm%ibcy_qx(iside) == IBC_INTERIOR) then
        call apply_fbcy_2dm_halo(dm%fbcy_qx, fl%qx, iside, dm%dpcc)
        if (dm%is_thermo) &
        call apply_fbcy_2dm_halo(dm%fbcy_gx, fl%gx, iside, dm%dpcc)
      end if
      if (dm%ibcy_qy(iside) == IBC_INTERIOR) then
        call apply_fbcy_2dm_halo(dm%fbcy_qy, fl%qy, iside, dm%dcpc)
        if (dm%is_thermo) &
        call apply_fbcy_2dm_halo(dm%fbcy_gy, fl%gy, iside, dm%dcpc)
        if (dm%icoordinate == ICYLINDRICAL) then
          ac4c_ypencil = dm%fbcy_qy
          call multiple_cylindrical_rn_x4x(ac4c_ypencil, dm%dcpc, dm%rpi, 1, IPENCIL(2))
          dm%fbcy_qyr = ac4c_ypencil
        end if
      end if
      if (dm%ibcy_qz(iside) == IBC_INTERIOR) then
        call apply_fbcy_2dm_halo(dm%fbcy_qz, fl%qz, iside, dm%dccp)
        if (dm%is_thermo) &
        call apply_fbcy_2dm_halo(dm%fbcy_gz, fl%gz, iside, dm%dccp)
        if (dm%icoordinate == ICYLINDRICAL) then
          ac4p_ypencil = dm%fbcy_qz
          call multiple_cylindrical_rn_x4x(ac4p_ypencil, dm%dcpp, dm%rci, 1, IPENCIL(2))
          dm%fbcy_qzr = ac4p_ypencil
        end if
      end if
      if (dm%ibcy_pr(iside) == IBC_INTERIOR) then
        call apply_fbcy_2dm_halo(dm%fbcy_pr, fl%pres, iside, dm%dccc)
      end if
    case ('z')
      if (dm%ibcz_qx(iside) == IBC_INTERIOR) then
          call apply_fbcz_2dm_halo(dm%fbcz_qx, fl%qx, iside, dm%dpcc)
          if (dm%is_thermo) &
          call apply_fbcz_2dm_halo(dm%fbcz_gx, fl%gx, iside, dm%dpcc)
      end if
      if (dm%ibcz_qy(iside) == IBC_INTERIOR) then
        call apply_fbcz_2dm_halo(dm%fbcz_qy, fl%qy, iside, dm%dcpc)
        if (dm%is_thermo) &
        call apply_fbcz_2dm_halo(dm%fbcz_gy, fl%gy, iside, dm%dcpc)
        if (dm%icoordinate == ICYLINDRICAL) then
          acp4_zpencil = dm%fbcz_qy
          call multiple_cylindrical_rn_xx4(acp4_zpencil, dm%dcpc, dm%rpi, 1, IPENCIL(3))
          dm%fbcz_qyr = acp4_zpencil
        end if
      end if
      if (dm%ibcz_qz(iside) == IBC_INTERIOR) then
        call apply_fbcz_2dm_halo(dm%fbcz_qz, fl%qz, iside, dm%dccp)
        if (dm%is_thermo) &
        call apply_fbcz_2dm_halo(dm%fbcz_gz, fl%gz, iside, dm%dccp)
        if (dm%icoordinate == ICYLINDRICAL) then
          acc4_zpencil = dm%fbcz_qz
          call multiple_cylindrical_rn_xx4(acc4_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
          dm%fbcz_qzr = acc4_zpencil
        end if
      end if
      if (dm%ibcz_pr(iside) == IBC_INTERIOR) then
        call apply_fbcz_2dm_halo(dm%fbcz_pr, fl%pres, iside, dm%dccc)
      end if
    end select
  end subroutine apply_fbc_2dm_flow_halo
!==========================================================================================================
!==========================================================================================================
  subroutine update_fbc_2dm_flow_halo(dm1, fl1, dm2, fl2)
    type(t_domain), intent(inout) :: dm1, dm2
    type(t_flow),   intent(in)    :: fl1, fl2

    integer :: n
    ! x-boundary conditions
    call apply_fbc_2dm_flow_halo(dm2, fl1, IFBC(1), 'x')
    call apply_fbc_2dm_flow_halo(dm1, fl2, IFBC(2), 'x')
    ! y-boundary conditions
    call apply_fbc_2dm_flow_halo(dm2, fl1, IFBC(1), 'y')
    call apply_fbc_2dm_flow_halo(dm1, fl2, IFBC(2), 'y')
    ! z-boundary conditions
    call apply_fbc_2dm_flow_halo(dm2, fl1, IFBC(1), 'z')
    call apply_fbc_2dm_flow_halo(dm1, fl2, IFBC(2), 'z')

    ! for turb inlet only, no thermal inlet
    if (dm2%is_thermo .and. (.not.dm1%is_thermo)) then
      do n = 1, 3, 2
        ! x-boundary conditions
        if(dm2%ibcx_qx(1) == IBC_INTERIOR) &
        dm2%fbcx_gx(n, :, :) = dm2%fbcx_qx(n, :, :) * dm2%fbcx_ftp(1, :, :)%d
        if(dm2%ibcx_qy(1) == IBC_INTERIOR) &
        dm2%fbcx_gy(n, :, :) = dm2%fbcx_qy(n, :, :) * dm2%fbcx_ftp(1, :, :)%d
        if(dm2%ibcx_qz(1) == IBC_INTERIOR) &
        dm2%fbcx_gz(n, :, :) = dm2%fbcx_qz(n, :, :) * dm2%fbcx_ftp(1, :, :)%d
        ! x-boundary conditions
        if(dm2%ibcy_qx(1) == IBC_INTERIOR) &
        dm2%fbcy_gx(:, n, :) = dm2%fbcy_qx(:, n, :) * dm2%fbcy_ftp(:, 1, :)%d
        if(dm2%ibcy_qy(1) == IBC_INTERIOR) then
          dm2%fbcy_gy(:, n, :) = dm2%fbcy_qy(:, n, :) * dm2%fbcy_ftp(:, 1, :)%d
          !if(dm2%icoordinate == ICYLINDRICAL) &
          !dm2%fbcy_gyr(:, n, :)= dm2%fbcy_qyr(:, n, :) * dm2%fbcy_ftp(:, 1, :)%d
        end if
        if(dm2%ibcy_qz(1) == IBC_INTERIOR) then
          dm2%fbcy_gz(:, n, :) = dm2%fbcy_qz(:, n, :) * dm2%fbcy_ftp(:, 1, :)%d
          !if(dm2%icoordinate == ICYLINDRICAL) &
          !dm2%fbcy_gzr(:, n, :)= dm2%fbcy_qzr(:, n, :) * dm2%fbcy_ftp(:, 1, :)%d
        end if
        ! z-boundary conditions
        if(dm2%ibcz_qx(1) == IBC_INTERIOR) &
        dm2%fbcz_gx(:, :, n) = dm2%fbcz_qx(:, :, n) * dm2%fbcz_ftp(:, :, 1)%d
        if(dm2%ibcz_qy(1) == IBC_INTERIOR) then
          dm2%fbcz_gy(:, :, n) = dm2%fbcz_qy(:, :, n) * dm2%fbcz_ftp(:, :, 1)%d
          !if(dm2%icoordinate == ICYLINDRICAL) &
          !dm2%fbcz_gyr(:, :, n) = dm2%fbcz_qyr(:, :, n) * dm2%fbcz_ftp(:, :, 1)%d
        end if
        if(dm2%ibcz_qz(1) == IBC_INTERIOR) then
          dm2%fbcz_gz(:, :, n) = dm2%fbcz_qz(:, :, n) * dm2%fbcz_ftp(:, :, 1)%d
          !if(dm2%icoordinate == ICYLINDRICAL) &
          !dm2%fbcz_gzr(:, :, n) = dm2%fbcz_qzr(:, :, n) * dm2%fbcz_ftp(:, :, 1)%d
        end if
      end do
    end if
    
    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
  subroutine update_fbc_2dm_thermo_halo(dm1, tm1, dm2, tm2)
    use thermo_info_mod
    type(t_domain), intent(inout) :: dm1, dm2
    type(t_thermo), intent(in)    :: tm1, tm2
    
    real(WP), dimension( dm1%dccc%ysz(1), dm1%dccc%ysz(2), dm1%dccc%ysz(3) ) :: accc0_ypencil
    real(WP), dimension( dm1%dccc%zsz(1), dm1%dccc%zsz(2), dm1%dccc%zsz(3) ) :: accc0_zpencil
    real(WP), dimension( dm2%dccc%ysz(1), dm2%dccc%ysz(2), dm2%dccc%ysz(3) ) :: accc1_ypencil
    real(WP), dimension( dm2%dccc%zsz(1), dm2%dccc%zsz(2), dm2%dccc%zsz(3) ) :: accc1_zpencil
!----------------------------------------------------------------------------------------------------------
!   bc in x - direction
!----------------------------------------------------------------------------------------------------------
    ! thermal field, dm1-dm2
    if(dm2%ibcx_Tm(1) == IBC_INTERIOR) then
      dm2%fbcx_ftp(1, :, :)%t = tm1%tTemp(dm1%nc(1),     :, :)
      dm2%fbcx_ftp(3, :, :)%t = tm1%tTemp(dm1%nc(1) - 1, :, :)
      call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm2%fbcx_ftp)
    end if

    if(dm1%ibcx_Tm(2) == IBC_INTERIOR) then
      dm1%fbcx_ftp(2, :, :)%t = tm2%tTemp(1, :, :)
      dm1%fbcx_ftp(4, :, :)%t = tm2%tTemp(2, :, :)
      call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm1%fbcx_ftp)
    end if
!----------------------------------------------------------------------------------------------------------
!   bc in y - direction
!----------------------------------------------------------------------------------------------------------
    ! thermal field, dm1-dm2
    if(dm2%ibcy_Tm(1) == IBC_INTERIOR) then
      call transpose_x_to_y(tm1%tTemp, accc0_ypencil, dm1%dccc)
      dm2%fbcy_ftp(:, 1, :)%t = accc0_ypencil(:, dm1%nc(1),     :)
      dm2%fbcy_ftp(:, 3, :)%t = accc0_ypencil(:, dm1%nc(1) - 1, :)
      call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm2%fbcy_ftp)
    end if

    if(dm1%ibcy_Tm(2) == IBC_INTERIOR) then
      call transpose_x_to_y(tm2%tTemp, accc1_ypencil, dm2%dccc)
      dm1%fbcy_ftp(:, 2, :)%t = accc1_ypencil(:, 1, :)
      dm1%fbcy_ftp(:, 4, :)%t = accc1_ypencil(:, 2, :)
      call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm1%fbcy_ftp)
    end if
!----------------------------------------------------------------------------------------------------------
!   bc in z - direction
!----------------------------------------------------------------------------------------------------------
    ! thermal field, dm1-dm2
    if(dm2%ibcz_Tm(1) == IBC_INTERIOR) then
      call transpose_x_to_y(tm1%tTemp,     accc0_ypencil, dm1%dccc)
      call transpose_y_to_z(accc0_ypencil, accc0_zpencil, dm1%dccc)
      dm2%fbcz_ftp(:, :, 1)%t = accc0_zpencil(:, :, dm1%nc(1)    )
      dm2%fbcz_ftp(:, :, 3)%t = accc0_zpencil(:, :, dm1%nc(1) - 1)
      call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm2%fbcz_ftp)
    end if

    if(dm1%ibcz_Tm(2) == IBC_INTERIOR) then
      call transpose_x_to_y(tm2%tTemp,     accc1_ypencil, dm2%dccc)
      call transpose_y_to_z(accc1_ypencil, accc1_zpencil, dm2%dccc)
      dm1%fbcz_ftp(:, :, 2)%t = accc1_zpencil(:, :, 1)
      dm1%fbcz_ftp(:, :, 4)%t = accc1_zpencil(:, :, 2)
      call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm1%fbcz_ftp)
    end if

    return
  end subroutine

end module