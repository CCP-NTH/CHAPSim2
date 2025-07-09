module convert_primary_conservative_mod
  public :: convert_primary_conservative
 contains 

  subroutine convert_primary_conservative(fl, dm, itag, iloc)
    use udf_type_mod
    use operations
    use decomp_2d
    use parameters_constant_mod
    use cylindrical_rn_mod
    implicit none
    type(t_domain), intent(inout)   :: dm
    type(t_flow  ), intent(inout) :: fl
    integer, intent(in) :: itag
    integer, intent(in) :: iloc

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) ::  d_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) ::  d_ccc_zpencil

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) ::  d_pcc_xpencil
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3)) ::  d_pcc_ypencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3)) ::  d_pcc_zpencil

    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) ::  d_cpc_xpencil
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3)) ::  d_cpc_zpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) ::  d_cpc_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: qy_cpc_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: gy_cpc_ypencil

    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) ::  d_ccp_xpencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) ::  d_ccp_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) ::  d_ccp_zpencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: qz_ccp_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: qz_ccp_zpencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: gz_ccp_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: gz_ccp_zpencil

    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc 
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4
    
    if(.not. dm%is_thermo) return
!----------------------------------------------------------------------------------------------------------
! x-pencil : u1 -> g1 = u1_pcc * d_pcc
!----------------------------------------------------------------------------------------------------------
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%d
    call Get_x_midp_C2P_3D (fl%dDens, d_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp, fbcx_4cc)
    if(iloc == IBLK .or. iloc == IALL) then
    if(itag == IQ2G) fl%gx = fl%qx * d_pcc_xpencil
    if(itag == IG2Q) fl%qx = fl%gx / d_pcc_xpencil
    end if
!----------------------------------------------------------------------------------------------------------
! y-pencil : u2 -> g2 = u2_cpc * d_cpc
!----------------------------------------------------------------------------------------------------------
    call transpose_x_to_y(fl%dDens, d_ccc_ypencil, dm%dccc)
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%d
    call Get_y_midp_C2P_3D (d_ccc_ypencil, d_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_c4c)
    call axis_estimating_radial_xpx(d_cpc_ypencil, dm%dcpc, IPENCIL(2), dm, IDIM(1)) 
    if(iloc == IBLK .or. iloc == IALL) then
    if(itag == IQ2G) then
      call transpose_x_to_y(fl%qy, qy_cpc_ypencil, dm%dcpc)
      gy_cpc_ypencil = qy_cpc_ypencil * d_cpc_ypencil
      call transpose_y_to_x(gy_cpc_ypencil, fl%gy, dm%dcpc)
    else if(itag == IG2Q) then
      call transpose_x_to_y(fl%gy, gy_cpc_ypencil, dm%dcpc)
      qy_cpc_ypencil = gy_cpc_ypencil / d_cpc_ypencil
      call transpose_y_to_x(qy_cpc_ypencil, fl%qy, dm%dcpc)
    else
    end if
    end if
!----------------------------------------------------------------------------------------------------------
! Z-pencil : u3 -> g3 = u3_ccp * d_ccp
!----------------------------------------------------------------------------------------------------------
    call transpose_y_to_z( d_ccc_ypencil,  d_ccc_zpencil, dm%dccc)
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%d
    call Get_z_midp_C2P_3D (d_ccc_zpencil, d_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp, fbcz_cc4)
    if(iloc == IBLK .or. iloc == IALL) then
    if(itag == IQ2G) then
      call transpose_x_to_y(fl%qz,          qz_ccp_ypencil, dm%dccp)
      call transpose_y_to_z(qz_ccp_ypencil, qz_ccp_zpencil, dm%dccp)
      gz_ccp_zpencil = qz_ccp_zpencil * d_ccp_zpencil
      call transpose_z_to_y(gz_ccp_zpencil, gz_ccp_ypencil, dm%dccp)
      call transpose_y_to_x(gz_ccp_ypencil, fl%gz,          dm%dccp)
    else if(itag == IG2Q) then
      call transpose_x_to_y(fl%gz,          gz_ccp_ypencil, dm%dccp)
      call transpose_y_to_z(gz_ccp_ypencil, gz_ccp_zpencil, dm%dccp)
      qz_ccp_zpencil = gz_ccp_zpencil / d_ccp_zpencil
      call transpose_z_to_y(qz_ccp_zpencil, qz_ccp_ypencil, dm%dccp)
      call transpose_y_to_x(qz_ccp_ypencil, fl%qz,          dm%dccp)
    else
    end if
    end if

!----------------------------------------------------------------------------------------------------------
! BC: - x pencil
!----------------------------------------------------------------------------------------------------------
    if(iloc == IBND .or. iloc == IALL) then
    if(dm%ibcx_qx(1) == IBC_DIRICHLET .or. dm%ibcx_qx(2) == IBC_DIRICHLET) then 
      if(itag == IQ2G) dm%fbcx_gx(:, :, :) = dm%fbcx_qx(:, :, :) * dm%fbcx_ftp(:, :, :)%d
      if(itag == IG2Q) dm%fbcx_qx(:, :, :) = dm%fbcx_gx(:, :, :) / dm%fbcx_ftp(:, :, :)%d
    end if

    if(dm%ibcx_qy(1) == IBC_DIRICHLET .or. dm%ibcx_qy(2) == IBC_DIRICHLET) then 
      call transpose_y_to_x(d_cpc_ypencil, d_cpc_xpencil, dm%dcpc)
      if(itag == IQ2G) then
        dm%fbcx_gy(1, :, :) = dm%fbcx_qy(1, :, :) * d_cpc_xpencil(1,              :, :)
        dm%fbcx_gy(2, :, :) = dm%fbcx_qy(2, :, :) * d_cpc_xpencil(dm%dcpc%xsz(1), :, :)
        dm%fbcx_gy(3, :, :) = dm%fbcx_gy(1, :, :)
        dm%fbcx_gy(4, :, :) = dm%fbcx_gy(2, :, :)
      else  if(itag == IG2Q)then
        dm%fbcx_qy(1, :, :) = dm%fbcx_gy(1, :, :) / d_cpc_xpencil(1,              :, :)
        dm%fbcx_qy(2, :, :) = dm%fbcx_gy(2, :, :) / d_cpc_xpencil(dm%dcpc%xsz(1), :, :)
        dm%fbcx_qy(3, :, :) = dm%fbcx_qy(1, :, :)
        dm%fbcx_qy(4, :, :) = dm%fbcx_qy(2, :, :)
      else
      end if
    end if

    if(dm%ibcx_qz(1) == IBC_DIRICHLET .or. dm%ibcx_qz(2) == IBC_DIRICHLET) then 
      call transpose_z_to_y(d_ccp_zpencil, d_ccp_ypencil, dm%dccp)
      call transpose_y_to_x(d_ccp_ypencil, d_ccp_xpencil, dm%dccp)
      if(itag == IQ2G) then
        dm%fbcx_gz(1, :, :) = dm%fbcx_qz(1, :, :) * d_ccp_xpencil(1,              :, :)
        dm%fbcx_gz(2, :, :) = dm%fbcx_qz(2, :, :) * d_ccp_xpencil(dm%dccp%xsz(1), :, :)
        dm%fbcx_gz(3, :, :) = dm%fbcx_gz(1, :, :)
        dm%fbcx_gz(4, :, :) = dm%fbcx_gz(2, :, :)
      else  if(itag == IG2Q)then
        dm%fbcx_qz(1, :, :) = dm%fbcx_gz(1, :, :) / d_ccp_xpencil(1,              :, :)
        dm%fbcx_qz(2, :, :) = dm%fbcx_gz(2, :, :) / d_ccp_xpencil(dm%dccp%xsz(1), :, :)
        dm%fbcx_qz(3, :, :) = dm%fbcx_qz(1, :, :)
        dm%fbcx_qz(4, :, :) = dm%fbcx_qz(2, :, :)
      else
      end if
    end if
!----------------------------------------------------------------------------------------------------------
! BC: - y pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%ibcy_qx(1) == IBC_DIRICHLET .or. dm%ibcy_qx(2) == IBC_DIRICHLET) then 
      call transpose_x_to_y(d_pcc_xpencil, d_pcc_ypencil, dm%dpcc)
      if(itag == IQ2G) then
        dm%fbcy_gx(:, 1, :) = dm%fbcy_qx(:, 1, :) * d_pcc_ypencil(:, 1, :)
        dm%fbcy_gx(:, 2, :) = dm%fbcy_qx(:, 2, :) * d_pcc_ypencil(:, dm%dpcc%ysz(2), :)
        dm%fbcy_gx(:, 3, :) = dm%fbcy_gx(:, 1, :)
        dm%fbcy_gx(:, 4, :) = dm%fbcy_gx(:, 2, :)
      else if(itag == IG2Q) then
        dm%fbcy_qx(:, 1, :) = dm%fbcy_gx(:, 1, :) / d_pcc_ypencil(:, 1, :)
        dm%fbcy_qx(:, 2, :) = dm%fbcy_gx(:, 2, :) / d_pcc_ypencil(:, dm%dpcc%ysz(2), :)
        dm%fbcy_qx(:, 3, :) = dm%fbcy_qx(:, 1, :)
        dm%fbcy_qx(:, 4, :) = dm%fbcy_qx(:, 2, :)
      else
      end if
    end if

    if(dm%ibcy_qy(1) == IBC_DIRICHLET .or. dm%ibcy_qy(1) == IBC_DIRICHLET) then 
      if(itag == IQ2G) then
        dm%fbcy_gy(:, 1, :) = dm%fbcy_qy(:, 1, :) * d_cpc_ypencil(:, 1,              :)
        dm%fbcy_gy(:, 2, :) = dm%fbcy_qy(:, 2, :) * d_cpc_ypencil(:, dm%dcpc%ysz(2), :)
        dm%fbcy_gy(:, 3, :) = dm%fbcy_gy(:, 1, :)
        dm%fbcy_gy(:, 4, :) = dm%fbcy_gy(:, 2, :)
      else if(itag == IG2Q) then
        dm%fbcy_qy(:, 1, :) = dm%fbcy_gy(:, 1, :) / d_cpc_ypencil(:, 1,              :)
        dm%fbcy_qy(:, 2, :) = dm%fbcy_gy(:, 2, :) / d_cpc_ypencil(:, dm%dcpc%ysz(2), :)
        dm%fbcy_qy(:, 3, :) = dm%fbcy_qy(:, 1, :)
        dm%fbcy_qy(:, 4, :) = dm%fbcy_qy(:, 2, :)
      else
      end if
    end if

    if(dm%ibcy_qz(1) == IBC_DIRICHLET .or. dm%ibcy_qz(1) == IBC_DIRICHLET) then 
      call transpose_z_to_y(d_ccp_zpencil, d_ccp_ypencil, dm%dccp)
      if(itag == IQ2G) then
        dm%fbcy_gz(:, 1, :) = dm%fbcy_qz(:, 1, :) * d_ccp_ypencil(:, 1,              :)
        dm%fbcy_gz(:, 2, :) = dm%fbcy_qz(:, 2, :) * d_ccp_ypencil(:, dm%dccp%ysz(2), :)
        dm%fbcy_gz(:, 3, :) = dm%fbcy_gz(:, 1, :)
        dm%fbcy_gz(:, 4, :) = dm%fbcy_gz(:, 2, :)
      else if(itag == IG2Q) then
        dm%fbcy_qz(:, 1, :) = dm%fbcy_gz(:, 1, :) / d_ccp_ypencil(:, 1,              :)
        dm%fbcy_qz(:, 2, :) = dm%fbcy_gz(:, 2, :) / d_ccp_ypencil(:, dm%dccp%ysz(2), :)
        dm%fbcy_qz(:, 3, :) = dm%fbcy_qz(:, 1, :)
        dm%fbcy_qz(:, 4, :) = dm%fbcy_qz(:, 2, :)
      else
      end if
    end if
!----------------------------------------------------------------------------------------------------------
! BC: - z pencil
!----------------------------------------------------------------------------------------------------------
    if(dm%ibcz_qx(1) == IBC_DIRICHLET .or. dm%ibcz_qx(1) == IBC_DIRICHLET) then 
      call transpose_x_to_y(d_pcc_xpencil, d_pcc_ypencil)
      call transpose_y_to_z(d_pcc_ypencil, d_pcc_zpencil, dm%dpcc)
      if(itag == IQ2G) then
        dm%fbcz_gx(:, :, 1) = dm%fbcz_qx(:, :, 1) * d_pcc_zpencil(:, :, 1             )
        dm%fbcz_gx(:, :, 2) = dm%fbcz_qx(:, :, 2) * d_pcc_zpencil(:, :, dm%dpcc%zsz(3))
        dm%fbcz_gx(:, :, 3) = dm%fbcz_gx(:, :, 1)
        dm%fbcz_gx(:, :, 4) = dm%fbcz_gx(:, :, 2)
      else if(itag == IG2Q) then
        dm%fbcz_qx(:, :, 1) = dm%fbcz_gx(:, :, 1) / d_pcc_zpencil(:, :, 1             )
        dm%fbcz_qx(:, :, 2) = dm%fbcz_gx(:, :, 2) / d_pcc_zpencil(:, :, dm%dpcc%zsz(3))
        dm%fbcz_qx(:, :, 3) = dm%fbcz_qx(:, :, 1)
        dm%fbcz_qx(:, :, 4) = dm%fbcz_qx(:, :, 2)
      else
      end if
    end if

    if(dm%ibcz_qy(1) == IBC_DIRICHLET .or. dm%ibcz_qy(1) == IBC_DIRICHLET) then 
      call transpose_y_to_x(d_cpc_ypencil, d_cpc_zpencil, dm%dcpc)
      if(itag == IQ2G) then
        dm%fbcz_gy(:, :, 1) = dm%fbcz_qy(:, :, 1) * d_cpc_zpencil(:, :, 1             )
        dm%fbcz_gy(:, :, 2) = dm%fbcz_qy(:, :, 2) * d_cpc_zpencil(:, :, dm%dcpc%zsz(3))
        dm%fbcz_gy(:, :, 3) = dm%fbcz_gy(:, :, 1)
        dm%fbcz_gy(:, :, 4) = dm%fbcz_gy(:, :, 2)
      else if(itag == IG2Q) then
        dm%fbcz_qy(:, :, 1) = dm%fbcz_gy(:, :, 1) / d_cpc_zpencil(:, :, 1             )
        dm%fbcz_qy(:, :, 2) = dm%fbcz_gy(:, :, 2) / d_cpc_zpencil(:, :, dm%dcpc%zsz(3))
        dm%fbcz_qy(:, :, 3) = dm%fbcz_qy(:, :, 1)
        dm%fbcz_qy(:, :, 4) = dm%fbcz_qy(:, :, 2)
      else
      end if
    end if

    if(dm%ibcz_qz(1) == IBC_DIRICHLET .or. dm%ibcz_qz(2) == IBC_DIRICHLET) then 
      if(itag == IQ2G) then
        dm%fbcz_gz(:, :, 1) = dm%fbcz_qz(:, :, 1) * d_ccp_zpencil(:, :, 1             )
        dm%fbcz_gz(:, :, 2) = dm%fbcz_qz(:, :, 2) * d_ccp_zpencil(:, :, dm%dccp%zsz(3))
        dm%fbcz_gz(:, :, 3) = dm%fbcz_gz(:, :, 1)
        dm%fbcz_gz(:, :, 4) = dm%fbcz_gz(:, :, 2)
      else if(itag == IG2Q) then
        dm%fbcz_qz(:, :, 1) = dm%fbcz_gz(:, :, 1) / d_ccp_zpencil(:, :, 1             )
        dm%fbcz_qz(:, :, 2) = dm%fbcz_gz(:, :, 2) / d_ccp_zpencil(:, :, dm%dccp%zsz(3))
        dm%fbcz_qz(:, :, 3) = dm%fbcz_qz(:, :, 1)
        dm%fbcz_qz(:, :, 4) = dm%fbcz_qz(:, :, 2)
      else
      end if
    end if
    end if


    return
  end subroutine
end module