module convert_primary_conservative_mod
  public :: convert_primary_conservative
 contains

  subroutine convert_primary_conservative(dm, dens, itag, iloc, qx, qy, qz, gx, gy, gz)
    use boundary_conditions_mod
    use cylindrical_rn_mod
    use decomp_2d
    use operations
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout)   :: dm
    integer, intent(in) :: itag
    integer, intent(in) :: iloc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent(in) :: dens
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent(inout), optional ::  qx, gx
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent(inout), optional ::  qy, gy
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent(inout), optional ::  qz, gz

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) ::  d_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) ::  d_ccc_zpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) ::  d_pcc_xpencil
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3)) ::  d_pcc_ypencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3)) ::  d_pcc_zpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) ::  d_cpc_xpencil
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3)) ::  d_cpc_zpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) ::  d_cpc_ypencil
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) ::  d_ccp_xpencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) ::  d_ccp_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) ::  d_ccp_zpencil

    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: acpc_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: accp_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: accp_zpencil


    logical :: is_yaxis1

    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4

    if(.not. dm%is_thermo) return
    if(iloc == IBLK .or. iloc == IALL) then
      if(.not. present(qx)) call Print_error_msg(' Lack of primary variables')
      if(.not. present(qy)) call Print_error_msg(' Lack of primary variables')
      if(.not. present(qz)) call Print_error_msg(' Lack of primary variables')
      if(.not. present(gx)) call Print_error_msg(' Lack of conservative variables')
      if(.not. present(gy)) call Print_error_msg(' Lack of conservative variables')
      if(.not. present(gz)) call Print_error_msg(' Lack of conservative variables')
    end if
!------------------------------------------------------------------------------
! x-pencil : u1 -> g1 = u1_pcc * d_pcc
!------------------------------------------------------------------------------
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%d
    call Get_x_midp_C2P_3D (dens, d_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp, fbcx_4cc)
    if(iloc == IBLK .or. iloc == IALL) then
      if(itag == IQ2G) gx = qx * d_pcc_xpencil
      if(itag == IG2Q) qx = gx / d_pcc_xpencil
    end if
!------------------------------------------------------------------------------
! y-pencil : u2 -> g2 = u2_cpc * d_cpc
!------------------------------------------------------------------------------
    call transpose_x_to_y(dens, d_ccc_ypencil, dm%dccc)
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%d
    call Get_y_midp_C2P_3D (d_ccc_ypencil, d_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_c4c)
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(d_cpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    end if
    if(iloc == IBLK .or. iloc == IALL) then
      if(itag == IQ2G) then
        call transpose_x_to_y(qy, acpc_ypencil, dm%dcpc)
        acpc_ypencil = acpc_ypencil * d_cpc_ypencil
        call transpose_y_to_x(acpc_ypencil, gy, dm%dcpc)
      else if(itag == IG2Q) then
        call transpose_x_to_y(gy, acpc_ypencil, dm%dcpc)
        acpc_ypencil = acpc_ypencil / d_cpc_ypencil
        call transpose_y_to_x(acpc_ypencil, qy, dm%dcpc)
      else
      end if
    end if
!------------------------------------------------------------------------------
! Z-pencil : u3 -> g3 = u3_ccp * d_ccp
!------------------------------------------------------------------------------
    call transpose_y_to_z(d_ccc_ypencil, d_ccc_zpencil, dm%dccc)
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%d
    call Get_z_midp_C2P_3D (d_ccc_zpencil, d_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp, fbcz_cc4)
    if(iloc == IBLK .or. iloc == IALL) then
      if(itag == IQ2G) then
        call transpose_to_z_pencil(qz, accp_zpencil, dm%dccp, IPENCIL(1))
        accp_zpencil = accp_zpencil * d_ccp_zpencil
        call transpose_from_z_pencil(accp_zpencil, gz, dm%dccp, IPENCIL(1))
      else if(itag == IG2Q) then
        call transpose_to_z_pencil(gz, accp_zpencil, dm%dccp, IPENCIL(1))
        accp_zpencil = accp_zpencil / d_ccp_zpencil
        call transpose_from_z_pencil(accp_zpencil, qz, dm%dccp, IPENCIL(1))
      else
      end if
    end if

!------------------------------------------------------------------------------
! BC: - x pencil
!------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------
! BC: - y pencil
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   On a cylindrical pipe the y(1) face is the axis, not a wall. Its two ghost
!   layers are rebuilt from the mirrored field every substep by
!   update_fbcy_cc_flow_halo, and slot 3 there is the mirror of the *second*
!   node/cell, not a copy of slot 1. Converting q <-> g on that side with the
!   single first-plane density, and then copying slot 1 into slot 3, overwrites
!   both layers with the wrong values. Harmless at CD2, where the boundary row
!   multiplies the ghosts by zero; a first-order error at the axis at CD4+.
!   So leave the axis side alone and let the halo routine own it.
!------------------------------------------------------------------------------
    is_yaxis1 = (dm%icase == ICASE_PIPE .and. dm%icoordinate == ICYLINDRICAL)

    if(dm%ibcy_qx(1) == IBC_DIRICHLET .or. dm%ibcy_qx(2) == IBC_DIRICHLET) then
      call transpose_x_to_y(d_pcc_xpencil, d_pcc_ypencil, dm%dpcc)
      if(itag == IQ2G) then
        if(.not. is_yaxis1) then
        dm%fbcy_gx(:, 1, :) = dm%fbcy_qx(:, 1, :) * d_pcc_ypencil(:, 1, :)
        dm%fbcy_gx(:, 3, :) = dm%fbcy_gx(:, 1, :)
        end if
        dm%fbcy_gx(:, 2, :) = dm%fbcy_qx(:, 2, :) * d_pcc_ypencil(:, dm%dpcc%ysz(2), :)
        dm%fbcy_gx(:, 4, :) = dm%fbcy_gx(:, 2, :)
      else if(itag == IG2Q) then
        if(.not. is_yaxis1) then
        dm%fbcy_qx(:, 1, :) = dm%fbcy_gx(:, 1, :) / d_pcc_ypencil(:, 1, :)
        dm%fbcy_qx(:, 3, :) = dm%fbcy_qx(:, 1, :)
        end if
        dm%fbcy_qx(:, 2, :) = dm%fbcy_gx(:, 2, :) / d_pcc_ypencil(:, dm%dpcc%ysz(2), :)
        dm%fbcy_qx(:, 4, :) = dm%fbcy_qx(:, 2, :)
      else
      end if
    end if

    if(dm%ibcy_qy(1) == IBC_DIRICHLET .or. dm%ibcy_qy(2) == IBC_DIRICHLET) then
      if(itag == IQ2G) then
        if(.not. is_yaxis1) then
        dm%fbcy_gy(:, 1, :) = dm%fbcy_qy(:, 1, :) * d_cpc_ypencil(:, 1,              :)
        dm%fbcy_gy(:, 3, :) = dm%fbcy_gy(:, 1, :)
        end if
        dm%fbcy_gy(:, 2, :) = dm%fbcy_qy(:, 2, :) * d_cpc_ypencil(:, dm%dcpc%ysz(2), :)
        dm%fbcy_gy(:, 4, :) = dm%fbcy_gy(:, 2, :)
      else if(itag == IG2Q) then
        if(.not. is_yaxis1) then
        dm%fbcy_qy(:, 1, :) = dm%fbcy_gy(:, 1, :) / d_cpc_ypencil(:, 1,              :)
        dm%fbcy_qy(:, 3, :) = dm%fbcy_qy(:, 1, :)
        end if
        dm%fbcy_qy(:, 2, :) = dm%fbcy_gy(:, 2, :) / d_cpc_ypencil(:, dm%dcpc%ysz(2), :)
        dm%fbcy_qy(:, 4, :) = dm%fbcy_qy(:, 2, :)
      else
      end if
    end if

    if(dm%ibcy_qz(1) == IBC_DIRICHLET .or. dm%ibcy_qz(2) == IBC_DIRICHLET) then
      call transpose_z_to_y(d_ccp_zpencil, d_ccp_ypencil, dm%dccp)
      if(itag == IQ2G) then
        if(.not. is_yaxis1) then
        dm%fbcy_gz(:, 1, :) = dm%fbcy_qz(:, 1, :) * d_ccp_ypencil(:, 1,              :)
        dm%fbcy_gz(:, 3, :) = dm%fbcy_gz(:, 1, :)
        end if
        dm%fbcy_gz(:, 2, :) = dm%fbcy_qz(:, 2, :) * d_ccp_ypencil(:, dm%dccp%ysz(2), :)
        dm%fbcy_gz(:, 4, :) = dm%fbcy_gz(:, 2, :)
      else if(itag == IG2Q) then
        if(.not. is_yaxis1) then
        dm%fbcy_qz(:, 1, :) = dm%fbcy_gz(:, 1, :) / d_ccp_ypencil(:, 1,              :)
        dm%fbcy_qz(:, 3, :) = dm%fbcy_qz(:, 1, :)
        end if
        dm%fbcy_qz(:, 2, :) = dm%fbcy_gz(:, 2, :) / d_ccp_ypencil(:, dm%dccp%ysz(2), :)
        dm%fbcy_qz(:, 4, :) = dm%fbcy_qz(:, 2, :)
      else
      end if
    end if
!------------------------------------------------------------------------------
! BC: - z pencil
!------------------------------------------------------------------------------
    if(dm%ibcz_qx(1) == IBC_DIRICHLET .or. dm%ibcz_qx(2) == IBC_DIRICHLET) then
      ! dm%dpcc is mandatory here: without it the 2decomp default descriptor is
      ! the cell-centred one, which has a different x extent from dpcc.
      call transpose_x_to_y(d_pcc_xpencil, d_pcc_ypencil, dm%dpcc)
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

    if(dm%ibcz_qy(1) == IBC_DIRICHLET .or. dm%ibcz_qy(2) == IBC_DIRICHLET) then
      ! y -> z, not y -> x: dm%fbcz_qy/gy are allocated on dcpc%zsz, so the
      ! density they are scaled by has to be in the z pencil. Serially all three
      ! pencil shapes coincide and the wrong transpose is invisible; on more than
      ! one rank it writes past the end of d_cpc_zpencil.
      call transpose_y_to_z(d_cpc_ypencil, d_cpc_zpencil, dm%dcpc)
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
