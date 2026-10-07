!==========================================================================================================
!> Cell-centred velocity and physical velocity-gradient tensor.
!>
!> Both the turbulence statistics (statistics_mod) and the LES subgrid models
!> (les_mod) need the same object: u_i and du_i/dx_j interpolated to the cell
!> centre, expressed in *physical* components. Keeping one copy here avoids the
!> two implementations drifting apart, as they had done: the LES copy carried
!> only the Cartesian assembly and silently returned the wrong tensor in a pipe.
!==========================================================================================================
module flow_gradient_mod
  use precision_mod, only: WP
  use udf_type_mod,  only: t_domain, t_flow
  implicit none
  private

  public :: get_velocity_and_gradient_ccc

contains
!==========================================================================================================
  !> Build the cell-centred velocity and the physical velocity-gradient tensor.
  !>
  !> In a Cartesian domain the component/direction order is (x, y, z). In a
  !> cylindrical domain it is (x, r, theta) and the returned tensor is the
  !> *physical* gradient, i.e.
  !>    L_xx = du_x/dx      L_xr = du_x/dr      L_xth = (1/r) du_x/dth
  !>    L_rx = du_r/dx      L_rr = du_r/dr      L_rth = (1/r) du_r/dth - u_th/r
  !>    L_thx = du_th/dx    L_thr = du_th/dr    L_thth = (1/r) du_th/dth + u_r/r
  !> not the derivatives of the stored variables (qy = r*u_r, qz = u_theta).
  !>
  !> - fl: flow state, read only.
  !> - dm: domain/decomposition and boundary-condition metadata.
  !> - dudx: output tensor, index order (i velocity component, j derivative direction).
  !> - opt_uccc: optional cell-centred physical velocity u_i. It is computed
  !>   unconditionally in cylindrical coordinates because the metric terms above
  !>   need it, so requesting it there is free.
  subroutine get_velocity_and_gradient_ccc(fl, dm, dudx, opt_uccc)
    use boundary_conditions_mod
    use cylindrical_rn_mod
    use operations
    use parameters_constant_mod
    use transpose_extended_mod
    implicit none
    type(t_flow),   intent(in)  :: fl
    type(t_domain), intent(in)  :: dm
    real(WP),       intent(out) :: dudx(:, :, :, :, :)
    real(WP),       intent(out), optional :: opt_uccc(:, :, :, :)
    !
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: uccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: appc_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_xpencil
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: apcp_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3) ) :: apcc_ypencil
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: appc_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: acpp_ypencil
    real(WP), dimension( dm%dppc%ysz(1), 4, dm%dppc%ysz(3) ) :: fbcy_p4c
    real(WP), dimension( dm%dcpp%ysz(1), 4, dm%dcpp%ysz(3) ) :: fbcy_c4p
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: apcp_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: acpp_zpencil
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil
    real(WP), dimension( 4, dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: fbcx_qyr
    integer :: j, k, jj
    logical :: is_uccc_needed
    !
    is_uccc_needed = present(opt_uccc) .or. (dm%icoordinate == ICYLINDRICAL)
!------------------------------------------------------------------------------
!   preparation for u_i
!------------------------------------------------------------------------------
    if(is_uccc_needed) then
    ! u1
    apcc_xpencil = fl%qx
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    uccc(:, :, :, 1) = accc_xpencil(:, :, :)
    ! u2
    acpc_xpencil = fl%qy
    call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy(:), dm%fbcy_qy)
    if(dm%icoordinate == ICYLINDRICAL) then
      do k = 1, dm%dccc%ysz(3)
        do j = 1, dm%dccc%ysz(2)
          jj = dm%dccc%yst(2) + j - 1
          accc_ypencil(:, j, k) = accc_ypencil(:, j, k) * dm%rci(jj)
        end do
      end do
    end if
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    uccc(:, :, :, 2) = accc_xpencil(:, :, :)
    ! u3
    accp_xpencil = fl%qz
    call transpose_to_z_pencil(accp_xpencil, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz(:), dm%fbcz_qz)
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    uccc(:, :, :, 3) = accc_xpencil(:, :, :)
    end if
!------------------------------------------------------------------------------
!   preparation for du_i/dx_j
!------------------------------------------------------------------------------
    ! du/dx, du/dy, du/dz
    call Get_x_1der_P2C_3D(fl%qx, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_qx)
    dudx(:, :, :, 1, 1) = accc_xpencil(:, :, :)
    call transpose_x_to_y(fl%qx, apcc_ypencil, dm%dpcc)
    call Get_y_1der_C2P_3D(apcc_ypencil, appc_ypencil, dm, dm%iAccuracy, dm%ibcy_qx, dm%fbcy_qx)
    fbcy_p4c = MAXP
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(appc_ypencil, IPENCIL(2), fbcy_p4c, dm%knc_sym, dm%dppc, is_ynode = .true., is_odd = .true., &
                            axis_mode = AXIS_RECON_M1, assign_axis_to_var = .true., nr = 0, opt_dz = dm%h(3))
    end if
    call Get_y_midp_P2C_3D(appc_ypencil, apcc_ypencil, dm, dm%iAccuracy, dm%ibcy_qx) ! should be BC of du/dy
    call transpose_y_to_x(apcc_ypencil, apcc_xpencil, dm%dpcc)
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx) ! should be BC of du/dy
    dudx(:, :, :, 1, 2) = accc_xpencil(:, :, :)
    call transpose_to_z_pencil(fl%qx, apcc_zpencil, dm%dpcc, IPENCIL(1))
    call Get_z_1der_C2P_3D(apcc_zpencil, apcp_zpencil, dm, dm%iAccuracy, dm%ibcz_qx, dm%fbcz_qx)
    call Get_z_midp_P2C_3D(apcp_zpencil, apcc_zpencil, dm, dm%iAccuracy, dm%ibcz_qx) ! should be BC of du/dz
    call transpose_from_z_pencil(apcc_zpencil, apcc_xpencil, dm%dpcc, IPENCIL(1))
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx) ! should be BC of du/dz
    dudx(:, :, :, 1, 3) = accc_xpencil(:, :, :)
    ! dv/dx, dv/dy, dv/dz
    call Get_x_1der_C2P_3D(fl%qy, appc_xpencil, dm, dm%iAccuracy, dm%ibcx_qy, dm%fbcx_qy)
    call Get_x_midp_P2C_3D(appc_xpencil, acpc_xpencil, dm, dm%iAccuracy, dm%ibcx_qy) ! should be BC of dv/dx
    call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy) ! should be BC of dv/dy
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    dudx(:, :, :, 2, 1) = accc_xpencil(:, :, :)
    call transpose_x_to_y(fl%qy, acpc_ypencil, dm%dcpc)
    call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    dudx(:, :, :, 2, 2) = accc_xpencil(:, :, :)
    call transpose_to_z_pencil(fl%qy, acpc_zpencil, dm%dcpc, IPENCIL(1))
    call Get_z_1der_C2P_3D(acpc_zpencil, acpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_qy)
    call Get_z_midp_P2C_3D(acpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, dm%ibcz_qy) ! should be BC of dv/dz
    call transpose_z_to_y(acpc_zpencil, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy) ! should be BC of dv/dz
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    dudx(:, :, :, 2, 3) = accc_xpencil(:, :, :)
    ! dw/dx, dw/dy, dw/dz
    call Get_x_1der_C2P_3D(fl%qz, apcp_xpencil, dm, dm%iAccuracy, dm%ibcx_qz, dm%fbcx_qz)
    call Get_x_midp_P2C_3D(apcp_xpencil, accp_xpencil, dm, dm%iAccuracy, dm%ibcx_qz) ! should be BC of dv/dx
    call transpose_to_z_pencil(accp_xpencil, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz) ! should be BC of dv/dy
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    dudx(:, :, :, 3, 1) = accc_xpencil(:, :, :)
    call transpose_x_to_y(fl%qz, accp_ypencil, dm%dccp)
    call Get_y_1der_C2P_3D(accp_ypencil, acpp_ypencil, dm, dm%iAccuracy, dm%ibcy_qz, dm%fbcy_qz)
    fbcy_c4p = MAXP
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(acpp_ypencil, IPENCIL(2), fbcy_c4p, dm%knc_sym, dm%dcpp, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0_M2, assign_axis_to_var = .true., nr = 0, opt_dz = dm%h(3))
    end if
    call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, dm%ibcy_qz) ! should be BC of du/dy
    call transpose_to_z_pencil(accp_ypencil, accp_zpencil, dm%dccp, IPENCIL(2))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz) ! should be BC of dv/dy
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    dudx(:, :, :, 3, 2) = accc_xpencil(:, :, :)
    call transpose_to_z_pencil(fl%qz, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_1der_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    dudx(:, :, :, 3, 3) = accc_xpencil(:, :, :)

    if(dm%icoordinate == ICYLINDRICAL) then
      ! Convert the raw derivative assembly to the physical cylindrical
      ! gradient tensor for components (u_x, u_r, u_theta) and directions
      ! (x, r, theta). qy is stored as r*u_r; qz is already u_theta.

      ! Rebuild all radial-velocity derivatives from qyr = qy/r.
      fbcx_qyr = dm%fbcx_qy
      do k = 1, dm%dcpc%xsz(3)
        do j = 1, dm%dcpc%xsz(2)
          jj = dm%dcpc%xst(2) + j - 1
          fbcx_qyr(:, j, k) = fbcx_qyr(:, j, k) * dm%rpi(jj)
        end do
      end do

      acpc_xpencil = fl%qy
      call multiple_cylindrical_rn(acpc_xpencil, dm%dcpc, dm%rpi, 1, IPENCIL(1))
      call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)

      call Get_x_1der_C2P_3D(acpc_xpencil, appc_xpencil, dm, dm%iAccuracy, dm%ibcx_qy, fbcx_qyr)
      call Get_x_midp_P2C_3D(appc_xpencil, acpc_xpencil, dm, dm%iAccuracy, dm%ibcx_qy)
      call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)
      call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qyr)
      call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
      dudx(:, :, :, 2, 1) = accc_xpencil(:, :, :)

      call transpose_x_to_y(fl%qy, acpc_ypencil, dm%dcpc)
      call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rpi, 1, IPENCIL(2))
      call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qyr)
      call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
      dudx(:, :, :, 2, 2) = accc_xpencil(:, :, :)

      call transpose_y_to_z(acpc_ypencil, acpc_zpencil, dm%dcpc)
      call Get_z_1der_C2P_3D(acpc_zpencil, acpp_zpencil, dm, dm%iAccuracy, dm%ibcz_qy, dm%fbcz_qyr)
      call Get_z_midp_P2C_3D(acpp_zpencil, acpc_zpencil, dm, dm%iAccuracy, dm%ibcz_qy)
      call transpose_z_to_y(acpc_zpencil, acpc_ypencil, dm%dcpc)
      call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qyr)
      call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
      dudx(:, :, :, 2, 3) = accc_xpencil(:, :, :)

      do k = 1, dm%dccc%xsz(3)
        do j = 1, dm%dccc%xsz(2)
          jj = dm%dccc%xst(2) + j - 1
          dudx(:, j, k, 1, 3) = dudx(:, j, k, 1, 3) * dm%rci(jj)
          dudx(:, j, k, 2, 3) = dudx(:, j, k, 2, 3) * dm%rci(jj) - uccc(:, j, k, 3) * dm%rci(jj)
          dudx(:, j, k, 3, 3) = dudx(:, j, k, 3, 3) * dm%rci(jj) + uccc(:, j, k, 2) * dm%rci(jj)
        end do
      end do
    end if

    if(present(opt_uccc)) opt_uccc(:, :, :, :) = uccc(:, :, :, :)

    return
  end subroutine get_velocity_and_gradient_ccc
!==========================================================================================================
end module flow_gradient_mod
