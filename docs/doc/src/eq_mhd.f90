module mhd_mod
! Note: This MHD solver is potential solver only.
!       Assumed: the induced magnetic field is negligible.
  use parameters_constant_mod
  implicit none 

  private :: cross_production_mhd
  public  :: initialise_mhd
  public  :: compute_Lorentz_force
contains
!==========================================================================================================
  subroutine initialise_mhd(fl, mh, dm)
    use udf_type_mod
    use math_mod
    use mpi_mod
    use print_msg_mod
    use io_visualisation_mod
    implicit none 
    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_mhd),    intent(inout) :: mh

    if(nrank==0) call Print_debug_start_msg('Initialising MHD ...')
!----------------------------------------------------------------------------------------------------------
!   allocate variables
!----------------------------------------------------------------------------------------------------------
    call alloc_x(mh%ep, dm%dccc); mh%ep = ZERO

    call alloc_x(mh%jx, dm%dpcc); mh%jx = ZERO
    call alloc_x(mh%jy, dm%dcpc); mh%jy = ZERO
    call alloc_x(mh%jz, dm%dccp); mh%jz = ZERO

    call alloc_x(mh%bx, dm%dpcc); mh%bx = ZERO
    call alloc_x(mh%by, dm%dcpc); mh%by = ZERO
    call alloc_x(mh%bz, dm%dccp); mh%bz = ZERO

    call alloc_x(fl%lrfx, dm%dpcc); fl%lrfx = ZERO
    call alloc_x(fl%lrfy, dm%dcpc); fl%lrfy = ZERO
    call alloc_x(fl%lrfz, dm%dccp); fl%lrfz = ZERO

    allocate( mh%fbcx_jx(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_jx(dm%dpcc%ysz(1),              4, dm%dpcc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_jx(dm%dpcc%zsz(1), dm%dpcc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_jy(             4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_jy(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_jy(dm%dcpc%zsz(1), dm%dcpc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_jz(             4, dm%dccp%xsz(2), dm%dccp%xsz(3)) )! default x pencil
    allocate( mh%fbcy_jz(dm%dccp%ysz(1),              4, dm%dccp%ysz(3)) )! default y pencil
    allocate( mh%fbcz_jz(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_bx(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_bx(dm%dpcc%ysz(1),              4, dm%dpcc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_bx(dm%dpcc%zsz(1), dm%dpcc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_by(             4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_by(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_by(dm%dcpc%zsz(1), dm%dcpc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_bz(             4, dm%dccp%xsz(2), dm%dccp%xsz(3)) )! default x pencil
    allocate( mh%fbcy_bz(dm%dccp%ysz(1),              4, dm%dccp%ysz(3)) )! default y pencil
    allocate( mh%fbcz_bz(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_ep(             4, dm%dccc%xsz(2), dm%dccc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_ep(dm%dccc%ysz(1),              4, dm%dccc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_ep(dm%dccc%zsz(1), dm%dccc%zsz(2),              4) )! default z pencil

    if(mh%is_NStuart) mh%NHartmn = sqrt_wp( ONE/fl%rre * mh%NStuart)
    if(mh%is_NHartmn) mh%NStuart = mh%NHartmn * mh%NHartmn * fl%rre
!----------------------------------------------------------------------------------------------------------
!   Since B=(a, b, c) is a uniform field (constant in space), thus all derivatives of B are zero.
!   This means: the the current density introduced by this magnetic field must be zero everywhere, 
!   including at the boundaries.
!----------------------------------------------------------------------------------------------------------
    mh%bx = mh%B_static(1)
    mh%by = mh%B_static(2)
    mh%bz = mh%B_static(3)

    mh%jx = ZERO
    mh%jy = ZERO
    mh%jz = ZERO

    mh%ep = ZERO
!----------------------------------------------------------------------------------------------------------
! Boundary for static magnetic field
!----------------------------------------------------------------------------------------------------------
    mh%ibcx_bx(:) = dm%ibcx_qx(:)
    mh%ibcx_by(:) = dm%ibcx_qy(:)
    mh%ibcx_bz(:) = dm%ibcx_qz(:)
    mh%ibcy_bx(:) = dm%ibcy_qx(:)
    mh%ibcy_by(:) = dm%ibcy_qy(:)
    mh%ibcy_bz(:) = dm%ibcy_qz(:)
    mh%ibcz_bx(:) = dm%ibcz_qx(:)
    mh%ibcz_by(:) = dm%ibcz_qy(:)
    mh%ibcz_bz(:) = dm%ibcz_qz(:)

    mh%fbcx_bx(:, :, :) = mh%B_static(1)
    mh%fbcy_bx(:, :, :) = mh%B_static(1)
    mh%fbcz_bx(:, :, :) = mh%B_static(1)
    mh%fbcx_by(:, :, :) = mh%B_static(2)
    mh%fbcy_by(:, :, :) = mh%B_static(2)
    mh%fbcz_by(:, :, :) = mh%B_static(2)
    mh%fbcx_bz(:, :, :) = mh%B_static(3)
    mh%fbcy_bz(:, :, :) = mh%B_static(3)
    mh%fbcz_bz(:, :, :) = mh%B_static(3)
!----------------------------------------------------------------------------------------------------------
! Boundary for current density
!----------------------------------------------------------------------------------------------------------
    mh%ibcx_jx(:) = dm%ibcx_qx(:)
    mh%ibcx_jy(:) = dm%ibcx_qy(:)
    mh%ibcx_jz(:) = dm%ibcx_qz(:)
    mh%ibcy_jx(:) = dm%ibcy_qx(:)
    mh%ibcy_jy(:) = dm%ibcy_qy(:)
    mh%ibcy_jz(:) = dm%ibcy_qz(:)
    mh%ibcz_jx(:) = dm%ibcz_qx(:)
    mh%ibcz_jy(:) = dm%ibcz_qy(:)
    mh%ibcz_jz(:) = dm%ibcz_qz(:)

    mh%fbcx_jx(:, :, :) = ZERO
    mh%fbcy_jx(:, :, :) = ZERO
    mh%fbcz_jx(:, :, :) = ZERO
    mh%fbcx_jy(:, :, :) = ZERO
    mh%fbcy_jy(:, :, :) = ZERO
    mh%fbcz_jy(:, :, :) = ZERO
    mh%fbcx_jz(:, :, :) = ZERO
    mh%fbcy_jz(:, :, :) = ZERO
    mh%fbcz_jz(:, :, :) = ZERO
!----------------------------------------------------------------------------------------------------------
! Boundary for electrical potential
!----------------------------------------------------------------------------------------------------------
    mh%ibcx_ep(:) = dm%ibcx_pr(:)
    mh%ibcy_ep(:) = dm%ibcy_pr(:)
    mh%ibcz_ep(:) = dm%ibcz_pr(:)

    mh%fbcx_ep(:, :, :) = ZERO
    mh%fbcy_ep(:, :, :) = ZERO
    mh%fbcz_ep(:, :, :) = ZERO

    !call write_visu_mhd(mh, fl, dm, 'initial_mhd')

    if(nrank==0) call Print_debug_end_msg()
    return
  end subroutine

!==========================================================================================================
  subroutine cross_production_mhd(fl, mh, ab_cross_x, ab_cross_y, ab_cross_z, str, dm) ! to add cylindrical
    use udf_type_mod
    use operations
    use print_msg_mod
    use decomp_2d
    implicit none 
    type(t_flow), intent(in) :: fl
    type(t_mhd),  intent(in) :: mh
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent(out) :: ab_cross_x
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent(out) :: ab_cross_y
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent(out) :: ab_cross_z
    character(8), intent(in) :: str

    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: ax, bx
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: ay, by
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) :: az, bz
    integer :: ibcx_ax(2), ibcy_ax(2), ibcz_ax(2)
    integer :: ibcx_ay(2), ibcy_ay(2), ibcz_ay(2)
    integer :: ibcx_az(2), ibcy_az(2), ibcz_az(2)
    integer :: ibcx_bx(2), ibcy_bx(2), ibcz_bx(2)
    integer :: ibcx_by(2), ibcy_by(2), ibcz_by(2)
    integer :: ibcx_bz(2), ibcy_bz(2), ibcz_bz(2)
    integer :: n, iacc
    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_ax, fbcx_bx
    real(WP), dimension( 4, dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: fbcx_ay, fbcx_by
    real(WP), dimension( 4, dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: fbcx_az, fbcx_bz
    
    real(WP), dimension( dm%dpcc%ysz(1), 4, dm%dpcc%ysz(3) ) :: fbcy_ax, fbcy_bx
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_ay, fbcy_by
    real(WP), dimension( dm%dccp%ysz(1), 4, dm%dccp%ysz(3) ) :: fbcy_az, fbcy_bz

    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), 4 ) :: fbcz_ax, fbcz_bx
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4 ) :: fbcz_ay, fbcz_by
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_az, fbcz_bz

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil

    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) ::   acpc_ypencil, &
                                                                             ax_cpc_ypencil, &
                                                                             bx_cpc_ypencil, &
                                                                             az_cpc_ypencil, &
                                                                             bz_cpc_ypencil
    
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) ::   accp_zpencil, &
                                                                             ax_ccp_zpencil, &
                                                                             bx_ccp_zpencil, &
                                                                             ay_ccp_zpencil, &
                                                                             by_ccp_zpencil

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) ::   apcc_xpencil, &
                                                                             ay_pcc_xpencil, &
                                                                             by_pcc_xpencil, &
                                                                             az_pcc_xpencil, &
                                                                             bz_pcc_xpencil
    
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: apcp_xpencil
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: appc_xpencil

    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: appc_ypencil
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3) ) :: apcc_ypencil
    real(WP), dimension( dm%dpcp%ysz(1), dm%dpcp%ysz(2), dm%dpcp%ysz(3) ) :: apcp_ypencil
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: acpp_ypencil

    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: acpp_zpencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: apcp_zpencil


    iacc = dm%iAccuracy
    if(trim(str) == 'ub_cross') then
      ax = fl%qx 
      ay = fl%qy
      az = fl%qz
      ibcx_ax = dm%ibcx_qx
      ibcy_ax = dm%ibcy_qx
      ibcz_ax = dm%ibcz_qx
      ibcx_ay = dm%ibcx_qy
      ibcy_ay = dm%ibcy_qy
      ibcz_ay = dm%ibcz_qy
      ibcx_az = dm%ibcx_qz
      ibcy_az = dm%ibcy_qz
      ibcz_az = dm%ibcz_qz

      fbcx_ax = dm%fbcx_qx
      fbcy_ax = dm%fbcy_qx
      fbcz_ax = dm%fbcz_qx
      fbcx_ay = dm%fbcx_qy
      fbcy_ay = dm%fbcy_qy
      fbcz_ay = dm%fbcz_qy
      fbcx_az = dm%fbcx_qz
      fbcy_az = dm%fbcy_qz
      fbcz_az = dm%fbcz_qz
    else if(trim(str) == 'jb_cross') then
      ax = mh%jx 
      ay = mh%jy
      az = mh%jz
      ibcx_ax = mh%ibcx_jx
      ibcy_ax = mh%ibcy_jx
      ibcz_ax = mh%ibcz_jx
      ibcx_ay = mh%ibcx_jy
      ibcy_ay = mh%ibcy_jy
      ibcz_ay = mh%ibcz_jy
      ibcx_az = mh%ibcx_jz
      ibcy_az = mh%ibcy_jz
      ibcz_az = mh%ibcz_jz

      fbcx_ax = mh%fbcx_jx
      fbcy_ax = mh%fbcy_jx
      fbcz_ax = mh%fbcz_jx
      fbcx_ay = mh%fbcx_jy
      fbcy_ay = mh%fbcy_jy
      fbcz_ay = mh%fbcz_jy
      fbcx_az = mh%fbcx_jz
      fbcy_az = mh%fbcy_jz
      fbcz_az = mh%fbcz_jz
    else
      call Print_error_msg('The required cross production is not supported.')
    end if
    bx = mh%bx 
    by = mh%by
    bz = mh%bz
    ibcx_bx = mh%ibcx_bx
    ibcy_bx = mh%ibcy_bx
    ibcz_bx = mh%ibcz_bx
    ibcx_by = mh%ibcx_by
    ibcy_by = mh%ibcy_by
    ibcz_by = mh%ibcz_by
    ibcx_bz = mh%ibcx_bz
    ibcy_bz = mh%ibcy_bz
    ibcz_bz = mh%ibcz_bz
    fbcx_bx = mh%fbcx_bx
    fbcy_bx = mh%fbcy_bx
    fbcz_bx = mh%fbcz_bx
    fbcx_by = mh%fbcx_by
    fbcy_by = mh%fbcy_by
    fbcz_by = mh%fbcz_by
    fbcx_bz = mh%fbcx_bz
    fbcy_bz = mh%fbcy_bz
    fbcz_bz = mh%fbcz_bz
!----------------------------------------------------------------------------------------------------------
! preparation for u_cross_b for staggered vector 
!----------------------------------------------------------------------------------------------------------
! ax_pcc_xpencil to ax_cpc_ypencil
    apcc_xpencil = ax
    call transpose_x_to_y (apcc_xpencil, apcc_ypencil, dm%dpcc)
    call Get_y_midp_C2P_3D(apcc_ypencil, appc_ypencil, dm, iacc, ibcy_ax(:), fbcy_ax(:, :, :))
    
    call transpose_y_to_x (appc_ypencil, appc_xpencil, dm%dppc)                            
    call Get_x_midp_P2C_3D(appc_xpencil, acpc_xpencil, dm, iacc, ibcx_ax(:))
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc)
    ax_cpc_ypencil = acpc_ypencil
! ax_pcc_xpencil to ax_ccp_zpencil
    call transpose_y_to_z (apcc_ypencil, apcc_zpencil, dm%dpcc)
    call Get_z_midp_C2P_3D(apcc_zpencil, apcp_zpencil, dm, iacc, ibcz_ax(:), fbcz_ax(:, :, :))
    call transpose_z_to_y (apcp_zpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_x (apcp_ypencil, apcp_xpencil, dm%dpcp)
    call Get_x_midp_P2C_3D(apcp_xpencil, accp_xpencil, dm, iacc, ibcx_ax(:))
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    ax_ccp_zpencil = accp_zpencil

! bx_pcc_xpencil to bx_cpc_ypencil
    apcc_xpencil = bx
    call transpose_x_to_y (apcc_xpencil, apcc_ypencil, dm%dpcc)
    call Get_y_midp_C2P_3D(apcc_ypencil, appc_ypencil, dm, iacc, ibcy_bx(:), fbcy_bx(:, :, :))
    call transpose_y_to_x (appc_ypencil, appc_xpencil, dm%dppc)                            
    call Get_x_midp_P2C_3D(appc_xpencil, acpc_xpencil, dm, iacc, ibcx_bx(:))
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc)
    bx_cpc_ypencil = acpc_ypencil
! bx_pcc_xpencil to bx_ccp_zpencil
    call transpose_y_to_z (apcc_ypencil, apcc_zpencil, dm%dpcc)
    call Get_z_midp_C2P_3D(apcc_zpencil, apcp_zpencil, dm, iacc, ibcz_bx(:), fbcz_bx(:, :, :))
    call transpose_z_to_y (apcp_zpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_x (apcp_ypencil, apcp_xpencil, dm%dpcp)
    call Get_x_midp_P2C_3D(apcp_xpencil, accp_xpencil, dm, iacc, ibcx_bx(:))
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    bx_ccp_zpencil = accp_zpencil
!----------------------------------------------------------------------------------------------------------
! ay_cpc_xpencil to ay_pcc_xpencil
    acpc_xpencil = ay
    call Get_x_midp_C2P_3D(acpc_xpencil, appc_xpencil, dm, iacc, ibcx_ay(:), fbcx_ay(:, :, :))
    call transpose_x_to_y (appc_xpencil, appc_ypencil, dm%dppc) 
    call Get_y_midp_P2C_3D(appc_ypencil, apcc_ypencil, dm, iacc, ibcy_ay(:)) 
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    ay_pcc_xpencil = apcc_xpencil
! ay_cpc_xpencil to ay_ccp_zpencil
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc) 
    call transpose_y_to_z (acpc_ypencil, acpc_zpencil, dm%dcpc) 
    call Get_z_midp_C2P_3D(acpc_zpencil, acpp_zpencil, dm, iacc, ibcz_ay(:), fbcz_ay(:, :, :)) 
    call transpose_z_to_y (acpp_zpencil, acpp_ypencil, dm%dcpp)
    call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, iacc, ibcy_ay(:)) 
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    ay_ccp_zpencil = accp_zpencil

! by_cpc_xpencil to by_pcc_xpencil
    acpc_xpencil = by
    call Get_x_midp_C2P_3D(acpc_xpencil, appc_xpencil, dm, iacc, ibcx_by(:), fbcx_by(:, :, :))
    call transpose_x_to_y (appc_xpencil, appc_ypencil, dm%dppc) 
    call Get_y_midp_P2C_3D(appc_ypencil, apcc_ypencil, dm, iacc, ibcy_by(:)) 
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    by_pcc_xpencil = apcc_xpencil
! by_cpc_xpencil to by_ccp_zpencil
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc) 
    call transpose_y_to_z (acpc_ypencil, acpc_zpencil, dm%dcpc) 
    call Get_z_midp_C2P_3D(acpc_zpencil, acpp_zpencil, dm, iacc, ibcz_by(:), fbcz_by(:, :, :)) 
    call transpose_z_to_y (acpp_zpencil, acpp_ypencil, dm%dcpp)
    call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, iacc, ibcy_by(:)) 
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    by_ccp_zpencil = accp_zpencil
!----------------------------------------------------------------------------------------------------------
! az_ccp_xpencil to az_cpc_ypencil
    accp_xpencil = az
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call Get_y_midp_C2P_3D(accp_ypencil, acpp_ypencil, dm, iacc, ibcy_az(:), fbcy_az(:, :, :)) 
    call transpose_y_to_z (acpp_ypencil, acpp_zpencil, dm%dcpp)
    call Get_z_midp_P2C_3D(acpp_zpencil, acpc_zpencil, dm, iacc, ibcz_az(:))
    call transpose_z_to_y (acpc_zpencil, acpc_ypencil, dm%dcpc)
    az_cpc_ypencil = acpc_ypencil
! az_ccp_xpencil to az_pcc_xpencil
    call Get_x_midp_C2P_3D(accp_xpencil, apcp_xpencil, dm, iacc, ibcx_az(:), fbcx_az(:, :, :)) 
    call transpose_x_to_y (apcp_xpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_z (apcp_ypencil, apcp_zpencil, dm%dpcp)
    call Get_z_midp_P2C_3D(apcp_zpencil, apcc_zpencil, dm, iacc, ibcz_az(:))
    call transpose_z_to_y (apcc_zpencil, apcc_ypencil, dm%dpcc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    az_pcc_xpencil = apcc_xpencil
! bz_ccp_xpencil to bz_cpc_ypencil
    accp_xpencil = bz
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call Get_y_midp_C2P_3D(accp_ypencil, acpp_ypencil, dm, iacc, ibcy_bz(:), fbcy_bz(:, :, :)) 
    call transpose_y_to_z (acpp_ypencil, acpp_zpencil, dm%dcpp)
    call Get_z_midp_P2C_3D(acpp_zpencil, acpc_zpencil, dm, iacc, ibcz_bz(:))
    call transpose_z_to_y (acpc_zpencil, acpc_ypencil, dm%dcpc)
    bz_cpc_ypencil = acpc_ypencil
! bz_ccp_xpencil to bz_pcc_xpencil
    call Get_x_midp_C2P_3D(accp_xpencil, apcp_xpencil, dm, iacc, ibcx_bz(:), fbcx_bz(:, :, :)) 
    call transpose_x_to_y (apcp_xpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_z (apcp_ypencil, apcp_zpencil, dm%dpcp)
    call Get_z_midp_P2C_3D(apcp_zpencil, apcc_zpencil, dm, iacc, ibcz_bz(:))
    call transpose_z_to_y (apcc_zpencil, apcc_ypencil, dm%dpcc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    bz_pcc_xpencil = apcc_xpencil
!----------------------------------------------------------------------------------------------------------
! Compute the cross product of two vectors (ux, uy, uz) and (bx, by, bz)
! The resulting vector (cx, cy, cz) is given by:
! cx = uy * bz - uz * by; locates at (i', j, k); require y(cpc)->y(pcc); z(ccp)->z(pcc)
! cy = uz * bx - ux * bz; locates at (i, j', k); require x(pcc)->x(cpc); z(ccp)->z(cpc)
! cz = ux * by - uy * bx; locates at (i, j, k'); require x(pcc)->x(ccp); y(cpc)->y(ccp) 
! This follows the right-hand rule and produces a vector perpendicular to both input vectors.
!----------------------------------------------------------------------------------------------------------
    apcc_xpencil = ay_pcc_xpencil * bz_pcc_xpencil - az_pcc_xpencil * by_pcc_xpencil
    acpc_ypencil = az_cpc_ypencil * bx_cpc_ypencil - ax_cpc_ypencil * bz_cpc_ypencil
    accp_zpencil = ax_ccp_zpencil * by_ccp_zpencil - ay_ccp_zpencil * bx_ccp_zpencil
    ab_cross_x = apcc_xpencil
    call transpose_y_to_x(acpc_ypencil, ab_cross_y,   dm%dcpc)
    call transpose_z_to_y(accp_zpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_x(accp_ypencil, ab_cross_z,   dm%dccp)

    return
  end subroutine 
!==========================================================================================================
  subroutine compute_Lorentz_force(fl, mh, dm)
    use udf_type_mod
    use operations
    use decomp_2d
    use continuity_eq_mod
    use poisson_interface_mod
    use io_visualisation_mod
    implicit none
!----------------------------------------------------------------------------------------------------------
! calculate the Lozrentz-force based on a static magnetic field B, B is time-independent
!----------------------------------------------------------------------------------------------------------
    type(t_flow), intent(inout) :: fl
    type(t_mhd),  intent(inout) :: mh
    type(t_domain), intent(in)  :: dm

    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: ub_cross_x
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: ub_cross_y
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) :: ub_cross_z
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: apcc_xpencil
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: acpc_xpencil
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) :: accp_xpencil
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: accc_ypencil
    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: acpc_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: accp_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: accc_zpencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: accp_zpencil
!----------------------------------------------------------------------------------------------------------
! calculate vector u cross-product vector b in x-pencil
!----------------------------------------------------------------------------------------------------------
    ub_cross_x = ZERO
    ub_cross_y = ZERO
    ub_cross_z = ZERO
    call cross_production_mhd(fl, mh, ub_cross_x, ub_cross_y, ub_cross_z, 'ub_cross', dm)
#ifdef DEBUG_STEPS
    call write_visu_any3darray(ub_cross_x, 'ub_cross_x', 'debug', dm%dpcc, dm, fl%iteration)
    call write_visu_any3darray(ub_cross_y, 'ub_cross_y', 'debug', dm%dcpc, dm, fl%iteration)
    call write_visu_any3darray(ub_cross_z, 'ub_cross_z', 'debug', dm%dccp, dm, fl%iteration)
#endif
!----------------------------------------------------------------------------------------------------------
! calculate div(ub_cross) in x-pencil
!----------------------------------------------------------------------------------------------------------
    call Get_divergence_vector(ub_cross_x, ub_cross_y, ub_cross_z, mh%ep, dm)
#ifdef DEBUG_STEPS
    call write_visu_any3darray(mh%ep, 'ep1', 'debug', dm%dccc, dm, fl%iteration)
#endif
!----------------------------------------------------------------------------------------------------------
! solving the Poisson equation for the electric potential
!----------------------------------------------------------------------------------------------------------
    call solve_fft_poisson(mh%ep, dm)
#ifdef DEBUG_STEPS
    call write_visu_any3darray(mh%ep, 'ep2', 'debug', dm%dccc, dm, fl%iteration)
#endif
!----------------------------------------------------------------------------------------------------------
! calculate the current density jx, jy, jz (a vector)
!----------------------------------------------------------------------------------------------------------
    call Get_x_1der_C2P_3D(mh%ep, apcc_xpencil, dm, dm%iAccuracy, mh%ibcx_ep, mh%fbcx_ep )
    mh%jx = - apcc_xpencil + ub_cross_x
    call transpose_x_to_y (mh%ep, accc_ypencil, dm%dccc)
    call Get_y_1der_C2P_3D(accc_ypencil, acpc_ypencil, dm, dm%iAccuracy, mh%ibcy_ep, mh%fbcy_ep)
    call transpose_y_to_x (acpc_ypencil, acpc_xpencil, dm%dcpc)
    mh%jy = - acpc_xpencil + ub_cross_y
    call transpose_y_to_z (accc_ypencil, accc_zpencil, dm%dccc)
    call Get_z_1der_C2P_3D(accc_zpencil, accp_zpencil, dm, dm%iAccuracy, mh%ibcz_ep, mh%fbcz_ep)
    call transpose_z_to_y (accp_zpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_x (accp_ypencil, accp_xpencil, dm%dccp)
    mh%jz = - accp_xpencil + ub_cross_z
#ifdef DEBUG_STEPS
    call write_visu_any3darray(mh%jx, 'jx', 'debug', dm%dpcc, dm, fl%iteration)
    call write_visu_any3darray(mh%jy, 'jy', 'debug', dm%dcpc, dm, fl%iteration)
    call write_visu_any3darray(mh%jz, 'jz', 'debug', dm%dccp, dm, fl%iteration)
#endif
!----------------------------------------------------------------------------------------------------------
! calculate the Lorentz force lrfx, lrfy, lrfz (a vector)
!----------------------------------------------------------------------------------------------------------
    call cross_production_mhd(fl, mh, fl%lrfx, fl%lrfy, fl%lrfz, 'jb_cross', dm)
!----------------------------------------------------------------------------------------------------------
! un-dimensionlise the value
!----------------------------------------------------------------------------------------------------------
    fl%lrfx = fl%lrfx * mh%Nstuart
    fl%lrfy = fl%lrfy * mh%Nstuart
    fl%lrfz = fl%lrfz * mh%Nstuart
#ifdef DEBUG_STEPS
    call write_visu_any3darray(fl%lrfx, 'lrfx', 'debug', dm%dpcc, dm, fl%iteration)
    call write_visu_any3darray(fl%lrfy, 'lrfy', 'debug', dm%dcpc, dm, fl%iteration)
    call write_visu_any3darray(fl%lrfz, 'lrfz', 'debug', dm%dccp, dm, fl%iteration)
#endif
  return
  end subroutine

end module