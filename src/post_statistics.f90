module statistics_mod
  use print_msg_mod
  use parameters_constant_mod
  implicit none

  character(13), parameter :: io_name = "statistics-io"
  integer, allocatable :: ncl_stat(:, :)

  !private :: write_statistics_array
  !private :: read_statistics_array

  public  :: init_statistics_flow
  public  :: update_statistics_flow
  public  :: write_statistics_flow

  public  :: init_statistics_thermo
  public  :: update_statistics_thermo
  public  :: write_statistics_thermo

contains
!==========================================================================================================
!==========================================================================================================
  subroutine init_statistics_flow(fl, dm)
    use udf_type_mod
    use parameters_constant_mod
    use io_tools_mod
    use typeconvert_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    integer :: i, j, k, n, s, l, ij, sl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc

    if(nrank == 0) call Print_debug_start_msg("Initialise flow statistics ...")

    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0

      ! do i = 1, 3
      !   if(dm%is_periodic(i)) then 
      !     ncl_stat(i, dm%idom) = xszS(i)
      !   else 
      !     ncl_stat(i, dm%idom) = MAX(xszS(i) - 1, 1)
      !   end if
      ! end do
      ncl_stat(1, dm%idom) = dm%dccc%xsz(1) ! default skip is 1.
      ncl_stat(2, dm%idom) = dm%dccc%xsz(2) ! default skip is 1.
      ncl_stat(3, dm%idom) = dm%dccc%xsz(3) ! default skip is 1.
    end if

    allocate ( fl%tavg_pr        (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
    allocate ( fl%tavg_u  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
    allocate ( fl%tavg_uu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
    allocate ( fl%tavg_uuu      (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 10) )
    allocate ( fl%tavg_pur       (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
    allocate ( fl%tavg_dudu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 45) )
    fl%tavg_u   = ZERO
    fl%tavg_pr         = ZERO
    fl%tavg_uu = ZERO
    fl%tavg_uuu       = ZERO
    fl%tavg_pur        = ZERO
    fl%tavg_dudu = ZERO


    if(fl%inittype == INIT_RESTART .and. fl%iterfrom > dm%stat_istart) then
      if(nrank == 0) call Print_debug_inline_msg("Reading flow statistics ...")
      ! time mean pressure, <p>
      call read_one_3d_array(fl%tavg_pr,'t_avg_pr', dm%idom, fl%iteration, dm%dccc)
      ! time mean velocity, <up>
      do i = 1, 3
        accc = fl%tavg_u (:, :, :, i)
        call read_one_3d_array(accc, 't_avg_u'//trim(int2str(i)), dm%idom, fl%iteration, dm%dccc)
        accc = fl%tavg_uu(:, :, :, i)
        call read_one_3d_array(accc, 't_avg_pu'//trim(int2str(i)), dm%idom, fl%iteration, dm%dccc)
      end do
      ! time mean Reynolds stress <uiuj>
      n = 0
      do i = 1, 3
        do j = i, 3
          n = n + 1
          if (n <= 6) then
            accc = fl%tavg_uu(:, :, :, n)
            call read_one_3d_array(accc, 't_avg_uu'//trim(int2str(i))//trim(int2str(j)), dm%idom, fl%iteration, dm%dccc)
          end if
        end do
      end do
      ! time mean <ui*uj*uk>
      n = 0
      do i = 1, 3
        do j = i, 3
          do k = j, 3
            n = n + 1
            if(n <= 10) then
              accc = fl%tavg_uuu(:, :, :, n)
              call read_one_3d_array(accc, 't_avg_uuu'//trim(int2str(i))//trim(int2str(j))//trim(int2str(k)), &
              dm%idom, fl%iteration, dm%dccc)
            end if
          end do
        end do
      end do
      ! time mean <dui/dxj * dus/dxl>
      n = 0
      do i = 1, 3
        do j = 1, 3
          ij = (i - 1) * 3 + j
          do s = 1, 3
            do l = 1, 3
              sl = (s - 1) * 3 + l
              if (ij<=sl) then
                n = n + 1
                accc = fl%tavg_dudu(:, :, :, n)
                call read_one_3d_array(accc, &
                't_avg_dudu'//trim(int2str(i))//trim(int2str(j))//trim(int2str(s))//trim(int2str(l)), &
                dm%idom, fl%iteration, dm%dccc)
              end if
            end do
          end do
        end do
      end do
    end if

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
  subroutine init_statistics_thermo(tm, dm)
    use udf_type_mod
    use parameters_constant_mod
    use io_tools_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm

    if(.not. dm%is_thermo) return

    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0
      ncl_stat(1, dm%idom) = dm%dccc%xsz(1) ! default skip is 1.
      ncl_stat(2, dm%idom) = dm%dccc%xsz(2) ! default skip is 1.
      ncl_stat(3, dm%idom) = dm%dccc%xsz(3) ! default skip is 1.  
    end if

    if(nrank == 0) call Print_debug_start_msg("Initialise thermo statistics ...")

    allocate ( tm%tavg_T  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
    allocate ( tm%tavg_TT (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
    
    tm%tavg_T  = ZERO
    tm%tavg_TT = ZERO

    if(tm%inittype == INIT_RESTART .and. tm%iterfrom > dm%stat_istart) then
      call read_one_3d_array(tm%tavg_T,  'time_averaged_t',  dm%idom, tm%iterfrom, dm%dccc)
      call read_one_3d_array(tm%tavg_TT, 'time_averaged_tt', dm%idom, tm%iterfrom, dm%dccc)
    end if
    if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================

  subroutine update_statistics_flow(fl, dm)
    use udf_type_mod
    use parameters_constant_mod
    use operations
    use transpose_extended_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: uccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3 ) :: dudx

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
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: apcp_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: acpp_zpencil 
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil 
   

    real(WP) :: ac, am
    integer :: nstat, n, i, j, k, s, l, ij, sl

! to do: coarse mesh does not work right now due to the above allocation.
    
!----------------------------------------------------------------------------------------------------------
!   this is for a asymptotic averaging ... 
!----------------------------------------------------------------------------------------------------------
    nstat = fl%iteration - dm%stat_istart + 1
    ac = ONE / real(nstat, WP)
    am = real(nstat - 1, WP) / real(nstat, WP)
    !if(nrank==0) write(*,*) 'averaging..', fl%iteration,  dm%stat_istart, nstat, ac, am
!----------------------------------------------------------------------------------------------------------
!   pressure, stored in cell centre
!----------------------------------------------------------------------------------------------------------
    fl%tavg_pr(:, :, :) = am * fl%tavg_pr(:, :, :) + ac * fl%pres(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   ux
!----------------------------------------------------------------------------------------------------------
    call Get_x_midp_P2C_3D(fl%qx, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    uccc(:, :, :, 1) = accc_xpencil(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   uy
!----------------------------------------------------------------------------------------------------------
    call transpose_x_to_y(fl%qy, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy(:), dm%fbcy_qy)
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    uccc(:, :, :, 2) = accc_xpencil(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   uz
!----------------------------------------------------------------------------------------------------------
    call transpose_to_z_pencil(fl%qz, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz(:), dm%fbcz_qz)
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    uccc(:, :, :, 3) = accc_xpencil(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   Time averaging of u, v, w
!----------------------------------------------------------------------------------------------------------
    do i = 1, 3
      fl%tavg_u(:, :, :, i) = am * fl%tavg_u(:, :, :, i) + &
                               ac * uccc(:, :, :, i)
    end do
!----------------------------------------------------------------------------------------------------------
!   tensor, uu, vv, ww, uv, uw, vw, x-pencil, stored in cell centre
!----------------------------------------------------------------------------------------------------------
    n = 0
    do i = 1, 3
      do j = i, 3
        n = n + 1
        if (n <= 6) &
        fl%tavg_uu(:, :, :, n) = am * fl%tavg_uu(:, :, :, n) + &
                                  ac * uccc(:, :, :, i) * uccc(:, :, :, j)
      end do
    end do
!----------------------------------------------------------------------------------------------------------
!   up, vp, wp
!----------------------------------------------------------------------------------------------------------
    do i = 1, 3
      fl%tavg_pur(:, :, :, i) = am * fl%tavg_pur(:, :, :, i) + &
                                ac * uccc(:, :, :, i) * fl%pres(:, :, :)
    end do
!----------------------------------------------------------------------------------------------------------
!(1,1,1); (1,1,2); (1,1,3); (1,2,2); (1,2,3); index(1-5)
!(1,3,3); (2,2,2); (2,2,3); (2,3,3); (3,3,3); index(6-10)
!----------------------------------------------------------------------------------------------------------
    n = 0
    do i = 1, 3
      do j = i, 3
        do k = j, 3
          n = n + 1
          if(n <= 10)&
          fl%tavg_uuu(:, :, :, n) = am * fl%tavg_uuu(:, :, :, n) + &
                                     ac * uccc(:, :, :, i) * uccc(:, :, :, j) * uccc(:, :, :, k)
        end do
      end do
    end do
!----------------------------------------------------------------------------------------------------------
!   du/dx, du/dy, du/dz
!----------------------------------------------------------------------------------------------------------
    call Get_x_1der_P2C_3D(fl%qx, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_qx)
    dudx(:, :, :, 1, 1) = accc_xpencil(:, :, :)
    call transpose_x_to_y(fl%qx, apcc_ypencil, dm%dpcc)
    call Get_y_1der_C2P_3D(apcc_ypencil, appc_ypencil, dm, dm%iAccuracy, dm%ibcy_qx, dm%fbcy_qx)
    call Get_y_midp_P2C_3D(appc_ypencil, apcc_ypencil, dm, dm%iAccuracy, dm%ibcy_qx) ! should be BC of du/dy
    call transpose_y_to_x(apcc_ypencil, apcc_xpencil, dm%dpcc)
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx) ! should be BC of du/dy
    dudx(:, :, :, 1, 2) = accc_xpencil(:, :, :)
    call transpose_to_z_pencil(fl%qx, apcc_zpencil, dm%dpcc, IPENCIL(1))
    call Get_z_1der_C2P_3D(apcc_zpencil, apcp_zpencil, dm, dm%iAccuracy, dm%ibcz_qx, dm%fbcz_qx)
    call Get_z_midp_P2C_3D(apcp_zpencil, apcc_zpencil, dm, dm%iAccuracy, dm%ibcz_qx) ! should be BC of du/dz
    call transpose_from_z_pencil(apcc_zpencil, apcc_xpencil, dm%dccc, IPENCIL(1))
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx) ! should be BC of du/dz
    dudx(:, :, :, 1, 3) = accc_xpencil(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   dv/dx, dv/dy, dv/dz
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
!   dw/dx, dw/dy, dw/dz
!----------------------------------------------------------------------------------------------------------
    call Get_x_1der_C2P_3D(fl%qz, apcp_xpencil, dm, dm%iAccuracy, dm%ibcx_qz, dm%fbcx_qz)
    call Get_x_midp_P2C_3D(apcp_xpencil, accp_xpencil, dm, dm%iAccuracy, dm%ibcx_qz) ! should be BC of dv/dx
    call transpose_to_z_pencil(accp_xpencil, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz) ! should be BC of dv/dy
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    dudx(:, :, :, 3, 1) = accc_xpencil(:, :, :)
    call transpose_x_to_y(fl%qz, accp_ypencil, dm%dccp)
    call Get_y_1der_C2P_3D(accp_ypencil, acpp_ypencil, dm, dm%iAccuracy, dm%ibcy_qz, dm%fbcy_qz)
    call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, dm%iAccuracy, dm%ibcy_qx) ! should be BC of du/dy
    call transpose_to_z_pencil(accp_ypencil, accp_zpencil, dm%dccp, IPENCIL(2))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz) ! should be BC of dv/dy
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    dudx(:, :, :, 3, 2) = accc_xpencil(:, :, :)
    call transpose_to_z_pencil(fl%qy, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_1der_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    dudx(:, :, :, 3, 3) = accc_xpencil(:, :, :)
!----------------------------------------------------------------------------------------------------------
!(1,1,1,1); (1,1,1,2); (1,1,1,3); (1,1,2,1); (1,1,2,2); (1,1,2,3); (1,1,3,1); (1,1,3,2); (1,1,3,3); index (01-09)
!(1,2,1,2); (1,2,1,3); (1,2,2,1); (1,2,2,2); (1,2,2,3); (1,2,3,1); (1,2,3,2); (1,2,3,3); (1,3,1,3); index (10-18)
!(1,3,2,1); (1,3,2,2); (1,3,2,3); (1,3,3,1); (1,3,3,2); (1,3,3,3); (2,1,2,1); (2,1,2,2); (2,1,2,3); index (19-27)
!(2,1,3,1); (2,1,3,2); (2,1,3,3); (2,2,2,2); (2,2,2,3); (2,2,3,1); (2,2,3,2); (2,2,3,3); (2,3,2,3); index (28-36)
!(2,3,3,1); (2,3,3,2); (2,3,3,3); (3,1,3,1); (3,1,3,2); (3,1,3,3); (3,2,3,2); (3,2,3,3); (3,3,3,3); index (37-45)  
!----------------------------------------------------------------------------------------------------------
    n = 0
    do i = 1, 3
      do j = 1, 3
        ij = (i - 1) * 3 + j
        do s = 1, 3
          do l = 1, 3
            sl = (s - 1) * 3 + l
            if (ij<=sl) then
              n = n + 1
              fl%tavg_dudu(:, :, :, n) = am * fl%tavg_dudu(:, :, :, n) + &
                                               ac * dudx(:, :, :, i, j) * dudx(:, :, :, s, l)
            end if
          end do
        end do
      end do
    end do

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_statistics_flow(fl, dm)
    use io_tools_mod
    use udf_type_mod
    use typeconvert_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl
    integer :: i, j, k, s, l, ij, sl, n
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc

    ! here is not only a repeat of those in io_visualisation
    ! because they have different written freqence and to be used for restart as well.
    if(nrank == 0) call Print_debug_inline_msg("Writing flow statistics ...")
    ! time mean pressure, <p>
    call write_one_3d_array(fl%tavg_pr, 't_avg_pr', dm%idom, fl%iteration, dm%dccc)
    ! time mean velocity, <up>
    do i = 1, 3
      accc = fl%tavg_u (:, :, :, i)
      call write_one_3d_array(accc, 't_avg_u'//trim(int2str(i)), dm%idom, fl%iteration, dm%dccc)
      accc = fl%tavg_pur(:, :, :, i)
      call write_one_3d_array(accc, 't_avg_pu'//trim(int2str(i)), dm%idom, fl%iteration, dm%dccc)
    end do
    ! time mean Reynolds stress <uiuj>
    n = 0
    do i = 1, 3
      do j = i, 3
        n = n + 1
        if (n <= 6) then
          accc = fl%tavg_uu(:, :, :, n)
          call write_one_3d_array(accc, 't_avg_uu'//trim(int2str(i))//trim(int2str(j)), dm%idom, fl%iteration, dm%dccc)
        end if
      end do
    end do
    ! time mean <ui*uj*uk>
    n = 0
    do i = 1, 3
      do j = i, 3
        do k = j, 3
          n = n + 1
          if( n <= 10) then
            accc = fl%tavg_uuu(:, :, :, n)
            call write_one_3d_array(accc, 't_avg_uuu'//trim(int2str(i))//trim(int2str(j))//trim(int2str(k)), &
            dm%idom, fl%iteration, dm%dccc)
          end if
        end do
      end do
    end do
    ! time mean <dui/dxj * dus/dxl>
    n = 0
    do i = 1, 3
      do j = 1, 3
        ij = (i - 1) * 3 + j
        do s = 1, 3
          do l = 1, 3
            sl = (s - 1) * 3 + l
            if (ij<=sl) then
              n = n + 1
              accc = fl%tavg_dudu(:, :, :, n)
              call write_one_3d_array(accc, &
              't_avg_dudu'//trim(int2str(i))//trim(int2str(j))//trim(int2str(s))//trim(int2str(l)), &
              dm%idom, fl%iteration, dm%dccc)
            end if
          end do
        end do
      end do
    end do
  
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine update_statistics_thermo(tm, dm)
    use udf_type_mod
    use parameters_constant_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    real(WP) :: ac, am
    integer :: nstat

! to do: coarse mesh does not work right now due to the above allocation.
    
!----------------------------------------------------------------------------------------------------------
!   this is for a asymptotic averaging ... 
!----------------------------------------------------------------------------------------------------------
    nstat = tm%iteration - dm%stat_istart + 1
    ac = ONE / real(nstat, WP)
    am = real(nstat - 1, WP) / real(nstat, WP)
!----------------------------------------------------------------------------------------------------------
!   temperature
!----------------------------------------------------------------------------------------------------------
    tm%tavg_T (:, :, :) = am * tm%tavg_T(:, :, :)  + ac * tm%tTemp(:, :, :)
    tm%tavg_TT(:, :, :) = am * tm%tavg_TT(:, :, :) + ac * tm%tTemp(:, :, :) * tm%tTemp(:, :, :)

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_statistics_thermo(tm, dm)
    use udf_type_mod
    use io_tools_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm

    if(nrank == 0) call Print_debug_inline_msg("Writing thermo statistics ...")
    call write_one_3d_array(tm%tavg_T,  'time_averaged_t', dm%idom, tm%iteration, dm%dccc)
    call write_one_3d_array(tm%tavg_TT, 'time_averaged_tt', dm%idom, tm%iteration, dm%dccc)


    return
  end subroutine

end module
