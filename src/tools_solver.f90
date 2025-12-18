module solver_tools_mod
  implicit none
  ! procedure
  private
  public  :: Check_cfl_convection
  public  :: Check_cfl_diffusion
  !
  public  :: Update_Re
  public  :: Update_PrGr
  public  :: Calculate_xz_mean_yprofile
  public  :: Adjust_to_xzmean_zero
  !public  :: Get_volumetric_average_3d ! not used anymore
  public  :: get_fbcx_ftp_4pc
  !
  public :: check_global_mass_balance
  !public :: check_global_energy_balance

  public :: damping_drhodt
contains
!==========================================================================================================
!> \brief The main code for initialising flow variables
!> This subroutine is called once in \ref initialise_chapsim.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]  
!==========================================================================================================
  subroutine Update_Re(iter, fl)
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    integer,     intent(in ) :: iter  
    type(t_flow),   intent(inout) :: fl
  !----------------------------------------------------------------------------------------------------------
  !  1/Re                                   
  !----------------------------------------------------------------------------------------------------------
    if(iter < fl%initReTo) then
      fl%rre = ONE / fl%reninit
    else
      fl%rre = ONE / fl%ren
    end if

    return
  end subroutine Update_Re

  subroutine Update_PrGr(fl, tm)
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    

    real(WP) :: u0, rtmp
  
!----------------------------------------------------------------------------------------------------------
!  1/(Re*Pr)                                   
!----------------------------------------------------------------------------------------------------------
    tm%rPrRen = fl%rre * fluidparam%ftp0ref%k / fluidparam%ftp0ref%m / fluidparam%ftp0ref%cp
!----------------------------------------------------------------------------------------------------------
!  gravity force                          
!----------------------------------------------------------------------------------------------------------  
    u0 = ONE / fl%rre * fluidparam%ftp0ref%m / fluidparam%ftp0ref%d / tm%ref_l0
    rtmp = tm%ref_l0 / u0 / u0 * GRAVITY
    fl%fgravity = ZERO
    if (fl%igravity == 1 ) then ! flow/gravity same dirction - x
      fl%fgravity(1) =  rtmp
    else if (fl%igravity == 2 ) then ! flow/gravity same dirction - y
      fl%fgravity(2) =  rtmp
    else if (fl%igravity == 3 ) then ! flow/gravity same dirction - z
      fl%fgravity(3) =  rtmp
    else if (fl%igravity == -1 ) then ! flow/gravity opposite dirction - x
      fl%fgravity(1) =  - rtmp
    else if (fl%igravity == -2 ) then ! flow/gravity opposite dirction - y
      fl%fgravity(2) =  - rtmp
    else if (fl%igravity == -3 ) then ! flow/gravity opposite dirction - z
      fl%fgravity(3) =  - rtmp
    else ! no gravity
      fl%fgravity = ZERO
    end if
    
    return
  end subroutine Update_PrGr
!==========================================================================================================
!> \brief The main code for initialising flow variables
!>
!> not changing storage position, exclude b.c. values, for example, developing
!> flow.
!> MPI : x-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]  none          NA
!==========================================================================================================
  subroutine Calculate_xz_mean_yprofile(var, dtmp, n, varxz_work1)
    use mpi_mod
    use udf_type_mod
    use parameters_constant_mod
    use io_files_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(in)  :: var ! x-pencil default
    integer,  intent(in)  :: n
    real(WP), dimension(n), optional, intent(out) :: varxz_work1

    real(wp) :: varxz( n )
    integer :: jj, i, j, k
    integer :: nk, ni!, nk_work, ni_work
    real(WP) :: varxz_work(n)
    !----------------------------------------------------------------------------------------------------------
    !   Default X-pencil
    !----------------------------------------------------------------------------------------------------------
    varxz = ZERO
    varxz_work = ZERO
    do j = 1, dtmp%xsz(2)
      nk = 0
      ni = 0
      jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
      do k = 1, dtmp%xsz(3)
        nk = nk + 1
        do i = 1, dtmp%xsz(1)
          ni = ni + 1
          varxz(jj) = varxz(jj) + var(i, j, k) !
        end do
      end do
      varxz(jj) = varxz(jj) / real(nk * ni, wp)
    end do
    

    !call mpi_barrier(MPI_COMM_WORLD, ierror)
    !call mpi_allreduce(ni, ni_work, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
    !call mpi_allreduce(nk, nk_work, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
    call mpi_allreduce(varxz, varxz_work, n, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
    varxz_work = varxz_work / real(p_col * p_col, wp)
    if(PRESENT(varxz_work1)) varxz_work1 = varxz_work

#ifdef DEBUG_STEPS
    if (nrank == 0) then
      open(121, file = trim(dir_chkp)//'/check_calculate_xz_mean_yprofile.dat', position="append")
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
        write(121, *) jj, varxz_work(jj)
      end do
    end if
#endif
    
    
    return
  end subroutine
!==========================================================================================================
!> \brief : 
!> MPI : x-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]            
!==========================================================================================================
  subroutine Adjust_to_xzmean_zero(var, dtmp, n, varxz)
    use mpi_mod
    use udf_type_mod
    use io_files_mod
    implicit none
    type(DECOMP_INFO),  intent(in) :: dtmp
    integer,            intent(in) :: n
    real(WP), dimension(n), intent(in) :: varxz
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: var
    
    integer :: jj, i, j, k

    do j = 1, dtmp%xsz(2)
      jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
      do k = 1, dtmp%xsz(3)
        do i = 1, dtmp%xsz(1)
          var(:, j, :) = var(:, j, :) - varxz(jj)
        end do
      end do
    end do

#ifdef DEBUG_STEPS
    open(121, file = trim(dir_chkp)//'/check_adjust_to_xzmean_zero.dat', position="append")
    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        do i = 1, dtmp%xsz(1)
          write(121, *) k, j, i, var(i, j, k)
        end do
      end do
    end do
    close(121)
#endif

    return
  end subroutine
!==========================================================================================================
!> \brief : 
!> MPI : x-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]         
!==========================================================================================================
  subroutine Check_cfl_diffusion(fl, dm)
    use parameters_constant_mod
    use udf_type_mod
    use mpi_mod
    use wtformat_mod
    use print_msg_mod
    
    implicit none
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(in) :: dm

    real(WP) :: cfl_diff, cfl_diff_work, rtmp, dyi, dtmax, dtmax_work
    integer :: i, j, k, jj
    real(wp) :: rsp(3), rmax(3), rmax_work(3), var(5), var_work(5)
    
    rmax(:) = ZERO
    cfl_diff = ZERO

    rsp(1) = dm%h2r(1)
    rsp(2) = dm%h2r(2)
    rsp(3) = dm%h2r(3)
    do j = 1, dm%dccc%xsz(2)
      jj = dm%dccc%xst(2) + j - 1 !local2global_yid(j, dm%dccc)
      if(dm%is_stretching(2)) then
        dyi = dm%yMappingcc(jj, 1) / dm%h(2)
        rsp(2) = dyi * dyi
      end if
      do k = 1, dm%dccc%xsz(3)
        if(dm%icoordinate == ICYLINDRICAL) &
        rsp(3) = dm%h2r(3) * dm%rci(jj) * dm%rci(jj)
        do i = 1, dm%dccc%xsz(1)
          rtmp = rsp(1) + rsp(2) + rsp(3)
          if(dm%is_thermo) rtmp = rtmp * fl%mVisc(i, j, k) / fl%dDens(i, j, k)
          if(rtmp > cfl_diff) then
            cfl_diff = rtmp
            rmax(:) = rsp(:)
          end if
        end do
      end do
    end do 

    dtmax = ONE/(TWO*fl%rre * cfl_diff)
    cfl_diff = cfl_diff * TWO * dm%dt * fl%rre

    !call mpi_barrier(MPI_COMM_WORLD, ierror)
    var(1:3) = rmax(1:3)
    var(4) = dtmax
    var(5) = cfl_diff
    call mpi_allreduce(var, var_work, 5, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    rmax_work(1:3) = var(1:3)
    dtmax_work = var(4)
    cfl_diff_work = var(5)

    if(nrank == 0) then
      write (*, wrtfmt1el) "Diffusion number :", cfl_diff_work
      if(cfl_diff_work > ONE) then 
        call Print_warning_msg("Warning: Diffusion number is larger than 1. Numerical instability could occur.")
        write(*,*) 'Please reduce the time step size lower than ', dtmax_work
        write(*,*) 'Or Please consider increase your mesh size'
        write(*,*) '1/delta^2 Contributes from x, y, z directions:', rmax_work(1:3)
      end if
    end if
    
    return
  end subroutine
!==========================================================================================================
!> \brief : to check CFL for convection terms
!> CFL = u^x/dx + v^y/dy + w^z/dz < limit
!> MPI : x-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]         
!==========================================================================================================
  subroutine Check_cfl_convection(u, v, w, dm, opt_cfl)
    use parameters_constant_mod
    use udf_type_mod
    use operations
    use decomp_2d
    use wtformat_mod
    use find_max_min_ave_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    real(WP), intent(out), optional :: opt_cfl
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent(in) :: u
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent(in) :: v
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent(in) :: w

    real(WP) :: var_xpencil (dm%dccc%xsz(1), &
                             dm%dccc%xsz(2), &
                             dm%dccc%xsz(3))
    real(WP) :: var_ypencil (dm%dccc%ysz(1), &
                             dm%dccc%ysz(2), &
                             dm%dccc%ysz(3))
    real(WP) :: var_zpencil (dm%dccc%zsz(1), &
                             dm%dccc%zsz(2), &
                             dm%dccc%zsz(3))
    real(WP) :: accc_xpencil (dm%dccc%xsz(1), &
                             dm%dccc%xsz(2), &
                             dm%dccc%xsz(3))
    real(WP) :: accc_ypencil (dm%dccc%ysz(1), &
                             dm%dccc%ysz(2), &
                             dm%dccc%ysz(3))
    real(WP) :: accc_zpencil (dm%dccc%zsz(1), &
                             dm%dccc%zsz(2), &
                             dm%dccc%zsz(3))
    real(WP) ::   v_ypencil (dm%dcpc%ysz(1), &
                             dm%dcpc%ysz(2), &
                             dm%dcpc%ysz(3))
    real(WP) ::   w_ypencil (dm%dccp%ysz(1), &
                             dm%dccp%ysz(2), &
                             dm%dccp%ysz(3))
    real(WP) ::   w_zpencil (dm%dccp%zsz(1), &
                             dm%dccp%zsz(2), &
                             dm%dccp%zsz(3))
    !real(WP)   :: cfl_convection, cfl_convection_work
    real(wp) :: cfl(2), dy
    integer :: j
!----------------------------------------------------------------------------------------------------------
! Initialisation
!----------------------------------------------------------------------------------------------------------
    var_xpencil = ZERO
    var_ypencil = ZERO
    var_zpencil = ZERO
    accc_xpencil = ZERO
    accc_ypencil = ZERO
    accc_zpencil = ZERO
!----------------------------------------------------------------------------------------------------------
! X-pencil : u_ccc / dx * dt
!----------------------------------------------------------------------------------------------------------
    call Get_x_midp_P2C_3D(u, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_qx)
    var_xpencil = accc_xpencil * dm%h1r(1) * dm%dt
!----------------------------------------------------------------------------------------------------------
! Y-pencil : v_ccc / dy / r * dt
!----------------------------------------------------------------------------------------------------------
    call transpose_x_to_y(var_xpencil, var_ypencil, dm%dccc)
    call transpose_x_to_y(v,             v_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(v_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
    accc_ypencil = accc_ypencil * dm%h1r(2) * dm%dt
    if(dm%is_stretching(2)) then
      do j = 1, dm%dccc%ysz(2)
        accc_ypencil(:, j, :) = accc_ypencil(:, j, :) * dm%yMappingcc(j, 1)
      end do 
    end if
    if(dm%icoordinate == ICYLINDRICAL) then
      do j = 1, dm%dccc%ysz(2)
        accc_ypencil(:, j, :) = accc_ypencil(:, j, :) * dm%rci(j) 
      end do 
    end if
    var_ypencil = var_ypencil +  accc_ypencil
!----------------------------------------------------------------------------------------------------------
! Z-pencil : w_ccc / dz /r2
!----------------------------------------------------------------------------------------------------------
    call transpose_y_to_z(var_ypencil, var_zpencil, dm%dccc)
    call transpose_x_to_y(w,             w_ypencil, dm%dccp)
    if(dm%icoordinate == ICYLINDRICAL) then
      do j = 1, dm%dccp%ysz(2)
        w_ypencil(:, j, :) = w_ypencil(:, j, :) * dm%rci(j) * dm%rci(j) 
      end do 
    end if
    call transpose_y_to_z(w_ypencil,     w_zpencil, dm%dccp)
    call Get_z_midp_P2C_3D(w_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    var_zpencil = var_zpencil +  accc_zpencil * dm%h1r(3) * dm%dt
!----------------------------------------------------------------------------------------------------------
! Z-pencil : Find the maximum 
!----------------------------------------------------------------------------------------------------------
    call Find_max_min_3d(var_zpencil, opt_calc='MAXI', opt_work=cfl, opt_name='CFL (convection) :')
    if(present(opt_cfl)) opt_cfl = cfl(2)
    if(cfl(2) > TWO) then 
      dm%dt = dm%dt / REAL(ceiling(cfl(2)/ 5.0_WP) * 5, WP)
      if(nrank == 0) then
        call Print_warning_msg("Warning: CFL is larger than 1.")
        write(*, wrtfmt1e) 'dt reduced to ', dm%dt
      end if
    end if
    
    return
  end subroutine
!==========================================================================================================
!>\brief : to calculate:
!>         fo = \int_1^nx \int_
!> This is based only y-direction stretching.
!> \todo Here is 2nd order Trapezoid Method. Need to improve! Check!
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi    called-freq    xdomain     module
!>         all    needed         specified   pubic
!----------------------------------------------------------------------------------------------------------
!> MPI : 
!>     default x-pencil
!>     working in : y-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]         
!==========================================================================================================
!   subroutine Get_volumetric_average_3d(is_ynp, ibcy, fbcy, dm, dtmp, var, fo_work)
!     use mpi_mod
!     use udf_type_mod
!     use parameters_constant_mod
!     use operations
!     use decomp_2d
!     use wtformat_mod
!     implicit none
!     type(t_domain),  intent(in) :: dm
!     logical,           intent(in) :: is_ynp
!     integer,           intent(in) :: ibcy(2)
!     real(WP),          intent(in) :: fbcy(:, :, :)
!     type(DECOMP_INFO), intent(in) :: dtmp
!     real(WP),          intent(in) :: var(:, :, :)
!     real(WP),          intent(out):: fo_work
 
!     real(WP), dimension( dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3) )  :: var_ypencil
!     real(WP), allocatable   :: vcp_ypencil(:, :, :)
!     real(WP)   :: vol, fo, vol_work
!     integer :: i, j, k, noy, jp

! ! #ifdef DEBUG_STEPS  
! !     if(nrank == 0) then
! !       if(present(str)) then
! !         call Print_debug_inline_msg("Calculating volumeric average of "//trim(str)//" in 3-D ...")
! !       else
! !         call Print_debug_inline_msg("Calculating volumeric average in 3-D ...")
! !       end if
! !     end if
! ! #endif

!     if(.not. dm%is_stretching(2) ) then 
!       vol = ZERO
!       fo  = ZERO
!       do k = 1, dtmp%xsz(3)
!         do j = 1, dtmp%xsz(2)
!           do i = 1, dtmp%xsz(1)
!             fo = fo + var(i, j, k)
!             vol = vol + ONE
!           end do
!         end do
!       end do
      
!     else
!     !----------------------------------------------------------------------------------------------------------
!     !   transpose to y pencil. Default is x-pencil.
!     !----------------------------------------------------------------------------------------------------------
!       var_ypencil = ZERO

!       call transpose_x_to_y(var, var_ypencil, dtmp)
!       !----------------------------------------------------------------------------------------------------------
!       !   In Y-pencil now
!       !----------------------------------------------------------------------------------------------------------
!       if( is_ynp )then
!         !----------------------------------------------------------------------------------------------------------
!         !   if variable is stored in y-nodes, extend them to y-cell centres (P2C)
!         !   for example, uy.
!         !----------------------------------------------------------------------------------------------------------
!         if( dm%is_periodic(2) ) then
!           noy = dtmp%ysz(2)
!         else
!           noy = dtmp%ysz(2) - 1
!         end if

!         allocate( vcp_ypencil(dtmp%ysz(1), noy, dtmp%ysz(3)) )
!         vcp_ypencil = ZERO

!         call Get_y_midp_P2C_3D(var_ypencil, vcp_ypencil, dm, dm%iAccuracy, ibcy, fbcy)

!         fo = ZERO
!         vol = ZERO
!         do k = 1, dtmp%ysz(3)
!           do i = 1, dtmp%ysz(1)
!             do j = 1, noy
!               !----------------------------------------------------------------------------------------------------------
!               !       j'    j'+1
!               !      _|__.__|_
!               !         j     
!               !----------------------------------------------------------------------------------------------------------
!               jp = j + 1
!               if( dm%is_periodic(2) .and. jp > dtmp%ysz(2)) jp = 1
!               fo = fo + &      
!                   ( var_ypencil(i, jp, k) + vcp_ypencil(i, j, k) ) * &
!                   ( dm%yp(j + 1) - dm%yc(j) ) * HALF + &
!                   ( var_ypencil(i, j,     k) + vcp_ypencil(i, j, k) ) * &
!                   ( dm%yc(j    ) - dm%yp(j) ) * HALF
!               vol = vol + ( dm%yp(j + 1) - dm%yp(j) )
!             end do
!           end do
!         end do
!         deallocate(vcp_ypencil)
!       else
!         !----------------------------------------------------------------------------------------------------------
!         !   if variable is not stored in y-nodes, extends them to y-nodes. C2P
!         !   for example, ux, density, etc.
!         !----------------------------------------------------------------------------------------------------------
!         if( dm%is_periodic(2) ) then
!           noy = dtmp%ysz(2)
!         else
!           noy = dtmp%ysz(2) + 1
!         end if
!         allocate( vcp_ypencil(dtmp%ysz(1), noy, dtmp%ysz(3)) )
!         vcp_ypencil = ZERO
!         call Get_y_midp_C2P_3D(var_ypencil, vcp_ypencil, dm, dm%iAccuracy, ibcy, fbcy)

!         fo = ZERO
!         vol = ZERO
!         do k = 1, dtmp%ysz(3)
!           do i = 1, dtmp%ysz(1)
!             do j = 1, dtmp%ysz(2)
!               !----------------------------------------------------------------------------------------------------------
!               !      j'    j'+1
!               !      _|__.__|_
!               !         j
!               !----------------------------------------------------------------------------------------------------------
!               jp = j + 1
!               if( dm%is_periodic(2) .and. jp > noy) jp = 1
!               ! method 1: 2nd order
!               ! fo = fo + &
!               !     ( vcp_ypencil(i, jp, k) + var_ypencil(i, j, k) ) * &
!               !     ( dm%yp(j + 1) - dm%yc(j) ) * HALF + &
!               !     ( var_ypencil(i, j,     k) + var_ypencil(i, j, k) ) * &
!               !     ( dm%yc(j    ) - dm%yp(j) ) * HALF
!               ! method 2: 1st order, same as CHAPSim1
!               fo = fo + vcp_ypencil(i, j, k)*(dm%yp(j + 1) - dm%yp(j))
!               vol = vol + ( dm%yp(j + 1) - dm%yp(j) )
!             end do
!           end do
!         end do
!         deallocate(vcp_ypencil)
!       end if

!     end if


!     call mpi_barrier(MPI_COMM_WORLD, ierror)
!     call mpi_allreduce( fo,  fo_work, 1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
!     call mpi_allreduce(vol, vol_work, 1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
!     fo_work = fo_work / vol_work

! #ifdef DEBUG_STEPS  
!     if(nrank == 0 ) then
!       write (*, wrtfmt1e) " volumetric average :", fo_work
!     end if
! #endif

!     return 
!   end subroutine Get_volumetric_average_3d

  !==========================================================================================================
  !==========================================================================================================
  subroutine get_fbcx_ftp_4pc(fbcx_ftp_4cc, fbcx_ftp_4pc, dm)
    use udf_type_mod
    use parameters_constant_mod
    use operations
    use print_msg_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)), intent(in)  :: fbcx_ftp_4cc
    real(WP), dimension(dm%d4pc%xsz(1), dm%d4pc%xsz(2), dm%d4pc%xsz(3)), intent(out) :: fbcx_ftp_4pc
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_4cc
    real(WP), dimension(dm%d4cc%ysz(1), dm%d4cc%ysz(2), dm%d4cc%ysz(3)) :: a4cc_ypencil
    real(WP), dimension(dm%d4pc%ysz(1), dm%d4pc%ysz(2), dm%d4pc%ysz(3)) :: a4pc_ypencil
    real(WP), dimension(dm%d4pc%xsz(1), dm%d4pc%xsz(2), dm%d4pc%xsz(3)) :: a4pc_xpencil
    integer :: i, j, k!, ibcy(2)
    real(WP) :: fbc


    if(dm%ibcx_ftp(2) == IBC_DIRICHLET) then
      fbcx_4cc(:, :, :) = fbcx_ftp_4cc(:, :, :)
      call transpose_x_to_y(fbcx_4cc, a4cc_ypencil, dm%d4cc)
      do i = 1, dm%d4pc%ysz(1)
        do k = 1, dm%d4pc%ysz(3)
          j = 1
          a4pc_ypencil(i, j, k) = (THREE * a4cc_ypencil(i, j, k) - a4cc_ypencil(i, j+1, k))/TWO
          j= dm%d4pc%ysz(2)
          if(j<=2) call Print_error_msg('get_fbcx_ftp_4pc decomposition error')
          a4pc_ypencil(i, j, k) = (THREE * a4cc_ypencil(i, j-1, k) - a4cc_ypencil(i, j-2, k))/TWO
          do j = 2, dm%d4pc%ysz(2)-1
            a4pc_ypencil(i, j, k) = (a4cc_ypencil(i, j-1, k) + a4cc_ypencil(i, j, k))/TWO
          end do
        end do
      end do 
      ! ibcy = IBC_INTRPL
      ! call Get_y_midp_C2P_3D(a4cc_ypencil, a4pc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_44c)
       call transpose_y_to_x(a4pc_ypencil, a4pc_xpencil, dm%d4pc)
       fbcx_ftp_4pc(:, :, :) = a4pc_xpencil(:, :, :)
    else
      fbcx_ftp_4pc(2, :, :) = MAXP 
    end if

    if(dm%ibcx_ftp(1) == IBC_DIRICHLET) then
      fbc = fbcx_ftp_4cc(1, 1, 1)
      fbcx_ftp_4pc(1, :, :) = fbc ! check
    else 
      fbcx_ftp_4pc(1, :, :) = MAXP 
    end if 

    ! write(*,*) '1-', fbcx_ftp_4pc(1, :, :)
    ! write(*,*) '2-', fbcx_ftp_4pc(2, :, :)
    ! write(*,*) '3-', fbcx_ftp_4pc(3, :, :)
    ! write(*,*) '4-', fbcx_ftp_4pc(4, :, :)

    return
  end subroutine

  !==========================================================================================================
  subroutine check_global_mass_balance(mass_imbalance, drhodt, dm)
    use udf_type_mod
    use parameters_constant_mod
    use find_max_min_ave_mod
    implicit none
    real(WP), dimension(:,:,:), intent(in) :: drhodt
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: mass_imbalance(8)
    !
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx
    real(WP), dimension(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) :: fbcy
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) :: fbcz
    real(WP) :: intg_m, intg_fbcx(2), intg_fbcy(2), intg_fbcz(2)
    !-----------------------------------------------------------------
    ! mass balance = density change + net mass flux through boundaries
    !-----------------------------------------------------------------
    ! density change introduced mass change = integral_volume(drho/dt) unit = kg/m3/s m3 = kg/s
    if(dm%is_thermo) then
      call Get_volumetric_average_3d(dm, dm%dccc, drhodt, intg_m, SPACE_INTEGRAL, 'drhodt')
    else
      intg_m = ZERO
    end if
    !-----------------------------------------------------------------
    ! mass flux through b.c. = integral_surface (mass flux), unit = kg/m3 m/s m2 = kg/s
    !-----------------------------------------------------------------
    ! x-bc
    if(dm%ibcx_qx(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
        fbcx = dm%fbcx_gx
      else
        fbcx = dm%fbcx_qx
      end if
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, fbcx, intg_fbcx, SPACE_INTEGRAL, 'fbcx')
    else
      intg_fbcx = ZERO
    end if
    ! y-bc
    if(dm%ibcy_qy(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
        fbcy = dm%fbcy_gy
      else
        fbcy = dm%fbcy_qy
      end if
      call Get_area_average_2d_for_fbcy(dm, dm%dcpc, fbcy, intg_fbcy, SPACE_INTEGRAL, 'fbcy', is_rf=.true.)
    else
      intg_fbcy = ZERO
    end if
    ! z-bc
    if(dm%ibcz_qz(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
        fbcz = dm%fbcz_gz
      else
        fbcz = dm%fbcz_qz
      end if
      call Get_area_average_2d_for_fbcz(dm, dm%dccp, fbcz, intg_fbcz, SPACE_INTEGRAL, 'fbcz')
    else
      intg_fbcz = ZERO
    end if
    ! mass change rate, kg/s
    mass_imbalance(1:2) = intg_fbcx(1:2)
    mass_imbalance(3:4) = intg_fbcy(1:2)
    mass_imbalance(5:6) = intg_fbcz(1:2)
    mass_imbalance(7)   = intg_m
    mass_imbalance(8)   = intg_m + &
                          intg_fbcx(1) - intg_fbcx(2) + &
                          intg_fbcy(1) - intg_fbcy(2) + &
                          intg_fbcz(1) - intg_fbcz(2) 
    return
  end subroutine 

 !==========================================================================================================
  subroutine damping_drhodt(accc_xpencil, dm)
    use udf_type_mod
    use transpose_extended_mod
    use parameters_constant_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(inout) :: accc_xpencil(:, :, :)
    integer :: i, k
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: accc_zpencil
    !
    if(.not. dm%is_thermo) return
    if(.not. is_damping_drhodt) return
    !
    if(dm%is_conv_outlet(1)) then
      do i = 1, dm%dccc%xsz(1)
          accc_xpencil(i,:,:) = accc_xpencil(i,:,:) * (ONE - dm%xdamping(i))
      end do
    end if
    !
    if(dm%is_conv_outlet(3)) then
      call transpose_to_z_pencil(accc_xpencil, accc_zpencil, dm%dccc, IPENCIL(1))
      do i = 1, dm%dccc%zsz(3)
        accc_zpencil(:,:,k) = accc_zpencil(:,:,k) * (ONE - dm%zdamping(k) )
      end do
    end if
    return
  end subroutine
  !
end module
