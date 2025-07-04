module io_restart_mod
  use print_msg_mod
  use parameters_constant_mod
  use decomp_2d_io
  use udf_type_mod
  use io_files_mod
  use io_tools_mod
  implicit none 

  character(len=10), parameter :: io_name = "restart-io"

  private :: write_instantaneous_array
  public  :: write_instantaneous_flow
  public  :: write_instantaneous_thermo
  private :: read_instantaneous_array
  public  :: read_instantaneous_flow
  public  :: read_instantaneous_thermo
  public  :: restore_flow_variables_from_restart
  public  :: restore_thermo_variables_from_restart

  private :: append_instantaneous_xoutlet
  private :: write_instantaneous_plane !not used
  public  :: write_instantaneous_xoutlet

  private :: assign_instantaneous_xinlet
  private :: read_instantaneous_plane !not used
  public  :: read_instantaneous_xinlet

contains 
!==========================================================================================================
!==========================================================================================================
  subroutine read_instantaneous_array(var, keyword, idom, iter, dtmp)
    implicit none 
    integer, intent(in) :: idom
    character(*), intent(in) :: keyword
    integer, intent(in) :: iter
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), dimension(:, :, :), intent(out) :: var( dtmp%xsz(1), &
                                                      dtmp%xsz(2), &
                                                      dtmp%xsz(3))
    character(120):: data_flname

    call generate_file_name(data_flname, idom, trim(keyword), 'bin', iter)
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(dir_data)//"/"//trim(data_flname))

    call decomp_2d_read_one(IPENCIL(1), var, trim(data_flname), &
          opt_dirname=trim(dir_data), &
          opt_decomp=dtmp, &
          opt_reduce_prec=.false.)

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_instantaneous_array(var, keyword, idom, iter, dtmp)
    implicit none 
    real(WP), contiguous, intent(in) :: var( :, :, :)
    type(DECOMP_INFO), intent(in) :: dtmp
    character(*), intent(in) :: keyword
    integer, intent(in) :: idom
    integer, intent(in) :: iter
    
    character(120):: data_flname_path

    call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter)
    call decomp_2d_write_one(IPENCIL(1), var, trim(data_flname_path), opt_decomp=dtmp)

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_instantaneous_flow(fl, dm)
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl

    character(120):: data_flname_path
    character(120):: keyword

    if(nrank == 0) call Print_debug_inline_msg("writing out instantaneous 3d flow data ...")

    call write_instantaneous_array(fl%qx, 'qx', dm%idom, fl%iteration, dm%dpcc)
    call write_instantaneous_array(fl%qy, 'qy', dm%idom, fl%iteration, dm%dcpc)
    call write_instantaneous_array(fl%qz, 'qz', dm%idom, fl%iteration, dm%dccp)
    call write_instantaneous_array(fl%pres, 'pr', dm%idom, fl%iteration, dm%dccc)

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_instantaneous_thermo(tm, dm)
    use thermo_info_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm

    character(120):: data_flname_path
    character(120):: keyword
    

    if(nrank == 0) call Print_debug_inline_msg("writing out instantaneous 3d thermo data ...")

    call write_instantaneous_array(tm%rhoh,  'rhoh', dm%idom, tm%iteration, dm%dccc)
    call write_instantaneous_array(tm%tTemp, 'temp', dm%idom, tm%iteration, dm%dccc)

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine read_instantaneous_flow(fl, dm)
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl

    character(120):: data_flname
    character(120):: keyword


    if(nrank == 0) call Print_debug_inline_msg("read instantaneous flow data ...")

    call read_instantaneous_array(fl%qx, 'qx', dm%idom, fl%iterfrom, dm%dpcc)
    call read_instantaneous_array(fl%qy, 'qy', dm%idom, fl%iterfrom, dm%dcpc)
    call read_instantaneous_array(fl%qz, 'qz', dm%idom, fl%iterfrom, dm%dccp)
    call read_instantaneous_array(fl%pres, 'pr', dm%idom, fl%iterfrom, dm%dccc)
    
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
  subroutine restore_flow_variables_from_restart(fl, dm)
    use mpi_mod
    use boundary_conditions_mod
    use solver_tools_mod
    use wtformat_mod
    use find_max_min_ave_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    real(WP) :: ubulk
    

    !call Get_volumetric_average_3d(.false., dm%ibcy_qx(:), dm%fbcy_qx(:, :, :), dm, dm%dpcc, fl%qx, ubulk, "ux")
    call Get_volumetric_average_3d_for_var_xcx(dm, dm%dpcc, fl%qx, ubulk, SPACE_AVERAGE, "ux")
    if(nrank == 0) then
        call Print_debug_inline_msg("The restarted mass flux is:")
        write (*, wrtfmt1e) ' average[u(x,y,z)]_[x,y,z]: ', ubulk
    end if
    !----------------------------------------------------------------------------------------------------------
    ! to check maximum velocity
    !----------------------------------------------------------------------------------------------------------
    call Find_max_min_3d(fl%qx, opt_name="qx: ")
    call Find_max_min_3d(fl%qy, opt_name="qy: ")
    call Find_max_min_3d(fl%qz, opt_name="qz: ")
    !----------------------------------------------------------------------------------------------------------
    ! to set up other parameters for flow only, which will be updated in thermo flow.
    !----------------------------------------------------------------------------------------------------------
    fl%pcor(:, :, :) = ZERO
    fl%pcor_zpencil_ggg(:, :, :) = ZERO
    if(dm%is_thermo) then
      fl%dDens  (:, :, :) = ONE
      fl%mVisc  (:, :, :) = ONE
      if(.not. is_drhodt_implicit) then
      fl%dDensm1(:, :, :) = ONE
      fl%dDensm2(:, :, :) = ONE
      end if
    end if

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine read_instantaneous_thermo(tm, dm)
    use thermo_info_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_thermo), intent(inout) :: tm

    character(120):: data_flname
    character(120):: keyword

    if (.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_inline_msg("read instantaneous thermo data ...")

    tm%iteration = tm%iterfrom

    keyword = 'rhoh'
    call generate_file_name(data_flname, dm%idom, keyword, 'bin', tm%iteration)
    call decomp_2d_read_one(IPENCIL(1), tm%rhoh, trim(data_flname), &
          opt_dirname=trim(dir_data), &
          opt_decomp=dm%dccc, &
          opt_reduce_prec=.false.)
    keyword = 'temp'
    call generate_file_name(data_flname, dm%idom, keyword, 'bin', tm%iteration)
    call decomp_2d_read_one(IPENCIL(1), tm%tTemp, trim(data_flname), &
          opt_dirname=trim(dir_data), &
          opt_decomp=dm%dccc, &
          opt_reduce_prec=.false.)
    tm%time = real(tm%iterfrom, WP) * dm%dt 

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==========================================================================================================
  subroutine restore_thermo_variables_from_restart(fl, tm, dm)
    use udf_type_mod
    use thermo_info_mod
    use eq_energy_mod
    use solver_tools_mod
    use convert_primary_conservative_mod
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm

    if (.not. dm%is_thermo) return

    call Update_thermal_properties(fl, tm, dm)
    call convert_primary_conservative (fl, dm, IQ2G)

    if(.not. is_drhodt_implicit) then
    fl%dDensm1(:, :, :) = fl%dDens(:, :, :)
    fl%dDensm2(:, :, :) = fl%dDens(:, :, :)
    end if

    return
  end subroutine


!==========================================================================================================
  subroutine append_instantaneous_xoutlet(fl, dm, niter)
    implicit none 
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(inout) :: dm
    integer, intent(out) :: niter

    integer :: j, k
    type(DECOMP_INFO) :: dtmp

    ! based on x pencil
    if(.not. dm%is_record_xoutlet) return
    if(fl%iteration < dm%ndbstart) return

    ! if dm%ndbfre = 10, and start from 16
    ! store : 
    !     To store: 16, 17, 18,..., 25 (MOD: 1, 2, ..., 0)
    !        niter:  1,  2,  3,..., 0
    niter = mod(fl%iteration - dm%ndbstart + 1, dm%ndbfre) !
    if(niter == 1) then ! re-initialize at begin of each cycle
      dm%fbcx_qx_outl1 = MAXP
      dm%fbcx_qx_outl2 = MAXP
      dm%fbcx_qy_outl1 = MAXP
      dm%fbcx_qy_outl2 = MAXP
      dm%fbcx_qz_outl1 = MAXP
      dm%fbcx_qz_outl2 = MAXP
      dm%fbcx_pr_outl1 = MAXP
      dm%fbcx_pr_outl2 = MAXP
    else if(niter == 0) then
      niter =  dm%ndbfre
    else
      ! do nothing
    end if

    dtmp = dm%dpcc
    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        dm%fbcx_qx_outl1(niter, j, k) = fl%qx(dtmp%xsz(1),   j, k)
        dm%fbcx_qx_outl2(niter, j, k) = fl%qx(dtmp%xsz(1)-1, j, k)
      end do
    end do

    !write(*, *) 'j, fl%qx(1, j, 1), dm%fbcx_qx_outl1(niter, j, 1)'
    ! do j = 1, dm%dpcc%xsz(2)
    !   write(*, *) j, fl%qx(dtmp%xsz(1), j, 1), dm%fbcx_qx_outl1(niter, j, 1)
    ! end do

    dtmp = dm%dcpc
    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        dm%fbcx_qy_outl1(niter, j, k) = fl%qy(dtmp%xsz(1),   j, k)
        dm%fbcx_qy_outl2(niter, j, k) = fl%qy(dtmp%xsz(1)-1, j, k)
      end do
    end do

    dtmp = dm%dccp
    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        dm%fbcx_qz_outl1(niter, j, k) = fl%qz(dtmp%xsz(1),   j, k)
        dm%fbcx_qz_outl2(niter, j, k) = fl%qz(dtmp%xsz(1)-1, j, k)
      end do
    end do

    dtmp = dm%dccc
    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        dm%fbcx_pr_outl1(niter, j, k) = fl%pres(dtmp%xsz(1),   j, k)
        dm%fbcx_pr_outl2(niter, j, k) = fl%pres(dtmp%xsz(1)-1, j, k)
      end do
    end do

    return
  end subroutine
!==========================================================================================================
  subroutine write_instantaneous_plane(var, keyword, idom, iter, niter, dtmp)
    implicit none 
    real(WP), contiguous, intent(in) :: var( :, :, :)
    type(DECOMP_INFO), intent(in) :: dtmp
    character(*), intent(in) :: keyword
    integer, intent(in) :: idom
    integer, intent(in) :: iter, niter

    character(120):: data_flname_path

    call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter)

    if(nrank==0) write(*, *) 'Write outlet plane data to ['//trim(data_flname_path)//"]"
 
    !call decomp_2d_open_io (io_in2outlet, trim(data_flname_path), decomp_2d_write_mode)
    !call decomp_2d_start_io(io_in2outlet, trim(data_flname_path))!

    !call decomp_2d_write_outflow(trim(data_flname_path), trim(keyword), niter, var, io_in2outlet, dtmp)
    !call decomp_2d_write_plane(IPENCIL(1), var, 1, dtmp%xsz(1), trim(data_flname_path), dtmp)
    call decomp_2d_write_plane(IPENCIL(1), var, data_flname_path, &
                                opt_nplanes=niter, &
                                opt_decomp = dtmp)
    !call decomp_2d_end_io(io_in2outlet, trim(data_flname_path))
    !call decomp_2d_close_io(io_in2outlet, trim(data_flname_path))

    return
  end subroutine
!==========================================================================================================
  subroutine write_instantaneous_xoutlet(fl, dm)
    implicit none 
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(inout) :: dm
    
    character(120):: data_flname_path
    integer :: idom, niter, iter, j

    if(.not. dm%is_record_xoutlet) return
    if(fl%iteration < dm%ndbstart) return

    call append_instantaneous_xoutlet(fl, dm, niter)

    ! if dm%ndbfre = 10, and start from 16
    ! store : 
    !     To store: 16, 17, 18,..., 25 (MOD: 1, 2, ..., 0)
    !        niter:  1,  2,  3,..., 0->10
    !    file name: 25, 35, 45 ... 
    !write(*,*) 'iter, niter', fl%iteration, niter
    if(niter == dm%ndbfre) then
      if( mod(fl%iteration - dm%ndbstart + 1, dm%ndbfre) /= 0 .and. nrank == 0) &
      call Print_warning_msg("niter /= dm%ndbfre, something wrong in writing outlet data")
      call write_instantaneous_array(dm%fbcx_qx_outl1, 'outlet1_qx', dm%idom, fl%iteration, dm%dxcc)
      call write_instantaneous_array(dm%fbcx_qx_outl2, 'outlet2_qx', dm%idom, fl%iteration, dm%dxcc)
      call write_instantaneous_array(dm%fbcx_qy_outl1, 'outlet1_qy', dm%idom, fl%iteration, dm%dxpc)
      call write_instantaneous_array(dm%fbcx_qy_outl2, 'outlet2_qy', dm%idom, fl%iteration, dm%dxpc)
      call write_instantaneous_array(dm%fbcx_qz_outl1, 'outlet1_qz', dm%idom, fl%iteration, dm%dxcp)
      call write_instantaneous_array(dm%fbcx_qz_outl2, 'outlet2_qz', dm%idom, fl%iteration, dm%dxcp)
      call write_instantaneous_array(dm%fbcx_pr_outl1, 'outlet1_pr', dm%idom, fl%iteration, dm%dxcc)
      call write_instantaneous_array(dm%fbcx_pr_outl2, 'outlet2_pr', dm%idom, fl%iteration, dm%dxcc)
    end if
! #ifdef DEBUG_STEPS
!     write(*,*) 'outlet bc'
!     do j = 1, dm%dpcc%xsz(2)
!       write(*,*) dm%dpcc%xst(2) + j - 1, &
!       dm%fbcx_qx_outl1(niter, j, 1), dm%fbcx_qx_outl2(niter, j, 1)
!     end do
!     write(*,*) 'inlet bc'
!     do j = 1, dm%dpcc%xsz(2)
!       write(*,*) dm%dpcc%xst(2) + j - 1, &
!       dm%fbcx_qx_out1(niter, j, 1), dm%fbcx_qx_out2(niter, j, 1)
!     end do
! #endif
    return
  end subroutine
!==========================================================================================================
  subroutine assign_instantaneous_xinlet(fl, dm)
    implicit none 
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(inout) :: dm

    integer :: iter, j, k
    type(DECOMP_INFO) :: dtmp

    ! based on x pencil
    if(.not. dm%is_read_xinlet) return

    if(fl%iteration > dm%ndbend) then
      iter = mod(fl%iteration, dm%ndbend) ! database recycle
    else if (fl%iteration == 0) then
      iter = 1
    else
      iter = fl%iteration
    end if

    iter = mod(iter, dm%ndbfre)
    if(iter == 0) iter = dm%ndbfre

    if(dm%ibcx_nominal(1, 1) == IBC_DATABASE) then
      dtmp = dm%dpcc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_qx(1, j, k) = dm%fbcx_qx_inl1(iter, j, k)
          dm%fbcx_qx(3, j, k) = dm%fbcx_qx_inl2(iter, j, k)
          ! check, below 
          !fl%qx(1, j, k) = dm%fbcx_qx(1, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_in1 = ', iter, dm%fbcx_qx_inl1(iter, :, 1)
      !if(nrank == 0) write(*,*) 'fbcx_in2 = ', iter, dm%fbcx_qx_inl1(iter, :, 32)
      !if(nrank == 0) write(*,*) 'fbcx_qx1 = ', iter, dm%fbcx_qx(1, :, 1)
      !if(nrank == 0) write(*,*) 'fbcx_qx2 = ', iter, dm%fbcx_qx(1, :, 32)
    end if

        ! test
    !write(*,*) 'j, fl%qx(1, j, 1), dm%fbcx_qx(1, j, 1)'
    !do j = 1, dm%dpcc%xsz(2)
      !write(*,*) j, fl%qx(1, j, 1), dm%fbcx_qx(1, j, 1)
    !end do

    if(dm%ibcx_nominal(1, 2) == IBC_DATABASE) then
      dtmp = dm%dcpc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_qy(1, j, k) = dm%fbcx_qy_inl1(iter, j, k)
          dm%fbcx_qy(3, j, k) = dm%fbcx_qy_inl2(iter, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_qy = ', iter, dm%fbcx_qy(1, :, :)
    end if

    if(dm%ibcx_nominal(1, 3) == IBC_DATABASE) then
      dtmp = dm%dccp
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_qz(1, j, k) = dm%fbcx_qz_inl1(iter, j, k)
          dm%fbcx_qz(3, j, k) = dm%fbcx_qz_inl2(iter, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_qz = ', iter, dm%fbcx_qz(1, :, :)
    end if

    if(dm%ibcx_nominal(1, 4) == IBC_DATABASE) then
      dtmp = dm%dccc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_pr(1, j, k) = dm%fbcx_pr_inl1(iter, j, k)
          dm%fbcx_pr(3, j, k) = dm%fbcx_pr_inl2(iter, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_pr = ', iter, dm%fbcx_pr(1, :, :)
    end if

    return
  end subroutine
!==========================================================================================================
  subroutine read_instantaneous_plane(var, keyword, idom, iter, nfre, dtmp)
    use decomp_2d_io
    implicit none 
    real(WP), contiguous, intent(out) :: var( :, :, :)
    type(DECOMP_INFO), intent(in) :: dtmp
    character(*), intent(in) :: keyword
    integer, intent(in) :: idom
    integer, intent(in) :: iter
    integer, intent(in) :: nfre

    character(120):: data_flname_path, flname

    call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter, flname)

    !call decomp_2d_open_io (io_in2outlet, trim(data_flname_path), decomp_2d_read_mode)
    if(nrank == 0) call Print_debug_inline_msg("Read data on a plane from file: "//trim(data_flname_path))
    !call decomp_2d_read_inflow(trim(data_flname_path), trim(keyword), nfre, var, io_in2outlet, dtmp)
    call decomp_2d_read_plane(IPENCIL(1), var, data_flname_path, nfre, &
                                opt_decomp = dtmp)

    !decomp_2d_read_plane(ipencil, var, varname, nplanes, &
                              !  opt_dirname, &
                              !  opt_mpi_file_open_info, &
                              !  opt_mpi_file_set_view_info, &
                              !  opt_reduce_prec, &
                              !  opt_decomp, &
                              !  opt_nb_req, &
                              !  opt_io)
    !write(*,*) var
    !call decomp_2d_close_io(io_in2outlet, trim(data_flname_path))

    return
  end subroutine
!==========================================================================================================
  subroutine read_instantaneous_xinlet(fl, dm)
    use typeconvert_mod
    implicit none 
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(inout) :: dm
    
    character(120):: data_flname_path
    integer :: idom, iter, niter, j


    if(.not. dm%is_read_xinlet) return


    ! if dm%ndbfre = 10, and start from 16
    ! store : 
    !     To store: 16, 17, 18,..., 25 (MOD: 1, 2, ..., 0)
    !        niter:  1,  2,  3,..., 0
    !    file name: 25, 35, 45 ...

    iter = fl%iteration
    if(iter > dm%ndbend) then
      iter = mod(iter, dm%ndbend) ! database recycle
    end if

    if(mod(iter, dm%ndbfre) == 1 .or. &
       iter == 0) then

      if (iter == 0) then
        niter = dm%ndbstart + dm%ndbfre - 1
      else
        niter = iter + dm%ndbstart - 1
        niter = niter + dm%ndbfre - 1
      end if

      if(niter > dm%ndbend) niter = dm%ndbstart + dm%ndbfre - 1

      if(nrank == 0) call Print_debug_mid_msg("Read inlet database at iteration "&
        //trim(int2str(iter))//'/'//trim(int2str(niter)))
      call read_instantaneous_array(dm%fbcx_qx_inl1, 'outlet1_qx', dm%idom, niter, dm%dxcc)
      call read_instantaneous_array(dm%fbcx_qx_inl2, 'outlet2_qx', dm%idom, niter, dm%dxcc)
      call read_instantaneous_array(dm%fbcx_qy_inl1, 'outlet1_qy', dm%idom, niter, dm%dxpc)
      call read_instantaneous_array(dm%fbcx_qy_inl2, 'outlet2_qy', dm%idom, niter, dm%dxpc)
      call read_instantaneous_array(dm%fbcx_qz_inl1, 'outlet1_qz', dm%idom, niter, dm%dxcp)
      call read_instantaneous_array(dm%fbcx_qz_inl2, 'outlet2_qz', dm%idom, niter, dm%dxcp)
      !call read_instantaneous_array(dm%fbcx_pr_inl1, 'outlet1_pr', dm%idom, niter, dm%dxcc)
      !call read_instantaneous_array(dm%fbcx_pr_inl2, 'outlet2_pr', dm%idom, niter, dm%dxcc)
! #ifdef DEBUG_STEPS
      !write(*,*) 'inlet bc1', niter, dm%fbcx_qx_inl1(:, 32, 1)
      !write(*,*) 'inlet bc2', niter, dm%fbcx_qx_inl1(:, 32, 32)
      ! write(*,*) 'inlet bc1-end', niter, dm%fbcx_qx_inl1(niter, :, 1)
      ! write(*,*) 'inlet bc2-end', niter, dm%fbcx_qx_inl1(niter, :, 32)
      ! do j = 1, dm%dpcc%xsz(2)
      !   write(*,*) dm%dpcc%xst(2) + j - 1, &
      !   dm%fbcx_qx_inl1(niter, j, 1:64)
      ! end do
! #endif
    end if

    call assign_instantaneous_xinlet(fl, dm) ! every iteration

    return
  end subroutine
!==========================================================================================================
end module 