module io_restart_mod
  use print_msg_mod
  use parameters_constant_mod
  use decomp_2d_io
  use udf_type_mod
  use io_files_mod
  use io_tools_mod
  implicit none 

  character(len=10), parameter :: io_name = "restart-io"

  public  :: write_instantaneous_flow
  public  :: read_instantaneous_flow
  public  :: restore_flow_variables_from_restart

  public  :: write_instantaneous_thermo
  public  :: read_instantaneous_thermo
  public  :: restore_thermo_variables_from_restart

  private :: append_instantaneous_xoutlet
  private :: assign_instantaneous_xinlet
  public  :: write_instantaneous_xoutlet
  public  :: read_instantaneous_xinlet

  !private :: write_instantaneous_plane !not used
  !private :: read_instantaneous_plane !not used

contains 

!==========================================================================================================
!==========================================================================================================
  subroutine write_instantaneous_flow(fl, dm)
    use io_tools_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl

    character(64):: data_flname_path
    character(64):: keyword

    if(nrank == 0) call Print_debug_inline_msg("writing out instantaneous 3d flow data ...")

    call write_one_3d_array(fl%qx, 'qx', dm%idom, fl%iteration, dm%dpcc)
    call write_one_3d_array(fl%qy, 'qy', dm%idom, fl%iteration, dm%dcpc)
    call write_one_3d_array(fl%qz, 'qz', dm%idom, fl%iteration, dm%dccp)
    call write_one_3d_array(fl%pres, 'pr', dm%idom, fl%iteration, dm%dccc)

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

    character(64):: data_flname_path
    character(64):: keyword
    

    if(nrank == 0) call Print_debug_inline_msg("writing out instantaneous 3d thermo data ...")

    call write_one_3d_array(tm%rhoh,  'rhoh', dm%idom, tm%iteration, dm%dccc)
    call write_one_3d_array(tm%tTemp, 'temp', dm%idom, tm%iteration, dm%dccc)

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine read_instantaneous_flow(fl, dm)
    use io_tools_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl

    character(64):: data_flname
    character(64):: keyword


    if(nrank == 0) call Print_debug_inline_msg("read instantaneous flow data ...")
    fl%iteration = fl%iterfrom
    fl%time = real(fl%iterfrom, WP) * dm%dt 

    call read_one_3d_array(fl%qx,   'qx', dm%idom, fl%iterfrom, dm%dpcc)
    call read_one_3d_array(fl%qy,   'qy', dm%idom, fl%iterfrom, dm%dcpc)
    call read_one_3d_array(fl%qz,   'qz', dm%idom, fl%iterfrom, dm%dccp)
    call read_one_3d_array(fl%pres, 'pr', dm%idom, fl%iterfrom, dm%dccc)
    
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
    

    call Get_volumetric_average_3d(dm, dm%dpcc, fl%qx, ubulk, SPACE_AVERAGE, "ux")
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

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine read_instantaneous_thermo(tm, dm)
    use thermo_info_mod
    use io_tools_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_thermo), intent(inout) :: tm

    character(64):: data_flname
    character(64):: keyword

    if (.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_inline_msg("read instantaneous thermo data ...")

    tm%iteration = tm%iterfrom
    tm%time = real(tm%iterfrom, WP) * dm%dt 

    call read_one_3d_array(tm%rhoh,  'rhoh', dm%idom, tm%iteration, dm%dccc)
    call read_one_3d_array(tm%tTemp, 'temp', dm%idom, tm%iteration, dm%dccc)


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

    call Update_thermal_properties(fl%dDens, fl%mVisc, tm, dm)
    call convert_primary_conservative (dm, fl%dDens, IQ2G, IALL, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)

    fl%dDens0(:, :, :) = fl%dDens(:, :, :)

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
! !==========================================================================================================
!   subroutine write_instantaneous_plane(var, keyword, idom, iter, niter, dtmp)
!     implicit none 
!     real(WP), contiguous, intent(in) :: var( :, :, :)
!     type(DECOMP_INFO), intent(in) :: dtmp
!     character(*), intent(in) :: keyword
!     integer, intent(in) :: idom
!     integer, intent(in) :: iter, niter

!     character(64):: data_flname_path

!     call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter)

!     if(nrank==0) write(*, *) 'Write outlet plane data to ['//trim(data_flname_path)//"]"
 
!     !call decomp_2d_open_io (io_in2outlet, trim(data_flname_path), decomp_2d_write_mode)
!     !call decomp_2d_start_io(io_in2outlet, trim(data_flname_path))!

!     !call decomp_2d_write_outflow(trim(data_flname_path), trim(keyword), niter, var, io_in2outlet, dtmp)
!     !call decomp_2d_write_plane(IPENCIL(1), var, 1, dtmp%xsz(1), trim(data_flname_path), dtmp)
!     call decomp_2d_write_plane(IPENCIL(1), var, data_flname_path, &
!                                 opt_nplanes=niter, &
!                                 opt_decomp = dtmp)
!     !call decomp_2d_end_io(io_in2outlet, trim(data_flname_path))
!     !call decomp_2d_close_io(io_in2outlet, trim(data_flname_path))

!     return
!   end subroutine
!==========================================================================================================
  subroutine write_instantaneous_xoutlet(fl, dm)
    use io_tools_mod
    implicit none 
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(inout) :: dm
    
    character(64):: data_flname_path
    integer :: idom, niter, iter, j

    if(.not. dm%is_record_xoutlet) return
    if(fl%iteration < dm%ndbstart) return

    call append_instantaneous_xoutlet(fl, dm, niter)

    ! if dm%ndbfre = 10, and start from 16
    ! store : 
    !     To store: 16, 17, 18,..., 25 (MOD: 1, 2, ..., 0)
    !        niter:  1,  2,  3,..., 0->10
    !     To store: 26, 27, 28,..., 35 (MOD: 1, 2, ..., 0)
    !        niter:  1,  2,  3,..., 0->10
    !    file name: 10, 20, ... 
    !write(*,*) 'iter, niter', fl%iteration, niter
    if(niter == dm%ndbfre) then
      if( mod(fl%iteration - dm%ndbstart + 1, dm%ndbfre) /= 0 .and. nrank == 0) &
      call Print_warning_msg("niter /= dm%ndbfre, something wrong in writing outlet data")
      iter = (fl%iteration - dm%ndbstart + 1 )/dm%ndbfre * dm%ndbfre
      call write_one_3d_array(dm%fbcx_qx_outl1, 'outlet1_qx', dm%idom, iter, dm%dxcc)
      call write_one_3d_array(dm%fbcx_qx_outl2, 'outlet2_qx', dm%idom, iter, dm%dxcc)
      call write_one_3d_array(dm%fbcx_qy_outl1, 'outlet1_qy', dm%idom, iter, dm%dxpc)
      call write_one_3d_array(dm%fbcx_qy_outl2, 'outlet2_qy', dm%idom, iter, dm%dxpc)
      call write_one_3d_array(dm%fbcx_qz_outl1, 'outlet1_qz', dm%idom, iter, dm%dxcp)
      call write_one_3d_array(dm%fbcx_qz_outl2, 'outlet2_qz', dm%idom, iter, dm%dxcp)
      call write_one_3d_array(dm%fbcx_pr_outl1, 'outlet1_pr', dm%idom, iter, dm%dxcc)
      call write_one_3d_array(dm%fbcx_pr_outl2, 'outlet2_pr', dm%idom, iter, dm%dxcc)
      !if(nrank == 0) write (*,*) " writing outlet database at ", fl%iteration, 'for iter =', iter -  dm%ndbfre, 'to ', iter
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
    use convert_primary_conservative_mod
    implicit none 
    type(t_flow), intent(inout) :: fl
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

    if(dm%is_thermo) then
      call convert_primary_conservative(dm, fl%dDens, IQ2G, IBND)
    end if

    return
  end subroutine
! !==========================================================================================================
!   subroutine read_instantaneous_plane(var, keyword, idom, iter, nfre, dtmp)
!     use decomp_2d_io
!     implicit none 
!     real(WP), contiguous, intent(out) :: var( :, :, :)
!     type(DECOMP_INFO), intent(in) :: dtmp
!     character(*), intent(in) :: keyword
!     integer, intent(in) :: idom
!     integer, intent(in) :: iter
!     integer, intent(in) :: nfre

!     character(64):: data_flname_path, flname

!     call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter, flname)

!     !call decomp_2d_open_io (io_in2outlet, trim(data_flname_path), decomp_2d_read_mode)
!     if(nrank == 0) call Print_debug_inline_msg("Read data on a plane from file: "//trim(data_flname_path))
!     !call decomp_2d_read_inflow(trim(data_flname_path), trim(keyword), nfre, var, io_in2outlet, dtmp)
!     call decomp_2d_read_plane(IPENCIL(1), var, data_flname_path, nfre, &
!                                 opt_decomp = dtmp)

!     !decomp_2d_read_plane(ipencil, var, varname, nplanes, &
!                               !  opt_dirname, &
!                               !  opt_mpi_file_open_info, &
!                               !  opt_mpi_file_set_view_info, &
!                               !  opt_reduce_prec, &
!                               !  opt_decomp, &
!                               !  opt_nb_req, &
!                               !  opt_io)
!     !write(*,*) var
!     !call decomp_2d_close_io(io_in2outlet, trim(data_flname_path))

!     return
!   end subroutine
!==========================================================================================================
  subroutine read_instantaneous_xinlet(fl, dm)
    use typeconvert_mod
    use io_tools_mod
    implicit none 
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    
    character(64):: data_flname_path
    integer :: idom, iter, niter, j, nmax


    if(.not. dm%is_read_xinlet) return


    ! if dm%ndbfre = 10, and start from 16
    ! store : 
    !     To store: 16, 17, 18,..., 25 (MOD: 1, 2, ..., 0)
    !        niter:  1,  2,  3,..., 0
    !    file name: 10, 20, 45 ...

    iter = fl%iteration
    nmax = ((dm%ndbend - dm%ndbstart + 1) / dm%ndbfre) * dm%ndbfre
    if(iter > nmax) then
      iter = mod(iter, nmax) ! database recycle
    end if

    if(mod(iter, dm%ndbfre) == 1 .or. &
       iter == 0) then

      if (iter == 0) then
        niter = dm%ndbfre
      else
        niter = iter + dm%ndbstart - 1
        niter = niter + dm%ndbfre
      end if

      !if(niter > dm%ndbend) niter = dm%ndbstart + dm%ndbfre - 1

      if(nrank == 0) call Print_debug_mid_msg("Read inlet database at iteration "&
        //trim(int2str(iter))//'/'//trim(int2str(niter)))
      call read_one_3d_array(dm%fbcx_qx_inl1, 'outlet1_qx', dm%idom, niter, dm%dxcc)
      call read_one_3d_array(dm%fbcx_qx_inl2, 'outlet2_qx', dm%idom, niter, dm%dxcc)
      call read_one_3d_array(dm%fbcx_qy_inl1, 'outlet1_qy', dm%idom, niter, dm%dxpc)
      call read_one_3d_array(dm%fbcx_qy_inl2, 'outlet2_qy', dm%idom, niter, dm%dxpc)
      call read_one_3d_array(dm%fbcx_qz_inl1, 'outlet1_qz', dm%idom, niter, dm%dxcp)
      call read_one_3d_array(dm%fbcx_qz_inl2, 'outlet2_qz', dm%idom, niter, dm%dxcp)

      dm%is_x_inlet_initialised = .true.

      !call read_one_3d_array(dm%fbcx_pr_inl1, 'outlet1_pr', dm%idom, niter, dm%dxcc)
      !call read_one_3d_array(dm%fbcx_pr_inl2, 'outlet2_pr', dm%idom, niter, dm%dxcc)
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

    if (.not. dm%is_x_inlet_initialised) then
      call Print_error_msg("Inlet not initialised, start iteration must be multiple of ndbfreq")
    end if

    call assign_instantaneous_xinlet(fl, dm) ! every iteration

    return
  end subroutine
end module 
!==========================================================================================================
!==========================================================================================================
module io_field_interpolation_mod
  use udf_type_mod
  USE precision_mod
  implicit none

  type(t_domain) :: domain_tgt
  type(t_flow)   :: flow_tgt
  type(t_thermo) :: thermo_tgt
  character(len = 21) :: input_tgt = 'input_chapsim_tgt.ini'

  private :: Read_input_parameters_tgt
  private :: binary_search_loc2index
  private :: trilinear_interp_point
  private :: build_up_interp_target_field_flow
  public  :: output_interp_target_field

  contains 
!==========================================================================================================
  subroutine Read_input_parameters_tgt(dm, fl, tm, flinput)
    use parameters_constant_mod
    use print_msg_mod
    implicit none
    character(len = *), intent(in) :: flinput 
    type(t_domain), intent(inout) :: dm
    type(t_flow)  , intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm

    integer, parameter :: IOMSG_LEN = 200
    character(len = IOMSG_LEN) :: iotxt
    integer :: ioerr, inputUnit
    integer  :: slen
    character(len = 80) :: secname
    character(len = 80) :: varname
    ! open file1000dd
    open ( newunit = inputUnit, &
           file    = flinput, &
           status  = 'old', &
           action  = 'read', &
           iostat  = ioerr, &
           iomsg   = iotxt )
    if(ioerr /= 0) then
      ! write (*, *) 'Problem openning : ', flinput, ' for reading.'
      ! write (*, *) 'Message: ', trim (iotxt)
      call Print_error_msg('Error in opening the input file: input_chapsim_interp_source.ini')
    end if
    !
    if(nrank == 0) &
    call Print_debug_start_msg("Reading General Parameters from "//flinput//" ...")
    ! reading input
    do 
      ! reading headings/comments
      read(inputUnit, '(a)', iostat = ioerr) secname
      slen = len_trim(secname)
      if (ioerr /=0 ) exit
      if ( (secname(1:1) == ';') .or. &
           (secname(1:1) == '#') .or. &
           (secname(1:1) == ' ') .or. &
           (slen == 0) ) then
        cycle
      end if
      if(nrank == 0) call Print_debug_mid_msg("Reading "//secname(1:slen))
      ! [domain]
      if ( secname(1:slen) == '[domain]' ) then
        read(inputUnit, *, iostat = ioerr) varname, dm%icase
        read(inputUnit, *, iostat = ioerr) varname, dm%lxx
        read(inputUnit, *, iostat = ioerr) varname, dm%lyt
        read(inputUnit, *, iostat = ioerr) varname, dm%lyb
        read(inputUnit, *, iostat = ioerr) varname, dm%lzz
      ! [mesh] 
      else if ( secname(1:slen) == '[mesh]' ) then
        read(inputUnit, *, iostat = ioerr) varname, dm%nc(1)
        read(inputUnit, *, iostat = ioerr) varname, dm%nc(2)
        read(inputUnit, *, iostat = ioerr) varname, dm%nc(3)
        read(inputUnit, *, iostat = ioerr) varname, dm%istret
        read(inputUnit, *, iostat = ioerr) varname, dm%mstret, dm%rstret
        read(inputUnit, *, iostat = ioerr) varname, dm%ifft_lib
      else
        exit
      end if
    end do
    ! end of reading, clearing dummies
    if(.not.IS_IOSTAT_END(ioerr)) &
    call Print_error_msg( 'Problem reading '//flinput // &
    ' in Subroutine: '// "Read_general_input_tgt")

    close(inputUnit)
    return
  end subroutine 
!==========================================================================================================
  SUBROUTINE binary_search_loc2index(x_target, x_array, idx)
    IMPLICIT NONE
    REAL(WP), INTENT(IN) :: x_target
    REAL(WP), INTENT(IN) :: x_array(:)
    INTEGER, INTENT(OUT) :: idx
    !
    INTEGER :: n, left, right, mid
    !
    n = size(x_array, 1)
    ! Handle boundary cases
    IF (x_target <= x_array(1)) THEN
      idx = 1
      RETURN
    END IF
    IF (x_target >= x_array(n)) THEN
      idx = n - 1
      RETURN
    END IF
    ! Binary search
    left = 1
    right = n
    DO WHILE (right - left > 1)
      mid = (left + right) / 2
      IF (x_target <= x_array(mid)) THEN
        right = mid
      ELSE
        left = mid
      END IF
    END DO
    idx = left
    RETURN
  END SUBROUTINE binary_search_loc2index
!==========================================================================================================
  SUBROUTINE trilinear_interp_point(x_target, y_target, z_target, &
                                        x_src, y_src, z_src, var_src, &
                                        var_interp)
    USE parameters_constant_mod
    IMPLICIT NONE
    REAL(WP), INTENT(IN) :: x_target, y_target, z_target
    REAL(WP), INTENT(IN) :: x_src(:), y_src(:), z_src(:)
    REAL(WP), INTENT(IN) :: var_src(:, :, :)
    REAL(WP), INTENT(OUT) :: var_interp

    INTEGER :: i_src, j_src, k_src, nx, ny, nz
    REAL(WP) :: xi, eta, zeta
    REAL(WP) :: dx, dy, dz
    REAL(WP) :: c000, c001, c010, c011, c100, c101, c110, c111
    REAL(WP) :: c00, c01, c10, c11, c0, c1

    nx = SIZE(x_src)
    ny = SIZE(y_src)
    nz = SIZE(z_src)

    !-------------------------------------------------------------
    ! Find enclosing cell indices
    !-------------------------------------------------------------
    CALL binary_search_loc2index(x_target, x_src, i_src)
    CALL binary_search_loc2index(y_target, y_src, j_src)
    CALL binary_search_loc2index(z_target, z_src, k_src)

    ! Clamp indices to valid range (ensure i_src+1 <= nx)
    i_src = MIN(i_src, nx-1)
    j_src = MIN(j_src, ny-1)
    k_src = MIN(k_src, nz-1)

    !-------------------------------------------------------------
    ! Compute normalized coordinates within the cell
    !-------------------------------------------------------------
    dx = x_src(i_src+1) - x_src(i_src)
    dy = y_src(j_src+1) - y_src(j_src)
    dz = z_src(k_src+1) - z_src(k_src)

    ! Avoid division by zero
    IF (dabs(dx) <= MINP) dx = 1.0_wp
    IF (dabs(dy) <= MINP) dy = 1.0_wp
    IF (dabs(dz) <= MINP) dz = 1.0_wp

    xi   = (x_target - x_src(i_src)) / dx
    eta  = (y_target - y_src(j_src)) / dy
    zeta = (z_target - z_src(k_src)) / dz

    ! Clamp normalized coordinates to [0,1] to avoid extrapolation outside last cell
    xi   = MAX(0.0_wp, MIN(1.0_wp, xi))
    eta  = MAX(0.0_wp, MIN(1.0_wp, eta))
    zeta = MAX(0.0_wp, MIN(1.0_wp, zeta))

    !-------------------------------------------------------------
    ! Trilinear interpolation
    !-------------------------------------------------------------
    c000 = var_src(i_src  , j_src  , k_src  )
    c001 = var_src(i_src  , j_src  , k_src+1)
    c010 = var_src(i_src  , j_src+1, k_src  )
    c011 = var_src(i_src  , j_src+1, k_src+1)
    c100 = var_src(i_src+1, j_src  , k_src  )
    c101 = var_src(i_src+1, j_src  , k_src+1)
    c110 = var_src(i_src+1, j_src+1, k_src  )
    c111 = var_src(i_src+1, j_src+1, k_src+1)

    ! Interpolate in z
    c00 = c000*(1.0_wp - zeta) + c001*zeta
    c01 = c010*(1.0_wp - zeta) + c011*zeta
    c10 = c100*(1.0_wp - zeta) + c101*zeta
    c11 = c110*(1.0_wp - zeta) + c111*zeta

    ! Interpolate in y
    c0 = c00*(1.0_wp - eta) + c01*eta
    c1 = c10*(1.0_wp - eta) + c11*eta

    ! Interpolate in x
    var_interp = c0*(1.0_wp - xi) + c1*xi
    RETURN
  END SUBROUTINE trilinear_interp_point
!==========================================================================================================
  subroutine build_up_interp_target_field_flow(fl_src, dm_src, fl_tgt, dm_tgt)
    use udf_type_mod
    use parameters_constant_mod
    implicit none
    type(t_domain), intent(in)    :: dm_src  ! source
    type(t_flow)  , intent(in)    :: fl_src  ! source
    type(t_domain), intent(inout) :: dm_tgt  ! target
    type(t_flow)  , intent(inout) :: fl_tgt  ! target
    !
    type(DECOMP_INFO) :: dtmp
    integer :: i, j, k, ii, jj, kk
    real(WP) :: xc_src(dm_src%nc(1))
    real(WP) :: zc_src(dm_src%nc(3))
    real(WP) :: xp_src(dm_src%np(1))
    real(WP) :: zp_src(dm_src%np(3))
    real(WP) :: x_target, y_target, z_target
    real(WP) :: var_target
    ! 
    ! x-center and x-face
    xc_src = dm_src%h(1) * ( [(REAL(i-1,WP)+HALF, i=1,dm_src%nc(1))] )
    xp_src = dm_src%h(1) * ( [(REAL(i-1,WP),      i=1,dm_src%np(1))] )
    ! z-center and z-face
    zc_src = dm_src%h(3) * ( [(REAL(k-1,WP)+HALF, k=1,dm_src%nc(3))] )
    zp_src = dm_src%h(3) * ( [(REAL(k-1,WP),      k=1,dm_src%np(3))] )
    ! ux 
    dtmp = dm_tgt%dpcc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      z_target = dm_tgt%h(3) * (REAL(kk - 1, WP) + HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        y_target = dm_tgt%yc(jj)
        do i = 1, dtmp%xsz(1)
          x_target = dm_tgt%h(1) * REAL(i - 1, WP)
          call trilinear_interp_point(x_target, y_target, z_target, &
                                      xp_src(:), dm_src%yc(:), zc_src(:), fl_src%qx(:, :, :), var_target)
          fl_tgt%qx(i, j, k) = var_target
        end do
      end do
    end do
    ! uy
    dtmp = dm_tgt%dcpc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      z_target = dm_tgt%h(3) * (REAL(kk - 1, WP) + HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        y_target = dm_tgt%yp(jj)
        do i = 1, dtmp%xsz(1)
          x_target = dm_tgt%h(1) * (REAL(i - 1, WP) + HALF)
          call trilinear_interp_point(x_target, y_target, z_target, &
                                      xc_src(:), dm_src%yp(:), zc_src(:), fl_src%qy(:, :, :), var_target)
          fl_tgt%qy(i, j, k) = var_target
        end do
      end do
    end do
    ! uz
    dtmp = dm_tgt%dccp
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      z_target = dm_tgt%h(3) * (REAL(kk - 1, WP))
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        y_target = dm_tgt%yc(jj)
        do i = 1, dtmp%xsz(1)
          x_target = dm_tgt%h(1) * (REAL(i - 1, WP) + HALF)
          call trilinear_interp_point(x_target, y_target, z_target, &
                                      xc_src(:), dm_src%yc(:), zp_src(:), fl_src%qz(:, :, :), var_target)
          fl_tgt%qz(i, j, k) = var_target
        end do
      end do
    end do
    ! p
    dtmp = dm_tgt%dccc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      z_target = dm_tgt%h(3) * (REAL(kk - 1, WP)+ HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        y_target = dm_tgt%yc(jj)
        do i = 1, dtmp%xsz(1)
          x_target = dm_tgt%h(1) * (REAL(i - 1, WP) + HALF)
          call trilinear_interp_point(x_target, y_target, z_target, &
                                      xc_src(:), dm_src%yc(:), zc_src(:), fl_src%pres(:, :, :), var_target)
          fl_tgt%pres(i, j, k) = var_target
        end do
      end do
    end do
    return
  end subroutine
!==========================================================================================================
  subroutine build_up_interp_target_field_thermo(tm_src, dm_src, tm_tgt, dm_tgt)
    use udf_type_mod
    use parameters_constant_mod
    implicit none
    type(t_domain), intent(in)    :: dm_src  ! source
    type(t_thermo), intent(in)    :: tm_src  ! source
    type(t_domain), intent(inout) :: dm_tgt  ! target
    type(t_thermo), intent(inout) :: tm_tgt  ! target
    !
    type(DECOMP_INFO) :: dtmp
    integer :: i, j, k, ii, jj, kk
    real(WP) :: xc_src(dm_src%nc(1))
    real(WP) :: zc_src(dm_src%nc(3))
    real(WP) :: x_target, y_target, z_target
    real(WP) :: var_target
    !
    ! xc
    do i = 1, dm_src%nc(1)
      xc_src(i) = dm_src%h(1) * (REAL(i - 1, WP) + HALF)
    end do
    ! zc
    do k = 1, dm_src%nc(3)
      zc_src(k) = dm_src%h(3) * (REAL(k - 1, WP) + HALF)
    end do
    ! scalars
    dtmp = dm_tgt%dccc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      z_target = dm_tgt%h(3) * (REAL(kk - 1, WP)+ HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        y_target = dm_tgt%yc(jj)
        do i = 1, dtmp%xsz(1)
          x_target = dm_tgt%h(1) * (REAL(i - 1, WP) + HALF)
          call trilinear_interp_point(x_target, y_target, z_target, &
                                      xc_src(:), dm_src%yc(:), zc_src(:), tm_src%rhoh(:, :, :), var_target)
          tm_tgt%rhoh(i, j, k) = var_target
          call trilinear_interp_point(x_target, y_target, z_target, &
                                      xc_src(:), dm_src%yc(:), zc_src(:), tm_src%tTemp(:, :, :), var_target)
          tm_tgt%tTemp(i, j, k) = var_target
        end do
      end do
    end do
    return
  end subroutine
!==========================================================================================================
  subroutine output_interp_target_field(dm_src, fl_src, tm_src)
    use parameters_constant_mod
    use udf_type_mod
    use io_files_mod
    use print_msg_mod
    use input_general_mod
    use geometry_mod
    use domain_decomposition_mod
    use io_restart_mod
    use io_visualisation_mod
    implicit none 
    type(t_domain), intent(in) :: dm_src
    type(t_flow)  , intent(in) :: fl_src
    type(t_thermo), intent(in), optional :: tm_src

    if(.not.file_exists(trim(input_tgt))) then
      call Print_warning_msg('No field interpolation is carried out.')
      return
    end if
    
    if(nproc > 1) call Print_error_msg('Field interpolation and io are in serial mode only.')
    ! geo/domain
    call Read_input_parameters_tgt(domain_tgt,flow_tgt, thermo_tgt, input_tgt)
    domain_tgt%is_periodic(:) = dm_src%is_periodic(:)
    domain_tgt%ibcx_qx = dm_src%ibcx_qx
    domain_tgt%ibcy_qy = dm_src%ibcy_qy
    domain_tgt%ibcz_qz = dm_src%ibcz_qz
    call Buildup_geometry_mesh_info(domain_tgt)
    call initialise_domain_decomposition(domain_tgt)
    ! allocate variables
    call alloc_x(flow_tgt%qx,   domain_tgt%dpcc) ; flow_tgt%qx = ZERO
    call alloc_x(flow_tgt%qy,   domain_tgt%dcpc) ; flow_tgt%qy = ZERO
    call alloc_x(flow_tgt%qz,   domain_tgt%dccp) ; flow_tgt%qz = ZERO
    call alloc_x(flow_tgt%pres, domain_tgt%dccc) ; flow_tgt%pres = ZERO
    ! interpolation from src to target
    call build_up_interp_target_field_flow(fl_src, dm_src, flow_tgt, domain_tgt)
    ! write out
    call write_instantaneous_flow(flow_tgt, domain_tgt)
    !call write_visu_flow(flow_tgt, domain_tgt)
    if(domain_tgt%is_thermo) then
      call alloc_x(thermo_tgt%rhoh,  domain_tgt%dccc) ; thermo_tgt%rhoh = ZERO
      call alloc_x(thermo_tgt%tTemp, domain_tgt%dccc) ; thermo_tgt%tTemp = ZERO
      call build_up_interp_target_field_thermo(tm_src, dm_src, thermo_tgt, domain_tgt)
      call write_instantaneous_thermo(thermo_tgt, domain_tgt)
      !call write_visu_thermo(thermo_tgt, flow_tgt, domain_tgt)
    end if 

    call Print_debug_mid_msg("Fields interpolation is completed successfully.")

    return
  end subroutine
end module 