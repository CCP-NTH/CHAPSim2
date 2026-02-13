
!==========================================================================================================
! Periodic averaging refactor
!
! What this fixes vs your current version:
!   - No duplicated hand-written loops for each periodicity pattern
!   - One clear dispatcher that decides WHAT to average (bulk / 1D profile / 2D plane)
!   - One set of reusable reducers:
!        mean_over_dir_1d():    average over 2 directions -> 1D profile
!        mean_over_dir_2d():    average over 1 direction  -> 2D plane (stored as 3D with size=1)
!
!==========================================================================================================

module visualisation_spatial_average_mod
  use parameters_constant_mod
  use print_msg_mod
  implicit none
  private

  integer, parameter :: XDIR=1, YDIR=2, ZDIR=3

  public  :: write_visu_savg_bin_and_xdmf
  private :: write_visu_profile
  private :: mean_over_two_dirs_to_profile
  private :: mean_over_one_dir_to_plane
  private :: mean_data_xpencil_over_xdir
  private :: mean_data_ypencil_over_ydir 
  private :: mean_data_zpencil_over_zdir 
  private :: extract_profile_from_ypencil 
  private :: remaining_dir

contains
  subroutine write_visu_savg_bin_and_xdmf(dm, data_in, field_name, visuname, iter)
    use udf_type_mod
    use decomp_2d
    use visualisation_field_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP),        intent(in) :: data_in(:,:,:)
    character(*),    intent(in) :: field_name, visuname
    integer,         intent(in) :: iter

    logical :: px, py, pz
    real(WP), allocatable :: prof(:)
    real(WP), allocatable :: savg_data(:,:,:)
    type(DECOMP_INFO) :: dtmp

    px = dm%is_periodic(XDIR)
    py = dm%is_periodic(YDIR)
    pz = dm%is_periodic(ZDIR)
    dtmp = dm%dccc
    !--------------------------------------------
    ! point
    !--------------------------------------------
    if (px .and. py .and. pz) then
      ! All periodic: usually only a bulk value makes sense.
      ! Keep behaviour: do nothing here (or add a bulk output if you want).
      return
    end if
    !--------------------------------------------
    ! two direction periodic -> 1D profile
    !--------------------------------------------
    ! Case: X and Z periodic, Y bounded -> produce Y-profile (mean over X and Z)
    if (px .and. pz .and. (.not. py)) then
      allocate(prof(dtmp%ysz(2)))
      call mean_over_two_dirs_to_profile(data_in, dm, YDIR, prof)
      call write_visu_profile(dm, prof, trim(field_name), YDIR, iter)
      deallocate(prof)
      return
    end if

    ! Case: X and Y periodic, Z bounded -> produce Z-profile (mean over X and Y)
    if (px .and. py .and. (.not. pz)) then
      allocate(prof(dtmp%zsz(3)))
      call mean_over_two_dirs_to_profile(data_in, dm, ZDIR, prof)
      call write_visu_profile(dm, prof, trim(field_name), ZDIR, iter)
      deallocate(prof)
      return
    end if

    ! Case: Y and Z periodic, X bounded -> produce X-profile (mean over Y and Z)
    if (py .and. pz .and. (.not. px)) then
      allocate(prof(dtmp%xsz(1)))
      call mean_over_two_dirs_to_profile(data_in, dm, XDIR, prof)
      call write_visu_profile(dm, prof, trim(field_name), XDIR, iter)
      deallocate(prof)
      return
    end if
    !--------------------------------------------
    ! one direction periodic -> 2d plane
    !--------------------------------------------
    ! Case: X periodic only -> average over X, keep YZ plane
    if (px .and. (.not. py) .and. (.not. pz)) then
      allocate(savg_data(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)))
      call mean_over_one_dir_to_plane(data_in, dm, XDIR, savg_data)
      call write_visu_plane_binary_and_xdmf(dm, savg_data, field_name, visuname, 1, 0, iter) 
      deallocate(savg_data)
      return
    end if

    ! Case: Y periodic only -> not supported
    if ((.not. px) .and. py .and. (.not. pz)) then
      allocate(savg_data(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)))
      call mean_over_one_dir_to_plane(data_in, dm, YDIR, savg_data)
      call write_visu_plane_binary_and_xdmf(dm, savg_data, field_name, visuname, 2, 0, iter) 
      deallocate(savg_data)
      return
    end if

    ! Case: Z periodic only -> average over Z, keep XY plane
    if ((.not. px) .and. (.not. py) .and. pz) then
      allocate(savg_data(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)))
      call mean_over_one_dir_to_plane(data_in, dm, ZDIR, savg_data)
      call write_visu_plane_binary_and_xdmf(dm, savg_data, field_name, visuname, 3, 0, iter) 
      deallocate(savg_data)
      return
    end if

    ! Other mixed periodicities not handled in your original logic:
    ! - X&Y periodic, Z bounded
    ! - Y periodic only, etc.
    ! Add cases here if you need them later.
    return

  end subroutine write_visu_savg_bin_and_xdmf

  subroutine write_visu_profile(dm, prof, varname, dir, iter)
    use precision_mod
    use decomp_2d
    use decomp_2d_io
    use udf_type_mod, only: t_domain
    use io_tools_mod
    use decomp_operation_mod
    use typeconvert_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: prof(:)
    character(len=*), intent(in) :: varname
    integer, intent(in) :: dir
    integer, intent(in), optional :: iter

    character(64):: data_flname
    character(64):: data_flname_path
    character(64):: visu_flname_path
    character(64):: keyword
    integer :: nsz(3)
    integer :: ioxdmf, iofl
    type(DECOMP_INFO) :: dtmp

    integer :: j

    dtmp = dm%dccc
!----------------------------------------------------------------------------------------------------------
! write data 
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then
      keyword = trim(varname)
      call generate_pathfile_name(data_flname_path, dm%idom, keyword, dir_data, 'dat', iter)
      open(newunit = iofl, file = data_flname_path, action = "write", status="replace")
      select case (dir)
      case (XDIR)
        do j = 1, dtmp%xsz(1)
          write(iofl, *) j, (j + HALF) * dm%h(1), prof(j) 
        end do
      case (YDIR)
        do j = 1, dtmp%ysz(2)
          write(iofl, *) j, dm%yc(j), prof(j) 
        end do
      case (ZDIR)
        do j = 1, dtmp%zsz(3)
          write(iofl, *) j, (j + HALF) * dm%h(3), prof(j) 
        end do
      end select
      
      close(iofl)
    end if

    return
  end subroutine 
  !========================================================================================================
  ! Average over two directions -> return 1D profile along the remaining direction.
  !
  ! Example: mean over X and Z -> profile along Y.
  ! Implementation strategy:
  !   - Compute mean over first direction in current pencil when possible.
  !   - Transpose as needed to average over second direction.
  !   - Return 1D vector of the remaining coordinate.
  !========================================================================================================
  subroutine mean_over_two_dirs_to_profile(data_xpencil, dm, dir, profile_out)
    use udf_type_mod
    use decomp_2d
    implicit none
    real(WP), intent(in)          :: data_xpencil(:,:,:)
    type(t_domain), intent(in)    :: dm
    integer, intent(in)           :: dir
    real(WP), allocatable, intent(out) :: profile_out(:)

    real(WP), allocatable :: tmp_x(:,:,:), tmp_y(:,:,:), tmp_z(:,:,:)
    type(DECOMP_INFO) :: dtmp

    dtmp = dm%dccc
    select case(dir)
    case (XDIR)
      ! Average over YZ to get X-profile
      allocate(tmp_x(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)))
      allocate(tmp_y(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)))
      allocate(tmp_z(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)))
      call transpose_x_to_y(data_xpencil, tmp_y, dtmp)
      call mean_data_ypencil_over_ydir(tmp_y, dtmp)
      call transpose_y_to_z(tmp_y, tmp_z, dtmp)
      call mean_data_zpencil_over_zdir(tmp_z, dtmp)
      call transpose_z_to_y(tmp_z, tmp_y, dtmp)
      call transpose_y_to_x(tmp_y, tmp_x, dtmp)
      profile_out = tmp_x(:, 1, 1)
      deallocate(tmp_x, tmp_y, tmp_z)

    case (YDIR)
      ! Average over XZ to get Y-profile
      allocate(tmp_x(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)))
      allocate(tmp_y(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)))
      allocate(tmp_z(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)))
      tmp_x = data_xpencil
      call mean_data_xpencil_over_xdir(tmp_x, dtmp)
      call transpose_x_to_y(tmp_x, tmp_y, dtmp)
      call transpose_y_to_z(tmp_y, tmp_z, dtmp)
      call mean_data_zpencil_over_zdir(tmp_z, dtmp)
      call transpose_z_to_y(tmp_z, tmp_y, dtmp)
      profile_out = tmp_y(1, :, 1)
      deallocate(tmp_x, tmp_y, tmp_z)

    case (ZDIR)
      ! Average over XY to get Z-profile
      allocate(tmp_x(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)))
      allocate(tmp_y(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)))
      allocate(tmp_z(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)))
      tmp_x = data_xpencil
      call mean_data_xpencil_over_xdir(tmp_x, dtmp)
      call transpose_x_to_y(tmp_x, tmp_y, dtmp)
      call mean_data_ypencil_over_ydir(tmp_y, dtmp)
      call transpose_y_to_z(tmp_y, tmp_z, dtmp)
      profile_out = tmp_z(1, 1, :)
      deallocate(tmp_x, tmp_y, tmp_z)

    case default
      call Print_error_msg("mean_over_two_dirs_to_profile: invalid dir")
      allocate(profile_out(0))
    end select

  end subroutine mean_over_two_dirs_to_profile
  !========================================================================================================
  ! Average over one direction -> return a 2D plane as a 3D array with size 1 in that direction.
  !
  ! Example:
  !   - mean over X -> YZ-plane (shape: (1, Ny, Nz) in x-pencil conceptually)
  !   - mean over Z -> XY-plane (shape: (Nx, Ny, 1))
  !
  !========================================================================================================
  subroutine mean_over_one_dir_to_plane(data_xpencil, dm, dirAvg, plane_out)
    use udf_type_mod
    use decomp_2d
    implicit none
    real(WP), intent(in)          :: data_xpencil(:,:,:)
    
    type(t_domain), intent(in)    :: dm
    integer, intent(in)           :: dirAvg
    real(WP), allocatable, intent(out) :: plane_out(:,:,:)

    real(WP), allocatable :: tmp_y(:,:,:), tmp_z(:,:,:)
    real(WP), allocatable :: avg(:,:,:)
    type(DECOMP_INFO) :: dtmp

    dtmp = dm%dccc
    select case(dirAvg)
    case (XDIR)
      ! Average over X in x-pencil directly
      allocate(avg(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)))
      avg = data_xpencil
      call mean_data_xpencil_over_xdir(avg, dtmp)
      plane_out = avg
      deallocate(avg)

    case (YDIR)
      ! Need y-pencil to average over Y then return to x-pencil
      allocate(tmp_y(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)))
      call transpose_x_to_y(data_xpencil, tmp_y, dtmp)
      call mean_data_ypencil_over_ydir(tmp_y, dtmp)

      allocate(plane_out(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)))
      call transpose_y_to_x(tmp_y, plane_out, dtmp)
      deallocate(tmp_y)

    case (ZDIR)
      ! Need z-pencil to average over Z then return to x-pencil
      allocate(tmp_y(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)))
      allocate(tmp_z(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)))
      call transpose_x_to_y(data_xpencil, tmp_y, dtmp)
      call transpose_y_to_z(tmp_y, tmp_z, dtmp)
      call mean_data_zpencil_over_zdir(tmp_z, dtmp)

      call transpose_z_to_y(tmp_z, tmp_y, dtmp)
      allocate(plane_out(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)))
      call transpose_y_to_x(tmp_y, plane_out, dtmp)

      deallocate(tmp_y, tmp_z)

    case default
      call Print_error_msg("mean_over_one_dir_to_plane: invalid dirAvg")
      allocate(plane_out(0,0,0))
    end select

  end subroutine mean_over_one_dir_to_plane
  !========================================================================================================
  ! In-place mean reducers that keep array shape but broadcast the mean along averaged dimension.
  ! This makes subsequent transposes trivial and avoids changing interface signatures.
  !========================================================================================================
  subroutine mean_data_xpencil_over_xdir(data_xpencil, dtmp)
    use decomp_2d
    implicit none
    real(WP), intent(inout) :: data_xpencil(:,:,:)
    type(DECOMP_INFO), intent(in) :: dtmp
    integer :: i,j,k
    real(WP) :: s

    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        s = sum(data_xpencil(:,j,k)) / real(dtmp%xsz(1), WP)
        data_xpencil(:,j,k) = s
      end do
    end do
    return
  end subroutine mean_data_xpencil_over_xdir
!========================================================================================================
  subroutine mean_data_ypencil_over_ydir(data_ypencil, dtmp)
    use decomp_2d
    implicit none
    real(WP), intent(inout) :: data_ypencil(:,:,:)
    type(DECOMP_INFO), intent(in) :: dtmp
    integer :: i,j,k
    real(WP) :: s

    do i = 1, dtmp%ysz(1)
      do k = 1, dtmp%ysz(3)
        s = sum(data_ypencil(i,:,k)) / real(dtmp%ysz(2), WP)
        data_ypencil(i,:,k) = s
      end do
    end do
  end subroutine mean_data_ypencil_over_ydir
!========================================================================================================
  subroutine mean_data_zpencil_over_zdir(data_zpencil, dtmp)
    use decomp_2d
    implicit none
    real(WP), intent(inout) :: data_zpencil(:,:,:)
    type(DECOMP_INFO), intent(in) :: dtmp
    integer :: i,j,k
    real(WP) :: s
    !
    do i = 1, dtmp%zsz(1)
      do j = 1, dtmp%zsz(2)
        s = sum(data_zpencil(i,j,:)) / real(dtmp%zsz(3), WP)
        data_zpencil(i,j,:) = s
      end do
    end do
    return
  end subroutine mean_data_zpencil_over_zdir


  !========================================================================================================
  ! Extract a 1D profile from y-pencil data after averaging.
  ! For your primary supported case (mean over X & Z), remaining is YDIR:
  !   profile(j) = a(1,j,1)
  !
  ! If you later add other cases, extend the select below.
  !========================================================================================================
  subroutine extract_profile_from_ypencil(a_ypencil, dtmp, dirRemain, profile_out)
    use decomp_2d
    implicit none
    real(WP), intent(in) :: a_ypencil(:,:,:)
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in) :: dirRemain
    real(WP), allocatable, intent(out) :: profile_out(:)

    select case(dirRemain)
    case (YDIR)
      allocate(profile_out(dtmp%ysz(2)))
      profile_out = a_ypencil(1,:,1)
    case default
      call Print_error_msg("extract_profile_from_ypencil: remaining dir not supported")
      allocate(profile_out(0))
    end select
  end subroutine extract_profile_from_ypencil


  pure function remaining_dir(dirA, dirB) result(dirR)
    integer, intent(in) :: dirA, dirB
    integer :: dirR
    integer :: s
    s = dirA + dirB
    dirR = XDIR + YDIR + ZDIR - s
  end function remaining_dir

end module visualisation_spatial_average_mod
!==========================================================================================================
module statistics_mod
  use print_msg_mod
  use parameters_constant_mod
  implicit none

  character(13), parameter :: io_name = "statistics-io"
  integer, allocatable :: ncl_stat(:, :)
  !
  integer, parameter :: STATS_READ  = 1
  integer, parameter :: STATS_WRITE = 2
  integer, parameter :: STATS_TAVG  = 3
  integer, parameter :: STATS_VISU3 = 4
  integer, parameter :: STATS_VISU1 = 5
  !
  private :: run_stats_action
  private :: run_stats_loops1
  private :: run_stats_loops3
  private :: run_stats_loops6
  private :: run_stats_loops10
  private :: run_stats_loops45
  !
  public  :: init_stats_flow
  public  :: init_stats_thermo
  public  :: init_stats_mhd
  !
  public  :: update_stats_flow
  public  :: update_stats_thermo
  public  :: update_stats_mhd
  !
  public  :: write_stats_flow
  public  :: write_stats_thermo
  public  :: write_stats_mhd
  !
  public  :: write_visu_stats_flow
  public  :: write_visu_stats_thermo
  public  :: write_visu_stats_mhd
contains
!==========================================================================================================
  subroutine run_stats_action(mode, accc_tavg, field_name, iter, dm, opt_accc, opt_visnm)
    use typeconvert_mod
    use udf_type_mod
    use visualisation_field_mod
    use visualisation_spatial_average_mod
    use io_tools_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    integer, intent(in) :: iter
    real(WP), contiguous, intent(inout) :: accc_tavg(:, :, :) 
    character(len=*), intent(in), optional :: opt_visnm
    real(WP), intent(in), optional :: opt_accc(:, :, :) 
    type(t_domain), intent(in) :: dm
    !
    real(WP) :: ac, am
    integer :: nstat
    !
    select case(mode)
    case(STATS_READ)
      call read_one_3d_array(accc_tavg, trim(field_name), dm%idom, iter, dm%dccc)
      !
    case(STATS_WRITE)
      call write_one_3d_array(accc_tavg, trim(field_name), dm%idom, iter, dm%dccc)
      !
    case(STATS_TAVG)
      if(.not. present(opt_accc)) call Print_error_msg("Error. Need Time Averaged Value.")
      nstat = iter - dm%stat_istart + 1
      ac = ONE / real(nstat, WP)
      am = real(nstat - 1, WP) / real(nstat, WP)
      accc_tavg = am * accc_tavg + ac * opt_accc
      !
    case(STATS_VISU3)
      call write_visu_field_bin_and_xdmf(dm, accc_tavg, field_name, trim(opt_visnm), iter, 0)
    case(STATS_VISU1)
      call write_visu_savg_bin_and_xdmf(dm, accc_tavg, trim(field_name),  trim(opt_visnm), iter)
      !
    case default
      call Print_error_msg("This action mode is not supported.")
      !
    end select
    return
  end subroutine
!==========================================================================================================
  subroutine run_stats_loops1(mode, accc_tavg, field_name, iter, dm, opt_accc1, opt_accc0, opt_visnm)
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    real(WP), dimension(:, :, :), contiguous, intent(inout) :: accc_tavg
    real(WP), dimension(:, :, :), intent(in), optional :: opt_accc1, opt_accc0
    integer, intent(in) :: iter
    type(t_domain), intent(in) :: dm
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: opt_accc
    !
    if(mode == STATS_TAVG) then
      if (.not. present(opt_accc1)) call Print_error_msg("Error in run_stats_loops1.")
      if(present(opt_accc0)) then
        opt_accc(:, :, :) = opt_accc1(:, :, :) * opt_accc0(:, :, :)
      else
        opt_accc(:, :, :) = opt_accc1(:, :, :)
      end if
    end if
    call run_stats_action(mode, accc_tavg, trim(field_name), iter, dm, opt_accc, opt_visnm)
    if(mode == STATS_TAVG) &
    accc_tavg(:, :, :) = accc_tavg(:, :, :)
    return
  end subroutine
!==========================================================================================================
  subroutine run_stats_loops3(mode, acccn_tavg, field_name, iter, dm, opt_acccn1, opt_accc0, opt_visnm)
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    real(WP), dimension(:, :, :, :), intent(inout) :: acccn_tavg
    real(WP), dimension(:, :, :, :), intent(in), optional :: opt_acccn1
    real(WP), dimension(:, :, :),    intent(in), optional :: opt_accc0
    integer, intent(in) :: iter
    type(t_domain), intent(in) :: dm
    integer :: i
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: accc_tavg, opt_accc
    !
    do i = 1, 3
      accc_tavg(:, :, :) = acccn_tavg(:, :, :, i)
      if(mode == STATS_TAVG) then
        if (.not. present(opt_acccn1)) call Print_error_msg("Error in run_stats_loops3.")
        if(present(opt_accc0)) then
          opt_accc(:, :, :) = opt_acccn1(:, :, :, i) * opt_accc0(:, :, :)
        else
          opt_accc(:, :, :) = opt_acccn1(:, :, :, i)
        end if
      end if
      call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i)), iter, dm, opt_accc, opt_visnm)
      if(mode == STATS_TAVG)&
      acccn_tavg(:, :, :, i) = accc_tavg(:, :, :)
    end do
    return
  end subroutine
!==========================================================================================================
  subroutine run_stats_loops6(mode, acccn_tavg, field_name, iter, dm, opt_acccn1, opt_acccn2, opt_accc0, opt_visnm)
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    real(WP), dimension(:, :, :, :), intent(inout) :: acccn_tavg
    real(WP), dimension(:, :, :, :), intent(in), optional :: opt_acccn1, opt_acccn2
    real(WP), dimension(:, :, :),    intent(in), optional :: opt_accc0
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer :: n, i, j
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: accc_tavg, opt_accc
    !
    n = 0
    do i = 1, 3
      do j = i, 3
        n = n + 1
        if (n <= 6) then
          accc_tavg(:, :, :) = acccn_tavg(:, :, :, n)
          if(mode == STATS_TAVG) then
            if (.not. present(opt_acccn1)) call Print_error_msg("Error in run_stats_loops6.")
            if (.not. present(opt_acccn2)) call Print_error_msg("Error in run_stats_loops6.")
            opt_accc(:, :, :) = opt_acccn1(:, :, :, i) * opt_acccn2(:, :, :, j)
            if(present(opt_accc0)) &
            opt_accc(:, :, :) = opt_accc(:, :, :) * opt_accc0(:, :, :)
          end if
          call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i))//trim(int2str(j)), iter, dm, opt_accc, opt_visnm)
          if(mode == STATS_TAVG)&
          acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
        end if
      end do
    end do
    return
  end subroutine
!==========================================================================================================
  subroutine run_stats_loops10(mode, acccn_tavg, field_name, iter, dm, opt_acccn1, opt_acccn2, opt_acccn3, opt_accc0, opt_visnm)
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    real(WP), dimension(:, :, :, :), intent(inout) :: acccn_tavg
    real(WP), dimension(:, :, :, :), intent(in), optional :: opt_acccn1, opt_acccn2, opt_acccn3
    real(WP), dimension(:, :, :),    intent(in), optional :: opt_accc0
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer :: n, i, j, k
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: accc_tavg, opt_accc
    !
    !   third-order correlation: <u_i * u_j * u_k> 
    !   (1,1,1); (1,1,2); (1,1,3); (1,2,2); (1,2,3); index(1-5)
    !   (1,3,3); (2,2,2); (2,2,3); (2,3,3); (3,3,3); index(6-10)
    !
    n = 0
      do i = 1, 3
        do j = i, 3
          do k = j, 3
            n = n + 1
            if(n <= 10) then
              accc_tavg(:, :, :) = acccn_tavg(:, :, :, n)
              if(mode == STATS_TAVG) then
                if (.not. present(opt_acccn1)) call Print_error_msg("Error in run_stats_loops10.")
                if (.not. present(opt_acccn2)) call Print_error_msg("Error in run_stats_loops10.")
                if (.not. present(opt_acccn3)) call Print_error_msg("Error in run_stats_loops10.")
                opt_accc(:, :, :) = opt_acccn1(:, :, :, i) * opt_acccn2(:, :, :, j) * opt_acccn3(:, :, :, k)
                if(present(opt_accc0)) &
                opt_accc(:, :, :) = opt_accc(:, :, :) * opt_accc0(:, :, :)
              end if
              call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i))//trim(int2str(j))//trim(int2str(k)), iter, dm, opt_accc, opt_visnm)
              if(mode == STATS_TAVG)&
              acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
            end if
          end do
        end do
      end do
      return
  end subroutine
!==========================================================================================================
  subroutine run_stats_loops45(mode, acccn_tavg, field_name, iter, dm, opt_acccnn1, opt_acccnn2, opt_accc0, opt_visnm)
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    real(WP), dimension(:, :, :, :),    intent(inout) :: acccn_tavg
    real(WP), dimension(:, :, :, :, :), intent(in), optional :: opt_acccnn1, opt_acccnn2
    real(WP), dimension(:, :, :),       intent(in), optional :: opt_accc0
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer :: n, i, j, ij, s, l, sl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: accc_tavg, opt_accc
!----------------------------------------------------------------------------------------------------------
    ![1,1,1,1]; (1,1,1,2); (1,1,1,3); [1,1,2,1]; (1,1,2,2); (1,1,2,3); [1,1,3,1]; (1,1,3,2); (1,1,3,3); index (01-09)
    ![1,2,1,2]; (1,2,1,3); (1,2,2,1); [1,2,2,2]; (1,2,2,3); (1,2,3,1); [1,2,3,2]; (1,2,3,3); [1,3,1,3]; index (10-18)
    !(1,3,2,1); (1,3,2,2); [1,3,2,3]; (1,3,3,1); (1,3,3,2); [1,3,3,3]; [2,1,2,1]; (2,1,2,2); (2,1,2,3); index (19-27)
    ![2,1,3,1]; (2,1,3,2); (2,1,3,3); [2,2,2,2]; (2,2,2,3); (2,2,3,1); [2,2,3,2]; (2,2,3,3); [2,3,2,3]; index (28-36)
    !(2,3,3,1); (2,3,3,2); [2,3,3,3]; [3,1,3,1]; (3,1,3,2); (3,1,3,3); [3,2,3,2]; (3,2,3,3); [3,3,3,3]; index (37-45)  
    ! epsilon_{ij} = (i,1,j,1)+(i,2,j,2)+(i,3,j,3)
    !
!----------------------------------------------------------------------------------------------------------
    ! n = 0
    ! do i = 1, 3
    !   do j = 1, 3
    !     ij = (i - 1) * 3 + j
    !     do s = 1, 3
    !       do l = 1, 3
    !         sl = (s - 1) * 3 + l
    !         if (ij<=sl) then
    !           n = n + 1
    !           accc_tavg(:, :, :) = acccn_tavg(:, :, :, n)
    !           if(mode == STATS_TAVG) then
    !             if (.not. present(opt_acccnn1)) call Print_error_msg("Error in run_stats_loops45.")
    !             if (.not. present(opt_acccnn2)) call Print_error_msg("Error in run_stats_loops45.")
    !             opt_accc(:, :, :) = opt_acccnn1(:, :, :, i, j) * opt_acccnn2(:, :, :, s, l)
    !             if(present(opt_accc0)) &
    !             opt_accc(:, :, :) = opt_accc(:, :, :) * opt_accc0(:, :, :)
    !           end if
    !           call run_stats_action(mode, accc_tavg, &
    !                trim(str)//trim(int2str(i))//trim(int2str(j))//trim(int2str(s))//trim(int2str(l)), &
    !                iter, dm, opt_accc, opt_visnm)
    !           if(mode == STATS_TAVG)&
    !           acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
    !         end if
    !       end do
    !     end do
    !   end do
    ! end do

    n = 0
    do i = 1, 3
      do j = i, 3
        n = n + 1
        if (n <= 6) then
          accc_tavg(:, :, :) = acccn_tavg(:, :, :, n)
          if(mode == STATS_TAVG) then
            if (.not. present(opt_acccnn1)) call Print_error_msg("Error in run_stats_loops45.")
            if (.not. present(opt_acccnn2)) call Print_error_msg("Error in run_stats_loops45.")
            opt_accc(:, :, :) = opt_acccnn1(:, :, :, i, 1) * opt_acccnn2(:, :, :, j, 1) + &
                                opt_acccnn1(:, :, :, i, 2) * opt_acccnn2(:, :, :, j, 2) + &
                                opt_acccnn1(:, :, :, i, 3) * opt_acccnn2(:, :, :, j, 3)
            if(present(opt_accc0)) &
            opt_accc(:, :, :) = opt_accc(:, :, :) * opt_accc0(:, :, :)
          end if
          call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i))//trim(int2str(j)), iter, dm, opt_accc, opt_visnm)
          if(mode == STATS_TAVG)&
          acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
        end if
      end do
    end do
    return

    
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine init_stats_flow(fl, dm)
    use udf_type_mod
    use parameters_constant_mod
    use io_tools_mod
    use typeconvert_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    integer :: iter, i, j, k, n, s, l, ij, sl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc
    !
    if(nrank == 0) call Print_debug_start_msg("Initialise flow statistics ...")
    !
    iter = fl%iterfrom
    !
    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0
      ncl_stat(1, dm%idom) = dm%dccc%xsz(1) ! default skip is 1.
      ncl_stat(2, dm%idom) = dm%dccc%xsz(2) ! default skip is 1.
      ncl_stat(3, dm%idom) = dm%dccc%xsz(3) ! default skip is 1.
    end if
    ! shared post-processing parameters
    allocate( fl%tavg_pr  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
    allocate( fl%tavg_u   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
    allocate( fl%tavg_pru (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
    allocate( fl%tavg_uu  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
    allocate( fl%tavg_uuu (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 10) )
    allocate( fl%tavg_dudu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
    fl%tavg_u    = ZERO
    fl%tavg_pr   = ZERO
    fl%tavg_uu   = ZERO
    fl%tavg_uuu  = ZERO
    fl%tavg_pru  = ZERO
    fl%tavg_dudu = ZERO
    ! Favre averaging only parameters
    if(dm%is_thermo) then
      allocate( fl%tavg_f   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
      allocate( fl%tavg_fu  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
      allocate( fl%tavg_fuu (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
      allocate( fl%tavg_fuuu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 10) )
      fl%tavg_f    = ZERO
      fl%tavg_fu   = ZERO
      fl%tavg_fuu  = ZERO
      fl%tavg_fuuu = ZERO
      !
      allocate( fl%tavg_fh  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
      allocate( fl%tavg_fuh (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
      allocate( fl%tavg_fuuh(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
      fl%tavg_fh   = ZERO
      fl%tavg_fuh  = ZERO
      fl%tavg_fuuh = ZERO
    end if
    !
    if(dm%is_mhd) then
      ! MHD statistics to be implemented
      allocate(fl%tavg_eu (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
      fl%tavg_eu = ZERO
    end if
    !
    if(fl%inittype == INIT_RESTART .and. fl%iterfrom > dm%stat_istart) then
      if(nrank == 0) call Print_debug_inline_msg("Reading flow statistics ...")
      ! shared parameters
      call run_stats_loops1 (STATS_READ, fl%tavg_pr,   't_avg_pr',   iter, dm)
      call run_stats_loops3 (STATS_READ, fl%tavg_u,    't_avg_u',    iter, dm)
      call run_stats_loops3 (STATS_READ, fl%tavg_pru,  't_avg_pru',  iter, dm)
      call run_stats_loops6 (STATS_READ, fl%tavg_uu,   't_avg_uu',   iter, dm)
      call run_stats_loops10(STATS_READ, fl%tavg_uuu,  't_avg_uuu',  iter, dm)
      call run_stats_loops45(STATS_READ, fl%tavg_dudu, 't_avg_dudu', iter, dm)
      ! farve averaging
      if(dm%is_thermo) then
        call run_stats_loops1 (STATS_READ, fl%tavg_f,    't_avg_f',    iter, dm)
        call run_stats_loops3 (STATS_READ, fl%tavg_fu,   't_avg_fu',   iter, dm)
        call run_stats_loops6 (STATS_READ, fl%tavg_fuu,  't_avg_fuu',  iter, dm)
        call run_stats_loops10(STATS_READ, fl%tavg_fuuu, 't_avg_fuuu', iter, dm)
        !
        call run_stats_loops1 (STATS_READ, fl%tavg_fh,   't_avg_fh',   iter, dm)
        call run_stats_loops3 (STATS_READ, fl%tavg_fuh,  't_avg_fuh',  iter, dm)
        call run_stats_loops6 (STATS_READ, fl%tavg_fuuh, 't_avg_fuhh', iter, dm)
      end if
      ! MHD
      if(dm%is_mhd) then
        call run_stats_loops3 (STATS_READ, fl%tavg_eu, 't_avg_eu', iter, dm)
      end if
    end if
    !
    if(nrank == 0) call Print_debug_end_msg()
    !
    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
  subroutine init_stats_thermo(tm, dm)
    use udf_type_mod
    use parameters_constant_mod
    use io_tools_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    integer :: iter
    !
    if(.not. dm%is_thermo) return
    !
    iter = tm%iterfrom
    !
    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0
      ncl_stat(1, dm%idom) = dm%dccc%xsz(1) ! default skip is 1.
      ncl_stat(2, dm%idom) = dm%dccc%xsz(2) ! default skip is 1.
      ncl_stat(3, dm%idom) = dm%dccc%xsz(3) ! default skip is 1.  
    end if
    !
    if(nrank == 0) call Print_debug_start_msg("Initialise thermo statistics ...")
    !
    allocate( tm%tavg_h   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
    allocate( tm%tavg_T   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
    allocate( tm%tavg_TT  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
    !allocate( tm%tavg_dTdT(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6))
    tm%tavg_h  = ZERO
    tm%tavg_T  = ZERO
    tm%tavg_TT = ZERO
    !tm%tavg_dTdT = ZERO
    !
    if(tm%inittype == INIT_RESTART .and. tm%iterfrom > dm%stat_istart) then
      call run_stats_loops1 (STATS_READ, tm%tavg_h,    't_avg_h',    iter, dm)
      call run_stats_loops1 (STATS_READ, tm%tavg_T,    't_avg_T',    iter, dm)
      call run_stats_loops1 (STATS_READ, tm%tavg_TT,   't_avg_TT',   iter, dm)
      !call run_stats_loops6 (STATS_READ, tm%tavg_dTdT, 't_avg_dTdT', iter, dm)
    end if
    !
    if(nrank == 0) call Print_debug_end_msg()
    !
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine init_stats_mhd(mh, dm)
    use udf_type_mod
    use parameters_constant_mod
    use io_tools_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_mhd), intent(inout) :: mh
    integer :: iter
    !
    if(.not. dm%is_mhd) return
    !
    iter = mh%iterfrom
    !
    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0
      ncl_stat(1, dm%idom) = dm%dccc%xsz(1) ! default skip is 1.
      ncl_stat(2, dm%idom) = dm%dccc%xsz(2) ! default skip is 1.
      ncl_stat(3, dm%idom) = dm%dccc%xsz(3) ! default skip is 1.  
    end if
    !
    if(nrank == 0) call Print_debug_start_msg("Initialise mhd statistics ...")
    !
    allocate( mh%tavg_e (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
    allocate( mh%tavg_j (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3))
    allocate( mh%tavg_ej(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3))
    allocate( mh%tavg_jj(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6))
    mh%tavg_e = ZERO
    mh%tavg_j = ZERO
    mh%tavg_ej = ZERO
    mh%tavg_jj = ZERO
    !
    if(mh%iterfrom > dm%stat_istart) then
      call run_stats_loops1(STATS_READ, mh%tavg_e,  't_avg_e',  iter, dm)
      call run_stats_loops3(STATS_READ, mh%tavg_j,  't_avg_j',  iter, dm)
      call run_stats_loops3(STATS_READ, mh%tavg_ej, 't_avg_ej', iter, dm)
      call run_stats_loops6(STATS_READ, mh%tavg_jj, 't_avg_jj', iter, dm)
    end if
    !
    if(nrank == 0) call Print_debug_end_msg()
    !
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================

  subroutine update_stats_flow(fl, dm, tm, mh)
    use udf_type_mod
    use parameters_constant_mod
    use operations
    use transpose_extended_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(in), optional :: tm
    type(t_mhd), intent(in), optional :: mh
    !
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: uccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3 ) :: dudx
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: appc_xpencil 
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil 
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_xpencil
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: apcp_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil, accc1_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3) ) :: apcc_ypencil
    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: appc_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil 
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: acpp_ypencil 
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil, accc1_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: apcp_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: acpp_zpencil 
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil 
    integer :: iter
    !
    iter = fl%iteration
!----------------------------------------------------------------------------------------------------------
!   preparation for u_i
!----------------------------------------------------------------------------------------------------------
    ! u1
    apcc_xpencil = fl%qx
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    uccc(:, :, :, 1) = accc_xpencil(:, :, :)
    ! u2
    acpc_xpencil = fl%qy
    call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy(:), dm%fbcy_qy)
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    uccc(:, :, :, 2) = accc_xpencil(:, :, :)
    ! u3
    accp_xpencil = fl%qz
    call transpose_to_z_pencil(accp_xpencil, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz(:), dm%fbcz_qz)
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    uccc(:, :, :, 3) = accc_xpencil(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   preparation for du_i/dx_j
!----------------------------------------------------------------------------------------------------------
    ! du/dx, du/dy, du/dz
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
!   time averaged 
!----------------------------------------------------------------------------------------------------------
    !flow - shared
    call run_stats_loops1 (STATS_TAVG, fl%tavg_pr,  't_avg_pr',  iter, dm, opt_accc1=fl%pres)
    call run_stats_loops3 (STATS_TAVG, fl%tavg_u,   't_avg_u',   iter, dm, opt_acccn1=uccc)
    call run_stats_loops3 (STATS_TAVG, fl%tavg_pru, 't_avg_pru', iter, dm, opt_acccn1=uccc, opt_accc0=fl%pres)
    call run_stats_loops6 (STATS_TAVG, fl%tavg_uu,  't_avg_uu',  iter, dm, opt_acccn1=uccc, opt_acccn2=uccc)
    call run_stats_loops10(STATS_TAVG, fl%tavg_uuu, 't_avg_uuu', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_acccn3=uccc)
    call run_stats_loops45(STATS_TAVG, fl%tavg_dudu,'t_avg_dudu',iter, dm, opt_acccnn1=dudx,opt_acccnn2=dudx)
    ! flow - Favre 
    if(dm%is_thermo) then
    call run_stats_loops1 (STATS_TAVG, fl%tavg_f,   't_avg_f',   iter, dm, opt_accc1=fl%dDens)
    call run_stats_loops3 (STATS_TAVG, fl%tavg_fu,  't_avg_fu',  iter, dm, opt_acccn1=uccc, opt_accc0=fl%dDens)
    call run_stats_loops6 (STATS_TAVG, fl%tavg_fuu, 't_avg_fuu', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_accc0=fl%dDens)
    call run_stats_loops10(STATS_TAVG, fl%tavg_fuuu,'t_avg_fuuu',iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_acccn3=uccc, opt_accc0=fl%dDens*tm%tTemp)
    !
    call run_stats_loops1 (STATS_TAVG, fl%tavg_fh,  't_avg_fh',   iter, dm, opt_accc1=fl%dDens, opt_accc0=tm%hEnth)
    call run_stats_loops3 (STATS_TAVG, fl%tavg_fuh, 't_avg_fuh',  iter, dm, opt_acccn1=uccc, opt_accc0=tm%hEnth*fl%dDens)
    call run_stats_loops6 (STATS_TAVG, fl%tavg_fuuh,'t_avg_fuuh', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_accc0=tm%hEnth*fl%dDens)
    end if
    ! MHD
    if(dm%is_mhd) then
      call run_stats_loops3 (STATS_TAVG, fl%tavg_eu, 't_avg_eu', iter, dm, opt_acccn1=uccc, opt_accc0=mh%ep)
    end if    !
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine update_stats_thermo(tm, dm)
    use udf_type_mod
    use parameters_constant_mod
    use operations
    use transpose_extended_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    !
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3) :: dTdx
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil, accc1_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil, accc1_zpencil
    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc 
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4
    integer :: iter
    !
    if(.not. dm%is_thermo) return
    !
    iter = tm%iteration
    ! preparation for dT/dx_j
    ! fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%t
    ! fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%t
    ! fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%t
    ! call Get_x_1der_C2C_3D(tm%tTemp, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp, fbcx_4cc)
    ! dTdx(:, :, :, 1) = accc_xpencil(:, :, :)
    ! call transpose_x_to_y(tm%tTemp, accc_ypencil, dm%dccc)
    ! call Get_y_1der_C2C_3D(accc_ypencil, accc1_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_c4c)
    ! call transpose_y_to_x(accc1_ypencil, accc_xpencil, dm%dccc)
    ! dTdx(:, :, :, 2) = accc_xpencil(:, :, :)
    ! call transpose_y_to_z(accc_ypencil, accc_zpencil, dm%dccc)
    ! call Get_z_1der_C2c_3D(accc_zpencil, accc1_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp, fbcz_cc4)
    ! call transpose_from_z_pencil(accc1_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    ! dTdx(:, :, :, 3) = accc_xpencil(:, :, :)
    !
    call run_stats_loops1(STATS_TAVG, tm%tavg_h,    't_avg_h',    iter, dm, opt_accc1=tm%hEnth)
    call run_stats_loops1(STATS_TAVG, tm%tavg_T,    't_avg_T',    iter, dm, opt_accc1=tm%tTemp)
    call run_stats_loops1(STATS_TAVG, tm%tavg_TT,   't_avg_TT',   iter, dm, opt_accc1=tm%tTemp, opt_accc0=tm%tTemp)
    !call run_stats_loops6(STATS_TAVG, tm%tavg_dTdT, 't_avg_dTdT', iter, dm, opt_acccn1=dTdx, opt_acccn2=dTdx)
    !      
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine update_stats_mhd(mh, dm)
    use udf_type_mod
    use parameters_constant_mod
    use operations
    use transpose_extended_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_mhd), intent(inout) :: mh
    !
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: jccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil 
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil 
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    integer :: iter
    if(.not. dm%is_mhd) return
    

    iter = mh%iteration
    !----------------------------------------------------------------------------------------------------------
    !   preparation for j_i
    !----------------------------------------------------------------------------------------------------------
    ! j1
    apcc_xpencil = mh%jx
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, mh%ibcx_jx(:), mh%fbcx_jx)
    jccc(:, :, :, 1) = accc_xpencil(:, :, :)
    ! j2
    acpc_xpencil = mh%jy
    call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, mh%ibcy_jy(:), mh%fbcy_jy)
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    jccc(:, :, :, 2) = accc_xpencil(:, :, :)
    ! j3
    accp_xpencil = mh%jz
    call transpose_to_z_pencil(accp_xpencil, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, mh%ibcz_jz(:), mh%fbcz_jz)
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    jccc(:, :, :, 3) = accc_xpencil(:, :, :)    
    !
    call run_stats_loops1(STATS_TAVG, mh%tavg_e,  't_avg_e',  iter, dm, opt_accc1=mh%ep)
    call run_stats_loops3(STATS_TAVG, mh%tavg_j,  't_avg_j',  iter, dm, opt_acccn1=jccc)
    call run_stats_loops3(STATS_TAVG, mh%tavg_ej, 't_avg_ej', iter, dm, opt_acccn1=jccc, opt_accc0=mh%ep)
    call run_stats_loops6(STATS_TAVG, mh%tavg_jj, 't_avg_jj', iter, dm, opt_acccn1=jccc, opt_acccn2=jccc)
    !      
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_stats_flow(fl, dm)
    use io_tools_mod
    use udf_type_mod
    use typeconvert_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    integer :: iter, i, j, k, s, l, ij, sl, n
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc

    ! here is not only a repeat of those in io_visualisation
    ! because they have different written freqence and to be used for restart as well.
    if(nrank == 0) call Print_debug_inline_msg("Writing flow statistics ...")
    iter = fl%iteration
    ! shared parameters
    call run_stats_loops1 (STATS_WRITE, fl%tavg_pr,  't_avg_pr',   iter, dm)
    call run_stats_loops3 (STATS_WRITE, fl%tavg_u,   't_avg_u',    iter, dm)
    call run_stats_loops3 (STATS_WRITE, fl%tavg_pru, 't_avg_pru',  iter, dm)
    call run_stats_loops6 (STATS_WRITE, fl%tavg_uu,  't_avg_uu',   iter, dm)
    call run_stats_loops10(STATS_WRITE, fl%tavg_uuu, 't_avg_uuu',  iter, dm)
    call run_stats_loops45(STATS_WRITE, fl%tavg_dudu,'t_avg_dudu', iter, dm)
    ! farve averaging
    if(dm%is_thermo) then
    call run_stats_loops1 (STATS_WRITE, fl%tavg_f,   't_avg_f',    iter, dm)
    call run_stats_loops3 (STATS_WRITE, fl%tavg_fu,  't_avg_fu',   iter, dm)
    call run_stats_loops6 (STATS_WRITE, fl%tavg_fuu, 't_avg_fuu',  iter, dm)
    call run_stats_loops10(STATS_WRITE, fl%tavg_fuuu,'t_avg_fuuu', iter, dm)
    !
    call run_stats_loops1 (STATS_WRITE, fl%tavg_fh,  't_avg_fh',   iter, dm)
    call run_stats_loops3 (STATS_WRITE, fl%tavg_fuh, 't_avg_fuh',  iter, dm)
    call run_stats_loops6 (STATS_WRITE, fl%tavg_fuuh,'t_avg_fuuh', iter, dm)
    end if
    ! MHD
    if(dm%is_mhd) then
      call run_stats_loops3 (STATS_WRITE, fl%tavg_eu, 't_avg_eu', iter, dm)
    end if
    !
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

!==========================================================================================================
!==========================================================================================================
  subroutine write_stats_thermo(tm, dm)
    use udf_type_mod
    use io_tools_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    integer :: iter
    !
    if(.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_inline_msg("Writing thermo statistics ...")
    !
    iter = tm%iteration
    call run_stats_loops1 (STATS_WRITE, tm%tavg_h,    't_avg_h',    iter, dm)
    call run_stats_loops1 (STATS_WRITE, tm%tavg_T,    't_avg_T',    iter, dm)
    call run_stats_loops1 (STATS_WRITE, tm%tavg_TT,   't_avg_TT',   iter, dm)
    !call run_stats_loops6 (STATS_WRITE, tm%tavg_dTdT, 't_avg_dTdT', iter, dm)
    !
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_stats_mhd(mh, dm)
    use udf_type_mod
    use io_tools_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_mhd), intent(inout) :: mh
    integer :: iter
    !
    if(.not. dm%is_mhd) return
    if(nrank == 0) call Print_debug_inline_msg("Writing mhd statistics ...")
    iter = mh%iteration
    call run_stats_loops1(STATS_WRITE, mh%tavg_e,  't_avg_e',  iter, dm)
    call run_stats_loops3(STATS_WRITE, mh%tavg_j,  't_avg_j',  iter, dm)
    call run_stats_loops3(STATS_WRITE, mh%tavg_ej, 't_avg_ej', iter, dm)
    call run_stats_loops6(STATS_WRITE, mh%tavg_jj, 't_avg_jj', iter, dm)
    !
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
  !==========================================================================================================
  subroutine write_visu_stats_flow(fl, dm)
    use udf_type_mod
    use precision_mod
    use typeconvert_mod
    use visualisation_field_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc
    integer :: iter, i, j, k, s, l, n, ij, sl
    character(64) :: visuname
    !
!----------------------------------------------------------------------------------------------------------
! write time averaged 3d data
!----------------------------------------------------------------------------------------------------------
    iter = fl%iteration
    visuname = 't_avg_flow'
    ! write xdmf header
    call write_visu_file_begin(dm, visuname, iter)
    ! shared parameters
    call run_stats_loops1 (STATS_VISU3, fl%tavg_pr,   't_avg_pr',   iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops3 (STATS_VISU3, fl%tavg_u,    't_avg_u',    iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops3 (STATS_VISU3, fl%tavg_pru,  't_avg_pru',  iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops6 (STATS_VISU3, fl%tavg_uu,   't_avg_uu',   iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops10(STATS_VISU3, fl%tavg_uuu,  't_avg_uuu',  iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops45(STATS_VISU3, fl%tavg_dudu, 't_avg_dudu', iter, dm, opt_visnm=trim(visuname))
    ! farve averaging
    if(dm%is_thermo) then
      call run_stats_loops1 (STATS_VISU3, fl%tavg_f,    't_avg_f',    iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3 (STATS_VISU3, fl%tavg_fu,   't_avg_fu',   iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops6 (STATS_VISU3, fl%tavg_fuu,  't_avg_fuu',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops10(STATS_VISU3, fl%tavg_fuuu, 't_avg_fuuu', iter, dm, opt_visnm=trim(visuname))
      !
      call run_stats_loops1 (STATS_VISU3, fl%tavg_fh,   't_avg_fh',   iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3 (STATS_VISU3, fl%tavg_fuh,  't_avg_fuh',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops6 (STATS_VISU3, fl%tavg_fuuh, 't_avg_fuuh', iter, dm, opt_visnm=trim(visuname))
    end if
    ! MHD
    if(dm%is_mhd) then
      call run_stats_loops3 (STATS_VISU3, fl%tavg_eu, 't_avg_eu', iter, dm, opt_visnm=trim(visuname))
    end if
    ! write xdmf footer
    call write_visu_file_end(dm, visuname, iter)
!----------------------------------------------------------------------------------------------------------
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!----------------------------------------------------------------------------------------------------------
    if( ANY(dm%is_periodic(:))) then
      visuname = 'tsp_avg_flow'
      ! write xdmf header for 1-periodic
      if(count(dm%is_periodic(1:3)) == 1 .and. nrank == 0) &
      call write_visu_file_begin(dm, visuname, iter, opt_is_savg=.true.)
      ! shared parameters
      call run_stats_loops1 (STATS_VISU1, fl%tavg_pr,   'tsp_avg_pr',   iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3 (STATS_VISU1, fl%tavg_u,    'tsp_avg_u',    iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3 (STATS_VISU1, fl%tavg_pru,  'tsp_avg_pru',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops6 (STATS_VISU1, fl%tavg_uu,   'tsp_avg_uu',   iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops10(STATS_VISU1, fl%tavg_uuu,  'tsp_avg_uuu',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops45(STATS_VISU1, fl%tavg_dudu, 'tsp_avg_dudu', iter, dm, opt_visnm=trim(visuname))
      ! farve averaging
      if(dm%is_thermo) then
        call run_stats_loops1 (STATS_VISU1, fl%tavg_f,    'tsp_avg_f',    iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops3 (STATS_VISU1, fl%tavg_fu,   'tsp_avg_fu',   iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops6 (STATS_VISU1, fl%tavg_fuu,  'tsp_avg_fuu',  iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops10(STATS_VISU1, fl%tavg_fuuu, 'tsp_avg_fuuu', iter, dm, opt_visnm=trim(visuname))
        !
        call run_stats_loops1 (STATS_VISU1, fl%tavg_fh,   'tsp_avg_fh',   iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops3 (STATS_VISU1, fl%tavg_fuh,  'tsp_avg_fuh',  iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops6 (STATS_VISU1, fl%tavg_fuuh, 'tsp_avg_fuuh', iter, dm, opt_visnm=trim(visuname))
      end if
      ! MHD
      if(dm%is_mhd) then
        call run_stats_loops3 (STATS_VISU1, fl%tavg_eu, 'tsp_avg_eu', iter, dm, opt_visnm=trim(visuname))
      end if
      ! write xdmf footer
      if(count(dm%is_periodic(1:3)) == 1 .and. nrank == 0) &
      call write_visu_file_end(dm, visuname, iter, opt_is_savg=.true.)
    end if
    !
    return
  end subroutine

  !==========================================================================================================
  subroutine write_visu_stats_thermo(tm, dm)
    use udf_type_mod
    use precision_mod
    use visualisation_field_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    integer :: iter 
    character(64) :: visuname
!----------------------------------------------------------------------------------------------------------
! write time averaged 3d data
!----------------------------------------------------------------------------------------------------------
    iter = tm%iteration
    visuname = 't_avg_thermo'
    ! write xdmf header
    if(nrank == 0) &
    call write_visu_file_begin(dm, visuname, iter)
    ! write data
    call run_stats_loops1(STATS_VISU3, tm%tavg_h,    't_avg_h',    iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops1(STATS_VISU3, tm%tavg_T,    't_avg_T',    iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops1(STATS_VISU3, tm%tavg_TT,   't_avg_TT',   iter, dm, opt_visnm=trim(visuname))
    !call run_stats_loops6(STATS_VISU3, tm%tavg_dTdT, 't_avg_dTdT', iter, dm, opt_visnm=trim(visuname))
    ! write xdmf footer
    if(nrank == 0) &
    call write_visu_file_end(dm, visuname, iter)
!----------------------------------------------------------------------------------------------------------
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!----------------------------------------------------------------------------------------------------------
    if( ANY(dm%is_periodic(:))) then
      visuname = 'tsp_avg_thermo'
      ! write xdmf header
      if(count(dm%is_periodic(1:3)) == 1 .and. nrank == 0) &
      call write_visu_file_begin(dm, visuname, iter, opt_is_savg=.true.)
      ! write data
      call run_stats_loops1 (STATS_VISU1, tm%tavg_h,    'tsp_avg_h',    iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops1 (STATS_VISU1, tm%tavg_T,    'tsp_avg_T',    iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops1 (STATS_VISU1, tm%tavg_TT,   'tsp_avg_TT',   iter, dm, opt_visnm=trim(visuname))
      !call run_stats_loops6 (STATS_VISU1, tm%tavg_dTdT, 'tsp_avg_dTdT', iter, dm, opt_visnm=trim(visuname))
      ! write xdmf footer
      if(count(dm%is_periodic(1:3)) == 1 .and. nrank == 0) &
      call write_visu_file_end(dm, visuname, iter, opt_is_savg=.true.)
    end if
    
    return
  end subroutine
!==========================================================================================================
  subroutine write_visu_stats_mhd(mh, dm)
    use udf_type_mod
    use precision_mod
    use visualisation_field_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_mhd), intent(inout) :: mh
    integer :: iter 
    character(64) :: visuname
!----------------------------------------------------------------------------------------------------------
! write time averaged 3d data
!----------------------------------------------------------------------------------------------------------
    iter = mh%iteration
    visuname = 't_avg_mhd'
    ! write xdmf header
    if(nrank == 0) &
    call write_visu_file_begin(dm, visuname, iter)
    ! write data
    call run_stats_loops1(STATS_VISU3, mh%tavg_e,  't_avg_e',  iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops3(STATS_VISU3, mh%tavg_j,  't_avg_j',  iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops3(STATS_VISU3, mh%tavg_ej, 't_avg_ej', iter, dm, opt_visnm=trim(visuname))
    call run_stats_loops6(STATS_VISU3, mh%tavg_jj, 't_avg_jj', iter, dm, opt_visnm=trim(visuname))
    ! write xdmf footer
    if(nrank == 0) &
    call write_visu_file_end(dm, visuname, iter)
!----------------------------------------------------------------------------------------------------------
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!----------------------------------------------------------------------------------------------------------
    if( ANY(dm%is_periodic(:))) then
      visuname = 'tsp_avg_mhd'
      ! write xdmf header
      if(count(dm%is_periodic(1:3)) == 1 .and. nrank == 0) &
      call write_visu_file_begin(dm, visuname, iter, opt_is_savg=.true.)
      ! write data
      call run_stats_loops1(STATS_VISU1, mh%tavg_e,  'tsp_avg_e',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3(STATS_VISU1, mh%tavg_j,  'tsp_avg_j',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3(STATS_VISU1, mh%tavg_ej, 'tsp_avg_ej', iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops6(STATS_VISU1, mh%tavg_jj, 'tsp_avg_jj', iter, dm, opt_visnm=trim(visuname))

      ! write xdmf footer
      if(count(dm%is_periodic(1:3)) == 1 .and. nrank == 0) &
      call write_visu_file_end(dm, visuname, iter, opt_is_savg=.true.)
    end if
    
    return
  end subroutine
end module
