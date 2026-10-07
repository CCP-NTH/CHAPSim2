
!==============================================================================
! Periodic averaging refactor
!
! What this fixes vs your current version:
!   - No duplicated hand-written loops for each periodicity pattern
!   - One clear dispatcher that decides WHAT to average (bulk / 1D profile / 2D plane)
!   - One set of reusable reducers:
!        mean_over_dir_1d():    average over 2 directions -> 1D profile
!        mean_over_dir_2d():    average over 1 direction  -> 2D plane (stored as 3D with size=1)
!
!==============================================================================

module visualisation_spatial_average_mod
  use parameters_constant_mod
  use print_msg_mod
  implicit none
  private

  integer, parameter :: XDIR=1, YDIR=2, ZDIR=3

  public  :: write_visu_savg_bin_and_xdmf
  public  :: begin_visu_profile_bundle
  public  :: end_visu_profile_bundle
  private :: write_visu_profile
  private :: write_visu_profile_bundle_section
  private :: write_visu_profile_bundle_ascii
  private :: append_profile_bundle_record
  private :: reset_profile_bundle_records
  private :: profile_remaining_direction
  private :: profile_bundle_stem
  private :: profile_coordinate_value
  private :: delete_existing_file
  private :: profile_coordinate_name
  private :: profile_direction_name
  private :: mean_over_two_dirs_to_profile
  private :: mean_over_one_dir_to_plane
  private :: mean_data_xpencil_over_xdir
  private :: mean_data_ypencil_over_ydir
  private :: mean_data_zpencil_over_zdir
  private :: extract_profile_from_ypencil
  private :: remaining_dir

  type :: t_profile_bundle_record
    character(len=128) :: field_name = ''
    real(WP), allocatable :: values(:)
  end type t_profile_bundle_record

  logical :: profile_bundle_active = .false.
  logical :: profile_bundle_do_write = .false.
  integer :: profile_bundle_dir = 0
  integer :: profile_bundle_npts = 0
  real(WP) :: profile_bundle_h(NDIM) = ZERO
  real(WP), allocatable :: profile_bundle_y_centres(:)
  character(len=256) :: profile_bundle_file = ''
  type(t_profile_bundle_record), allocatable :: profile_bundle_records(:)
  integer :: profile_bundle_record_count = 0

contains
  subroutine begin_visu_profile_bundle(dm, visuname, iter)
    use io_files_mod
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname
    integer, intent(in) :: iter

    character(256) :: output_files(1)
    character(len=256) :: stem
    character(len=256) :: legacy_file
    character(len=256) :: stale_file
    integer :: ulegacy
    logical :: legacy_exists

    profile_bundle_active = .false.
    profile_bundle_do_write = .false.
    profile_bundle_dir = 0
    profile_bundle_npts = 0
    profile_bundle_h = dm%h(1:NDIM)
    if(allocated(profile_bundle_y_centres)) deallocate(profile_bundle_y_centres)
    allocate(profile_bundle_y_centres(size(dm%yc)))
    profile_bundle_y_centres = dm%yc
    profile_bundle_file = ''
    call reset_profile_bundle_records()

    if(dm%restart_data_layout_write /= RESTART_LAYOUT_BUNDLED) return
    if(count(dm%is_periodic(1:3)) /= 2) return

    profile_bundle_dir = profile_remaining_direction(dm)
    if(profile_bundle_dir == 0) return
    select case(profile_bundle_dir)
    case(XDIR)
      profile_bundle_npts = dm%nc(1)
    case(YDIR)
      profile_bundle_npts = dm%nc(2)
    case(ZDIR)
      profile_bundle_npts = dm%nc(3)
    end select

    stem = profile_bundle_stem(visuname, profile_bundle_dir)
    call generate_pathfile_name(output_files(1), dm%idom, trim(stem), dir_visu_data, 'dat', iter)

    call prepare_output_file_set(output_files, dm%existing_output_policy, trim(visuname)//' profile table', &
                                 profile_bundle_do_write)

    profile_bundle_active = .true.
    profile_bundle_file = output_files(1)

    if(nrank == 0 .and. profile_bundle_do_write) then
      call generate_pathfile_name(legacy_file, dm%idom, trim(visuname), dir_visu_data, 'dat', iter)
      inquire(file=trim(legacy_file), exist=legacy_exists)
      if(legacy_exists) then
        open(newunit=ulegacy, file=trim(legacy_file), status='old')
        close(ulegacy, status='delete')
      end if
      call generate_pathfile_name(stale_file, dm%idom, trim(stem), dir_visu_data, 'bin', iter)
      call delete_existing_file(stale_file)
      call generate_pathfile_name(stale_file, dm%idom, trim(stem)//'_meta', dir_visu_data, 'dat', iter)
      call delete_existing_file(stale_file)
      call generate_pathfile_name(stale_file, dm%idom, trim(stem), dir_visu_xdmf, 'xdmf', iter)
      call delete_existing_file(stale_file)
    end if

    return
  end subroutine begin_visu_profile_bundle
!==============================================================================
  subroutine end_visu_profile_bundle()
    implicit none

    if(nrank == 0 .and. profile_bundle_active .and. profile_bundle_do_write) then
      call write_visu_profile_bundle_ascii()
    end if

    profile_bundle_active = .false.
    profile_bundle_do_write = .false.
    profile_bundle_dir = 0
    profile_bundle_npts = 0
    profile_bundle_h = ZERO
    if(allocated(profile_bundle_y_centres)) deallocate(profile_bundle_y_centres)
    profile_bundle_file = ''
    call reset_profile_bundle_records()

    return
  end subroutine end_visu_profile_bundle
!==============================================================================
  subroutine write_visu_savg_bin_and_xdmf(dm, data_in, field_name, visuname, iter)
    use decomp_2d
    use udf_type_mod
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
      call write_visu_profile(dm, prof, trim(field_name), trim(visuname), YDIR, iter)
      deallocate(prof)
      return
    end if

    ! Case: X and Y periodic, Z bounded -> produce Z-profile (mean over X and Y)
    if (px .and. py .and. (.not. pz)) then
      allocate(prof(dtmp%zsz(3)))
      call mean_over_two_dirs_to_profile(data_in, dm, ZDIR, prof)
      call write_visu_profile(dm, prof, trim(field_name), trim(visuname), ZDIR, iter)
      deallocate(prof)
      return
    end if

    ! Case: Y and Z periodic, X bounded -> produce X-profile (mean over Y and Z)
    if (py .and. pz .and. (.not. px)) then
      allocate(prof(dtmp%xsz(1)))
      call mean_over_two_dirs_to_profile(data_in, dm, XDIR, prof)
      call write_visu_profile(dm, prof, trim(field_name), trim(visuname), XDIR, iter)
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
      call write_visu_plane_binary_and_xdmf(dm, savg_data, field_name, visuname, 1, 0, iter, dm%existing_output_policy)
      deallocate(savg_data)
      return
    end if

    ! Case: Y periodic only -> not supported
    if ((.not. px) .and. py .and. (.not. pz)) then
      allocate(savg_data(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)))
      call mean_over_one_dir_to_plane(data_in, dm, YDIR, savg_data)
      call write_visu_plane_binary_and_xdmf(dm, savg_data, field_name, visuname, 2, 0, iter, dm%existing_output_policy)
      deallocate(savg_data)
      return
    end if

    ! Case: Z periodic only -> average over Z, keep XY plane
    if ((.not. px) .and. (.not. py) .and. pz) then
      allocate(savg_data(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)))
      call mean_over_one_dir_to_plane(data_in, dm, ZDIR, savg_data)
      call write_visu_plane_binary_and_xdmf(dm, savg_data, field_name, visuname, 3, 0, iter, dm%existing_output_policy)
      deallocate(savg_data)
      return
    end if

    ! Other mixed periodicities not handled in your original logic:
    ! - X&Y periodic, Z bounded
    ! - Y periodic only, etc.
    ! Add cases here if you need them later.
    return

  end subroutine write_visu_savg_bin_and_xdmf

  subroutine write_visu_profile(dm, prof, varname, visuname, dir, iter)
    use decomp_2d
    use decomp_2d_io
    use decomp_operation_mod
    use io_tools_mod
    use precision_mod
    use typeconvert_mod
    use udf_type_mod, only: t_domain
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: prof(:)
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: visuname
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
!------------------------------------------------------------------------------
! write data
!------------------------------------------------------------------------------
    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      if(profile_bundle_active) then
        call write_visu_profile_bundle_section(dm, prof, varname, dir)
        return
      end if
      call begin_visu_profile_bundle(dm, visuname, iter)
      call write_visu_profile_bundle_section(dm, prof, varname, dir)
      call end_visu_profile_bundle()
      return
    end if

    if(nrank == 0) then
      keyword = trim(varname)
      call generate_pathfile_name(data_flname_path, dm%idom, keyword, dir_visu_data, 'dat', iter)
      open(newunit = iofl, file = data_flname_path, action = "write", status="replace")
      select case (dir)
      case (XDIR)
        do j = 1, dtmp%xsz(1)
          write(iofl, *) j, (j - 1 + HALF) * dm%h(1), prof(j)
        end do
      case (YDIR)
        do j = 1, dtmp%ysz(2)
          write(iofl, *) j, dm%yc(j), prof(j)
        end do
      case (ZDIR)
        do j = 1, dtmp%zsz(3)
          write(iofl, *) j, (j - 1 + HALF) * dm%h(3), prof(j)
        end do
      end select

      close(iofl)
    end if

    return
  end subroutine
!==============================================================================
  subroutine write_visu_profile_bundle_section(dm, prof, varname, dir)
    use udf_type_mod, only: t_domain
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: prof(:)
    character(len=*), intent(in) :: varname
    integer, intent(in) :: dir

    if(nrank /= 0) return
    if(.not. profile_bundle_active) return
    if(.not. profile_bundle_do_write) return
    if(dir /= profile_bundle_dir) return

    call append_profile_bundle_record(varname, prof)

    return
  end subroutine write_visu_profile_bundle_section
!==============================================================================
  subroutine append_profile_bundle_record(field_name, values)
    implicit none
    character(*), intent(in) :: field_name
    real(WP), intent(in) :: values(:)

    type(t_profile_bundle_record), allocatable :: records_tmp(:)
    integer :: nnew

    nnew = profile_bundle_record_count + 1
    allocate(records_tmp(nnew))
    if(profile_bundle_record_count > 0) &
      records_tmp(1:profile_bundle_record_count) = profile_bundle_records(1:profile_bundle_record_count)

    records_tmp(nnew)%field_name = trim(field_name)
    allocate(records_tmp(nnew)%values(size(values)))
    records_tmp(nnew)%values = values
    call move_alloc(records_tmp, profile_bundle_records)
    profile_bundle_record_count = nnew

    return
  end subroutine append_profile_bundle_record
!==============================================================================
  subroutine reset_profile_bundle_records()
    implicit none

    if(allocated(profile_bundle_records)) deallocate(profile_bundle_records)
    profile_bundle_record_count = 0

    return
  end subroutine reset_profile_bundle_records
!==============================================================================
  subroutine write_visu_profile_bundle_ascii()
    implicit none

    integer :: u, irec, j

    if(profile_bundle_record_count <= 0) return

    call Print_debug_mid_msg('Writing '//trim(profile_bundle_file))
    open(newunit=u, file=trim(profile_bundle_file), status='replace', action='write')
    write(u,'(A)') '# CHAPSim2 time-and-space averaged profile'
    write(u,'(A)') '# format_version: CHAPSim_profile_ascii_v1'
    write(u,'(A,1X,A)') '# direction:', trim(profile_direction_name(profile_bundle_dir))
    write(u,'(A,1X,A)') '# coordinate:', trim(profile_coordinate_name(profile_bundle_dir))
    write(u,'(A,1X,I0)') '# npoints:', profile_bundle_npts
    write(u,'(A)', advance='no') '# columns: index '//trim(profile_coordinate_name(profile_bundle_dir))
    do irec = 1, profile_bundle_record_count
      write(u,'(1X,A)', advance='no') trim(profile_bundle_records(irec)%field_name)
    end do
    write(u,*)
    do j = 1, profile_bundle_npts
      write(u,'(I8,1X,ES24.16E3)', advance='no') j, profile_coordinate_value(j)
      do irec = 1, profile_bundle_record_count
        write(u,'(1X,ES24.16E3)', advance='no') profile_bundle_records(irec)%values(j)
      end do
      write(u,*)
    end do
    close(u)

    return
  end subroutine write_visu_profile_bundle_ascii
!==============================================================================
  pure function profile_coordinate_value(j) result(coord)
    implicit none
    integer, intent(in) :: j
    real(WP) :: coord

    select case(profile_bundle_dir)
    case(XDIR)
      coord = (real(j, WP) - HALF) * profile_bundle_h(XDIR)
    case(YDIR)
      coord = profile_bundle_y_centres(j)
    case(ZDIR)
      coord = (real(j, WP) - HALF) * profile_bundle_h(ZDIR)
    case default
      coord = ZERO
    end select

    return
  end function profile_coordinate_value
!==============================================================================
!==============================================================================
  pure function profile_remaining_direction(dm) result(dir)
    use udf_type_mod, only: t_domain
    implicit none
    type(t_domain), intent(in) :: dm
    integer :: dir

    dir = 0
    if((.not. dm%is_periodic(XDIR)) .and. dm%is_periodic(YDIR) .and. dm%is_periodic(ZDIR)) dir = XDIR
    if(dm%is_periodic(XDIR) .and. (.not. dm%is_periodic(YDIR)) .and. dm%is_periodic(ZDIR)) dir = YDIR
    if(dm%is_periodic(XDIR) .and. dm%is_periodic(YDIR) .and. (.not. dm%is_periodic(ZDIR))) dir = ZDIR

    return
  end function profile_remaining_direction
!==============================================================================
  pure function profile_bundle_stem(visuname, dir) result(stem)
    implicit none
    character(*), intent(in) :: visuname
    integer, intent(in) :: dir
    character(len=256) :: stem

    stem = trim(visuname)//'_'//profile_direction_name(dir)//'profile'

    return
  end function profile_bundle_stem
!==============================================================================
  subroutine delete_existing_file(file_w_path)
    implicit none
    character(*), intent(in) :: file_w_path

    integer :: u, ios
    logical :: ex

    inquire(file=trim(file_w_path), exist=ex)
    if(ex) then
      open(newunit=u, file=trim(file_w_path), status='old', action='readwrite', iostat=ios)
      if(ios == 0) close(u, status='delete')
    end if

    return
  end subroutine delete_existing_file
!==============================================================================
  pure function profile_direction_name(dir) result(name)
    implicit none
    integer, intent(in) :: dir
    character(len=1) :: name

    select case(dir)
    case(XDIR); name = 'x'
    case(YDIR); name = 'y'
    case(ZDIR); name = 'z'
    case default; name = '?'
    end select

    return
  end function profile_direction_name
!==============================================================================
  pure function profile_coordinate_name(dir) result(name)
    implicit none
    integer, intent(in) :: dir
    character(len=8) :: name

    select case(dir)
    case(XDIR); name = 'xc'
    case(YDIR); name = 'yc'
    case(ZDIR); name = 'zc'
    case default; name = 'unknown'
    end select

    return
  end function profile_coordinate_name
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
    use decomp_2d
    use udf_type_mod
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
    use decomp_2d
    use udf_type_mod
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
!==============================================================================
module statistics_mod
  use parameters_constant_mod
  use print_msg_mod
  implicit none

  character(13), parameter :: io_name = "statistics-io"
  integer, allocatable :: ncl_stat(:, :)
  !
  integer, parameter :: STATS_READ  = 1
  integer, parameter :: STATS_WRITE = 2
  integer, parameter :: STATS_TAVG  = 3
  integer, parameter :: STATS_VISU3 = 4
  integer, parameter :: STATS_VISU1 = 5
  integer, parameter :: NDUDU_MAX = 45
  integer, parameter :: NDUDU_ACTIVE = 6
  !
  private :: run_stats_action
  private :: run_stats_loops1
  private :: run_stats_loops3
  private :: run_stats_loops6
  private :: run_stats_loops9
  private :: run_stats_loops10
  private :: run_stats_loops45
  private :: init_spectrum_uu
  private :: accumulate_spectrum_uu
  private :: accumulate_spectrum_dir
  private :: write_spectrum_uu
  private :: write_spectrum_file
  private :: write_spectrum_bundle_uu
  private :: one_sided_rfft_power
  private :: spectrum_yplus
  private :: spectrum_wall_distance
  private :: require_stats_restart_file
  private :: write_stats_bundle_metadata
  private :: validate_stats_bundle_metadata
  private :: read_stats_sample_count
  private :: restore_stats_sample_count
  private :: stats_bundle_write1
  private :: stats_bundle_writeN
  private :: stats_bundle_read1
  private :: stats_bundle_readN
  private :: flow_stats_bundle_signature
  private :: thermo_stats_bundle_signature
  private :: mhd_stats_bundle_signature
  private :: flow_stats_bundle_fields
  private :: thermo_stats_bundle_fields
  private :: mhd_stats_bundle_fields
  private :: append_stats_field
  private :: append_stats_components
  private :: append_stats_components6
  private :: append_stats_components9
  private :: append_stats_components10
  private :: append_stats_dudu_active
  private :: write_flow_stats_bundle
  private :: read_flow_stats_bundle
  private :: write_thermo_stats_bundle
  private :: read_thermo_stats_bundle
  private :: write_mhd_stats_bundle
  private :: read_mhd_stats_bundle
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
!==============================================================================
  function flow_stats_bundle_signature(dm) result(sig)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(256) :: sig

    write(sig, '(A,I0,A,L1)') 'flow_stats_v1;stat_level=', dm%stat_level, ';is_thermo=', dm%is_thermo
    return
  end function flow_stats_bundle_signature
!==============================================================================
  function thermo_stats_bundle_signature(dm) result(sig)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(256) :: sig

    write(sig, '(A,I0)') 'thermo_stats_v1;stat_level=', dm%stat_level
    return
  end function thermo_stats_bundle_signature
!==============================================================================
  function mhd_stats_bundle_signature(dm) result(sig)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(256) :: sig

    write(sig, '(A,I0)') 'mhd_stats_v1;stat_level=', dm%stat_level
    return
  end function mhd_stats_bundle_signature
!==============================================================================
  subroutine append_stats_field(fields, field_name)
    implicit none
    character(*), intent(inout) :: fields
    character(*), intent(in) :: field_name

    if(len_trim(fields) > 0) fields = trim(fields)//','
    fields = trim(fields)//trim(field_name)//':dccc'
    return
  end subroutine append_stats_field
!==============================================================================
  subroutine append_stats_components(fields, field_name, ncomp)
    use typeconvert_mod, only: int2str
    implicit none
    character(*), intent(inout) :: fields
    character(*), intent(in) :: field_name
    integer, intent(in) :: ncomp

    integer :: n

    do n = 1, ncomp
      call append_stats_field(fields, trim(field_name)//trim(int2str(n)))
    end do
    return
  end subroutine append_stats_components
!==============================================================================
  subroutine append_stats_components6(fields, field_name)
    use typeconvert_mod, only: int2str
    implicit none
    character(*), intent(inout) :: fields
    character(*), intent(in) :: field_name

    integer :: i, j

    do i = 1, 3
      do j = i, 3
        call append_stats_field(fields, trim(field_name)//trim(int2str(i))//trim(int2str(j)))
      end do
    end do
    return
  end subroutine append_stats_components6
!==============================================================================
  subroutine append_stats_components9(fields, field_name)
    use typeconvert_mod, only: int2str
    implicit none
    character(*), intent(inout) :: fields
    character(*), intent(in) :: field_name

    integer :: i, j

    do i = 1, 3
      do j = 1, 3
        call append_stats_field(fields, trim(field_name)//trim(int2str(i))//trim(int2str(j)))
      end do
    end do
    return
  end subroutine append_stats_components9
!==============================================================================
  subroutine append_stats_components10(fields, field_name)
    use typeconvert_mod, only: int2str
    implicit none
    character(*), intent(inout) :: fields
    character(*), intent(in) :: field_name

    integer :: i, j, k

    do i = 1, 3
      do j = i, 3
        do k = j, 3
          call append_stats_field(fields, trim(field_name)//trim(int2str(i))// &
                                  trim(int2str(j))//trim(int2str(k)))
        end do
      end do
    end do
    return
  end subroutine append_stats_components10
!==============================================================================
  subroutine append_stats_dudu_active(fields, field_name)
    use typeconvert_mod, only: int2str
    implicit none
    character(*), intent(inout) :: fields
    character(*), intent(in) :: field_name

    integer :: i, j

    do i = 1, 3
      do j = i, 3
        call append_stats_field(fields, trim(field_name)//trim(int2str(i))//trim(int2str(j)))
      end do
    end do
    return
  end subroutine append_stats_dudu_active
!==============================================================================
  function flow_stats_bundle_fields(dm) result(fields)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(4096) :: fields

    fields = ''
    if(dm%stat_level > ISTATL0) then
      call append_stats_field(fields, 't_avg_pr')
      call append_stats_components(fields, 't_avg_u', 3)
      call append_stats_components9(fields, 't_avg_dudx')
      call append_stats_components(fields, 't_avg_vort', 3)
    end if
    if(dm%stat_level > ISTATL1) then
      call append_stats_components6(fields, 't_avg_uu')
      call append_stats_components6(fields, 't_avg_vortvort')
    end if
    if(dm%stat_level > ISTATL2) then
      call append_stats_components(fields, 't_avg_pru', 3)
      call append_stats_components9(fields, 't_avg_prdu')
      call append_stats_components10(fields, 't_avg_uuu')
      call append_stats_dudu_active(fields, 't_avg_dudu')
    end if

    if(dm%is_thermo) then
      if(dm%stat_level > ISTATL0) then
        call append_stats_field(fields, 't_avg_f')
        call append_stats_components(fields, 't_avg_fu', 3)
        call append_stats_field(fields, 't_avg_fh')
      end if
      if(dm%stat_level > ISTATL1) then
        call append_stats_components6(fields, 't_avg_fuu')
        call append_stats_components(fields, 't_avg_fuh', 3)
        call append_stats_components(fields, 't_avg_Tu', 3)
      end if
      if(dm%stat_level > ISTATL2) then
        call append_stats_components10(fields, 't_avg_fuuu')
        call append_stats_components6(fields, 't_avg_fuuh')
      end if
    end if
    return
  end function flow_stats_bundle_fields
!==============================================================================
  function thermo_stats_bundle_fields(dm) result(fields)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(4096) :: fields

    fields = ''
    if(dm%stat_level > ISTATL0) then
      call append_stats_field(fields, 't_avg_h')
      call append_stats_field(fields, 't_avg_T')
    end if
    if(dm%stat_level > ISTATL1) then
      call append_stats_field(fields, 't_avg_TT')
    end if
    return
  end function thermo_stats_bundle_fields
!==============================================================================
  function mhd_stats_bundle_fields(dm) result(fields)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(4096) :: fields

    fields = ''
    if(dm%stat_level > ISTATL0) then
      call append_stats_field(fields, 't_avg_e')
      call append_stats_components(fields, 't_avg_j', 3)
    end if
    if(dm%stat_level > ISTATL1) then
      call append_stats_components(fields, 't_avg_eu', 3)
      call append_stats_components(fields, 't_avg_ej', 3)
      call append_stats_components9(fields, 't_avg_ju')
      call append_stats_components6(fields, 't_avg_jj')
    end if
    return
  end function mhd_stats_bundle_fields
!==============================================================================
  subroutine write_stats_bundle_metadata(dm, group_name, iter, signature, fields, nsamples, &
                                         opt_existing_output_policy)
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    character(*), intent(in) :: signature
    character(*), intent(in) :: fields
    integer, intent(in) :: iter
    integer, intent(in) :: nsamples
    ! Supplied only on the per-field write path, where this file is not part of
    ! an output set that prepare_output_file_set has already screened. Without
    ! it, OUTPUT_POLICY_SKIP would leave the previous run's averages on disk
    ! beside this run's sample count.
    integer, intent(in), optional :: opt_existing_output_policy

    character(256) :: meta_file
    character(256) :: output_files(1)
    integer :: u
    logical :: do_write

    call generate_pathfile_name(meta_file, dm%idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
    if(present(opt_existing_output_policy)) then
      output_files(1) = meta_file
      call prepare_output_file_set(output_files, opt_existing_output_policy, &
                                   trim(group_name)//' metadata', do_write)
      if(.not. do_write) return
    end if

    if(nrank /= 0) return

    open(newunit=u, file=trim(meta_file), status='replace', action='write')
    write(u, '(A)') 'CHAPSim_stats_bundle_v1'
    write(u, '(A,1X,A)') 'group', trim(group_name)
    write(u, '(A,1X,I0)') 'iter', iter
    write(u, '(A,1X,A)') 'signature', trim(signature)
    write(u, '(A,1X,A)') 'fields', trim(fields)
    ! Appended after the five keys validate_stats_bundle_metadata reads by
    ! position, so a file written here still loads in a build that predates
    ! them, and a file written by one is still validated by this build -
    ! read_stats_sample_count simply reports it as absent.
    write(u, '(A,1X,I0)') 'stat_istart', dm%stat_istart
    write(u, '(A,1X,I0)') 'nsamples', nsamples
    close(u)

    return
  end subroutine write_stats_bundle_metadata
!==============================================================================
! The sample population of a stored set of time averages, and the stat_istart
! that set was accumulated under. Read by key, not by position, and tolerant of
! a missing file or missing keys: a checkpoint written before the count existed
! restarts with found = .false., and the caller falls back to the old derived
! weight with a warning. A stat_istart that disagrees is fatal - it silently
! re-weights every stored average, which is exactly the failure this records.
!==============================================================================
  subroutine read_stats_sample_count(dm, group_name, iter, nsamples, found)
    use io_tools_mod
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    integer, intent(in) :: iter
    integer, intent(out) :: nsamples
    logical, intent(out) :: found

    character(256) :: meta_file
    character(4096) :: line, key
    integer :: u, ioerr, ival, istart_read
    logical :: has_istart

    nsamples = 0
    found = .false.
    has_istart = .false.
    istart_read = 0

    call generate_pathfile_name(meta_file, dm%idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
    if(.not. file_exists(trim(meta_file))) return

    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    if(ioerr /= 0) return

    do
      read(u, '(A)', iostat=ioerr) line
      if(ioerr /= 0) exit
      read(line, *, iostat=ioerr) key, ival
      if(ioerr /= 0) cycle
      if(trim(key) == 'nsamples') then
        nsamples = ival
        found = .true.
      else if(trim(key) == 'stat_istart') then
        istart_read = ival
        has_istart = .true.
      end if
    end do
    close(u)

    if(has_istart .and. istart_read /= dm%stat_istart) &
    call Print_error_msg("The stored "//trim(group_name)//" averages were accumulated with "// &
      "stat_istart = "//trim(int2str(istart_read))//", but this run sets stat_istart = "// &
      trim(int2str(dm%stat_istart))//". Continuing would re-weight them against a window they "// &
      "do not cover. Restart with the original stat_istart, or set it at or above the restart "// &
      "iteration to start a fresh average.")

    return
  end subroutine read_stats_sample_count
!==============================================================================
! Restore the sample count that goes with a set of time averages just read from
! a checkpoint. Pre-count checkpoints carry no count, so fall back to the weight
! the solver used to derive - correct for the unbroken single-run case those
! files came from - and say so.
!==============================================================================
  subroutine restore_stats_sample_count(dm, group_name, iter, nsamples)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    integer, intent(in) :: iter
    integer, intent(out) :: nsamples
    logical :: found

    call read_stats_sample_count(dm, trim(group_name), iter, nsamples, found)
    if(found) return

    nsamples = max(iter - dm%stat_istart, 0)
    if(nrank == 0) &
    call Print_warning_msg("The "//trim(group_name)//" checkpoint at iteration "// &
      trim(int2str(iter))//" carries no sample count, so it is assumed to hold "// &
      trim(int2str(nsamples))//" samples, one per iteration since stat_istart. That is right "// &
      "for a checkpoint from an uninterrupted run and wrong if the field was ever frozen or "// &
      "injected fresh part-way through.")

    return
  end subroutine restore_stats_sample_count
!==============================================================================
  subroutine validate_stats_bundle_metadata(dm, group_name, iter, signature, fields)
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    character(*), intent(in) :: signature
    character(*), intent(in) :: fields
    integer, intent(in) :: iter

    character(256) :: meta_file
    character(4096) :: line, key, value
    integer :: u, ioerr, iter_read

    call generate_pathfile_name(meta_file, dm%idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
    if(.not. file_exists(trim(meta_file))) &
    call Print_error_msg("The statistics bundle metadata file "//trim(meta_file)//" does not exist.")

    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    if(ioerr /= 0) call Print_error_msg("Failed to open statistics bundle metadata file "//trim(meta_file))

    read(u, '(A)', iostat=ioerr) line
    if(ioerr /= 0 .or. trim(line) /= 'CHAPSim_stats_bundle_v1') &
    call Print_error_msg("Unsupported statistics bundle metadata format in "//trim(meta_file))

    read(u, '(A)', iostat=ioerr) line
    read(line, *, iostat=ioerr) key, value
    if(ioerr /= 0 .or. trim(key) /= 'group' .or. trim(value) /= trim(group_name)) &
    call Print_error_msg("Statistics bundle group mismatch in "//trim(meta_file))

    read(u, '(A)', iostat=ioerr) line
    read(line, *, iostat=ioerr) key, iter_read
    if(ioerr /= 0 .or. trim(key) /= 'iter' .or. iter_read /= iter) &
    call Print_error_msg("Statistics bundle iteration mismatch in "//trim(meta_file))

    read(u, '(A)', iostat=ioerr) line
    if(ioerr /= 0 .or. trim(line) /= 'signature '//trim(signature)) &
    call Print_error_msg("Statistics bundle signature mismatch in "//trim(meta_file))

    read(u, '(A)', iostat=ioerr) line
    if(ioerr == 0) then
      if(trim(line) /= 'fields '//trim(fields)) then
        write(*,*) "Expected fields: ", trim(fields)
        write(*,*) "Read fields:     ", trim(line)
        call Print_error_msg("Statistics bundle field list mismatch in "//trim(meta_file))
      end if
    else if(nrank == 0) then
      call Print_warning_msg("Statistics bundle metadata has no field list: "//trim(meta_file))
    end if

    close(u)

    return
  end subroutine validate_stats_bundle_metadata
!==============================================================================
  subroutine stats_bundle_write1(io, dm, var)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use udf_type_mod
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: var(:, :, :)

    call decomp_2d_write_var(io, IPENCIL(1), var, opt_decomp=dm%dccc)
    return
  end subroutine stats_bundle_write1
!==============================================================================
  subroutine stats_bundle_writeN(io, dm, var, ncomp)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use udf_type_mod
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: var(:, :, :, :)
    integer, intent(in) :: ncomp
    integer :: n

    do n = 1, ncomp
      call decomp_2d_write_var(io, IPENCIL(1), var(:, :, :, n), opt_decomp=dm%dccc)
    end do
    return
  end subroutine stats_bundle_writeN
!==============================================================================
  subroutine stats_bundle_read1(io, dm, var)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use udf_type_mod
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: var(:, :, :)

    call decomp_2d_read_var(io, IPENCIL(1), var, opt_decomp=dm%dccc)
    return
  end subroutine stats_bundle_read1
!==============================================================================
  subroutine stats_bundle_readN(io, dm, var, ncomp)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use udf_type_mod
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: var(:, :, :, :)
    integer, intent(in) :: ncomp
    integer :: n

    do n = 1, ncomp
      call decomp_2d_read_var(io, IPENCIL(1), var(:, :, :, n), opt_decomp=dm%dccc)
    end do
    return
  end subroutine stats_bundle_readN
!==============================================================================
  subroutine write_flow_stats_bundle(fl, dm)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use checkpoint_metadata_mod, only: write_checkpoint_manifest
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(in) :: dm

    character(256) :: bundle_file
    character(256) :: output_files(2)
    integer :: iter
    logical :: do_write
    type(d2d_io_mpi) :: io

    iter = fl%iteration
    call generate_pathfile_name(output_files(1), dm%idom, 'flow_stats', dir_data, 'bin', iter)
    call generate_pathfile_name(output_files(2), dm%idom, 'flow_stats_meta', dir_data, 'dat', iter)
    bundle_file = output_files(1)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'flow statistics bundle', do_write)
    if(.not. do_write) return
    call io%open(trim(bundle_file), decomp_2d_write_mode)

    if(dm%stat_level > ISTATL0) then
      call stats_bundle_write1(io, dm, fl%tavg_pr)
      call stats_bundle_writeN(io, dm, fl%tavg_u,    3)
      call stats_bundle_writeN(io, dm, fl%tavg_dudx, 9)
      call stats_bundle_writeN(io, dm, fl%tavg_vort, 3)
    end if
    if(dm%stat_level > ISTATL1) then
      call stats_bundle_writeN(io, dm, fl%tavg_uu,       6)
      call stats_bundle_writeN(io, dm, fl%tavg_vortvort, 6)
    end if
    if(dm%stat_level > ISTATL2) then
      call stats_bundle_writeN(io, dm, fl%tavg_pru,  3)
      call stats_bundle_writeN(io, dm, fl%tavg_prdu, 9)
      call stats_bundle_writeN(io, dm, fl%tavg_uuu,  10)
      call stats_bundle_writeN(io, dm, fl%tavg_dudu, NDUDU_ACTIVE)
    end if

    if(dm%is_thermo) then
      if(dm%stat_level > ISTATL0) then
        call stats_bundle_write1(io, dm, fl%tavg_f)
        call stats_bundle_writeN(io, dm, fl%tavg_fu, 3)
        call stats_bundle_write1(io, dm, fl%tavg_fh)
      end if
      if(dm%stat_level > ISTATL1) then
        call stats_bundle_writeN(io, dm, fl%tavg_fuu, 6)
        call stats_bundle_writeN(io, dm, fl%tavg_fuh, 3)
        call stats_bundle_writeN(io, dm, fl%tavg_Tu,  3)
      end if
      if(dm%stat_level > ISTATL2) then
        call stats_bundle_writeN(io, dm, fl%tavg_fuuu, 10)
        call stats_bundle_writeN(io, dm, fl%tavg_fuuh, 6)
      end if
    end if

    call io%close()
    call write_stats_bundle_metadata(dm, 'flow_stats', iter, flow_stats_bundle_signature(dm), &
                                     flow_stats_bundle_fields(dm), fl%nstat_samples)
    call write_checkpoint_manifest(dm%idom, iter, fl%time, dm%dt)

    return
  end subroutine write_flow_stats_bundle
!==============================================================================
  subroutine read_flow_stats_bundle(fl, dm)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(in) :: dm

    character(256) :: bundle_file
    integer :: iter
    type(d2d_io_mpi) :: io

    iter = fl%iterfrom
    call validate_stats_bundle_metadata(dm, 'flow_stats', iter, flow_stats_bundle_signature(dm), &
                                        flow_stats_bundle_fields(dm))
    call generate_pathfile_name(bundle_file, dm%idom, 'flow_stats', dir_data, 'bin', iter)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The flow statistics bundle file "//trim(bundle_file)//" does not exist.")
    call io%open(trim(bundle_file), decomp_2d_read_mode)

    if(dm%stat_level > ISTATL0) then
      call stats_bundle_read1(io, dm, fl%tavg_pr)
      call stats_bundle_readN(io, dm, fl%tavg_u,    3)
      call stats_bundle_readN(io, dm, fl%tavg_dudx, 9)
      call stats_bundle_readN(io, dm, fl%tavg_vort, 3)
    end if
    if(dm%stat_level > ISTATL1) then
      call stats_bundle_readN(io, dm, fl%tavg_uu,       6)
      call stats_bundle_readN(io, dm, fl%tavg_vortvort, 6)
    end if
    if(dm%stat_level > ISTATL2) then
      call stats_bundle_readN(io, dm, fl%tavg_pru,  3)
      call stats_bundle_readN(io, dm, fl%tavg_prdu, 9)
      call stats_bundle_readN(io, dm, fl%tavg_uuu,  10)
      call stats_bundle_readN(io, dm, fl%tavg_dudu, NDUDU_ACTIVE)
    end if

    if(dm%is_thermo) then
      if(dm%stat_level > ISTATL0) then
        call stats_bundle_read1(io, dm, fl%tavg_f)
        call stats_bundle_readN(io, dm, fl%tavg_fu, 3)
        call stats_bundle_read1(io, dm, fl%tavg_fh)
      end if
      if(dm%stat_level > ISTATL1) then
        call stats_bundle_readN(io, dm, fl%tavg_fuu, 6)
        call stats_bundle_readN(io, dm, fl%tavg_fuh, 3)
        call stats_bundle_readN(io, dm, fl%tavg_Tu,  3)
      end if
      if(dm%stat_level > ISTATL2) then
        call stats_bundle_readN(io, dm, fl%tavg_fuuu, 10)
        call stats_bundle_readN(io, dm, fl%tavg_fuuh, 6)
      end if
    end if

    call io%close()

    return
  end subroutine read_flow_stats_bundle
!==============================================================================
  subroutine write_thermo_stats_bundle(tm, dm)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use checkpoint_metadata_mod, only: write_checkpoint_manifest
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_thermo), intent(in) :: tm
    type(t_domain), intent(in) :: dm

    character(256) :: bundle_file
    character(256) :: output_files(2)
    integer :: iter
    logical :: do_write
    type(d2d_io_mpi) :: io

    iter = tm%iteration
    call generate_pathfile_name(output_files(1), dm%idom, 'thermo_stats', dir_data, 'bin', iter)
    call generate_pathfile_name(output_files(2), dm%idom, 'thermo_stats_meta', dir_data, 'dat', iter)
    bundle_file = output_files(1)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'thermo statistics bundle', do_write)
    if(.not. do_write) return
    call io%open(trim(bundle_file), decomp_2d_write_mode)

    if(dm%stat_level > ISTATL0) then
      call stats_bundle_write1(io, dm, tm%tavg_h)
      call stats_bundle_write1(io, dm, tm%tavg_T)
    end if
    if(dm%stat_level > ISTATL1) then
      call stats_bundle_write1(io, dm, tm%tavg_TT)
    end if

    call io%close()
    call write_stats_bundle_metadata(dm, 'thermo_stats', iter, thermo_stats_bundle_signature(dm), &
                                     thermo_stats_bundle_fields(dm), tm%nstat_samples)
    call write_checkpoint_manifest(dm%idom, iter, tm%time, dm%dt)

    return
  end subroutine write_thermo_stats_bundle
!==============================================================================
  subroutine read_thermo_stats_bundle(tm, dm)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(in) :: dm

    character(256) :: bundle_file
    integer :: iter
    type(d2d_io_mpi) :: io

    iter = tm%iterfrom
    call validate_stats_bundle_metadata(dm, 'thermo_stats', iter, thermo_stats_bundle_signature(dm), &
                                        thermo_stats_bundle_fields(dm))
    call generate_pathfile_name(bundle_file, dm%idom, 'thermo_stats', dir_data, 'bin', iter)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The thermo statistics bundle file "//trim(bundle_file)//" does not exist.")
    call io%open(trim(bundle_file), decomp_2d_read_mode)

    if(dm%stat_level > ISTATL0) then
      call stats_bundle_read1(io, dm, tm%tavg_h)
      call stats_bundle_read1(io, dm, tm%tavg_T)
    end if
    if(dm%stat_level > ISTATL1) then
      call stats_bundle_read1(io, dm, tm%tavg_TT)
    end if

    call io%close()

    return
  end subroutine read_thermo_stats_bundle
!==============================================================================
  subroutine write_mhd_stats_bundle(mh, dm)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_mhd), intent(in) :: mh
    type(t_domain), intent(in) :: dm

    character(256) :: bundle_file
    character(256) :: output_files(2)
    integer :: iter
    logical :: do_write
    type(d2d_io_mpi) :: io

    iter = mh%iteration
    call generate_pathfile_name(output_files(1), dm%idom, 'mhd_stats', dir_data, 'bin', iter)
    call generate_pathfile_name(output_files(2), dm%idom, 'mhd_stats_meta', dir_data, 'dat', iter)
    bundle_file = output_files(1)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'MHD statistics bundle', do_write)
    if(.not. do_write) return
    call io%open(trim(bundle_file), decomp_2d_write_mode)

    if(dm%stat_level > ISTATL0) then
      call stats_bundle_write1(io, dm, mh%tavg_e)
      call stats_bundle_writeN(io, dm, mh%tavg_j, 3)
    end if
    if(dm%stat_level > ISTATL1) then
      call stats_bundle_writeN(io, dm, mh%tavg_eu, 3)
      call stats_bundle_writeN(io, dm, mh%tavg_ej, 3)
      call stats_bundle_writeN(io, dm, mh%tavg_ju, 9)
      call stats_bundle_writeN(io, dm, mh%tavg_jj, 6)
    end if

    call io%close()
    call write_stats_bundle_metadata(dm, 'mhd_stats', iter, mhd_stats_bundle_signature(dm), &
                                     mhd_stats_bundle_fields(dm), mh%nstat_samples)

    return
  end subroutine write_mhd_stats_bundle
!==============================================================================
  subroutine read_mhd_stats_bundle(mh, dm)
    use decomp_2d_io
    use decomp_2d_io_object_mpi
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_mhd), intent(inout) :: mh
    type(t_domain), intent(in) :: dm

    character(256) :: bundle_file
    integer :: iter
    type(d2d_io_mpi) :: io

    iter = mh%iterfrom
    call validate_stats_bundle_metadata(dm, 'mhd_stats', iter, mhd_stats_bundle_signature(dm), &
                                        mhd_stats_bundle_fields(dm))
    call generate_pathfile_name(bundle_file, dm%idom, 'mhd_stats', dir_data, 'bin', iter)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The MHD statistics bundle file "//trim(bundle_file)//" does not exist.")
    call io%open(trim(bundle_file), decomp_2d_read_mode)

    if(dm%stat_level > ISTATL0) then
      call stats_bundle_read1(io, dm, mh%tavg_e)
      call stats_bundle_readN(io, dm, mh%tavg_j, 3)
    end if
    if(dm%stat_level > ISTATL1) then
      call stats_bundle_readN(io, dm, mh%tavg_eu, 3)
      call stats_bundle_readN(io, dm, mh%tavg_ej, 3)
      call stats_bundle_readN(io, dm, mh%tavg_ju, 9)
      call stats_bundle_readN(io, dm, mh%tavg_jj, 6)
    end if

    call io%close()

    return
  end subroutine read_mhd_stats_bundle
!==============================================================================
  subroutine require_stats_restart_file(field_name, iter, dm, owner_name)
    use io_tools_mod
    use typeconvert_mod
    use udf_type_mod
    implicit none
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in) :: owner_name
    integer, intent(in) :: iter
    type(t_domain), intent(in) :: dm
    character(64) :: data_flname_path
    logical :: exists

    call generate_pathfile_name(data_flname_path, dm%idom, trim(field_name), dir_data, 'bin', iter)
    inquire(file=trim(data_flname_path), exist=exists)
    if(.not. exists) then
      call Print_error_msg("Statistics restart is incompatible for "//trim(owner_name)// &
        ": missing "//trim(data_flname_path)//". The current input requests stat_level="// &
        trim(int2str(dm%stat_level))//" and iterfrom > stat_istart, so all requested statistics "// &
        "must already exist at the restart iteration. This usually means stat_level was increased "// &
        "after the saved statistics were written. Restart with the old stat_level, or set stat_istart >= iterfrom "// &
        "to start a new statistics window.")
    end if
    return
  end subroutine require_stats_restart_file
!==============================================================================
  subroutine run_stats_action(mode, accc_tavg, field_name, iter, dm, opt_accc, opt_visnm, opt_nstat)
    use io_tools_mod
    use typeconvert_mod
    use udf_type_mod
    use visualisation_field_mod
    use visualisation_spatial_average_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    integer, intent(in) :: iter
    real(WP), contiguous, intent(inout) :: accc_tavg(:, :, :)
    character(len=*), intent(in), optional :: opt_visnm
    real(WP), intent(in), optional :: opt_accc(:, :, :)
    ! Sample population of accc_tavg *including* the one being folded in now.
    ! Required for STATS_TAVG; the owning field counts it, because the elapsed
    ! iteration count is not the sample count whenever the stream has a gap.
    integer, intent(in), optional :: opt_nstat
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
      call write_one_3d_array(accc_tavg, trim(field_name), dm%idom, iter, dm%dccc, dm%existing_output_policy)
      !
    case(STATS_TAVG)
      if(.not. present(opt_accc)) call Print_error_msg("Error. Need Time Averaged Value.")
      if(.not. present(opt_nstat)) call Print_error_msg("Error. A time average needs its sample count.")
      nstat = opt_nstat
      if(nstat > 0) then
        ac = ONE / real(nstat, WP)
        am = real(nstat - 1, WP) / real(nstat, WP)
        accc_tavg = am * accc_tavg + ac * opt_accc
      end if
      !
    case(STATS_VISU3)
      call write_visu_field_bin_and_xdmf(dm, accc_tavg, field_name, trim(opt_visnm), iter, 0, &
                                         opt_is_restart_data=.true.)
    case(STATS_VISU1)
      call write_visu_savg_bin_and_xdmf(dm, accc_tavg, trim(field_name),  trim(opt_visnm), iter)
      !
    case default
      call Print_error_msg("This action mode is not supported.")
      !
    end select
    return
  end subroutine
!==============================================================================
  subroutine run_stats_loops1(mode, accc_tavg, field_name, iter, dm, opt_accc1, opt_accc0, opt_visnm, opt_nstat)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    integer, intent(in), optional :: opt_nstat ! sample count, required for STATS_TAVG
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
    call run_stats_action(mode, accc_tavg, trim(field_name), iter, dm, opt_accc, opt_visnm, opt_nstat)
    if(mode == STATS_TAVG .or. mode == STATS_READ) &
    accc_tavg(:, :, :) = accc_tavg(:, :, :)
    return
  end subroutine
!==============================================================================
  subroutine run_stats_loops3(mode, acccn_tavg, field_name, iter, dm, opt_acccn1, opt_accc0, opt_visnm, opt_nstat)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    integer, intent(in), optional :: opt_nstat ! sample count, required for STATS_TAVG
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
      call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i)), iter, dm, opt_accc, opt_visnm, opt_nstat)
      if(mode == STATS_TAVG .or. mode == STATS_READ)&
      acccn_tavg(:, :, :, i) = accc_tavg(:, :, :)
    end do
    return
  end subroutine
!==============================================================================
  subroutine run_stats_loops6(mode, acccn_tavg, field_name, iter, dm, opt_acccn1, opt_acccn2, opt_accc0, opt_visnm, opt_nstat)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    integer, intent(in), optional :: opt_nstat ! sample count, required for STATS_TAVG
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
          call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i))//trim(int2str(j)), iter, dm, opt_accc, opt_visnm, opt_nstat)
          if(mode == STATS_TAVG .or. mode == STATS_READ)&
          acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
        end if
      end do
    end do
    return
  end subroutine
!==============================================================================
  subroutine run_stats_loops9(mode, acccn_tavg, field_name, iter, dm, opt_acccnn1, opt_accc0, opt_visnm, opt_nstat)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    integer, intent(in), optional :: opt_nstat ! sample count, required for STATS_TAVG
    real(WP), dimension(:, :, :, :),    intent(inout) :: acccn_tavg
    real(WP), dimension(:, :, :, :, :), intent(in), optional :: opt_acccnn1
    real(WP), dimension(:, :, :),       intent(in), optional :: opt_accc0
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer :: n, i, j, ij, s, l, sl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: accc_tavg, opt_accc
    ! format dudx(:, :, :, M, N) = du_M/dx_N
    ! 1 = du1/dx1; 2 = du1/dx2; 3 = du1/dx3
    ! 4 = du2/dx1; 5 = du2/dx2; 6 = du2/dx3
    ! 7 = du3/dx1; 8 = du3/dx2; 9 = du3/dx3
    n = 0
    do i = 1, 3
      do j = 1, 3
        n = n + 1
        accc_tavg(:, :, :) = acccn_tavg(:, :, :, n)
        if(mode == STATS_TAVG) then
          if (.not. present(opt_acccnn1)) call Print_error_msg("Error in run_stats_loops9.")
          opt_accc(:, :, :) = opt_acccnn1(:, :, :, i, j)
          if(present(opt_accc0)) &
          opt_accc(:, :, :) = opt_accc(:, :, :) * opt_accc0(:, :, :)
        end if
        call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i))//trim(int2str(j)), iter, dm, opt_accc, opt_visnm, opt_nstat)
        if(mode == STATS_TAVG .or. mode == STATS_READ)&
        acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
      end do
    end do
    return
  end subroutine
!==============================================================================
  subroutine run_stats_loops10(mode, acccn_tavg, field_name, iter, dm, opt_acccn1, opt_acccn2, opt_acccn3, opt_accc0, opt_visnm, opt_nstat)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    integer, intent(in), optional :: opt_nstat ! sample count, required for STATS_TAVG
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
              call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i))//trim(int2str(j))//trim(int2str(k)), iter, dm, opt_accc, opt_visnm, opt_nstat)
              if(mode == STATS_TAVG .or. mode == STATS_READ)&
              acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
            end if
          end do
        end do
      end do
      return
  end subroutine
!==============================================================================
  subroutine run_stats_loops45(mode, acccn_tavg, field_name, iter, dm, opt_ndudusz, opt_acccnn1, opt_acccnn2, opt_accc0, opt_visnm, opt_nstat)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: mode
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: opt_visnm
    integer, intent(in), optional :: opt_nstat ! sample count, required for STATS_TAVG
    real(WP), dimension(:, :, :, :),    intent(inout) :: acccn_tavg
    real(WP), dimension(:, :, :, :, :), intent(in), optional :: opt_acccnn1, opt_acccnn2
    real(WP), dimension(:, :, :),       intent(in), optional :: opt_accc0
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer, intent(in), optional :: opt_ndudusz
    integer :: n, i, j, ij, s, l, sl, ndudusz
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: accc_tavg, opt_accc
!------------------------------------------------------------------------------
    ![1,1,1,1]; (1,1,1,2); (1,1,1,3); [1,1,2,1]; (1,1,2,2); (1,1,2,3); [1,1,3,1]; (1,1,3,2); (1,1,3,3); index (01-09)
    ![1,2,1,2]; (1,2,1,3); (1,2,2,1); [1,2,2,2]; (1,2,2,3); (1,2,3,1); [1,2,3,2]; (1,2,3,3); [1,3,1,3]; index (10-18)
    !(1,3,2,1); (1,3,2,2); [1,3,2,3]; (1,3,3,1); (1,3,3,2); [1,3,3,3]; [2,1,2,1]; (2,1,2,2); (2,1,2,3); index (19-27)
    ![2,1,3,1]; (2,1,3,2); (2,1,3,3); [2,2,2,2]; (2,2,2,3); (2,2,3,1); [2,2,3,2]; (2,2,3,3); [2,3,2,3]; index (28-36)
    !(2,3,3,1); (2,3,3,2); [2,3,3,3]; [3,1,3,1]; (3,1,3,2); (3,1,3,3); [3,2,3,2]; (3,2,3,3); [3,3,3,3]; index (37-45)
    ! epsilon_{ij} = (i,1,j,1)+(i,2,j,2)+(i,3,j,3)
    ! Storage reserves NDUDU_MAX slots for future full extensions, but the
    ! current statistics/read-write-visualisation path only uses the first
    ! NDUDU_ACTIVE symmetric contracted components.
!------------------------------------------------------------------------------
    if(present(opt_ndudusz)) then
      ndudusz = opt_ndudusz
    else
      ndudusz = NDUDU_MAX
    end if

    if(ndudusz == NDUDU_MAX) then
      n = 0
      do i = 1, 3
        do j = 1, 3
          ij = (i - 1) * 3 + j
          do s = 1, 3
            do l = 1, 3
              sl = (s - 1) * 3 + l
              if (ij<=sl) then
                n = n + 1
                accc_tavg(:, :, :) = acccn_tavg(:, :, :, n)
                if(mode == STATS_TAVG) then
                  if (.not. present(opt_acccnn1)) call Print_error_msg("Error in run_stats_loops45.")
                  if (.not. present(opt_acccnn2)) call Print_error_msg("Error in run_stats_loops45.")
                  opt_accc(:, :, :) = opt_acccnn1(:, :, :, i, j) * opt_acccnn2(:, :, :, s, l)
                  if(present(opt_accc0)) &
                  opt_accc(:, :, :) = opt_accc(:, :, :) * opt_accc0(:, :, :)
                end if
                call run_stats_action(mode, accc_tavg, &
                    trim(field_name)//trim(int2str(i))//trim(int2str(j))//trim(int2str(s))//trim(int2str(l)), &
                    iter, dm, opt_accc, opt_visnm, opt_nstat)
                if(mode == STATS_TAVG .or. mode == STATS_READ)&
                acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
              end if
            end do
          end do
        end do
      end do
    else if (ndudusz == NDUDU_ACTIVE) then
      n = 0
      do i = 1, 3
        do j = i, 3
          n = n + 1
          if (n <= NDUDU_ACTIVE) then
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
            call run_stats_action(mode, accc_tavg, trim(field_name)//trim(int2str(i))//trim(int2str(j)), iter, dm, opt_accc, opt_visnm, opt_nstat)
            if(mode == STATS_TAVG .or. mode == STATS_READ)&
            acccn_tavg(:, :, :, n) = accc_tavg(:, :, :)
          end if
        end do
      end do
    else
      call Print_error_msg('Error in run_stats_loops45')
    end if

    return
  end subroutine
!==============================================================================
  subroutine init_spectrum_uu(fl, dm)
    use udf_type_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(in)    :: dm
    integer :: nkx, nkz
    external :: RFFTI

    fl%nspec_samples = 0
    fl%nspec_istart  = 0

    if(dm%is_periodic(1)) then
      nkx = dm%nc(1) / 2 + 1
      allocate(fl%spec_uu_kx(nkx, dm%dccc%xsz(2)))
      fl%spec_uu_kx = ZERO
      allocate(fl%spec_fft_wx(4 * dm%nc(1) + 15))
      call RFFTI(dm%nc(1), fl%spec_fft_wx)
    end if

    if(dm%is_periodic(3)) then
      nkz = dm%nc(3) / 2 + 1
      allocate(fl%spec_uu_kz(nkz, dm%dccc%zsz(2)))
      fl%spec_uu_kz = ZERO
      allocate(fl%spec_fft_wz(4 * dm%nc(3) + 15))
      call RFFTI(dm%nc(3), fl%spec_fft_wz)
    end if

    if(nrank == 0 .and. (dm%is_periodic(1) .or. dm%is_periodic(3))) &
      call Print_debug_inline_msg("Initialised online Euu spectra.")

    return
  end subroutine
!==============================================================================
  subroutine accumulate_spectrum_uu(fl, dm, uccc)
    use mpi_mod
    use transpose_extended_mod
    use udf_type_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(in)    :: dm
    real(WP), intent(in) :: uccc(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3)

    real(WP) :: umean(dm%nc(2)), umean_work(dm%nc(2))
    real(WP) :: uprime_x(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3))
    real(WP) :: uprime_z(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3))
    integer :: i, j, k, jj

    if((.not. allocated(fl%spec_uu_kx)) .and. (.not. allocated(fl%spec_uu_kz))) return
    if(fl%iteration <= dm%stat_istart) return

    umean = ZERO
    do k = 1, dm%dccc%xsz(3)
      do j = 1, dm%dccc%xsz(2)
        jj = dm%dccc%xst(2) + j - 1
        do i = 1, dm%dccc%xsz(1)
          umean(jj) = umean(jj) + uccc(i, j, k, 1)
        end do
      end do
    end do
    call MPI_ALLREDUCE(umean, umean_work, dm%nc(2), MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
    umean = umean_work / real(dm%nc(1) * dm%nc(3), WP)

    do k = 1, dm%dccc%xsz(3)
      do j = 1, dm%dccc%xsz(2)
        jj = dm%dccc%xst(2) + j - 1
        do i = 1, dm%dccc%xsz(1)
          uprime_x(i, j, k) = uccc(i, j, k, 1) - umean(jj)
        end do
      end do
    end do

    if(allocated(fl%spec_uu_kx)) &
      call accumulate_spectrum_dir(uprime_x, dm%nc(1), fl%spec_fft_wx, fl%spec_uu_kx, 1)

    if(allocated(fl%spec_uu_kz)) then
      call transpose_to_z_pencil(uprime_x, uprime_z, dm%dccc, IPENCIL(1))
      call accumulate_spectrum_dir(uprime_z, dm%nc(3), fl%spec_fft_wz, fl%spec_uu_kz, 3)
    end if

    if(fl%nspec_samples == 0) fl%nspec_istart = fl%iteration
    fl%nspec_samples = fl%nspec_samples + 1

    return
  end subroutine
!==============================================================================
  subroutine accumulate_spectrum_dir(uprime, nfft, wsave, spec, fft_dim)
    implicit none
    integer, intent(in) :: nfft, fft_dim
    real(WP), intent(in) :: uprime(:, :, :)
    real(WP), intent(inout) :: wsave(:)
    real(WP), intent(inout) :: spec(:, :)

    real(WP) :: line(nfft)
    integer :: i, j, k, m, nk
    external :: RFFTF

    nk = nfft / 2 + 1
    if(fft_dim == 1) then
      do k = 1, size(uprime, 3)
        do j = 1, size(uprime, 2)
          line(:) = uprime(:, j, k)
          call RFFTF(nfft, line, wsave)
          do m = 1, nk
            spec(m, j) = spec(m, j) + one_sided_rfft_power(line, nfft, m)
          end do
        end do
      end do
    else if(fft_dim == 3) then
      do j = 1, size(uprime, 2)
        do i = 1, size(uprime, 1)
          line(:) = uprime(i, j, :)
          call RFFTF(nfft, line, wsave)
          do m = 1, nk
            spec(m, j) = spec(m, j) + one_sided_rfft_power(line, nfft, m)
          end do
        end do
      end do
    else
      call Print_error_msg("Unsupported spectrum FFT direction.")
    end if

    return
  end subroutine
!==============================================================================
  pure function one_sided_rfft_power(line, nfft, m) result(power)
    implicit none
    real(WP), intent(in) :: line(:)
    integer, intent(in) :: nfft, m
    real(WP) :: power, scale

    scale = ONE / real(nfft * nfft, WP)
    if(m == 1) then
      power = line(1) * line(1) * scale
    else if(mod(nfft, 2) == 0 .and. m == nfft / 2 + 1) then
      power = line(nfft) * line(nfft) * scale
    else
      power = TWO * (line(2 * m - 2) * line(2 * m - 2) + &
                     line(2 * m - 1) * line(2 * m - 1)) * scale
    end if

    return
  end function
!==============================================================================
  subroutine write_spectrum_uu(fl, dm)
    use udf_type_mod
    implicit none
    type(t_flow),   intent(in) :: fl
    type(t_domain), intent(in) :: dm

    if(fl%nspec_samples <= 0) return

    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      call write_spectrum_bundle_uu(fl, dm)
      return
    end if

    if(allocated(fl%spec_uu_kx)) &
      call write_spectrum_file(fl%spec_uu_kx, dm, fl, 'spectrum_uu_kx', dm%nc(1), dm%nc(3), &
                               dm%dccc%xst(2), dm%lxx)
    if(allocated(fl%spec_uu_kz)) &
      call write_spectrum_file(fl%spec_uu_kz, dm, fl, 'spectrum_uu_kz', dm%nc(3), dm%nc(1), &
                               dm%dccc%zst(2), dm%lzz)

    return
  end subroutine
!==============================================================================
  subroutine write_spectrum_bundle_uu(fl, dm)
    use io_files_mod
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_flow),   intent(in) :: fl
    type(t_domain), intent(in) :: dm

    character(256) :: output_files(1)
    integer :: unit_s
    logical :: do_write

    unit_s = -1
    call generate_pathfile_name(output_files(1), dm%idom, 'spectrum', dir_data, 'dat', fl%iteration)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'spectrum bundle', do_write)
    if(.not. do_write) return

    if(nrank == 0) then
      open(newunit=unit_s, file=trim(output_files(1)), action='write', status='replace')
      write(unit_s, '(A)') '# CHAPSim_spectrum_bundle_v1'
      write(unit_s, '(A,1X,I0)') '# domain', dm%idom
      write(unit_s, '(A,1X,I0)') '# iter', fl%iteration
      write(unit_s, '(A,1X,I0)') '# samples', fl%nspec_samples
      write(unit_s, '(A,1X,I0)') '# window_first_iter', fl%nspec_istart
      write(unit_s, '(A)') '# columns: k_index  y_index  k  y  yplus  E  kE'
    end if

    if(allocated(fl%spec_uu_kx)) &
      call write_spectrum_file(fl%spec_uu_kx, dm, fl, 'spectrum_uu_kx', dm%nc(1), dm%nc(3), &
                               dm%dccc%xst(2), dm%lxx, opt_unit=unit_s, &
                               opt_component='uu', opt_direction='kx')
    if(allocated(fl%spec_uu_kz)) &
      call write_spectrum_file(fl%spec_uu_kz, dm, fl, 'spectrum_uu_kz', dm%nc(3), dm%nc(1), &
                               dm%dccc%zst(2), dm%lzz, opt_unit=unit_s, &
                               opt_component='uu', opt_direction='kz')

    if(nrank == 0) close(unit_s)

    return
  end subroutine write_spectrum_bundle_uu
!==============================================================================
  subroutine write_spectrum_file(spec_local, dm, fl, keyword, nfft, nline_avg, yst_local, length_dir, &
                                 opt_unit, opt_component, opt_direction)
    use io_files_mod
    use io_tools_mod
    use mpi_mod
    use udf_type_mod
    implicit none
    real(WP), intent(in) :: spec_local(:, :)
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl
    character(*), intent(in) :: keyword
    integer, intent(in) :: nfft, nline_avg, yst_local
    real(WP), intent(in) :: length_dir
    integer, intent(in), optional :: opt_unit
    character(*), intent(in), optional :: opt_component, opt_direction

    real(WP), allocatable :: spec_send(:, :), spec_global(:, :)
    real(WP) :: kval, eavg, yplus
    character(256) :: filename
    character(32) :: component, direction
    integer :: m, j, jj, unit_s, nk

    nk = nfft / 2 + 1
    allocate(spec_send(nk, dm%nc(2)))
    allocate(spec_global(nk, dm%nc(2)))
    spec_send = ZERO
    spec_global = ZERO

    do j = 1, size(spec_local, 2)
      jj = yst_local + j - 1
      spec_send(:, jj) = spec_local(:, j)
    end do

    call MPI_REDUCE(spec_send, spec_global, nk * dm%nc(2), MPI_REAL_WP, MPI_SUM, 0, MPI_COMM_WORLD, ierror)

    if(nrank == 0) then
      if(present(opt_unit)) then
        unit_s = opt_unit
        component = 'unknown'
        direction = 'unknown'
        if(present(opt_component)) component = opt_component
        if(present(opt_direction)) direction = opt_direction
        write(unit_s, '(A)') '# BEGIN '//trim(keyword)
        write(unit_s, '(A,1X,A)') '# variable', trim(component)
        write(unit_s, '(A,1X,A)') '# direction', trim(direction)
        write(unit_s, '(A,1X,I0)') '# nfft', nfft
        write(unit_s, '(A,1X,I0)') '# averaged_lines_per_sample', nline_avg
      else
        call generate_pathfile_name(filename, dm%idom, trim(keyword), dir_data, 'dat', fl%iteration)
        open(newunit=unit_s, file=trim(filename), action='write', status='replace')
        ! The averaging window, stated because it need not run from stat_istart:
        ! the spectra are not checkpointed, so a restart begins a fresh window.
        write(unit_s, '(A,1X,I0)') '# samples', fl%nspec_samples
        write(unit_s, '(A,1X,I0)') '# window_first_iter', fl%nspec_istart
        write(unit_s, '(A)') '# k_index  y_index  k  y  yplus  Euu  kEuu'
      end if
      do j = 1, dm%nc(2)
        yplus = spectrum_yplus(dm, fl, j)
        do m = 1, nk
          kval = TWOPI * real(m - 1, WP) / length_dir
          eavg = spec_global(m, j) / real(nline_avg * fl%nspec_samples, WP)
          write(unit_s, '(2I8,5ES24.15)') m, j, kval, dm%yc(j), yplus, eavg, kval * eavg
        end do
      end do
      if(present(opt_unit)) then
        write(unit_s, '(A)') '# END '//trim(keyword)
      else
        close(unit_s)
      end if
    end if

    deallocate(spec_send, spec_global)

    return
  end subroutine
!==============================================================================
  pure function spectrum_yplus(dm, fl, j) result(yplus)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl
    integer, intent(in) :: j
    real(WP) :: yplus

    yplus = spectrum_wall_distance(dm, j) * fl%ren

    return
  end function
!==============================================================================
  pure function spectrum_wall_distance(dm, j) result(ywall)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: j
    real(WP) :: ywall

    if(dm%icase == ICASE_CHANNEL .or. dm%icase == ICASE_ANNULAR) then
      ywall = min(abs(dm%yc(j) - dm%lyb), abs(dm%lyt - dm%yc(j)))
    else if(dm%icase == ICASE_PIPE) then
      ywall = abs(dm%lyt - dm%yc(j))
    else
      ywall = ZERO
    end if

    return
  end function
!==============================================================================
!==============================================================================
  subroutine init_stats_flow(fl, dm)
    use io_tools_mod
    use parameters_constant_mod
    use typeconvert_mod
    use udf_type_mod
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
    if(dm%stat_level > ISTATL0) then
      allocate( fl%tavg_pr  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
      allocate( fl%tavg_u   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
      allocate( fl%tavg_dudx(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 9) )
      allocate( fl%tavg_vort(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
      fl%tavg_u    = ZERO
      fl%tavg_pr   = ZERO
      fl%tavg_dudx = ZERO
      fl%tavg_vort = ZERO
    end if
    if(dm%stat_level > ISTATL1) then
      allocate( fl%tavg_uu  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
      allocate( fl%tavg_vortvort(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
      fl%tavg_uu   = ZERO
      fl%tavg_vortvort = ZERO
      call init_spectrum_uu(fl, dm)
    end if
    if(dm%stat_level > ISTATL2) then
      allocate( fl%tavg_pru (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
      allocate( fl%tavg_uuu (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 10) )
      allocate( fl%tavg_prdu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 9) )
      allocate( fl%tavg_dudu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), NDUDU_MAX) )
      fl%tavg_uuu  = ZERO
      fl%tavg_pru  = ZERO
      fl%tavg_prdu = ZERO
      fl%tavg_dudu = ZERO
    end if
    ! Favre averaging only parameters
    if(dm%is_thermo) then
      if(dm%stat_level > ISTATL0) then
        allocate( fl%tavg_f   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
        allocate( fl%tavg_fu  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
        allocate( fl%tavg_fh  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)   ) )
        fl%tavg_f    = ZERO
        fl%tavg_fu   = ZERO
        fl%tavg_fh   = ZERO
      end if
      if(dm%stat_level > ISTATL1) then
        allocate( fl%tavg_fuu (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
        allocate( fl%tavg_fuh (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
        allocate( fl%tavg_Tu  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) )
        fl%tavg_fuu  = ZERO
        fl%tavg_fuh  = ZERO
        fl%tavg_Tu   = ZERO
      end if
      if(dm%stat_level > ISTATL2) then
        allocate( fl%tavg_fuuu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 10) )
        allocate( fl%tavg_fuuh(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6) )
        fl%tavg_fuuu = ZERO
        fl%tavg_fuuh = ZERO
      end if
    end if
    !
    ! A reset clock restarts the timeline at 0, so the stored averages belong to
    ! a stretch of time this run does not continue and the sample count derived
    ! from iter - stat_istart no longer matches them. Start the accumulators
    ! empty instead; see the restart_clock block in Read_input_parameters.
    if(fl%inittype == INIT_RESTART .and. fl%iterfrom > dm%stat_istart .and. &
       dm%restart_clock == RESTART_CLOCK_CONTINUE) then
      if(nrank == 0) call Print_debug_inline_msg("Reading flow statistics ...")
      call restore_stats_sample_count(dm, 'flow_stats', iter, fl%nstat_samples)
      ! The Euu spectra are deliberately not restored: their only record on disk
      ! is the reduced, normalised text diagnostic, and reconstructing the
      ! per-rank accumulators from it would make restart depend on the format of
      ! a human-readable output file. They therefore cover a shorter window than
      ! the tavg_* fields; both windows are stated in the written headers.
      if(nrank == 0 .and. dm%stat_level > ISTATL1 .and. &
         (dm%is_periodic(1) .or. dm%is_periodic(3))) &
        call Print_warning_msg("Euu spectra restart empty; they average only &
             &from this restart onwards, unlike the time-averaged fields.")
      if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
        call read_flow_stats_bundle(fl, dm)
      else
      if(dm%stat_level > ISTATL0) then
        call require_stats_restart_file('t_avg_pr',   iter, dm, 'flow')
        call require_stats_restart_file('t_avg_u1',   iter, dm, 'flow')
        call require_stats_restart_file('t_avg_dudx11', iter, dm, 'flow')
        call require_stats_restart_file('t_avg_vort1', iter, dm, 'flow')
      end if
      if(dm%stat_level > ISTATL1) then
        call require_stats_restart_file('t_avg_uu11',   iter, dm, 'flow')
        call require_stats_restart_file('t_avg_vortvort11', iter, dm, 'flow')
      end if
      if(dm%stat_level > ISTATL2) then
        call require_stats_restart_file('t_avg_pru1',   iter, dm, 'flow')
        call require_stats_restart_file('t_avg_prdu11', iter, dm, 'flow')
        call require_stats_restart_file('t_avg_uuu111', iter, dm, 'flow')
        call require_stats_restart_file('t_avg_dudu11', iter, dm, 'flow')
      end if
      if(dm%is_thermo) then
        if(dm%stat_level > ISTATL0) then
          call require_stats_restart_file('t_avg_f',   iter, dm, 'flow Favre')
          call require_stats_restart_file('t_avg_fu1', iter, dm, 'flow Favre')
          call require_stats_restart_file('t_avg_fh',  iter, dm, 'flow Favre')
        end if
        if(dm%stat_level > ISTATL1) then
          call require_stats_restart_file('t_avg_fuu11', iter, dm, 'flow Favre')
          call require_stats_restart_file('t_avg_fuh1',  iter, dm, 'flow Favre')
          call require_stats_restart_file('t_avg_Tu1',   iter, dm, 'flow thermo')
        end if
        if(dm%stat_level > ISTATL2) then
          call require_stats_restart_file('t_avg_fuuu111', iter, dm, 'flow Favre')
          call require_stats_restart_file('t_avg_fuuh11',  iter, dm, 'flow Favre')
        end if
      end if
      ! shared parameters
      if(dm%stat_level > ISTATL0) then
        call run_stats_loops1 (STATS_READ, fl%tavg_pr,   't_avg_pr',   iter, dm)
        call run_stats_loops3 (STATS_READ, fl%tavg_u,    't_avg_u',    iter, dm)
        call run_stats_loops9 (STATS_READ, fl%tavg_dudx, 't_avg_dudx', iter, dm)
        call run_stats_loops3 (STATS_READ, fl%tavg_vort, 't_avg_vort', iter, dm)
      end if
      if(dm%stat_level > ISTATL1) then
        call run_stats_loops6 (STATS_READ, fl%tavg_uu,   't_avg_uu',   iter, dm)
        call run_stats_loops6 (STATS_READ, fl%tavg_vortvort, 't_avg_vortvort', iter, dm)
      end if
      if(dm%stat_level > ISTATL2) then
        call run_stats_loops3 (STATS_READ, fl%tavg_pru,  't_avg_pru',  iter, dm)
        call run_stats_loops9 (STATS_READ, fl%tavg_prdu, 't_avg_prdu', iter, dm)
        call run_stats_loops10(STATS_READ, fl%tavg_uuu,  't_avg_uuu',  iter, dm)
        call run_stats_loops45(STATS_READ, fl%tavg_dudu, 't_avg_dudu', iter, dm, opt_ndudusz=NDUDU_ACTIVE)
      end if
      ! farve averaging
      if(dm%is_thermo) then
        if(dm%stat_level > ISTATL0) then
          call run_stats_loops1 (STATS_READ, fl%tavg_f,    't_avg_f',    iter, dm)
          call run_stats_loops3 (STATS_READ, fl%tavg_fu,   't_avg_fu',   iter, dm)
          call run_stats_loops1 (STATS_READ, fl%tavg_fh,   't_avg_fh',   iter, dm)
        end if
        if(dm%stat_level > ISTATL1) then
          call run_stats_loops6 (STATS_READ, fl%tavg_fuu,  't_avg_fuu',  iter, dm)
          call run_stats_loops3 (STATS_READ, fl%tavg_fuh,  't_avg_fuh',  iter, dm)
          call run_stats_loops3 (STATS_READ, fl%tavg_Tu,   't_avg_Tu',   iter, dm)
        end if
        if(dm%stat_level > ISTATL2) then
          call run_stats_loops10(STATS_READ, fl%tavg_fuuu, 't_avg_fuuu', iter, dm)
          call run_stats_loops6 (STATS_READ, fl%tavg_fuuh, 't_avg_fuuh', iter, dm)
        end if
      end if
      end if
    end if
    !
    if(nrank == 0) call Print_debug_end_msg()
    !
    return
  end subroutine

!==============================================================================
!==============================================================================
  subroutine init_stats_thermo(tm, dm)
    use io_tools_mod
    use parameters_constant_mod
    use udf_type_mod
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
    if(dm%stat_level > ISTATL0) then
      allocate( tm%tavg_h   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
      allocate( tm%tavg_T   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
      tm%tavg_h  = ZERO
      tm%tavg_T  = ZERO
    end if
    if(dm%stat_level > ISTATL1) then
      allocate( tm%tavg_TT  (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
      tm%tavg_TT = ZERO
    end if
    !allocate( tm%tavg_dTdT(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6))
    !tm%tavg_dTdT = ZERO
    !
    if(tm%inittype == INIT_RESTART .and. tm%iterfrom > dm%stat_istart .and. &
       dm%restart_clock == RESTART_CLOCK_CONTINUE) then
      call restore_stats_sample_count(dm, 'thermo_stats', iter, tm%nstat_samples)
      if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
        call read_thermo_stats_bundle(tm, dm)
      else
      if(dm%stat_level > ISTATL0) then
        call require_stats_restart_file('t_avg_h',  iter, dm, 'thermo')
        call require_stats_restart_file('t_avg_T',  iter, dm, 'thermo')
      end if
      if(dm%stat_level > ISTATL1) then
        call require_stats_restart_file('t_avg_TT', iter, dm, 'thermo')
      end if
      if(dm%stat_level > ISTATL0) then
        call run_stats_loops1 (STATS_READ, tm%tavg_h,    't_avg_h',    iter, dm)
        call run_stats_loops1 (STATS_READ, tm%tavg_T,    't_avg_T',    iter, dm)
      end if
      if(dm%stat_level > ISTATL1) then
        call run_stats_loops1 (STATS_READ, tm%tavg_TT,   't_avg_TT',   iter, dm)
      end if
      !call run_stats_loops6 (STATS_READ, tm%tavg_dTdT, 't_avg_dTdT', iter, dm)
      end if
    end if
    !
    if(nrank == 0) call Print_debug_end_msg()
    !
    return
  end subroutine
!==============================================================================
!==============================================================================
  subroutine init_stats_mhd(mh, fl, dm)
    use io_tools_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl
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
    if(dm%stat_level > ISTATL0) then
      allocate( mh%tavg_e (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) )
      allocate( mh%tavg_j (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3))
      mh%tavg_e = ZERO
      mh%tavg_j = ZERO
    end if
    if(dm%stat_level > ISTATL1) then
      allocate( mh%tavg_eu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3))
      allocate( mh%tavg_ej(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3))
      allocate( mh%tavg_ju(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 9))
      allocate( mh%tavg_jj(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 6))
      mh%tavg_eu = ZERO
      mh%tavg_ej = ZERO
      mh%tavg_ju = ZERO
      mh%tavg_jj = ZERO
    end if
    !
    ! inittype is the flow's: the MHD field has no initialisation mode of its
    ! own, it is rebuilt from the flow by initialise_mhd. Reading its stored
    ! averages therefore depends on the flow having been restarted, exactly as
    ! in init_stats_flow - without that test a fresh run whose stat_istart sits
    ! below a stale iterfrom would try to read a checkpoint it never wrote.
    if(fl%inittype == INIT_RESTART .and. mh%iterfrom > dm%stat_istart .and. &
       dm%restart_clock == RESTART_CLOCK_CONTINUE) then
      call restore_stats_sample_count(dm, 'mhd_stats', iter, mh%nstat_samples)
      if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
        call read_mhd_stats_bundle(mh, dm)
      else
      if(dm%stat_level > ISTATL0) then
        call require_stats_restart_file('t_avg_e',  iter, dm, 'MHD')
        call require_stats_restart_file('t_avg_j1', iter, dm, 'MHD')
      end if
      if(dm%stat_level > ISTATL1) then
        call require_stats_restart_file('t_avg_eu1', iter, dm, 'MHD')
        call require_stats_restart_file('t_avg_ej1', iter, dm, 'MHD')
        call require_stats_restart_file('t_avg_ju11', iter, dm, 'MHD')
        call require_stats_restart_file('t_avg_jj11', iter, dm, 'MHD')
      end if
      if(dm%stat_level > ISTATL0) then
        call run_stats_loops1(STATS_READ, mh%tavg_e,  't_avg_e',  iter, dm)
        call run_stats_loops3(STATS_READ, mh%tavg_j,  't_avg_j',  iter, dm)
      end if
      if(dm%stat_level > ISTATL1) then
        call run_stats_loops3(STATS_READ, mh%tavg_eu, 't_avg_eu', iter, dm)
        call run_stats_loops3(STATS_READ, mh%tavg_ej, 't_avg_ej', iter, dm)
        call run_stats_loops9(STATS_READ, mh%tavg_ju, 't_avg_ju', iter, dm)
        call run_stats_loops6(STATS_READ, mh%tavg_jj, 't_avg_jj', iter, dm)
      end if
      end if
    end if
    !
    if(nrank == 0) call Print_debug_end_msg()
    !
    return
  end subroutine
!==============================================================================
!==============================================================================

  subroutine update_stats_flow(fl, dm, tm, mh)
    use flow_gradient_mod, only: get_velocity_and_gradient_ccc
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(in), optional :: tm
    type(t_mhd), intent(in), optional :: mh
    !
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: uccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3 ) :: dudx
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: vort
    integer :: iter
    !
    iter = fl%iteration
    ! <= , not < : at iter == stat_istart the old weight nstat = iter - stat_istart
    ! was zero and nothing was folded in, so accumulation has always begun at
    ! stat_istart + 1. The solver loop guards with iter > stat_istart as well.
    if(iter <= dm%stat_istart) return
    ! Count the sample before it is folded in, so nstat is the population of the
    ! accumulator after this call. Everything below shares the one count.
    fl%nstat_samples = fl%nstat_samples + 1
!------------------------------------------------------------------------------
!   preparation for u_i and du_i/dx_j, both cell-centred and in physical
!   components. Shared with the LES models, see flow_gradient_mod.
!------------------------------------------------------------------------------
    if(dm%stat_level > ISTATL0) then
      call get_velocity_and_gradient_ccc(fl, dm, dudx, opt_uccc = uccc)
    end if
!------------------------------------------------------------------------------
!   vorticity from physical velocity-gradient tensor
!------------------------------------------------------------------------------
    if(dm%stat_level > ISTATL0) then
      vort(:, :, :, 1) = dudx(:, :, :, 3, 2) - dudx(:, :, :, 2, 3)
      vort(:, :, :, 2) = dudx(:, :, :, 1, 3) - dudx(:, :, :, 3, 1)
      vort(:, :, :, 3) = dudx(:, :, :, 2, 1) - dudx(:, :, :, 1, 2)
    end if
!------------------------------------------------------------------------------
!   time averaged
!------------------------------------------------------------------------------
    !flow - shared
    if(dm%stat_level > ISTATL0) then
      call run_stats_loops1 (STATS_TAVG, fl%tavg_pr,  't_avg_pr',  iter, dm, opt_accc1=fl%pres, opt_nstat=fl%nstat_samples)
      call run_stats_loops3 (STATS_TAVG, fl%tavg_u,   't_avg_u',   iter, dm, opt_acccn1=uccc, opt_nstat=fl%nstat_samples)
      call run_stats_loops9 (STATS_TAVG, fl%tavg_dudx,'t_avg_dudx',iter, dm, opt_acccnn1=dudx, opt_nstat=fl%nstat_samples)
      call run_stats_loops3 (STATS_TAVG, fl%tavg_vort,'t_avg_vort',iter, dm, opt_acccn1=vort, opt_nstat=fl%nstat_samples)
    end if
    if(dm%stat_level > ISTATL1) then
      call accumulate_spectrum_uu(fl, dm, uccc)
      call run_stats_loops6 (STATS_TAVG, fl%tavg_uu,  't_avg_uu',  iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_nstat=fl%nstat_samples)
      call run_stats_loops6 (STATS_TAVG, fl%tavg_vortvort,'t_avg_vortvort',iter, dm, opt_acccn1=vort, opt_acccn2=vort, opt_nstat=fl%nstat_samples)
    end if
    if(dm%stat_level > ISTATL2) then
      call run_stats_loops3 (STATS_TAVG, fl%tavg_pru, 't_avg_pru', iter, dm, opt_acccn1=uccc, opt_accc0=fl%pres, opt_nstat=fl%nstat_samples)
      call run_stats_loops9 (STATS_TAVG, fl%tavg_prdu,'t_avg_prdu',iter, dm, opt_acccnn1=dudx, opt_accc0=fl%pres, opt_nstat=fl%nstat_samples)
      call run_stats_loops10(STATS_TAVG, fl%tavg_uuu, 't_avg_uuu', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_acccn3=uccc, opt_nstat=fl%nstat_samples)
      call run_stats_loops45(STATS_TAVG, fl%tavg_dudu,'t_avg_dudu',iter, dm, opt_acccnn1=dudx,opt_acccnn2=dudx, opt_ndudusz=NDUDU_ACTIVE, opt_nstat=fl%nstat_samples)
    end if
    ! flow - Favre
    if(dm%is_thermo) then
    if(dm%stat_level > ISTATL0) then
      call run_stats_loops1 (STATS_TAVG, fl%tavg_f,   't_avg_f',   iter, dm, opt_accc1=fl%dDens, opt_nstat=fl%nstat_samples)
      call run_stats_loops3 (STATS_TAVG, fl%tavg_fu,  't_avg_fu',  iter, dm, opt_acccn1=uccc, opt_accc0=fl%dDens, opt_nstat=fl%nstat_samples)
      call run_stats_loops1 (STATS_TAVG, fl%tavg_fh,  't_avg_fh',  iter, dm, opt_accc1=fl%dDens, opt_accc0=tm%hEnth, opt_nstat=fl%nstat_samples)
    end if
    if(dm%stat_level > ISTATL1) then
      call run_stats_loops6 (STATS_TAVG, fl%tavg_fuu, 't_avg_fuu', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_accc0=fl%dDens, opt_nstat=fl%nstat_samples)
      call run_stats_loops3 (STATS_TAVG, fl%tavg_fuh, 't_avg_fuh', iter, dm, opt_acccn1=uccc, opt_accc0=tm%hEnth*fl%dDens, opt_nstat=fl%nstat_samples)
      call run_stats_loops3 (STATS_TAVG, fl%tavg_Tu,  't_avg_Tu',  iter, dm, opt_acccn1=uccc, opt_accc0=tm%tTemp, opt_nstat=fl%nstat_samples)
    end if
    if(dm%stat_level > ISTATL2) then
      call run_stats_loops10(STATS_TAVG, fl%tavg_fuuu,'t_avg_fuuu', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_acccn3=uccc, opt_accc0=fl%dDens*tm%tTemp, opt_nstat=fl%nstat_samples)
      call run_stats_loops6 (STATS_TAVG, fl%tavg_fuuh,'t_avg_fuuh', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_accc0=tm%hEnth*fl%dDens, opt_nstat=fl%nstat_samples)
    end if
   end if
    !
    return
  end subroutine
!==============================================================================
!==============================================================================
  subroutine update_stats_thermo(tm, dm)
    use operations
    use parameters_constant_mod
    use transpose_extended_mod
    use udf_type_mod
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
    if(iter <= dm%stat_istart) return ! see the same guard in update_stats_flow
    tm%nstat_samples = tm%nstat_samples + 1
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
    if(dm%stat_level > ISTATL0) then
      call run_stats_loops1(STATS_TAVG, tm%tavg_h,    't_avg_h',    iter, dm, opt_accc1=tm%hEnth, opt_nstat=tm%nstat_samples)
      call run_stats_loops1(STATS_TAVG, tm%tavg_T,    't_avg_T',    iter, dm, opt_accc1=tm%tTemp, opt_nstat=tm%nstat_samples)
    end if
    if(dm%stat_level > ISTATL1) then
      call run_stats_loops1(STATS_TAVG, tm%tavg_TT,   't_avg_TT',   iter, dm, opt_accc1=tm%tTemp, opt_accc0=tm%tTemp, opt_nstat=tm%nstat_samples)
    end if
    !call run_stats_loops6(STATS_TAVG, tm%tavg_dTdT, 't_avg_dTdT', iter, dm, opt_acccn1=dTdx, opt_acccn2=dTdx)
    !
    return
  end subroutine
!==============================================================================
!==============================================================================
  subroutine update_stats_mhd(mh, fl, dm)
    use cylindrical_rn_mod
    use operations
    use parameters_constant_mod
    use transpose_extended_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    type(t_mhd), intent(inout) :: mh
    !
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: uccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: jccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3 ) :: juccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    integer :: iter, i, j, k, jj
    if(.not. dm%is_mhd) return


    iter = mh%iteration
    if(iter <= dm%stat_istart) return ! see the same guard in update_stats_flow
    mh%nstat_samples = mh%nstat_samples + 1
    !------------------------------------------------------------------------------
    !   preparation for u_i
    !------------------------------------------------------------------------------
    if(dm%stat_level > ISTATL1) then
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
    !   preparation for j_i
    !------------------------------------------------------------------------------
    if(dm%stat_level > ISTATL0) then
    ! j1
    apcc_xpencil = mh%jx
    call Get_x_midp_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, mh%ibcx_jx(:), mh%fbcx_jx)
    jccc(:, :, :, 1) = accc_xpencil(:, :, :)
    ! j2
    acpc_xpencil = mh%jy
    call transpose_x_to_y(acpc_xpencil, acpc_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, mh%ibcy_jy(:), mh%fbcy_jy)
    if(dm%icoordinate == ICYLINDRICAL) then
      do k = 1, dm%dccc%ysz(3)
        do j = 1, dm%dccc%ysz(2)
          jj = dm%dccc%yst(2) + j - 1
          accc_ypencil(:, j, k) = accc_ypencil(:, j, k) * dm%rci(jj)
        end do
      end do
    end if
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    jccc(:, :, :, 2) = accc_xpencil(:, :, :)
    ! j3
    accp_xpencil = mh%jz
    call transpose_to_z_pencil(accp_xpencil, accp_zpencil, dm%dccp, IPENCIL(1))
    call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, mh%ibcz_jz(:), mh%fbcz_jz)
    call transpose_from_z_pencil(accc_zpencil, accc_xpencil, dm%dccc, IPENCIL(1))
    jccc(:, :, :, 3) = accc_xpencil(:, :, :)
    end if
    !
    if(dm%stat_level > ISTATL0) then
    call run_stats_loops1(STATS_TAVG, mh%tavg_e,  't_avg_e',  iter, dm, opt_accc1=mh%ep, opt_nstat=mh%nstat_samples)
    call run_stats_loops3(STATS_TAVG, mh%tavg_j,  't_avg_j',  iter, dm, opt_acccn1=jccc, opt_nstat=mh%nstat_samples)
    end if
    if(dm%stat_level > ISTATL1) then
    do i = 1, 3
      do j = 1, 3
        juccc(:, :, :, i, j) = jccc(:, :, :, i) * uccc(:, :, :, j)
      end do
    end do
    call run_stats_loops3(STATS_TAVG, mh%tavg_eu, 't_avg_eu', iter, dm, opt_acccn1=uccc, opt_accc0=mh%ep, opt_nstat=mh%nstat_samples)
    call run_stats_loops3(STATS_TAVG, mh%tavg_ej, 't_avg_ej', iter, dm, opt_acccn1=jccc, opt_accc0=mh%ep, opt_nstat=mh%nstat_samples)
    call run_stats_loops9(STATS_TAVG, mh%tavg_ju, 't_avg_ju', iter, dm, opt_acccnn1=juccc, opt_nstat=mh%nstat_samples)
    call run_stats_loops6(STATS_TAVG, mh%tavg_jj, 't_avg_jj', iter, dm, opt_acccn1=jccc, opt_acccn2=jccc, opt_nstat=mh%nstat_samples)
    end if
    !
    return
  end subroutine
!==============================================================================
!==============================================================================
  subroutine write_stats_flow(fl, dm)
    use io_tools_mod
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    integer :: iter, i, j, k, s, l, ij, sl, n
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc

    ! here is not only a repeat of those in io_visualisation
    ! because they have different written freqence and to be used for restart as well.
    if(nrank == 0) call Print_debug_inline_msg("Writing flow statistics ...")
    iter = fl%iteration
    if(iter < dm%stat_istart) return
    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      call write_flow_stats_bundle(fl, dm)
      if(dm%stat_level > ISTATL1) call write_spectrum_uu(fl, dm)
      if(nrank == 0) call Print_debug_end_msg()
      return
    end if
    ! shared parameters
    if(dm%stat_level > ISTATL0) then
      call run_stats_loops1 (STATS_WRITE, fl%tavg_pr,  't_avg_pr',   iter, dm)
      call run_stats_loops3 (STATS_WRITE, fl%tavg_u,   't_avg_u',    iter, dm)
      call run_stats_loops9 (STATS_WRITE, fl%tavg_dudx,'t_avg_dudx', iter, dm)
      call run_stats_loops3 (STATS_WRITE, fl%tavg_vort,'t_avg_vort', iter, dm)
    end if
    if(dm%stat_level > ISTATL1) then
      call run_stats_loops6 (STATS_WRITE, fl%tavg_uu,  't_avg_uu',   iter, dm)
      call run_stats_loops6 (STATS_WRITE, fl%tavg_vortvort, 't_avg_vortvort', iter, dm)
      call write_spectrum_uu(fl, dm)
    end if
    if(dm%stat_level > ISTATL2) then
      call run_stats_loops3 (STATS_WRITE, fl%tavg_pru, 't_avg_pru',  iter, dm)
      call run_stats_loops9 (STATS_WRITE, fl%tavg_prdu,'t_avg_prdu', iter, dm)
      call run_stats_loops10(STATS_WRITE, fl%tavg_uuu, 't_avg_uuu',  iter, dm)
      call run_stats_loops45(STATS_WRITE, fl%tavg_dudu,'t_avg_dudu', iter, dm, opt_ndudusz=NDUDU_ACTIVE)
    end if
    ! farve averaging
    if(dm%is_thermo) then
    if(dm%stat_level > ISTATL0) then
      call run_stats_loops1 (STATS_WRITE, fl%tavg_f,   't_avg_f',    iter, dm)
      call run_stats_loops3 (STATS_WRITE, fl%tavg_fu,  't_avg_fu',   iter, dm)
      call run_stats_loops1 (STATS_WRITE, fl%tavg_fh,  't_avg_fh',   iter, dm)
    end if
    if(dm%stat_level > ISTATL1) then
      call run_stats_loops6 (STATS_WRITE, fl%tavg_fuu, 't_avg_fuu',  iter, dm)
      call run_stats_loops3 (STATS_WRITE, fl%tavg_fuh, 't_avg_fuh',  iter, dm)
      call run_stats_loops3 (STATS_WRITE, fl%tavg_Tu,  't_avg_Tu',   iter, dm)
    end if
    if(dm%stat_level > ISTATL2) then
      call run_stats_loops10(STATS_WRITE, fl%tavg_fuuu,'t_avg_fuuu', iter, dm)
      call run_stats_loops6 (STATS_WRITE, fl%tavg_fuuh,'t_avg_fuuh', iter, dm)
    end if
    end if
    ! The per-field layout stores no manifest of its own, so the sample count
    ! would have nowhere to live. Write the same metadata file the bundled
    ! layout writes; only read_stats_sample_count reads it on this path, so the
    ! signature and field list it also carries impose nothing on a restart.
    call write_stats_bundle_metadata(dm, 'flow_stats', iter, flow_stats_bundle_signature(dm), &
                                     flow_stats_bundle_fields(dm), fl%nstat_samples, &
                                     opt_existing_output_policy = dm%existing_output_policy)
    !
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

!==============================================================================
!==============================================================================
  subroutine write_stats_thermo(tm, dm)
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    integer :: iter
    !
    if(.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_inline_msg("Writing thermo statistics ...")
    !
    iter = tm%iteration
    if(iter < dm%stat_istart) return
    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      call write_thermo_stats_bundle(tm, dm)
      if(nrank == 0) call Print_debug_end_msg()
      return
    end if
    ! todo: add bc. visualisation
    if(dm%stat_level > ISTATL0) then
      call run_stats_loops1 (STATS_WRITE, tm%tavg_h,    't_avg_h',    iter, dm)
      call run_stats_loops1 (STATS_WRITE, tm%tavg_T,    't_avg_T',    iter, dm)
    end if
    if(dm%stat_level > ISTATL1) then
      call run_stats_loops1 (STATS_WRITE, tm%tavg_TT,   't_avg_TT',   iter, dm)
    end if
    !call run_stats_loops6 (STATS_WRITE, tm%tavg_dTdT, 't_avg_dTdT', iter, dm)
    call write_stats_bundle_metadata(dm, 'thermo_stats', iter, thermo_stats_bundle_signature(dm), &
                                     thermo_stats_bundle_fields(dm), tm%nstat_samples, &
                                     opt_existing_output_policy = dm%existing_output_policy)
    !
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==============================================================================
!==============================================================================
  subroutine write_stats_mhd(mh, dm)
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_mhd), intent(inout) :: mh
    integer :: iter
    !
    if(.not. dm%is_mhd) return
    if(nrank == 0) call Print_debug_inline_msg("Writing mhd statistics ...")
    iter = mh%iteration
    if(iter < dm%stat_istart) return
    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      call write_mhd_stats_bundle(mh, dm)
      if(nrank == 0) call Print_debug_end_msg()
      return
    end if
    if(dm%stat_level > ISTATL0) then
    call run_stats_loops1(STATS_WRITE, mh%tavg_e,  't_avg_e',  iter, dm)
    call run_stats_loops3(STATS_WRITE, mh%tavg_j,  't_avg_j',  iter, dm)
    end if
    if(dm%stat_level > ISTATL1) then
    call run_stats_loops3(STATS_WRITE, mh%tavg_eu, 't_avg_eu', iter, dm)
    call run_stats_loops3(STATS_WRITE, mh%tavg_ej, 't_avg_ej', iter, dm)
    call run_stats_loops9(STATS_WRITE, mh%tavg_ju, 't_avg_ju', iter, dm)
    call run_stats_loops6(STATS_WRITE, mh%tavg_jj, 't_avg_jj', iter, dm)
    end if
    call write_stats_bundle_metadata(dm, 'mhd_stats', iter, mhd_stats_bundle_signature(dm), &
                                     mhd_stats_bundle_fields(dm), mh%nstat_samples, &
                                     opt_existing_output_policy = dm%existing_output_policy)
    !
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
  !==============================================================================
  subroutine write_visu_stats_flow(fl, dm)
    use precision_mod
    use typeconvert_mod
    use udf_type_mod
    use visualisation_field_mod
    use visualisation_spatial_average_mod, only: begin_visu_profile_bundle, end_visu_profile_bundle
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc
    integer :: iter, i, j, k, s, l, n, ij, sl
    character(64) :: visuname
    logical :: use_profile_bundle
    !
!------------------------------------------------------------------------------
! write time averaged 3d data
!------------------------------------------------------------------------------
    iter = fl%iteration
    if(iter < dm%stat_istart) return
    if(dm%stat_visu_mode /= STAT_VISU_MODE_TSP_ONLY .and. &
       dm%restart_data_layout_write /= RESTART_LAYOUT_BUNDLED) then
      visuname = 't_avg_flow'
      ! write xdmf header
      call write_visu_file_begin(dm, visuname, iter)
      ! shared parameters
      if(dm%stat_level > ISTATL0) then
        call run_stats_loops1 (STATS_VISU3, fl%tavg_pr,   't_avg_pr',   iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops3 (STATS_VISU3, fl%tavg_u,    't_avg_u',    iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops9 (STATS_VISU3, fl%tavg_dudx, 't_avg_dudx', iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops3 (STATS_VISU3, fl%tavg_vort, 't_avg_vort', iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL1) then
        call run_stats_loops6 (STATS_VISU3, fl%tavg_uu,   't_avg_uu',   iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops6 (STATS_VISU3, fl%tavg_vortvort, 't_avg_vortvort', iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL2) then
        call run_stats_loops3 (STATS_VISU3, fl%tavg_pru,  't_avg_pru',  iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops9 (STATS_VISU3, fl%tavg_prdu, 't_avg_prdu', iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops10(STATS_VISU3, fl%tavg_uuu,  't_avg_uuu',  iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops45(STATS_VISU3, fl%tavg_dudu, 't_avg_dudu', iter, dm, opt_visnm=trim(visuname), opt_ndudusz=NDUDU_ACTIVE)
      end if
      ! farve averaging
      if(dm%is_thermo) then
        if(dm%stat_level > ISTATL0) then
          call run_stats_loops1 (STATS_VISU3, fl%tavg_f,    't_avg_f',    iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops3 (STATS_VISU3, fl%tavg_fu,   't_avg_fu',   iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops1 (STATS_VISU3, fl%tavg_fh,   't_avg_fh',   iter, dm, opt_visnm=trim(visuname))
        end if
        if(dm%stat_level > ISTATL1) then
          call run_stats_loops6 (STATS_VISU3, fl%tavg_fuu,  't_avg_fuu',  iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops3 (STATS_VISU3, fl%tavg_fuh,  't_avg_fuh',  iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops3 (STATS_VISU3, fl%tavg_Tu,   't_avg_Tu',   iter, dm, opt_visnm=trim(visuname))
        end if
        if(dm%stat_level > ISTATL2) then
          call run_stats_loops10(STATS_VISU3, fl%tavg_fuuu, 't_avg_fuuu', iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops6 (STATS_VISU3, fl%tavg_fuuh, 't_avg_fuuh', iter, dm, opt_visnm=trim(visuname))
        end if
      end if
      ! write xdmf footer
      call write_visu_file_end(dm, visuname, iter)
    end if
!------------------------------------------------------------------------------
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!------------------------------------------------------------------------------
    if( ANY(dm%is_periodic(:))) then
      visuname = 'tsp_avg_flow'
      use_profile_bundle = (dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED .and. &
                            count(dm%is_periodic(1:3)) == 2 .and. dm%stat_level > ISTATL0)
      if(use_profile_bundle) call begin_visu_profile_bundle(dm, visuname, iter)
      ! write xdmf header for 1-periodic
      if(count(dm%is_periodic(1:3)) == 1) &
      call write_visu_file_begin(dm, visuname, iter, opt_is_savg=.true.)
      ! shared parameters
      if(dm%stat_level > ISTATL0) then
        call run_stats_loops1 (STATS_VISU1, fl%tavg_pr,   'tsp_avg_pr',   iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops3 (STATS_VISU1, fl%tavg_u,    'tsp_avg_u',    iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops9 (STATS_VISU1, fl%tavg_dudx, 'tsp_avg_dudx', iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops3 (STATS_VISU1, fl%tavg_vort, 'tsp_avg_vort', iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL1) then
        call run_stats_loops6 (STATS_VISU1, fl%tavg_uu,   'tsp_avg_uu',   iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops6 (STATS_VISU1, fl%tavg_vortvort, 'tsp_avg_vortvort', iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL2) then
        call run_stats_loops3 (STATS_VISU1, fl%tavg_pru,  'tsp_avg_pru',  iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops9 (STATS_VISU1, fl%tavg_prdu, 'tsp_avg_prdu', iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops10(STATS_VISU1, fl%tavg_uuu,  'tsp_avg_uuu',  iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops45(STATS_VISU1, fl%tavg_dudu, 'tsp_avg_dudu', iter, dm, opt_visnm=trim(visuname), opt_ndudusz=NDUDU_ACTIVE)
      end if
      ! farve averaging
      if(dm%is_thermo) then
        if(dm%stat_level > ISTATL0) then
          call run_stats_loops1 (STATS_VISU1, fl%tavg_f,    'tsp_avg_f',    iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops3 (STATS_VISU1, fl%tavg_fu,   'tsp_avg_fu',   iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops1 (STATS_VISU1, fl%tavg_fh,   'tsp_avg_fh',   iter, dm, opt_visnm=trim(visuname))
        end if
        if(dm%stat_level > ISTATL1) then
          call run_stats_loops6 (STATS_VISU1, fl%tavg_fuu,  'tsp_avg_fuu',  iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops3 (STATS_VISU1, fl%tavg_fuh,  'tsp_avg_fuh',  iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops3 (STATS_VISU1, fl%tavg_Tu,   'tsp_avg_Tu',   iter, dm, opt_visnm=trim(visuname))
        end if
        if(dm%stat_level > ISTATL2) then
          call run_stats_loops10(STATS_VISU1, fl%tavg_fuuu, 'tsp_avg_fuuu', iter, dm, opt_visnm=trim(visuname))
          call run_stats_loops6 (STATS_VISU1, fl%tavg_fuuh, 'tsp_avg_fuuh', iter, dm, opt_visnm=trim(visuname))
        end if
      end if
      ! write xdmf footer
      if(count(dm%is_periodic(1:3)) == 1) &
      call write_visu_file_end(dm, visuname, iter, opt_is_savg=.true.)
      if(use_profile_bundle) call end_visu_profile_bundle()
    end if
    !
    return
  end subroutine

  !==============================================================================
  subroutine write_visu_stats_thermo(tm, dm)
    use precision_mod
    use udf_type_mod
    use visualisation_field_mod
    use visualisation_spatial_average_mod, only: begin_visu_profile_bundle, end_visu_profile_bundle
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    integer :: iter
    character(64) :: visuname
    logical :: use_profile_bundle
!------------------------------------------------------------------------------
! write time averaged 3d data
!------------------------------------------------------------------------------
    iter = tm%iteration
    if(iter < dm%stat_istart) return
    if(dm%stat_visu_mode /= STAT_VISU_MODE_TSP_ONLY .and. &
       dm%restart_data_layout_write /= RESTART_LAYOUT_BUNDLED) then
      visuname = 't_avg_thermo'
      ! write xdmf header
      call write_visu_file_begin(dm, visuname, iter)
      ! write data
      if(dm%stat_level > ISTATL0) then
        call run_stats_loops1(STATS_VISU3, tm%tavg_h,    't_avg_h',    iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops1(STATS_VISU3, tm%tavg_T,    't_avg_T',    iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL1) then
        call run_stats_loops1(STATS_VISU3, tm%tavg_TT,   't_avg_TT',   iter, dm, opt_visnm=trim(visuname))
      end if
      !call run_stats_loops6(STATS_VISU3, tm%tavg_dTdT, 't_avg_dTdT', iter, dm, opt_visnm=trim(visuname))
      ! write xdmf footer
      call write_visu_file_end(dm, visuname, iter)
    end if
!------------------------------------------------------------------------------
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!------------------------------------------------------------------------------
    if( ANY(dm%is_periodic(:))) then
      visuname = 'tsp_avg_thermo'
      use_profile_bundle = (dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED .and. &
                            count(dm%is_periodic(1:3)) == 2 .and. dm%stat_level > ISTATL0)
      if(use_profile_bundle) call begin_visu_profile_bundle(dm, visuname, iter)
      ! write xdmf header
      if(count(dm%is_periodic(1:3)) == 1) &
      call write_visu_file_begin(dm, visuname, iter, opt_is_savg=.true.)
      ! write data
      if(dm%stat_level > ISTATL0) then
        call run_stats_loops1 (STATS_VISU1, tm%tavg_h,    'tsp_avg_h',    iter, dm, opt_visnm=trim(visuname))
        call run_stats_loops1 (STATS_VISU1, tm%tavg_T,    'tsp_avg_T',    iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL1) then
        call run_stats_loops1 (STATS_VISU1, tm%tavg_TT,   'tsp_avg_TT',   iter, dm, opt_visnm=trim(visuname))
      end if
      !call run_stats_loops6 (STATS_VISU1, tm%tavg_dTdT, 'tsp_avg_dTdT', iter, dm, opt_visnm=trim(visuname))
      ! write xdmf footer
      if(count(dm%is_periodic(1:3)) == 1) &
      call write_visu_file_end(dm, visuname, iter, opt_is_savg=.true.)
      if(use_profile_bundle) call end_visu_profile_bundle()
    end if

    return
  end subroutine
!==============================================================================
  subroutine write_visu_stats_mhd(mh, dm)
    use precision_mod
    use udf_type_mod
    use visualisation_field_mod
    use visualisation_spatial_average_mod, only: begin_visu_profile_bundle, end_visu_profile_bundle
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_mhd), intent(inout) :: mh
    integer :: iter
    character(64) :: visuname
    logical :: use_profile_bundle
!------------------------------------------------------------------------------
! write time averaged 3d data
!------------------------------------------------------------------------------
    iter = mh%iteration
    if(iter < dm%stat_istart) return
    if(dm%stat_visu_mode /= STAT_VISU_MODE_TSP_ONLY .and. &
       dm%restart_data_layout_write /= RESTART_LAYOUT_BUNDLED) then
      visuname = 't_avg_mhd'
      ! write xdmf header
      call write_visu_file_begin(dm, visuname, iter)
      ! write data
      if(dm%stat_level > ISTATL0) then
      call run_stats_loops1(STATS_VISU3, mh%tavg_e,  't_avg_e',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3(STATS_VISU3, mh%tavg_j,  't_avg_j',  iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL1) then
      call run_stats_loops3(STATS_VISU3, mh%tavg_eu, 't_avg_eu', iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3(STATS_VISU3, mh%tavg_ej, 't_avg_ej', iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops9(STATS_VISU3, mh%tavg_ju, 't_avg_ju', iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops6(STATS_VISU3, mh%tavg_jj, 't_avg_jj', iter, dm, opt_visnm=trim(visuname))
      end if
      ! write xdmf footer
      call write_visu_file_end(dm, visuname, iter)
    end if
!------------------------------------------------------------------------------
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!------------------------------------------------------------------------------
    if( ANY(dm%is_periodic(:))) then
      visuname = 'tsp_avg_mhd'
      use_profile_bundle = (dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED .and. &
                            count(dm%is_periodic(1:3)) == 2 .and. dm%stat_level > ISTATL0)
      if(use_profile_bundle) call begin_visu_profile_bundle(dm, visuname, iter)
      ! write xdmf header
      if(count(dm%is_periodic(1:3)) == 1) &
      call write_visu_file_begin(dm, visuname, iter, opt_is_savg=.true.)
      ! write data
      if(dm%stat_level > ISTATL0) then
      call run_stats_loops1(STATS_VISU1, mh%tavg_e,  'tsp_avg_e',  iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3(STATS_VISU1, mh%tavg_j,  'tsp_avg_j',  iter, dm, opt_visnm=trim(visuname))
      end if
      if(dm%stat_level > ISTATL1) then
      call run_stats_loops3(STATS_VISU1, mh%tavg_eu, 'tsp_avg_eu', iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops3(STATS_VISU1, mh%tavg_ej, 'tsp_avg_ej', iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops9(STATS_VISU1, mh%tavg_ju, 'tsp_avg_ju', iter, dm, opt_visnm=trim(visuname))
      call run_stats_loops6(STATS_VISU1, mh%tavg_jj, 'tsp_avg_jj', iter, dm, opt_visnm=trim(visuname))
      end if

      ! write xdmf footer
      if(count(dm%is_periodic(1:3)) == 1) &
      call write_visu_file_end(dm, visuname, iter, opt_is_savg=.true.)
      if(use_profile_bundle) call end_visu_profile_bundle()
    end if

    return
  end subroutine
end module
