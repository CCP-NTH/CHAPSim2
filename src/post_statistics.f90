
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

    character(256):: data_flname
    character(256):: data_flname_path
    character(256):: visu_flname_path
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
  public  :: store_stats_prev_flow
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
  !
  ! statistical-window reconstruction (post-processing mode)
  public  :: stats_window_mean
  public  :: check_stats_window
  public  :: read_stats_window
  public  :: write_stats_window
  private :: stats_window_combine
  private :: combine_window_flow
  private :: combine_window_thermo
  private :: combine_window_mhd
  private :: read_window_meta
  private :: check_window_snapshot
  private :: check_window_group
  private :: check_window_joint_thermo
  private :: check_window_checkpoint
  private :: check_window_variance
  private :: append_window_meta_keys
  private :: pp_note
  !
  ! State shared by read_stats_window and write_stats_window: per group
  ! (1 flow, 2 thermo, 3 MHD) the sample counts at a and b and the field
  ! list, the snapshot times, and the provenance lines collected on rank 0.
  integer, parameter :: PP_NGROUP = 3
  character(len=12), parameter :: pp_group_name(PP_NGROUP) = &
    [character(len=12) :: 'flow_stats', 'thermo_stats', 'mhd_stats']
  logical  :: pp_group_on(PP_NGROUP) = .false.
  integer  :: pp_na(PP_NGROUP) = 0, pp_nb(PP_NGROUP) = 0
  character(len=4096) :: pp_fields(PP_NGROUP) = ''
  ! joint-interval status of flow/thermo (1) and flow/MHD (2); '' = not present
  character(len=512) :: pp_joint(2) = ''
  character(len=64)  :: pp_fuuu_def = ''   ! fuuu_definition of the window ('' = former rho*T weight)
  logical  :: pp_has_time(2) = .false.
  real(WP) :: pp_time(2) = ZERO, pp_dt(2) = ZERO
  integer, parameter :: PP_REPORT_MAX = 400
  character(len=4200) :: pp_report(PP_REPORT_MAX)
  integer :: pp_nreport = 0
contains
!==============================================================================
  function flow_stats_bundle_signature(dm) result(sig)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(256) :: sig

    write(sig, '(A,I0,A,L1)') 'flow_stats_v1;stat_level=', dm%stat_level, ';is_thermo=', dm%is_thermo
    ! B3.5: full-step pressure statistics change the bundle content (versioned)
    ! full-step pressure statistics change the bundle content (versioned; v2 = completed-step pairing)
    if(is_pfs_stats .and. dm%stat_level > ISTATL0) sig = trim(sig)//';pfs_stats=v2'
    if(is_pfs_stats_mid .and. dm%stat_level > ISTATL2) sig = trim(sig)//';pfs_mid=T'
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
    if(is_pfs_stats) then
      if(dm%stat_level > ISTATL0) then
        call append_stats_field(fields, 't_avg_prfs')
        call append_stats_field(fields, 't_avg_prfs2')
      end if
      if(dm%stat_level > ISTATL2) then
        call append_stats_components(fields, 't_avg_prfsu', 3)
        call append_stats_components9(fields, 't_avg_prfsdu')
      end if
    end if
    if(is_pfs_stats_mid .and. dm%stat_level > ISTATL2) then
      call append_stats_components(fields, 't_avg_prfsmu', 3)
      call append_stats_components9(fields, 't_avg_prfsmdu')
      call append_stats_components(fields, 't_avg_umid', 3)
      call append_stats_components9(fields, 't_avg_dudxmid')
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
    ! t_avg_fuuu holds rho u_i u_j u_k (rho T u_i u_j u_k in files without this key)
    if(index(fields, 't_avg_fuuu') > 0) write(u, '(A)') 'fuuu_definition rho'
    ! labels are run iterations for every group (thermal files of builds before 2026-10-30 were
    ! labelled by the thermal step count; check_window_joint_thermo)
    write(u, '(A)') 'label_clock run'
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
  !> Value of a trailing key of a statistics metadata file ('' when the key or the file is absent).
  function stats_meta_key(dm, group_name, iter, key) result(val)
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name, key
    integer, intent(in) :: iter
    character(64) :: val
    character(256) :: meta_file
    character(4096) :: line
    integer :: u, ioerr, sp

    val = ''
    call generate_pathfile_name(meta_file, dm%idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
    if(.not. file_exists(trim(meta_file))) return
    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    if(ioerr /= 0) return
    do
      read(u, '(A)', iostat=ioerr) line
      if(ioerr /= 0) exit
      line = adjustl(line)
      sp = index(line, ' ')
      if(sp <= 1) cycle
      if(line(1:sp - 1) == trim(key)) then
        val = adjustl(line(sp + 1:))
        exit
      end if
    end do
    close(u)
  end function stats_meta_key
!==============================================================================
  !> Flow statistics that hold t_avg_fuuu must have been accumulated with its current definition,
  !> rho u_i u_j u_k (key fuuu_definition rho). Files without the key hold rho T u_i u_j u_k; adding
  !> new samples to them would average two different quantities, so a restart stops.
  subroutine check_stats_fuuu_definition(dm, iter)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    if(.not. (dm%is_thermo .and. dm%stat_level > ISTATL2)) return
    if(trim(stats_meta_key(dm, 'flow_stats', iter, 'fuuu_definition')) == 'rho') return
    call Print_error_msg('The flow statistics at iteration '//trim(int2str(iter))//' hold t_avg_fuuu '// &
      'with its former definition rho*T*u_i u_j u_k (no fuuu_definition key); it now accumulates the '// &
      'Favre triple moment rho*u_i u_j u_k. Continuing would average two different quantities. Start a '// &
      'fresh average (stat_istart at or above the restart iteration), or use stat_level <= 2.')
  end subroutine check_stats_fuuu_definition
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
    if(.not. file_exists(trim(meta_file))) then
      if(index(group_name, 'thermo') > 0) call Print_error_msg("The statistics bundle metadata file "// &
        trim(meta_file)//" does not exist. Thermal statistics written by builds before 2026-10-30 are "// &
        "labelled by the thermal step count: if the energy equation started after the flow "// &
        "(niterthermofirst > niterflowfirst), their labels are lower than the flow step at which they "// &
        "were written.")
      call Print_error_msg("The statistics bundle metadata file "//trim(meta_file)//" does not exist.")
    end if

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
  !> Stop unless the statistics .bin holds exactly the arrays of the validated
  !> field list. Every entry is one full cell-centred (dccc) array, written in
  !> list order by stats_bundle_write1/N, and an MPI-IO read past the end of the
  !> file is not an error, so a truncated or padded bundle would otherwise be read
  !> without notice (same rule as check_bundle_file_size for restart bundles).
  !> Rank 0 measures the file and broadcasts the size, so every rank reaches the
  !> same verdict. Collective: call on all ranks.
  subroutine check_stats_bundle_file_size(dm, group_name, iter, fields)
    use iso_fortran_env, only: int64, real32
    use decomp_2d_constants, only: DEFAULT_OPT_REDUCE_PREC
    use mpi_mod
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    integer,      intent(in) :: iter
    character(*), intent(in) :: fields

    character(256) :: bin_file, meta_file
    character(1024) :: msg
    integer(int64) :: expected_bytes, file_bytes, value_bytes, cells
    integer :: narrays, k

    ! bytes per stored value as decomp_2d_write_var stores it (real32 under SAVE_SINGLE)
    value_bytes = int(storage_size(1.0_WP) / 8, int64)
    if(DEFAULT_OPT_REDUCE_PREC) value_bytes = int(storage_size(1.0_real32) / 8, int64)
    ! global dccc size: full extent of each direction in its own pencil
    cells = int(dm%dccc%xsz(1), int64) * int(dm%dccc%ysz(2), int64) * int(dm%dccc%zsz(3), int64)
    narrays = 0
    if(len_trim(fields) > 0) then
      narrays = 1
      do k = 1, len_trim(fields)
        if(fields(k:k) == ',') narrays = narrays + 1
      end do
    end if
    expected_bytes = int(narrays, int64) * cells * value_bytes

    call generate_pathfile_name(bin_file, dm%idom, trim(group_name), dir_data, 'bin', iter)
    call generate_pathfile_name(meta_file, dm%idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
    file_bytes = -1_int64
    if(nrank == 0) then
      if(file_exists(trim(bin_file))) inquire(file=trim(bin_file), size=file_bytes)
    end if
    call mpi_bcast(file_bytes, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierror)

    if(file_bytes < 0_int64) &
    call Print_error_msg("The statistics bundle file "//trim(bin_file)//" does not exist or its size cannot be read.")
    if(file_bytes /= expected_bytes) then
      write(msg, '(A,A,I0,A,I0,A,I0,A,A)') trim(bin_file), ' has ', file_bytes, ' bytes, expected ', &
        expected_bytes, ' for the ', narrays, ' arrays listed in ', trim(meta_file)
      if(file_bytes < expected_bytes) then
        call Print_error_msg("Statistics bundle size mismatch (file too short): "//trim(msg))
      else
        call Print_error_msg("Statistics bundle size mismatch (file too long): "//trim(msg))
      end if
    end if

    return
  end subroutine check_stats_bundle_file_size
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
    if(is_pfs_stats) then
      if(dm%stat_level > ISTATL0) then
        call stats_bundle_write1(io, dm, fl%tavg_prfs)
        call stats_bundle_write1(io, dm, fl%tavg_prfs2)
      end if
      if(dm%stat_level > ISTATL2) then
        call stats_bundle_writeN(io, dm, fl%tavg_prfsu,  3)
        call stats_bundle_writeN(io, dm, fl%tavg_prfsdu, 9)
      end if
    end if
    if(is_pfs_stats_mid .and. dm%stat_level > ISTATL2) then
      call stats_bundle_writeN(io, dm, fl%tavg_prfsmu,  3)
      call stats_bundle_writeN(io, dm, fl%tavg_prfsmdu, 9)
      call stats_bundle_writeN(io, dm, fl%tavg_umid,    3)
      call stats_bundle_writeN(io, dm, fl%tavg_dudxmid, 9)
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
    if(is_pfs_stats .and. nrank == 0) call Print_note_msg('Full-step pressure statistics (CHAPSIM_B3_PSTAT=1): '// &
      'the flow statistics restart must have been written with the same setting (signature pfs_stats=T).')
    call validate_stats_bundle_metadata(dm, 'flow_stats', iter, flow_stats_bundle_signature(dm), &
                                        flow_stats_bundle_fields(dm))
    call check_stats_bundle_file_size(dm, 'flow_stats', iter, flow_stats_bundle_fields(dm))
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
    if(is_pfs_stats) then
      if(dm%stat_level > ISTATL0) then
        call stats_bundle_read1(io, dm, fl%tavg_prfs)
        call stats_bundle_read1(io, dm, fl%tavg_prfs2)
      end if
      if(dm%stat_level > ISTATL2) then
        call stats_bundle_readN(io, dm, fl%tavg_prfsu,  3)
        call stats_bundle_readN(io, dm, fl%tavg_prfsdu, 9)
      end if
    end if
    if(is_pfs_stats_mid .and. dm%stat_level > ISTATL2) then
      call stats_bundle_readN(io, dm, fl%tavg_prfsmu,  3)
      call stats_bundle_readN(io, dm, fl%tavg_prfsmdu, 9)
      call stats_bundle_readN(io, dm, fl%tavg_umid,    3)
      call stats_bundle_readN(io, dm, fl%tavg_dudxmid, 9)
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
    call check_stats_bundle_file_size(dm, 'thermo_stats', iter, thermo_stats_bundle_fields(dm))
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
    use checkpoint_metadata_mod, only: write_checkpoint_manifest, read_checkpoint_metadata
    use io_tools_mod
    use udf_type_mod
    implicit none
    type(t_mhd), intent(in) :: mh
    type(t_domain), intent(in) :: dm

    character(256) :: bundle_file
    character(256) :: output_files(2)
    integer :: iter
    logical :: do_write
    logical :: found_manifest
    real(WP) :: time_manifest, dt_manifest
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
    ! The MHD bundle is written after the flow (and thermo) bundles of the same checkpoint, whose
    ! writers have already produced checkpoint_meta without it. Rewrite the manifest so that it lists
    ! mhd_stats too; time and dt come from that manifest because t_mhd carries no clock of its own
    ! (ES24.16 round-trips a double exactly). No manifest yet: nothing to refresh.
    if(nrank == 0) then
      call read_checkpoint_metadata(dm%idom, iter, time_manifest, dt_manifest, found_manifest)
      if(found_manifest) call write_checkpoint_manifest(dm%idom, iter, time_manifest, dt_manifest)
    end if

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
    call check_stats_bundle_file_size(dm, 'mhd_stats', iter, mhd_stats_bundle_fields(dm))
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
    ! allocate once for all domains, fill the calling domain every time (statistics cover
    ! every cell: no cell skip is implemented)
    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0
    end if
    ncl_stat(1:3, dm%idom) = dm%dccc%xsz(1:3)
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
    ! B3.5 experimental full-step pressure statistics (bundled layout only)
    if(is_pfs_stats .and. dm%stat_level > ISTATL0) then
      if(dm%restart_data_layout_write /= RESTART_LAYOUT_BUNDLED .or. &
         dm%restart_data_layout_read  /= RESTART_LAYOUT_BUNDLED) &
        call Print_error_msg('CHAPSIM_B3_PSTAT=1 (full-step pressure statistics) requires restart_data_layout = bundled.')
      allocate( fl%tavg_prfs (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) ); fl%tavg_prfs  = ZERO
      allocate( fl%tavg_prfs2(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom)) ); fl%tavg_prfs2 = ZERO
      if(dm%stat_level > ISTATL2) then
        allocate( fl%tavg_prfsu (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) ); fl%tavg_prfsu  = ZERO
        allocate( fl%tavg_prfsdu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 9) ); fl%tavg_prfsdu = ZERO
        if(is_pfs_stats_mid) then
          allocate( fl%uccc_prev(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3) ); fl%uccc_prev = ZERO
          allocate( fl%dudx_prev(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3) ); fl%dudx_prev = ZERO
          allocate( fl%tavg_prfsmu (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) ); fl%tavg_prfsmu  = ZERO
          allocate( fl%tavg_prfsmdu(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 9) ); fl%tavg_prfsmdu = ZERO
          allocate( fl%tavg_umid   (ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 3) ); fl%tavg_umid    = ZERO
          allocate( fl%tavg_dudxmid(ncl_stat(1, dm%idom), ncl_stat(2, dm%idom), ncl_stat(3, dm%idom), 9) ); fl%tavg_dudxmid = ZERO
        end if
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
      call check_stats_fuuu_definition(dm, iter)
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
    ! allocate once for all domains, fill the calling domain every time (statistics cover
    ! every cell: no cell skip is implemented)
    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0
    end if
    ncl_stat(1:3, dm%idom) = dm%dccc%xsz(1:3)
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
    ! allocate once for all domains, fill the calling domain every time (statistics cover
    ! every cell: no cell skip is implemented)
    if(.not. allocated(ncl_stat)) then
      allocate (ncl_stat(3, nxdomain))
      ncl_stat = 0
    end if
    ncl_stat(1:3, dm%idom) = dm%dccc%xsz(1:3)
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
    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! Experimental full-step pressure statistics: Pbar of the completed step
    ! [t-dt, t] paired with the completed-step velocity / gradient u(t), grad u(t)
    ! (preferred pairing; first order). Same sample count and sub-grid as all
    ! other averages. Covariances: <p'u'> = <Pbar u> - <Pbar><u>, using the
    ! existing <u>, <du/dx> sampled at the same times. Optional midpoint pairing
    ! with its own <u_mid>, <grad u_mid> (consistent covariances).
    !------------------------------------------------------------------------
    if(is_pfs_stats .and. dm%stat_level > ISTATL0) then
      if(.not. fl%is_pres_rk_valid) call Print_error_msg('Full-step pressure statistics: Pbar not available.')
      call run_stats_loops1 (STATS_TAVG, fl%tavg_prfs,  't_avg_prfs',  iter, dm, opt_accc1=fl%pres_rk, &
                             opt_nstat=fl%nstat_samples)
      call run_stats_loops1 (STATS_TAVG, fl%tavg_prfs2, 't_avg_prfs2', iter, dm, opt_accc1=fl%pres_rk, &
                             opt_accc0=fl%pres_rk, opt_nstat=fl%nstat_samples)
      if(dm%stat_level > ISTATL2) then
        call run_stats_loops3 (STATS_TAVG, fl%tavg_prfsu, 't_avg_prfsu', iter, dm, opt_acccn1=uccc, &
                               opt_accc0=fl%pres_rk, opt_nstat=fl%nstat_samples)
        call run_stats_loops9 (STATS_TAVG, fl%tavg_prfsdu,'t_avg_prfsdu',iter, dm, opt_acccnn1=dudx, &
                               opt_accc0=fl%pres_rk, opt_nstat=fl%nstat_samples)
        if(is_pfs_stats_mid) then
          if(.not. fl%is_stats_prev_valid) call Print_error_msg( &
            'Midpoint pressure statistics: previous-step velocity not available (internal sequencing error).')
          fl%uccc_prev = HALF * (fl%uccc_prev + uccc)
          fl%dudx_prev = HALF * (fl%dudx_prev + dudx)
          call run_stats_loops3 (STATS_TAVG, fl%tavg_prfsmu, 't_avg_prfsmu', iter, dm, opt_acccn1=fl%uccc_prev, &
                                 opt_accc0=fl%pres_rk, opt_nstat=fl%nstat_samples)
          call run_stats_loops9 (STATS_TAVG, fl%tavg_prfsmdu,'t_avg_prfsmdu',iter, dm, opt_acccnn1=fl%dudx_prev, &
                                 opt_accc0=fl%pres_rk, opt_nstat=fl%nstat_samples)
          call run_stats_loops3 (STATS_TAVG, fl%tavg_umid, 't_avg_umid', iter, dm, opt_acccn1=fl%uccc_prev, &
                                 opt_nstat=fl%nstat_samples)
          call run_stats_loops9 (STATS_TAVG, fl%tavg_dudxmid,'t_avg_dudxmid',iter, dm, opt_acccnn1=fl%dudx_prev, &
                                 opt_nstat=fl%nstat_samples)
          fl%uccc_prev = uccc
          fl%dudx_prev = dudx
        end if
      end if
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
      ! Favre triple moment rho u_i u_j u_k, the turbulent-transport moment of the Favre stress budget.
      ! Until 2026-10-30 the weight was rho*T, copied from the former t_avg_tfuu (rho T u_i u_j); the
      ! metadata key fuuu_definition separates the two (see check_stats_fuuu_definition).
      call run_stats_loops10(STATS_TAVG, fl%tavg_fuuu,'t_avg_fuuu', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_acccn3=uccc, opt_accc0=fl%dDens, opt_nstat=fl%nstat_samples)
      call run_stats_loops6 (STATS_TAVG, fl%tavg_fuuh,'t_avg_fuuh', iter, dm, opt_acccn1=uccc, opt_acccn2=uccc, opt_accc0=tm%hEnth*fl%dDens, opt_nstat=fl%nstat_samples)
    end if
   end if
    !
    return
  end subroutine
!==============================================================================
  !> B3.5: store u and grad(u) of the current completed state as the previous
  !> step of the next full-step pressure-statistics sample. Called after the
  !> initial / restart state is set up and at iter == stat_istart (the first
  !> sample is taken at stat_istart + 1); later samples update it themselves.
  !> A restart therefore needs no extra field: the restart state is the
  !> previous completed step of the first step after the restart.
  subroutine store_stats_prev_flow(fl, dm)
    use flow_gradient_mod, only: get_velocity_and_gradient_ccc
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(inout) :: fl
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3 ) :: uccc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3 ) :: dudx

    if(.not. (is_pfs_stats_mid .and. dm%stat_level > ISTATL2)) return
    call get_velocity_and_gradient_ccc(fl, dm, dudx, opt_uccc = uccc)
    fl%uccc_prev = uccc
    fl%dudx_prev = dudx
    fl%is_stats_prev_valid = .true.

    return
  end subroutine store_stats_prev_flow
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
!==============================================================================
!==============================================================================
! Statistical-window reconstruction (is_postprocess, pp_stat_window = a, b).
!
! Every stored statistic is a running mean A(t) = S(t)/N(t) of the samples
! stat_istart+1 ... t (run_stats_action, STATS_TAVG), N counted per group
! (flow, thermo, MHD). The mean over the samples a+1 ... b is
!     A_ab = (N_b A_b - N_a A_a) / (N_b - N_a)
!          = A_b + N_a/(N_b - N_a) * (A_b - A_a),
! the second form differencing two like-magnitude means instead of two large
! products. Only stored raw moments are combined; central moments (rms,
! Reynolds stresses, Favre fluxes) are formed from the combined raw moments
! downstream, never by differencing derived quantities. Samples are one per
! iteration, so the result is iteration-weighted exactly as the stored
! statistics are; it equals a discrete time average only for constant dt.
!==============================================================================
  elemental function stats_window_mean(avg_a, avg_b, na, nb) result(avg_ab)
    implicit none
    real(WP), intent(in) :: avg_a, avg_b
    integer,  intent(in) :: na, nb
    real(WP) :: avg_ab

    avg_ab = avg_b + (real(na, WP) / real(nb - na, WP)) * (avg_b - avg_a)
    return
  end function stats_window_mean
!==============================================================================
  ! In place, avg_b <- A_ab. Explicit-shape dummies: the allocatable tavg_*
  ! arrays are contiguous, so arrays of any rank are passed without a copy.
  subroutine stats_window_combine(n, avg_b, avg_a, na, nb)
    implicit none
    integer,  intent(in) :: n, na, nb
    real(WP), intent(inout) :: avg_b(n)
    real(WP), intent(in) :: avg_a(n)
    integer :: i

    do i = 1, n
      avg_b(i) = stats_window_mean(avg_a(i), avg_b(i), na, nb)
    end do
    return
  end subroutine stats_window_combine
!==============================================================================
  subroutine combine_window_flow(fl, fl_a, na, nb)
    use udf_type_mod
    implicit none
    type(t_flow), intent(inout) :: fl
    type(t_flow), intent(in)    :: fl_a
    integer, intent(in) :: na, nb

    if(allocated(fl%tavg_pr))      call stats_window_combine(size(fl%tavg_pr),      fl%tavg_pr,      fl_a%tavg_pr,      na, nb)
    if(allocated(fl%tavg_u))       call stats_window_combine(size(fl%tavg_u),       fl%tavg_u,       fl_a%tavg_u,       na, nb)
    if(allocated(fl%tavg_dudx))    call stats_window_combine(size(fl%tavg_dudx),    fl%tavg_dudx,    fl_a%tavg_dudx,    na, nb)
    if(allocated(fl%tavg_vort))    call stats_window_combine(size(fl%tavg_vort),    fl%tavg_vort,    fl_a%tavg_vort,    na, nb)
    if(allocated(fl%tavg_uu))      call stats_window_combine(size(fl%tavg_uu),      fl%tavg_uu,      fl_a%tavg_uu,      na, nb)
    if(allocated(fl%tavg_vortvort))call stats_window_combine(size(fl%tavg_vortvort),fl%tavg_vortvort,fl_a%tavg_vortvort,na, nb)
    if(allocated(fl%tavg_pru))     call stats_window_combine(size(fl%tavg_pru),     fl%tavg_pru,     fl_a%tavg_pru,     na, nb)
    if(allocated(fl%tavg_prdu))    call stats_window_combine(size(fl%tavg_prdu),    fl%tavg_prdu,    fl_a%tavg_prdu,    na, nb)
    if(allocated(fl%tavg_uuu))     call stats_window_combine(size(fl%tavg_uuu),     fl%tavg_uuu,     fl_a%tavg_uuu,     na, nb)
    if(allocated(fl%tavg_dudu))    call stats_window_combine(size(fl%tavg_dudu),    fl%tavg_dudu,    fl_a%tavg_dudu,    na, nb)
    if(allocated(fl%tavg_f))       call stats_window_combine(size(fl%tavg_f),       fl%tavg_f,       fl_a%tavg_f,       na, nb)
    if(allocated(fl%tavg_fu))      call stats_window_combine(size(fl%tavg_fu),      fl%tavg_fu,      fl_a%tavg_fu,      na, nb)
    if(allocated(fl%tavg_fh))      call stats_window_combine(size(fl%tavg_fh),      fl%tavg_fh,      fl_a%tavg_fh,      na, nb)
    if(allocated(fl%tavg_fuu))     call stats_window_combine(size(fl%tavg_fuu),     fl%tavg_fuu,     fl_a%tavg_fuu,     na, nb)
    if(allocated(fl%tavg_fuh))     call stats_window_combine(size(fl%tavg_fuh),     fl%tavg_fuh,     fl_a%tavg_fuh,     na, nb)
    if(allocated(fl%tavg_Tu))      call stats_window_combine(size(fl%tavg_Tu),      fl%tavg_Tu,      fl_a%tavg_Tu,      na, nb)
    if(allocated(fl%tavg_fuuu))    call stats_window_combine(size(fl%tavg_fuuu),    fl%tavg_fuuu,    fl_a%tavg_fuuu,    na, nb)
    if(allocated(fl%tavg_fuuh))    call stats_window_combine(size(fl%tavg_fuuh),    fl%tavg_fuuh,    fl_a%tavg_fuuh,    na, nb)
    if(allocated(fl%tavg_prfs))    call stats_window_combine(size(fl%tavg_prfs),    fl%tavg_prfs,    fl_a%tavg_prfs,    na, nb)
    if(allocated(fl%tavg_prfs2))   call stats_window_combine(size(fl%tavg_prfs2),   fl%tavg_prfs2,   fl_a%tavg_prfs2,   na, nb)
    if(allocated(fl%tavg_prfsu))   call stats_window_combine(size(fl%tavg_prfsu),   fl%tavg_prfsu,   fl_a%tavg_prfsu,   na, nb)
    if(allocated(fl%tavg_prfsdu))  call stats_window_combine(size(fl%tavg_prfsdu),  fl%tavg_prfsdu,  fl_a%tavg_prfsdu,  na, nb)
    if(allocated(fl%tavg_prfsmu))  call stats_window_combine(size(fl%tavg_prfsmu),  fl%tavg_prfsmu,  fl_a%tavg_prfsmu,  na, nb)
    if(allocated(fl%tavg_prfsmdu)) call stats_window_combine(size(fl%tavg_prfsmdu), fl%tavg_prfsmdu, fl_a%tavg_prfsmdu, na, nb)
    if(allocated(fl%tavg_umid))    call stats_window_combine(size(fl%tavg_umid),    fl%tavg_umid,    fl_a%tavg_umid,    na, nb)
    if(allocated(fl%tavg_dudxmid)) call stats_window_combine(size(fl%tavg_dudxmid), fl%tavg_dudxmid, fl_a%tavg_dudxmid, na, nb)
    return
  end subroutine combine_window_flow
!==============================================================================
  subroutine combine_window_thermo(tm, tm_a, na, nb)
    use udf_type_mod
    implicit none
    type(t_thermo), intent(inout) :: tm
    type(t_thermo), intent(in)    :: tm_a
    integer, intent(in) :: na, nb

    if(allocated(tm%tavg_h))  call stats_window_combine(size(tm%tavg_h),  tm%tavg_h,  tm_a%tavg_h,  na, nb)
    if(allocated(tm%tavg_T))  call stats_window_combine(size(tm%tavg_T),  tm%tavg_T,  tm_a%tavg_T,  na, nb)
    if(allocated(tm%tavg_TT)) call stats_window_combine(size(tm%tavg_TT), tm%tavg_TT, tm_a%tavg_TT, na, nb)
    return
  end subroutine combine_window_thermo
!==============================================================================
  subroutine combine_window_mhd(mh, mh_a, na, nb)
    use udf_type_mod
    implicit none
    type(t_mhd), intent(inout) :: mh
    type(t_mhd), intent(in)    :: mh_a
    integer, intent(in) :: na, nb

    if(allocated(mh%tavg_e))  call stats_window_combine(size(mh%tavg_e),  mh%tavg_e,  mh_a%tavg_e,  na, nb)
    if(allocated(mh%tavg_j))  call stats_window_combine(size(mh%tavg_j),  mh%tavg_j,  mh_a%tavg_j,  na, nb)
    if(allocated(mh%tavg_eu)) call stats_window_combine(size(mh%tavg_eu), mh%tavg_eu, mh_a%tavg_eu, na, nb)
    if(allocated(mh%tavg_ej)) call stats_window_combine(size(mh%tavg_ej), mh%tavg_ej, mh_a%tavg_ej, na, nb)
    if(allocated(mh%tavg_ju)) call stats_window_combine(size(mh%tavg_ju), mh%tavg_ju, mh_a%tavg_ju, na, nb)
    if(allocated(mh%tavg_jj)) call stats_window_combine(size(mh%tavg_jj), mh%tavg_jj, mh_a%tavg_jj, na, nb)
    return
  end subroutine combine_window_mhd
!==============================================================================
  ! Provenance line: kept on rank 0 for stat_window_provenance.dat and echoed.
  subroutine pp_note(line)
    implicit none
    character(*), intent(in) :: line

    if(nrank /= 0) return
    write(*, '(2X,A)') trim(line)
    if(pp_nreport < PP_REPORT_MAX) then
      pp_nreport = pp_nreport + 1
      pp_report(pp_nreport) = line
    end if
    return
  end subroutine pp_note
!==============================================================================
  ! Statistics manifest, read by key: the sample count, its origin and the
  ! field list. Absent keys are reported as absent, never defaulted.
  subroutine read_window_meta(meta_file, has_n, nsamples, has_istart, istart, fields)
    implicit none
    character(*), intent(in)  :: meta_file
    logical,      intent(out) :: has_n, has_istart
    integer,      intent(out) :: nsamples, istart
    character(*), intent(out) :: fields
    character(len=4200) :: line
    integer :: u, ioerr, ival, sp

    has_n = .false.
    has_istart = .false.
    nsamples = 0
    istart = 0
    fields = ''
    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    if(ioerr /= 0) call Print_error_msg('Statistical window: cannot open '//trim(meta_file))
    do
      read(u, '(A)', iostat=ioerr) line
      if(ioerr /= 0) exit
      line = adjustl(line)
      sp = index(line, ' ')
      if(sp <= 1) cycle
      select case(line(1:sp - 1))
      case('nsamples')
        read(line(sp + 1:), *, iostat=ioerr) ival
        if(ioerr == 0) then
          nsamples = ival
          has_n = .true.
        end if
      case('stat_istart')
        read(line(sp + 1:), *, iostat=ioerr) ival
        if(ioerr == 0) then
          istart = ival
          has_istart = .true.
        end if
      case('fields')
        fields = adjustl(line(sp + 1:))
      end select
    end do
    close(u)
    return
  end subroutine read_window_meta
!==============================================================================
  ! One window end of one group: the manifest must match this input and build
  ! (format, group, iteration, signature, field list), carry an explicit sample
  ! count with the input's stat_istart, and - bundled layout - the binary must
  ! hold exactly n_fields global dccc arrays of this mesh.
  subroutine check_window_snapshot(dm, igroup, signature, fields, iter, nsamples)
    use io_files_mod, only: dir_data, file_exists
    use io_tools_mod, only: generate_pathfile_name
    use iso_fortran_env, only: int64
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: igroup, iter
    character(*),   intent(in) :: signature, fields
    integer,        intent(out) :: nsamples
    character(256) :: meta_file, bin_file
    character(4096) :: fields_read
    character(16) :: grp
    logical :: has_n, has_istart
    integer :: istart, nfields, i
    integer(int64) :: file_bytes, expected_bytes

    grp = pp_group_name(igroup)
    call generate_pathfile_name(meta_file, dm%idom, trim(grp)//'_meta', dir_data, 'dat', iter)
    if(.not. file_exists(trim(meta_file))) call Print_error_msg('Statistical window: the '//trim(grp)// &
      ' snapshot at iteration '//trim(int2str(iter))//' is missing ('//trim(meta_file)//'). Both ends '// &
      'of the window need the cumulative statistics written at that iteration (a statistics checkpoint).')
    if(nrank == 0) call Print_debug_inline_msg('Checking '//trim(meta_file))
    call validate_stats_bundle_metadata(dm, trim(grp), iter, signature, fields)
    call read_window_meta(meta_file, has_n, nsamples, has_istart, istart, fields_read)
    if(.not. (has_n .and. has_istart)) call Print_error_msg('Statistical window: '//trim(meta_file)// &
      ' carries no explicit sample count (keys nsamples / stat_istart). It was written by a CHAPSim2 '// &
      'build older than the counted statistics (bb0c2f48, 2026-10-06). The post-processing mode does not '// &
      'infer sample counts from iteration numbers, so this snapshot cannot be used.')
    if(istart /= dm%stat_istart) call Print_error_msg('Statistical window: '//trim(meta_file)// &
      ' was accumulated from stat_istart = '//trim(int2str(istart))//', but the input sets stat_istart = '// &
      trim(int2str(dm%stat_istart))//'. Use the stat_istart of the run that wrote the statistics; '// &
      'snapshots with different origins cannot be combined.')
    if(nsamples < 0 .or. nsamples > iter - istart) call Print_error_msg('Statistical window: '// &
      trim(meta_file)//' reports nsamples = '//trim(int2str(nsamples))//', impossible for iteration '// &
      trim(int2str(iter))//' and stat_istart = '//trim(int2str(istart))//'.')
    if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
      call generate_pathfile_name(bin_file, dm%idom, trim(grp), dir_data, 'bin', iter)
      if(.not. file_exists(trim(bin_file))) call Print_error_msg('Statistical window: '// &
        trim(bin_file)//' is missing, although its manifest exists.')
      nfields = 0
      if(len_trim(fields_read) > 0) then
        nfields = 1
        do i = 1, len_trim(fields_read)
          if(fields_read(i:i) == ',') nfields = nfields + 1
        end do
      end if
      expected_bytes = int(nfields, int64) * int(dm%nc(1), int64) * int(dm%nc(2), int64) * &
                       int(dm%nc(3), int64) * int(storage_size(ONE) / 8, int64)
      inquire(file=trim(bin_file), size=file_bytes)
      if(file_bytes /= expected_bytes) call Print_error_msg('Statistical window: '//trim(bin_file)// &
        ' holds '//trim(int2str(int(file_bytes / 1024_int64)))//' KiB, but '//trim(int2str(nfields))// &
        ' fields on the input mesh need '//trim(int2str(int(expected_bytes / 1024_int64)))//' KiB. '// &
        'The file is truncated or belongs to a different mesh.')
    end if
    call pp_note('snapshot '//trim(grp)//' iter '//trim(int2str(iter))//': nsamples '// &
      trim(int2str(nsamples))//', stat_istart '//trim(int2str(istart))//', signature and field list match')
    return
  end subroutine check_window_snapshot
!==============================================================================
  ! Window checks of one group, metadata only: both ends, then the interval
  ! count. N_b - N_a = b - a is required - the group was sampled at every
  ! iteration of the window. That also rejects a reset that kept stat_istart
  ! (a fresh field initialisation part-way), which no other key records.
  ! Intermediate statistics checkpoints (multiples of ckpt_nfre) are checked
  ! for the same continuity where they exist.
  subroutine check_window_group(dm, igroup, signature, fields, iter_a, iter_b)
    use io_files_mod, only: dir_data, file_exists
    use io_tools_mod, only: generate_pathfile_name
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: igroup, iter_a, iter_b
    character(*),   intent(in) :: signature, fields
    character(256) :: meta_file
    character(4096) :: fields_read
    character(1024) :: checked
    logical :: has_n, has_istart
    integer :: na, nb, n, istart, it, nchecked

    na = 0
    if(iter_a > dm%stat_istart) then
      call check_window_snapshot(dm, igroup, signature, fields, iter_a, na)
    else
      call pp_note('snapshot '//trim(pp_group_name(igroup))//' iter '//trim(int2str(iter_a))// &
        ': a = stat_istart, the empty origin, N_a = 0 (no file needed)')
    end if
    call check_window_snapshot(dm, igroup, signature, fields, iter_b, nb)
    if(nb - na /= iter_b - iter_a) call Print_error_msg('Statistical window: the '// &
      trim(pp_group_name(igroup))//' counts give N_b - N_a = '//trim(int2str(nb))//' - '// &
      trim(int2str(na))//' = '//trim(int2str(nb - na))//' samples, but the window a = '// &
      trim(int2str(iter_a))//' ... b = '//trim(int2str(iter_b))//' spans '//trim(int2str(iter_b - iter_a))// &
      ' iterations. The group was not sampled at every iteration in between (frozen field, fresh field '// &
      'initialisation or a statistics reset that kept stat_istart). Continuous sampling is required.')

    nchecked = 0
    checked = ''
    if(dm%ckpt_nfre > 0) then
      it = (iter_a / dm%ckpt_nfre + 1) * dm%ckpt_nfre
      do while(it < iter_b)
        call generate_pathfile_name(meta_file, dm%idom, trim(pp_group_name(igroup))//'_meta', dir_data, 'dat', it)
        if(file_exists(trim(meta_file))) then
          call read_window_meta(meta_file, has_n, n, has_istart, istart, fields_read)
          if(.not. (has_n .and. has_istart)) call Print_error_msg('Statistical window: intermediate '// &
            'snapshot '//trim(meta_file)//' carries no sample count.')
          if(istart /= dm%stat_istart .or. n - na /= it - iter_a) call Print_error_msg( &
            'Statistical window: intermediate snapshot '//trim(meta_file)//' (nsamples '// &
            trim(int2str(n))//', stat_istart '//trim(int2str(istart))//') breaks the continuous '// &
            'accumulation between a and b.')
          nchecked = nchecked + 1
          if(len_trim(checked) < 1000) checked = trim(checked)//' '//trim(int2str(it))
        end if
        it = it + dm%ckpt_nfre
      end do
    end if
    call pp_note('window '//trim(pp_group_name(igroup))//': N_a '//trim(int2str(na))//', N_b '// &
      trim(int2str(nb))//', N_window '//trim(int2str(nb - na))//' = b - a; intermediate snapshots '// &
      'consistent at multiples of ckpt_nfre: '//trim(int2str(nchecked))//trim(checked))

    pp_group_on(igroup) = .true.
    pp_na(igroup) = na
    pp_nb(igroup) = nb
    pp_fields(igroup) = fields
    return
  end subroutine check_window_group
!==============================================================================
  ! Time and dt of a window end (checkpoint manifest) and the mesh recorded in
  ! the flow restart manifest of the same checkpoint, where those files were
  ! kept. A recorded mesh that differs from the input is fatal; a missing
  ! record is reported as a provenance limit.
  subroutine check_window_checkpoint(dm, iter, k)
    use io_files_mod, only: dir_data, file_exists
    use io_tools_mod, only: generate_pathfile_name
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: iter, k
    character(256) :: meta_file
    character(512) :: line
    integer :: u, ioerr, sp, icase, icoord, istret, mstret, nc(3)
    real(WP) :: rval, length(4), rstret
    logical :: periodic(3), has(6), is_same

    call generate_pathfile_name(meta_file, dm%idom, 'checkpoint_meta', dir_data, 'dat', iter)
    if(file_exists(trim(meta_file))) then
      open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
      do while(ioerr == 0)
        read(u, '(A)', iostat=ioerr) line
        if(ioerr /= 0) exit
        line = adjustl(line)
        sp = index(line, ' ')
        if(sp <= 1) cycle
        if(line(1:sp - 1) == 'time') then
          read(line(sp + 1:), *, iostat=ioerr) rval
          if(ioerr == 0) then
            pp_time(k) = rval
            pp_has_time(k) = .true.
          end if
        else if(line(1:sp - 1) == 'dt') then
          read(line(sp + 1:), *, iostat=ioerr) rval
          if(ioerr == 0) pp_dt(k) = rval
        end if
        ioerr = 0
      end do
      close(u)
    end if

    call generate_pathfile_name(meta_file, dm%idom, 'flow_restart_meta', dir_data, 'dat', iter)
    if(.not. file_exists(trim(meta_file))) then
      call pp_note('mesh at iter '//trim(int2str(iter))//': not verifiable (no flow_restart_meta kept); '// &
        'only the bundle size constrains the mesh')
      return
    end if
    has = .false.
    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    do while(ioerr == 0)
      read(u, '(A)', iostat=ioerr) line
      if(ioerr /= 0) exit
      line = adjustl(line)
      sp = index(line, ' ')
      if(sp <= 1) cycle
      select case(line(1:sp - 1))
      case('icase')
        read(line(sp + 1:), *, iostat=ioerr) icase
        has(1) = (ioerr == 0)
      case('coordinate')
        read(line(sp + 1:), *, iostat=ioerr) icoord
        has(2) = (ioerr == 0)
      case('nc')
        read(line(sp + 1:), *, iostat=ioerr) nc(1:3)
        has(3) = (ioerr == 0)
      case('length')
        read(line(sp + 1:), *, iostat=ioerr) length(1:4)
        has(4) = (ioerr == 0)
      case('stretch')
        read(line(sp + 1:), *, iostat=ioerr) istret, mstret, rstret
        has(5) = (ioerr == 0)
      case('periodic')
        read(line(sp + 1:), *, iostat=ioerr) periodic(1:3)
        has(6) = (ioerr == 0)
      end select
      ioerr = 0
    end do
    close(u)
    if(.not. all(has)) then
      call pp_note('mesh at iter '//trim(int2str(iter))//': flow_restart_meta incomplete, not verified')
      return
    end if
    is_same = (icase == dm%icase) .and. (icoord == dm%icoordinate) .and. all(nc == dm%nc(1:3)) .and. &
              (istret == dm%istret) .and. (mstret == dm%mstret) .and. &
              all(periodic .eqv. dm%is_periodic(1:3)) .and. &
              abs(rstret - dm%rstret) <= 1.0e-12_WP * max(ONE, abs(dm%rstret)) .and. &
              abs(length(1) - dm%lxx) <= 1.0e-12_WP * max(ONE, abs(dm%lxx)) .and. &
              abs(length(2) - dm%lyb) <= 1.0e-12_WP * max(ONE, abs(dm%lyb)) .and. &
              abs(length(3) - dm%lyt) <= 1.0e-12_WP * max(ONE, abs(dm%lyt)) .and. &
              abs(length(4) - dm%lzz) <= 1.0e-12_WP * max(ONE, abs(dm%lzz))
    if(.not. is_same) call Print_error_msg('Statistical window: the mesh recorded in '//trim(meta_file)// &
      ' (case, coordinates, nc, lengths, stretching, periodicity) differs from the input mesh. The '// &
      'snapshots do not belong to this case set-up.')
    call pp_note('mesh at iter '//trim(int2str(iter))//': matches the input (flow_restart_meta)')
    return
  end subroutine check_window_checkpoint
!==============================================================================
  ! Realisability of a reconstructed diagonal second moment,
  !   var = <ff> - <f>^2            (Reynolds)  or
  !   var = <rho ff> - <rho f>^2/<rho>  (Favre, opt_f0 = <rho>).
  ! The stored running means carry a rounding error of at most ~3 eps per
  ! sample folded in, i.e. |dA| <= 3 N eps |A|; the reconstruction amplifies
  ! it by kappa = (N_a + N_b)/(N_b - N_a), and var collects the error of <ff>
  ! and twice that of <f>^2 <= <ff>. So |d var| <= 9 kappa N_b eps <ff>, and
  ! tau = 16 kappa N_b eps |<ff>| bounds round-off. A negative var within tau
  ! is counted as round-off and left in the raw moments (downstream may clip
  ! it to zero); one beyond tau is genuinely unrealisable and stops the run.
  subroutine check_window_variance(name, ff, f, kappa, nb, opt_f0)
    use iso_fortran_env, only: int64
    use mpi_mod
    use typeconvert_mod
    implicit none
    character(*), intent(in) :: name
    real(WP), intent(in) :: ff(:, :, :), f(:, :, :)
    real(WP), intent(in) :: kappa
    integer,  intent(in) :: nb
    real(WP), intent(in), optional :: opt_f0(:, :, :)
    real(WP) :: tol_fac, var, rel, rel_min, rel_min_work
    integer(int64) :: cnt(3), cnt_work(3)   ! points, round-off negatives, invalid
    real(WP) :: cntr(3), cntr_work(3)
    integer :: i, j, k
    character(32) :: str

    tol_fac = 16.0_WP * kappa * real(nb, WP) * epsilon(ONE)
    cnt = 0_int64
    rel_min = huge(ONE)
    do k = 1, size(ff, 3)
      do j = 1, size(ff, 2)
        do i = 1, size(ff, 1)
          cnt(1) = cnt(1) + 1_int64
          if(present(opt_f0)) then
            if(opt_f0(i, j, k) <= ZERO) then
              cnt(3) = cnt(3) + 1_int64
              cycle
            end if
            var = ff(i, j, k) - f(i, j, k) * f(i, j, k) / opt_f0(i, j, k)
          else
            var = ff(i, j, k) - f(i, j, k) * f(i, j, k)
          end if
          rel = var / max(abs(ff(i, j, k)), tiny(ONE))
          rel_min = min(rel_min, rel)
          if(var < -tol_fac * abs(ff(i, j, k))) then
            cnt(3) = cnt(3) + 1_int64
          else if(var < ZERO) then
            cnt(2) = cnt(2) + 1_int64
          end if
        end do
      end do
    end do
    ! Reduce the counts as scalar REAL(WP), like every other MPI_ALLREDUCE buffer in this
    ! file (keeps the implicit MPI interface type- and rank-consistent). Exact: a double
    ! holds every integer up to 2**53, far above any grid point count.
    cntr = real(cnt, WP)
    do i = 1, 3
      call MPI_ALLREDUCE(cntr(i), cntr_work(i), 1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
    end do
    cnt_work = nint(cntr_work, int64)
    call MPI_ALLREDUCE(rel_min, rel_min_work, 1, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)
    write(str, '(ES12.4)') rel_min_work
    call pp_note('variance '//trim(name)//': points '//trim(int2str(int(cnt_work(1))))// &
      ', negative within round-off '//trim(int2str(int(cnt_work(2))))//', unrealisable '// &
      trim(int2str(int(cnt_work(3))))//', min var/<ff> '//trim(adjustl(str)))
    if(cnt_work(3) > 0_int64) call Print_error_msg('Statistical window: the reconstructed variance '// &
      trim(name)//' is negative beyond round-off at '//trim(int2str(int(cnt_work(3))))//' points '// &
      '(tolerance 16 kappa N_b eps <ff>). The two snapshots are not consistent with one accumulation.')
    return
  end subroutine check_window_variance
!==============================================================================
  !> Read the cumulative statistics at iterations iter_a and iter_b, check that
  !> they belong to one continuous accumulation, and leave the window averages
  !> over iter_a+1 ... iter_b in fl (tm, mh), with nstat_samples = N_b - N_a and
  !> iteration = iter_b. All metadata checks of all groups run before any large
  !> array is read. The snapshots are read through init_stats_flow / _thermo /
  !> _mhd, the unchanged restart readers, for both layouts.
  !> Whether the flow and thermal statistics of the window cover the same
  !> solver steps. Each field labels its files with its own counter, which
  !> advances only while that field is advanced, so identical labels do not
  !> imply identical steps; neither the statistics nor the time and checkpoint
  !> manifests record the offset. What is known comes from the input (the input
  !> of the run segment that wrote snapshot b) and the restart convention: a
  !> segment starts both counters at iteration_start s, and the flow / thermal
  !> field is first advanced at max(s+1, niterflowfirst) / max(s+1,
  !> niterthermofirst). Within the segment the thermal labels therefore trail
  !> the flow labels by the difference of the two, and are aligned again only
  !> at the next restart.
  !>   offset /= 0 and b > s : snapshot b of the two groups belongs to different
  !>                           steps - a positively identified mismatch: stop
  !>   offset == 0, a >= s   : the window lies in a segment in which both
  !>                           equations start together - consistent
  !>   otherwise             : earlier segments are not described by the input
  !>                           or the files - not established (recorded, not
  !>                           claimed as verified)
  subroutine check_window_joint_thermo(dm, iter_a, iter_b, flow_start, thermo_start)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: iter_a, iter_b, flow_start, thermo_start
    integer :: s, first_flow, first_thermo, offset

    ! Thermal statistics carrying 'label_clock run' are labelled by the run iteration like the flow's,
    ! whatever niterthermofirst was; only files of older builds need the input-based derivation below.
    if(trim(stats_meta_key(dm, 'thermo_stats', iter_b, 'label_clock')) == 'run') then
      if(trim(stats_meta_key(dm, 'thermo_stats', iter_a, 'label_clock')) == 'run') then
        pp_joint(1) = 'consistent: both thermal snapshots are labelled by the run iteration (label_clock run)'
      else
        pp_joint(1) = 'NOT ESTABLISHED: thermo_stats '//trim(int2str(iter_a))//' predates label_clock run; '// &
          'its flow/thermal label alignment is not recorded'
        if(nrank == 0) call Print_warning_msg('Statistical window: the thermal snapshot at '// &
          trim(int2str(iter_a))//' was written by an older build; flow/thermal alignment of that end is '// &
          'recorded as not established.')
      end if
      call pp_note('joint interval flow_stats/thermo_stats: '//trim(pp_joint(1)))
      return
    end if
    s = dm%iteration_start
    first_flow   = max(s + 1, flow_start)
    first_thermo = max(s + 1, thermo_start)
    offset = first_thermo - first_flow
    if(offset /= 0 .and. iter_b > s) call Print_error_msg('Statistical window: flow and thermal statistics '// &
      'cover different physical intervals. In the run segment from iteration '//trim(int2str(s))// &
      ' (input) the flow is first advanced at '//trim(int2str(first_flow))//' and the energy equation at '// &
      trim(int2str(first_thermo))//', so every thermo_stats label trails the flow_stats label of the same '// &
      'step by '//trim(int2str(offset))//' iterations (negative: leads): thermo_stats '//trim(int2str(iter_b))// &
      ' was written '//trim(int2str(offset))//' steps after flow_stats '//trim(int2str(iter_b))// &
      '. The two groups cannot be presented as one window. Each group is consistent on its own counter, '// &
      'but this mode reconstructs a domain jointly.')
    if(offset == 0 .and. iter_a >= s) then
      pp_joint(1) = 'consistent: the window lies in the run segment from iteration '//trim(int2str(s))// &
        ' in which flow and energy equation are first advanced together at '//trim(int2str(first_flow))
    else
      pp_joint(1) = 'NOT ESTABLISHED: the window reaches before iteration '//trim(int2str(s))// &
        ', the start of the run segment the input describes; flow/thermal label alignment in earlier '// &
        'segments is not recorded in the files and is not verified'
      if(nrank == 0) call Print_warning_msg('Statistical window: flow/thermal interval alignment before '// &
        'iteration '//trim(int2str(s))//' cannot be established from the input or the files; recorded '// &
        'as not established in the provenance record.')
    end if
    call pp_note('joint interval flow_stats/thermo_stats: '//trim(pp_joint(1)))
    return
  end subroutine check_window_joint_thermo
!==============================================================================
  !> Metadata checks of the window, every group, before anything is read or
  !> written: needs only the input (no decomposition), so the post-processing
  !> mode runs it before it creates its output directory, and an incompatible
  !> window leaves nothing behind.
  subroutine check_stats_window(dm, iter_a, iter_b, is_tm, is_mh, opt_flow_start, opt_thermo_start)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: iter_a, iter_b
    logical,        intent(in) :: is_tm, is_mh
    ! niterflowfirst / niterthermofirst of the input (required with is_tm)
    integer,        intent(in), optional :: opt_flow_start, opt_thermo_start
    character(32) :: str

    pp_nreport = 0
    pp_group_on = .false.
    pp_has_time = .false.
    pp_joint = ''
    if(is_tm .and. .not. (present(opt_flow_start) .and. present(opt_thermo_start))) &
      call Print_error_msg('Internal error: check_stats_window needs the flow and thermal start iterations.')
    if(nrank == 0) call Print_debug_start_msg('Statistical window: checking snapshots '// &
      trim(int2str(iter_a))//' and '//trim(int2str(iter_b)))
    if(dm%stat_level <= ISTATL0) call Print_error_msg('Statistical window: stat_level = 0, the run '// &
      'stores no statistics.')
    if(iter_a < dm%stat_istart) call Print_error_msg('Statistical window: a = '//trim(int2str(iter_a))// &
      ' lies before stat_istart = '//trim(int2str(dm%stat_istart))//', where the accumulation starts.')

    call check_window_group(dm, 1, flow_stats_bundle_signature(dm), flow_stats_bundle_fields(dm), iter_a, iter_b)
    ! both window ends must hold t_avg_fuuu with the same definition (key fuuu_definition)
    pp_fuuu_def = ''
    if(is_tm .and. dm%stat_level > ISTATL2) then
      pp_fuuu_def = stats_meta_key(dm, 'flow_stats', iter_b, 'fuuu_definition')
      if(trim(stats_meta_key(dm, 'flow_stats', iter_a, 'fuuu_definition')) /= trim(pp_fuuu_def)) &
        call Print_error_msg('Statistical window: t_avg_fuuu has different definitions at the two '// &
          'window ends (rho*u_i u_j u_k with the key fuuu_definition, rho*T*u_i u_j u_k without); they '// &
          'cannot be differenced.')
    end if
    if(is_tm) then
      call check_window_joint_thermo(dm, iter_a, iter_b, opt_flow_start, opt_thermo_start)
      call check_window_group(dm, 2, thermo_stats_bundle_signature(dm), thermo_stats_bundle_fields(dm), iter_a, iter_b)
    end if
    if(is_mh) then
      call check_window_group(dm, 3, mhd_stats_bundle_signature(dm), mhd_stats_bundle_fields(dm), iter_a, iter_b)
      ! The MHD field takes the flow's counter (mh%iteration = fl%iteration,
      ! eq_mhd.f90) and is sampled in the same steps as the flow.
      pp_joint(2) = 'consistent: the MHD statistics share the flow counter and sampling steps'
      call pp_note('joint interval flow_stats/mhd_stats: '//trim(pp_joint(2)))
    end if
    call check_window_checkpoint(dm, iter_a, 1)
    call check_window_checkpoint(dm, iter_b, 2)
    if(pp_has_time(1) .and. pp_has_time(2)) then
      write(str, '(ES16.8)') pp_time(2) - pp_time(1)
      call pp_note('physical window t_b - t_a = '//trim(adjustl(str)))
      if(abs(pp_dt(2) - pp_dt(1)) > 1.0e-12_WP * abs(pp_dt(2))) then
        call pp_note('WARNING: dt differs between the window ends; the iteration-weighted average '// &
          'is not the time average over the window')
        if(nrank == 0) call Print_warning_msg('dt differs between the two snapshots: the reconstructed '// &
          'statistics are iteration-weighted, not time-weighted.')
      end if
    end if
    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine check_stats_window
!==============================================================================
  !> Read the cumulative statistics at iterations iter_a and iter_b, already
  !> checked by check_stats_window, and leave the window averages over
  !> iter_a+1 ... iter_b in fl (tm, mh), with nstat_samples = N_b - N_a and
  !> iteration = iter_b. The snapshots are read through init_stats_flow /
  !> _thermo / _mhd, the unchanged restart readers, for both layouts.
  subroutine read_stats_window(dm, fl, iter_a, iter_b, tm, mh)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl
    integer,        intent(in)    :: iter_a, iter_b
    type(t_thermo), intent(inout), optional :: tm
    type(t_mhd),    intent(inout), optional :: mh
    type(t_domain) :: dm_rd
    character(32) :: str

    if(.not. pp_group_on(1)) call Print_error_msg('Internal error: read_stats_window before check_stats_window.')
    if(present(tm) .neqv. pp_group_on(2)) call Print_error_msg('Internal error: thermo group not checked.')
    if(present(mh) .neqv. pp_group_on(3)) call Print_error_msg('Internal error: MHD group not checked.')
    ! A read is a restart read on the continued clock, whatever this input says.
    dm_rd = dm
    dm_rd%restart_clock = RESTART_CLOCK_CONTINUE
    call read_window_flow()
    if(present(tm)) call read_window_thermo()
    if(present(mh)) call read_window_mhd()
    return

  contains
    ! One group at a time, so that snapshot A of a group is freed (local
    ! variable) before the next group is read.
    subroutine read_window_flow()
      type(t_flow) :: fl_a
      real(WP) :: kappa
      integer :: na, nb

      na = pp_na(1)
      nb = pp_nb(1)
      if(nrank == 0 .and. dm%stat_level > ISTATL1) call Print_note_msg('Statistical window: the Euu '// &
        'spectra are not checkpointed and are not reconstructed; the restart reader''s spectra warning '// &
        'below does not apply to this mode.')
      fl_a%inittype = INIT_RESTART
      fl_a%iterfrom = iter_a
      fl_a%nstat_samples = 0
      call init_stats_flow(fl_a, dm_rd)
      fl%inittype = INIT_RESTART
      fl%iterfrom = iter_b
      fl%nstat_samples = 0
      call init_stats_flow(fl, dm_rd)
      if(fl_a%nstat_samples /= na .or. fl%nstat_samples /= nb) call Print_error_msg( &
        'Statistical window: flow sample counts changed between the checks and the read.')
      call combine_window_flow(fl, fl_a, na, nb)
      kappa = real(na + nb, WP) / real(nb - na, WP)
      write(str, '(ES12.4)') kappa
      call pp_note('flow_stats: kappa = (N_a+N_b)/(N_b-N_a) = '//trim(adjustl(str)))
      if(kappa > 1.0e3_WP .and. nrank == 0) call Print_warning_msg('Statistical window: kappa = '// &
        trim(adjustl(str))//'; a short window late in a long accumulation loses digits to cancellation.')
      if(allocated(fl%tavg_uu)) then
        call check_window_variance('uu11', fl%tavg_uu(:, :, :, 1), fl%tavg_u(:, :, :, 1), kappa, nb)
        call check_window_variance('uu22', fl%tavg_uu(:, :, :, 4), fl%tavg_u(:, :, :, 2), kappa, nb)
        call check_window_variance('uu33', fl%tavg_uu(:, :, :, 6), fl%tavg_u(:, :, :, 3), kappa, nb)
      end if
      if(allocated(fl%tavg_vortvort)) then
        call check_window_variance('vortvort11', fl%tavg_vortvort(:, :, :, 1), fl%tavg_vort(:, :, :, 1), kappa, nb)
        call check_window_variance('vortvort22', fl%tavg_vortvort(:, :, :, 4), fl%tavg_vort(:, :, :, 2), kappa, nb)
        call check_window_variance('vortvort33', fl%tavg_vortvort(:, :, :, 6), fl%tavg_vort(:, :, :, 3), kappa, nb)
      end if
      if(allocated(fl%tavg_fuu)) then
        call check_window_variance('fuu11 (Favre)', fl%tavg_fuu(:, :, :, 1), fl%tavg_fu(:, :, :, 1), kappa, nb, &
                                   opt_f0=fl%tavg_f)
        call check_window_variance('fuu22 (Favre)', fl%tavg_fuu(:, :, :, 4), fl%tavg_fu(:, :, :, 2), kappa, nb, &
                                   opt_f0=fl%tavg_f)
        call check_window_variance('fuu33 (Favre)', fl%tavg_fuu(:, :, :, 6), fl%tavg_fu(:, :, :, 3), kappa, nb, &
                                   opt_f0=fl%tavg_f)
      end if
      if(allocated(fl%tavg_prfs2)) &
        call check_window_variance('prfs2', fl%tavg_prfs2, fl%tavg_prfs, kappa, nb)
      fl%nstat_samples = nb - na
      fl%iteration = iter_b
      if(pp_has_time(2)) fl%time = pp_time(2)
      return
    end subroutine read_window_flow

    subroutine read_window_thermo()
      type(t_thermo) :: tm_a
      real(WP) :: kappa
      integer :: na, nb

      na = pp_na(2)
      nb = pp_nb(2)
      tm_a%inittype = INIT_RESTART
      tm_a%iterfrom = iter_a
      tm_a%nstat_samples = 0
      call init_stats_thermo(tm_a, dm_rd)
      tm%inittype = INIT_RESTART
      tm%iterfrom = iter_b
      tm%nstat_samples = 0
      call init_stats_thermo(tm, dm_rd)
      if(tm_a%nstat_samples /= na .or. tm%nstat_samples /= nb) call Print_error_msg( &
        'Statistical window: thermo sample counts changed between the checks and the read.')
      call combine_window_thermo(tm, tm_a, na, nb)
      kappa = real(na + nb, WP) / real(nb - na, WP)
      write(str, '(ES12.4)') kappa
      call pp_note('thermo_stats: kappa = '//trim(adjustl(str)))
      if(allocated(tm%tavg_TT)) call check_window_variance('TT', tm%tavg_TT, tm%tavg_T, kappa, nb)
      tm%nstat_samples = nb - na
      tm%iteration = iter_b
      if(pp_has_time(2)) tm%time = pp_time(2)
      return
    end subroutine read_window_thermo

    subroutine read_window_mhd()
      type(t_mhd) :: mh_a
      type(t_flow) :: fl_restart   ! init_stats_mhd takes the restart decision from the flow
      real(WP) :: kappa
      integer :: na, nb

      na = pp_na(3)
      nb = pp_nb(3)
      fl_restart%inittype = INIT_RESTART
      mh_a%iterfrom = iter_a
      mh_a%nstat_samples = 0
      call init_stats_mhd(mh_a, fl_restart, dm_rd)
      mh%iterfrom = iter_b
      mh%nstat_samples = 0
      call init_stats_mhd(mh, fl_restart, dm_rd)
      if(mh_a%nstat_samples /= na .or. mh%nstat_samples /= nb) call Print_error_msg( &
        'Statistical window: MHD sample counts changed between the checks and the read.')
      call combine_window_mhd(mh, mh_a, na, nb)
      kappa = real(na + nb, WP) / real(nb - na, WP)
      write(str, '(ES12.4)') kappa
      call pp_note('mhd_stats: kappa = '//trim(adjustl(str)))
      if(allocated(mh%tavg_jj)) then
        call check_window_variance('jj11', mh%tavg_jj(:, :, :, 1), mh%tavg_j(:, :, :, 1), kappa, nb)
        call check_window_variance('jj22', mh%tavg_jj(:, :, :, 4), mh%tavg_j(:, :, :, 2), kappa, nb)
        call check_window_variance('jj33', mh%tavg_jj(:, :, :, 6), mh%tavg_j(:, :, :, 3), kappa, nb)
      end if
      mh%nstat_samples = nb - na
      mh%iteration = iter_b
      return
    end subroutine read_window_mhd
  end subroutine read_stats_window
!==============================================================================
  ! Mark a written manifest as derived window data. Appended after the keys
  ! every reader uses, so the file still loads as an ordinary statistics
  ! manifest (stat_istart = a, nsamples = N_b - N_a).
  subroutine append_window_meta_keys(dm, igroup, iter_a, iter_b, source_istart)
    use io_files_mod, only: dir_data, file_exists
    use io_tools_mod, only: generate_pathfile_name
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: igroup, iter_a, iter_b, source_istart
    character(256) :: meta_file
    integer :: u

    if(nrank /= 0) return
    call generate_pathfile_name(meta_file, dm%idom, trim(pp_group_name(igroup))//'_meta', dir_data, 'dat', iter_b)
    if(.not. file_exists(trim(meta_file))) call Print_error_msg('Statistical window: '//trim(meta_file)// &
      ' was not written.')
    open(newunit=u, file=trim(meta_file), status='old', action='write', position='append')
    write(u, '(A)') 'derived stat_window'
    write(u, '(A,1X,I0)') 'window_start', iter_a
    write(u, '(A,1X,I0)') 'window_end', iter_b
    write(u, '(A,1X,I0)') 'source_stat_istart', source_istart
    write(u, '(A,2(1X,I0))') 'source_nsamples', pp_na(igroup), pp_nb(igroup)
    write(u, '(A,1X,ES24.16E3)') 'kappa', real(pp_na(igroup) + pp_nb(igroup), WP) / &
                                           real(pp_nb(igroup) - pp_na(igroup), WP)
    close(u)
    return
  end subroutine append_window_meta_keys
!==============================================================================
  !> Write the window averages left by read_stats_window with the ordinary
  !> statistics writers, in the layout they were read in, as a run with
  !> stat_istart = iter_a would have written them at iter_b. The caller has
  !> already pointed dir_data and dir_visu* below out_root. Writes the
  !> provenance record out_root/stat_window_provenance.dat.
  subroutine write_stats_window(dm, fl, iter_a, iter_b, out_root, tm, mh)
    use typeconvert_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl
    integer,        intent(in)    :: iter_a, iter_b
    character(*),   intent(in)    :: out_root
    type(t_thermo), intent(inout), optional :: tm
    type(t_mhd),    intent(inout), optional :: mh
    type(t_domain) :: dm_out
    character(512) :: cwd
    character(16) :: layout
    integer :: u, ig, i, ilen, istat

    dm_out = dm
    dm_out%stat_istart = iter_a
    dm_out%restart_data_layout_write = dm%restart_data_layout_read
    ! out_root was created by this run and was empty: nothing to overwrite.
    dm_out%existing_output_policy = OUTPUT_POLICY_OVERWRITE
    if(pp_has_time(2)) dm_out%dt = pp_dt(2)

    call write_stats_flow(fl, dm_out)
    call append_window_meta_keys(dm_out, 1, iter_a, iter_b, dm%stat_istart)
    if(present(tm)) then
      call write_stats_thermo(tm, dm_out)
      call append_window_meta_keys(dm_out, 2, iter_a, iter_b, dm%stat_istart)
    end if
    if(present(mh)) then
      call write_stats_mhd(mh, dm_out)
      call append_window_meta_keys(dm_out, 3, iter_a, iter_b, dm%stat_istart)
    end if
    call write_visu_stats_flow(fl, dm_out)
    if(present(tm)) call write_visu_stats_thermo(tm, dm_out)
    if(present(mh)) call write_visu_stats_mhd(mh, dm_out)

    if(nrank /= 0) return
    layout = 'per_field'
    if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) layout = 'bundled'
    cwd = ''
    call get_environment_variable('PWD', cwd, ilen, istat)
    open(newunit=u, file=trim(out_root)//'/stat_window_provenance.dat', status='new', action='write')
    write(u, '(A)') '# CHAPSim2 statistical-window reconstruction - DERIVED DATA.'
    write(u, '(A)') '# The statistics below this directory are interval averages rebuilt from two'
    write(u, '(A)') '# cumulative statistics snapshots. They are NOT cumulative restart statistics'
    write(u, '(A)') '# of the run and must not be used to continue an accumulation.'
    write(u, '(A)') 'format CHAPSim_stat_window_v1'
    write(u, '(A,1X,I0)') 'domain', dm%idom
    write(u, '(A,1X,I0)') 'window_start', iter_a
    write(u, '(A,1X,I0)') 'window_end', iter_b
    write(u, '(A)') 'window_samples iterations window_start+1 ... window_end, one equally weighted sample each'
    write(u, '(A,1X,I0)') 'source_stat_istart', dm%stat_istart
    write(u, '(A,1X,A)') 'source_data_dir', trim(cwd)//'/1_data'
    write(u, '(A,1X,A)') 'layout', trim(layout)
    write(u, '(A,1X,I0)') 'stat_level', dm%stat_level
    write(u, '(A)') 'formula A_ab = (N_b A_b - N_a A_a)/(N_b - N_a), evaluated as A_b + N_a/(N_b-N_a) (A_b - A_a)'
    write(u, '(A)') 'weighting iteration-weighted (one sample per iteration); equal to the discrete time '// &
                    'average only for constant dt'
    if(pp_has_time(1)) write(u, '(A,2(1X,ES24.16E3))') 'time_dt_a', pp_time(1), pp_dt(1)
    if(pp_has_time(2)) write(u, '(A,2(1X,ES24.16E3))') 'time_dt_b', pp_time(2), pp_dt(2)
    do ig = 1, PP_NGROUP
      if(.not. pp_group_on(ig)) cycle
      write(u, '(A,1X,A,3(1X,I0),1X,ES16.8)') 'group', trim(pp_group_name(ig)), pp_na(ig), pp_nb(ig), &
        pp_nb(ig) - pp_na(ig), real(pp_na(ig) + pp_nb(ig), WP) / real(pp_nb(ig) - pp_na(ig), WP)
      write(u, '(A,1X,A,1X,A)') 'fields', trim(pp_group_name(ig)), trim(pp_fields(ig))
    end do
    write(u, '(A)') '# group columns: name N_a N_b N_window kappa=(N_a+N_b)/(N_b-N_a)'
    if(len_trim(pp_joint(1)) > 0) write(u, '(A,1X,A)') 'joint_interval flow_stats+thermo_stats', trim(pp_joint(1))
    if(len_trim(pp_joint(2)) > 0) write(u, '(A,1X,A)') 'joint_interval flow_stats+mhd_stats', trim(pp_joint(2))
    write(u, '(A)') '# checks and diagnostics:'
    do i = 1, pp_nreport
      write(u, '(A,1X,A)') 'check', trim(pp_report(i))
    end do
    write(u, '(A)') '# limitations:'
    write(u, '(A)') 'limitation only raw moments are written; rms, Reynolds stresses, k and Favre fluxes '// &
                    'are formed from them by the post-processing scripts'
    write(u, '(A)') 'limitation negative variances within round-off are left unclipped in the raw moments'
    write(u, '(A)') 'limitation Euu spectra are not checkpointed and are not reconstructed'
    write(u, '(A)') 'limitation the statistics files record no code version, Re/Pr/Ha/B, boundary '// &
                    'conditions or dt history; a physics change at a restart inside the window, or a '// &
                    'field definition changed between solver versions, cannot be detected'
    if(trim(pp_fuuu_def) == 'rho') then
      write(u, '(A)') 'note t_avg_fuuu holds the Favre triple moment <rho u_i u_j u_k>'
    else
      write(u, '(A)') 'limitation t_avg_fuuu holds <rho T u_i u_j u_k> (former definition, files without '// &
                      'fuuu_definition), not a Favre triple moment'
    end if
    write(u, '(A)') 'limitation thermal snapshots without label_clock run (builds before 2026-10-30) are '// &
                    'labelled by the thermal step count; their flow/thermal alignment is derived from the '// &
                    'input (the segment that wrote snapshot b) and the restart convention, and is recorded as '// &
                    'not established for earlier segments'
    write(u, '(A)') 'limitation a rerun on a reset clock into the same directory with existing_output_policy '// &
                    '= skip can leave snapshots of two runs with consistent labels, counts and times; this '// &
                    'cannot be detected from the statistics files'
    close(u)
    return
  end subroutine write_stats_window
end module
