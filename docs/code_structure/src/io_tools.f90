module io_tools_mod
  use decomp_2d_io
  use io_files_mod
  use parameters_constant_mod
  use print_msg_mod
  use udf_type_mod
  implicit none

  !------------------------------------------------------------------------------
  ! io parameters
  !------------------------------------------------------------------------------
  character(*), parameter :: io_restart = "restart-io"
  character(*), parameter :: io_in2outlet = "outlet2inlet-io"

  public :: initialise_decomp_io
  !public :: generate_file_name
  public :: generate_pathfile_name
  public :: visu_xdmf_relative_path

  public  :: write_one_3d_array
  public  :: read_one_3d_array
  public  :: prepare_output_file_set
  public  :: remove_output_file_if_overwrite

contains

!==============================================================================
!==============================================================================
  subroutine read_one_3d_array(var, keyword, idom, iter, dtmp)
    use iso_fortran_env, only: int64
    implicit none
    integer, intent(in) :: idom
    character(*), intent(in) :: keyword
    integer, intent(in) :: iter
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), dimension(:, :, :), intent(out) :: var( dtmp%xsz(1), &
                                                      dtmp%xsz(2), &
                                                      dtmp%xsz(3))
    character(64):: data_flname_path
    character(512) :: msg
    integer(int64) :: file_bytes, expected_bytes, elem_bytes

    call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter)
    if(.not.file_exists(data_flname_path)) &
    call Print_error_msg("The file "//trim(data_flname_path)//" does not exist.")

    inquire(file=trim(data_flname_path), size=file_bytes)
    elem_bytes = int(storage_size(var), int64) / 8_int64
    expected_bytes = int(dtmp%xsz(1), int64) * &
                     int(dtmp%ysz(2), int64) * &
                     int(dtmp%zsz(3), int64) * elem_bytes
    if(file_bytes /= expected_bytes) then
      write(msg, '(A,A,A,I0,A,I0,A,I0,A,I0,A,I0,A)') &
        'Read size mismatch for ', trim(data_flname_path), ': file has ', &
        file_bytes, ' bytes, expected ', expected_bytes, ' bytes for global shape (', &
        dtmp%xsz(1), ',', dtmp%ysz(2), ',', dtmp%zsz(3), &
        '). Regenerate the matching restart/outlet file.'
      call Print_error_msg(trim(msg))
    end if

    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(data_flname_path))

    call decomp_2d_read_one(IPENCIL(1), var, trim(data_flname_path), &
          opt_decomp=dtmp, &
          opt_reduce_prec=.false.)

    return
  end subroutine
!==============================================================================
!==============================================================================
  subroutine write_one_3d_array(var, field_name, idom, iter, dtmp, existing_output_policy, opt_reduce_prec, opt_path)
    use typeconvert_mod
    implicit none
    real(WP), contiguous, intent(in) :: var( :, :, :)
    type(DECOMP_INFO), intent(in) :: dtmp
    character(*), intent(in) :: field_name
    integer, intent(in) :: idom
    integer, intent(in) :: iter
    integer, intent(in) :: existing_output_policy
    logical, intent(in), optional :: opt_reduce_prec
    character(*), intent(in), optional :: opt_path

    character(256):: field_file
    character(256):: field_path
    logical :: ex
    character(len=:), allocatable :: bak_file
    logical :: do_write
    logical :: reduce_prec

    field_path = dir_data
    if(present(opt_path)) field_path = opt_path
    call generate_pathfile_name(field_file, idom, trim(field_name), trim(field_path), 'bin', iter)

    do_write = .true.
    if(existing_output_policy == OUTPUT_POLICY_SKIP) then
      if (file_exists(trim(field_file))) then
        if (nrank == 0) then
          call Print_warning_msg("File "//trim(field_file)// &
                                " already exists; skip writing "//trim(field_name)// &
                                " at iteration "//trim(int2str(iter)))
        end if
        do_write = .false.
      end if
    else if (existing_output_policy == OUTPUT_POLICY_RENAME_EXISTING) then
      if (file_exists(trim(field_file))) then
        call rename_existing_file(trim(field_file))
      end if
    else
      ! do nothing, just overwrite if file exists
    end if

    if (do_write) then
      if(nrank == 0) call Print_debug_mid_msg("Writing "//trim(field_file))
      reduce_prec = .false.
      if(present(opt_reduce_prec)) reduce_prec = opt_reduce_prec
      call decomp_2d_write_one(IPENCIL(1), var, trim(field_file), &
                               opt_decomp=dtmp, opt_reduce_prec=reduce_prec)
    end if
    !
    return
  end subroutine
!==============================================================================
  subroutine prepare_output_file_set(files, existing_output_policy, output_name, do_write)
    use mpi_mod
    implicit none
    character(*), intent(in) :: files(:)
    integer, intent(in) :: existing_output_policy
    character(*), intent(in) :: output_name
    logical, intent(out) :: do_write

    integer :: i
    logical :: any_exists

    any_exists = .false.
    if(nrank == 0) then
      do i = 1, size(files)
        any_exists = any_exists .or. file_exists(trim(files(i)))
      end do
    end if
    call mpi_bcast(any_exists, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierror)

    do_write = .true.
    select case(existing_output_policy)
    case(OUTPUT_POLICY_SKIP)
      if(any_exists) then
        if(nrank == 0) call Print_warning_msg("An output file for "//trim(output_name)// &
          " already exists; skip writing the complete output set.")
        do_write = .false.
      end if
    case(OUTPUT_POLICY_RENAME_EXISTING)
      if(any_exists) then
        do i = 1, size(files)
          call rename_existing_file(trim(files(i)))
        end do
        call mpi_barrier(MPI_COMM_WORLD, ierror)
      end if
    case default
      continue
    end select

    return
  end subroutine prepare_output_file_set
!==============================================================================
  subroutine remove_output_file_if_overwrite(file_w_path, existing_output_policy)
    use mpi_mod
    implicit none
    character(*), intent(in) :: file_w_path
    integer, intent(in) :: existing_output_policy

    integer :: u, ios
    logical :: ex

    if(existing_output_policy /= OUTPUT_POLICY_OVERWRITE) return

    if(nrank == 0) then
      inquire(file=trim(file_w_path), exist=ex)
      if(ex) then
        open(newunit=u, file=trim(file_w_path), status='old', action='readwrite', iostat=ios)
        if(ios == 0) close(u, status='delete')
      end if
    end if
    call mpi_barrier(MPI_COMM_WORLD, ierror)

    return
  end subroutine remove_output_file_if_overwrite
!==============================================================================
  subroutine rename_existing_file(file_w_path)
    implicit none
    character(*), intent(in) :: file_w_path
    character(256) :: bak_file
    logical :: ex
    !
    ex = file_exists(trim(file_w_path))
    if(nrank == 0 .and. ex) then
      bak_file = trim(file_w_path)//'.bak'
      if (file_exists(trim(bak_file))) then
        call execute_command_line('rm -f ' // trim(bak_file))
      end if
      call execute_command_line('mv ' // trim(file_w_path) // ' ' // trim(bak_file))
    end if
    return
  end subroutine rename_existing_file
  !==============================================================================


!==============================================================================
  subroutine initialise_decomp_io(dm)
    use decomp_2d_io
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm

!------------------------------------------------------------------------------
! if not #ifdef ADIOS2, do nothing below.
!------------------------------------------------------------------------------
    call decomp_2d_io_init()
!------------------------------------------------------------------------------
! re-define the grid mesh size, considering the nskip
! based on decomp_info of dppp (default one defined)
!------------------------------------------------------------------------------
    ! if(dm%visu_nskip(1) > 1 .or. dm%visu_nskip(2) > 1 .or. dm%visu_nskip(3) > 1) then
    !   call init_coarser_mesh_statV(dm%visu_nskip(1), dm%visu_nskip(2), dm%visu_nskip(3), from1=.true.)
    ! end if
    !call init_coarser_mesh_statS(dm%stat_nskip(1), dm%stat_nskip(2), dm%stat_nskip(3), is_start1)

  end subroutine
!==============================================================================
  subroutine generate_pathfile_name(flname_path, dmtag, keyword, path, extension, opt_timetag, opt_flname)
    use typeconvert_mod
    implicit none
    integer, intent(in)      :: dmtag
    character(*), intent(in) :: keyword
    character(*), intent(in) :: path
    character(*), intent(in) :: extension
    character(*), intent(inout), optional :: opt_flname
    character(*), intent(out) :: flname_path
    integer, intent(in), optional     :: opt_timetag
    character(64) :: flname

    if(present(opt_timetag)) then
      flname = "/domain"//trim(int2str(dmtag))//'_'//trim(keyword)//'_'//trim(int2str(opt_timetag))//"."//trim(extension)
    else
      flname = "/domain"//trim(int2str(dmtag))//'_'//trim(keyword)//"."//trim(extension)
    end if
    if(present(opt_flname)) then
      opt_flname = flname
    end if
    flname_path = trim(path)//trim(flname)

    return
  end subroutine
!==============================================================================
  function visu_xdmf_relative_path(pathname) result(relpath)
    implicit none
    character(*), intent(in) :: pathname
    character(len=256) :: relpath
    integer :: ndata, nvisu, nvisu_data, nvisu_mesh, nvisu_xdmf

    relpath = trim(pathname)
    ndata = len_trim(dir_data)
    nvisu = len_trim(dir_visu)
    nvisu_data = len_trim(dir_visu_data)
    nvisu_mesh = len_trim(dir_visu_mesh)
    nvisu_xdmf = len_trim(dir_visu_xdmf)

    if(len_trim(pathname) > nvisu_data) then
      if(pathname(1:nvisu_data) == trim(dir_visu_data)) then
        relpath = '../'//trim(pathname(nvisu + 2:))
        return
      end if
    end if
    if(len_trim(pathname) > nvisu_mesh) then
      if(pathname(1:nvisu_mesh) == trim(dir_visu_mesh)) then
        relpath = '../'//trim(pathname(nvisu + 2:))
        return
      end if
    end if
    if(len_trim(pathname) > nvisu_xdmf) then
      if(pathname(1:nvisu_xdmf) == trim(dir_visu_xdmf)) then
        relpath = trim(pathname(nvisu_xdmf + 2:))
        return
      end if
    end if
    if(len_trim(pathname) > ndata) then
      if(pathname(1:ndata) == trim(dir_data)) then
        relpath = '../../'//trim(pathname)
        return
      end if
    end if

    return
  end function visu_xdmf_relative_path
!==============================================================================
  ! subroutine generate_file_name(flname, dmtag, keyword, extension, timetag)
  !   use typeconvert_mod
  !   implicit none
  !   integer, intent(in)      :: dmtag

  !   character(*), intent(in) :: keyword
  !   character(*), intent(in) :: extension
  !   character(*), intent(out) :: flname
  !   integer, intent(in), optional      :: timetag

  !   if(present(timetag)) then
  !     flname = "domain"//trim(int2str(dmtag))//'_'//trim(keyword)//'_'//trim(int2str(timetag))//"."//trim(extension)
  !   else
  !     flname = "domain"//trim(int2str(dmtag))//'_'//trim(keyword)//"."//trim(extension)
  !   end if


  !   return
  ! end subroutine
!==============================================================================
end module
