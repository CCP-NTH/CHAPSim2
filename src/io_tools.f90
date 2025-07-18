module io_tools_mod
  use print_msg_mod
  use parameters_constant_mod
  use decomp_2d_io
  use udf_type_mod
  use io_files_mod
  implicit none

  !----------------------------------------------------------------------------------------------------------
  ! io parameters
  !----------------------------------------------------------------------------------------------------------
  character(*), parameter :: io_restart = "restart-io"
  character(*), parameter :: io_in2outlet = "outlet2inlet-io"

  integer, parameter :: Ivisu_3D    = 0, &
                        Ivisu_2D_YZ = 1, & ! yz plane, should not change this value. 
                        Ivisu_2D_XZ = 2, & ! xz plane
                        Ivisu_2D_XY = 3, & ! xy plane
                        Ivisu_1D_Y  = 4    ! y profile

  public :: initialise_decomp_io
  public :: generate_file_name
  public :: generate_pathfile_name

  public  :: write_one_3d_array
  public  :: read_one_3d_array

contains

!==========================================================================================================
!==========================================================================================================
  subroutine read_one_3d_array(var, keyword, idom, iter, dtmp)
    implicit none 
    integer, intent(in) :: idom
    character(*), intent(in) :: keyword
    integer, intent(in) :: iter
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), dimension(:, :, :), intent(out) :: var( dtmp%xsz(1), &
                                                      dtmp%xsz(2), &
                                                      dtmp%xsz(3))
    character(64):: data_flname_path

    call generate_file_name(data_flname_path, idom, trim(keyword), 'bin', iter)
    if(.not.file_exists(data_flname_path)) call Print_error_msg("The file "//trim(data_flname_path)//" does not exist.")

    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(dir_data)//"/"//trim(data_flname_path))

    call decomp_2d_read_one(IPENCIL(1), var, trim(data_flname_path), &
          opt_dirname=trim(dir_data), &
          opt_decomp=dtmp, &
          opt_reduce_prec=.false.)

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine write_one_3d_array(var, keyword, idom, iter, dtmp, opt_flname)
    implicit none 
    real(WP), contiguous, intent(in) :: var( :, :, :)
    type(DECOMP_INFO), intent(in) :: dtmp
    character(*), intent(in) :: keyword
    integer, intent(in) :: idom
    integer, intent(in) :: iter
    character(64), intent(out), optional :: opt_flname

    character(64):: data_flname_path

    call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter)
    if(.not.file_exists(data_flname_path)) &
    call decomp_2d_write_one(IPENCIL(1), var, trim(data_flname_path), opt_decomp=dtmp)
    if(present(opt_flname)) opt_flname = data_flname_path

    return
  end subroutine
  
!==========================================================================================================
  subroutine initialise_decomp_io(dm)
    use udf_type_mod
    use decomp_2d_io
    implicit none 
    type(t_domain), intent(in) :: dm
    
    logical is_start1 ! is index starting from 1.
!----------------------------------------------------------------------------------------------------------
! if not #ifdef ADIOS2, do nothing below.
!----------------------------------------------------------------------------------------------------------
    call decomp_2d_io_init()
!----------------------------------------------------------------------------------------------------------
! re-define the grid mesh size, considering the nskip
! based on decomp_info of dppp (default one defined)
!---------------------------------------------------------------------------------------------------------- 
    is_start1 = .true.
    !call init_coarser_mesh_statV(dm%visu_nskip(1), dm%visu_nskip(2), dm%visu_nskip(3), is_start1)
    !call init_coarser_mesh_statS(dm%stat_nskip(1), dm%stat_nskip(2), dm%stat_nskip(3), is_start1)

  end subroutine 
!==========================================================================================================
  subroutine generate_pathfile_name(flname_path, dmtag, keyword, path, extension, opt_timetag, opt_flname)
    use typeconvert_mod
    implicit none 
    integer, intent(in)      :: dmtag
    character(*), intent(in) :: keyword
    character(*), intent(in) :: path
    character(*), intent(in) :: extension
    character(64), intent(inout), optional :: opt_flname
    character(64), intent(out) :: flname_path
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
!==========================================================================================================
  subroutine generate_file_name(flname, dmtag, keyword, extension, timetag)
    use typeconvert_mod
    implicit none 
    integer, intent(in)      :: dmtag
    
    character(*), intent(in) :: keyword
    character(*), intent(in) :: extension
    character(64), intent(out) :: flname
    integer, intent(in), optional      :: timetag

    if(present(timetag)) then
      flname = "domain"//trim(int2str(dmtag))//'_'//trim(keyword)//'_'//trim(int2str(timetag))//"."//trim(extension)
    else 
      flname = "domain"//trim(int2str(dmtag))//'_'//trim(keyword)//"."//trim(extension)
    end if

    
    return
  end subroutine
!==========================================================================================================
end module