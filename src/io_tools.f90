module io_tools_mod
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

contains
  
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
    character(120), intent(inout), optional :: opt_flname
    character(120), intent(out) :: flname_path
    integer, intent(in), optional     :: opt_timetag
    character(120) :: flname

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
    character(120), intent(out) :: flname
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