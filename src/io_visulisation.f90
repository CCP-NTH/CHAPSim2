module io_visualisation_mod
  use io_tools_mod
  use parameters_constant_mod
  use print_msg_mod
  implicit none 

  character(len=*), parameter :: io_name = "solution-io"

  integer, parameter :: XDMF_HEADER = 1, &
                        XDMF_FOOTER = 2
  integer, parameter :: PLANE_AVERAGE = -1
  character(6), parameter :: SCALAR = "Scalar", &
                             VECTOR = "Vector", &
                             TENSOR = "Tensor"
  character(4), parameter :: CELL = "Cell", &
                             NODE = "Node"
  
  integer, parameter :: N_DIRECTION = 0, &
                        X_DIRECTION = 1, &
                        Y_DIRECTION = 2, &
                        Z_DIRECTION = 3
  integer :: nnd_visu(3)
  integer :: ncl_visu(3)

  !real(WP), allocatable :: xp(:), yp(:), zp(:)
  real(WP), allocatable :: rp(:, :, :), ta(:, :, :)
  real(WP), allocatable :: xp(:, :, :), yp(:, :, :), zp(:, :, :)
  real(WP), allocatable :: xp1(:), yp1(:), zp1(:)
  character(64):: grid_flname, grid_flname_1t2d, grid_flname_x, grid_flname_y, grid_flname_z
  !character(6)  :: svisudim

  public  :: write_visu_headerfooter
  public  :: write_visu_field
  public  :: visu_average_periodic_data
  private :: write_visu_profile
  private :: process_and_write_field
  private :: write_mesh_binary_cylindrical

  public  :: write_visu_ini
  public  :: write_visu_flow
  public  :: write_visu_thermo
  public  :: write_visu_mhd
  public  :: write_visu_any3darray
  
contains

!========================================================================================================
! Generic subroutine to process and write a field (velocity or thermal)
!========================================================================================================
  subroutine process_and_write_field(field, dm, field_name, filename, iteration, direction, opt_bc)
    use udf_type_mod
    use operations
    implicit none
    real(WP), contiguous, intent(in) :: field(:, :, :)
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: field_name, filename
    integer, intent(in) :: iteration
    integer, intent(in), optional :: opt_bc(:)
    integer, intent(in) :: direction

    ! Local variables
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: accc_xpencil
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: accc_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: accc_zpencil
    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: acpc_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: accp_ypencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: accp_zpencil

#ifdef DEBUG_STEPS
    if(nrank == 0) &
    call Print_debug_inline_msg("Writing the field [" // trim(field_name) // &
         "] to the file with a keyword [" // trim(filename)//"]")
#endif

    select case (direction)
      case (N_DIRECTION)
        ! Process scalar field (e.g., pressure)
        call write_visu_field(dm, field, dm%dccc, trim(field_name), trim(filename), SCALAR, CELL, iteration)
      case (X_DIRECTION)
        ! Process x-direction field (e.g., qx or gx)
        call Get_x_midp_P2C_3D(field, accc_xpencil, dm, dm%iAccuracy, opt_bc)
        call write_visu_field(dm, accc_xpencil, dm%dccc, trim(field_name), trim(filename), SCALAR, CELL, iteration)

      case (Y_DIRECTION)
        ! Process y-direction field (e.g., qy or gy)
        call transpose_x_to_y(field, acpc_ypencil, dm%dcpc)
        call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, opt_bc)
        call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
        call write_visu_field(dm, accc_xpencil, dm%dccc, trim(field_name), trim(filename), SCALAR, CELL, iteration)

      case (Z_DIRECTION)
        ! Process z-direction field (e.g., qz or gz)
        call transpose_x_to_y(field, accp_ypencil, dm%dccp)
        call transpose_y_to_z(accp_ypencil, accp_zpencil, dm%dccp)
        call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, opt_bc)
        call transpose_z_to_y(accc_zpencil, accc_ypencil, dm%dccc)
        call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
        call write_visu_field(dm, accc_xpencil, dm%dccc, trim(field_name), trim(filename), SCALAR, CELL, iteration)

      case default
        if (nrank == 0) call Print_debug_inline_msg("Error: Invalid direction in process_and_write_field.")
    end select
  end subroutine process_and_write_field


  ! subroutine write_mesh_hdf5(xp, yp, zp, filename)
  !   use precision_mod  
  !   use hdf5
  !   implicit none 
  !   real(WP), intent(in) :: xp(:,:,:), yp(:,:,:), zp(:,:,:)
  !   character(len=*), intent(in) :: filename

  !   ! --- HDF5 handles and variables ---
  !   integer(HID_T) :: file_id, dset_id, dspace_id, plist_id
  !   integer(HSIZE_T), dimension(4) :: ndims
  !   integer :: error, rank
  !   real(WP), allocatable :: coords(:,:,:,:)
  !   logical :: parallel_io_available = .false.

  !   ! --- Initialize HDF5 ---
  !   call h5open_f(error)
  !   if (error /= 0) error stop "HDF5 initialization failed"

  !   ! --- Create file access property list ---
  !   call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
  !   if (error /= 0) error stop "Failed to create property list"
    
  !   ! --- Try to set MPI-IO (only if compiled with parallel HDF5) ---
  !   parallel_io_available = .true.
  !   call h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL, error)
  !   if (error /= 0) then
  !       parallel_io_available = .false.
  !       call h5pclose_f(plist_id, error)
  !       call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
  !   end if

  !   ! --- Create HDF5 file ---
  !   call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, file_id, error, access_prp=plist_id)
  !   if (error /= 0) error stop "Failed to create HDF5 file: "//trim(filename)
  !   call h5pclose_f(plist_id, error)

  !   ! --- Dataset dimensions ---
  !   rank = 4
  !   ndims = [size(xp,1), size(xp,2), size(xp,3), 3]

  !   ! --- Create dataspace ---
  !   call h5screate_simple_f(rank, ndims, dspace_id, error)
  !   if (error /= 0) error stop "Failed to create dataspace"

  !   ! --- Create dataset ---
  !   ! Note: Changed dataset name from filename to "coordinates" to avoid confusion
  !   if (WP == kind(1.0)) then
  !       call h5dcreate_f(file_id, "coordinates", H5T_NATIVE_REAL, dspace_id, dset_id, error)
  !   else if (WP == kind(1.0d0)) then
  !       call h5dcreate_f(file_id, "coordinates", H5T_NATIVE_DOUBLE, dspace_id, dset_id, error)
  !   else
  !       error stop "Unsupported precision in write_mesh_hdf5"
  !   end if
  !   if (error /= 0) error stop "Failed to create dataset"

  !   ! --- Write data ---
  !   allocate(coords(ndims(1), ndims(2), ndims(3), ndims(4)), stat=error)
  !   if (error /= 0) error stop "Allocation failed"
    
  !   coords(:,:,:,1) = xp
  !   coords(:,:,:,2) = yp
  !   coords(:,:,:,3) = zp

  !   ! Use the correct precision for writing
  !   if (WP == kind(1.0)) then
  !       call h5dwrite_f(dset_id, H5T_NATIVE_REAL, coords, ndims, error)
  !   else
  !       call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, coords, ndims, error)
  !   end if
  !   if (error /= 0) error stop "Failed to write dataset"

  !   ! --- Cleanup ---
  !   deallocate(coords, stat=error)
  !   call h5dclose_f(dset_id, error)
  !   call h5sclose_f(dspace_id, error)
  !   call h5fclose_f(file_id, error)
  !   call h5close_f(error)

  ! end subroutine write_mesh_hdf5

  subroutine write_mesh_binary_cylindrical(xp, yp, zp, filename, opt_nx)
    use precision_mod  
    use iso_fortran_env, only: int32
    implicit none 
    real(WP), intent(in) :: xp(:,:,:), yp(:,:,:), zp(:,:,:)
    character(len=*), intent(in) :: filename
    integer, intent(in), optional:: opt_nx

    ! --- HDF5 handles and variables ---
    integer(int32) :: ndims(3)
    integer :: error, i, j, k, unit
    real(WP) :: coord_buffer(3)
    logical :: parallel_io_available = .false.

    if(nrank /= 0) return
    ! --- Dataset dimensions ---
    ndims = [size(xp,1), size(xp,2), size(xp,3)]
    if(present(opt_nx)) ndims(1) = opt_nx + 1

    ! Write to binary file
    open(newunit=unit, file=trim(filename), access='stream', form='unformatted', &
          status='replace', action='write', iostat=error, convert='BIG_ENDIAN')
    if (error /= 0) error stop "Failed to open binary file for writing"

    ! Write dimensions first
    write(unit) ndims(1), ndims(2), ndims(3)
    ! Write coordinates in order (x,y,z) for each point
    do k = 1, ndims(3)
      do j = 1, ndims(2)
        do i = 1, ndims(1)
          if(present(opt_nx)) then
            coord_buffer(1) = real(i, WP)/real(ndims(1), WP) * 10.0_WP
            coord_buffer(2) = yp(1,j,k)
            coord_buffer(3) = zp(1,j,k)
          else
             coord_buffer(1) = xp(i,j,k)
             coord_buffer(2) = yp(j,j,k)
             coord_buffer(3) = zp(k,j,k)
          end if
          write(unit) coord_buffer
        end do
      end do
    end do

    close(unit)

  end subroutine write_mesh_binary_cylindrical
!==========================================================================================================
! xszV means:
! x - xpencil, could also be y, z
! sz - size, could also be st, en
! V - visualisation
! xszV is the same as dppp%xsz/nskip, based on nodes, considering the skip nodes
!==========================================================================================================
  subroutine write_visu_ini(dm)
    use udf_type_mod
    use parameters_constant_mod, only: MAXP
    use decomp_2d, only: xszV, yszV, zszV
    use io_files_mod
    use math_mod
    use typeconvert_mod 
    use iso_fortran_env, only: int32
    implicit none 
    type(t_domain), intent(in) :: dm

    integer :: i, j ,k
    character(64):: keyword
    integer :: iogrid
    character(12) :: istr(3)
    integer :: nnd(3)

    if(nrank /= 0) return
    if(nrank == 0) call Print_debug_start_msg("Writing visu initial files ...")
! note:: skip_nodes is not considered here, the visualisation is based on the nodes.
!----------------------------------------------------------------------------------------------------------
! allocate
!----------------------------------------------------------------------------------------------------------
    !if(.not. allocated(nnd_visu)) allocate (nnd_visu(3, nxdomain))
    !if(.not. allocated(ncl_visu)) allocate (ncl_visu(3, nxdomain))
    nnd_visu = 0
    ncl_visu = 0
! !----------------------------------------------------------------------------------------------------------
! ! global size
! !----------------------------------------------------------------------------------------------------------
!     !svisudim = ''
!     !if(dm%visu_idim == Ivisu_3D) then
!       !svisudim = "3d"
!       nnd_visu(1) = xszV(1)
!       nnd_visu(2) = yszV(2)
!       nnd_visu(3) = zszV(3)
!       do i = 1, 3
!         if(dm%is_periodic(i)) then 
!           ncl_visu(i, dm%idom) = nnd_visu(i, dm%idom)
!           nnd_visu(i, dm%idom) = nnd_visu(i, dm%idom) + 1
!         else 
!           ncl_visu(i, dm%idom) = MAX(nnd_visu(i, dm%idom) - 1, 1)
!         end if
!       end do
!     ! !else if(dm%visu_idim == Ivisu_2D_YZ) then
!     !   svisudim = "2d_xa"
!     !   nnd_visu(1) = 1
!     !   nnd_visu(2) = yszV(2)
!     !   nnd_visu(3) = zszV(3)
!     !   do i = 1, 3
!     !     if(dm%is_periodic(i)) then 
!     !       ncl_visu(i, dm%idom) = nnd_visu(i, dm%idom)
!     !       nnd_visu(i, dm%idom) = nnd_visu(i, dm%idom) + 1
!     !     else 
!     !       ncl_visu(i, dm%idom) = MAX(nnd_visu(i, dm%idom) - 1, 1)
!     !     end if
!     !   end do
!     ! else if(dm%visu_idim == Ivisu_2D_XZ) then
!     !   svisudim = "2d_ya"
!     !   nnd_visu(1) = xszV(1)
!     !   nnd_visu(2) = 1
!     !   nnd_visu(3) = zszV(3)
!     !   do i = 1, 3
!     !     if(dm%is_periodic(i)) then 
!     !       ncl_visu(i, dm%idom) = nnd_visu(i, dm%idom)
!     !       nnd_visu(i, dm%idom) = nnd_visu(i, dm%idom) + 1
!     !     else 
!     !       ncl_visu(i, dm%idom) = MAX(nnd_visu(i, dm%idom) - 1, 1)
!     !     end if
!     !   end do
!     ! else if(dm%visu_idim == Ivisu_2D_XY) then
!     !   svisudim = "2d_za"
!     !   nnd_visu(1) = xszV(1)
!     !   nnd_visu(2) = yszV(2)
!     !   nnd_visu(3) = 1
!     !   do i = 1, 3
!     !     if(dm%is_periodic(i)) then 
!     !       ncl_visu(i, dm%idom) = nnd_visu(i, dm%idom)
!     !       nnd_visu(i, dm%idom) = nnd_visu(i, dm%idom) + 1
!     !     else 
!     !       ncl_visu(i, dm%idom) = MAX(nnd_visu(i, dm%idom) - 1, 1)
!     !     end if
!     !   end do
!     ! else if(dm%visu_idim == Ivisu_1D_Y) then
!     !   svisudim = "2d_xza"
!     !   nnd_visu(1) = 1
!     !   nnd_visu(2) = yszV(2)
!     !   nnd_visu(3) = 1
!     !   do i = 1, 3
!     !     if(dm%is_periodic(i)) then 
!     !       ncl_visu(i, dm%idom) = nnd_visu(i, dm%idom)
!     !       nnd_visu(i, dm%idom) = nnd_visu(i, dm%idom) + 1
!     !     else 
!     !       ncl_visu(i, dm%idom) = MAX(nnd_visu(i, dm%idom) - 1, 1)
!     !     end if
!     !   end do
!     ! else
!     !   svisudim = "3d"
!     !   nnd_visu(1) = xszV(1)
!     !   nnd_visu(2) = yszV(2)
!     !   nnd_visu(3) = zszV(3)
!     !   do i = 1, 3
!     !     if(dm%is_periodic(i)) then 
!     !       ncl_visu(i, dm%idom) = nnd_visu(i, dm%idom)
!     !       nnd_visu(i, dm%idom) = nnd_visu(i, dm%idom) + 1
!     !     else 
!     !       ncl_visu(i, dm%idom) = MAX(nnd_visu(i, dm%idom) - 1, 1)
!     !     end if
!     !   end do
!     ! end if
!----------------------------------------------------------------------------------------------------------
! calculated structured grids geometry - Cartesian Coordinates
!----------------------------------------------------------------------------------------------------------    
    nnd_visu(1:3) = dm%np_geo(1:3)
    ncl_visu(1:3) = dm%nc(1:3)
    nnd(1:3) = nnd_visu(1:3)
    if(nrank == 0) then
      if(dm%icoordinate == ICARTESIAN) then
        if(.not. allocated(xp)) allocate ( xp1(nnd_visu(1)))
        if(.not. allocated(yp)) allocate ( yp1(nnd_visu(2)))
        if(.not. allocated(zp)) allocate ( zp1(nnd_visu(3)))
        xp1 = MAXP
        yp1 = MAXP
        zp1 = MAXP

        do i = 1, nnd_visu(1)
          xp1(i) = real(i-1, WP) * dm%h(1)
        enddo
        do j = 1, nnd_visu(2)
          if(dm%is_stretching(2)) then 
            yp1(j) = dm%yp(j)
          else 
            yp1(j) = real(j-1, WP) * dm%h(2)
          end if
        end do
        do k = 1, nnd_visu(3)
          zp1(k) = real(k-1, WP) * dm%h(3)
        enddo
      end if
!----------------------------------------------------------------------------------------------------------
! calculated structured grids geometry - Cylindrical Coordinates
!---------------------------------------------------------------------------------------------------------- 
      if(dm%icoordinate == ICYLINDRICAL) then
        if(.not. allocated(xp)) allocate ( xp(nnd_visu(1), nnd_visu(2), nnd_visu(3)))
        if(.not. allocated(yp)) allocate ( yp(nnd_visu(1), nnd_visu(2), nnd_visu(3)))
        if(.not. allocated(zp)) allocate ( zp(nnd_visu(1), nnd_visu(2), nnd_visu(3)))
        if(.not. allocated(rp)) allocate ( rp(nnd_visu(1), nnd_visu(2), nnd_visu(3)))
        if(.not. allocated(ta)) allocate ( ta(nnd_visu(1), nnd_visu(2), nnd_visu(3)))
        rp = MAXP
        ta = MAXP
        xp = MAXP
        yp = MAXP
        zp = MAXP

        do i = 1, nnd_visu(1)
          xp(i, :, :) = real(i-1, WP) * dm%h(1)
        enddo
        do j = 1, nnd_visu(2)
          if(dm%is_stretching(2)) then 
            yp(:, j, :) = dm%yp(j)
          else 
            yp(:, j, :) = real(j-1, WP) * dm%h(2)
          end if
        end do
        do k = 1, nnd_visu(3)
          zp(:, :, k) = real(k-1, WP) * dm%h(3)
        enddo

        rp(:, :, :) = yp(:, :, :) 
        ta(:, :, :) = zp(:, :, :) 
        do k = 1, nnd_visu(3)
          do j = 1, nnd_visu(2)
            do i = 1, nnd_visu(1)
              zp(i, j, k) = rp(i, j, k) * dcos(ta(i, j, k))
              yp(i, j, k) = rp(i, j, k) * dsin(ta(i, j, k))
            end do
          end do
        end do
      end if
!----------------------------------------------------------------------------------------------------------
! write grids - Cartesian Coordinates, well-structured rectangular grid
!---------------------------------------------------------------------------------------------------------- 
      istr(1) = trim(int2str(nnd_visu(1)))
      istr(2) = trim(int2str(nnd_visu(2)))
      istr(3) = trim(int2str(nnd_visu(3)))

      if(dm%icoordinate == ICARTESIAN) then
      ! Write binary files for each coordinate direction in double precision
        keyword = "grid_x"
        call generate_pathfile_name(grid_flname_x, dm%idom, keyword, dir_data, 'bin')
        open(newunit=iogrid, file=trim(grid_flname_x), access='stream', form='unformatted', &
            status='replace', action='write')
        write(iogrid) int(size(xp1), kind=int32)  ! Write dimension as 4-byte integer
        write(iogrid) xp1      ! Write coordinates as 8-byte floats (double precision)
        close(iogrid)

        keyword = "grid_y"
        call generate_pathfile_name(grid_flname_y, dm%idom, keyword, dir_data, 'bin')
        open(newunit=iogrid, file=trim(grid_flname_y), access='stream', form='unformatted', &
            status='replace', action='write')
        write(iogrid) int(size(yp1), kind=int32)
        write(iogrid) yp1
        close(iogrid)

        keyword = "grid_z"
        call generate_pathfile_name(grid_flname_z, dm%idom, keyword, dir_data, 'bin')
        open(newunit=iogrid, file=trim(grid_flname_z), access='stream', form='unformatted', &
            status='replace', action='write')
        write(iogrid) int(size(zp1), kind=int32)
        write(iogrid) zp1
        close(iogrid)
      end if
!----------------------------------------------------------------------------------------------------------
! write grids - Cylindrical Coordinates, well-structured non-rectangular grid
!---------------------------------------------------------------------------------------------------------- 
      if(dm%icoordinate == ICYLINDRICAL) then
        if(dm%is_record_xoutlet) then
          keyword = "grids_2d_outlet"
          nnd(1) = dm%ndbfre + 1
          call generate_pathfile_name(grid_flname_1t2d, dm%idom, keyword, dir_data, 'bin')
          call write_mesh_binary_cylindrical(xp, yp, zp, trim(grid_flname_1t2d), opt_nx = dm%ndbfre)
          call write_visu_headerfooter(nnd, keyword, dm%idom, dm%icoordinate, XDMF_HEADER, 0)
          call write_visu_headerfooter(nnd, keyword, dm%idom, dm%icoordinate, XDMF_FOOTER, 0)
        end if
        keyword = "grids_3d"
        !call generate_pathfile_name(grid_flname, dm%idom, keyword, dir_visu, 'h5')
        !call write_mesh_hdf5(xp, yp, zp, trim(grid_flname))
        call generate_pathfile_name(grid_flname, dm%idom, keyword, dir_data, 'bin')
        call write_mesh_binary_cylindrical(xp, yp, zp, trim(grid_flname))
      end if

      call write_visu_headerfooter(nnd, 'grids_3d', dm%idom, dm%icoordinate, XDMF_HEADER, 0)
      call write_visu_headerfooter(nnd, 'grids_3d', dm%idom, dm%icoordinate, XDMF_FOOTER, 0)

    end if

    if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine

!==========================================================================================================
! ref: https://www.xdmf.org/index.php/XDMF_Model_and_Format
!==========================================================================================================
  subroutine write_visu_headerfooter(nnd, visuname, idom, icoor, iheadfoot, iter)
    use precision_mod
    use parameters_constant_mod, only: MAXP
    use udf_type_mod, only: t_domain
    use decomp_2d, only: xszV, yszV, zszV
    use decomp_2d_constants, only: mytype
    use decomp_2d_mpi
    use io_files_mod
    use typeconvert_mod
    use iso_fortran_env, only: int32
    implicit none 
    integer, intent(in)        :: iheadfoot
    integer, intent(in)        :: iter
    integer, intent(in)        :: nnd(3)
    integer, intent(in)        :: icoor
    integer, intent(in)        :: idom
    character(*), intent(in)   :: visuname

    character(64):: keyword
    character(64):: visu_flname
    character(12):: istr(3)
    character(len=20) :: byte_str
    integer :: ioxdmf
    integer :: nsz, i, j, k

    if(nrank /= 0) return
!----------------------------------------------------------------------------------------------------------
! visu file name
!----------------------------------------------------------------------------------------------------------
    keyword = trim(visuname)
    call generate_pathfile_name(visu_flname, idom, keyword, dir_visu, 'xdmf', iter)
    if(file_exists(trim(visu_flname))) then
      open(newunit = ioxdmf, file = trim(visu_flname), action = "write", position="append")
    else 
      open(newunit = ioxdmf, file = trim(visu_flname), status="new", action = "write")
    end if
    nsz = nnd(3) * nnd(2) * nnd(1)

    ! Calculate header size (3 integers)
    write(byte_str, '(I0)') storage_size(0_int32)/8 * 3
!----------------------------------------------------------------------------------------------------------
! xdmf head
!----------------------------------------------------------------------------------------------------------
    if(iheadfoot == XDMF_HEADER) then
      istr(1) = trim(int2str(nnd(1)))
      istr(2) = trim(int2str(nnd(2)))
      istr(3) = trim(int2str(nnd(3)))
!----------------------------------------------------------------------------------------------------------
! write header
! geometry is based on node coordinates
! to do: write mesh into mesh.bin 
!----------------------------------------------------------------------------------------------------------
      ! Write XDMF header
      write(ioxdmf, '(a)')'<?xml version="1.0" ?>'
      write(ioxdmf, '(a)')'<Xdmf Version="3.0">'
      write(ioxdmf, '(a)')' <Domain>'
      write(ioxdmf, '(a)')'   <Grid Name="'//trim(keyword)//'" GridType="Uniform">'
      
      if(icoor == ICARTESIAN) then
        ! Write topology
        !-- For Fortran loop order i,j,k
        !-- XDMF Dimensions are in REVERSE order: k,j,i
        write(ioxdmf, '(a)')'     <Topology TopologyType="3DRectMesh" Dimensions=" '&
                                //trim(istr(3))//' '//trim(istr(2))//' '//trim(istr(1))//'"/>'
        ! Write geometry
        write(ioxdmf, '(a)') '     <Geometry GeometryType="VXVYVZ">'
        write(ioxdmf, '(a)') '        <DataItem ItemType="Uniform"'
        write(ioxdmf, '(a)') '                 Dimensions="'//trim(istr(1))//'"'
        write(ioxdmf, '(a)') '                 NumberType="Float"'
        write(ioxdmf, '(a)') '                 Precision="8"'      ! 8-byte precision
        write(ioxdmf, '(a)') '                 Format="Binary"'
        write(ioxdmf, '(a)') '                 Seek="4">'         ! Skip 4-byte integer header
        write(ioxdmf, '(a)') '          ../'//trim(grid_flname_x)
        write(ioxdmf, '(a)') '        </DataItem>'
        write(ioxdmf, '(a)') '        <DataItem ItemType="Uniform"'
        write(ioxdmf, '(a)') '                 Dimensions="'//trim(istr(2))//'"'
        write(ioxdmf, '(a)') '                 NumberType="Float"'
        write(ioxdmf, '(a)') '                 Precision="8"'      ! 8-byte precision
        write(ioxdmf, '(a)') '                 Format="Binary"'
        write(ioxdmf, '(a)') '                 Seek="4">'         ! Skip 4-byte integer header
        write(ioxdmf, '(a)') '          ../'//trim(grid_flname_y)
        write(ioxdmf, '(a)') '        </DataItem>'
        write(ioxdmf, '(a)') '        <DataItem ItemType="Uniform"'
        write(ioxdmf, '(a)') '                 Dimensions="'//trim(istr(3))//'"'
        write(ioxdmf, '(a)') '                 NumberType="Float"'
        write(ioxdmf, '(a)') '                 Precision="8"'      ! 8-byte precision
        write(ioxdmf, '(a)') '                 Format="Binary"'
        write(ioxdmf, '(a)') '                 Seek="4">'         ! Skip 4-byte integer header
        write(ioxdmf, '(a)') '          ../'//trim(grid_flname_z)
        write(ioxdmf, '(a)')'        </DataItem>'
        write(ioxdmf, '(a)')'      </Geometry>'
      end if
      if(icoor == ICYLINDRICAL) then
        ! Write topology
        write(ioxdmf, '(a)')'     <Topology TopologyType="3DSMesh" Dimensions=" '&
                               //trim(istr(3))//' '//trim(istr(2))//' '//trim(istr(1))//'"/>'
        ! Write geometry
        write(ioxdmf, '(a)') '     <Geometry GeometryType="XYZ">'
        write(ioxdmf, '(a)') '        <DataItem ItemType="Uniform"'
        write(ioxdmf, '(a)') '                 Dimensions="'//trim(int2str(nsz))//' 3"'
        write(ioxdmf, '(a)') '                 NumberType="Float"'
        !write(ioxdmf, '(a)') '                 Precision="'//merge('4','8',WP==kind(1.0))//'"'
        write(ioxdmf, '(a)') '                 Precision="8"'
        write(ioxdmf, '(a)') '                 Format="Binary"'
        write(ioxdmf, '(a)') '                 Endian="Big"'
        write(ioxdmf, '(a)') '                 Seek="'//trim(byte_str)//'">'
        write(ioxdmf, '(a)') '           '//"../"//trim(grid_flname)
        write(ioxdmf, '(a)')'        </DataItem>'
        write(ioxdmf, '(a)')'      </Geometry>'
      end if  
      
    else if (iheadfoot == XDMF_FOOTER) then 
      write(ioxdmf, '(a)')'   </Grid>'
      write(ioxdmf, '(a)')' </Domain>'
      write(ioxdmf, '(a)')'</Xdmf>'
    else 
    end if
    close(ioxdmf)
    return
  end subroutine
!==========================================================================================================
! ref: https://www.xdmf.org/index.php/XDMF_Model_and_Format
!==========================================================================================================
  subroutine write_visu_field(dm, var, dtmp, varname, visuname, attributetype, centring, iter)
    use precision_mod
    use decomp_2d
    use decomp_2d_io
    use udf_type_mod, only: t_domain
    use io_files_mod
    use decomp_operation_mod
    use typeconvert_mod
    use io_tools_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), contiguous, intent(in) :: var(:, :, :)
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: visuname
    character(*), intent(in) :: attributetype
    character(*), intent(in) :: centring
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in), optional :: iter

    character(64):: data_flname
    character(64):: data_flname_path
    character(64):: visu_flname_path
    character(64):: keyword
    integer :: nsz(3), nsz0
    integer :: ioxdmf

    if((.not. is_same_decomp(dtmp, dm%dccc))) then
      if(nrank == 0) call Print_error_msg("Data is not stored at cell centre. varname = " // trim(varname))
    end if
!----------------------------------------------------------------------------------------------------------
! xmdf file name
!----------------------------------------------------------------------------------------------------------
    keyword = trim(visuname)
    call generate_pathfile_name(visu_flname_path, dm%idom, keyword, dir_visu, 'xdmf', iter)
!----------------------------------------------------------------------------------------------------------
! write data into binary file
!----------------------------------------------------------------------------------------------------------
    if(dm%visu_idim == Ivisu_3D) then
      call generate_pathfile_name(data_flname_path, dm%idom, trim(keyword), dir_data, 'bin', iter)
      if(.not.file_exists(data_flname_path)) &
      call write_one_3d_array(var, trim(varname), dm%idom, iter, dtmp, opt_flname = data_flname_path)
    if(nrank == 0) write(*,*) data_flname_path
    else if(dm%visu_idim == Ivisu_1D_Y) then
      !to add 1D profile
    else 
      !keyword = trim(varname)
      !call generate_file_name(data_flname, dm%idom, keyword, 'bin', iter)
      !call generate_pathfile_name(data_flname_path, dm%idom, keyword, dir_data, 'bin', iter)
      !call decomp_2d_write_plane(IPENCIL(1), var, dm%visu_idim, PLANE_AVERAGE, trim(dir_data), &
      !      trim(data_flname), io_name, opt_decomp=dtmp) ! to update, to check
    end if
!----------------------------------------------------------------------------------------------------------
! dataitem for xdmf file
!----------------------------------------------------------------------------------------------------------
    if (nrank == 0) then
      if(file_exists(trim(visu_flname_path))) then
        open(newunit = ioxdmf, file = trim(visu_flname_path), action = "write", status = "old", position = "append")
      else
        open(newunit = ioxdmf, file = trim(visu_flname_path), action = "write", status = "new")
      end if

      if(trim(centring) == TRIM(CELL)) then
        nsz(1:3) = ncl_visu(1:3)
      else if (trim(centring) == TRIM(NODE)) then
        nsz(1:3) = nnd_visu(1:3)
      else
      end if
      nsz0 = nsz(1) * nsz(2) * nsz(3)

      if(dm%icoordinate == ICARTESIAN) then
        keyword = trim(int2str(nsz(3)))//' '//trim(int2str(nsz(2)))//' '//trim(int2str(nsz(1)))
      end if
      if(dm%icoordinate == ICYLINDRICAL) then
        !keyword = trim(int2str(nsz0))//' 3'
        keyword = trim(int2str(nsz(3)))//' '//trim(int2str(nsz(2)))//' '//trim(int2str(nsz(1)))
      end if  

      write(ioxdmf, '(a)') '      <Attribute Name="'//trim(varname)// &
                          '" AttributeType="'//trim(attributetype)// &
                          '" Center="'//trim(centring)//'">'
      write(ioxdmf, '(a)') '        <DataItem ItemType="Uniform"'
      write(ioxdmf, '(a)') '                 NumberType="Float"'
      write(ioxdmf, '(a)') '                 Precision="8"'
      write(ioxdmf, '(a)') '                 Format="Binary"'
      write(ioxdmf, '(a)') '                 Dimensions="'//trim(keyword)//'">'
      write(ioxdmf, '(a)') '          ../'//trim(data_flname_path)
      write(ioxdmf, '(a)') '        </DataItem>'
      write(ioxdmf, '(a)') '      </Attribute>'
      close(ioxdmf)
    end if

    return
  end subroutine 
  !==========================================================================================================
! ref: https://www.xdmf.org/index.php/XDMF_Model_and_Format
!==========================================================================================================
  subroutine write_visu_profile(dm, var, dtmp, varname, visuname, attributetype, centring, idim, iter)
    use precision_mod
    use decomp_2d
    use decomp_2d_io
    use udf_type_mod, only: t_domain
    use io_files_mod
    use decomp_operation_mod
    use typeconvert_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: var(:)
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: visuname
    character(*), intent(in) :: attributetype
    character(*), intent(in) :: centring
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in) :: idim
    integer, intent(in), optional :: iter

    character(64):: data_flname
    character(64):: data_flname_path
    character(64):: visu_flname_path
    character(64):: keyword
    integer :: nsz(3)
    integer :: ioxdmf, iofl

    integer :: j

    if((.not. is_same_decomp(dtmp, dm%dccc))) then
      if(nrank == 0) call Print_error_msg("Data is not stored at cell centre. varname = " // trim(varname))
    end if
!----------------------------------------------------------------------------------------------------------
! write data 
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then
      keyword = trim(varname)
      call generate_pathfile_name(data_flname_path, dm%idom, keyword, dir_data, 'dat', iter)
      open(newunit = iofl, file = data_flname_path, action = "write", status="replace")
      if(idim /= 2) call Print_error_msg('Error in direction')
      do j = 1, dtmp%ysz(2)
        write(iofl, *) j, dm%yc(j), var(j) 
      end do
      close(iofl)
    end if

    return
  end subroutine 
!==========================================================================================================
! Subroutine to write flow visualization data
!==========================================================================================================
  subroutine write_visu_flow(fl, dm, suffix)
    use udf_type_mod
    use precision_mod
    use operations
    use parameters_constant_mod
    implicit none

    ! Input variables
    type(t_domain), intent(in) :: dm
    type(t_flow), intent(in) :: fl
    character(4), intent(in), optional :: suffix

    ! Local variables
    integer :: iteration
    character(64) :: visu_filename

    ! Initialize iteration and filename
    iteration = fl%iteration
    visu_filename = 'flow'
    if (present(suffix)) visu_filename = trim(visu_filename) // '_' // trim(suffix)

    ! Write XDMF header
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(visu_filename),dm%idom, dm%icoordinate, XDMF_HEADER, iteration)

    ! Write pressure field (cell-centered)
    call process_and_write_field(fl%pres, dm, "pr", trim(visu_filename), iteration, &
                                N_DIRECTION)
    call process_and_write_field(fl%pcor, dm, "phi", trim(visu_filename), iteration, &
                                N_DIRECTION)

    ! Process and write velocity components (qx, qy, qz)
    call process_and_write_field(fl%qx, dm, "qx_ccc", trim(visu_filename), iteration, &
                                X_DIRECTION, opt_bc=dm%ibcx_qx)
    call process_and_write_field(fl%qy, dm, "qy_ccc", trim(visu_filename), iteration, &
                                Y_DIRECTION, opt_bc=dm%ibcy_qy)
    call process_and_write_field(fl%qz, dm, "qz_ccc", trim(visu_filename), iteration, &
                                Z_DIRECTION, opt_bc=dm%ibcz_qz)

    ! Process and write thermal fields if enabled
    if (dm%is_thermo) then
      call process_and_write_field(fl%gx, dm, "gx_ccc", trim(visu_filename), iteration, &
                                  X_DIRECTION, opt_bc=dm%ibcx_qx)
      call process_and_write_field(fl%gy, dm, "gy_ccc", trim(visu_filename), iteration, &
                                  Y_DIRECTION, opt_bc=dm%ibcy_qy)
      call process_and_write_field(fl%gz, dm, "gz_ccc", trim(visu_filename), iteration, &
                                  Z_DIRECTION, opt_bc=dm%ibcz_qz)
    end if

    ! Write XDMF footer
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(visu_filename), dm%idom, dm%icoordinate, XDMF_FOOTER, iteration)

    ! Debug message
    if (nrank == 0) call Print_debug_inline_msg("Flow field visualization data written successfully.")

    return
  end subroutine

  !==========================================================================================================
  subroutine write_visu_thermo(tm, fl, dm, str)
    use udf_type_mod
    use precision_mod
    use operations
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm
    type(t_flow),   intent(in) :: fl
    character(4), intent(in), optional :: str

    integer :: iter 
    character(64) :: visuname

    iter = tm%iteration
    visuname = 'thermo'
    if(present(str)) visuname = trim(visuname)//'_'//trim(str)
!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(visuname), dm%idom, dm%icoordinate, XDMF_HEADER, iter)
!----------------------------------------------------------------------------------------------------------
! write data, temperature, to cell centre
!----------------------------------------------------------------------------------------------------------
    call write_visu_field(dm, tm%tTemp, dm%dccc, "Temp", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%dDens, dm%dccc, "Dens", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%mVisc, dm%dccc, "Visc", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, tm%kCond, dm%dccc, "Cond", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, tm%hEnth, dm%dccc, "Enth", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%drhodt,dm%dccc, "dddt", trim(visuname), SCALAR, CELL, iter)
!----------------------------------------------------------------------------------------------------------
! write xdmf footer
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(visuname), dm%idom, dm%icoordinate, XDMF_FOOTER, iter)

    if(nrank == 0) call Print_debug_inline_msg("Write out visualisation for thermal field.")
    
    return
  end subroutine
!==========================================================================================================
  subroutine write_visu_mhd(mh, fl, dm, suffix)
    use udf_type_mod
    use precision_mod
    use operations
    use parameters_constant_mod
    implicit none

    ! Input variables
    type(t_domain), intent(in) :: dm
    type(t_flow), intent(in) :: fl
    type(t_mhd), intent(in) :: mh
    character(4), intent(in), optional :: suffix

    ! Local variables
    integer :: iteration
    character(64) :: visu_filename

    ! Initialize iteration and filename
    iteration = fl%iteration
    visu_filename = 'mhd'
    if (present(suffix)) visu_filename = trim(visu_filename) // '_' // trim(suffix)

    ! Write XDMF header
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(visu_filename), dm%idom, dm%icoordinate, XDMF_HEADER, iteration)

    ! Write electric potential field (cell-centered)
    call process_and_write_field(mh%ep, dm, "potential", trim(visu_filename), iteration, &
                                N_DIRECTION)

    ! Process and write current density components (jx, jy, jz)
    call process_and_write_field(mh%jx, dm, "jx_current", trim(visu_filename), iteration, &
                                X_DIRECTION, opt_bc=mh%ibcx_jx)
    call process_and_write_field(mh%jy, dm, "jy_current", trim(visu_filename), iteration, &
                                Y_DIRECTION, opt_bc=mh%ibcy_jy)
    call process_and_write_field(mh%jz, dm, "jz_current", trim(visu_filename), iteration, &
                                Z_DIRECTION, opt_bc=mh%ibcz_jz)
    ! Process and write Lorentz components
    call process_and_write_field(fl%lrfx, dm, "fx_Lorentz", trim(visu_filename), iteration, &
                                X_DIRECTION, opt_bc=dm%ibcx_qx)
    call process_and_write_field(fl%lrfy, dm, "fy_Lorentz", trim(visu_filename), iteration, &
                                Y_DIRECTION, opt_bc=dm%ibcy_qy)
    call process_and_write_field(fl%lrfz, dm, "fz_Lorentz", trim(visu_filename), iteration, &
                                Z_DIRECTION, opt_bc=dm%ibcz_qz)
    ! Write XDMF footer
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(visu_filename), dm%idom, dm%icoordinate, XDMF_FOOTER, iteration)

    ! Debug message
    if (nrank == 0) call Print_debug_inline_msg("MHD field visualization data written successfully.")

    return
  end subroutine
  
  !==========================================================================================================
  subroutine write_visu_any3darray(var, varname, visuname, dtmp, dm, iter)
    use udf_type_mod
    use precision_mod
    use operations
    use decomp_operation_mod
    use io_files_mod
    use parameters_constant_mod
    implicit none 
    type(t_domain), intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dtmp
    character(*), intent(in) :: varname
    real(WP), dimension( dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3) ), intent(in) :: var
    integer, intent(in) :: iter 
    character(*), intent(in) :: visuname

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil

    character(64) :: keyword

!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    keyword = trim(visuname)//"_"//trim(varname)//'_visu'
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(keyword), dm%idom, dm%icoordinate, XDMF_HEADER, iter)
    !if(nrank==0) write(*,*) keyword, iter
!----------------------------------------------------------------------------------------------------------
! write data, temperature, to cell centre
!----------------------------------------------------------------------------------------------------------
    if (is_same_decomp(dtmp, dm%dccc)) then
      call process_and_write_field(var, dm, trim(varname), trim(keyword), iter, N_DIRECTION)  
    else if (is_same_decomp(dtmp, dm%dpcc)) then
      call process_and_write_field(var, dm, trim(varname), trim(keyword), iter, X_DIRECTION, opt_bc=dm%ibcx_qx)  
    else if (is_same_decomp(dtmp, dm%dcpc)) then
      call process_and_write_field(var, dm, trim(varname), trim(keyword), iter, Y_DIRECTION, opt_bc=dm%ibcy_qy)  
    else if (is_same_decomp(dtmp, dm%dccp)) then
      call process_and_write_field(var, dm, trim(varname), trim(keyword), iter, Z_DIRECTION, opt_bc=dm%ibcz_qz)  
    else
      call Print_error_msg ("Given decomp_into is not supported "//trim(varname))
    end if
!----------------------------------------------------------------------------------------------------------
! write xdmf footer
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) &
    call write_visu_headerfooter(nnd_visu(1:3), trim(keyword), dm%idom, dm%icoordinate, XDMF_FOOTER, iter)
    
    return
  end subroutine


!==========================================================================================================
!==========================================================================================================
  subroutine visu_average_periodic_data(data_in, str1, dtmp, dm, str2, iter)
    use udf_type_mod
    use parameters_constant_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(in)  :: data_in
    character(*), intent(in) :: str1
    character(*), intent(in) :: str2
    integer, intent(in) :: iter
    

    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)) :: a_xpencil
    real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: a_ypencil
    real(WP), dimension(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)) :: a_zpencil
    real(WP), dimension(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)) :: b_zpencil

    real(WP), dimension( dtmp%ysz(2)) :: var

    integer :: i, j, k
    real(WP) :: sum

    if(dm%is_periodic(1) .and. &
       dm%is_periodic(3) .and. &
       dm%is_periodic(2)) then

    ! do nothing here, but bulk value output

    else if(dm%is_periodic(1) .and. &
            dm%is_periodic(3) .and. &
      .not. dm%is_periodic(2)) then

      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          sum =  ZERO
          do i = 1, dtmp%xsz(1)
            sum = sum + data_in(i, j, k) 
          end do
          sum =  sum/real(dtmp%xsz(1), WP)
          a_xpencil(:, j, k) = sum
        end do
      end do

      call transpose_x_to_y(a_xpencil, a_ypencil, dtmp)
      call transpose_y_to_z(a_ypencil, a_zpencil, dtmp)
      do i = 1, dtmp%zsz(1)
        do j = 1, dtmp%zsz(2)
          sum =  ZERO
          do k = 1, dtmp%zsz(3)
            sum = sum + a_zpencil(i, j, k) 
          end do
          b_zpencil(i, j, :) =  sum/real(dtmp%zsz(3), WP)
        end do
      end do
      call transpose_z_to_y(b_zpencil, a_ypencil, dtmp)
      var = a_ypencil(1,:,1)
      call write_visu_profile(dm, var, dm%dccc, trim(str1), trim(str2), SCALAR, CELL, 2, iter)
  
    else if(dm%is_periodic(1) .and. &
      .not. dm%is_periodic(3) .and. &
      .not. dm%is_periodic(2)) then

      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          sum =  ZERO
          do i = 1, dtmp%xsz(1)
            sum = sum + data_in(i, j, k) 
          end do
          sum =  sum/real(dtmp%xsz(1), WP)
          a_xpencil(:, j, k) = sum
        end do
      end do

      call write_visu_field(dm, a_xpencil, dm%dccc, trim(str1), trim(str2), SCALAR, CELL, iter)

    else if( &
      .not. dm%is_periodic(1) .and. &
            dm%is_periodic(3) .and. &
      .not. dm%is_periodic(2)) then

      call transpose_x_to_y(data_in,   a_ypencil, dtmp)
      call transpose_y_to_z(a_ypencil, a_zpencil, dtmp)
      do j = 1, dtmp%zsz(2)
        do i = 1, dtmp%zsz(1)
          sum =  ZERO
          do k = 1, dtmp%zsz(3)
            sum = sum + a_zpencil(i, j, k) 
          end do
          sum =  sum /real(dtmp%zsz(3), WP)
          b_zpencil(i, j, :) = sum
        end do
      end do

      call transpose_z_to_y(b_zpencil, a_ypencil, dtmp)
      call transpose_y_to_x(a_ypencil, a_xpencil, dtmp)

      call write_visu_field(dm, a_xpencil, dm%dccc, trim(str1), trim(str2), SCALAR, CELL, iter)

    else
      ! do nothing here
      !data_out = data_in
    end if


    return
  end subroutine



end module
