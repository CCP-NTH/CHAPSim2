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
  integer, allocatable :: nnd_visu(:, :)
  integer, allocatable :: ncl_visu(:, :)

  !real(WP), allocatable :: xp(:), yp(:), zp(:)
  real(WP), allocatable :: rp(:, :, :), ta(:, :, :)
  real(WP), allocatable :: xp(:, :, :), yp(:, :, :), zp(:, :, :)
  real(WP), allocatable :: xp1(:), yp1(:), zp1(:)
  character(120):: grid_flname
  !character(6)  :: svisudim

  private :: write_visu_headerfooter
  private :: write_visu_field
  private :: visu_average_periodic_data
  private :: write_visu_profile
  private :: process_and_write_field

  public  :: write_visu_ini
  public  :: write_visu_flow
  public  :: write_visu_thermo
  public  :: write_visu_mhd
  public  :: write_visu_any3darray

  public  :: write_visu_stats_flow
  public  :: write_visu_stats_thermo
  
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
    call Print_debug_mid_msg("Writing the field [" // trim(field_name) // &
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
        if (nrank == 0) call Print_debug_mid_msg("Error: Invalid direction in process_and_write_field.")
    end select
  end subroutine process_and_write_field
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
    implicit none 
    type(t_domain), intent(in) :: dm

    integer :: i, j ,k
    character(120):: keyword
    integer :: iogrid
    character(12) :: istr(3)

    if(nrank == 0) call Print_debug_start_msg("Writing visu initial files ...")
! note:: skip_nodes is not considered here, the visualisation is based on the nodes.
!----------------------------------------------------------------------------------------------------------
! allocate
!----------------------------------------------------------------------------------------------------------
    if(.not. allocated(nnd_visu)) allocate (nnd_visu(3, nxdomain))
    if(.not. allocated(ncl_visu)) allocate (ncl_visu(3, nxdomain))
    nnd_visu = 0
    ncl_visu = 0
! !----------------------------------------------------------------------------------------------------------
! ! global size
! !----------------------------------------------------------------------------------------------------------
!     !svisudim = ''
!     !if(dm%visu_idim == Ivisu_3D) then
!       !svisudim = "3d"
!       nnd_visu(1, dm%idom) = xszV(1)
!       nnd_visu(2, dm%idom) = yszV(2)
!       nnd_visu(3, dm%idom) = zszV(3)
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
!     !   nnd_visu(1, dm%idom) = 1
!     !   nnd_visu(2, dm%idom) = yszV(2)
!     !   nnd_visu(3, dm%idom) = zszV(3)
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
!     !   nnd_visu(1, dm%idom) = xszV(1)
!     !   nnd_visu(2, dm%idom) = 1
!     !   nnd_visu(3, dm%idom) = zszV(3)
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
!     !   nnd_visu(1, dm%idom) = xszV(1)
!     !   nnd_visu(2, dm%idom) = yszV(2)
!     !   nnd_visu(3, dm%idom) = 1
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
!     !   nnd_visu(1, dm%idom) = 1
!     !   nnd_visu(2, dm%idom) = yszV(2)
!     !   nnd_visu(3, dm%idom) = 1
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
!     !   nnd_visu(1, dm%idom) = xszV(1)
!     !   nnd_visu(2, dm%idom) = yszV(2)
!     !   nnd_visu(3, dm%idom) = zszV(3)
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
    nnd_visu(1:3, dm%idom) = dm%np_geo(1:3)
    ncl_visu(1:3, dm%idom) = dm%nc(1:3)
    if(nrank == 0) then
      if(dm%icoordinate == ICARTESIAN) then
        if(.not. allocated(xp)) allocate ( xp1(nnd_visu(1, dm%idom)))
        if(.not. allocated(yp)) allocate ( yp1(nnd_visu(2, dm%idom)))
        if(.not. allocated(zp)) allocate ( zp1(nnd_visu(3, dm%idom)))
        xp1 = MAXP
        yp1 = MAXP
        zp1 = MAXP

        do i = 1, nnd_visu(1, dm%idom)
          xp1(i) = real(i-1, WP) * dm%h(1)
        enddo
        do j = 1, nnd_visu(2, dm%idom)
          if(dm%is_stretching(2)) then 
            yp1(j) = dm%yp(j)
          else 
            yp1(j) = real(j-1, WP) * dm%h(2)
          end if
        end do
        do k = 1, nnd_visu(3, dm%idom)
          zp1(k) = real(k-1, WP) * dm%h(3)
        enddo
      end if
!----------------------------------------------------------------------------------------------------------
! calculated structured grids geometry - Cylindrical Coordinates
!---------------------------------------------------------------------------------------------------------- 
      if(dm%icoordinate == ICYLINDRICAL) then
        if(.not. allocated(xp)) allocate ( xp(nnd_visu(1, dm%idom), nnd_visu(2, dm%idom), nnd_visu(3, dm%idom)))
        if(.not. allocated(yp)) allocate ( yp(nnd_visu(1, dm%idom), nnd_visu(2, dm%idom), nnd_visu(3, dm%idom)))
        if(.not. allocated(zp)) allocate ( zp(nnd_visu(1, dm%idom), nnd_visu(2, dm%idom), nnd_visu(3, dm%idom)))
        if(.not. allocated(rp)) allocate ( rp(nnd_visu(1, dm%idom), nnd_visu(2, dm%idom), nnd_visu(3, dm%idom)))
        if(.not. allocated(ta)) allocate ( ta(nnd_visu(1, dm%idom), nnd_visu(2, dm%idom), nnd_visu(3, dm%idom)))
        rp = MAXP
        ta = MAXP
        xp = MAXP
        yp = MAXP
        zp = MAXP

        do i = 1, nnd_visu(1, dm%idom)
          xp(i, :, :) = real(i-1, WP) * dm%h(1)
        enddo
        do j = 1, nnd_visu(2, dm%idom)
          if(dm%is_stretching(2)) then 
            yp(:, j, :) = dm%yp(j)
          else 
            yp(:, j, :) = real(j-1, WP) * dm%h(2)
          end if
        end do
        do k = 1, nnd_visu(3, dm%idom)
          zp(:, :, k) = real(k-1, WP) * dm%h(3)
        enddo

        rp(:, :, :) = yp(:, :, :) 
        ta(:, :, :) = zp(:, :, :) 
        do k = 1, nnd_visu(3, dm%idom)
          do j = 1, nnd_visu(2, dm%idom)
            do i = 1, nnd_visu(1, dm%idom)
              zp(i, j, k) = rp(i, j, k) * dcos(ta(i, j, k))
              yp(i, j, k) = rp(i, j, k) * dsin(ta(i, j, k))
            end do
          end do
        end do
      end if
!----------------------------------------------------------------------------------------------------------
! write grids - Cartesian Coordinates, well-structured rectangular grid
!---------------------------------------------------------------------------------------------------------- 
      istr(1) = trim(int2str(nnd_visu(1, dm%idom)))
      istr(2) = trim(int2str(nnd_visu(2, dm%idom)))
      istr(3) = trim(int2str(nnd_visu(3, dm%idom)))

      if(dm%icoordinate == ICARTESIAN) then
        keyword = "grid_x"
        call generate_pathfile_name(grid_flname, dm%idom, keyword, dir_visu, 'dat')
        open(newunit = iogrid, file = trim(grid_flname), action = "write", status="replace")
        write(iogrid, *) xp1
        close(iogrid)

        keyword = "grid_y"
        call generate_pathfile_name(grid_flname, dm%idom, keyword, dir_visu, 'dat')
        open(newunit = iogrid, file = trim(grid_flname), action = "write", status="replace")
        write(iogrid, *) yp1
        close(iogrid)

        keyword = "grid_z"
        call generate_pathfile_name(grid_flname, dm%idom, keyword, dir_visu, 'dat')
        open(newunit = iogrid, file = trim(grid_flname), action = "write", status="replace")
        write(iogrid, *) zp1
        close(iogrid)
      end if
!----------------------------------------------------------------------------------------------------------
! write grids - Cylindrical Coordinates, well-structured non-rectangular grid
!---------------------------------------------------------------------------------------------------------- 
      if(dm%icoordinate == ICYLINDRICAL) then
        keyword = "grids"
        call generate_pathfile_name(grid_flname, dm%idom, keyword, dir_data, 'bin')
        open(newunit = iogrid, file = trim(grid_flname), access="stream", form="unformatted", status="replace")
        ! Write topology
        !write(iogrid, *) "Topology: 3D Grid"
        !write(iogrid, *) "Dimensions: ", trim(istr(3)), trim(istr(2)), trim(istr(1))
        do k = 1, nnd_visu(3, dm%idom)
          do j = 1, nnd_visu(2, dm%idom)
            do i = 1, nnd_visu(1, dm%idom)
              write(iogrid) xp(i, j, k), yp(i, j, k), zp(i, j, k)
            end do
          end do
        end do
        close(iogrid)
      end if
    ! ! Write XDMF header 
    ! call write_visu_headerfooter(dm, 'grid', XDMF_HEADER, 0)
    ! ! Write XDMF footer
    ! call write_visu_headerfooter(dm, 'grid', XDMF_FOOTER, 0)

    end if

    if(nrank == 0) call Print_debug_end_msg

    return
  end subroutine

!==========================================================================================================
! ref: https://www.xdmf.org/index.php/XDMF_Model_and_Format
!==========================================================================================================
  subroutine write_visu_headerfooter(dm, visuname, iheadfoot, iter)
    use precision_mod
    use parameters_constant_mod, only: MAXP
    use udf_type_mod, only: t_domain
    use decomp_2d, only: xszV, yszV, zszV
    use decomp_2d_constants, only: mytype
    use decomp_2d_mpi
    use io_files_mod
    use typeconvert_mod
    implicit none 
    integer, intent(in)        :: iheadfoot
    integer, intent(in)        :: iter
    type(t_domain), intent(in) :: dm
    character(*), intent(in)   :: visuname

    character(120):: keyword
    character(120):: visu_flname
    character(12):: istr(3)

    integer :: ioxdmf
    
    integer :: nsz, i, j, k
    if(nrank /= 0) return
!----------------------------------------------------------------------------------------------------------
! visu file name
!----------------------------------------------------------------------------------------------------------
    keyword = trim(visuname)
    call generate_pathfile_name(visu_flname, dm%idom, keyword, dir_visu, 'xdmf', iter)
    open(newunit = ioxdmf, file = trim(visu_flname), action = "write", position="append")
    nsz = nnd_visu(3, dm%idom) * nnd_visu(2, dm%idom) * nnd_visu(1, dm%idom)
!----------------------------------------------------------------------------------------------------------
! xdmf head
!----------------------------------------------------------------------------------------------------------
    if(iheadfoot == XDMF_HEADER) then
      istr(1) = trim(int2str(nnd_visu(1, dm%idom)))
      istr(2) = trim(int2str(nnd_visu(2, dm%idom)))
      istr(3) = trim(int2str(nnd_visu(3, dm%idom)))
!----------------------------------------------------------------------------------------------------------
! write header
! geometry is based on node coordinates
! to do: write mesh into mesh.bin 
!----------------------------------------------------------------------------------------------------------
      ! Write XDMF header
      write(ioxdmf, '(A)')'<?xml version="1.0" ?>'
      write(ioxdmf, '(A)')'<Xdmf Version="3.0">'
      write(ioxdmf, *)' <Domain>'
      write(ioxdmf, *)'   <Grid Name="'//trim(keyword)//'" GridType="Uniform">'
      
      if(dm%icoordinate == ICARTESIAN) then
        ! Write topology
        write(ioxdmf, *)'     <Topology TopologyType="3DRectMesh" Dimensions=" '&
                                //trim(istr(3))//' '//trim(istr(2))//' '//trim(istr(1))//'">'
        write(ioxdmf, *)'     </Topology>'
        ! Write geometry
        write(ioxdmf, *)'     <Geometry GeometryType="VXVYVZ">'
        write(ioxdmf, *)'        <DataItem Format="XML" NumberType="Float" Precision="8" Dimensions="'//trim(istr(1))//'">'
        write(ioxdmf, *)'         ', xp1(1:nnd_visu(1, dm%idom))
        write(ioxdmf, *)'        </DataItem>'
        write(ioxdmf, *)'        <DataItem Format="XML" NumberType="Float" Precision="8" Dimensions="'//trim(istr(2))//'">'
        write(ioxdmf, *)'         ', yp1(1:nnd_visu(2, dm%idom))
        write(ioxdmf, *)'        </DataItem>'
        write(ioxdmf, *)'        <DataItem Format="XML" NumberType="Float" Precision="8" Dimensions="'//trim(istr(3))//'">'
        write(ioxdmf, *)'         ', zp1(1:nnd_visu(3, dm%idom))
      end if
      if(dm%icoordinate == ICYLINDRICAL) then
        ! Write topology
        write(ioxdmf, *)'     <Topology TopologyType="3DSMesh" Dimensions=" '&
                               //trim(istr(3))//' '//trim(istr(2))//' '//trim(istr(1))//'">'
        write(ioxdmf, *)'     </Topology>'
        ! Write geometry
        write(ioxdmf, *)'     <Geometry GeometryType="XYZ">'
        write(ioxdmf, *)'        <DataItem Dimensions="' // trim(int2str(nsz)) // &
                              ' 3" NumberType="Float" Precision="8" Format="Binary">'
        write(ioxdmf, *)'          ', trim(grid_flname)
        ! do k = 1, nnd_visu(3, dm%idom)
        !   do j = 1, nnd_visu(2, dm%idom)
        !     do i = 1, nnd_visu(1, dm%idom)
        !       write(ioxdmf, *) xp(i, j, k), yp(i, j, k), zp(i, j, k)
        !     end do
        !   end do
        ! end do
      end if  
      write(ioxdmf, *)'        </DataItem>'
      write(ioxdmf, *)'      </Geometry>'
    else if (iheadfoot == XDMF_FOOTER) then 
      write(ioxdmf, *)'   </Grid>'
      write(ioxdmf, *)' </Domain>'
      write(ioxdmf, '(A)')'</Xdmf>'
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
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), contiguous, intent(in) :: var(:, :, :)
    character(len=*), intent(in) :: varname
    character(len=*), intent(in) :: visuname
    character(*), intent(in) :: attributetype
    character(*), intent(in) :: centring
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in), optional :: iter

    character(120):: data_flname
    character(120):: data_flname_path
    character(120):: visu_flname_path
    character(120):: keyword
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
      keyword = trim(varname)
      call generate_pathfile_name(data_flname_path, dm%idom, keyword, dir_data, 'bin', iter)
      if(.not.file_exists(data_flname_path)) &
      call decomp_2d_write_one(X_PENCIL, var, trim(data_flname_path), opt_decomp=dtmp)

    else if(dm%visu_idim == Ivisu_1D_Y) then
      !to add 1D profile
    else 
      keyword = trim(varname)
      call generate_file_name(data_flname, dm%idom, keyword, 'bin', iter)
      call generate_pathfile_name(data_flname_path, dm%idom, keyword, dir_data, 'bin', iter)
      !call decomp_2d_write_plane(X_PENCIL, var, dm%visu_idim, PLANE_AVERAGE, trim(dir_data), &
      !      trim(data_flname), io_name, opt_decomp=dtmp) ! to update, to check
    end if
!----------------------------------------------------------------------------------------------------------
! dataitem for xdmf file
!----------------------------------------------------------------------------------------------------------
    if (nrank == 0) then
      open(newunit = ioxdmf, file = trim(visu_flname_path), action = "write", status = "old", position = "append")

      if(trim(centring) == TRIM(CELL)) then
        nsz(1:3) = ncl_visu(1:3, dm%idom)
      else if (trim(centring) == TRIM(NODE)) then
        nsz(1:3) = nnd_visu(1:3, dm%idom)
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

      write(ioxdmf, *)'      <Attribute Name="'//trim(varname)// &
                            '" AttributeType="'//trim(attributetype)// &
                            '" Center="'//trim(centring)//'">'
      write(ioxdmf, *)'        <DataItem NumberType="Float" Precision="8" Format="Binary" Dimensions="' &
                                //trim(keyword)//'">'
      write(ioxdmf, *)'              '//"../"//trim(data_flname_path)
      write(ioxdmf, *)'        </DataItem>'
      write(ioxdmf, *)'       </Attribute>'
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

    character(120):: data_flname
    character(120):: data_flname_path
    character(120):: visu_flname_path
    character(120):: keyword
    integer :: nsz(3)
    integer :: ioxdmf, iofl

    integer :: j

    if((.not. is_same_decomp(dtmp, dm%dccc))) then
      if(nrank == 0) call Print_error_msg("Data is not stored at cell centre. varname = " // trim(varname))
    end if
!----------------------------------------------------------------------------------------------------------
! write data 
!----------------------------------------------------------------------------------------------------------
    keyword = trim(varname)
    call generate_pathfile_name(data_flname_path, dm%idom, keyword, dir_data, 'dat', iter)
    open(newunit = iofl, file = data_flname_path, action = "write", status="replace")
    if(idim /= 2) call Print_error_msg('Error in direction')
    do j = 1, dtmp%ysz(2)
      write(iofl, *) j, dm%yc(j), var(j) 
    end do

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
    character(120) :: visu_filename

    ! Initialize iteration and filename
    iteration = fl%iteration
    visu_filename = 'flow'
    if (present(suffix)) visu_filename = trim(visu_filename) // '_' // trim(suffix)

    ! Write XDMF header
    call write_visu_headerfooter(dm, trim(visu_filename), XDMF_HEADER, iteration)

    ! Write pressure field (cell-centered)
    call process_and_write_field(fl%pres, dm, "pressure", trim(visu_filename), iteration, &
                                N_DIRECTION)

    ! Process and write velocity components (qx, qy, qz)
    call process_and_write_field(fl%qx, dm, "qx_velocity", trim(visu_filename), iteration, &
                                X_DIRECTION, opt_bc=dm%ibcx_qx)
    call process_and_write_field(fl%qy, dm, "qy_velocity", trim(visu_filename), iteration, &
                                Y_DIRECTION, opt_bc=dm%ibcy_qy)
    call process_and_write_field(fl%qz, dm, "qz_velocity", trim(visu_filename), iteration, &
                                Z_DIRECTION, opt_bc=dm%ibcz_qz)

    ! Process and write thermal fields if enabled
    if (dm%is_thermo) then
      call process_and_write_field(fl%gx, dm, "gx_thermal", trim(visu_filename), iteration, &
                                  X_DIRECTION, opt_bc=dm%ibcx_qx)
      call process_and_write_field(fl%gy, dm, "gy_thermal", trim(visu_filename), iteration, &
                                  Y_DIRECTION, opt_bc=dm%ibcy_qy)
      call process_and_write_field(fl%gz, dm, "gz_thermal", trim(visu_filename), iteration, &
                                  Z_DIRECTION, opt_bc=dm%ibcz_qz)
    end if

    ! Write XDMF footer
    call write_visu_headerfooter(dm, trim(visu_filename), XDMF_FOOTER, iteration)

    ! Debug message
    if (nrank == 0) call Print_debug_mid_msg("Flow field visualization data written successfully.")

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
    character(120) :: visuname

    iter = tm%iteration
    visuname = 'thermo'
    if(present(str)) visuname = trim(visuname)//'_'//trim(str)
!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    call write_visu_headerfooter(dm, trim(visuname), XDMF_HEADER, iter)
!----------------------------------------------------------------------------------------------------------
! write data, temperature, to cell centre
!----------------------------------------------------------------------------------------------------------
    call write_visu_field(dm, tm%tTemp, dm%dccc, "Temp_visu", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%dDens, dm%dccc, "Dens_visu", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%mVisc, dm%dccc, "Visc_visu", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, tm%kCond, dm%dccc, "Cond_visu", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, tm%hEnth, dm%dccc, "Enth_visu", trim(visuname), SCALAR, CELL, iter)
!----------------------------------------------------------------------------------------------------------
! write xdmf footer
!----------------------------------------------------------------------------------------------------------
    call write_visu_headerfooter(dm, trim(visuname), XDMF_FOOTER, iter)

    if(nrank == 0) call Print_debug_mid_msg("Write out visualisation for thermal field.")
    
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
    character(120) :: visu_filename

    ! Initialize iteration and filename
    iteration = fl%iteration
    visu_filename = 'mhd'
    if (present(suffix)) visu_filename = trim(visu_filename) // '_' // trim(suffix)

    ! Write XDMF header
    call write_visu_headerfooter(dm, trim(visu_filename), XDMF_HEADER, iteration)

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
    call write_visu_headerfooter(dm, trim(visu_filename), XDMF_FOOTER, iteration)

    ! Debug message
    if (nrank == 0) call Print_debug_mid_msg("MHD field visualization data written successfully.")

    return
  end subroutine

!==========================================================================================================
  subroutine write_visu_stats_flow(fl, dm)
    use udf_type_mod
    use precision_mod
    use operations
    implicit none 
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl

    integer :: iter 
    character(120) :: visuname

!==========================================================================================================
! write time averaged 3d data
!==========================================================================================================
    iter = fl%iteration
    visuname = 'time_averaged_flow'
!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    call write_visu_headerfooter(dm, trim(visuname), XDMF_HEADER, iter)
!----------------------------------------------------------------------------------------------------------
! write data, 
!----------------------------------------------------------------------------------------------------------
    call write_visu_field(dm, fl%pr_mean,                     dm%dccc, "time_averaged_pr", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%u_vector_mean  (:, :, :, 1), dm%dccc, "time_averaged_ux", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%u_vector_mean  (:, :, :, 2), dm%dccc, "time_averaged_uy", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%u_vector_mean  (:, :, :, 3), dm%dccc, "time_averaged_uz", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%uu_tensor6_mean(:, :, :, 1), dm%dccc, "time_averaged_uu", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%uu_tensor6_mean(:, :, :, 2), dm%dccc, "time_averaged_vv", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%uu_tensor6_mean(:, :, :, 3), dm%dccc, "time_averaged_ww", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%uu_tensor6_mean(:, :, :, 4), dm%dccc, "time_averaged_uv", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%uu_tensor6_mean(:, :, :, 5), dm%dccc, "time_averaged_uw", trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, fl%uu_tensor6_mean(:, :, :, 6), dm%dccc, "time_averaged_vw", trim(visuname), SCALAR, CELL, iter)
!----------------------------------------------------------------------------------------------------------
! write xdmf footer
!----------------------------------------------------------------------------------------------------------
    call write_visu_headerfooter(dm, trim(visuname), XDMF_FOOTER, iter)
!==========================================================================================================
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!==========================================================================================================
    if( ANY(dm%is_periodic(:))) then

    iter = fl%iteration
    visuname = 'time_space_averaged_flow'
!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    if(.not. (dm%is_periodic(1) .and. dm%is_periodic(3))) &
    call write_visu_headerfooter(dm, trim(visuname), XDMF_HEADER, iter)
!----------------------------------------------------------------------------------------------------------
! write data, 
!----------------------------------------------------------------------------------------------------------
    call visu_average_periodic_data(                    fl%pr_mean, dm%dccc, dm, "time_space_averaged_pr", trim(visuname), iter)
    call visu_average_periodic_data(  fl%u_vector_mean(:, :, :, 1), dm%dccc, dm, "time_space_averaged_ux", trim(visuname), iter)
    call visu_average_periodic_data(  fl%u_vector_mean(:, :, :, 2), dm%dccc, dm, "time_space_averaged_uy", trim(visuname), iter)
    call visu_average_periodic_data(  fl%u_vector_mean(:, :, :, 3), dm%dccc, dm, "time_space_averaged_uz", trim(visuname), iter)
    call visu_average_periodic_data(fl%uu_tensor6_mean(:, :, :, 1), dm%dccc, dm, "time_space_averaged_uu", trim(visuname), iter)
    call visu_average_periodic_data(fl%uu_tensor6_mean(:, :, :, 2), dm%dccc, dm, "time_space_averaged_vv", trim(visuname), iter)
    call visu_average_periodic_data(fl%uu_tensor6_mean(:, :, :, 3), dm%dccc, dm, "time_space_averaged_ww", trim(visuname), iter)
    call visu_average_periodic_data(fl%uu_tensor6_mean(:, :, :, 4), dm%dccc, dm, "time_space_averaged_uv", trim(visuname), iter)
    call visu_average_periodic_data(fl%uu_tensor6_mean(:, :, :, 5), dm%dccc, dm, "time_space_averaged_uw", trim(visuname), iter)
    call visu_average_periodic_data(fl%uu_tensor6_mean(:, :, :, 6), dm%dccc, dm, "time_space_averaged_vw", trim(visuname), iter)
!----------------------------------------------------------------------------------------------------------
! write xdmf footer
!----------------------------------------------------------------------------------------------------------
    if(.not. (dm%is_periodic(1) .and. dm%is_periodic(3))) &
    call write_visu_headerfooter(dm, trim(visuname), XDMF_FOOTER, iter)

    end if

    return
  end subroutine

  !==========================================================================================================
  subroutine write_visu_stats_thermo(tm, dm)
    use udf_type_mod
    use precision_mod
    use operations
    implicit none 
    
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm

    integer :: iter 
    character(120) :: visuname
    
!==========================================================================================================
! write time averaged 3d data
!==========================================================================================================
    iter = tm%iteration
    visuname = 'time_averaged_thermo'
!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    call write_visu_headerfooter(dm, trim(visuname), XDMF_HEADER, iter)
!----------------------------------------------------------------------------------------------------------
! write data, 
!----------------------------------------------------------------------------------------------------------
    call write_visu_field(dm, tm%t_mean,  dm%dccc, "time_averaged_T",  trim(visuname), SCALAR, CELL, iter)
    call write_visu_field(dm, tm%tt_mean, dm%dccc, "time_averaged_TT", trim(visuname), SCALAR, CELL, iter)
!----------------------------------------------------------------------------------------------------------
! write xdmf footer
!----------------------------------------------------------------------------------------------------------
    call write_visu_headerfooter(dm, trim(visuname), XDMF_FOOTER, iter)
!==========================================================================================================
! write time averaged and space averaged 3d data (stored 2d or 1d data)
!==========================================================================================================
    if( ANY(dm%is_periodic(:))) then

    iter = tm%iteration
    visuname = 'time_space_averaged_thermo'
!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    if(.not. (dm%is_periodic(1) .and. dm%is_periodic(3))) &
    call write_visu_headerfooter(dm, trim(visuname), XDMF_HEADER, iter)
!----------------------------------------------------------------------------------------------------------
! write data, 
!----------------------------------------------------------------------------------------------------------
    call visu_average_periodic_data(tm%t_mean,   dm%dccc, dm, "time_space_averaged_T",   trim(visuname), iter)
    call visu_average_periodic_data(tm%tt_mean,  dm%dccc, dm, "time_space_averaged_TT",  trim(visuname), iter)
!----------------------------------------------------------------------------------------------------------
! write xdmf footer
!----------------------------------------------------------------------------------------------------------
    if(.not. (dm%is_periodic(1) .and. dm%is_periodic(3))) &
    call write_visu_headerfooter(dm, trim(visuname), XDMF_FOOTER, iter)

    end if
    
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

    character(120) :: keyword

!----------------------------------------------------------------------------------------------------------
! write xdmf header
!----------------------------------------------------------------------------------------------------------
    keyword = trim(visuname)//"_"//trim(varname)//'_visu'
    call write_visu_headerfooter(dm, trim(keyword), XDMF_HEADER, iter)
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
    call write_visu_headerfooter(dm, trim(keyword), XDMF_FOOTER, iter)
    
    return
  end subroutine


!==========================================================================================================
!==========================================================================================================
  subroutine visu_average_periodic_data(data_in, dtmp, dm, str1, str2, iter)
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
