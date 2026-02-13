
!==========================================================================================================
!  VISUALISATION I/O (mesh + fields) for XDMF + raw binary (.bin)
!  - Mesh: write once at start (or when mesh changes), with XDMF describing the grid structure.
!  - Fields: write at each output time, with XDMF describing the attribute and referencing the grid.
!  Key design: keep XDMF files human-readable and editable, with binary files for data. This allows easy post-processing and visualization with tools like ParaView.
!  Author: Wei Wang (2026-02)
!  Date: 2026-02
!==========================================================================================================

module visualisation_xdmf_io_mod
  use iso_fortran_env, only: int32
  use parameters_constant_mod, only: WP, NDIM
  implicit none
  private

  private :: xdmf_seek_bytes_int32
  public :: xdmf_begin_grid_cart
  public :: xdmf_begin_grid_cyl
  public :: xdmf_end_grid
  public :: xdmf_write_attribute_scalar
  public :: xdmf_dims_kji_string

contains
  !========================================================================================================
  pure function xdmf_seek_bytes_int32(n_int32) result(seekstr)
    integer, intent(in) :: n_int32
    character(len=32) :: seekstr
    integer :: bytes
    bytes = n_int32 * (storage_size(0_int32)/8)
    write(seekstr,'(I0)') bytes
  end function xdmf_seek_bytes_int32
  !========================================================================================================
  pure function xdmf_dims_kji_string(nijk) result(s)
    integer, intent(in) :: nijk(NDIM)
    character(len=64) :: s
    write(s,'(I0,1X,I0,1X,I0)') nijk(3), nijk(2), nijk(1)
  end function xdmf_dims_kji_string
  !========================================================================================================
  subroutine xdmf_begin_grid_cart(unit, grid_name, nnode, grid_files_1d)
    use typeconvert_mod, only: int2str
    integer, intent(in) :: unit
    character(*), intent(in) :: grid_name
    integer, intent(in) :: nnode(NDIM)
    character(*), intent(in) :: grid_files_1d(NDIM) ! x,y,z 1D files

    character(len=64) :: dims

    dims = xdmf_dims_kji_string(nnode)

    write(unit,'(A)') '<?xml version="1.0" ?>'
    write(unit,'(A)') '<Xdmf Version="3.0">'
    write(unit,'(A)') '  <Domain>'
    write(unit,'(A)') '    <Grid Name="'//trim(grid_name)//'" GridType="Uniform">'
    write(unit,'(A)') '      <Topology TopologyType="3DRectMesh" Dimensions="'//trim(dims)//'"/>'
    write(unit,'(A)') '      <Geometry GeometryType="VXVYVZ">'

    call xdmf_write_dataitem_1d(unit, grid_files_1d(1), nnode(1))
    call xdmf_write_dataitem_1d(unit, grid_files_1d(2), nnode(2))
    call xdmf_write_dataitem_1d(unit, grid_files_1d(3), nnode(3))

    write(unit,'(A)') '      </Geometry>'
  end subroutine xdmf_begin_grid_cart
  !========================================================================================================
  subroutine xdmf_write_dataitem_1d(unit, filename, npts)
    use typeconvert_mod, only: int2str
    integer, intent(in) :: unit, npts
    character(*), intent(in) :: filename
    ! Your 1D coord bin format: [int32 npts][real*8 data...]
    write(unit,'(A)') '        <DataItem ItemType="Uniform"'
    write(unit,'(A)') '                  Dimensions="'//trim(int2str(npts))//'"'
    write(unit,'(A)') '                  NumberType="Float"'
    write(unit,'(A)') '                  Precision="8"'
    write(unit,'(A)') '                  Format="Binary"'
    write(unit,'(A)') '                  Seek="'//trim(xdmf_seek_bytes_int32(1))//'">'
    write(unit,'(A)') '          ../'//trim(filename)
    write(unit,'(A)') '        </DataItem>'
  end subroutine xdmf_write_dataitem_1d
  !========================================================================================================
  subroutine xdmf_begin_grid_cyl(unit, grid_name, nnode, grid_file)
    use typeconvert_mod, only: int2str
    integer, intent(in) :: unit
    character(*), intent(in) :: grid_name
    integer, intent(in) :: nnode(NDIM)
    character(*), intent(in) :: grid_file

    integer :: npts
    character(len=64) :: topo_dims
    character(len=64) :: geom_dims
    character(len=32) :: seek

    topo_dims = xdmf_dims_kji_string(nnode)
    npts = nnode(1)*nnode(2)*nnode(3)

    ! Your XYZ bin format: [int32 nx,ny,nz][ (x,y,z) as real*8 repeated ]
    ! Seek = 3 int32 header = 12 bytes typically
    seek = xdmf_seek_bytes_int32(3)
    write(geom_dims,'(I0,1X,I0)') npts, 3

    write(unit,'(A)') '<?xml version="1.0" ?>'
    write(unit,'(A)') '<Xdmf Version="3.0">'
    write(unit,'(A)') '  <Domain>'
    write(unit,'(A)') '    <Grid Name="'//trim(grid_name)//'" GridType="Uniform">'
    write(unit,'(A)') '      <Topology TopologyType="3DSMesh" Dimensions="'//trim(topo_dims)//'"/>'
    write(unit,'(A)') '      <Geometry GeometryType="XYZ">'
    write(unit,'(A)') '        <DataItem ItemType="Uniform"'
    write(unit,'(A)') '                  Dimensions="'//trim(geom_dims)//'"'
    write(unit,'(A)') '                  NumberType="Float"'
    write(unit,'(A)') '                  Precision="8"'
    write(unit,'(A)') '                  Format="Binary"'
    write(unit,'(A)') '                  Endian="Big"'
    write(unit,'(A)') '                  Seek="'//trim(seek)//'">'
    write(unit,'(A)') '          ../'//trim(grid_file)
    write(unit,'(A)') '        </DataItem>'
    write(unit,'(A)') '      </Geometry>'
  end subroutine xdmf_begin_grid_cyl
  !======================================================================================================== 
  subroutine xdmf_write_attribute_scalar(unit, name, center, dims_kji, data_file_rel)
    integer, intent(in) :: unit
    character(*), intent(in) :: name, center, dims_kji, data_file_rel

    write(unit,'(A)') '      <Attribute Name="'//trim(name)//'" AttributeType="Scalar" Center="'//trim(center)//'">'
    write(unit,'(A)') '        <DataItem ItemType="Uniform"'
    write(unit,'(A)') '                  NumberType="Float"'
    write(unit,'(A)') '                  Precision="8"'
    write(unit,'(A)') '                  Format="Binary"'
    write(unit,'(A)') '                  Dimensions="'//trim(dims_kji)//'">'
    write(unit,'(A)') '          ../'//trim(data_file_rel)
    write(unit,'(A)') '        </DataItem>'
    write(unit,'(A)') '      </Attribute>'
  end subroutine xdmf_write_attribute_scalar
  !========================================================================================================
  subroutine xdmf_end_grid(unit)
    integer, intent(in) :: unit
    write(unit,'(A)') '    </Grid>'
    write(unit,'(A)') '  </Domain>'
    write(unit,'(A)') '</Xdmf>'
  end subroutine xdmf_end_grid

end module visualisation_xdmf_io_mod
!========================================================================================================
!========================================================================================================
module visualisation_mesh_mod
  use iso_fortran_env, only: int32
  use parameters_constant_mod, only: WP, NDIM
  use parameters_constant_mod
  use visualisation_xdmf_io_mod
  use io_tools_mod
  implicit none
  private
  !
  integer, save :: NMINPL(NDIM)=(/16, 8, 16/)
  integer, parameter, public :: NSLICE = 3   ! 3 slices per direction by default (quarter-ish positions)
  !
  integer, parameter, public :: Ivisu_3D   = 0, & ! visualise 3d field only
                                Ivisu_2D   = 1, & ! visualise 2d field (3 planes in each dir) only
                                Ivisu_3D2D = 2    ! visualise both 3d and 2d
  integer, save, public :: nave_plane(NDIM)
  type(DECOMP_INFO), public :: d1cc
  type(DECOMP_INFO), public :: dc1c
  type(DECOMP_INFO), public :: dcc1
  integer, save, public :: slice_idx(NDIM, 0:NSLICE)
  integer, save, public :: nnd_visu(NDIM), ncl_visu(NDIM)
  character(len=256), save, public :: grid_cart_3d_fl(NDIM)
  character(len=256), save, public :: grid_cart_slice(NDIM, NDIM, 0:NSLICE)  ! (coordfile index xyz, dir=1..3, n=1..NSLICE)
  character(len=256), save, public :: grid_cyl_3d_fl
  character(len=256), save, public :: grid_cyl_slice(NDIM, 0:NSLICE)         ! (dir, n)
  
  
  public  :: write_visu_ini
  private :: compute_visu_nnode
  private :: compute_slice_indices
  private :: build_cartesian_coords
  private :: build_cylindrical_to_cart
  private :: write_mesh_cartesian
  private :: write_cartesian_slice_one
  private :: write_mesh_cylindrical

contains
  !========================================================================================================
  subroutine write_visu_ini(dm)
    use udf_type_mod
    use parameters_constant_mod, only: ICARTESIAN, ICYLINDRICAL
    use decomp_2d, only: xszV, yszV, zszV
    implicit none
    type(t_domain), intent(in) :: dm
    !
    real(WP), allocatable :: x1(:), y1(:), z1(:)
    real(WP), allocatable :: x3(:,:,:), y3(:,:,:), z3(:,:,:)
    logical :: px, py, pz
    !
    px = dm%is_periodic(1)
    py = dm%is_periodic(2)
    pz = dm%is_periodic(3)
    nave_plane = 0
    ! Case: X periodic only -> average over X, keep YZ plane
    if (px .and. (.not. py) .and. (.not. pz)) then
      nave_plane(1) = 1
    end if
    ! Case: Y periodic only -> not supported
    if ((.not. px) .and. py .and. (.not. pz)) then
      nave_plane(2) = 2
    end if
    ! Case: Z periodic only -> average over Z, keep XY plane
    if ((.not. px) .and. (.not. py) .and. pz) then
      nave_plane(3) = 3
    end if

    call compute_visu_nnode(dm, nnd_visu)
    ncl_visu = nnd_visu - 1
    call compute_slice_indices(nnd_visu, slice_idx)
    call decomp_info_init(1, dm%nc(2), dm%nc(3), d1cc)
    call decomp_info_init(dm%nc(1), 1, dm%nc(3), dc1c)
    call decomp_info_init(dm%nc(1), dm%nc(2), 1, dcc1)

    if (nrank /= 0) return

    select case (dm%icoordinate)
    case (ICARTESIAN)
      allocate(x1(nnd_visu(1)), y1(nnd_visu(2)), z1(nnd_visu(3)))
      call build_cartesian_coords(dm, x1, y1, z1)
      call write_mesh_cartesian(dm, nnd_visu, x1, y1, z1)
      deallocate(x1, y1, z1)

    case (ICYLINDRICAL)
      allocate(x3(nnd_visu(1),nnd_visu(2),nnd_visu(3)))
      allocate(y3(nnd_visu(1),nnd_visu(2),nnd_visu(3)))
      allocate(z3(nnd_visu(1),nnd_visu(2),nnd_visu(3)))
      call build_cylindrical_to_cart(dm, x3, y3, z3)
      call write_mesh_cylindrical(dm, nnd_visu, x3, y3, z3)
      deallocate(x3, y3, z3)

    case default
      error stop "write_visu_ini: unknown coordinate system"
    end select

  end subroutine write_visu_ini
  !========================================================================================================
  subroutine compute_visu_nnode(dm, nnode)
    use udf_type_mod
    use decomp_2d, only: xszV, yszV, zszV
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(out) :: nnode(NDIM)

    if (any(dm%visu_nskip(1:3) > 1)) then
      nnode = [ xszV(1), yszV(2), zszV(3) ]  ! your existing convention
    else
      nnode = dm%np_geo(1:3)
    end if
  end subroutine compute_visu_nnode
  !========================================================================================================
  subroutine compute_slice_indices(nnode, idx_out)
    integer, intent(in)  :: nnode(NDIM)
    integer, intent(out) :: idx_out(NDIM, 0:NSLICE)
    integer :: d, n
    integer :: imax

    do d = 1, NDIM
      ! we need a 2-layer slab => start index must satisfy i <= nnode(d)-1
      imax = max(1, nnode(d)-1)
      do n = 0, NSLICE
        ! evenly spaced inside domain (avoids boundary by default)
        idx_out(d,n) = 1 + n * (imax-1) / (NSLICE+1)
        idx_out(d,n) = max(1, min(imax, idx_out(d,n)))
        if(n==1) &
        idx_out(d,n) = min(NMINPL(d), idx_out(d,n))
        if(n==NSLICE) &
        idx_out(d,n) = max(nnode(d)-1-NMINPL(d), idx_out(d,n))
        if(n==0) &
        idx_out(d,n) = 1
      end do
      
    end do
  end subroutine compute_slice_indices
  !========================================================================================================
  subroutine build_cartesian_coords(dm, x, y, z)
    use udf_type_mod
    use parameters_constant_mod, only: MAXP
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: x(:), y(:), z(:)
    integer :: i

    x = MAXP; y = MAXP; z = MAXP

    do i = 1, size(x)
      x(i) = real(i-1, WP) * dm%h(1) * dm%visu_nskip(1)
    end do
    do i = 1, size(y)
      if (dm%is_stretching(2)) then
        y(i) = dm%yp(i)
      else
        y(i) = real(i-1, WP) * dm%h(2) * dm%visu_nskip(2)
      end if
    end do
    do i = 1, size(z)
      z(i) = real(i-1, WP) * dm%h(3) * dm%visu_nskip(3)
    end do
  end subroutine build_cartesian_coords
  !========================================================================================================
  subroutine build_cylindrical_to_cart(dm, x, y, z)
    use udf_type_mod
    use parameters_constant_mod, only: MAXP
    use math_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: x(:,:,:), y(:,:,:), z(:,:,:)

    integer :: i,j,k
    real(WP) :: r, th

    x = MAXP; y = MAXP; z = MAXP

    do k = 1, size(x,3)
      do j = 1, size(x,2)
        do i = 1, size(x,1)
          x(i,j,k) = real(i-1, WP) * dm%h(1) * dm%visu_nskip(1)

          if (dm%is_stretching(2)) then
            r = dm%yp(j)
          else
            r = real(j-1, WP) * dm%h(2) * dm%visu_nskip(2)
          end if

          th = real(k-1, WP) * dm%h(3) * dm%visu_nskip(3)

          ! convert (r,theta) -> (y,z); keep x as axial
          y(i,j,k) = r * sin_wp(th)
          z(i,j,k) = r * cos_wp(th)
        end do
      end do
    end do
  end subroutine build_cylindrical_to_cart
  !========================================================================================================
  subroutine write_mesh_cartesian(dm, nnode, x, y, z)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: nnode(NDIM)
    real(WP), intent(in) :: x(:), y(:), z(:)

    integer :: dir, n, n2(NDIM), nen
    character(len=256) :: xdmf_name
    character(len=256) :: grid1d(NDIM)

    ! 3D (rectilinear) coord files
    call write_bin_cart_1d(dm%idom, 'grid_x', x, grid_cart_3d_fl(1))
    call write_bin_cart_1d(dm%idom, 'grid_y', y, grid_cart_3d_fl(2))
    call write_bin_cart_1d(dm%idom, 'grid_z', z, grid_cart_3d_fl(3))

    call write_xdmf_mesh_cart(dm%idom, 'grids_3d', nnode, grid_cart_3d_fl)

    ! 2D slices: each slice has a 2-point coord file in the sliced direction
    nen = 0
    if(dm%visu_idim == Ivisu_2D .or. dm%visu_idim == Ivisu_3D2D) then
      nen = NSLICE
    end if
    do n = 0, nen
      do dir = 1, NDIM 
        if (n == 0 .and. dir /= nave_plane(dir)) then
          cycle 
        end if
        call write_cartesian_slice_one(dm, nnode, dir, n, x, y, z)
      end do
    end do

  end subroutine write_mesh_cartesian
  !========================================================================================================
  subroutine write_cartesian_slice_one(dm, nnode3, dir, n, x, y, z)
    use udf_type_mod
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: nnode3(NDIM), dir, n
    real(WP), intent(in) :: x(:), y(:), z(:)

    integer :: n2(NDIM), npl
    character(len=256) :: grid_name
    character(len=256) :: f2, gfiles(NDIM)

    n2 = nnode3
    n2(dir) = 2
    npl = slice_idx(dir, n)
    select case (dir)
    case (1)
      grid_name = 'grid_xi'//trim(int2str(npl))
      call write_bin_cart_1d(dm%idom, grid_name, x(npl:npl+1), f2)
      gfiles = grid_cart_3d_fl
      gfiles(1) = f2
    case (2)
      grid_name = 'grid_yi'//trim(int2str(npl))
      call write_bin_cart_1d(dm%idom, grid_name, y(npl:npl+1), f2)
      gfiles = grid_cart_3d_fl
      gfiles(2) = f2
    case (3)
      grid_name = 'grid_zi'//trim(int2str(npl))
      call write_bin_cart_1d(dm%idom, grid_name, z(npl:npl+1), f2)
      gfiles = grid_cart_3d_fl
      gfiles(3) = f2
    end select

    grid_cart_slice(:,dir,n) = gfiles(:)
    call write_xdmf_mesh_cart(dm%idom, trim(grid_name), n2, gfiles)

  end subroutine write_cartesian_slice_one
  !========================================================================================================
  subroutine write_mesh_cylindrical(dm, nnode, x, y, z)
    use udf_type_mod   
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: nnode(NDIM)
    real(WP), intent(in) :: x(:,:,:), y(:,:,:), z(:,:,:)

    integer :: dir, n2(NDIM), npl, n, nen
    character(len=256) :: grid_name
    character(len=256) :: fxyz

    ! 3D curvilinear mesh: one XYZ binary
    grid_name = 'grids_3d'
    call generate_pathfile_name(grid_cyl_3d_fl, dm%idom, grid_name, dir_data, 'bin')
    call write_bin_cyl_xyz(grid_cyl_3d_fl, x, y, z, nnode)
    call write_xdmf_mesh_cyl(dm%idom, grid_name, nnode, grid_cyl_3d_fl)

    ! slices: write a 2-layer slab as XYZ too (same file format)
    nen = 0
    if(dm%visu_idim == Ivisu_2D .or. dm%visu_idim == Ivisu_3D2D) then
      nen = NSLICE
    end if
    do n = 0, nen
      do dir = 1, NDIM 
        if (n == 0 .and. dir /= nave_plane(dir)) then
          cycle 
        end if
        n2 = nnode
        n2(dir) = 2
        npl = slice_idx(dir, n)
        select case(dir)
        case(1)
          grid_name = 'grid_xi'//trim(int2str(npl))
          call generate_pathfile_name(fxyz, dm%idom, grid_name, dir_data, 'bin')
          call write_bin_cyl_xyz(fxyz, x(npl:npl+1,:,:), y(npl:npl+1,:,:), z(npl:npl+1,:,:), n2)

        case(2)
          grid_name = 'grid_yi'//trim(int2str(npl))
          call generate_pathfile_name(fxyz, dm%idom, grid_name, dir_data, 'bin')
          call write_bin_cyl_xyz(fxyz, x(:,npl:npl+1,:), y(:,npl:npl+1,:), z(:,npl:npl+1,:), n2)

        case(3)
          grid_name = 'grid_zi'//trim(int2str(npl))
          call generate_pathfile_name(fxyz, dm%idom, grid_name, dir_data, 'bin')
          call write_bin_cyl_xyz(fxyz, x(:,:,npl:npl+1), y(:,:,npl:npl+1), z(:,:,npl:npl+1), n2)
        end select

        grid_cyl_slice(dir, n) = fxyz
        call write_xdmf_mesh_cyl(dm%idom, grid_name, n2, fxyz)
      end do
    end do
    return
  end subroutine write_mesh_cylindrical
  !========================================================================================================
  subroutine write_bin_cart_1d(idom, keyword, a, filename_out)
    implicit none
    integer, intent(in) :: idom
    character(*), intent(in) :: keyword
    real(WP), intent(in) :: a(:)
    character(len=*), intent(out) :: filename_out
    integer :: u, ios

    call generate_pathfile_name(filename_out, idom, keyword, dir_data, 'bin')

    open(newunit=u, file=trim(filename_out), access='stream', form='unformatted', &
         status='replace', action='write', iostat=ios)
    if (ios /= 0) error stop 'write_bin_cart_1d: cannot open file'

    write(u) int(size(a), int32)
    write(u) a
    close(u)
  end subroutine write_bin_cart_1d
  !========================================================================================================
  subroutine write_bin_cyl_xyz(filename, x, y, z, nnode)
    implicit none
    character(*), intent(in) :: filename
    real(WP), intent(in) :: x(:,:,:), y(:,:,:), z(:,:,:)
    integer, intent(in) :: nnode(NDIM)

    integer :: u, ios
    integer(int32) :: hdr(3)
    integer :: i,j,k
    real(WP) :: buf(3)

    hdr = int(nnode, int32)

    open(newunit=u, file=trim(filename), access='stream', form='unformatted', &
         status='replace', action='write', iostat=ios, convert='BIG_ENDIAN')
    if (ios /= 0) error stop 'write_bin_cyl_xyz: cannot open file'

    write(u) hdr(1), hdr(2), hdr(3)
    do k = 1, nnode(3)
      do j = 1, nnode(2)
        do i = 1, nnode(1)
          buf = [ x(i,j,k), y(i,j,k), z(i,j,k) ]
          write(u) buf
        end do
      end do
    end do
    close(u)
  end subroutine write_bin_cyl_xyz
  !========================================================================================================
  subroutine write_xdmf_mesh_cart(idom, visuname, nnode, gridfiles)
    implicit none
    integer, intent(in) :: idom
    character(*), intent(in) :: visuname
    integer, intent(in) :: nnode(NDIM)
    character(*), intent(in) :: gridfiles(NDIM)

    integer :: u
    character(len=256) :: xdmf_file

    call generate_pathfile_name(xdmf_file, idom, visuname, dir_visu, 'xdmf', 0)
    open(newunit=u, file=trim(xdmf_file), status='replace', action='write')
    call xdmf_begin_grid_cart(u, xdmf_file, nnode, gridfiles)
    call xdmf_end_grid(u)
    close(u)
  end subroutine write_xdmf_mesh_cart
  !========================================================================================================
  subroutine write_xdmf_mesh_cyl(idom, grid_name, nnode, grid_file)
    implicit none
    integer, intent(in) :: idom
    character(*), intent(in) :: grid_name
    integer, intent(in) :: nnode(NDIM)
    character(*), intent(in) :: grid_file

    character(len=256) :: xdmf
    integer :: unit

    call generate_pathfile_name(xdmf, idom, grid_name, dir_visu, 'xdmf', 0)

    open(newunit=unit, file=trim(xdmf), status='replace', action='write')
    call xdmf_begin_grid_cyl(unit, grid_name, nnode, grid_file)
    call xdmf_end_grid(unit)
    close(unit)
  end subroutine write_xdmf_mesh_cyl

end module visualisation_mesh_mod
!==========================================================================================================
!  FIELD OUTPUT: write binary (3D + slices) then append XDMF attributes into the matching XDMF files.
!  Key change vs your current version:
!    - Mesh XDMF files are created once by visualisation_mesh_mod.
!    - Field XDMF writing *appends* <Attribute> blocks into those files before </Grid>.
!      (We do this by writing a field-xdmf file that *re-declares the grid* OR by
!       having separate XDMF per field. Below keeps your original pattern: one XDMF per field,
!       with grid + attributes in that file. It’s consistent and easy.)
!==========================================================================================================
module visualisation_field_mod
  use parameters_constant_mod, only: WP, NDIM
  use visualisation_xdmf_io_mod
  use visualisation_mesh_mod
  use io_tools_mod
  implicit none
  private

  integer, parameter :: N_DIRECTION = 0, &
                        X_DIRECTION = 1, &
                        Y_DIRECTION = 2, &
                        Z_DIRECTION = 3
  public :: write_visu_any3darray
  public :: write_visu_flow
  public :: write_visu_thermo
  public :: write_visu_mhd
  public :: write_visu_field_bin_and_xdmf
  public :: write_visu_file_begin
  public :: write_visu_file_end
  public :: write_visu_plane_binary_and_xdmf
  !
  private :: find_n_from_npl
  private :: stagger_to_ccc
  private :: slice_prefix
  private :: write_coarsened_3d
  private :: write_plane_bin
  private :: write_slice_field_xdmf
  private :: write_visu_3d_binary_and_xdmf

contains

  !----------------------------- High-level drivers --------------------------------
  !==========================================================================================================
  subroutine write_visu_flow(fl, dm, suffix)
    use udf_type_mod
    implicit none
    type(t_flow),   intent(in) :: fl
    type(t_domain), intent(in) :: dm
    character(*),   intent(in), optional :: suffix
    character(len=256) :: visuname
    integer :: iter

    iter = fl%iteration
    visuname = 'flow'
    if (present(suffix)) visuname = trim(visuname)//'_'//trim(suffix)
    
    call write_visu_file_begin(dm, visuname, iter)
    call write_visu_field_bin_and_xdmf(dm, fl%pres, 'pressure', visuname, iter, N_DIRECTION)
    call write_visu_field_bin_and_xdmf(dm, fl%pcor, 'phi',      visuname, iter, N_DIRECTION)

    call write_visu_field_bin_and_xdmf(dm, fl%qx, 'qx_ccc', visuname, iter, X_DIRECTION, opt_ibc=dm%ibcx_qx)
    call write_visu_field_bin_and_xdmf(dm, fl%qy, 'qy_ccc', visuname, iter, Y_DIRECTION, opt_ibc=dm%ibcy_qy)
    call write_visu_field_bin_and_xdmf(dm, fl%qz, 'qz_ccc', visuname, iter, Z_DIRECTION, opt_ibc=dm%ibcz_qz)

    if (dm%is_thermo) then
      call write_visu_field_bin_and_xdmf(dm, fl%gx, 'gx_ccc', visuname, iter, X_DIRECTION, opt_ibc=dm%ibcx_qx)
      call write_visu_field_bin_and_xdmf(dm, fl%gy, 'gy_ccc', visuname, iter, Y_DIRECTION, opt_ibc=dm%ibcy_qy)
      call write_visu_field_bin_and_xdmf(dm, fl%gz, 'gz_ccc', visuname, iter, Z_DIRECTION, opt_ibc=dm%ibcz_qz)
    end if
    call write_visu_file_end(dm, visuname, iter)
    return
  end subroutine write_visu_flow
  !==========================================================================================================
  subroutine write_visu_thermo(tm, fl, dm, suffix)
    use udf_type_mod
    implicit none
    type(t_thermo), intent(in) :: tm
    type(t_flow),   intent(in) :: fl
    type(t_domain), intent(in) :: dm
    character(*),   intent(in), optional :: suffix
    character(len=256) :: visuname
    integer :: iter

    iter = tm%iteration
    visuname = 'thermo'
    if (present(suffix)) visuname = trim(visuname)//'_'//trim(suffix)
    
    call write_visu_file_begin(dm, visuname, iter)
    call write_visu_field_bin_and_xdmf(dm, tm%tTemp, 'Temperature',  visuname, iter, N_DIRECTION)
    call write_visu_field_bin_and_xdmf(dm, fl%dDens, 'Density',      visuname, iter, N_DIRECTION)
    call write_visu_field_bin_and_xdmf(dm, fl%mVisc, 'Viscosity',    visuname, iter, N_DIRECTION)
    call write_visu_field_bin_and_xdmf(dm, tm%kCond, 'Conductivity', visuname, iter, N_DIRECTION)
    call write_visu_field_bin_and_xdmf(dm, tm%hEnth, 'Enthalpy',     visuname, iter, N_DIRECTION)
    call write_visu_field_bin_and_xdmf(dm, fl%drhodt,'drho_dt',      visuname, iter, N_DIRECTION)
    call write_visu_file_end(dm, visuname, iter)
    return
  end subroutine write_visu_thermo
  !==========================================================================================================
  subroutine write_visu_mhd(mh, fl, dm, suffix)
    use udf_type_mod
    implicit none
    type(t_mhd),   intent(in) :: mh
    type(t_flow),  intent(in) :: fl
    type(t_domain),intent(in) :: dm
    character(*),  intent(in), optional :: suffix
    character(len=256) :: visuname
    integer :: iter

    iter = fl%iteration
    visuname = 'mhd'
    if (present(suffix)) visuname = trim(visuname)//'_'//trim(suffix)
    ! 
    call write_visu_file_begin(dm, visuname, iter)
    call write_visu_field_bin_and_xdmf(dm, mh%ep, 'electric_potential', visuname, iter, N_DIRECTION)

    call write_visu_field_bin_and_xdmf(dm, mh%jx, 'jx_current', visuname, iter, X_DIRECTION, opt_ibc=mh%ibcx_jx)
    call write_visu_field_bin_and_xdmf(dm, mh%jy, 'jy_current', visuname, iter, Y_DIRECTION, opt_ibc=mh%ibcy_jy)
    call write_visu_field_bin_and_xdmf(dm, mh%jz, 'jz_current', visuname, iter, Z_DIRECTION, opt_ibc=mh%ibcz_jz)

    call write_visu_field_bin_and_xdmf(dm, fl%lrfx, 'fx_Lorentz', visuname, iter, X_DIRECTION, opt_ibc=dm%ibcx_qx)
    call write_visu_field_bin_and_xdmf(dm, fl%lrfy, 'fy_Lorentz', visuname, iter, Y_DIRECTION, opt_ibc=dm%ibcy_qy)
    call write_visu_field_bin_and_xdmf(dm, fl%lrfz, 'fz_Lorentz', visuname, iter, Z_DIRECTION, opt_ibc=dm%ibcz_qz)
    !
    call write_visu_file_end(dm, visuname, iter)
    return 
  end subroutine write_visu_mhd
  !==========================================================================================================
  subroutine write_visu_any3darray(var, varname, visuname, dtmp, dm, iter)
    use udf_type_mod
    use decomp_operation_mod
    implicit none
    real(WP), intent(in)          :: var(:,:,:)
    character(*), intent(in)      :: varname
    character(*), intent(in)      :: visuname
    type(DECOMP_INFO), intent(in) :: dtmp
    type(t_domain), intent(in)    :: dm
    integer, intent(in)           :: iter

    character(len=256) :: outname

    outname = trim(visuname)//'_'//trim(varname)//'_visu'

    call write_visu_file_begin(dm, outname, iter)

    if (is_same_decomp(dtmp, dm%dccc)) then
      call write_visu_field_bin_and_xdmf(dm, var, trim(varname), outname, iter, N_DIRECTION)
    else if (is_same_decomp(dtmp, dm%dpcc)) then
      call write_visu_field_bin_and_xdmf(dm, var, trim(varname), outname, iter, X_DIRECTION, opt_ibc=dm%ibcx_qx)
    else if (is_same_decomp(dtmp, dm%dcpc)) then
      call write_visu_field_bin_and_xdmf(dm, var, trim(varname), outname, iter, Y_DIRECTION, opt_ibc=dm%ibcy_qy)
    else if (is_same_decomp(dtmp, dm%dccp)) then
      call write_visu_field_bin_and_xdmf(dm, var, trim(varname), outname, iter, Z_DIRECTION, opt_ibc=dm%ibcz_qz)
    else
      call Print_error_msg("write_visu_any3darray: unsupported decomposition for "//trim(varname))
    end if

    call write_visu_file_end(dm, outname, iter)
  end subroutine write_visu_any3darray
  !----------------------------- File begin/end -----------------------------------
  !==========================================================================================================
  subroutine write_visu_file_begin(dm, visuname, iter, opt_is_savg)
    use parameters_constant_mod, only: ICARTESIAN, ICYLINDRICAL
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    character(*),   intent(in) :: visuname
    integer,        intent(in) :: iter
    logical,        intent(in), optional :: opt_is_savg
    !
    integer :: nnode(3), n2node(3)
    integer :: u
    integer :: dir, n, npl, nst, nen
    character(len=256) :: xdmf_file, fullname
    character(len=256) :: g1d(3)
    !
    if (nrank /= 0) return
    !
    if(.not. present(opt_is_savg) .or. .not. opt_is_savg) then
    if(dm%visu_idim == Ivisu_3D .or. dm%visu_idim == Ivisu_3D2D) then
      call generate_pathfile_name(xdmf_file, dm%idom, visuname, dir_visu, 'xdmf', iter)
      open(newunit=u, file=trim(xdmf_file), status='replace', action='write')
      nnode = nnd_visu(1:3) 
      if (dm%icoordinate == ICARTESIAN) then
        call xdmf_begin_grid_cart(u, xdmf_file, nnode, grid_cart_3d_fl)
      else if (dm%icoordinate == ICYLINDRICAL) then
        call xdmf_begin_grid_cyl(u, xdmf_file, nnode, grid_cyl_3d_fl)
      end if
      close(u)
    end if
    end if
    !
    if (present(opt_is_savg) .and. opt_is_savg) then
      nen = 0
      nst = 0
    else
      nst = 1
      if (dm%visu_idim == Ivisu_2D .or. dm%visu_idim == Ivisu_3D2D) then
        nen = NSLICE
      else
        nen = 0
      end if
    end if

    do n = nst, nen
      do dir = 1, NDIM 
        if (n == 0 .and. dir /= nave_plane(dir)) then
          cycle 
        end if
        n2node = nnd_visu(1:3)
        n2node(dir) = 2
        npl = slice_idx(dir, n)
        fullname = trim(visuname)//'_'//slice_prefix(dir)//trim(int2str(npl))
        call generate_pathfile_name(xdmf_file, dm%idom, fullname, dir_visu, 'xdmf', iter)
        open(newunit=u, file=trim(xdmf_file), status='replace', action='write')
        if (dm%icoordinate == ICARTESIAN) then
          g1d = grid_cart_slice(:, dir, find_n_from_npl(dir,npl))
          call xdmf_begin_grid_cart(u, fullname, n2node, g1d)
        else
          call xdmf_begin_grid_cyl(u, fullname, n2node, grid_cyl_slice(dir, find_n_from_npl(dir,npl)))
        end if
        close(u)
      end do
    end do

    return
  end subroutine write_visu_file_begin
  !==========================================================================================================
  subroutine write_visu_file_end(dm, visuname, iter, opt_is_savg)
    use parameters_constant_mod, only: ICARTESIAN, ICYLINDRICAL
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    character(*),   intent(in) :: visuname
    integer,        intent(in) :: iter
    logical,        intent(in), optional :: opt_is_savg
    !
    integer :: nnode(3)
    integer :: u
    integer :: dir, n, npl, nen, nst
    character(len=256) :: xdmf_file, fullname
    !
    if (nrank /= 0) return
    !
    if(.not. present(opt_is_savg) .or. .not. opt_is_savg) then
    if(dm%visu_idim == Ivisu_3D .or. dm%visu_idim == Ivisu_3D2D) then
      call generate_pathfile_name(xdmf_file, dm%idom, visuname, dir_visu, 'xdmf', iter)
      open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
      call xdmf_end_grid(u)
      close(u)
    end if
    end if
    !
    if (present(opt_is_savg) .and. opt_is_savg) then
      nen = 0
      nst = 0
    else
      nst = 1
      if (dm%visu_idim == Ivisu_2D .or. dm%visu_idim == Ivisu_3D2D) then
        nen = NSLICE
      else
        nen = 0
      end if
    end if
    do n = nst, nen
      do dir = 1, NDIM 
        if (n == 0 .and. dir /= nave_plane(dir)) then
          cycle 
        end if
        npl = slice_idx(dir,n)
        fullname = trim(visuname)//'_'//slice_prefix(dir)//trim(int2str(npl))
        call generate_pathfile_name(xdmf_file, dm%idom, fullname, dir_visu, 'xdmf', iter)
        open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
        call xdmf_end_grid(u)
        close(u)
      end do
    end do
    return
  end subroutine write_visu_file_end
  !----------------------------- Core field writer --------------------------------
  !==========================================================================================================
  subroutine write_visu_field_bin_and_xdmf(dm, field_in, field_name, visuname, iter, direction, opt_ibc)
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP),        intent(in) :: field_in(:,:,:)
    character(*),    intent(in) :: field_name, visuname
    integer,         intent(in) :: iter
    integer,         intent(in) :: direction
    integer,         intent(in), optional :: opt_ibc(:)

    real(WP), allocatable :: accc(:,:,:)
    integer :: dir, n
    ! data transfer to cell-centered if needed
    allocate(accc(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)))
    if(direction /= N_DIRECTION) then
      call stagger_to_ccc(dm, field_in, accc, direction, opt_ibc)
    else
      accc = field_in
    end if 
    ! 
    if (dm%visu_idim == Ivisu_3D .or. dm%visu_idim == Ivisu_3D2D) then
      call write_visu_3d_binary_and_xdmf(dm, accc, field_name, visuname, iter)
    end if
    !
    if (dm%visu_idim == Ivisu_2D .or. dm%visu_idim == Ivisu_3D2D) then
      do dir = 1, NDIM
        do n = 1, NSLICE
          call write_visu_plane_binary_and_xdmf(dm, accc, field_name, visuname, dir, n, iter) 
        end do
      end do
    end if

    deallocate(accc)
  end subroutine write_visu_field_bin_and_xdmf
  !==========================================================================================================
  subroutine stagger_to_ccc(dm, fin, fout, direction, opt_ibc)
    use udf_type_mod
    use operations
    use decomp_2d
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in)  :: fin(:,:,:)
    real(WP), intent(out) :: fout(:,:,:)
    integer,  intent(in)  :: direction
    integer,  intent(in), optional :: opt_ibc(:)

    real(WP), allocatable :: acpc_ypencil(:,:,:), &
                             accc_ypencil(:,:,:), &
                             accp_ypencil(:,:,:), &
                             accp_zpencil(:,:,:), &
                             accc_zpencil(:,:,:)

    select case(direction)
    case (N_DIRECTION)
      fout = fin

    case (X_DIRECTION)
      call Get_x_midp_P2C_3D(fin, fout, dm, dm%iAccuracy, opt_ibc)

    case (Y_DIRECTION)
      allocate(acpc_ypencil(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)))
      allocate(accc_ypencil(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      call transpose_x_to_y(fin, acpc_ypencil, dm%dcpc)
      call Get_y_midp_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, opt_ibc)
      call transpose_y_to_x(accc_ypencil, fout, dm%dccc)
      deallocate(acpc_ypencil, accc_ypencil)

    case (Z_DIRECTION)
      allocate(accp_ypencil(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)))
      allocate(accp_zpencil(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)))
      allocate(accc_zpencil(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)))
      allocate(accc_ypencil(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      call transpose_x_to_y(fin, accp_ypencil, dm%dccp)
      call transpose_y_to_z(accp_ypencil, accp_zpencil, dm%dccp)
      call Get_z_midp_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, opt_ibc)
      call transpose_z_to_y(accc_zpencil, accc_ypencil, dm%dccc)
      call transpose_y_to_x(accc_ypencil, fout, dm%dccc)
      deallocate(accp_ypencil, accp_zpencil, accc_zpencil, accc_ypencil)

    case default
      call Print_error_msg("stagger_to_ccc: invalid direction")
      fout = fin
    end select
    return
  end subroutine stagger_to_ccc
  !==========================================================================================================
  subroutine write_visu_3d_binary_and_xdmf(dm, accc, field_name, visuname, iter)
    use udf_type_mod
    use decomp_2d
    use decomp_2d_io
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: accc(:,:,:)
    character(*), intent(in) :: field_name, visuname
    integer, intent(in) :: iter
    !
    character(len=256) :: bin_file
    character(len=256) :: xdmf_file
    character(len=64) :: dimstring
    integer :: u
    !----------------------- 3D binary -----------------------
    call generate_pathfile_name(bin_file, dm%idom, trim(field_name), dir_data, 'bin', iter)
    if (.not. file_exists(trim(bin_file))) then
      if (all(dm%visu_nskip(1:3) == 1)) then
        call write_one_3d_array(accc, trim(field_name), dm%idom, iter, dm%dccc)
      else
        call write_coarsened_3d(dm, accc, field_name, iter)
      end if
    end if
    !----------------------- 3D binary -----------------------
    if (nrank == 0) then
      call generate_pathfile_name(xdmf_file, dm%idom, visuname, dir_visu, 'xdmf', iter)
      open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
      dimstring = xdmf_dims_kji_string(ncl_visu(1:3))
      call xdmf_write_attribute_scalar(u, field_name, 'Cell', dimstring, bin_file)
      close(u)
    end if
    return
  end subroutine write_visu_3d_binary_and_xdmf
  !==========================================================================================================
  subroutine write_visu_plane_binary_and_xdmf(dm, accc, field_name, visuname, dir, n, iter)
    use udf_type_mod
    use decomp_2d
    use decomp_2d_io
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: accc(:,:,:)
    character(*), intent(in) :: field_name, visuname
    integer, intent(in) :: iter, dir, n

    character(len=256) :: data3d
    character(len=256) :: xdmf
    integer :: u
    integer :: ncell(3), nnode(3)
    integer :: npl
    character(len=256) :: bin_file
    character(len=256) :: slice_tag
    integer :: ncell2(3)

    nnode = nnd_visu(1:3)
    ncell = ncl_visu(1:3)

    !----------------------- 2D slices binary -----------------------
    npl = slice_idx(dir, n)
    slice_tag = slice_prefix(dir)//trim(int2str(npl))
    call generate_pathfile_name(bin_file, dm%idom, trim(field_name)//'_'//trim(slice_tag), dir_data, 'bin', iter)
    if (.not. file_exists(trim(bin_file))) then
      call write_plane_bin(dm, accc, dir, npl, bin_file)
    end if
    !----------------------- 2D slices XDMF -----------------------
    if (nrank == 0) then
      call write_slice_field_xdmf(dm, visuname, field_name, bin_file, dir, npl, iter)
    end if

  end subroutine write_visu_plane_binary_and_xdmf
  !==========================================================================================================
  pure function slice_prefix(dir) result(p)
    integer, intent(in) :: dir
    character(len=2) :: p
    select case(dir)
    case(1); p='xi'
    case(2); p='yi'
    case(3); p='zi'
    end select
  end function slice_prefix
  !==========================================================================================================
  subroutine write_coarsened_3d(dm, ccc, field_name, iter)
    use udf_type_mod
    use decomp_2d
    use decomp_2d_io
    use io_tools_mod
    use parameters_constant_mod, only: MAXP, IPENCIL
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: ccc(:,:,:)
    character(*), intent(in) :: field_name
    integer, intent(in) :: iter
    real(WP), allocatable :: coarse(:,:,:)

    allocate(coarse(xstV(1):xenV(1), xstV(2):xenV(2), xstV(3):xenV(3)))
    coarse = MAXP
    call fine_to_coarseV(IPENCIL(1), ccc, coarse)
    call write_one_3d_array(coarse, trim(field_name), dm%idom, iter, dm%dccc)
    deallocate(coarse)
  end subroutine write_coarsened_3d
  !==========================================================================================================
  subroutine write_plane_bin(dm, accc_in, dir, npl, bin_file)
    use udf_type_mod
    use decomp_2d_io
    use transpose_extended_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: accc_in(:,:,:)
    integer, intent(in) :: dir, npl
    character(*), intent(in) :: bin_file

    real(WP), allocatable :: accc_yp(:,:,:), accc_zp(:,:,:)
    real(WP), allocatable :: accc(:,:,:)

    select case(dir)
    case(1)
      allocate(accc(d1cc%xsz(1), d1cc%xsz(2), d1cc%xsz(3)))
      accc(1, :, :) = accc_in(npl, :, :)
      call decomp_2d_write_one(IPENCIL(1), accc, trim(bin_file), opt_decomp=d1cc)
      deallocate(accc)

    case(2)
      allocate(accc_yp(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      call transpose_x_to_y(accc_in, accc_yp, dm%dccc)
      allocate(accc(dc1c%ysz(1), dc1c%ysz(2), dc1c%ysz(3)))
      accc(:, 1, :) = accc_yp(:, npl, :)
      call decomp_2d_write_one(IPENCIL(2), accc, trim(bin_file), opt_decomp=dc1c)
      deallocate(accc, accc_yp)

    case(3)
      allocate(accc_yp(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      allocate(accc_zp(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)))
      call transpose_x_to_y(accc_in, accc_yp, dm%dccc)
      call transpose_y_to_z(accc_yp, accc_zp, dm%dccc)
      allocate(accc(dcc1%zsz(1), dcc1%zsz(2), dcc1%zsz(3)))
      accc(:, :, 1) = accc_zp(:, :, npl)
      call decomp_2d_write_one(IPENCIL(3), accc, trim(bin_file), opt_decomp=dcc1)
      deallocate(accc, accc_yp, accc_zp)
    end select

    return
  end subroutine write_plane_bin
  !==========================================================================================================
  subroutine write_slice_field_xdmf(dm, visuname, field_name, bin_file, dir, npl, iter)
    use udf_type_mod
    use parameters_constant_mod
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname, field_name, bin_file
    integer, intent(in) :: dir, npl, iter

    character(len=256) :: xdmf_file
    character(len=256) :: name
    integer :: u
    integer :: n2cell(3)
    character(len=64) :: dimstring

    name = trim(visuname)//'_'//slice_prefix(dir)//trim(int2str(npl))

    n2cell = ncl_visu(1:3)
    n2cell(dir) = 1   ! cells reduce by 1 in sliced direction (2 nodes -> 1 cell)

    call generate_pathfile_name(xdmf_file, dm%idom, name, dir_visu, 'xdmf', iter)
    open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
    dimstring = xdmf_dims_kji_string(n2cell)
    call xdmf_write_attribute_scalar(u, field_name, 'Cell', dimstring, bin_file)
    close(u)
  end subroutine write_slice_field_xdmf
  !==========================================================================================================
  pure function find_n_from_npl(dir, npl) result(nfound)
    integer, intent(in) :: dir, npl
    integer :: nfound, n
    nfound = 1
    do n = 1, NSLICE
      if (slice_idx(dir,n) == npl) then
        nfound = n
        return
      end if
    end do
  end function find_n_from_npl

end module visualisation_field_mod