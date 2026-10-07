
!==============================================================================
!  VISUALISATION I/O (mesh + fields) for XDMF + raw binary (.bin)
!  - Mesh: write once at start (or when mesh changes), with XDMF describing the grid structure.
!  - Fields: write at each output time, with XDMF describing the attribute and referencing the grid.
!  Key design: keep XDMF files human-readable and editable, with binary files for data. This allows easy post-processing and visualization with tools like ParaView.
!  Author: Wei Wang (2026-02)
!  Date: 2026-02
!==============================================================================

!> Low-level XDMF writer helpers for CHAPSim visualisation output.
!>
!> This module writes the XML fragments that describe Cartesian rectilinear
!> grids, cylindrical curvilinear grids, scalar attributes, and binary data
!> references for ParaView/VisIt-compatible XDMF files.
module visualisation_xdmf_io_mod
  use iso_fortran_env, only: int32, int64
  use parameters_constant_mod, only: WP, NDIM
  implicit none
  private

  private :: xdmf_seek_bytes_int32
  private :: xdmf_seek_bytes_int64
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
  pure function xdmf_seek_bytes_int64(nbytes) result(seekstr)
    integer(int64), intent(in) :: nbytes
    character(len=32) :: seekstr
    write(seekstr,'(I0)') nbytes
  end function xdmf_seek_bytes_int64
  !========================================================================================================
  !> Convert CHAPSim `i,j,k` dimensions to the XDMF `k,j,i` ordering.
  !> - nijk (in): Dimensions in solver order.
  !> Return: Dimension string used by XDMF topology and attribute records.
  pure function xdmf_dims_kji_string(nijk) result(s)
    integer, intent(in) :: nijk(NDIM)
    character(len=64) :: s
    write(s,'(I0,1X,I0,1X,I0)') nijk(3), nijk(2), nijk(1)
  end function xdmf_dims_kji_string
  !========================================================================================================
  !> Start an XDMF rectilinear Cartesian grid.
  !> - unit (in): Open Fortran unit for the XDMF file.
  !> - grid_name (in): Name of the XDMF grid.
  !> - nnode (in): Node counts in solver order.
  !> - grid_files_1d (in): Binary coordinate files for x, y, and z.
  subroutine xdmf_begin_grid_cart(unit, grid_name, nnode, grid_files_1d, opt_fragment)
    use typeconvert_mod, only: int2str
    integer, intent(in) :: unit
    character(*), intent(in) :: grid_name
    integer, intent(in) :: nnode(NDIM)
    character(*), intent(in) :: grid_files_1d(NDIM) ! x,y,z 1D files
    logical, intent(in), optional :: opt_fragment

    character(len=64) :: dims
    logical :: is_fragment

    dims = xdmf_dims_kji_string(nnode)
    is_fragment = .false.
    if(present(opt_fragment)) is_fragment = opt_fragment

    if(.not. is_fragment) then
      write(unit,'(A)') '<?xml version="1.0" ?>'
      write(unit,'(A)') '<Xdmf Version="3.0">'
      write(unit,'(A)') '  <Domain>'
    end if
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
    write(unit,'(A)') '          '//trim(filename)
    write(unit,'(A)') '        </DataItem>'
  end subroutine xdmf_write_dataitem_1d
  !========================================================================================================
  !> Start an XDMF curvilinear grid for cylindrical cases.
  !> - unit (in): Open Fortran unit for the XDMF file.
  !> - grid_name (in): Name of the XDMF grid.
  !> - nnode (in): Node counts in solver order.
  !> - grid_file (in): Binary XYZ coordinate file.
  subroutine xdmf_begin_grid_cyl(unit, grid_name, nnode, grid_file, opt_fragment)
    use typeconvert_mod, only: int2str
    integer, intent(in) :: unit
    character(*), intent(in) :: grid_name
    integer, intent(in) :: nnode(NDIM)
    character(*), intent(in) :: grid_file
    logical, intent(in), optional :: opt_fragment

    integer :: npts
    character(len=64) :: topo_dims
    character(len=64) :: geom_dims
    character(len=32) :: seek
    logical :: is_fragment

    topo_dims = xdmf_dims_kji_string(nnode)
    npts = nnode(1)*nnode(2)*nnode(3)
    is_fragment = .false.
    if(present(opt_fragment)) is_fragment = opt_fragment

    ! Cylindrical XYZ bin format: [int32 nx,ny,nz][ (x,y,z) as real*8 repeated ]
    ! Use little-endian for ParaView/XDMF binary compatibility.
    ! Seek = 3 int32 header = 12 bytes typically.
    seek = xdmf_seek_bytes_int32(3)
    write(geom_dims,'(I0,1X,I0)') npts, 3

    if(.not. is_fragment) then
      write(unit,'(A)') '<?xml version="1.0" ?>'
      write(unit,'(A)') '<Xdmf Version="3.0">'
      write(unit,'(A)') '  <Domain>'
    end if
    write(unit,'(A)') '    <Grid Name="'//trim(grid_name)//'" GridType="Uniform">'
    write(unit,'(A)') '      <Topology TopologyType="3DSMesh" Dimensions="'//trim(topo_dims)//'"/>'
    write(unit,'(A)') '      <Geometry GeometryType="XYZ">'
    write(unit,'(A)') '        <DataItem ItemType="Uniform"'
    write(unit,'(A)') '                  Dimensions="'//trim(geom_dims)//'"'
    write(unit,'(A)') '                  NumberType="Float"'
    write(unit,'(A)') '                  Precision="8"'
    write(unit,'(A)') '                  Format="Binary"'
    write(unit,'(A)') '                  Endian="Little"'
    write(unit,'(A)') '                  Seek="'//trim(seek)//'">'
    write(unit,'(A)') '          '//trim(grid_file)
    write(unit,'(A)') '        </DataItem>'
    write(unit,'(A)') '      </Geometry>'
  end subroutine xdmf_begin_grid_cyl
  !========================================================================================================
  !> Add a scalar field attribute to the current XDMF grid.
  !> - unit (in): Open Fortran unit for the XDMF file.
  !> - name (in): Attribute name shown in visualisation tools.
  !> - center (in): XDMF centering, typically `Cell` or `Node`.
  !> - dims_kji (in): Field dimensions in XDMF order.
  !> - data_file_rel (in): Relative binary data path referenced by the XDMF file.
  subroutine xdmf_write_attribute_scalar(unit, name, center, dims_kji, data_file_rel, opt_precision, opt_seek_bytes)
    use typeconvert_mod, only: int2str
    integer, intent(in) :: unit
    character(*), intent(in) :: name, center, dims_kji, data_file_rel
    integer, intent(in), optional :: opt_precision
    integer(int64), intent(in), optional :: opt_seek_bytes

    integer :: precision

    precision = 8
    if(present(opt_precision)) precision = opt_precision
    write(unit,'(A)') '      <Attribute Name="'//trim(name)//'" AttributeType="Scalar" Center="'//trim(center)//'">'
    write(unit,'(A)') '        <DataItem ItemType="Uniform"'
    write(unit,'(A)') '                  NumberType="Float"'
    write(unit,'(A)') '                  Precision="'//trim(int2str(precision))//'"'
    write(unit,'(A)') '                  Format="Binary"'
    if(present(opt_seek_bytes)) &
      write(unit,'(A)') '                  Seek="'//trim(xdmf_seek_bytes_int64(opt_seek_bytes))//'"'
    write(unit,'(A)') '                  Dimensions="'//trim(dims_kji)//'">'
    write(unit,'(A)') '          '//trim(data_file_rel)
    write(unit,'(A)') '        </DataItem>'
    write(unit,'(A)') '      </Attribute>'
  end subroutine xdmf_write_attribute_scalar
  !========================================================================================================
  !> Close the current XDMF grid, domain, and document.
  !> - unit (in): Open Fortran unit for the XDMF file.
  subroutine xdmf_end_grid(unit, opt_fragment)
    integer, intent(in) :: unit
    logical, intent(in), optional :: opt_fragment

    logical :: is_fragment

    is_fragment = .false.
    if(present(opt_fragment)) is_fragment = opt_fragment

    write(unit,'(A)') '    </Grid>'
    if(.not. is_fragment) then
      write(unit,'(A)') '  </Domain>'
      write(unit,'(A)') '</Xdmf>'
    end if
  end subroutine xdmf_end_grid

end module visualisation_xdmf_io_mod
!========================================================================================================
!========================================================================================================
!> Mesh-file generation for visualisation output.
!>
!> Writes Cartesian one-dimensional coordinate files or cylindrical XYZ grids
!> and the XDMF mesh descriptors used by field-output routines.
module visualisation_mesh_mod
  use io_tools_mod
  use iso_fortran_env, only: int32
  use parameters_constant_mod
  use visualisation_xdmf_io_mod
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
  !> Initialise visualisation mesh files and slice metadata.
  !> - dm (in): Domain descriptor containing mesh, decomposition, and geometry metadata.
  subroutine write_visu_ini(dm)
    use parameters_constant_mod, only: ICARTESIAN, ICYLINDRICAL
    use udf_type_mod
    !use decomp_2d, only: xszV, yszV, zszV
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
      if(.not. is_IO_off) call write_mesh_cartesian(dm, nnd_visu, x1, y1, z1)
      deallocate(x1, y1, z1)

    case (ICYLINDRICAL)
      allocate(x3(nnd_visu(1),nnd_visu(2),nnd_visu(3)))
      allocate(y3(nnd_visu(1),nnd_visu(2),nnd_visu(3)))
      allocate(z3(nnd_visu(1),nnd_visu(2),nnd_visu(3)))
      call build_cylindrical_to_cart(dm, x3, y3, z3)
      if(.not. is_IO_off) call write_mesh_cylindrical(dm, nnd_visu, x3, y3, z3)
      deallocate(x3, y3, z3)

    case default
      error stop "write_visu_ini: unknown coordinate system"
    end select

  end subroutine write_visu_ini
  !========================================================================================================
  subroutine compute_visu_nnode(dm, nnode)
    use udf_type_mod
    !use decomp_2d, only: xszV, yszV, zszV
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(out) :: nnode(NDIM)

    !if (any(dm%visu_nskip(1:3) > 1)) then
    !  nnode = [ xszV(1), yszV(2), zszV(3) ]  ! your existing convention
    !else
      nnode = dm%np_geo(1:3)
    !end if
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
    use parameters_constant_mod, only: MAXP
    use udf_type_mod
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
    use math_mod
    use parameters_constant_mod, only: MAXP
    use udf_type_mod
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

          ! Convert (r,theta) -> (y,z); keep x as axial. theta sweeps from +y
          ! towards +z so that (e_x, e_r, e_theta) is right-handed, i.e.
          ! e_x x e_r = e_theta. Every cross product and curl in the solver -
          ! u x B, j x B, the vorticity tensor - uses the right-handed formula
          ! c_1 = a_2 b_3 - a_3 b_2 on the (x, r, theta) index triple, so the
          ! mapping has to supply a right-handed triad or those expressions all
          ! carry a hidden global minus sign. The same convention fixes the
          ! Cartesian-to-cylindrical decomposition of B_static (eq_mhd) and of
          ! gravity (eq_momentum2); the three must agree.
          y(i,j,k) = r * cos_wp(th)
          z(i,j,k) = r * sin_wp(th)
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
    use typeconvert_mod, only: int2str
    use udf_type_mod
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
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: nnode(NDIM)
    real(WP), intent(in) :: x(:,:,:), y(:,:,:), z(:,:,:)

    integer :: dir, n2(NDIM), npl, n, nen
    character(len=256) :: grid_name
    character(len=256) :: fxyz

    ! 3D curvilinear mesh: one XYZ binary
    grid_name = 'grids_3d'
    call generate_pathfile_name(fxyz, dm%idom, grid_name, dir_visu_mesh, 'bin')
    call write_bin_cyl_xyz(fxyz, x, y, z, nnode)
    grid_cyl_3d_fl = visu_xdmf_relative_path(fxyz)
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
          call generate_pathfile_name(fxyz, dm%idom, grid_name, dir_visu_mesh, 'bin')
          call write_bin_cyl_xyz(fxyz, x(npl:npl+1,:,:), y(npl:npl+1,:,:), z(npl:npl+1,:,:), n2)

        case(2)
          grid_name = 'grid_yi'//trim(int2str(npl))
          call generate_pathfile_name(fxyz, dm%idom, grid_name, dir_visu_mesh, 'bin')
          call write_bin_cyl_xyz(fxyz, x(:,npl:npl+1,:), y(:,npl:npl+1,:), z(:,npl:npl+1,:), n2)

        case(3)
          grid_name = 'grid_zi'//trim(int2str(npl))
          call generate_pathfile_name(fxyz, dm%idom, grid_name, dir_visu_mesh, 'bin')
          call write_bin_cyl_xyz(fxyz, x(:,:,npl:npl+1), y(:,:,npl:npl+1), z(:,:,npl:npl+1), n2)
        end select

        grid_cyl_slice(dir, n) = visu_xdmf_relative_path(fxyz)
        call write_xdmf_mesh_cyl(dm%idom, grid_name, n2, grid_cyl_slice(dir, n))
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
    character(len=256) :: filename

    call generate_pathfile_name(filename, idom, keyword, dir_visu_mesh, 'bin')

    open(newunit=u, file=trim(filename), access='stream', form='unformatted', &
         status='replace', action='write', iostat=ios)
    if (ios /= 0) error stop 'write_bin_cart_1d: cannot open file'

    write(u) int(size(a), int32)
    write(u) a
    close(u)
    filename_out = visu_xdmf_relative_path(filename)
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
         status='replace', action='write', iostat=ios, convert='LITTLE_ENDIAN')
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

    call generate_pathfile_name(xdmf_file, idom, visuname, dir_visu_xdmf, 'xdmf', 0)
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

    call generate_pathfile_name(xdmf, idom, grid_name, dir_visu_xdmf, 'xdmf', 0)

    open(newunit=unit, file=trim(xdmf), status='replace', action='write')
    call xdmf_begin_grid_cyl(unit, grid_name, nnode, grid_file)
    call xdmf_end_grid(unit)
    close(unit)
  end subroutine write_xdmf_mesh_cyl

end module visualisation_mesh_mod
!==============================================================================
!  FIELD OUTPUT: write binary (3D + slices) then append XDMF attributes into the matching XDMF files.
!  Key change vs your current version:
!    - Mesh XDMF files are created once by visualisation_mesh_mod.
!    - Field XDMF writing *appends* <Attribute> blocks into those files before </Grid>.
!      (We do this by writing a field-xdmf file that *re-declares the grid* OR by
!       having separate XDMF per field. Below keeps your original pattern: one XDMF per field,
!       with grid + attributes in that file. It’s consistent and easy.)
!==============================================================================
!> High-level visualisation output for flow, thermal, MHD, and generic fields.
!>
!> This module writes binary field data and matching XDMF descriptors for full
!> three-dimensional outputs and configured plane/slice outputs.
module visualisation_field_mod
  use decomp_2d_io
  use decomp_2d_io_object_mpi
  use io_tools_mod
  use iso_fortran_env, only: int64
  use parameters_constant_mod, only: WP, NDIM, VISU_PRECISION_SINGLE, RESTART_LAYOUT_BUNDLED
  use visualisation_mesh_mod
  use visualisation_xdmf_io_mod
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
  !private :: write_coarsened_3d
  private :: is_visu_single_precision
  private :: write_plane_bin
  private :: write_plane_to_slice_bundle
  private :: write_slice_field_xdmf
  private :: write_visu_3d_binary_and_xdmf
  private :: begin_visu_bundle
  private :: end_visu_bundle
  private :: begin_visu_slice_bundle
  private :: end_visu_slice_bundle
  private :: visu_field_bytes
  private :: visu_slice_field_bytes
  private :: visu_source_location
  private :: append_visu_slice_xdmf_record
  private :: cleanup_visu_slice_bundle_wrapper_xdmf
  private :: cleanup_visu_slice_per_file_xdmf
  private :: reset_visu_slice_xdmf_records
  private :: visu_slice_bundle_plane_name
  private :: write_visu_slice_bundle_grid
  private :: write_visu_slice_bundle_wrapper_xdmfs
  private :: write_visu_slice_bundle_xdmf
  private :: write_visu_slice_metadata_fields
  private :: write_constant_plane_to_slice_bundle
  private :: cleanup_visu_slice_map_file

  type :: t_visu_slice_xdmf_record
    character(len=128) :: field_name = ''
    character(len=256) :: bin_ref = ''
    integer :: dir = 0
    integer :: npl = 0
    integer :: precision = 8
    integer(int64) :: seek_bytes = 0_int64
  end type t_visu_slice_xdmf_record

  type(d2d_io_mpi) :: visu_bundle_io
  logical :: visu_bundle_active = .false.
  logical :: visu_bundle_do_write = .false.
  character(len=256) :: visu_bundle_file = ''
  character(len=256) :: visu_bundle_ref = ''
  character(len=256) :: visu_bundle_meta_file = ''
  integer :: visu_bundle_meta_unit = -1
  integer(int64) :: visu_bundle_offset_bytes = 0_int64

  type(d2d_io_mpi) :: visu_slice_bundle_io
  logical :: visu_slice_bundle_active = .false.
  logical :: visu_slice_bundle_do_write = .false.
  character(len=256) :: visu_slice_bundle_file = ''
  character(len=256) :: visu_slice_bundle_ref = ''
  character(len=256) :: visu_slice_bundle_meta_file = ''
  character(len=256) :: visu_slice_bundle_xdmf_file = ''
  integer :: visu_slice_bundle_meta_unit = -1
  integer(int64) :: visu_slice_bundle_offset_bytes = 0_int64
  logical :: visu_slice_bundle_is_savg = .false.
  integer :: visu_slice_bundle_savg_dir = 0
  integer :: visu_slice_bundle_savg_npl = 0
  type(t_visu_slice_xdmf_record), allocatable :: visu_slice_xdmf_records(:)
  integer :: visu_slice_xdmf_record_count = 0

contains

  !----------------------------- High-level drivers --------------------------------
  !==============================================================================
  !> Write flow variables to visualisation files.
  !> - fl (in): Flow state.
  !> - dm (in): Domain descriptor.
  !> - suffix (in): Optional suffix appended to the visualisation file name.
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
    call write_visu_field_bin_and_xdmf(dm, fl%pres, 'pr', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_pr', opt_reuse_bin_name='pr')
    call write_visu_field_bin_and_xdmf(dm, fl%pcor, 'phi', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_phi')

    call write_visu_field_bin_and_xdmf(dm, fl%qx, 'qx_ccc', visuname, iter, X_DIRECTION, &
                                       opt_ibc=dm%ibcx_qx, opt_bin_name='visu_qx')
    call write_visu_field_bin_and_xdmf(dm, fl%qy, 'qy_ccc', visuname, iter, Y_DIRECTION, &
                                       opt_ibc=dm%ibcy_qy, opt_bin_name='visu_qy')
    call write_visu_field_bin_and_xdmf(dm, fl%qz, 'qz_ccc', visuname, iter, Z_DIRECTION, &
                                       opt_ibc=dm%ibcz_qz, opt_bin_name='visu_qz')

    if (dm%is_thermo) then
      call write_visu_field_bin_and_xdmf(dm, fl%gx, 'gx_ccc', visuname, iter, X_DIRECTION, &
                                         opt_ibc=dm%ibcx_qx, opt_bin_name='visu_gx')
      call write_visu_field_bin_and_xdmf(dm, fl%gy, 'gy_ccc', visuname, iter, Y_DIRECTION, &
                                         opt_ibc=dm%ibcy_qy, opt_bin_name='visu_gy')
      call write_visu_field_bin_and_xdmf(dm, fl%gz, 'gz_ccc', visuname, iter, Z_DIRECTION, &
                                         opt_ibc=dm%ibcz_qz, opt_bin_name='visu_gz')
    end if
    call write_visu_file_end(dm, visuname, iter)
    return
  end subroutine write_visu_flow
  !==============================================================================
  !> Write thermal and variable-property fields to visualisation files.
  !> - tm (in): Thermal state.
  !> - fl (in): Flow state containing density and viscosity.
  !> - dm (in): Domain descriptor.
  !> - suffix (in): Optional suffix appended to the visualisation file name.
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
    call write_visu_field_bin_and_xdmf(dm, tm%tTemp, 'Temperature',  visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_Temperature', opt_reuse_bin_name='temp')
    call write_visu_field_bin_and_xdmf(dm, fl%dDens, 'Density', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_Density')
    call write_visu_field_bin_and_xdmf(dm, fl%mVisc, 'Viscosity', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_Viscosity')
    call write_visu_field_bin_and_xdmf(dm, tm%kCond, 'Thermal_conductivity', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_Thermal_conductivity')
    call write_visu_field_bin_and_xdmf(dm, tm%eCond, 'Electrical_conductivity', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_Electrical_conductivity')
    call write_visu_field_bin_and_xdmf(dm, tm%hEnth, 'Enthalpy', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_Enthalpy')
    call write_visu_field_bin_and_xdmf(dm, fl%drhodt, 'drho_dt', visuname, iter, N_DIRECTION, &
                                       opt_bin_name='visu_drho_dt')
    call write_visu_file_end(dm, visuname, iter)
    return
  end subroutine write_visu_thermo
  !==============================================================================
  !> Write MHD fields and Lorentz-force components to visualisation files.
  !> - mh (in): MHD state.
  !> - fl (in): Flow state containing Lorentz-force arrays.
  !> - dm (in): Domain descriptor.
  !> - suffix (in): Optional suffix appended to the visualisation file name.
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
  !==============================================================================
  !> Write an arbitrary three-dimensional field using its decomposition metadata.
  !> - var (in): Field data.
  !> - varname (in): Field name used in output files.
  !> - visuname (in): Visualisation group/file stem.
  !> - dtmp (in): Decomposition descriptor for `var`.
  !> - dm (in): Domain descriptor.
  !> - iter (in): Iteration number used in output names.
  subroutine write_visu_any3darray(var, varname, visuname, dtmp, dm, iter)
    use decomp_operation_mod
    use udf_type_mod
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
  !==============================================================================
  subroutine begin_visu_bundle(dm, visuname, iter, is_savg)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*),   intent(in) :: visuname
    integer,        intent(in) :: iter
    logical,        intent(in) :: is_savg

    character(256) :: output_files(2)

    visu_bundle_active = .false.
    visu_bundle_do_write = .false.
    visu_bundle_file = ''
    visu_bundle_ref = ''
    visu_bundle_meta_file = ''
    visu_bundle_meta_unit = -1
    visu_bundle_offset_bytes = 0_int64

    if(is_savg) return
    if(.not. (dm%visu_idim == Ivisu_3D .or. dm%visu_idim == Ivisu_3D2D)) return

    call generate_pathfile_name(output_files(1), dm%idom, trim(visuname)//'_visu', dir_visu_data, 'bin', iter)
    call generate_pathfile_name(output_files(2), dm%idom, trim(visuname)//'_visu_meta', dir_visu_data, 'dat', iter)

    if(dm%restart_data_layout_write /= RESTART_LAYOUT_BUNDLED) then
      call remove_output_file_if_overwrite(output_files(1), dm%existing_output_policy)
      call remove_output_file_if_overwrite(output_files(2), dm%existing_output_policy)
      return
    end if

    call prepare_output_file_set(output_files, dm%existing_output_policy, trim(visuname)//' visualisation bundle', &
                                 visu_bundle_do_write)

    visu_bundle_active = .true.
    visu_bundle_file = output_files(1)
    visu_bundle_ref = visu_xdmf_relative_path(visu_bundle_file)
    visu_bundle_meta_file = output_files(2)

    if(visu_bundle_do_write) then
      if(nrank == 0) call Print_debug_mid_msg('Writing '//trim(visu_bundle_file))
      call visu_bundle_io%open(trim(visu_bundle_file), decomp_2d_write_mode)
      if(nrank == 0) then
        open(newunit=visu_bundle_meta_unit, file=trim(visu_bundle_meta_file), status='replace', action='write')
        write(visu_bundle_meta_unit, '(A)') 'CHAPSim_visu_bundle_v1'
        write(visu_bundle_meta_unit, '(A,1X,A)') 'group', trim(visuname)
        write(visu_bundle_meta_unit, '(A,1X,I0)') 'domain', dm%idom
        write(visu_bundle_meta_unit, '(A,1X,I0)') 'iter', iter
        write(visu_bundle_meta_unit, '(A,1X,I0)') 'precision_bytes', dm%visu_precision
        write(visu_bundle_meta_unit, '(A)') &
          'fields name original_file source center dimensions_kji precision_bytes offset_bytes nbytes'
      end if
    end if

    return
  end subroutine begin_visu_bundle
  !==============================================================================
  subroutine end_visu_bundle()
    implicit none

    if(visu_bundle_active .and. visu_bundle_do_write) then
      call visu_bundle_io%close()
      if(nrank == 0 .and. visu_bundle_meta_unit /= -1) close(visu_bundle_meta_unit)
    end if

    visu_bundle_active = .false.
    visu_bundle_do_write = .false.
    visu_bundle_file = ''
    visu_bundle_ref = ''
    visu_bundle_meta_file = ''
    visu_bundle_meta_unit = -1
    visu_bundle_offset_bytes = 0_int64

    return
  end subroutine end_visu_bundle
  !==============================================================================
  subroutine begin_visu_slice_bundle(dm, visuname, iter, is_savg)
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*),   intent(in) :: visuname
    integer,        intent(in) :: iter
    logical,        intent(in) :: is_savg

    character(256) :: output_files(2)
    character(len=16) :: slice_tag
    integer :: dir, savg_dir, savg_npl

    visu_slice_bundle_active = .false.
    visu_slice_bundle_do_write = .false.
    visu_slice_bundle_file = ''
    visu_slice_bundle_ref = ''
    visu_slice_bundle_meta_file = ''
    visu_slice_bundle_xdmf_file = ''
    visu_slice_bundle_meta_unit = -1
    visu_slice_bundle_offset_bytes = 0_int64
    visu_slice_bundle_is_savg = .false.
    visu_slice_bundle_savg_dir = 0
    visu_slice_bundle_savg_npl = 0
    call reset_visu_slice_xdmf_records()

    if(is_savg) then
      if(count(dm%is_periodic(1:3)) /= 1) return
      savg_dir = 0
      do dir = 1, NDIM
        if(dm%is_periodic(dir)) savg_dir = dir
      end do
      if(savg_dir == 0) return
      savg_npl = slice_idx(savg_dir, 0)
      slice_tag = slice_prefix(savg_dir)//trim(int2str(savg_npl))
      call generate_pathfile_name(output_files(1), dm%idom, trim(visuname)//'_'//trim(slice_tag), &
                                  dir_visu_data, 'bin', iter)
      call generate_pathfile_name(output_files(2), dm%idom, trim(visuname)//'_'//trim(slice_tag)//'_meta', &
                                  dir_visu_data, 'dat', iter)
      call generate_pathfile_name(visu_slice_bundle_xdmf_file, dm%idom, trim(visuname)//'_'//trim(slice_tag), &
                                  dir_visu_xdmf, 'xdmf', iter)
      visu_slice_bundle_is_savg = .true.
      visu_slice_bundle_savg_dir = savg_dir
      visu_slice_bundle_savg_npl = savg_npl
    else
      if(.not. (dm%visu_idim == Ivisu_2D .or. dm%visu_idim == Ivisu_3D2D)) return
      call generate_pathfile_name(output_files(1), dm%idom, trim(visuname)//'_slices_visu', dir_visu_data, 'bin', iter)
      call generate_pathfile_name(output_files(2), dm%idom, trim(visuname)//'_slices_visu_meta', dir_visu_data, 'dat', iter)
      call generate_pathfile_name(visu_slice_bundle_xdmf_file, dm%idom, trim(visuname)//'_slices_visu', &
                                  dir_visu_xdmf, 'xdmf', iter)
    end if

    if(dm%restart_data_layout_write /= RESTART_LAYOUT_BUNDLED) then
      call remove_output_file_if_overwrite(output_files(1), dm%existing_output_policy)
      call remove_output_file_if_overwrite(output_files(2), dm%existing_output_policy)
      call remove_output_file_if_overwrite(visu_slice_bundle_xdmf_file, dm%existing_output_policy)
      if(.not. is_savg) then
        call cleanup_visu_slice_bundle_wrapper_xdmf(dm, visuname, iter)
        call cleanup_visu_slice_map_file()
      end if
      return
    end if

    call prepare_output_file_set(output_files, dm%existing_output_policy, trim(visuname)//' slice visualisation bundle', &
                                 visu_slice_bundle_do_write)

    visu_slice_bundle_active = .true.
    visu_slice_bundle_file = output_files(1)
    visu_slice_bundle_ref = visu_xdmf_relative_path(visu_slice_bundle_file)
    visu_slice_bundle_meta_file = output_files(2)

    if(visu_slice_bundle_do_write) then
      if(.not. is_savg) then
        call cleanup_visu_slice_per_file_xdmf(dm, visuname, iter)
        call cleanup_visu_slice_map_file()
      end if
      if(nrank == 0) call Print_debug_mid_msg('Writing '//trim(visu_slice_bundle_file))
      call visu_slice_bundle_io%open(trim(visu_slice_bundle_file), decomp_2d_write_mode)
      if(nrank == 0) then
        open(newunit=visu_slice_bundle_meta_unit, file=trim(visu_slice_bundle_meta_file), &
             status='replace', action='write')
        write(visu_slice_bundle_meta_unit, '(A)') 'CHAPSim_visu_slice_bundle_v1'
        write(visu_slice_bundle_meta_unit, '(A,1X,A)') 'group', trim(visuname)
        write(visu_slice_bundle_meta_unit, '(A,1X,I0)') 'domain', dm%idom
        write(visu_slice_bundle_meta_unit, '(A,1X,I0)') 'iter', iter
        write(visu_slice_bundle_meta_unit, '(A,1X,I0)') 'precision_bytes', dm%visu_precision
        write(visu_slice_bundle_meta_unit, '(A)') &
          'fields name original_file slice dimensions_kji precision_bytes offset_bytes nbytes'
      end if
    end if

    return
  end subroutine begin_visu_slice_bundle
  !==============================================================================
  subroutine end_visu_slice_bundle()
    implicit none

    if(visu_slice_bundle_active .and. visu_slice_bundle_do_write) then
      call visu_slice_bundle_io%close()
      if(nrank == 0 .and. visu_slice_bundle_meta_unit /= -1) close(visu_slice_bundle_meta_unit)
    end if

    visu_slice_bundle_active = .false.
    visu_slice_bundle_do_write = .false.
    visu_slice_bundle_file = ''
    visu_slice_bundle_ref = ''
    visu_slice_bundle_meta_file = ''
    visu_slice_bundle_xdmf_file = ''
    visu_slice_bundle_meta_unit = -1
    visu_slice_bundle_offset_bytes = 0_int64
    visu_slice_bundle_is_savg = .false.
    visu_slice_bundle_savg_dir = 0
    visu_slice_bundle_savg_npl = 0
    call reset_visu_slice_xdmf_records()

    return
  end subroutine end_visu_slice_bundle
  !==============================================================================
  subroutine reset_visu_slice_xdmf_records()
    implicit none

    if(allocated(visu_slice_xdmf_records)) deallocate(visu_slice_xdmf_records)
    visu_slice_xdmf_record_count = 0

    return
  end subroutine reset_visu_slice_xdmf_records
  !==============================================================================
  subroutine append_visu_slice_xdmf_record(field_name, bin_ref, dir, npl, precision, seek_bytes)
    implicit none
    character(*), intent(in) :: field_name
    character(*), intent(in) :: bin_ref
    integer, intent(in) :: dir, npl, precision
    integer(int64), intent(in) :: seek_bytes

    type(t_visu_slice_xdmf_record), allocatable :: records_tmp(:)
    integer :: nnew

    nnew = visu_slice_xdmf_record_count + 1
    allocate(records_tmp(nnew))
    if(visu_slice_xdmf_record_count > 0) &
      records_tmp(1:visu_slice_xdmf_record_count) = visu_slice_xdmf_records(1:visu_slice_xdmf_record_count)

    records_tmp(nnew)%field_name = trim(field_name)
    records_tmp(nnew)%bin_ref = trim(bin_ref)
    records_tmp(nnew)%dir = dir
    records_tmp(nnew)%npl = npl
    records_tmp(nnew)%precision = precision
    records_tmp(nnew)%seek_bytes = seek_bytes

    call move_alloc(records_tmp, visu_slice_xdmf_records)
    visu_slice_xdmf_record_count = nnew

    return
  end subroutine append_visu_slice_xdmf_record
  !==============================================================================
  subroutine cleanup_visu_slice_per_file_xdmf(dm, visuname, iter)
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname
    integer, intent(in) :: iter

    character(len=256) :: xdmf_file
    character(len=256) :: fullname
    integer :: dir, n, npl

    do n = 1, NSLICE
      do dir = 1, NDIM
        npl = slice_idx(dir, n)
        fullname = trim(visuname)//'_'//slice_prefix(dir)//trim(int2str(npl))
        call generate_pathfile_name(xdmf_file, dm%idom, fullname, dir_visu_xdmf, 'xdmf', iter)
        call remove_output_file_if_overwrite(xdmf_file, dm%existing_output_policy)
      end do
    end do

    return
  end subroutine cleanup_visu_slice_per_file_xdmf
  !==============================================================================
  subroutine cleanup_visu_slice_bundle_wrapper_xdmf(dm, visuname, iter)
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname
    integer, intent(in) :: iter

    character(len=256) :: plane_name
    character(len=256) :: xdmf_file
    integer :: dir, n, npl

    do n = 1, NSLICE
      do dir = 1, NDIM
        npl = slice_idx(dir, n)
        call visu_slice_bundle_plane_name(dm, visuname, dir, n, npl, iter, plane_name)
        xdmf_file = trim(dir_visu_xdmf)//'/'//trim(plane_name)//'.xdmf'
        call remove_output_file_if_overwrite(xdmf_file, dm%existing_output_policy)
      end do
    end do

    return
  end subroutine cleanup_visu_slice_bundle_wrapper_xdmf
  !==============================================================================
  subroutine visu_slice_bundle_plane_name(dm, visuname, dir, n, npl, iter, plane_name)
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname
    integer, intent(in) :: dir, n, npl, iter
    character(*), intent(out) :: plane_name

    character(len=1) :: axis_name

    select case(dir)
    case(1)
      axis_name = 'x'
    case(2)
      axis_name = 'y'
    case(3)
      axis_name = 'z'
    end select

    plane_name = 'domain'//trim(int2str(dm%idom))//'_'//trim(visuname)//'_'//axis_name// &
                 '_slice'//trim(int2str(n))//'_'//slice_prefix(dir)//trim(int2str(npl))// &
                 '_iter'//trim(int2str(iter))

    return
  end subroutine visu_slice_bundle_plane_name
  !==============================================================================
  subroutine write_visu_slice_bundle_grid(u, dm, grid_name, dir, npl, opt_fragment)
    use parameters_constant_mod, only: ICARTESIAN, ICYLINDRICAL
    use udf_type_mod
    implicit none
    integer, intent(in) :: u
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: grid_name
    integer, intent(in) :: dir, npl
    logical, intent(in), optional :: opt_fragment

    character(len=256) :: g1d(3)
    character(len=64) :: dimstring
    integer :: irec
    integer :: n2node(3), n2cell(3)
    logical :: is_fragment

    is_fragment = .false.
    if(present(opt_fragment)) is_fragment = opt_fragment

    n2node = nnd_visu(1:3)
    n2node(dir) = 2

    if (dm%icoordinate == ICARTESIAN) then
      g1d = grid_cart_slice(:, dir, find_n_from_npl(dir,npl))
      call xdmf_begin_grid_cart(u, grid_name, n2node, g1d, opt_fragment=is_fragment)
    else if (dm%icoordinate == ICYLINDRICAL) then
      call xdmf_begin_grid_cyl(u, grid_name, n2node, grid_cyl_slice(dir, find_n_from_npl(dir,npl)), &
                              opt_fragment=is_fragment)
    end if

    n2cell = ncl_visu(1:3)
    n2cell(dir) = 1
    dimstring = xdmf_dims_kji_string(n2cell)
    do irec = 1, visu_slice_xdmf_record_count
      if(visu_slice_xdmf_records(irec)%dir == dir .and. visu_slice_xdmf_records(irec)%npl == npl) then
        call xdmf_write_attribute_scalar(u, trim(visu_slice_xdmf_records(irec)%field_name), 'Cell', dimstring, &
                                         trim(visu_slice_xdmf_records(irec)%bin_ref), &
                                         visu_slice_xdmf_records(irec)%precision, &
                                         opt_seek_bytes=visu_slice_xdmf_records(irec)%seek_bytes)
      end if
    end do
    call xdmf_end_grid(u, opt_fragment=is_fragment)

    return
  end subroutine write_visu_slice_bundle_grid
  !==============================================================================
  subroutine write_visu_slice_bundle_wrapper_xdmfs(dm, visuname, iter)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname
    integer, intent(in) :: iter

    character(len=256) :: plane_name
    character(len=256) :: xdmf_file
    integer :: dir, n, npl, u

    if(nrank /= 0) return

    do dir = 1, NDIM
      do n = 1, NSLICE
        npl = slice_idx(dir, n)
        call visu_slice_bundle_plane_name(dm, visuname, dir, n, npl, iter, plane_name)
        xdmf_file = trim(dir_visu_xdmf)//'/'//trim(plane_name)//'.xdmf'
        open(newunit=u, file=trim(xdmf_file), status='replace', action='write')
        call write_visu_slice_bundle_grid(u, dm, plane_name, dir, npl)
        close(u)
      end do
    end do

    return
  end subroutine write_visu_slice_bundle_wrapper_xdmfs
  !==============================================================================
  subroutine write_constant_plane_to_slice_bundle(dm, dir, value)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: dir
    real(WP), intent(in) :: value

    real(WP), allocatable :: accc(:,:,:)

    select case(dir)
    case(1)
      allocate(accc(d1cc%xsz(1), d1cc%xsz(2), d1cc%xsz(3)))
      accc = value
      call decomp_2d_write_var(visu_slice_bundle_io, IPENCIL(1), accc, &
                               opt_decomp=d1cc, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc)
    case(2)
      allocate(accc(dc1c%ysz(1), dc1c%ysz(2), dc1c%ysz(3)))
      accc = value
      call decomp_2d_write_var(visu_slice_bundle_io, IPENCIL(2), accc, &
                               opt_decomp=dc1c, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc)
    case(3)
      allocate(accc(dcc1%zsz(1), dcc1%zsz(2), dcc1%zsz(3)))
      accc = value
      call decomp_2d_write_var(visu_slice_bundle_io, IPENCIL(3), accc, &
                               opt_decomp=dcc1, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc)
    case default
      call Print_error_msg('write_constant_plane_to_slice_bundle: invalid slice direction')
    end select

    return
  end subroutine write_constant_plane_to_slice_bundle
  !==============================================================================
  subroutine write_visu_slice_metadata_fields(dm)
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm

    character(len=16) :: slice_tag
    character(len=64) :: dimstring
    integer :: dir, n, npl, slice_id
    integer :: ncell2(3)
    integer(int64) :: field_offset_bytes
    integer(int64) :: field_nbytes

    if(.not. visu_slice_bundle_active) return
    if(.not. visu_slice_bundle_do_write) return
    if(visu_slice_bundle_is_savg) return

    ! ParaView collapses bundled slice XDMF into one source; expose constant
    ! cell fields so users can Threshold individual planes without data copies.
    slice_id = 0
    do dir = 1, NDIM
      do n = 1, NSLICE
        slice_id = slice_id + 1
        npl = slice_idx(dir, n)
        slice_tag = slice_prefix(dir)//trim(int2str(npl))
        ncell2 = ncl_visu(1:3)
        ncell2(dir) = 1
        dimstring = xdmf_dims_kji_string(ncell2)
        field_offset_bytes = visu_slice_bundle_offset_bytes
        field_nbytes = visu_slice_field_bytes(dir, dm%visu_precision)
        call write_constant_plane_to_slice_bundle(dm, dir, real(slice_id, WP))
        if(nrank == 0) then
          write(visu_slice_bundle_meta_unit, '(A,1X,A,1X,A,1X,A,1X,I0,1X,I0,1X,I0)') &
            'slice_id', trim(visu_slice_bundle_file), trim(slice_tag), trim(dimstring), &
            dm%visu_precision, field_offset_bytes, field_nbytes
          call append_visu_slice_xdmf_record('slice_id', visu_slice_bundle_ref, dir, npl, dm%visu_precision, &
                                             field_offset_bytes)
        end if
        visu_slice_bundle_offset_bytes = visu_slice_bundle_offset_bytes + field_nbytes
      end do
    end do

    do dir = 1, NDIM
      do n = 1, NSLICE
        npl = slice_idx(dir, n)
        slice_tag = slice_prefix(dir)//trim(int2str(npl))
        ncell2 = ncl_visu(1:3)
        ncell2(dir) = 1
        dimstring = xdmf_dims_kji_string(ncell2)
        field_offset_bytes = visu_slice_bundle_offset_bytes
        field_nbytes = visu_slice_field_bytes(dir, dm%visu_precision)
        call write_constant_plane_to_slice_bundle(dm, dir, real(dir, WP))
        if(nrank == 0) then
          write(visu_slice_bundle_meta_unit, '(A,1X,A,1X,A,1X,A,1X,I0,1X,I0,1X,I0)') &
            'slice_dir', trim(visu_slice_bundle_file), trim(slice_tag), trim(dimstring), &
            dm%visu_precision, field_offset_bytes, field_nbytes
          call append_visu_slice_xdmf_record('slice_dir', visu_slice_bundle_ref, dir, npl, dm%visu_precision, &
                                             field_offset_bytes)
        end if
        visu_slice_bundle_offset_bytes = visu_slice_bundle_offset_bytes + field_nbytes
      end do
    end do

    return
  end subroutine write_visu_slice_metadata_fields
  !==============================================================================
  subroutine cleanup_visu_slice_map_file()
    implicit none

    character(len=256) :: map_file
    integer :: ntrim, u
    logical :: exists

    ntrim = len_trim(visu_slice_bundle_xdmf_file)
    if(ntrim > 5 .and. visu_slice_bundle_xdmf_file(ntrim-4:ntrim) == '.xdmf') then
      map_file = visu_slice_bundle_xdmf_file(1:ntrim-5)//'_slice_map.txt'
    else
      map_file = trim(visu_slice_bundle_xdmf_file)//'_slice_map.txt'
    end if
    inquire(file=trim(map_file), exist=exists)
    if(exists) then
      open(newunit=u, file=trim(map_file), status='old')
      close(u, status='delete')
    end if

    return
  end subroutine cleanup_visu_slice_map_file
  !==============================================================================
  subroutine write_visu_slice_bundle_xdmf(dm, visuname, iter)
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname
    integer, intent(in) :: iter

    character(len=256) :: fullname
    character(len=256) :: root_name
    integer :: dir, n, npl
    integer :: u

    if(nrank /= 0) return
    if(.not. visu_slice_bundle_active) return
    if(.not. visu_slice_bundle_do_write) return
    if(visu_slice_xdmf_record_count <= 0) return

    if(visu_slice_bundle_is_savg) then
      root_name = trim(visuname)//'_'//slice_prefix(visu_slice_bundle_savg_dir)// &
                  trim(int2str(visu_slice_bundle_savg_npl))
      open(newunit=u, file=trim(visu_slice_bundle_xdmf_file), status='replace', action='write')
      call write_visu_slice_bundle_grid(u, dm, root_name, visu_slice_bundle_savg_dir, &
                                        visu_slice_bundle_savg_npl)
      close(u)
      return
    end if

    root_name = 'domain'//trim(int2str(dm%idom))//'_'//trim(visuname)//'_slices_visu_'//trim(int2str(iter))

    open(newunit=u, file=trim(visu_slice_bundle_xdmf_file), status='replace', action='write')
    write(u,'(A)') '<?xml version="1.0" ?>'
    write(u,'(A)') '<Xdmf Version="3.0">'
    write(u,'(A)') '  <Domain>'
    write(u,'(A)') '    <Grid Name="'//trim(root_name)//'" GridType="Collection" CollectionType="Spatial">'

    do dir = 1, NDIM
      do n = 1, NSLICE
        npl = slice_idx(dir, n)
        call visu_slice_bundle_plane_name(dm, visuname, dir, n, npl, iter, fullname)
        call write_visu_slice_bundle_grid(u, dm, fullname, dir, npl, opt_fragment=.true.)
      end do
    end do

    write(u,'(A)') '    </Grid>'
    write(u,'(A)') '  </Domain>'
    write(u,'(A)') '</Xdmf>'
    close(u)

    return
  end subroutine write_visu_slice_bundle_xdmf
  !==============================================================================
  function visu_field_bytes(precision_bytes) result(nbytes)
    implicit none
    integer, intent(in) :: precision_bytes
    integer(int64) :: nbytes

    nbytes = int(ncl_visu(1), int64) * int(ncl_visu(2), int64) * &
             int(ncl_visu(3), int64) * int(precision_bytes, int64)
  end function visu_field_bytes
  !==============================================================================
  pure function visu_slice_field_bytes(dir, precision_bytes) result(nbytes)
    implicit none
    integer, intent(in) :: dir
    integer, intent(in) :: precision_bytes
    integer(int64) :: nbytes
    integer :: n2cell(3)

    n2cell = ncl_visu(1:3)
    n2cell(dir) = 1
    nbytes = int(n2cell(1), int64) * int(n2cell(2), int64) * &
             int(n2cell(3), int64) * int(precision_bytes, int64)
  end function visu_slice_field_bytes
  !==============================================================================
  pure function visu_source_location(direction) result(source)
    implicit none
    integer, intent(in) :: direction
    character(len=32) :: source

    select case(direction)
    case(N_DIRECTION)
      source = 'cell_centered'
    case(X_DIRECTION)
      source = 'x_staggered_to_cell'
    case(Y_DIRECTION)
      source = 'y_staggered_to_cell'
    case(Z_DIRECTION)
      source = 'z_staggered_to_cell'
    case default
      source = 'unknown'
    end select
  end function visu_source_location
  !==============================================================================
  !> Open and write the header/mesh references for a visualisation XDMF file.
  !> - dm (in): Domain descriptor.
  !> - visuname (in): Visualisation file stem.
  !> - iter (in): Iteration number used in output names.
  !> - opt_is_savg (in): Optional flag for statistics-average output.
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
    logical :: is_savg
    !
    is_savg = present(opt_is_savg)
    call begin_visu_bundle(dm, visuname, iter, is_savg)
    call begin_visu_slice_bundle(dm, visuname, iter, is_savg)

    if (nrank /= 0) return
    !
    if(.not. is_savg) then
    if(dm%visu_idim == Ivisu_3D .or. dm%visu_idim == Ivisu_3D2D) then
      call generate_pathfile_name(xdmf_file, dm%idom, visuname, dir_visu_xdmf, 'xdmf', iter)
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
    if (is_savg) then
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

    if(.not. visu_slice_bundle_active) then
      do n = nst, nen
        do dir = 1, NDIM
          if (n == 0 .and. dir /= nave_plane(dir)) then
            cycle
          end if
          n2node = nnd_visu(1:3)
          n2node(dir) = 2
          npl = slice_idx(dir, n)
          fullname = trim(visuname)//'_'//slice_prefix(dir)//trim(int2str(npl))
          call generate_pathfile_name(xdmf_file, dm%idom, fullname, dir_visu_xdmf, 'xdmf', iter)
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
    end if

    return
  end subroutine write_visu_file_begin
  !==============================================================================
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
    logical :: is_savg
    !
    is_savg = present(opt_is_savg)
    !
    if (nrank == 0) then
      if(.not. is_savg) then
      if(dm%visu_idim == Ivisu_3D .or. dm%visu_idim == Ivisu_3D2D) then
        call generate_pathfile_name(xdmf_file, dm%idom, visuname, dir_visu_xdmf, 'xdmf', iter)
        open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
        call xdmf_end_grid(u)
        close(u)
      end if
      end if
    end if
    !
    if (is_savg) then
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
    if (nrank == 0 .and. (.not. visu_slice_bundle_active)) then
      do n = nst, nen
        do dir = 1, NDIM
          if (n == 0 .and. dir /= nave_plane(dir)) then
            cycle
          end if
          npl = slice_idx(dir,n)
          fullname = trim(visuname)//'_'//slice_prefix(dir)//trim(int2str(npl))
          call generate_pathfile_name(xdmf_file, dm%idom, fullname, dir_visu_xdmf, 'xdmf', iter)
          open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
          call xdmf_end_grid(u)
          close(u)
        end do
      end do
    end if
    call write_visu_slice_metadata_fields(dm)
    if(visu_slice_bundle_active .and. visu_slice_bundle_do_write) &
      call cleanup_visu_slice_bundle_wrapper_xdmf(dm, visuname, iter)
    call write_visu_slice_bundle_xdmf(dm, visuname, iter)
    call end_visu_bundle()
    call end_visu_slice_bundle()
    return
  end subroutine write_visu_file_end
  !----------------------------- Core field writer --------------------------------
  !==============================================================================
  subroutine write_visu_field_bin_and_xdmf(dm, field_in, field_name, visuname, iter, direction, &
                                           opt_ibc, opt_bin_name, opt_reuse_bin_name, opt_is_restart_data)
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP),        intent(in) :: field_in(:,:,:)
    character(*),    intent(in) :: field_name, visuname
    integer,         intent(in) :: iter
    integer,         intent(in) :: direction
    integer,         intent(in), optional :: opt_ibc(:)
    character(*),    intent(in), optional :: opt_bin_name
    character(*),    intent(in), optional :: opt_reuse_bin_name
    logical,         intent(in), optional :: opt_is_restart_data

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
      call write_visu_3d_binary_and_xdmf(dm, accc, field_name, visuname, iter, &
                                         direction, opt_bin_name, opt_reuse_bin_name, opt_is_restart_data)
    end if
    !
    if (dm%visu_idim == Ivisu_2D .or. dm%visu_idim == Ivisu_3D2D) then
      do dir = 1, NDIM
        do n = 1, NSLICE
          call write_visu_plane_binary_and_xdmf(dm, accc, field_name, visuname, dir, n, iter, dm%existing_output_policy, &
                                                opt_bin_name)
        end do
      end do
    end if

    deallocate(accc)
  end subroutine write_visu_field_bin_and_xdmf
  !==============================================================================
  subroutine stagger_to_ccc(dm, fin, fout, direction, opt_ibc)
    use decomp_2d
    use operations
    use udf_type_mod
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
  !==============================================================================
  subroutine write_visu_3d_binary_and_xdmf(dm, accc, field_name, visuname, iter, direction, &
                                           opt_bin_name, opt_reuse_bin_name, opt_is_restart_data)
    use decomp_2d
    use decomp_2d_io
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: accc(:,:,:)
    character(*), intent(in) :: field_name, visuname
    integer, intent(in) :: iter, direction
    character(*), intent(in), optional :: opt_bin_name
    character(*), intent(in), optional :: opt_reuse_bin_name
    logical, intent(in), optional :: opt_is_restart_data
    !
    character(len=256) :: bin_file
    character(len=256) :: bin_ref
    character(len=256) :: bin_name
    character(len=256) :: bin_path
    character(len=256) :: original_bin_file
    character(len=256) :: reuse_file
    character(len=256) :: xdmf_file
    character(len=32) :: source_location
    character(len=64) :: dimstring
    integer(int64) :: field_offset_bytes
    integer(int64) :: field_nbytes
    integer :: u
    integer :: xdmf_precision
    logical :: do_reuse
    logical :: is_restart_data

    !----------------------- 3D binary  -----------------------
    bin_name = field_name
    if(present(opt_bin_name)) bin_name = opt_bin_name
    is_restart_data = .false.
    if(present(opt_is_restart_data)) is_restart_data = opt_is_restart_data
    bin_path = dir_visu_data
    if(is_restart_data) bin_path = dir_data
    xdmf_precision = dm%visu_precision
    if(is_restart_data) xdmf_precision = storage_size(accc) / 8
    source_location = visu_source_location(direction)

    do_reuse = .false.
    if((.not. is_restart_data) .and. (.not. is_visu_single_precision(dm)) .and. present(opt_reuse_bin_name)) then
      call generate_pathfile_name(reuse_file, dm%idom, trim(opt_reuse_bin_name), dir_data, 'bin', iter)
      do_reuse = file_exists(trim(reuse_file))
      if(do_reuse) bin_file = reuse_file
    end if

    if(.not. do_reuse) then
      call generate_pathfile_name(original_bin_file, dm%idom, trim(bin_name), trim(bin_path), 'bin', iter)
      if(visu_bundle_active .and. (.not. is_restart_data)) then
        bin_file = visu_bundle_file
        field_offset_bytes = visu_bundle_offset_bytes
        field_nbytes = visu_field_bytes(xdmf_precision)
        if(visu_bundle_do_write) then
          call remove_output_file_if_overwrite(original_bin_file, dm%existing_output_policy)
          call decomp_2d_write_var(visu_bundle_io, IPENCIL(1), accc, opt_decomp=dm%dccc, &
                                   opt_reduce_prec=is_visu_single_precision(dm))
          if(nrank == 0) then
            write(visu_bundle_meta_unit, '(A,1X,A,1X,A,1X,A,1X,A,1X,I0,1X,I0,1X,I0)') &
              trim(field_name), trim(original_bin_file), trim(source_location), 'Cell', &
              trim(xdmf_dims_kji_string(ncl_visu(1:3))), &
              xdmf_precision, field_offset_bytes, field_nbytes
          end if
        end if
        visu_bundle_offset_bytes = visu_bundle_offset_bytes + field_nbytes
      else
        bin_file = original_bin_file
        call write_one_3d_array(accc, trim(bin_name), dm%idom, iter, dm%dccc, dm%existing_output_policy, &
                                opt_reduce_prec=(is_visu_single_precision(dm) .and. (.not. is_restart_data)), &
                                opt_path=trim(bin_path))
      end if
    end if
    bin_ref = visu_xdmf_relative_path(bin_file)
    !----------------------- 3D xdmf -----------------------
    if (nrank == 0) then
      call generate_pathfile_name(xdmf_file, dm%idom, visuname, dir_visu_xdmf, 'xdmf', iter)
      open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
      dimstring = xdmf_dims_kji_string(ncl_visu(1:3))
      if(visu_bundle_active .and. (.not. do_reuse) .and. (.not. is_restart_data)) then
        call xdmf_write_attribute_scalar(u, field_name, 'Cell', dimstring, bin_ref, xdmf_precision, &
                                         opt_seek_bytes=field_offset_bytes)
      else
        call xdmf_write_attribute_scalar(u, field_name, 'Cell', dimstring, bin_ref, xdmf_precision)
      end if
      close(u)
    end if
    return
  end subroutine write_visu_3d_binary_and_xdmf
  !==============================================================================
  subroutine write_visu_plane_binary_and_xdmf(dm, accc, field_name, visuname, dir, n, iter, existing_output_policy, opt_bin_name)
    use decomp_2d
    use decomp_2d_io
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: accc(:,:,:)
    character(*), intent(in) :: field_name, visuname
    integer, intent(in) :: iter, dir, n, existing_output_policy
    character(*), intent(in), optional :: opt_bin_name

    integer :: npl
    character(len=256) :: bin_file
    character(len=256) :: bin_ref
    character(len=256) :: bin_name
    character(len=256) :: slice_tag
    integer :: ncell2(3)
    integer(int64) :: field_offset_bytes
    integer(int64) :: field_nbytes

    !----------------------- 2D slices binary -----------------------
    npl = slice_idx(dir, n)
    slice_tag = slice_prefix(dir)//trim(int2str(npl))
    bin_name = field_name
    if(present(opt_bin_name)) bin_name = opt_bin_name
    call generate_pathfile_name(bin_file, dm%idom, trim(bin_name)//'_'//trim(slice_tag), &
                                dir_visu_data, 'bin', iter)

    if(visu_slice_bundle_active) then
      field_offset_bytes = visu_slice_bundle_offset_bytes
      field_nbytes = visu_slice_field_bytes(dir, dm%visu_precision)
      if(visu_slice_bundle_do_write) then
        call remove_output_file_if_overwrite(bin_file, existing_output_policy)
        call write_plane_to_slice_bundle(dm, accc, dir, npl)
        if(nrank == 0) then
          ncell2 = ncl_visu(1:3)
          ncell2(dir) = 1
          write(visu_slice_bundle_meta_unit, '(A,1X,A,1X,A,1X,A,1X,I0,1X,I0,1X,I0)') &
            trim(field_name), trim(bin_file), trim(slice_tag), trim(xdmf_dims_kji_string(ncell2)), &
            dm%visu_precision, field_offset_bytes, field_nbytes
        end if
      end if
      bin_ref = visu_slice_bundle_ref
      if(nrank == 0 .and. visu_slice_bundle_do_write) &
        call append_visu_slice_xdmf_record(field_name, bin_ref, dir, npl, dm%visu_precision, field_offset_bytes)
      visu_slice_bundle_offset_bytes = visu_slice_bundle_offset_bytes + field_nbytes
    else
      call write_plane_bin(dm, accc, dir, npl, bin_file, existing_output_policy)
      bin_ref = visu_xdmf_relative_path(bin_file)
    end if
    !----------------------- 2D slices XDMF -----------------------
    if (nrank == 0 .and. (.not. visu_slice_bundle_active)) &
      call write_slice_field_xdmf(dm, visuname, field_name, bin_ref, dir, npl, iter)

  end subroutine write_visu_plane_binary_and_xdmf
  !==============================================================================
  pure function slice_prefix(dir) result(p)
    integer, intent(in) :: dir
    character(len=2) :: p
    select case(dir)
    case(1); p='xi'
    case(2); p='yi'
    case(3); p='zi'
    end select
  end function slice_prefix
  !==============================================================================
  ! subroutine write_coarsened_3d(dm, ccc, field_name, iter)
  !   use udf_type_mod
  !   use decomp_2d
  !   use decomp_2d_io
  !   use io_tools_mod
  !   use parameters_constant_mod, only: MAXP, IPENCIL
  !   implicit none
  !   type(t_domain), intent(in) :: dm
  !   real(WP), intent(in) :: ccc(:,:,:)
  !   character(*), intent(in) :: field_name
  !   integer, intent(in) :: iter
  !   real(WP), allocatable :: coarse(:,:,:)

  !   allocate(coarse(xstV(1):xenV(1), xstV(2):xenV(2), xstV(3):xenV(3)))
  !   coarse = MAXP
  !   call fine_to_coarseV(IPENCIL(1), ccc, coarse)
  !   call write_one_3d_array(coarse, trim(field_name), dm%idom, iter, dm%dccc)
  !   deallocate(coarse)
  ! end subroutine write_coarsened_3d
  !==============================================================================
  pure function is_visu_single_precision(dm) result(is_single)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    logical :: is_single

    is_single = (dm%visu_precision == VISU_PRECISION_SINGLE)
  end function is_visu_single_precision
  !==============================================================================
  subroutine write_plane_bin(dm, accc_in, dir, npl, bin_file, existing_output_policy)
    use decomp_2d_io
    use transpose_extended_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: accc_in(:,:,:)
    integer, intent(in) :: dir, npl
    character(*), intent(in) :: bin_file
    integer, intent(in) :: existing_output_policy

    real(WP), allocatable :: accc_yp(:,:,:), accc_zp(:,:,:)
    real(WP), allocatable :: accc(:,:,:)
    logical :: do_write

    do_write = .true.
    select case (existing_output_policy)
    case (OUTPUT_POLICY_OVERWRITE)
      continue
    case (OUTPUT_POLICY_SKIP)
      if (file_exists(trim(bin_file))) then
        if (nrank == 0) then
          call Print_warning_msg("File "//trim(bin_file)// &
                                " already exists; skip writing")
        end if
        do_write = .false.
      end if
    case (OUTPUT_POLICY_RENAME_EXISTING)
      if (file_exists(trim(bin_file))) then
        call rename_existing_file(trim(bin_file))
      end if
    case default
      continue
    end select
    !
    if(.not. do_write) return
    !
    select case(dir)
    case(1)
      allocate(accc(d1cc%xsz(1), d1cc%xsz(2), d1cc%xsz(3)))
      accc(1, :, :) = accc_in(npl, :, :)
      call decomp_2d_write_one(IPENCIL(1), accc, trim(bin_file), &
                               opt_decomp=d1cc, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc)

    case(2)
      allocate(accc_yp(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      call transpose_x_to_y(accc_in, accc_yp, dm%dccc)
      allocate(accc(dc1c%ysz(1), dc1c%ysz(2), dc1c%ysz(3)))
      accc(:, 1, :) = accc_yp(:, npl, :)
      call decomp_2d_write_one(IPENCIL(2), accc, trim(bin_file), &
                               opt_decomp=dc1c, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc, accc_yp)

    case(3)
      allocate(accc_yp(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      allocate(accc_zp(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)))
      call transpose_x_to_y(accc_in, accc_yp, dm%dccc)
      call transpose_y_to_z(accc_yp, accc_zp, dm%dccc)
      allocate(accc(dcc1%zsz(1), dcc1%zsz(2), dcc1%zsz(3)))
      accc(:, :, 1) = accc_zp(:, :, npl)
      call decomp_2d_write_one(IPENCIL(3), accc, trim(bin_file), &
                               opt_decomp=dcc1, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc, accc_yp, accc_zp)
    end select

    return
  end subroutine write_plane_bin
  !==============================================================================
  subroutine write_plane_to_slice_bundle(dm, accc_in, dir, npl)
    use transpose_extended_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: accc_in(:,:,:)
    integer, intent(in) :: dir, npl

    real(WP), allocatable :: accc_yp(:,:,:), accc_zp(:,:,:)
    real(WP), allocatable :: accc(:,:,:)

    select case(dir)
    case(1)
      allocate(accc(d1cc%xsz(1), d1cc%xsz(2), d1cc%xsz(3)))
      accc(1, :, :) = accc_in(npl, :, :)
      call decomp_2d_write_var(visu_slice_bundle_io, IPENCIL(1), accc, &
                               opt_decomp=d1cc, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc)

    case(2)
      allocate(accc_yp(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      call transpose_x_to_y(accc_in, accc_yp, dm%dccc)
      allocate(accc(dc1c%ysz(1), dc1c%ysz(2), dc1c%ysz(3)))
      accc(:, 1, :) = accc_yp(:, npl, :)
      call decomp_2d_write_var(visu_slice_bundle_io, IPENCIL(2), accc, &
                               opt_decomp=dc1c, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc, accc_yp)

    case(3)
      allocate(accc_yp(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)))
      allocate(accc_zp(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)))
      call transpose_x_to_y(accc_in, accc_yp, dm%dccc)
      call transpose_y_to_z(accc_yp, accc_zp, dm%dccc)
      allocate(accc(dcc1%zsz(1), dcc1%zsz(2), dcc1%zsz(3)))
      accc(:, :, 1) = accc_zp(:, :, npl)
      call decomp_2d_write_var(visu_slice_bundle_io, IPENCIL(3), accc, &
                               opt_decomp=dcc1, opt_reduce_prec=is_visu_single_precision(dm))
      deallocate(accc, accc_yp, accc_zp)
    end select

    return
  end subroutine write_plane_to_slice_bundle
  !==============================================================================
  subroutine write_slice_field_xdmf(dm, visuname, field_name, bin_file, dir, npl, iter, opt_seek_bytes)
    use parameters_constant_mod
    use typeconvert_mod, only: int2str
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: visuname, field_name, bin_file
    integer, intent(in) :: dir, npl, iter
    integer(int64), intent(in), optional :: opt_seek_bytes

    character(len=256) :: xdmf_file
    character(len=256) :: name
    integer :: u
    integer :: n2cell(3)
    character(len=64) :: dimstring

    name = trim(visuname)//'_'//slice_prefix(dir)//trim(int2str(npl))

    n2cell = ncl_visu(1:3)
    n2cell(dir) = 1   ! cells reduce by 1 in sliced direction (2 nodes -> 1 cell)

    call generate_pathfile_name(xdmf_file, dm%idom, name, dir_visu_xdmf, 'xdmf', iter)
    open(newunit=u, file=trim(xdmf_file), status='old', action='write', position='append')
    dimstring = xdmf_dims_kji_string(n2cell)
    if(present(opt_seek_bytes)) then
      call xdmf_write_attribute_scalar(u, field_name, 'Cell', dimstring, bin_file, dm%visu_precision, &
                                       opt_seek_bytes=opt_seek_bytes)
    else
      call xdmf_write_attribute_scalar(u, field_name, 'Cell', dimstring, bin_file, dm%visu_precision)
    end if
    close(u)
  end subroutine write_slice_field_xdmf
  !==============================================================================
  pure function find_n_from_npl(dir, npl) result(nfound)
    integer, intent(in) :: dir, npl
    integer :: nfound, n
    nfound = 0
    do n = 0, NSLICE
      if (slice_idx(dir,n) == npl) then
        nfound = n
        return
      end if
    end do
  end function find_n_from_npl

end module visualisation_field_mod
