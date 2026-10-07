module mhd_mod
! Note: This MHD solver is potential solver only.
!       Assumed: the induced magnetic field is negligible.
!       Electrical conductivity is treated as constant in the MHD solve.
  use parameters_constant_mod
  implicit none

  private :: cross_production_mhd
  private :: initialise_static_magnetic_field
  private :: resolve_electrical_bc
  private :: build_current_ibc
  public  :: initialise_mhd
  public  :: compute_Lorentz_force
  public  :: check_current_conservation
contains
!==============================================================================
  subroutine initialise_mhd(fl, mh, dm)
    use boundary_conditions_mod
    use math_mod
    use mpi_mod
    use print_msg_mod
    use udf_type_mod
    !use visualisation_field_mod
    implicit none
    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_mhd),    intent(inout) :: mh
    ! Matches the '  u-bc :' style of the boundary-condition table in bc_general.
    character(len = *), parameter :: fmt_epbc = '(2X, A10, 2(A3, A14))'

    if(nrank==0) call Print_debug_start_msg('Initialising MHD ...')
!------------------------------------------------------------------------------
!   allocate variables
!------------------------------------------------------------------------------
    fl%max_div_j         = ZERO
    fl%current_imbalance = ZERO

    call alloc_x(mh%ep, dm%dccc); mh%ep = ZERO

    call alloc_x(mh%jx, dm%dpcc); mh%jx = ZERO
    call alloc_x(mh%jy, dm%dcpc); mh%jy = ZERO
    call alloc_x(mh%jz, dm%dccp); mh%jz = ZERO

    call alloc_x(mh%bx, dm%dpcc); mh%bx = ZERO
    call alloc_x(mh%by, dm%dcpc); mh%by = ZERO
    call alloc_x(mh%bz, dm%dccp); mh%bz = ZERO

    call alloc_x(fl%lrfx, dm%dpcc); fl%lrfx = ZERO
    call alloc_x(fl%lrfy, dm%dcpc); fl%lrfy = ZERO
    call alloc_x(fl%lrfz, dm%dccp); fl%lrfz = ZERO

    allocate( mh%fbcx_jx(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_jx(dm%dpcc%ysz(1),              4, dm%dpcc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_jx(dm%dpcc%zsz(1), dm%dpcc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_jy(             4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_jy(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_jy(dm%dcpc%zsz(1), dm%dcpc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_jz(             4, dm%dccp%xsz(2), dm%dccp%xsz(3)) )! default x pencil
    allocate( mh%fbcy_jz(dm%dccp%ysz(1),              4, dm%dccp%ysz(3)) )! default y pencil
    allocate( mh%fbcz_jz(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_bx(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_bx(dm%dpcc%ysz(1),              4, dm%dpcc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_bx(dm%dpcc%zsz(1), dm%dpcc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_by(             4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_by(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_by(dm%dcpc%zsz(1), dm%dcpc%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_bz(             4, dm%dccp%xsz(2), dm%dccp%xsz(3)) )! default x pencil
    allocate( mh%fbcy_bz(dm%dccp%ysz(1),              4, dm%dccp%ysz(3)) )! default y pencil
    allocate( mh%fbcz_bz(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) )! default z pencil

    allocate( mh%fbcx_ep(             4, dm%dccc%xsz(2), dm%dccc%xsz(3)) )! default x pencil
    allocate( mh%fbcy_ep(dm%dccc%ysz(1),              4, dm%dccc%ysz(3)) )! default y pencil
    allocate( mh%fbcz_ep(dm%dccc%zsz(1), dm%dccc%zsz(2),              4) )! default z pencil

    if(mh%is_NStuart) mh%NHartmn = sqrt_wp( ONE/fl%rre * mh%NStuart)
    if(mh%is_NHartmn) mh%NStuart = mh%NHartmn * mh%NHartmn * fl%rre
    mh%iterfrom = fl%iterfrom
    mh%jx = ZERO
    mh%jy = ZERO
    mh%jz = ZERO

    mh%ep = ZERO
!------------------------------------------------------------------------------
! Boundary for static magnetic field
!------------------------------------------------------------------------------
    mh%ibcx_bx(:) = dm%ibcx_qx(:)
    mh%ibcx_by(:) = dm%ibcx_qy(:)
    mh%ibcx_bz(:) = dm%ibcx_qz(:)
    mh%ibcy_bx(:) = dm%ibcy_qx(:)
    mh%ibcy_by(:) = dm%ibcy_qy(:)
    mh%ibcy_bz(:) = dm%ibcy_qz(:)
    mh%ibcz_bx(:) = dm%ibcz_qx(:)
    mh%ibcz_by(:) = dm%ibcz_qy(:)
    mh%ibcz_bz(:) = dm%ibcz_qz(:)

    call initialise_static_magnetic_field(mh, dm)
!------------------------------------------------------------------------------
! Boundary for electrical potential
!------------------------------------------------------------------------------
    call resolve_electrical_bc(mh%ebcx_nominal, dm%ibcx_pr, mh%ibcx_ep, 'x')
    call resolve_electrical_bc(mh%ebcy_nominal, dm%ibcy_pr, mh%ibcy_ep, 'y')
    call resolve_electrical_bc(mh%ebcz_nominal, dm%ibcz_pr, mh%ibcz_ep, 'z')
!------------------------------------------------------------------------------
! Boundary for current density. It follows the electrical condition, not the
! velocity one, so this has to come after the block above.
!------------------------------------------------------------------------------
    call build_current_ibc(mh%ibcx_ep, dm%ibcx_qx, mh%ibcx_jx, 'x', is_normal = .true. )
    call build_current_ibc(mh%ibcx_ep, dm%ibcx_qy, mh%ibcx_jy, 'x', is_normal = .false.)
    call build_current_ibc(mh%ibcx_ep, dm%ibcx_qz, mh%ibcx_jz, 'x', is_normal = .false.)
    call build_current_ibc(mh%ibcy_ep, dm%ibcy_qx, mh%ibcy_jx, 'y', is_normal = .false.)
    call build_current_ibc(mh%ibcy_ep, dm%ibcy_qy, mh%ibcy_jy, 'y', is_normal = .true. )
    call build_current_ibc(mh%ibcy_ep, dm%ibcy_qz, mh%ibcy_jz, 'y', is_normal = .false.)
    call build_current_ibc(mh%ibcz_ep, dm%ibcz_qx, mh%ibcz_jx, 'z', is_normal = .false.)
    call build_current_ibc(mh%ibcz_ep, dm%ibcz_qy, mh%ibcz_jy, 'z', is_normal = .false.)
    call build_current_ibc(mh%ibcz_ep, dm%ibcz_qz, mh%ibcz_jz, 'z', is_normal = .true. )

    ! Only the IBC_DIRICHLET slots are read, and for the current density those are
    ! exactly the normal components on an insulating wall, where j.n = 0.
    mh%fbcx_jx(:, :, :) = ZERO
    mh%fbcy_jx(:, :, :) = ZERO
    mh%fbcz_jx(:, :, :) = ZERO
    mh%fbcx_jy(:, :, :) = ZERO
    mh%fbcy_jy(:, :, :) = ZERO
    mh%fbcz_jy(:, :, :) = ZERO
    mh%fbcx_jz(:, :, :) = ZERO
    mh%fbcy_jz(:, :, :) = ZERO
    mh%fbcz_jz(:, :, :) = ZERO
    ! Log it: the electrical BC defaults to the pressure BC, and a default that is
    ! never printed is a default nobody checks.
    if(nrank == 0) then
      write (*, fmt_epbc) '  ep-bc x :', '||', get_name_bc(mh%ibcx_ep(1)), '||', get_name_bc(mh%ibcx_ep(2))
      write (*, fmt_epbc) '  ep-bc y :', '||', get_name_bc(mh%ibcy_ep(1)), '||', get_name_bc(mh%ibcy_ep(2))
      write (*, fmt_epbc) '  ep-bc z :', '||', get_name_bc(mh%ibcz_ep(1)), '||', get_name_bc(mh%ibcz_ep(2))
    end if

    mh%fbcx_ep(:, :, :) = ZERO
    mh%fbcy_ep(:, :, :) = ZERO
    mh%fbcz_ep(:, :, :) = ZERO

    if(dm%icase == ICASE_PIPE) call update_fbcy_cc_mhd_halo(mh, dm)

    !call write_visu_mhd(mh, fl, dm, 'initial_mhd')

    if(nrank==0) call Print_debug_end_msg()
    return
  end subroutine

!==============================================================================
!> \brief Map the named electrical boundary condition onto the IBC_* code the
!>        operators and the Poisson solver understand.
!>
!> - EBC_INSULATING is j.n = 0, i.e. d(ep)/dn = (u x B).n. The solve itself carries
!>   homogeneous Neumann; compute_Lorentz_force supplies the (u x B).n part by
!>   dropping that normal flux, which is the discrete equivalent (see there).
!> - EBC_CONDUCTING is the perfectly-conducting limit, ep = const on the plane.
!>   It maps cleanly onto IBC_DIRICHLET, but no shipped case exercises it and it
!>   is untested, so it is rejected rather than run silently.
!> - EBC_INHERIT reproduces the behaviour from before this input existed.
!>
!> - ebc     (in ): named condition per side, EBC_*
!> - ibc_pr  (in ): pressure BC of the same direction, used by EBC_INHERIT
!> - ibc_ep  (out): resolved IBC_* code per side
!> - dir     (in ): direction label, for error messages only
!==============================================================================
  subroutine resolve_electrical_bc(ebc, ibc_pr, ibc_ep, dir)
    use print_msg_mod, only : Print_error_msg
    implicit none
    integer,          intent(in)  :: ebc(2)
    integer,          intent(in)  :: ibc_pr(2)
    integer,          intent(out) :: ibc_ep(2)
    character(len=*), intent(in)  :: dir
    integer :: n

    do n = 1, 2
      select case(ebc(n))
      case(EBC_INHERIT)
        ibc_ep(n) = ibc_pr(n)
      case(EBC_INSULATING)
        ibc_ep(n) = IBC_NEUMANN
      case(EBC_PERIODIC)
        ibc_ep(n) = IBC_PERIODIC
      case(EBC_CONDUCTING)
        ibc_ep(n) = IBC_DIRICHLET ! the mapping is this, but the path is unverified
        call Print_error_msg('ebc'//dir//' = conducting is recognised but not implemented: '// &
          'the perfectly-conducting (Dirichlet electric potential) path has no test coverage.')
      case default
        ibc_ep(n) = ibc_pr(n)
        call Print_error_msg('Unresolved electrical bc in direction '//dir//'.')
      end select
!------------------------------------------------------------------------------
!     An IBC_INTERIOR side is not a boundary of the physical domain at all - it is
!     the pipe axis, or a multi-domain join - and carries two ghost layers rather
!     than a boundary value. Naming an electrical condition there would replace the
!     axis mirror with a wall, so only inheritance is meaningful.
!------------------------------------------------------------------------------
      if(ibc_pr(n) == IBC_INTERIOR .and. ebc(n) /= EBC_INHERIT) &
        call Print_error_msg('ebc'//dir//' cannot be named on an interior side '// &
          '(pipe axis or domain join); leave it to inherit.')
    end do
!------------------------------------------------------------------------------
!   The Poisson solver transforms a direction as a whole, so a periodic electric
!   potential and a periodic mesh have to agree; mismatching them would silently
!   invert the wrong operator.
!------------------------------------------------------------------------------
    if( any(ibc_ep == IBC_PERIODIC) .neqv. any(ibc_pr == IBC_PERIODIC) ) &
      call Print_error_msg('ebc'//dir//' periodicity does not match the mesh periodicity in '//dir//'.')
    if( (ibc_ep(1) == IBC_PERIODIC) .neqv. (ibc_ep(2) == IBC_PERIODIC) ) &
      call Print_error_msg('ebc'//dir//' must be periodic on both sides or neither.')

    return
  end subroutine resolve_electrical_bc

!==============================================================================
!> \brief Boundary condition for one component of the current density on one
!>        pair of sides.
!>
!> The current density obeys the *electrical* boundary condition, not the
!> velocity one. Copying the velocity codes and then forcing all nine fbc planes
!> to zero - which is what this used to do - imposes j = 0 as a vector at every
!> wall. Only its normal component is zero there.
!>
!> On an insulating wall (EBC_INSULATING, resolved to IBC_NEUMANN on ep):
!>   - j.n = 0 by definition of insulating, so the normal component is Dirichlet
!>     with the zero already stored in the fbc planes;
!>   - the tangential components are not constrained at all. In Hartmann flow
!>     the wall-tangential return current j_z(wall) = sigma*E_z is finite, and
!>     with B = (0, By, 0) the Lorentz force at the wall is f_x = -j_z*B_y and
!>     f_z = j_x*B_y - built entirely from the tangential components. Pinning
!>     them to zero removes the wall Lorentz force itself. They are therefore
!>     extrapolated from the interior (IBC_INTRPL).
!>
!> Periodic and interior (pipe axis) sides are passed through: they are not
!> physical boundaries, and the axis slots are filled by update_fbcy_cc_mhd_halo,
!> which asserts IBC_INTERIOR on exactly these codes.
!>
!> Note that max|div(j)| cannot see this change: the discrete charge balance is
!> satisfied by construction once the normal face fluxes are dropped, whatever
!> the tangential components do. The justification here is physical, not a
!> metric that moved.
!>
!> - ibc_ep    (in ): resolved electric-potential BC for the two sides
!> - ibc_q     (in ): velocity BC of the same direction, the fallback
!> - ibc_j     (out): BC for this current-density component
!> - dir       (in ): direction label, for the warning text only
!> - is_normal (in ): true when this component is normal to the two sides
!==============================================================================
  subroutine build_current_ibc(ibc_ep, ibc_q, ibc_j, dir, is_normal)
    use print_msg_mod, only : Print_warning_msg
    use mpi_mod, only : nrank
    implicit none
    integer,          intent(in)  :: ibc_ep(2)
    integer,          intent(in)  :: ibc_q(2)
    integer,          intent(out) :: ibc_j(2)
    character(len=*), intent(in)  :: dir
    logical,          intent(in)  :: is_normal

    integer :: n

    do n = 1, 2
      select case(ibc_ep(n))
      case(IBC_PERIODIC, IBC_INTERIOR)
        ibc_j(n) = ibc_ep(n)
      case(IBC_NEUMANN)
        if(is_normal) then
          ibc_j(n) = IBC_DIRICHLET ! j.n = 0, the fbc plane carries the zero
        else
          ibc_j(n) = IBC_INTRPL    ! tangential current is free at an insulating wall
        end if
      case default
        ! Only the insulating wall is derived here. Anything else - today that
        ! means the unreachable conducting branch - keeps the previous behaviour
        ! rather than guessing, and says so.
        ibc_j(n) = ibc_q(n)
        if(nrank == 0) call Print_warning_msg('Current-density b.c. in '//dir// &
          ' falls back to the velocity b.c.: no rule for this electrical condition.')
      end select
    end do

    return
  end subroutine build_current_ibc

!==============================================================================
  subroutine initialise_static_magnetic_field(mh, dm)
    use math_mod
    use udf_type_mod
    implicit none
    type(t_mhd),    intent(inout) :: mh
    type(t_domain), intent(in)    :: dm

    integer :: i, j, k, kk, jj, jmax
    real(WP) :: theta, br, btheta
    real(WP) :: br_l, br_r, btheta_l, btheta_r

    mh%bx = mh%B_static(1)
    mh%fbcx_bx(:, :, :) = mh%B_static(1)
    mh%fbcy_bx(:, :, :) = mh%B_static(1)
    mh%fbcz_bx(:, :, :) = mh%B_static(1)

    if(dm%icoordinate /= ICYLINDRICAL) then
      mh%by = mh%B_static(2)
      mh%bz = mh%B_static(3)
      mh%fbcx_by(:, :, :) = mh%B_static(2)
      mh%fbcy_by(:, :, :) = mh%B_static(2)
      mh%fbcz_by(:, :, :) = mh%B_static(2)
      mh%fbcx_bz(:, :, :) = mh%B_static(3)
      mh%fbcy_bz(:, :, :) = mh%B_static(3)
      mh%fbcz_bz(:, :, :) = mh%B_static(3)
      return
    end if

!------------------------------------------------------------------------------
!   B_static is a global Cartesian vector (Bx, By, Bz). In cylindrical geometry,
!   the stored radial component follows qy storage: by = r * Br.
!   With the mesh mapping y = r*cos(theta), z = r*sin(theta), the basis is
!   e_r = (cos, sin) and e_theta = d(e_r)/d(theta) = (-sin, cos), so
!     Br     =  By * cos(theta) + Bz * sin(theta)
!     Btheta = -By * sin(theta) + Bz * cos(theta)
!   That mapping makes (e_x, e_r, e_theta) right-handed, which matters here more
!   than it does for gravity: cross_production_mhd evaluates u x B and j x B with
!   the right-handed formula c_1 = a_2 b_3 - a_3 b_2 on the (x, r, theta) triple.
!   Under the earlier left-handed mapping both of those carried a global minus,
!   which cancelled in the Lorentz force (ep and j simply came out sign-flipped)
!   but left every reported current and potential negated. See
!   build_cylindrical_to_cart in io_visulisation.f90 for the single convention.
!------------------------------------------------------------------------------
    do k = 1, dm%dcpc%xsz(3)
      kk = dm%dcpc%xst(3) + k - 1
      theta = (real(kk - 1, WP) + HALF) * dm%h(3)
      br = mh%B_static(2) * cos_wp(theta) + mh%B_static(3) * sin_wp(theta)
      do j = 1, dm%dcpc%xsz(2)
        jj = dm%dcpc%xst(2) + j - 1
        mh%by(:, j, k) = dm%rp(jj) * br
      end do
    end do

    do k = 1, dm%dccp%xsz(3)
      kk = dm%dccp%xst(3) + k - 1
      theta = real(kk - 1, WP) * dm%h(3)
      btheta = - mh%B_static(2) * sin_wp(theta) + mh%B_static(3) * cos_wp(theta)
      mh%bz(:, :, k) = btheta
    end do

    do k = 1, dm%dcpc%xsz(3)
      kk = dm%dcpc%xst(3) + k - 1
      theta = (real(kk - 1, WP) + HALF) * dm%h(3)
      br = mh%B_static(2) * cos_wp(theta) + mh%B_static(3) * sin_wp(theta)
      do j = 1, dm%dcpc%xsz(2)
        jj = dm%dcpc%xst(2) + j - 1
        mh%fbcx_by(:, j, k) = dm%rp(jj) * br
      end do
    end do

    do k = 1, dm%dccp%xsz(3)
      kk = dm%dccp%xst(3) + k - 1
      theta = real(kk - 1, WP) * dm%h(3)
      btheta = - mh%B_static(2) * sin_wp(theta) + mh%B_static(3) * cos_wp(theta)
      mh%fbcx_bz(:, :, k) = btheta
    end do

    jmax = dm%dcpc%yst(2) + dm%dcpc%ysz(2) - 1
    do k = 1, dm%dcpc%ysz(3)
      kk = dm%dcpc%yst(3) + k - 1
      theta = (real(kk - 1, WP) + HALF) * dm%h(3)
      br = mh%B_static(2) * cos_wp(theta) + mh%B_static(3) * sin_wp(theta)
      do i = 1, dm%dcpc%ysz(1)
        mh%fbcy_by(i, 1, k) = dm%rp(1) * br
        mh%fbcy_by(i, 2, k) = dm%rp(jmax) * br
      ! Slots 3/4 are the *second* ghost layer, read only when the corresponding side
      ! is IBC_INTERIOR. Duplicating slots 1/2 into them is correct for the Dirichlet
      ! walls of an annulus, where they are never read, and it is harmless for a pipe
      ! only because update_fbcy_cc_mhd_halo rewrites slots 1 and 3 from the axis
      ! mirror before any operator sees them. It is a placeholder, not a value.
        mh%fbcy_by(i, 3, k) = mh%fbcy_by(i, 1, k)
        mh%fbcy_by(i, 4, k) = mh%fbcy_by(i, 2, k)
      end do
    end do

    do k = 1, dm%dccp%ysz(3)
      kk = dm%dccp%yst(3) + k - 1
      theta = real(kk - 1, WP) * dm%h(3)
      btheta = - mh%B_static(2) * sin_wp(theta) + mh%B_static(3) * cos_wp(theta)
      mh%fbcy_bz(:, :, k) = btheta
    end do

    theta = ZERO
    br_l = mh%B_static(2) * cos_wp(theta) + mh%B_static(3) * sin_wp(theta)
    btheta_l = - mh%B_static(2) * sin_wp(theta) + mh%B_static(3) * cos_wp(theta)
    theta = dm%lzz
    br_r = mh%B_static(2) * cos_wp(theta) + mh%B_static(3) * sin_wp(theta)
    btheta_r = - mh%B_static(2) * sin_wp(theta) + mh%B_static(3) * cos_wp(theta)
    do j = 1, dm%dcpc%zsz(2)
      jj = dm%dcpc%zst(2) + j - 1
      mh%fbcz_by(:, j, 1) = dm%rp(jj) * br_l
      mh%fbcz_by(:, j, 2) = dm%rp(jj) * br_r
      mh%fbcz_by(:, j, 3) = mh%fbcz_by(:, j, 1)
      mh%fbcz_by(:, j, 4) = mh%fbcz_by(:, j, 2)
    end do
    mh%fbcz_bz(:, :, 1) = btheta_l
    mh%fbcz_bz(:, :, 2) = btheta_r
    mh%fbcz_bz(:, :, 3) = btheta_l
    mh%fbcz_bz(:, :, 4) = btheta_r

    return
  end subroutine

  subroutine cross_production_mhd(fl, mh, ab_cross_x, ab_cross_y, ab_cross_z, str, dm)
    use bc_dirichlet_mod
    use boundary_conditions_mod
    use cylindrical_rn_mod
    use decomp_2d
    use operations
    use print_msg_mod
    use udf_type_mod
    implicit none
    type(t_flow), intent(in) :: fl
    type(t_mhd),  intent(in) :: mh
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent(out) :: ab_cross_x
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent(out) :: ab_cross_y
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent(out) :: ab_cross_z
    character(8), intent(in) :: str

    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: ax, bx
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: ay, by
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) :: az, bz
    integer :: ibcx_ax(2), ibcy_ax(2), ibcz_ax(2)
    integer :: ibcx_ay(2), ibcy_ay(2), ibcz_ay(2)
    integer :: ibcx_az(2), ibcy_az(2), ibcz_az(2)
    integer :: ibcx_bx(2), ibcy_bx(2), ibcz_bx(2)
    integer :: ibcx_by(2), ibcy_by(2), ibcz_by(2)
    integer :: ibcx_bz(2), ibcy_bz(2), ibcz_bz(2)
    integer :: n, iacc
    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_ax, fbcx_bx
    real(WP), dimension( 4, dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: fbcx_ay, fbcx_by
    real(WP), dimension( 4, dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: fbcx_az, fbcx_bz

    real(WP), dimension( dm%dpcc%ysz(1), 4, dm%dpcc%ysz(3) ) :: fbcy_ax, fbcy_bx
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_ay, fbcy_by
    real(WP), dimension( dm%dccp%ysz(1), 4, dm%dccp%ysz(3) ) :: fbcy_az, fbcy_bz

    ! The radial component (ay = r*u_r or r*J_r, by = r*B_r) is interpolated from a
    ! y-node layout down to cells twice: once on dppc and once on dcpp. Both P2C
    ! sweeps need their own y-boundary plane, and neither fbcy_ay nor fbcy_by can be
    ! reused because those are dcpc-shaped. Without them Get_y_midp_P2C_1D demotes
    ! the pipe axis from IBC_INTERIOR to IBC_INTRPL and one-sidedly extrapolates
    ! across r = 0 instead of mirroring - dormant at CD2, first order at CD4/CP4/CP6.
    real(WP), dimension( dm%dppc%ysz(1), 4, dm%dppc%ysz(3) ) :: fbcy_ppc
    real(WP), dimension( dm%dcpp%ysz(1), 4, dm%dcpp%ysz(3) ) :: fbcy_cpp

    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), 4 ) :: fbcz_ax, fbcz_bx
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4 ) :: fbcz_ay, fbcz_by
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_az, fbcz_bz

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil

    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) ::   acpc_ypencil, &
                                                                             ax_cpc_ypencil, &
                                                                             bx_cpc_ypencil, &
                                                                             az_cpc_ypencil, &
                                                                             bz_cpc_ypencil

    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) ::   accp_zpencil, &
                                                                             ax_ccp_zpencil, &
                                                                             bx_ccp_zpencil, &
                                                                             ay_ccp_zpencil, &
                                                                             by_ccp_zpencil

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) ::   apcc_xpencil, &
                                                                             ay_pcc_xpencil, &
                                                                             by_pcc_xpencil, &
                                                                             az_pcc_xpencil, &
                                                                             bz_pcc_xpencil

    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: accp_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dpcp%xsz(1), dm%dpcp%xsz(2), dm%dpcp%xsz(3) ) :: apcp_xpencil
    real(WP), dimension( dm%dppc%xsz(1), dm%dppc%xsz(2), dm%dppc%xsz(3) ) :: appc_xpencil

    real(WP), dimension( dm%dppc%ysz(1), dm%dppc%ysz(2), dm%dppc%ysz(3) ) :: appc_ypencil
    real(WP), dimension( dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3) ) :: apcc_ypencil
    real(WP), dimension( dm%dpcp%ysz(1), dm%dpcp%ysz(2), dm%dpcp%ysz(3) ) :: apcp_ypencil
    real(WP), dimension( dm%dcpp%ysz(1), dm%dcpp%ysz(2), dm%dcpp%ysz(3) ) :: acpp_ypencil

    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil
    real(WP), dimension( dm%dcpp%zsz(1), dm%dcpp%zsz(2), dm%dcpp%zsz(3) ) :: acpp_zpencil
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil
    real(WP), dimension( dm%dpcp%zsz(1), dm%dpcp%zsz(2), dm%dpcp%zsz(3) ) :: apcp_zpencil


    iacc = dm%iAccuracy
    if(trim(str) == 'ub_cross') then
      ax = fl%qx
      ay = fl%qy
      az = fl%qz
      ibcx_ax = dm%ibcx_qx
      ibcy_ax = dm%ibcy_qx
      ibcz_ax = dm%ibcz_qx
      ibcx_ay = dm%ibcx_qy
      ibcy_ay = dm%ibcy_qy
      ibcz_ay = dm%ibcz_qy
      ibcx_az = dm%ibcx_qz
      ibcy_az = dm%ibcy_qz
      ibcz_az = dm%ibcz_qz

      fbcx_ax = dm%fbcx_qx
      fbcy_ax = dm%fbcy_qx
      fbcz_ax = dm%fbcz_qx
      fbcx_ay = dm%fbcx_qy
      fbcy_ay = dm%fbcy_qy
      fbcz_ay = dm%fbcz_qy
      fbcx_az = dm%fbcx_qz
      fbcy_az = dm%fbcy_qz
      fbcz_az = dm%fbcz_qz
    else if(trim(str) == 'jb_cross') then
      ax = mh%jx
      ay = mh%jy
      az = mh%jz
      ibcx_ax = mh%ibcx_jx
      ibcy_ax = mh%ibcy_jx
      ibcz_ax = mh%ibcz_jx
      ibcx_ay = mh%ibcx_jy
      ibcy_ay = mh%ibcy_jy
      ibcz_ay = mh%ibcz_jy
      ibcx_az = mh%ibcx_jz
      ibcy_az = mh%ibcy_jz
      ibcz_az = mh%ibcz_jz

      fbcx_ax = mh%fbcx_jx
      fbcy_ax = mh%fbcy_jx
      fbcz_ax = mh%fbcz_jx
      fbcx_ay = mh%fbcx_jy
      fbcy_ay = mh%fbcy_jy
      fbcz_ay = mh%fbcz_jy
      fbcx_az = mh%fbcx_jz
      fbcy_az = mh%fbcy_jz
      fbcz_az = mh%fbcz_jz
    else
      call Print_error_msg('The required cross production is not supported.')
    end if
    bx = mh%bx
    by = mh%by
    bz = mh%bz
    ibcx_bx = mh%ibcx_bx
    ibcy_bx = mh%ibcy_bx
    ibcz_bx = mh%ibcz_bx
    ibcx_by = mh%ibcx_by
    ibcy_by = mh%ibcy_by
    ibcz_by = mh%ibcz_by
    ibcx_bz = mh%ibcx_bz
    ibcy_bz = mh%ibcy_bz
    ibcz_bz = mh%ibcz_bz
    fbcx_bx = mh%fbcx_bx
    fbcy_bx = mh%fbcy_bx
    fbcz_bx = mh%fbcz_bx
    fbcx_by = mh%fbcx_by
    fbcy_by = mh%fbcy_by
    fbcz_by = mh%fbcz_by
    fbcx_bz = mh%fbcx_bz
    fbcy_bz = mh%fbcy_bz
    fbcz_bz = mh%fbcz_bz
!------------------------------------------------------------------------------
! preparation for u_cross_b for staggered vector
!------------------------------------------------------------------------------
! ax_pcc_xpencil to ax_cpc_ypencil
    apcc_xpencil = ax
    call transpose_x_to_y (apcc_xpencil, apcc_ypencil, dm%dpcc)
    call Get_y_midp_C2P_3D(apcc_ypencil, appc_ypencil, dm, iacc, ibcy_ax(:), fbcy_ax(:, :, :))
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(appc_ypencil, IPENCIL(2), fbcy_ax, dm%knc_sym, dm%dppc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    end if

    call transpose_y_to_x (appc_ypencil, appc_xpencil, dm%dppc)
    call Get_x_midp_P2C_3D(appc_xpencil, acpc_xpencil, dm, iacc, ibcx_ax(:))
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc)
    ax_cpc_ypencil = acpc_ypencil
! ax_pcc_xpencil to ax_ccp_zpencil
    call transpose_y_to_z (apcc_ypencil, apcc_zpencil, dm%dpcc)
    call Get_z_midp_C2P_3D(apcc_zpencil, apcp_zpencil, dm, iacc, ibcz_ax(:), fbcz_ax(:, :, :))
    call transpose_z_to_y (apcp_zpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_x (apcp_ypencil, apcp_xpencil, dm%dpcp)
    call Get_x_midp_P2C_3D(apcp_xpencil, accp_xpencil, dm, iacc, ibcx_ax(:))
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    ax_ccp_zpencil = accp_zpencil

! bx_pcc_xpencil to bx_cpc_ypencil
    apcc_xpencil = bx
    call transpose_x_to_y (apcc_xpencil, apcc_ypencil, dm%dpcc)
    call Get_y_midp_C2P_3D(apcc_ypencil, appc_ypencil, dm, iacc, ibcy_bx(:), fbcy_bx(:, :, :))
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(appc_ypencil, IPENCIL(2), fbcy_bx, dm%knc_sym, dm%dppc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    end if
    call transpose_y_to_x (appc_ypencil, appc_xpencil, dm%dppc)
    call Get_x_midp_P2C_3D(appc_xpencil, acpc_xpencil, dm, iacc, ibcx_bx(:))
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc)
    bx_cpc_ypencil = acpc_ypencil
! bx_pcc_xpencil to bx_ccp_zpencil
    call transpose_y_to_z (apcc_ypencil, apcc_zpencil, dm%dpcc)
    call Get_z_midp_C2P_3D(apcc_zpencil, apcp_zpencil, dm, iacc, ibcz_bx(:), fbcz_bx(:, :, :))
    call transpose_z_to_y (apcp_zpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_x (apcp_ypencil, apcp_xpencil, dm%dpcp)
    call Get_x_midp_P2C_3D(apcp_xpencil, accp_xpencil, dm, iacc, ibcx_bx(:))
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    bx_ccp_zpencil = accp_zpencil
!------------------------------------------------------------------------------
! ay_cpc_xpencil to ay_pcc_xpencil
    acpc_xpencil = ay
    call Get_x_midp_C2P_3D(acpc_xpencil, appc_xpencil, dm, iacc, ibcx_ay(:), fbcx_ay(:, :, :))
    call transpose_x_to_y (appc_xpencil, appc_ypencil, dm%dppc)
    ! ay = r * u_r (or r * J_r) is even across the axis, hence no sign reversal.
    call extract_dirichlet_fbcy(fbcy_ppc, appc_ypencil, dm%dppc, dm)
    call Get_y_midp_P2C_3D(appc_ypencil, apcc_ypencil, dm, iacc, ibcy_ay(:), fbcy_ppc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    ay_pcc_xpencil = apcc_xpencil
! ay_cpc_xpencil to ay_ccp_zpencil
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc)
    call transpose_y_to_z (acpc_ypencil, acpc_zpencil, dm%dcpc)
    call Get_z_midp_C2P_3D(acpc_zpencil, acpp_zpencil, dm, iacc, ibcz_ay(:), fbcz_ay(:, :, :))
    call transpose_z_to_y (acpp_zpencil, acpp_ypencil, dm%dcpp)
    call extract_dirichlet_fbcy(fbcy_cpp, acpp_ypencil, dm%dcpp, dm)
    call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, iacc, ibcy_ay(:), fbcy_cpp)
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    ay_ccp_zpencil = accp_zpencil

! by_cpc_xpencil to by_pcc_xpencil
    acpc_xpencil = by
    call Get_x_midp_C2P_3D(acpc_xpencil, appc_xpencil, dm, iacc, ibcx_by(:), fbcx_by(:, :, :))
    call transpose_x_to_y (appc_xpencil, appc_ypencil, dm%dppc)
    ! by = r * B_r follows qy storage, so it is even across the axis as well.
    call extract_dirichlet_fbcy(fbcy_ppc, appc_ypencil, dm%dppc, dm)
    call Get_y_midp_P2C_3D(appc_ypencil, apcc_ypencil, dm, iacc, ibcy_by(:), fbcy_ppc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    by_pcc_xpencil = apcc_xpencil
! by_cpc_xpencil to by_ccp_zpencil
    call transpose_x_to_y (acpc_xpencil, acpc_ypencil, dm%dcpc)
    call transpose_y_to_z (acpc_ypencil, acpc_zpencil, dm%dcpc)
    call Get_z_midp_C2P_3D(acpc_zpencil, acpp_zpencil, dm, iacc, ibcz_by(:), fbcz_by(:, :, :))
    call transpose_z_to_y (acpp_zpencil, acpp_ypencil, dm%dcpp)
    call extract_dirichlet_fbcy(fbcy_cpp, acpp_ypencil, dm%dcpp, dm)
    call Get_y_midp_P2C_3D(acpp_ypencil, accp_ypencil, dm, iacc, ibcy_by(:), fbcy_cpp)
    call transpose_y_to_z (accp_ypencil, accp_zpencil, dm%dccp)
    by_ccp_zpencil = accp_zpencil
!------------------------------------------------------------------------------
! az_ccp_xpencil to az_cpc_ypencil
    accp_xpencil = az
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call Get_y_midp_C2P_3D(accp_ypencil, acpp_ypencil, dm, iacc, ibcy_az(:), fbcy_az(:, :, :))
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(acpp_ypencil, IPENCIL(2), fbcy_az, dm%knc_sym, dm%dcpp, is_ynode = .true., is_odd = .true., &
                            axis_mode = AXIS_RECON_M1, assign_axis_to_var = .true., nr = 0, opt_dz = dm%h(3))
    end if
    call transpose_y_to_z (acpp_ypencil, acpp_zpencil, dm%dcpp)
    call Get_z_midp_P2C_3D(acpp_zpencil, acpc_zpencil, dm, iacc, ibcz_az(:))
    call transpose_z_to_y (acpc_zpencil, acpc_ypencil, dm%dcpc)
    az_cpc_ypencil = acpc_ypencil
! az_ccp_xpencil to az_pcc_xpencil
    call Get_x_midp_C2P_3D(accp_xpencil, apcp_xpencil, dm, iacc, ibcx_az(:), fbcx_az(:, :, :))
    call transpose_x_to_y (apcp_xpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_z (apcp_ypencil, apcp_zpencil, dm%dpcp)
    call Get_z_midp_P2C_3D(apcp_zpencil, apcc_zpencil, dm, iacc, ibcz_az(:))
    call transpose_z_to_y (apcc_zpencil, apcc_ypencil, dm%dpcc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    az_pcc_xpencil = apcc_xpencil
! bz_ccp_xpencil to bz_cpc_ypencil
    accp_xpencil = bz
    call transpose_x_to_y (accp_xpencil, accp_ypencil, dm%dccp)
    call Get_y_midp_C2P_3D(accp_ypencil, acpp_ypencil, dm, iacc, ibcy_bz(:), fbcy_bz(:, :, :))
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(acpp_ypencil, IPENCIL(2), fbcy_bz, dm%knc_sym, dm%dcpp, is_ynode = .true., is_odd = .true., &
                            axis_mode = AXIS_RECON_M1, assign_axis_to_var = .true., nr = 0, opt_dz = dm%h(3))
    end if
    call transpose_y_to_z (acpp_ypencil, acpp_zpencil, dm%dcpp)
    call Get_z_midp_P2C_3D(acpp_zpencil, acpc_zpencil, dm, iacc, ibcz_bz(:))
    call transpose_z_to_y (acpc_zpencil, acpc_ypencil, dm%dcpc)
    bz_cpc_ypencil = acpc_ypencil
! bz_ccp_xpencil to bz_pcc_xpencil
    call Get_x_midp_C2P_3D(accp_xpencil, apcp_xpencil, dm, iacc, ibcx_bz(:), fbcx_bz(:, :, :))
    call transpose_x_to_y (apcp_xpencil, apcp_ypencil, dm%dpcp)
    call transpose_y_to_z (apcp_ypencil, apcp_zpencil, dm%dpcp)
    call Get_z_midp_P2C_3D(apcp_zpencil, apcc_zpencil, dm, iacc, ibcz_bz(:))
    call transpose_z_to_y (apcc_zpencil, apcc_ypencil, dm%dpcc)
    call transpose_y_to_x (apcc_ypencil, apcc_xpencil, dm%dpcc)
    bz_pcc_xpencil = apcc_xpencil
!------------------------------------------------------------------------------
! Compute the cross product of two vectors (ux, uy, uz) and (bx, by, bz)
! The resulting vector (cx, cy, cz) is given by:
! cx = uy * bz - uz * by; locates at (i', j, k); require y(cpc)->y(pcc); z(ccp)->z(pcc)
! cy = uz * bx - ux * bz; locates at (i, j', k); require x(pcc)->x(cpc); z(ccp)->z(cpc)
! cz = ux * by - uy * bx; locates at (i, j, k'); require x(pcc)->x(ccp); y(cpc)->y(ccp)
! This follows the right-hand rule and produces a vector perpendicular to both input vectors.
!------------------------------------------------------------------------------
    if(dm%icoordinate == ICYLINDRICAL) then
      apcc_xpencil = ay_pcc_xpencil * bz_pcc_xpencil - az_pcc_xpencil * by_pcc_xpencil
      call multiple_cylindrical_rn(apcc_xpencil, dm%dpcc, dm%rci, 1, IPENCIL(1))

      acpc_ypencil = az_cpc_ypencil * bx_cpc_ypencil - ax_cpc_ypencil * bz_cpc_ypencil
      call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rp, 1, IPENCIL(2))

      accp_zpencil = ax_ccp_zpencil * by_ccp_zpencil - ay_ccp_zpencil * bx_ccp_zpencil
      call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    else
      apcc_xpencil = ay_pcc_xpencil * bz_pcc_xpencil - az_pcc_xpencil * by_pcc_xpencil
      acpc_ypencil = az_cpc_ypencil * bx_cpc_ypencil - ax_cpc_ypencil * bz_cpc_ypencil
      accp_zpencil = ax_ccp_zpencil * by_ccp_zpencil - ay_ccp_zpencil * bx_ccp_zpencil
    end if
    ab_cross_x = apcc_xpencil
    call transpose_y_to_x(acpc_ypencil, ab_cross_y,   dm%dcpc)
    call transpose_z_to_y(accp_zpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_x(accp_ypencil, ab_cross_z,   dm%dccp)

    return
  end subroutine
!==============================================================================
  subroutine compute_Lorentz_force(fl, mh, dm)
    use bc_dirichlet_mod
    use boundary_conditions_mod
    use continuity_eq_mod
    use cylindrical_rn_mod
    use decomp_2d
    use operations
    use poisson_interface_mod
    use udf_type_mod
    use visualisation_field_mod
    implicit none
!------------------------------------------------------------------------------
! calculate the Lozrentz-force based on a static magnetic field B, B is time-independent
!------------------------------------------------------------------------------
    type(t_flow), intent(inout) :: fl
    type(t_mhd),  intent(inout) :: mh
    type(t_domain), intent(in)  :: dm

    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: ub_cross_x
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: ub_cross_y
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) :: ub_cross_z
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: apcc_xpencil
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: acpc_xpencil
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)) :: accp_xpencil
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: accc_ypencil
    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: acpc_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: accp_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: accc_zpencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: accp_zpencil
    real(WP), dimension(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3))              :: fbcy_c4c
    real(WP), dimension(4, dm%dccc%xsz(2), dm%dccc%xsz(3))              :: fbcx_4cc
    ! Boundary planes of u x B, for the divergence that forms the Poisson source.
    real(WP), dimension(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx_ub
    real(WP), dimension(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) :: fbcy_ub
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) :: fbcz_ub
    integer :: iacc_prj(NDIM)
    !
!------------------------------------------------------------------------------
!   grad(ep) below must be differenced with the same scheme the FFT Poisson at
!   :600 inverts, exactly as the pressure gradient is in eq_momentum2. The
!   electric potential satisfies lap(ep) = div(u x B) and the current is then
!   j = -grad(ep) + u x B, so div(j) = 0 holds discretely only if D.G reproduces
!   the solver's operator; with a mismatched scheme it does not, and the charge
!   conservation the whole MHD coupling rests on is lost. Get_divergence_vector
!   builds the right-hand side with the same per-direction choice.
!------------------------------------------------------------------------------
    iacc_prj = get_projection_accuracy(dm)
    mh%iteration = fl%iteration
!------------------------------------------------------------------------------
! calculate vector u cross-product vector b in x-pencil
!------------------------------------------------------------------------------
    ub_cross_x = ZERO
    ub_cross_y = ZERO
    ub_cross_z = ZERO
    call cross_production_mhd(fl, mh, ub_cross_x, ub_cross_y, ub_cross_z, 'ub_cross', dm)
#ifdef DEBUG_STEPS
    call write_visu_any3darray(ub_cross_x, 'ub_cross_x', 'debug', dm%dpcc, dm, fl%iteration)
    call write_visu_any3darray(ub_cross_y, 'ub_cross_y', 'debug', dm%dcpc, dm, fl%iteration)
    call write_visu_any3darray(ub_cross_z, 'ub_cross_z', 'debug', dm%dccp, dm, fl%iteration)
#endif
!------------------------------------------------------------------------------
! insulating planes: drop the (u x B) normal flux
!
! An insulating boundary carries no current, j.n = 0, which by Ohm's law
! j = -grad(ep) + u x B means d(ep)/dn = (u x B).n - an *inhomogeneous* Neumann
! condition. The FFT solver only offers the homogeneous one, so impose it in flux
! form instead: fold the known d(ep)/dn into the right-hand side, where it cancels
! the (u x B).n boundary term of div(u x B) exactly, leaving the same solve with
! the normal flux removed from the source. Zeroing the boundary plane of the array
! does precisely that, because the divergence below is a P2C derivative and reads
! the boundary face from the array, not from the fbc slots. It also makes the
! current itself right: with d(ep)/dn = 0 from the homogeneous solve, the face
! value j.n = -0 + 0 is exactly zero, and D.j = -L(ep) + S then closes in the
! first cell as well as in the interior.
!
! Without this the source violates the compatibility condition of the singular
! all-Neumann system, sum(S*V) = closed_integral (u x B).n /= 0. The solver
! projects the constant out and what remains is a *uniform*
! div(j) = (1/V) closed_integral (u x B).n spread over the whole domain - 3.3e-3
! in MHD_channel_scp_inout_Tw, whose outlet carries (u x B)_x = -B_y*u_z because
! its field is wall-normal only.
!
! The zeroing itself is orientation-agnostic: ub_cross_x holds the full component
! (u x B)_x = u_y*B_z - u_z*B_y, formed pointwise in cross_production_mhd, and the
! whole boundary plane of that array is dropped. B_x does not enter a flux through
! an x-normal face and needs no handling. So discrete charge conservation is
! restored for any uniform B - measured max|div(j)| is 2.9e-12 for B = (0,10,0)
! and 3.2e-10 for B = (0,10,5), both round-off, against 3.3e-3 unfixed.
!
! What *is* orientation-dependent is whether j.n = 0 is the right condition to
! impose, and the metric above cannot see it - div(j) = 0 holds by construction
! once the face fluxes are dropped, however wrong the boundary physics:
!
!   Wall-normal B (B_z = 0, or a small in-plane tilt). Pointwise j_x = 0 is
!   essentially exact. With mean flow along x the EMF is u_x*B_y in z, the current
!   loops close in the cross-plane, and z-mirror symmetry gives <u_z> = 0, so no
!   mean axial current wants to cross the plane in the first place.
!
!   Generally inclined B (B_z /= 0). That symmetry argument does not survive. A
!   nonzero B_z can drive a genuine mean secondary flow <u_z>(y) /= 0 - the
!   oblique-field duct effect - so recirculating current may legitimately cross
!   the cut plane. With insulating side walls and no imposed axial current source
!   the only guaranteed property is the *integral* one, closed_surface j_x dA = 0
!   over the cross-section; pointwise cancellation is then an approximation whose
!   error is invisible to max|div(j)|. Judge such a case on the physics, not on
!   this metric.
!
! Separately, and independent of orientation, this is an approximation to the
! unbounded-duct problem: it assumes the return-current loops close inside the
! domain. A duct with the magnet localised in [x1, x2] instead needs B tapered to
! zero before the cut planes, so that they sit in a field-free region.
!
! TODO (open question, not implemented): whether to offer a net-zero-current
! integral constraint - enforce closed_surface j_x dA = 0 over the plane while
! letting j_x vary pointwise within it - as an alternative mode for strongly
! inclined fields. It would be the weakest condition that still makes the singular
! system compatible, and so the honest one when the pointwise form is not
! justified. Needs a case that actually exercises it before it is worth building.
!------------------------------------------------------------------------------
    ! x needs no ownership test: every rank holds the full x extent of an x-pencil.
    if(mh%ibcx_ep(1) == IBC_NEUMANN) ub_cross_x(1, :, :) = ZERO
    if(mh%ibcx_ep(2) == IBC_NEUMANN) ub_cross_x(dm%dpcc%xsz(1), :, :) = ZERO
    ! y and z are pencil-decomposed in an x-pencil, so only the ranks holding the
    ! global first/last plane may touch it. Both are no-ops for a no-slip wall,
    ! where u = 0 makes u x B vanish anyway, but the condition is on the boundary
    ! being insulating, not on the wall being solid.
    if(mh%ibcy_ep(1) == IBC_NEUMANN .and. dm%dcpc%xst(2) == 1) &
      ub_cross_y(:, 1, :) = ZERO
    if(mh%ibcy_ep(2) == IBC_NEUMANN .and. dm%dcpc%xen(2) == dm%np(2)) &
      ub_cross_y(:, dm%dcpc%xsz(2), :) = ZERO
    if(mh%ibcz_ep(1) == IBC_NEUMANN .and. dm%dccp%xst(3) == 1) &
      ub_cross_z(:, :, 1) = ZERO
    if(mh%ibcz_ep(2) == IBC_NEUMANN .and. dm%dccp%xen(3) == dm%np(3)) &
      ub_cross_z(:, :, dm%dccp%xsz(3)) = ZERO
!------------------------------------------------------------------------------
! calculate div(ub_cross) in x-pencil
!------------------------------------------------------------------------------
!   u x B is a velocity-like staggered vector, so it shares the velocity's boundary
!   topology but not its boundary values: at an inlet dm%fbcx_qx is the prescribed
!   u_x profile and at the pipe axis dm%fbcy_qy is the qy mirror. Take the planes
!   from u x B itself - each component is node-centred in its own direction, so the
!   boundary value is already in the array, and extract_dirichlet_fbcy adds the
!   axis mirror for a pipe (even parity: ub_cross_y stores r*(u x B)_r, as qy does).
    call extract_dirichlet_fbcx(fbcx_ub, ub_cross_x, dm%dpcc)
    call transpose_x_to_y(ub_cross_y, acpc_ypencil, dm%dcpc)
    call extract_dirichlet_fbcy(fbcy_ub, acpc_ypencil, dm%dcpc, dm)
    call transpose_x_to_y(ub_cross_z, accp_ypencil, dm%dccp)
    call transpose_y_to_z(accp_ypencil, accp_zpencil, dm%dccp)
    call extract_dirichlet_fbcz(fbcz_ub, accp_zpencil, dm%dccp)
    call Get_divergence_vector(ub_cross_x, ub_cross_y, ub_cross_z, mh%ep, dm, &
                               fbcx_ub, fbcy_ub, fbcz_ub)
#ifdef DEBUG_STEPS
    call write_visu_any3darray(mh%ep, 'ep1', 'debug', dm%dccc, dm, fl%iteration)
#endif
!------------------------------------------------------------------------------
! solving the Poisson equation for the electric potential
!
! In cylindrical coordinates the FFT/TDMA solver does not invert lap(ep) but the
! r^2-weighted form
!   r^2 lap(ep) = r d/dr(r d(ep)/dr) + d2(ep)/dth2 + r^2 d2(ep)/dx2,
! which is what makes the operator separable in x and theta (see the tridiagonal
! assembly in poisson_1stderivcomp_fft2d, where aa/cc carry rp*rc and the axial
! wavenumber carries rc^2). The caller therefore owes the solver a source already
! multiplied by r^2, exactly as solve_pressure_poisson does for fl%pcor. Without
! it ep is not the potential, and j = -grad(ep) + u x B is not discretely
! solenoidal - charge conservation, which the whole MHD coupling rests on, is
! lost at O(1) in every cylindrical case.
!------------------------------------------------------------------------------
    if (dm%icoordinate == ICYLINDRICAL) then
      call multiple_cylindrical_rn(mh%ep, dm%dccc, dm%rc, 2, IPENCIL(1))
    end if
    call solve_fft_poisson(mh%ep, dm)
    if(dm%icase == ICASE_PIPE) call update_fbcy_cc_mhd_halo(mh, dm)
#ifdef DEBUG_STEPS
    call write_visu_any3darray(mh%ep, 'ep2', 'debug', dm%dccc, dm, fl%iteration)
#endif
!------------------------------------------------------------------------------
! calculate the current density jx, jy, jz (a vector)
!------------------------------------------------------------------------------
    call Get_x_1der_C2P_3D(mh%ep, apcc_xpencil, dm, iacc_prj(1), mh%ibcx_ep, mh%fbcx_ep )
    mh%jx = - apcc_xpencil + ub_cross_x
    call transpose_x_to_y (mh%ep, accc_ypencil, dm%dccc)
    call Get_y_1der_C2P_3D(accc_ypencil, acpc_ypencil, dm, iacc_prj(2), mh%ibcy_ep, mh%fbcy_ep)
    fbcy_c4c = MAXP
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(acpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .true., &
                            axis_mode = AXIS_RECON_M1, assign_axis_to_var = .true., nr = 0, opt_dz = dm%h(3))
    end if
    call transpose_y_to_x (acpc_ypencil, acpc_xpencil, dm%dcpc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(acpc_xpencil, dm%dcpc, dm%rp, 1, IPENCIL(1))
    mh%jy = - acpc_xpencil + ub_cross_y
    call transpose_y_to_z (accc_ypencil, accc_zpencil, dm%dccc)
    call Get_z_1der_C2P_3D(accc_zpencil, accp_zpencil, dm, iacc_prj(3), mh%ibcz_ep, mh%fbcz_ep)
    call transpose_z_to_y (accp_zpencil, accp_ypencil, dm%dccp)
    call transpose_y_to_x (accp_ypencil, accp_xpencil, dm%dccp)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_xpencil, dm%dccp, dm%rci, 1, IPENCIL(1))
    mh%jz = - accp_xpencil + ub_cross_z
    if(dm%icase == ICASE_PIPE) call update_fbcy_cc_mhd_halo(mh, dm)
#ifdef DEBUG_STEPS
    call write_visu_any3darray(mh%jx, 'jx', 'debug', dm%dpcc, dm, fl%iteration)
    call write_visu_any3darray(mh%jy, 'jy', 'debug', dm%dcpc, dm, fl%iteration)
    call write_visu_any3darray(mh%jz, 'jz', 'debug', dm%dccp, dm, fl%iteration)
#endif
!------------------------------------------------------------------------------
! calculate the Lorentz force lrfx, lrfy, lrfz (a vector)
!------------------------------------------------------------------------------
    call cross_production_mhd(fl, mh, fl%lrfx, fl%lrfy, fl%lrfz, 'jb_cross', dm)
!------------------------------------------------------------------------------
! un-dimensionlise the value
!------------------------------------------------------------------------------
    fl%lrfx = fl%lrfx * mh%Nstuart
    fl%lrfy = fl%lrfy * mh%Nstuart
    fl%lrfz = fl%lrfz * mh%Nstuart
#ifdef DEBUG_STEPS
    call write_visu_any3darray(fl%lrfx, 'lrfx', 'debug', dm%dpcc, dm, fl%iteration)
    call write_visu_any3darray(fl%lrfy, 'lrfy', 'debug', dm%dcpc, dm, fl%iteration)
    call write_visu_any3darray(fl%lrfz, 'lrfz', 'debug', dm%dccp, dm, fl%iteration)
#endif
  return
  end subroutine

!==============================================================================
  subroutine check_current_conservation(fl, mh, dm)
    use bc_dirichlet_mod
    use continuity_eq_mod
    use decomp_2d
    use find_max_min_ave_mod
    use udf_type_mod
    use wtformat_mod
    implicit none
    type(t_flow), intent(inout) :: fl
    type(t_mhd),  intent(in) :: mh
    type(t_domain), intent(in) :: dm
    real(WP) :: intg_m, intg_fbcx(2), intg_fbcy(2), intg_fbcz(2), crrt_imbalance(8)
    real(WP) :: maxmin_div(2)
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div
    ! Boundary planes of j itself. mh%fbc*_j* are zeroed at initialisation and only
    ! the pipe-axis slots of fbcy_jy are ever refreshed, so using them here would
    ! both mis-difference div(j) above CD2 and report every boundary current as
    ! zero - which silently turns the imbalance below into a restatement of the
    ! volume integral instead of a balance.
    real(WP), dimension(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx_jvec
    real(WP), dimension(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) :: fbcy_jvec
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) :: fbcz_jvec
    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: acpc_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: accp_ypencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: accp_zpencil
    !
    if(.not. dm%is_mhd) return
    !-----------------------------------------------------------------
    ! divergence-free current density flux
    !
    ! Measured as max|div(j)|, not max(div(j)): a large negative divergence is
    ! just as much a charge-conservation failure as a positive one. This matches
    ! the convention used for the mass residual in Check_mass_conservation.
    !-----------------------------------------------------------------
    call extract_dirichlet_fbcx(fbcx_jvec, mh%jx, dm%dpcc)
    call transpose_x_to_y(mh%jy, acpc_ypencil, dm%dcpc)
    call extract_dirichlet_fbcy(fbcy_jvec, acpc_ypencil, dm%dcpc, dm)
    call transpose_x_to_y(mh%jz, accp_ypencil, dm%dccp)
    call transpose_y_to_z(accp_ypencil, accp_zpencil, dm%dccp)
    call extract_dirichlet_fbcz(fbcz_jvec, accp_zpencil, dm%dccp)
    call Get_divergence_vector(mh%jx, mh%jy, mh%jz, div, dm, fbcx_jvec, fbcy_jvec, fbcz_jvec)
    maxmin_div = ZERO
    call Find_max_min_3d(div, opt_abs='ABS', opt_calc='MAXI', opt_work=maxmin_div, &
                         opt_name="elementary |div(j_vec)| =")
    fl%max_div_j = maxmin_div(2)
    !
    call Get_volumetric_average_3d(dm, dm%dccc, div, intg_m, SPACE_INTEGRAL, 'Ivol')
    !-----------------------------------------------------------------
    ! current density flux through b.c. = integral_surface
    !-----------------------------------------------------------------
    ! x-bc
    intg_fbcx = ZERO
    if(dm%ibcx_qx(1)/=IBC_PERIODIC)then
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, fbcx_jvec, intg_fbcx, SPACE_INTEGRAL, 'fbcx')
    end if
    ! y-bc
    intg_fbcy = ZERO
    if(dm%ibcy_qy(1)/=IBC_PERIODIC)then
      call Get_area_average_2d_for_fbcy(dm, dm%dcpc, fbcy_jvec, intg_fbcy, SPACE_INTEGRAL, 'fbcy', is_rf=.true.)
    end if
    ! z-bc
    intg_fbcz = ZERO
    if(dm%ibcz_qz(1)/=IBC_PERIODIC)then
      call Get_area_average_2d_for_fbcz(dm, dm%dccp, fbcz_jvec, intg_fbcz, SPACE_INTEGRAL, 'fbcz')
    end if
    !
    ! current change rate
    crrt_imbalance(1:2) = intg_fbcx(1:2)
    crrt_imbalance(3:4) = intg_fbcy(1:2)
    crrt_imbalance(5:6) = intg_fbcz(1:2)
    crrt_imbalance(7)   = intg_m
    crrt_imbalance(8)   = intg_m + &
                          intg_fbcx(1) - intg_fbcx(2) + &
                          intg_fbcy(1) - intg_fbcy(2) + &
                          intg_fbcz(1) - intg_fbcz(2)
    fl%current_imbalance = crrt_imbalance(8)

    if (nrank == 0) then
        write (*, wrtfmt1el) 'global electric current imbalance = ', crrt_imbalance(8)
    end if

  end subroutine

end module
