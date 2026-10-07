!> Boundary-condition orchestration for flow, thermo, MHD, and pipe-axis treatment.
!>
!> This module translates user-facing nominal boundary-condition IDs into the
!> calculation boundary conditions used by interpolation, derivative, pressure,
!> and halo-update routines. It also owns the storage for flow/thermal boundary
!> planes and the special reconstruction rules used at the pipe centreline.
module boundary_conditions_mod
  use bc_dirichlet_mod
  use bc_ndomain_interior_mod
  use parameters_constant_mod
  use print_msg_mod
  use udf_type_mod
  implicit none

  integer, parameter, public :: AXIS_RECON_NONE      = 0
  integer, parameter, public :: AXIS_RECON_ZERO      = 1 ! quantities constrained to vanish on the axis.
  integer, parameter, public :: AXIS_RECON_M1        = 2 ! vector-like cross-plane quantities whose regular centreline behavior is the first azimuthal Fourier mode.
  integer, parameter, public :: AXIS_RECON_M0        = 3 ! scalar-like fields whose centreline limit must be single-valued and azimuthally invariant.
  integer, parameter, public :: AXIS_RECON_M0_M2     = 4 ! quadratic or tensor-like products of m=1 quantities, whose regular centreline content consists of axisymmetric and second azimuthal modes.

  integer, save :: mbcx_cov1(2), &
                   mbcy_cov1(2), &
                   mbcz_cov1(2), &
                   mbcx_tau1(2), &
                   mbcy_tau1(2), &
                   mbcz_tau1(2), &
                   mbcx_cov2(2), &
                   mbcy_cov2(2), &
                   mbcz_cov2(2), &
                   mbcr_cov2(2), &
                   mbcy_tau2(2), &
                   mbcx_tau2(2), &
                   mbcz_tau2(2), &
                   mbcr_tau2(2), &
                   mbcx_cov3(2), &
                   mbcy_cov3(2), &
                   mbcz_cov3(2), &
                   mbcr_cov3(2), &
                   mbcy_tau3(2), &
                   mbcx_tau3(2), &
                   mbcz_tau3(2), &
                   mbcr_tau3(2), &
                   ebcx_conv(2), &
                   ebcy_conv(2), &
                   ebcz_conv(2), &
                   ebcx_difu(2), &
                   ebcy_difu(2), &
                   ebcz_difu(2)
  logical, save :: is_fbcx_velo_required, &
                   is_fbcy_velo_required, &
                   is_fbcz_velo_required

  private :: reassign_ibc           ! re-assign calcuation ibc and keep the nominal bc
  public  :: config_calc_basic_ibc  ! applied once only, just before calculation

  public  :: allocate_fbc_flow   ! applied once only
  public  :: allocate_fbc_thermo ! applied once only

  !private :: axis_mirroring_interior_fbcy
  public  :: axis_mirror_fbcy
  !private :: build_axis_qyr_fbcy
  public  :: update_fbcy_cc_flow_halo   ! for pipe only, applied every NS, cc for circle central point and var stored in xcx
  public  :: update_fbcy_cc_thermo_halo ! for pipe only, applied every NS, cc for circle central point and var stored in xcx
  public  :: update_fbcy_cc_mhd_halo    ! for pipe only, applied every MHD refresh, cc for circle central point and var stored in xcx

  public  :: build_bc_symm_operation    ! applied if necessary
  public  :: config_calc_eqs_ibc

  public  :: get_fbcx_iTh
  public  :: get_fbcy_iTh
  public  :: get_fbcz_iTh

  public  :: get_name_bc ! also used to log the resolved electric-potential BC

  public  :: get_ibc_for_sgs_coef_c2p ! shared by the SGS momentum viscosity and the SGS enthalpy flux

contains
!==============================================================================
!> Return a readable name for a boundary-condition ID.
!> - ibc (in): Integer boundary-condition identifier.
!> Return: Fixed-length character string used in diagnostic output.
function get_name_bc(ibc) result(str)
  integer, intent(in) :: ibc
  character(14) :: str

  select case(ibc)
  case (IBC_INTERIOR)
    str = 'IBC_INTERIOR'
  case ( IBC_PERIODIC )
    str = 'IBC_PERIODIC'
  case ( IBC_SYMMETRIC )
    str = 'IBC_SYMMETRIC'
  case ( IBC_ASYMMETRIC )
    str = 'IBC_ASYMMETRIC'
  case ( IBC_DIRICHLET )
    str = 'IBC_DIRICHLET'
  case ( IBC_NEUMANN )
    str = 'IBC_NEUMANN'
  case ( IBC_INTRPL )
    str = 'IBC_INTRPL'
  case ( IBC_CONVECTIVE )
    str = 'IBC_CONVECTIVE'
  !case ( IBC_TURBGEN )
    !str = 'IBC_TURBGEN'
  case ( IBC_PROFILE1D )
    str = 'IBC_PROFILE1D'
  case ( IBC_DATABASE )
    str = 'IBC_DATABASE'
  case ( IBC_POISEUILLE )
    str = 'IBC_POISEUILLE'
  case default
    call Print_error_msg('Boundary Conditions Not Supported.')
  end select
  str = ' '//trim(adjustl(str))

  return
end function

!==============================================================================
  !> Convert nominal user boundary conditions to calculation boundary conditions.
  !>
  !> Profile, Poiseuille, database, and convective outlet settings are expanded
  !> into field-wise Dirichlet/Neumann choices used by the numerical kernels.
  !> - bc_nominal (in): Nominal boundary-condition table, side by variable.
  !> - ibc (out): Calculation boundary-condition table.
  subroutine reassign_ibc(bc_nominal, ibc)
    integer, intent(in) :: bc_nominal(2, 5)
    integer, intent(out) :: ibc(2, 5)
    integer :: n, m

    do n = 1, 2
      do m = 1, 5
        select case (bc_nominal(n, m))

          case (IBC_PROFILE1D)
            ! Use Dirichlet BC for all variables
            ibc(n, m) = IBC_DIRICHLET
          case (IBC_POISEUILLE)
            ! Use Dirichlet BC for all variables
            ibc(n, m) = IBC_DIRICHLET
          ! case (IBC_TURBGEN)
          !   select case (m)
          !     case (5)
          !       ! Temperature: assume no incoming thermal flow (initialize temperature)
          !       ibc(n, m) = IBC_DIRICHLET
          !     case (4)
          !       ! Pressure: use Neumann BC
          !       ibc(n, m) = IBC_NEUMANN
          !     case default
          !       ! Velocity components: use Dirichlet BC
          !       ibc(n, m) = IBC_DIRICHLET
          !   end select

          case (IBC_DATABASE)
            select case (m)
              case (5)
                ! Temperature: same as above
                ibc(n, m) = IBC_DIRICHLET
              case (4)
                ! Pressure: use Neumann BC
                ibc(n, m) = IBC_NEUMANN
              case default
                ! Velocity components: use Dirichlet BC (verify if correct)
                ibc(n, m) = IBC_DIRICHLET
            end select

          case (IBC_CONVECTIVE)
            ! Typically for convective outlet conditions
            if (m == 4) then
              ! Pressure: Neumann
              ibc(n, m) = IBC_NEUMANN !IBC_DIRICHLET !IBC_NEUMANN
            else
              ! Velocity and temperature: Dirichlet (to be verified)
              ibc(n, m) = IBC_DIRICHLET
            end if

          case default
            ! Use the nominal value directly
            ibc(n, m) = bc_nominal(n, m)

        end select
      end do
      ! check
      ! if(ibc(n, 1) == IBC_DIRICHLET .and. &
      !    ibc(n, 2) == IBC_DIRICHLET .and. &
      !    ibc(n, 3) == IBC_DIRICHLET) then
      !    ibc(n, 4) = IBC_NEUMANN
      ! end if
    end do

    return
  end subroutine
!==============================================================================
  !> Boundary classification for interpolating a cell-centred subgrid-scale
  !> coefficient (mu_sgs, or mu_sgs/Pr_sgs) onto the faces where it is used.
  !>
  !> The classification is taken from the *nominal* velocity boundary IDs rather
  !> than from the pressure or temperature class, because only the velocity IDs
  !> separate a wall from a flow-through plane. Pressure is IBC_NEUMANN at a
  !> no-slip wall and at an inlet/outlet alike, and temperature is Dirichlet both
  !> at an isothermal wall and at a prescribed inflow; neither discriminates.
  !> The nominal velocity table does: a physical wall sets all three components
  !> Dirichlet, an inflow sets IBC_PROFILE1D/IBC_POISEUILLE/IBC_DATABASE, and an
  !> outflow sets IBC_CONVECTIVE. The nominal table survives config_calc_basic_ibc
  !> intact (only the wall-normal symmetric component is promoted to asymmetric
  !> there) and is therefore still readable at RHS-assembly time. No new mask is
  !> introduced.
  !>
  !> Per side the resulting treatment is:
  !>
  !>  periodic / interior / symmetric - passed through. None is a physical
  !>    boundary: a periodic side needs no boundary value; an interior side (the
  !>    cylindrical axis, or a multi-domain join) is a regularity boundary whose
  !>    halo is reconstructed from the field itself; a symmetry plane is even for
  !>    a scalar, so the mirror stencil in the C2P operator gives the face value.
  !>    IBC_ASYMMETRIC maps to IBC_SYMMETRIC: parity does not pass from a field to
  !>    its diffusion coefficient. An odd field satisfies f(-y) = -f(y), but the
  !>    coefficient is a non-negative scalar function of the local state and so is
  !>    even whatever the parity of the field it multiplies. Odd parity would
  !>    force it to zero on the plane and negative on one side of it.
  !>
  !>  physical wall (all three velocity components nominally Dirichlet) -
  !>    IBC_DIRICHLET, with the caller supplying the known face value as fbc. For
  !>    wall-resolved LES the WALE eddy viscosity vanishes as nu_sgs ~ y^3, so the
  !>    callers pass fbc = 0: no subgrid momentum stress and no subgrid enthalpy
  !>    flux at the wall, molecular transport only. (A wall model would not have
  !>    a zero wall value; none exists in this code.) Marking the side Dirichlet
  !>    is what makes Prepare_TDMA_interp_C2P_RHS_array assign that value to the
  !>    face exactly. Dropping the fbc instead demotes the side to IBC_INTRPL
  !>    (reduce_bc_to_interp, basics_operations2), a one-sided extrapolation that
  !>    overshoots the convex y^3 profile and can return a negative coefficient.
  !>
  !>  inlet / outlet - IBC_NEUMANN, with the caller supplying fbc = 0. There is
  !>    no established boundary prescription for mu_sgs on a flow-through plane:
  !>    the inflow database carries velocity and temperature, not a subgrid
  !>    coefficient, and the convective outlet advects the resolved field only.
  !>    Zero normal gradient is a provisional numerical choice, selected so that
  !>    the turbulent subgrid transport is *retained* at the plane instead of
  !>    being switched off by a spurious wall condition. Under bc_ghost_cd the
  !>    Neumann row uses the interior stencil with ghost cells built as
  !>    fc(0) = fi(1) - fbc*dp (buildup_ghost_cells_C), so with fbc = 0 the ghost
  !>    copies its neighbour and the cd2 face value reduces to the adjacent cell
  !>    value - non-negative, and preserving the incoming subgrid activity.
  !>    It is first order at the plane and is not a physical closure; it affects
  !>    one face layer at the inlet and the outlet.
  !>
  !> Anything else is rejected rather than silently treated as one of the above.
  !>
  !> Limitation: the wall test is "all three components nominally Dirichlet", so
  !> an inflow prescribed as a plain constant Dirichlet velocity triple would be
  !> read as a wall and lose its subgrid coefficient at that plane. Every inflow
  !> in tests/ uses PROFILE1D, POISEUILLE or DATABASE, which are distinguishable;
  !> a mixed side (one component Dirichlet, another a flow-through class) already
  !> falls to the inlet/outlet branch below.
  !> - ibc_nominal (in): nominal BC table for this direction, (side, variable),
  !>   variables ordered u, v, w, p, T. Only the three velocity columns are read.
  !> - ibc_coef (out): BC pair to use for the coefficient's C2P interpolation.
  subroutine get_ibc_for_sgs_coef_c2p(ibc_nominal, ibc_coef)
    use typeconvert_mod, only : int2str
    implicit none
    integer, intent(in ) :: ibc_nominal(2, 5)
    integer, intent(out) :: ibc_coef(2)
    integer :: n
    integer :: ibcv(3)

    do n = 1, 2
      ibcv(1:3) = ibc_nominal(n, 1:3)
      if    (all(ibcv == IBC_PERIODIC)) then
        ibc_coef(n) = IBC_PERIODIC
      else if(all(ibcv == IBC_INTERIOR)) then
        ibc_coef(n) = IBC_INTERIOR
      else if(all(ibcv == IBC_SYMMETRIC .or. ibcv == IBC_ASYMMETRIC)) then
        ibc_coef(n) = IBC_SYMMETRIC
      else if(all(ibcv == IBC_DIRICHLET)) then
        ibc_coef(n) = IBC_DIRICHLET   ! physical wall, fbc = 0 supplied by caller
      else if(all(ibcv == IBC_PROFILE1D  .or. ibcv == IBC_POISEUILLE .or. &
                  ibcv == IBC_DATABASE   .or. ibcv == IBC_CONVECTIVE .or. &
                  ibcv == IBC_DIRICHLET  .or. ibcv == IBC_NEUMANN    .or. &
                  ibcv == IBC_INTRPL)) then
        ibc_coef(n) = IBC_NEUMANN     ! inlet/outlet, fbc = 0 supplied by caller
      else
        call Print_error_msg('get_ibc_for_sgs_coef_c2p: cannot classify a side '//&
             'with nominal velocity bc '//trim(int2str(ibcv(1)))//' '//&
             trim(int2str(ibcv(2)))//' '//trim(int2str(ibcv(3))))
      end if
    end do

    return
  end subroutine get_ibc_for_sgs_coef_c2p
!==============================================================================
!==============================================================================
! to get all ibc for calculation
!==============================================================================
  !> Configure basic field boundary conditions from the user input.
  !>
  !> Applies symmetry corrections, converts nominal IDs to calculation IDs, and
  !> stores the resulting velocity, pressure, temperature, and thermal-property
  !> boundary-condition arrays in the domain object.
  !> - dm (inout): Domain descriptor containing nominal and calculation BCs.
  subroutine config_calc_basic_ibc(dm)
    use wtformat_mod
    type(t_domain), intent(inout) :: dm
    integer :: n
    integer :: ibcx(2, 5), ibcy(2, 5), ibcz(2, 5)
    character(len = 38) :: fmt = '(2X, A10, 2(A3, A14, A3, A14), 2F13.4)'
!------------------------------------------------------------------------------
! to check velocity symmetric and asymmetric
!------------------------------------------------------------------------------
    do n = 1, 2
      if(dm%ibcx_nominal(n, 1) == IBC_SYMMETRIC) &
         dm%ibcx_nominal(n, 1) =  IBC_ASYMMETRIC
      if(dm%ibcy_nominal(n, 2) == IBC_SYMMETRIC) &
         dm%ibcy_nominal(n, 2) =  IBC_ASYMMETRIC
      if(dm%ibcz_nominal(n, 3) == IBC_SYMMETRIC) &
         dm%ibcz_nominal(n, 3) =  IBC_ASYMMETRIC
    end do
!------------------------------------------------------------------------------
! to set up real bc for calculation from given nominal b.c.
!------------------------------------------------------------------------------
    call reassign_ibc(dm%ibcx_nominal, ibcx(1:2, 1:5))
    call reassign_ibc(dm%ibcy_nominal, ibcy(1:2, 1:5))
    call reassign_ibc(dm%ibcz_nominal, ibcz(1:2, 1:5))
!------------------------------------------------------------------------------
! allocate bc to variables
!------------------------------------------------------------------------------
    do n = 1, 2
      dm%ibcx_qx(n) = ibcx(n, 1)
      dm%ibcx_qy(n) = ibcx(n, 2)
      dm%ibcx_qz(n) = ibcx(n, 3)
      dm%ibcx_pr(n) = ibcx(n, 4)
      dm%ibcx_Tm(n) = ibcx(n, 5)

      dm%ibcy_qx(n) = ibcy(n, 1)
      dm%ibcy_qy(n) = ibcy(n, 2)
      dm%ibcy_qz(n) = ibcy(n, 3)
      dm%ibcy_pr(n) = ibcy(n, 4)
      dm%ibcy_Tm(n) = ibcy(n, 5)

      dm%ibcz_qx(n) = ibcz(n, 1)
      dm%ibcz_qy(n) = ibcz(n, 2)
      dm%ibcz_qz(n) = ibcz(n, 3)
      dm%ibcz_pr(n) = ibcz(n, 4)
      dm%ibcz_Tm(n) = ibcz(n, 5)

      dm%ibcx_ftp(n) = dm%ibcx_Tm(n)
      dm%ibcy_ftp(n) = dm%ibcy_Tm(n)
      dm%ibcz_ftp(n) = dm%ibcz_Tm(n)

      if(dm%ibcx_Tm(n) == IBC_NEUMANN) dm%ibcx_ftp(n) = IBC_DIRICHLET
      if(dm%ibcy_Tm(n) == IBC_NEUMANN) dm%ibcy_ftp(n) = IBC_DIRICHLET
      if(dm%ibcz_Tm(n) == IBC_NEUMANN) dm%ibcz_ftp(n) = IBC_DIRICHLET

      ! if(dm%ibcx_qx(n) == IBC_DIRICHLET) then
      !   dm%ibcx_pr(n) = IBC_NEUMANN
      !   dm%fbcx_const(n, 4) = ZERO
      ! end if
      ! if(dm%ibcy_qy(n) == IBC_DIRICHLET) then
      !   dm%ibcy_pr(n) = IBC_NEUMANN
      !   dm%fbcy_const(n, 4) = ZERO
      ! end if
      ! if(dm%ibcz_qz(n) == IBC_DIRICHLET) then
      !   dm%ibcz_pr(n) = IBC_NEUMANN
      !   dm%fbcz_const(n, 4) = ZERO
      ! end if
    end do

    if(dm%icase == ICASE_PIPE) then
      ! already done in input_general.f90
    end if

    if(nrank == 0) then
      call Print_debug_start_msg('Norminal and calculated boundary conditions')
      write (*, *) '      is periodic in xyz? ', dm%is_periodic(1:3)
      write (*, *) '      BC in the X direction: norminal BC Left, calc BC Left, norminal BC Right, calc BC Right'
      write (*, fmt) '  u-bc :', '||', get_name_bc(dm%ibcx_nominal(1, 1)), '=> ', get_name_bc(dm%ibcx_qx(1)), &
                                 '||', get_name_bc(dm%ibcx_nominal(2, 1)), '=> ', get_name_bc(dm%ibcx_qx(2)), dm%fbcx_const(1:2, 1)
      write (*, fmt) '  v-bc :', '||', get_name_bc(dm%ibcx_nominal(1, 2)), '=> ', get_name_bc(dm%ibcx_qy(1)), &
                                 '||', get_name_bc(dm%ibcx_nominal(2, 2)), '=> ', get_name_bc(dm%ibcx_qy(2)), dm%fbcx_const(1:2, 2)
      write (*, fmt) '  w-bc :', '||', get_name_bc(dm%ibcx_nominal(1, 3)), '=> ', get_name_bc(dm%ibcx_qz(1)), &
                                 '||', get_name_bc(dm%ibcx_nominal(2, 3)), '=> ', get_name_bc(dm%ibcx_qz(2)), dm%fbcx_const(1:2, 3)
      write (*, fmt) '  p-bc :', '||', get_name_bc(dm%ibcx_nominal(1, 4)), '=> ', get_name_bc(dm%ibcx_pr(1)), &
                                 '||', get_name_bc(dm%ibcx_nominal(2, 4)), '=> ', get_name_bc(dm%ibcx_pr(2)), dm%fbcx_const(1:2, 4)
      if(dm%is_thermo) &
      write (*, fmt) '  T-bc :', '||', get_name_bc(dm%ibcx_nominal(1, 5)), '=> ', get_name_bc(dm%ibcx_Tm(1)), &
                                 '||', get_name_bc(dm%ibcx_nominal(2, 5)), '=> ', get_name_bc(dm%ibcx_Tm(2)), dm%fbcx_const(1:2, 5)
      write (*, wrtfmt1s) '      BC in the Y direction: norminal BC, calc BC'
      write (*, fmt) '  u-bc :', '||', get_name_bc(dm%ibcy_nominal(1, 1)), '=> ', get_name_bc(dm%ibcy_qx(1)), &
                                 '||', get_name_bc(dm%ibcy_nominal(2, 1)), '=> ', get_name_bc(dm%ibcy_qx(2)), dm%fbcy_const(1:2, 1)
      write (*, fmt) '  v-bc :', '||', get_name_bc(dm%ibcy_nominal(1, 2)), '=> ', get_name_bc(dm%ibcy_qy(1)), &
                                 '||', get_name_bc(dm%ibcy_nominal(2, 2)), '=> ', get_name_bc(dm%ibcy_qy(2)), dm%fbcy_const(1:2, 2)
      write (*, fmt) '  w-bc :', '||', get_name_bc(dm%ibcy_nominal(1, 3)), '=> ', get_name_bc(dm%ibcy_qz(1)), &
                                 '||', get_name_bc(dm%ibcy_nominal(2, 3)), '=> ', get_name_bc(dm%ibcy_qz(2)), dm%fbcy_const(1:2, 3)
      write (*, fmt) '  p-bc :', '||', get_name_bc(dm%ibcy_nominal(1, 4)), '=> ', get_name_bc(dm%ibcy_pr(1)), &
                                 '||', get_name_bc(dm%ibcy_nominal(2, 4)), '=> ', get_name_bc(dm%ibcy_pr(2)), dm%fbcy_const(1:2, 4)
      if(dm%is_thermo) &
      write (*, fmt) '  T-bc :', '||', get_name_bc(dm%ibcy_nominal(1, 5)), '=> ', get_name_bc(dm%ibcy_Tm(1)), &
                                 '||', get_name_bc(dm%ibcy_nominal(2, 5)), '=> ', get_name_bc(dm%ibcy_Tm(2)), dm%fbcy_const(1:2, 5)
      write (*, wrtfmt1s) '      BC in the Z direction: norminal BC, calc BC'
      write (*, fmt) '  u-bc :', '||', get_name_bc(dm%ibcz_nominal(1, 1)), '=> ', get_name_bc(dm%ibcz_qx(1)), &
                                 '||', get_name_bc(dm%ibcz_nominal(2, 1)), '=> ', get_name_bc(dm%ibcz_qx(2)), dm%fbcz_const(1:2, 1)
      write (*, fmt) '  v-bc :', '||', get_name_bc(dm%ibcz_nominal(1, 2)), '=> ', get_name_bc(dm%ibcz_qy(1)), &
                                 '||', get_name_bc(dm%ibcz_nominal(2, 2)), '=> ', get_name_bc(dm%ibcz_qy(2)), dm%fbcz_const(1:2, 2)
      write (*, fmt) '  w-bc :', '||', get_name_bc(dm%ibcz_nominal(1, 3)), '=> ', get_name_bc(dm%ibcz_qz(1)), &
                                 '||', get_name_bc(dm%ibcz_nominal(2, 3)), '=> ', get_name_bc(dm%ibcz_qz(2)), dm%fbcz_const(1:2, 3)
      write (*, fmt) '  p-bc :', '||', get_name_bc(dm%ibcz_nominal(1, 4)), '=> ', get_name_bc(dm%ibcz_pr(1)), &
                                 '||', get_name_bc(dm%ibcz_nominal(2, 4)), '=> ', get_name_bc(dm%ibcz_pr(2)), dm%fbcz_const(1:2, 4)
      if(dm%is_thermo) &
      write (*, fmt) '  T-bc :', '||', get_name_bc(dm%ibcz_nominal(1, 5)), '=> ', get_name_bc(dm%ibcz_Tm(1)), &
                                 '||', get_name_bc(dm%ibcz_nominal(2, 5)), '=> ', get_name_bc(dm%ibcz_Tm(2)), dm%fbcz_const(1:2, 5)
    end if

    return
  end subroutine

!==============================================================================
!==============================================================================
  !> Allocate flow boundary-condition storage for all active domain faces.
  !> - dm (inout): Domain descriptor receiving face-boundary arrays.
  subroutine allocate_fbc_flow(dm)
    type(t_domain), intent(inout)  :: dm
!------------------------------------------------------------------------------
! to set up real bc values for calculation from given nominal b.c. values
! bc always saved on the boundar face centre
! warning: this bc treatment is not proper for a inlet plane with field data.... to check and to update
!------------------------------------------------------------------------------
    allocate( dm%fbcx_qx(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil
    allocate( dm%fbcy_qx(dm%dpcc%ysz(1),              4, dm%dpcc%ysz(3)) )! default y pencil
    allocate( dm%fbcz_qx(dm%dpcc%zsz(1), dm%dpcc%zsz(2),              4) )! default z pencil

    allocate( dm%fbcx_qy(             4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) )! default x pencil
    allocate( dm%fbcy_qy(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) )! default y pencil
    allocate( dm%fbcz_qy(dm%dcpc%zsz(1), dm%dcpc%zsz(2),              4) )! default z pencil

    allocate( dm%fbcx_qz(             4, dm%dccp%xsz(2), dm%dccp%xsz(3)) )! default x pencil
    allocate( dm%fbcy_qz(dm%dccp%ysz(1),              4, dm%dccp%ysz(3)) )! default y pencil
    allocate( dm%fbcz_qz(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) )! default z pencil

    allocate( dm%fbcx_pr(             4, dm%dccc%xsz(2), dm%dccc%xsz(3)) )! default x pencil
    allocate( dm%fbcy_pr(dm%dccc%ysz(1),              4, dm%dccc%ysz(3)) )! default y pencil
    allocate( dm%fbcz_pr(dm%dccc%zsz(1), dm%dccc%zsz(2),              4) )! default z pencil

    if(dm%icoordinate == ICYLINDRICAL) then
      allocate( dm%fbcy_qyr(dm%dcpc%ysz(1), 4,              dm%dcpc%ysz(3)) )
      allocate( dm%fbcz_qyr(dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4             ) )
      allocate( dm%fbcy_qzr(dm%dccp%ysz(1), 4,              dm%dccp%ysz(3)) )
      allocate( dm%fbcz_qzr(dm%dccp%zsz(1), dm%dccp%zsz(2), 4             ) )
      allocate( dm%axisy_qyr(dm%dcpc%ysz(1),                dm%dcpc%ysz(3)) )
      dm%axisy_qyr = ZERO
    end if

    if(dm%is_record_xoutlet) then
      allocate (dm%fbcx_qx_outl1(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
      allocate (dm%fbcx_qx_outl2(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
      allocate (dm%fbcx_qy_outl1(dm%dxpc%xsz(1), dm%dxpc%xsz(2), dm%dxpc%xsz(3)) )
      allocate (dm%fbcx_qy_outl2(dm%dxpc%xsz(1), dm%dxpc%xsz(2), dm%dxpc%xsz(3)) )
      allocate (dm%fbcx_qz_outl1(dm%dxcp%xsz(1), dm%dxcp%xsz(2), dm%dxcp%xsz(3)) )
      allocate (dm%fbcx_qz_outl2(dm%dxcp%xsz(1), dm%dxcp%xsz(2), dm%dxcp%xsz(3)) )
      ! Pressure outlet database is optional and currently not replayed by
      ! read_instantaneous_xinlet, so avoid the extra database storage by default.
      !allocate (dm%fbcx_pr_outl1(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
      !allocate (dm%fbcx_pr_outl2(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
    end if

    if(dm%is_read_xinlet) then
      ! assume inlet has same mesh resolution with the main domain, to be updated if necessary. todo:
      allocate (dm%fbcx_qx_inl1(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
      allocate (dm%fbcx_qx_inl2(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
      allocate (dm%fbcx_qy_inl1(dm%dxpc%xsz(1), dm%dxpc%xsz(2), dm%dxpc%xsz(3)) )
      allocate (dm%fbcx_qy_inl2(dm%dxpc%xsz(1), dm%dxpc%xsz(2), dm%dxpc%xsz(3)) )
      allocate (dm%fbcx_qz_inl1(dm%dxcp%xsz(1), dm%dxcp%xsz(2), dm%dxcp%xsz(3)) )
      allocate (dm%fbcx_qz_inl2(dm%dxcp%xsz(1), dm%dxcp%xsz(2), dm%dxcp%xsz(3)) )
      ! Pressure database replay is disabled unless the corresponding read path is enabled.
      !allocate (dm%fbcx_pr_inl1(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
      !allocate (dm%fbcx_pr_inl2(dm%dxcc%xsz(1), dm%dxcc%xsz(2), dm%dxcc%xsz(3)) )
    end if

    return
  end subroutine
!==============================================================================
!==============================================================================
  !> Allocate thermal boundary-condition storage for all active domain faces.
  !> - dm (inout): Domain descriptor receiving thermal face-boundary arrays.
  subroutine allocate_fbc_thermo(dm)
    type(t_domain), intent(inout) :: dm

    if( .not. dm%is_thermo) return

    allocate( dm%fbcx_gx(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil
    allocate( dm%fbcx_gy(4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) )! default x pencil
    allocate( dm%fbcx_gz(4, dm%dccp%xsz(2), dm%dccp%xsz(3)) )! default x pencil

    allocate( dm%fbcy_gx(dm%dpcc%ysz(1), 4, dm%dpcc%ysz(3)) )! default y pencil
    allocate( dm%fbcy_gy(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) )! default y pencil
    allocate( dm%fbcy_gz(dm%dccp%ysz(1), 4, dm%dccp%ysz(3)) )! default y pencil

    allocate( dm%fbcz_gx(dm%dpcc%zsz(1), dm%dpcc%zsz(2), 4) )! default z pencil
    allocate( dm%fbcz_gy(dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4) )! default z pencil
    allocate( dm%fbcz_gz(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) )! default z pencil

    !if(dm%icoordinate == ICYLINDRICAL) then
      !allocate( dm%fbcy_gyr(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) )
      !allocate( dm%fbcy_gzr(dm%dccp%ysz(1), 4, dm%dccp%ysz(3)) )
      !allocate( dm%fbcz_gyr(dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4) )
      !allocate( dm%fbcz_gzr(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) )
    !end if

    allocate( dm%fbcx_qw (4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil
    allocate( dm%fbcx_ftp(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) )! default x pencil

    allocate( dm%fbcy_qw (dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) )! default y pencil
    allocate( dm%fbcy_ftp(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) )! default y pencil

    allocate( dm%fbcz_qw (dm%dccp%zsz(1), dm%dccp%zsz(2), 4) )! default z pencil
    allocate( dm%fbcz_ftp(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) )! default z pencil

    return
  end subroutine

!==============================================================================
  !> Update pipe-axis halo values for flow variables stored around the y-axis centreline.
  !> - fl (inout): Flow variables whose cylindrical-axis halos are reconstructed.
  !> - dm (in): Domain and symmetry metadata for the pipe axis.
  subroutine update_fbcy_cc_flow_halo(fl, dm)  ! cylindrical pipe axis treatment only
    use cylindrical_rn_mod
    use find_max_min_ave_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl

    if (dm%icase /= ICASE_PIPE .or. dm%icoordinate /= ICYLINDRICAL) return

  ! qx : even symmetry across axis
    if(dm%ibcy_qx(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_qx for the centre of the pipe.')
    call axis_mirror_fbcy(fl%qx, IPENCIL(1), dm%fbcy_qx, dm%knc_sym, dm%dpcc, is_ynode = .false., is_odd = .false.)

!------------------------------------------------------------------------------
!   Parity across the axis. Write the extension along a line through the axis as
!   f_ext(-r, theta) = sigma * f(r, theta + pi); is_odd selects sigma = -1.
!   For a regular field the cross-plane Cartesian components (U_y, U_z) are
!   smooth at r = 0, and
!     qy = r * ur = y*U_y + z*U_z   -> a smooth function of (y, z), so sigma = +1
!     qyr =    ur = qy / r          -> dividing by r flips it,       so sigma = -1
!     qz =     ut = -U_y*sin + ..   -> a component, odd under theta+pi, sigma = -1
!     qzr =    ut / r               -> flips again,                   sigma = +1
!   Every multiplication or division by r flips sigma; so does d/dr. Verified
!   numerically against the exact ghosts of a uniform transverse flow.
!------------------------------------------------------------------------------
  ! qy = ur * r: even symmetry across axis (r and ur each flip)
  ! qyr = ur = qy / r : odd
    if(dm%ibcy_qy(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_qy for the centre of the pipe.')
    call axis_mirror_fbcy(fl%qy, IPENCIL(1), dm%fbcy_qy, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                          axis_mode = AXIS_RECON_ZERO, assign_axis_to_var = .true., nr = 0)
    ! fbcy_qyr gets the two ghost nodes below the axis; axisy_qyr gets ur ON the
    ! axis, rebuilt from the m=1 harmonic. assign_axis_to_var cannot deliver the
    ! latter here (it rejects nr /= 0, and would write into fl%qy, not qy/r), so
    ! the consumer in eq_momentum2 reads axisy_qyr instead.
    call axis_mirror_fbcy(fl%qy, IPENCIL(1), dm%fbcy_qyr, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .true., &
                          nr = 1, opt_r = dm%rpi, opt_dz = dm%h(3), &
                          axis_mode = AXIS_RECON_M1, opt_axis_val = dm%axisy_qyr)

  ! qz = ut : odd symmetry across axis
  ! qzr = qz / r : even, because dividing by r flips the parity
    if(dm%ibcy_qz(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_qz for the centre of the pipe.')
    call axis_mirror_fbcy(fl%qz, IPENCIL(1), dm%fbcy_qz, dm%knc_sym, dm%dccp, is_ynode = .false., is_odd = .true.)
    call axis_mirror_fbcy(fl%qz, IPENCIL(1), dm%fbcy_qzr, dm%knc_sym, dm%dccp, is_ynode = .false., is_odd = .false., &
                          nr = 1, opt_r = dm%rci)

  ! pressure : even symmetry across axis
    if(dm%ibcy_pr(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_pr for the centre of the pipe.')
    call axis_mirror_fbcy(fl%pres, IPENCIL(1), dm%fbcy_pr, dm%knc_sym, dm%dccc, is_ynode = .false., is_odd = .false.)

  ! thermal variables
    if(dm%is_thermo) then
  ! gx : even symmetry
      if(dm%ibcy_qx(1) /= IBC_INTERIOR) &
        call Print_error_msg('Error in ibcy_gx for the centre of the pipe.')
      call axis_mirror_fbcy(fl%gx, IPENCIL(1), dm%fbcy_gx, dm%knc_sym, dm%dpcc, is_ynode = .false., is_odd = .false.)

  ! gy = rho * r * ur : even, as qy, since rho is a scalar
      if(dm%ibcy_qy(1) /= IBC_INTERIOR) &
        call Print_error_msg('Error in ibcy_qy for the centre of the pipe.')
      call axis_mirror_fbcy(fl%gy, IPENCIL(1), dm%fbcy_gy, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_ZERO, assign_axis_to_var = .true., nr = 0)
      !call build_axis_qyr_fbcy(fl%gy, dm%fbcy_gyr, dm%knc_sym, dm%dcpc, dm%rpi), not used!

  ! gz : odd symmetry
      if(dm%ibcy_qz(1) /= IBC_INTERIOR) &
        call Print_error_msg('Error in ibcy_gz for the centre of the pipe.')
      call axis_mirror_fbcy(fl%gz, IPENCIL(1), dm%fbcy_gz, dm%knc_sym, dm%dccp, is_ynode = .false., is_odd = .true.)

    end if

    return
  end subroutine update_fbcy_cc_flow_halo
!==============================================================================
  !> Update pipe-axis halo values for thermal variables.
  !> - tm (inout): Thermal variables whose cylindrical-axis halos are reconstructed.
  !> - dm (in): Domain and symmetry metadata for the pipe axis.
  subroutine update_fbcy_cc_thermo_halo(tm, dm)  ! cylindrical pipe axis treatment only
    use find_max_min_ave_mod
    use thermo_info_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    type(t_thermo), intent(inout) :: tm

    real(WP) :: fbcy(dm%dccc%ysz(1), 4, dm%dccc%ysz(3))

    ! Only for thermo-enabled cylindrical pipe cases
    if (.not. dm%is_thermo) return
    if (dm%icase /= ICASE_PIPE .or. dm%icoordinate /= ICYLINDRICAL) return

  ! Thermodynamic primary variable at axis: even symmetry
    if(dm%ibcy_Tm(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_Tm for the centre of the pipe.')

  ! Table-based property update: use enthalpy
    if(fluidparam%ipropertyState == IPROPERTY_TABLE) then
      fbcy = dm%fbcy_ftp%h
      call axis_mirror_fbcy(tm%hEnth, IPENCIL(1), fbcy, dm%knc_sym, dm%dccc, is_ynode = .false., is_odd = .false.)
      dm%fbcy_ftp%h = fbcy
      call ftp_refresh_thermal_properties_from_H_3Dftp(dm%fbcy_ftp)
    end if

  ! Function-based property update: use temperature
    if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then
      fbcy = dm%fbcy_ftp%t
      call axis_mirror_fbcy(tm%tTemp, IPENCIL(1), fbcy, dm%knc_sym, dm%dccc, is_ynode = .false., is_odd = .false.)
      dm%fbcy_ftp%t = fbcy
      call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm%fbcy_ftp)
    end if

    return
  end subroutine update_fbcy_cc_thermo_halo
!==============================================================================
  !> Update pipe-axis halo values for MHD variables.
  !> - mh (inout): MHD variables whose cylindrical-axis halos are reconstructed.
  !> - dm (in): Domain and symmetry metadata for the pipe axis.
  subroutine update_fbcy_cc_mhd_halo(mh, dm)  ! cylindrical pipe axis treatment only
    implicit none

    type(t_mhd),    intent(inout) :: mh
    type(t_domain), intent(in)    :: dm

    if (.not. dm%is_mhd) return
    if (dm%icase /= ICASE_PIPE .or. dm%icoordinate /= ICYLINDRICAL) return

  ! Electric potential : even symmetry across axis
    if(mh%ibcy_ep(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_ep for the centre of the pipe.')
    call axis_mirror_fbcy(mh%ep, IPENCIL(1), mh%fbcy_ep, dm%knc_sym, dm%dccc, is_ynode = .false., is_odd = .false.)

  ! Current density components
    if(mh%ibcy_jx(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_jx for the centre of the pipe.')
    call axis_mirror_fbcy(mh%jx, IPENCIL(1), mh%fbcy_jx, dm%knc_sym, dm%dpcc, is_ynode = .false., is_odd = .false.)

  ! jy = r * Jr (eq_mhd: jy is scaled by rp), so it is even across the axis, as qy.
    if(mh%ibcy_jy(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_jy for the centre of the pipe.')
    call axis_mirror_fbcy(mh%jy, IPENCIL(1), mh%fbcy_jy, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                          axis_mode = AXIS_RECON_ZERO, assign_axis_to_var = .true., nr = 0)

    if(mh%ibcy_jz(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_jz for the centre of the pipe.')
    call axis_mirror_fbcy(mh%jz, IPENCIL(1), mh%fbcy_jz, dm%knc_sym, dm%dccp, is_ynode = .false., is_odd = .true.)

  ! Magnetic field components
    if(mh%ibcy_bx(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_bx for the centre of the pipe.')
    call axis_mirror_fbcy(mh%bx, IPENCIL(1), mh%fbcy_bx, dm%knc_sym, dm%dpcc, is_ynode = .false., is_odd = .false.)

  ! by = r * Br (eq_mhd: "the stored radial component follows qy storage"), so even.
    if(mh%ibcy_by(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_by for the centre of the pipe.')
    call axis_mirror_fbcy(mh%by, IPENCIL(1), mh%fbcy_by, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                          axis_mode = AXIS_RECON_ZERO, assign_axis_to_var = .true., nr = 0)

    if(mh%ibcy_bz(1) /= IBC_INTERIOR) &
      call Print_error_msg('Error in ibcy_bz for the centre of the pipe.')
    call axis_mirror_fbcy(mh%bz, IPENCIL(1), mh%fbcy_bz, dm%knc_sym, dm%dccp, is_ynode = .false., is_odd = .true.)

    return
  end subroutine update_fbcy_cc_mhd_halo
!==============================================================================
  !==============================================================================
  !==============================================================================
  !> Reconstruct or mirror a cylindrical field across the pipe centreline.
  !>
  !> The reconstruction mode encodes the regular Fourier content expected at the
  !> axis: zero, first azimuthal mode, axisymmetric mode, or mixed axisymmetric
  !> and second-mode content.
  !> - var (inout): Field to mirror or reconstruct near the axis.
  !> - pencil (in): Pencil orientation of `var`.
  !> - fbcy (inout): Boundary plane storage associated with the y direction.
  !> - ksym (in): Symmetric azimuthal-index map.
  !> - dtmp (in): Decomposition descriptor for `var`.
  !> - is_ynode (in): `.true.` if `var` is node-centred in y (dcpc/dppc/dcpp),
  !>   `.false.` if it is cell-centred in y (dpcc/dccp/dccc). Required, not
  !>   optional, so that every call site is forced to state the staggering: the
  !>   two conventions need different source indices for the ghost layers and a
  !>   silent default is how that mistake comes back.
  !> - is_odd (in): Select odd or even parity.
  !> - axis_mode (in): Centreline reconstruction mode.
  !> - assign_axis_to_var (in): If true, write reconstructed axis values into `var`.
  !> - nr (in): Radial index used for axis reconstruction.
  !> - opt_axis_val (out): Reconstructed value ON the axis node. Kept separate
  !>   from `fbcy` because slot 1 of `fbcy` is the first ghost, not the axis.
  subroutine axis_mirror_fbcy(var, pencil, fbcy, ksym, dtmp, is_ynode, is_odd, axis_mode, assign_axis_to_var, nr, opt_r, opt_dz, &
                              axis_rn_mode, opt_axis_val)
    use cylindrical_rn_mod
    use math_mod
    implicit none
    type(DECOMP_INFO), intent(in)  :: dtmp
    real(WP), intent(inout)        :: var(:, :, :)
    real(WP), intent(inout)        :: fbcy(:, :, :)
    integer, intent(in)            :: ksym(:)
    integer, intent(in)            :: pencil
    logical, intent(in)            :: is_ynode
    logical, intent(in), optional  :: is_odd
    integer, intent(in), optional  :: axis_mode
    logical, intent(in), optional  :: assign_axis_to_var
    integer, intent(in), optional  :: nr
    real(WP), intent(in), optional :: opt_r(:)
    real(WP), intent(in), optional :: opt_dz
    integer, intent(in), optional      :: axis_rn_mode
    real(WP), intent(out), optional    :: opt_axis_val(:, :)

    !
    real(WP), dimension(dtmp%ysz(1), dtmp%ysz(3)) :: axis_val
    real(WP), dimension(size(var,1), size(var,2), size(var,3)) :: dummy
    real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: var_ypencil
    real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: var_ypencil_sym
    real(WP), dimension(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)) :: var_zpencil, var_zpencil_sym

    integer :: i, k, axis_mode_local, nr_local
    real(WP) :: sign_sym
    real(WP), dimension(dtmp%zsz(1)) :: ucart_z, ucart_y, ucart_0, ucart_2c, ucart_2s
    real(WP) :: theta

    dummy = var
    axis_mode_local = AXIS_RECON_NONE
    nr_local = 0
    if(present(axis_mode)) axis_mode_local = axis_mode
    if(present(nr)) nr_local = nr
    if(nr_local > 0) then
      if(.not. present(opt_r)) call Print_error_msg("Wrong usage of axis_mirror_fbcy - 1")
      call multiple_cylindrical_rn(dummy, dtmp, opt_r, nr_local, pencil)
    end if
    call transpose_to_z_pencil(dummy, var_zpencil, dtmp, pencil)

    sign_sym = ONE
    if (present(is_odd)) then
      if (is_odd) sign_sym = -ONE
    end if
    do k = 1, dtmp%zsz(3)
      var_zpencil_sym(:, :, k) = sign_sym * var_zpencil(:, :, ksym(k))
    end do

    call reconstruct_axis_ring(var_zpencil_sym, var_zpencil, dtmp, axis_mode_local, opt_dz)

    call transpose_z_to_y(var_zpencil_sym, var_ypencil_sym, dtmp)
!------------------------------------------------------------------------------
!   Ghost layers for the IBC_INTERIOR axis. Slot 1 is the first ghost and slot 3
!   the second, matching buildup_ghost_cells_C/_P in basics_operations2: those
!   read fbc(1) into f(0) and fbc(3) into f(-1). The two y-staggerings need
!   different source indices, because the mirror of a point at radius r lands at
!   -r on the far side of the axis:
!     cell-centred in y (dpcc/dccp/dccc): cell 1 is at +dr/2, so the mirror of
!       cell 1 is ghost cell 0 and the mirror of cell 2 is ghost cell -1.
!     node-centred in y (dcpc/dppc/dcpp): node 1 IS the axis (r = 0), so the
!       mirror of node 2 is ghost node 0 and the mirror of node 3 is ghost
!       node -1. Using the cell rule here shifts the whole line by one node.
!   This is invisible at CD2, where the boundary row's ghost coefficient is
!   exactly zero, and becomes a first-order error at the axis at CD4/CP4/CP6.
!------------------------------------------------------------------------------
    if(is_ynode) then
      fbcy(:, 1, :) = var_ypencil_sym(:, 2, :)
      fbcy(:, 3, :) = var_ypencil_sym(:, 3, :)
    else
      fbcy(:, 1, :) = var_ypencil_sym(:, 1, :)
      fbcy(:, 3, :) = var_ypencil_sym(:, 2, :)
    end if
!------------------------------------------------------------------------------
!   The value ON the axis node. Only a y-node array has one: index 1 is the axis
!   itself, and reconstruct_axis_ring has just rebuilt it from the azimuthal
!   content that stays regular as r -> 0. It is returned separately from fbcy
!   because a single slot cannot be both the axis node and the ghost outside it.
!------------------------------------------------------------------------------
    axis_val(:, :) = var_ypencil_sym(:, 1, :)
    if(axis_mode_local == AXIS_RECON_ZERO) then
      if(.not. is_ynode) &
        call Print_error_msg("AXIS_RECON_ZERO is only defined for a y-node array - axis_mirror_fbcy")
      axis_val(:, :) = ZERO
    end if
    if(present(opt_axis_val)) opt_axis_val(:, :) = axis_val(:, :)

    if(present(assign_axis_to_var)) then
      if(assign_axis_to_var) then
        if(.not. present(nr)) call Print_error_msg("Wrong usage of axis_mirror_fbcy - 2")
        if(nr_local /= 0) call Print_error_msg("Wrong usage of axis_mirror_fbcy - 3")
        if(.not. is_ynode)  call Print_error_msg("Wrong usage of axis_mirror_fbcy - 7")
        call transpose_to_y_pencil(var, var_ypencil, dtmp, pencil)
        var_ypencil(:, 1, :) =  axis_val(:, :)
        call transpose_from_y_pencil(var_ypencil, var, dtmp, pencil)
      end if
    end if

    return
  contains
    subroutine reconstruct_axis_ring(var_zsym, var_zsrc, dtmp_loc, axis_mode_loc, opt_dz_loc)
      real(WP), intent(inout) :: var_zsym(:, :, :)
      real(WP), intent(in)    :: var_zsrc(:, :, :)
      type(DECOMP_INFO), intent(in) :: dtmp_loc
      integer, intent(in) :: axis_mode_loc
      real(WP), intent(in), optional :: opt_dz_loc
      integer :: i, k
      real(WP) :: theta

      select case(axis_mode_loc)
      case (AXIS_RECON_NONE, AXIS_RECON_ZERO)
        return
      case (AXIS_RECON_M0)
        if(dtmp_loc%zst(2) == 1) then
          do i = 1, dtmp_loc%zsz(1)
            do k = 1, dtmp_loc%zsz(3)
              var_zsym(i, 1, k) = HALF * (var_zsym(i, 2, k) + var_zsrc(i, 2, k))
            end do
            ucart_z(i) = sum(var_zsym(i, 1, :)) / real(dtmp_loc%zsz(3), WP)
            var_zsym(i, 1, :) = ucart_z(i)
          end do
        end if
      case (AXIS_RECON_M0_M2)
        if(.not. present(opt_dz_loc)) call Print_error_msg("Wrong usage of axis_mirror_fbcy - 4")
        if(dtmp_loc%zst(2) == 1) then
          do i = 1, dtmp_loc%zsz(1)
            ucart_0(i)  = ZERO
            ucart_2c(i) = ZERO
            ucart_2s(i) = ZERO

            do k = 1, dtmp_loc%zsz(3)
              var_zsym(i, 1, k) = HALF * (var_zsym(i, 2, k) + var_zsrc(i, 2, k))
              theta = opt_dz_loc * real((k - 1), WP)
              ucart_0(i)  = ucart_0(i)  + var_zsym(i, 1, k)
              ucart_2c(i) = ucart_2c(i) + var_zsym(i, 1, k) * cos_wp(TWO * theta)
              ucart_2s(i) = ucart_2s(i) + var_zsym(i, 1, k) * sin_wp(TWO * theta)
            end do

            ucart_0(i)  = ucart_0(i) / real(dtmp_loc%zsz(3), WP)
            ucart_2c(i) = ucart_2c(i) * TWO / real(dtmp_loc%zsz(3), WP)
            ucart_2s(i) = ucart_2s(i) * TWO / real(dtmp_loc%zsz(3), WP)

            do k = 1, dtmp_loc%zsz(3)
              theta = opt_dz_loc * real((k - 1), WP)
              var_zsym(i, 1, k) = ucart_0(i) + &
                                  ucart_2c(i) * cos_wp(TWO * theta) + &
                                  ucart_2s(i) * sin_wp(TWO * theta)
            end do
          end do
        end if
      case (AXIS_RECON_M1)
!       ucart_y/ucart_z below are the m=1 sin and cos Fourier coefficients of the
!       first off-axis ring, not components in any particular Cartesian frame.
!       Projecting onto cos/sin and summing the same pair back is an identity, so
!       this block is independent of the (r,theta) -> (y,z) mapping convention and
!       must NOT be swapped along with the mapping sites in io_visulisation.f90,
!       eq_momentum2.f90 and eq_mhd.f90.
        if(.not. present(opt_dz_loc)) call Print_error_msg("Wrong usage of axis_mirror_fbcy - 5")
        if(dtmp_loc%zst(2) == 1) then
          do i = 1, dtmp_loc%zsz(1)
            ucart_y(i) = ZERO
            ucart_z(i) = ZERO

            do k = 1, dtmp_loc%zsz(3)
              var_zsym(i, 1, k) = HALF * (var_zsym(i, 2, k) + var_zsrc(i, 2, k))
              theta = opt_dz_loc * real((k - 1), WP)
              ucart_z(i) = ucart_z(i) + var_zsym(i, 1, k) * cos_wp(theta)
              ucart_y(i) = ucart_y(i) + var_zsym(i, 1, k) * sin_wp(theta)
            end do

            ucart_z(i) = ucart_z(i) * TWO / real(dtmp_loc%zsz(3), WP)
            ucart_y(i) = ucart_y(i) * TWO / real(dtmp_loc%zsz(3), WP)

            do k = 1, dtmp_loc%zsz(3)
              theta = opt_dz_loc * real((k - 1), WP)
              var_zsym(i, 1, k) = ucart_z(i) * cos_wp(theta) + ucart_y(i) * sin_wp(theta)
            end do
          end do
        end if
      case default
        call Print_error_msg("Wrong usage of axis_mirror_fbcy - 6")
      end select
    end subroutine reconstruct_axis_ring
  end subroutine axis_mirror_fbcy

!   subroutine build_axis_axpx_r_fbcy(axpx_xpencil, fbcy_ar, ksym, dtmp, rpi, dtheta)
!     ! qyr = qy/r = ur.
!     ! ur is a cylindrical vector component.
!     ! Across the axis: ur(r,theta) = -ur(r,theta+pi).
!     ! The axis value is reconstructed from regular Cartesian transverse components.
!     use cylindrical_rn_mod
!     use math_mod
!     type(DECOMP_INFO), intent(in) :: dtmp
!     real(WP), intent(in)          :: axpx_xpencil(:, :, :)
!     real(WP), intent(inout)       :: fbcy_ar(:, :, :)
!     integer, intent(in)           :: ksym(:)
!     real(WP), intent(in)          :: rpi(:)
!     real(WP), intent(in)          :: dtheta

!     real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)) :: qyr_xpencil
!     real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: qyr_ypencil, qyr_ypencil_sym
!     real(WP), dimension(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)) :: qyr_zpencil, qyr_zpencil_sym

!     real(WP), dimension(dtmp%zsz(1)) :: ucart_z, ucart_y
!     integer :: i, k
!     real(WP) :: theta

!     qyr_xpencil = axpx_xpencil
!     call multiple_cylindrical_rn(qyr_xpencil, dtmp, rpi, 1, IPENCIL(1))   ! qyr = qy / r

!     call transpose_x_to_y(qyr_xpencil, qyr_ypencil, dtmp)
!     call transpose_y_to_z(qyr_ypencil, qyr_zpencil, dtmp)

!     do k = 1, dtmp%zsz(3)
!       qyr_zpencil_sym(:, :, k) = - qyr_zpencil(:, :, ksym(k))
!     end do

!     call transpose_z_to_y(qyr_zpencil_sym, qyr_ypencil_sym, dtmp)
!     qyr_ypencil(:, 1, :) = (qyr_ypencil_sym(:, 2, :) + qyr_ypencil(:, 2, :)) * HALF
!     call transpose_y_to_z(qyr_ypencil, qyr_zpencil_sym, dtmp)

!     ! Reconstruct the regular axis-limit value using the same azimuthal
!     ! extracting the first Fourier mode by direct summation
!     !  |z
!     !  |___y
!     if(dtmp%zst(2) == 1) then
!       do i = 1, dtmp%zsz(1)
!         ucart_y(i) = ZERO
!         ucart_z(i) = ZERO

!         do k = 1, dtmp%zsz(3)
!           theta = dtheta * real((k - 1), WP)
!           ucart_z(i) = ucart_z(i) + qyr_zpencil_sym(i, 1, k) * cos_wp(theta)
!           ucart_y(i) = ucart_y(i) + qyr_zpencil_sym(i, 1, k) * sin_wp(theta)
!         end do

!         ucart_z(i) = ucart_z(i) * TWO / real(dtmp%zsz(3), WP)
!         ucart_y(i) = ucart_y(i) * TWO / real(dtmp%zsz(3), WP)

!         do k = 1, dtmp%zsz(3)
!           theta = dtheta * real((k - 1), WP)
!           qyr_zpencil_sym(i, 1, k) = ucart_z(i) * cos_wp(theta) + ucart_y(i) * sin_wp(theta)
!         end do
!       end do
!     end if

!     call transpose_z_to_y(qyr_zpencil_sym, qyr_ypencil_sym, dtmp)

!     fbcy_ar(:, 1, :) = qyr_ypencil_sym(:, 1, :)
!     fbcy_ar(:, 3, :) = qyr_ypencil_sym(:, 2, :)

!     return
!   end subroutine build_axis_axpx_r_fbcy
!   !==============================================================================

! !==============================================================================
!   subroutine build_axis_qyr_fbcy(qy_xpencil, fbcy_qyr, ksym, dtmp, rpi, dtheta)
!     ! qyr = qy/r = ur.
!     ! ur is a cylindrical vector component.
!     ! Across the axis: ur(r,theta) = -ur(r,theta+pi).
!     ! The axis value is reconstructed from regular Cartesian transverse components.
!     use cylindrical_rn_mod
!     use math_mod
!     type(DECOMP_INFO), intent(in) :: dtmp
!     real(WP), intent(in)          :: qy_xpencil(:, :, :)
!     real(WP), intent(inout)       :: fbcy_qyr(:, :, :)
!     integer, intent(in)           :: ksym(:)
!     real(WP), intent(in)          :: rpi(:)
!     real(WP), intent(in)          :: dtheta

!     real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)) :: qyr_xpencil
!     real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: qyr_ypencil, qyr_ypencil_sym
!     real(WP), dimension(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)) :: qyr_zpencil, qyr_zpencil_sym

!     real(WP), dimension(dtmp%zsz(1)) :: ucart_z, ucart_y
!     integer :: i, k
!     real(WP) :: theta

!     qyr_xpencil = qy_xpencil
!     call multiple_cylindrical_rn(qyr_xpencil, dtmp, rpi, 1, IPENCIL(1))   ! qyr = qy / r

!     call transpose_x_to_y(qyr_xpencil, qyr_ypencil, dtmp)
!     call transpose_y_to_z(qyr_ypencil, qyr_zpencil, dtmp)

!     do k = 1, dtmp%zsz(3)
!       qyr_zpencil_sym(:, :, k) = - qyr_zpencil(:, :, ksym(k))
!     end do

!     call transpose_z_to_y(qyr_zpencil_sym, qyr_ypencil_sym, dtmp)
!     qyr_ypencil(:, 1, :) = (qyr_ypencil_sym(:, 2, :) + qyr_ypencil(:, 2, :)) * HALF
!     call transpose_y_to_z(qyr_ypencil, qyr_zpencil_sym, dtmp)

!     ! Reconstruct the regular axis-limit value using the same azimuthal
!     ! extracting the first Fourier mode by direct summation
!     !  |z
!     !  |___y
!     if(dtmp%zst(2) == 1) then
!       do i = 1, dtmp%zsz(1)
!         ucart_y(i) = ZERO
!         ucart_z(i) = ZERO

!         do k = 1, dtmp%zsz(3)
!           theta = dtheta * real((k - 1), WP)
!           ucart_z(i) = ucart_z(i) + qyr_zpencil_sym(i, 1, k) * cos_wp(theta)
!           ucart_y(i) = ucart_y(i) + qyr_zpencil_sym(i, 1, k) * sin_wp(theta)
!         end do

!         ucart_z(i) = ucart_z(i) * TWO / real(dtmp%zsz(3), WP)
!         ucart_y(i) = ucart_y(i) * TWO / real(dtmp%zsz(3), WP)

!         do k = 1, dtmp%zsz(3)
!           theta = dtheta * real((k - 1), WP)
!           qyr_zpencil_sym(i, 1, k) = ucart_z(i) * cos_wp(theta) + ucart_y(i) * sin_wp(theta)
!         end do
!       end do
!     end if

!     call transpose_z_to_y(qyr_zpencil_sym, qyr_ypencil_sym, dtmp)

!     fbcy_qyr(:, 1, :) = qyr_ypencil_sym(:, 1, :)
!     fbcy_qyr(:, 3, :) = qyr_ypencil_sym(:, 2, :)

!     return
!   end subroutine build_axis_qyr_fbcy
  !==============================================================================
  !==============================================================================

! !==============================================================================
! !==============================================================================
!   subroutine axis_mirroring_interior_fbcy(var_xpencil, fbcy, ksym, dtmp, is_qr_qrdr, is_reversed)
!     type(DECOMP_INFO), intent(in) :: dtmp
!     real(WP), intent(in) :: var_xpencil(:, :, :)
!     real(WP), intent(inout) :: fbcy(:, :, :)
!     integer, intent(in) :: ksym(:)
!     logical, intent(in), optional :: is_reversed
!     integer, intent(in), optional :: is_qr_qrdr

!     real(WP), dimension( dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3) ) :: var_ypencil, var_ypencil1
!     real(WP), dimension( dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3) ) :: var_zpencil, var_zpencil1

!     integer :: k
!     real(WP) :: sign

!     !if (dm%icase /= ICASE_PIPE .or. dm%icoordinate /= ICYLINDRICAL) return

!     sign = ONE
! !------------------------------------------------------------------------------
! !   transpose from x to z
! !------------------------------------------------------------------------------
!     if(present(is_reversed)) then
!       if(is_reversed) sign = - ONE
!     end if
!     call transpose_x_to_y(var_xpencil, var_ypencil, dtmp)
!     call transpose_y_to_z(var_ypencil, var_zpencil, dtmp)

!     do k = 1, dtmp%zsz(3)
!       var_zpencil1(:, :, k) = sign * var_zpencil(:, :, ksym(k))
!     end do
!     call transpose_z_to_y(var_zpencil1, var_ypencil1, dtmp)
!     fbcy(:, 1, :) = var_ypencil1(:, 1, :)
!     fbcy(:, 3, :) = var_ypencil1(:, 2, :)

!     if(present(is_qr_qrdr)) then ! for qy/r
!       if(is_qr_qrdr == 1) then
!         fbcy(:, 1, :) = ZERO
!       else if (is_qr_qrdr == 2) then
!         fbcy(:, 1, :) = (var_ypencil1(:, 2, :) + var_ypencil(:, 2, :)) * HALF ! multiple values
!       else
!       end if
!     end if

!     return
!   end subroutine

! !==============================================================================
! !==============================================================================
!   subroutine update_fbcy_cc_flow_halo(fl, dm)  ! for cylindrical only
!     use find_max_min_ave_mod
!     use cylindrical_rn_mod
!     implicit none
!     type(t_domain), intent(inout) :: dm
!     type(t_flow), intent(inout)      :: fl

!     real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil

!     ! Check if the case and coordinate system are valid
!     if (dm%icase /= ICASE_PIPE .or. dm%icoordinate /= ICYLINDRICAL) return
! #ifdef DEBUG_STEPS
!     if(nrank == 0) &
!     call Print_debug_inline_msg('Update boundary conditions in y-direction for the centre of the pipe.')
! #endif
! !------------------------------------------------------------------------------
! !   ! Update qx boundary condition in y-direction (interior cell center)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_qx(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_qx for the centre of the pipe.')
!     call axis_mirroring_interior_fbcy(fl%qx, dm%fbcy_qx, dm%knc_sym, dm%dpcc)
! !------------------------------------------------------------------------------
! !   ! Update qy boundary conditions in y-direction (on nodes)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_qy(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_qy for the centre of the pipe.')
!     call axis_mirroring_interior_fbcy(fl%qy, dm%fbcy_qy, dm%knc_sym, dm%dcpc, &
!             is_qr_qrdr = 1, is_reversed = .true.)
!     ! Update qy/r boundary conditions in y-direction (on nodes)
!     acpc_xpencil = fl%qy
!     call multiple_cylindrical_rn(acpc_xpencil, dm%dcpc, dm%rpi, 1, IPENCIL(1)) ! qr/r
!     call axis_mirroring_interior_fbcy(acpc_xpencil, dm%fbcy_qyr, dm%knc_sym, dm%dcpc, &
!             is_qr_qrdr = 2, is_reversed = .true.)
! !------------------------------------------------------------------------------
! !   Update qz boundary condition in y-direction (interior cell center)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_qz(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_qz for the centre of the pipe.') !
!     call axis_mirroring_interior_fbcy(fl%qz, dm%fbcy_qz, dm%knc_sym, dm%dccp, is_reversed = .true.) ! check
!     dm%fbcy_qzr(:, 1, :) = dm%fbcy_qz(:, 1, :) * dm%rci(1) ! interior, not at axis
!     dm%fbcy_qzr(:, 3, :) = dm%fbcy_qz(:, 3, :) * dm%rci(2)
! !------------------------------------------------------------------------------
! !   Update pressure boundary condition in y-direction (interior)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_pr(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_pr for the centre of the pipe.') !
!     call axis_mirroring_interior_fbcy(fl%pres, dm%fbcy_pr, dm%knc_sym, dm%dccc)
! !------------------------------------------------------------------------------
! !   Thermal variables
! !------------------------------------------------------------------------------
!     if(dm%is_thermo) then
! !------------------------------------------------------------------------------
! !   ! Update gx boundary condition in y-direction (interior)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_qx(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_gx for the centre of the pipe.')
!     call axis_mirroring_interior_fbcy(fl%gx, dm%fbcy_gx, dm%knc_sym, dm%dpcc)
! !------------------------------------------------------------------------------
! !   ! Update gy ang gy/r boundary condition in y-direction (interior)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_qy(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_gy for the centre of the pipe.')
!     call axis_mirroring_interior_fbcy(fl%gy, dm%fbcy_gy, dm%knc_sym, dm%dcpc, &
!             is_qr_qrdr = 1, is_reversed = .true.)
! !------------------------------------------------------------------------------
! !   ! Update gz boundary condition in y-direction (interior)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_qz(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_qz for the centre of the pipe.') !
!     call axis_mirroring_interior_fbcy(fl%gz, dm%fbcy_gz, dm%knc_sym, dm%dccp, is_reversed = .true.)
!     !dm%fbcy_gzr(:, 1, :) = dm%fbcy_gz(:, 1, :) * dm%rci(1)
!     !dm%fbcy_gzr(:, 3, :) = dm%fbcy_gz(:, 3, :) * dm%rci(2)
!     end if

! #ifdef DEBUG_STEPS
!     if(nrank == 0) &
!     call Print_debug_end_msg()
! #endif
!     return
!   end subroutine

! !==============================================================================
! !==============================================================================
!   subroutine update_fbcy_cc_thermo_halo(tm, dm)  ! for cylindrical only
!     use thermo_info_mod
!     use find_max_min_ave_mod
!     use cylindrical_rn_mod
!     implicit none
!     type(t_domain), intent(inout) :: dm
!     type(t_thermo), intent(in)    :: tm
!     real(WP) :: fbcy(dm%dccc%ysz(1), 4, dm%dccc%ysz(3))

!     ! Check if thermo is enabled and the case and coordinate system are valid
!     if (.not. dm%is_thermo .or. &
!         dm%icase /= ICASE_PIPE .or. &
!         dm%icoordinate /= ICYLINDRICAL) return

! !------------------------------------------------------------------------------
! !   ! Update thermo boundary condition in y-direction (interior)
! !------------------------------------------------------------------------------
!     if(dm%ibcy_Tm(1) /= IBC_INTERIOR) call Print_error_msg('Error in ibcy_Tm for the centre of the pipe.') !
!     if(fluidparam%ipropertyState == IPROPERTY_TABLE) then
!       fbcy = dm%fbcy_ftp%h
!       call axis_mirroring_interior_fbcy(tm%hEnth, fbcy, dm%knc_sym, dm%dccc)
!       dm%fbcy_ftp%h = fbcy
!       call ftp_refresh_thermal_properties_from_H_3Dftp(dm%fbcy_ftp)
!     end if

!     if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then
!       fbcy = dm%fbcy_ftp%t
!       call axis_mirroring_interior_fbcy(tm%tTemp, fbcy, dm%knc_sym, dm%dccc)
!       dm%fbcy_ftp%t = fbcy
!       call ftp_refresh_thermal_properties_from_T_undim_3Dftp(dm%fbcy_ftp)
!     end if

!     return
!   end subroutine

!==============================================================================
! to calculate boundary during calculation from primary boundary
  !> Build symmetry-operation metadata for a pair of boundary-condition IDs.
  !> - ibc (in): Boundary-condition IDs at the two sides of a direction.
  !> - mbc (out): Symmetry multiplier or mode information.
  !> - jbc (out): Boundary index relation used by derivative/interpolation kernels.
  subroutine build_bc_symm_operation(ibc, mbc, jbc)
    integer, intent(in)  :: ibc(2)
    integer, intent(out) :: mbc(2, 3)
    integer, intent(in), optional :: jbc(2)

    integer :: i

    mbc(:, JBC_SELF) = ibc(:)
    mbc(:, JBC_GRAD) = ibc(:)
    mbc(:, JBC_PROD) = ibc(:)

    do i = 1, 2
      if(present(jbc)) then

        if(ibc(i)==IBC_SYMMETRIC .and. jbc(i)==IBC_SYMMETRIC) then
          mbc(i, JBC_PROD) = IBC_SYMMETRIC
        else if (ibc(i)==IBC_SYMMETRIC .and. jbc(i)==IBC_ASYMMETRIC) then
          mbc(i, JBC_PROD) = IBC_ASYMMETRIC
        else if (ibc(i)==IBC_ASYMMETRIC .and. jbc(i)==IBC_SYMMETRIC) then
          mbc(i, JBC_PROD) = IBC_ASYMMETRIC
        else if (ibc(i)==IBC_ASYMMETRIC .and. jbc(i)==IBC_ASYMMETRIC) then
          mbc(i, JBC_PROD) = IBC_SYMMETRIC
        else
          if(ibc(i)/=jbc(i)) then
            if(ibc(i) == IBC_DIRICHLET) mbc(i, :) = ibc(i)
            if(jbc(i) == IBC_DIRICHLET) mbc(i, :) = jbc(i)
            if(ibc(i) == IBC_PERIODIC .or. jbc(i) == IBC_PERIODIC) then
              if(nrank==0) write(*, '(A20, I2.1, A5, I2.1)') "BCs for the side ", i, " are ", ibc(i), jbc(i)
              call Print_warning_msg("The two operational variables have different boundary conditions.")
            end if
          else
            mbc(i, :) = ibc(i)
          end if
        end if

      else

        if(ibc(i)==IBC_SYMMETRIC) then
          mbc(i, JBC_SELF) = ibc(i)               ! variable itself
          mbc(i, JBC_GRAD) = IBC_ASYMMETRIC       ! d(var)/dn,
          mbc(i, JBC_PROD) = ibc(i)               ! var * var
        else if(ibc(i)==IBC_ASYMMETRIC) then
          mbc(i, JBC_SELF) = ibc(i)              ! variable itself
          mbc(i, JBC_GRAD) = IBC_SYMMETRIC       ! d(var)/dn,
          mbc(i, JBC_PROD) = IBC_SYMMETRIC       ! var * var
        else
          mbc(i, :) = ibc(i)
        end if

      end if

    end do


    return
  end subroutine
!==============================================================================
  !> Configure equation-specific boundary-condition helper arrays.
  !>
  !> Prepares the boundary-condition metadata used by convective, diffusive,
  !> covariant, and stress-related operations after the basic BC tables are set.
  !> - dm (inout): Domain descriptor receiving equation-specific BC metadata.
  subroutine config_calc_eqs_ibc(dm)
    use wtformat_mod
    type(t_domain), intent(inout)   :: dm

    integer :: mbc(2, 3), mbc0(2, 3)
    integer :: bc(2)
!------------------------------------------------------------------------------
!   x-mom
!------------------------------------------------------------------------------
    call build_bc_symm_operation(dm%ibcx_qx, mbc, dm%ibcx_qx)
    mbcx_cov1(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for x-mom x-convection :", get_name_bc(mbcx_cov1(1)), get_name_bc(mbcx_cov1(2))

    call build_bc_symm_operation(dm%ibcy_qy, mbc, dm%ibcy_qx)
    mbcy_cov1(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for x-mom y-convection :", get_name_bc(mbcy_cov1(1)), get_name_bc(mbcy_cov1(2))

    call build_bc_symm_operation(dm%ibcz_qz, mbc, dm%ibcz_qx)
    mbcz_cov1(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for x-mom z-convection :", get_name_bc(mbcz_cov1(1)), get_name_bc(mbcz_cov1(2))

    call build_bc_symm_operation(dm%ibcx_qx, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcx_ftp, mbc, bc)
    mbcx_tau1(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for x-mom x-diffusion  :", get_name_bc(mbcx_tau1(1)), get_name_bc(mbcx_tau1(2))

    call build_bc_symm_operation(dm%ibcy_qx, mbc) !du/dy_ppc
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcy_ftp, mbc, bc) ! mu_ppc * du/dy_ppc
    call build_bc_symm_operation(dm%ibcy_ftp, mbc0, dm%ibcy_qy) ! mu_ppc * dv/dx_ppc
    !if(dm%icase == ICASE_PIPE) mbc(1, JBC_PROD) = IBC_DIRICHLET
    if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BCy in mbcy_tau1 is wrong.")
    mbcy_tau1(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for x-mom y-diffusion  :", get_name_bc(mbcy_tau1(1)), get_name_bc(mbcy_tau1(2))

    call build_bc_symm_operation(dm%ibcz_qx, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcz_ftp, mbc, bc)
    call build_bc_symm_operation(dm%ibcz_ftp, mbc0, dm%ibcz_qz)
    if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BCz in mbcy_tau1 is wrong.")
    mbcz_tau1(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for x-mom z-diffusion  :", get_name_bc(mbcz_tau1(1)), get_name_bc(mbcz_tau1(2))
!------------------------------------------------------------------------------
!   y-mom
!------------------------------------------------------------------------------
    call build_bc_symm_operation(dm%ibcx_qx, mbc, dm%ibcx_qy)
    mbcx_cov2(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom x-convection :", get_name_bc(mbcx_cov2(1)), get_name_bc(mbcx_cov2(2))

    call build_bc_symm_operation(dm%ibcy_qy, mbc, dm%ibcy_qy)
    mbcy_cov2(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom y-convection :", get_name_bc(mbcy_cov2(1)), get_name_bc(mbcy_cov2(2))

    call build_bc_symm_operation(dm%ibcz_qz, mbc, dm%ibcz_qy)
    mbcz_cov2(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom z-convection :", get_name_bc(mbcz_cov2(1)), get_name_bc(mbcz_cov2(2))

    if(dm%icoordinate == ICYLINDRICAL) then
      call build_bc_symm_operation(dm%ibcy_qz, mbc, dm%ibcy_qz)
      mbcr_cov2(1:2) = mbc(1:2, JBC_PROD)
      if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom r-convection :", get_name_bc(mbcr_cov2(1)), get_name_bc(mbcr_cov2(2))
    end if

    call build_bc_symm_operation(dm%ibcx_qy, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcx_ftp, mbc, bc)
    call build_bc_symm_operation(dm%ibcx_ftp, mbc0, dm%ibcx_qx)
    if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BC in mbcx_tau2 is wrong.")
    mbcx_tau2(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom x-diffusion  :", get_name_bc(mbcx_tau2(1)), get_name_bc(mbcx_tau2(2))

    call build_bc_symm_operation(dm%ibcy_qy, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcy_ftp, mbc, bc)
    mbcy_tau2(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom y-diffusion  :", get_name_bc(mbcy_tau2(1)), get_name_bc(mbcy_tau2(2))

    call build_bc_symm_operation(dm%ibcz_qy, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcz_ftp, mbc, bc)
    call build_bc_symm_operation(dm%ibcz_ftp, mbc0, dm%ibcz_qz)
    if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BC in mbcz_tau2 is wrong.")
    mbcz_tau2(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom z-diffusion  :", get_name_bc(mbcz_tau2(1)), get_name_bc(mbcz_tau2(2))

    if(dm%icoordinate == ICYLINDRICAL) then
      call build_bc_symm_operation(dm%ibcy_qz, mbc, dm%ibcy_ftp)
      mbcr_tau2(1:2) = mbc(1:2, JBC_PROD)
      if(nrank==0) write(*, wrtfmt3s) "The bc for y-mom r-diffusion  :", get_name_bc(mbcr_tau2(1)),  get_name_bc(mbcr_tau2(2))
    end if
!------------------------------------------------------------------------------
!   z-mom
!------------------------------------------------------------------------------
    call build_bc_symm_operation(dm%ibcx_qx, mbc, dm%ibcx_qz)
    mbcx_cov3(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom x-convection :", get_name_bc(mbcx_cov3(1)), get_name_bc(mbcx_cov3(2))

    call build_bc_symm_operation(dm%ibcy_qy, mbc, dm%ibcy_qz)
    mbcy_cov3(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom y-convection :", get_name_bc(mbcy_cov3(1)), get_name_bc(mbcy_cov3(2))

    call build_bc_symm_operation(dm%ibcz_qz, mbc, dm%ibcz_qz)
    mbcz_cov3(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom z-convection :", get_name_bc(mbcz_cov3(1)), get_name_bc(mbcz_cov3(2))

    if(dm%icoordinate == ICYLINDRICAL) then
      call build_bc_symm_operation(dm%ibcy_qy, mbc, dm%ibcy_qz)
      mbcr_cov3(1:2) = mbc(1:2, JBC_PROD)
      if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom r-convection :", get_name_bc(mbcr_cov3(1)), get_name_bc(mbcr_cov3(2))
    end if

    call build_bc_symm_operation(dm%ibcx_qz, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcx_ftp, mbc, bc)
    call build_bc_symm_operation(dm%ibcx_ftp, mbc0, dm%ibcx_qx)
    if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BCx in mbcx_tau3 is wrong.")
    mbcx_tau3(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom x-diffusion  :", get_name_bc(mbcx_tau3(1)), get_name_bc(mbcx_tau3(2))

    call build_bc_symm_operation(dm%ibcy_qz, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcy_ftp, mbc, bc)
    call build_bc_symm_operation(dm%ibcy_ftp, mbc0, dm%ibcy_qy)
    !write(*,*) get_name_bc(mbc0(1, JBC_PROD)), get_name_bc(mbc(1, JBC_PROD))
    if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BCy in mbcy_tau3 is wrong.")
    mbcy_tau3(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom y-diffusion  :", get_name_bc(mbcy_tau3(1)), get_name_bc(mbcy_tau3(2))

    call build_bc_symm_operation(dm%ibcz_qz, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcz_ftp, mbc, bc)
    call build_bc_symm_operation(dm%ibcz_ftp, mbc0, dm%ibcz_qz)
    if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BCz in mbcy_tau3 is wrong.")
    mbcz_tau3 = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom z-diffusion  :", get_name_bc(mbcz_tau3(1)), get_name_bc(mbcz_tau3(2))

    if(dm%icoordinate == ICYLINDRICAL) then
      call build_bc_symm_operation(dm%ibcy_qz, mbc)
      bc(1:2) = mbc(1:2, JBC_GRAD)
      call build_bc_symm_operation(dm%ibcy_ftp, mbc, bc)
      call build_bc_symm_operation(dm%ibcy_ftp, mbc0, dm%ibcy_qz)
      if(mbc0(1, JBC_PROD)/= mbc(1, JBC_PROD)) call Print_error_msg("BCr in mbcy_tau3 is wrong.")
      mbcr_tau3(1:2) = mbc(1:2, JBC_PROD)
      if(nrank==0) write(*, wrtfmt3s) "The bc for z-mom r-diffusion  :", get_name_bc(mbcr_tau3(1)), get_name_bc(mbcr_tau3(2))
    end if
!------------------------------------------------------------------------------
!   energy-eqs
!------------------------------------------------------------------------------
    if(dm%is_thermo)  then
    call build_bc_symm_operation(dm%ibcx_qx, mbc, dm%ibcx_ftp)
    ebcx_conv(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for energy x-convection :", get_name_bc(ebcx_conv(1)), get_name_bc(ebcx_conv(2))

    call build_bc_symm_operation(dm%ibcy_qy, mbc, dm%ibcy_ftp)
    ebcy_conv(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for energy y-convection :", get_name_bc(ebcy_conv(1)), get_name_bc(ebcy_conv(2))

    call build_bc_symm_operation(dm%ibcz_qz, mbc, dm%ibcz_ftp)
    ebcz_conv(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for energy z-convection :", get_name_bc(ebcz_conv(1)), get_name_bc(ebcz_conv(2))

    call build_bc_symm_operation(dm%ibcx_Tm, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcx_ftp, mbc, bc)
    ebcx_difu = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for energy x-diffusion  :", get_name_bc(ebcx_difu(1)), get_name_bc(ebcx_difu(2))

    call build_bc_symm_operation(dm%ibcy_Tm, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcy_ftp, mbc, bc)
    ebcy_difu(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for energy y-diffusion  :", get_name_bc(ebcy_difu(1)), get_name_bc(ebcy_difu(2))

    call build_bc_symm_operation(dm%ibcz_Tm, mbc)
    bc(1:2) = mbc(1:2, JBC_GRAD)
    call build_bc_symm_operation(dm%ibcz_ftp, mbc, bc)
    ebcz_difu(1:2) = mbc(1:2, JBC_PROD)
    if(nrank==0) write(*, wrtfmt3s) "The bc for energy z-diffusion  :", get_name_bc(ebcz_difu(1)), get_name_bc(ebcz_difu(2))
    end if
!------------------------------------------------------------------------------
! preparation for b.c. - Dirichlet
!------------------------------------------------------------------------------
    is_fbcx_velo_required = .false.
    if(dm%ibcx_qx(1) == IBC_DIRICHLET .or. &
       dm%ibcx_qx(2) == IBC_DIRICHLET .or. &
       dm%ibcx_qy(1) == IBC_DIRICHLET .or. &
       dm%ibcx_qy(2) == IBC_DIRICHLET .or. &
       dm%ibcx_qz(1) == IBC_DIRICHLET .or. &
       dm%ibcx_qz(2) == IBC_DIRICHLET ) then
       is_fbcx_velo_required = .true.
      ! to add neumann later, check
    end if
    is_fbcy_velo_required = .false.
    if(dm%ibcy_qx(1) == IBC_DIRICHLET .or. &
       dm%ibcy_qx(2) == IBC_DIRICHLET .or. &
       dm%ibcy_qy(1) == IBC_DIRICHLET .or. &
       dm%ibcy_qy(2) == IBC_DIRICHLET .or. &
       dm%ibcy_qz(1) == IBC_DIRICHLET .or. &
       dm%ibcy_qz(2) == IBC_DIRICHLET ) then
       is_fbcy_velo_required = .true.
      ! to add neumann later, check
    end if
    is_fbcz_velo_required = .false.
    if(dm%ibcz_qx(1) == IBC_DIRICHLET .or. &
       dm%ibcz_qx(2) == IBC_DIRICHLET .or. &
       dm%ibcz_qy(1) == IBC_DIRICHLET .or. &
       dm%ibcz_qy(2) == IBC_DIRICHLET .or. &
       dm%ibcz_qz(1) == IBC_DIRICHLET .or. &
       dm%ibcz_qz(2) == IBC_DIRICHLET ) then
       is_fbcz_velo_required = .true.
      ! to add neumann later, check
    end if
!------------------------------------------------------------------------------
! preparation for b.c. - INTERIOR - check here!!! to do!
!------------------------------------------------------------------------------
    if(dm%ibcx_qx(1) == IBC_INTERIOR .or. &
       dm%ibcx_qx(2) == IBC_INTERIOR .or. &
       dm%ibcx_qy(1) == IBC_INTERIOR .or. &
       dm%ibcx_qy(2) == IBC_INTERIOR .or. &
       dm%ibcx_qz(1) == IBC_INTERIOR .or. &
       dm%ibcx_qz(2) == IBC_INTERIOR ) then
       is_fbcx_velo_required = .true.
      ! to add neumann later, check
    end if
    if(dm%ibcy_qx(1) == IBC_INTERIOR .or. &
       dm%ibcy_qx(2) == IBC_INTERIOR .or. &
       dm%ibcy_qy(1) == IBC_INTERIOR .or. &
       dm%ibcy_qy(2) == IBC_INTERIOR .or. &
       dm%ibcy_qz(1) == IBC_INTERIOR .or. &
       dm%ibcy_qz(2) == IBC_INTERIOR ) then
       is_fbcy_velo_required = .true.
      ! to add neumann later, check
    end if
    if(dm%ibcz_qx(1) == IBC_INTERIOR .or. &
       dm%ibcz_qx(2) == IBC_INTERIOR .or. &
       dm%ibcz_qy(1) == IBC_INTERIOR .or. &
       dm%ibcz_qy(2) == IBC_INTERIOR .or. &
       dm%ibcz_qz(1) == IBC_INTERIOR .or. &
       dm%ibcz_qz(2) == IBC_INTERIOR ) then
       is_fbcz_velo_required = .true.
      ! to add neumann later, check
    end if

    return
  end subroutine

!==============================================================================
!==============================================================================
  !> Fill x-face thermal boundary values from the thermal state.
  !> - ibc (in): Boundary-condition IDs for the x faces.
  !> - dm (in): Domain descriptor.
  !> - fbc (out): Face-boundary thermal values.
  !> - tm (in): Thermal state.
  subroutine get_fbcx_iTh(ibc, dm, fbc, tm, opt_k)
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: ibc(2)
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm
    real(WP), intent(in), optional :: opt_k(:, :, :)
    real(WP), intent(out) :: fbc(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3))

    integer :: n
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc
    real(WP) :: framp

    fbcx_4cc = ONE
    framp = ONE
    if(ibc(1) == IBC_NEUMANN .or. &
       ibc(2) == IBC_NEUMANN) then
      if(.not. present(opt_k)) &
      call Print_error_msg("opt_k is required for Neumann BC in get_fbcx_iTh.")
      call extract_dirichlet_fbcx(fbcx_4cc, opt_k, dm%dpcc)
      if(tm%is_use_qw_ramp) framp = get_qw_ramp_factor(tm%iteration, tm%istt_qw_ramp, tm%iend_qw_ramp)
    end if

    fbc = ZERO
    do n = 1, 2
      if(ibc(n) == IBC_DIRICHLET) then
        fbc(n, :, :) = dm%fbcx_ftp(n, :, :)%t
      else if(ibc(n) == IBC_NEUMANN) then
        fbc(n, :, :) = -dm%fbcx_qw(n, :, :)/fbcx_4cc(n, :, :) * framp
      else
        fbc(n, :, :) = ZERO
      end if
    end do
    return
  end subroutine
!==============================================================================
  !> Fill y-face thermal boundary values from the thermal state.
  !> - ibc (in): Boundary-condition IDs for the y faces.
  !> - dm (in): Domain descriptor.
  !> - fbc (out): Face-boundary thermal values.
  !> - tm (in): Thermal state.
  subroutine get_fbcy_iTh(ibc, dm, fbc, tm, opt_k)
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: ibc(2)
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm
    real(WP), intent(in), optional :: opt_k(:, :, :)
    real(WP), intent(out) :: fbc(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3))

    integer :: n
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP) :: framp

    fbcy_c4c = ONE
    framp = ONE
    if(ibc(1) == IBC_NEUMANN .or. &
       ibc(2) == IBC_NEUMANN) then
      if(.not. present(opt_k)) &
      call Print_error_msg("opt_k is required for Neumann BC in get_fbcy_iTh.")
      call extract_dirichlet_fbcy(fbcy_c4c, opt_k, dm%dcpc, dm)
      if(tm%is_use_qw_ramp) framp = get_qw_ramp_factor(tm%iteration, tm%istt_qw_ramp, tm%iend_qw_ramp)
    end if

    fbc = ZERO
    do n = 1, 2
      if(ibc(n) == IBC_DIRICHLET) then
        fbc(:, n, :) = dm%fbcy_ftp(:, n, :)%t
      else if(ibc(n) == IBC_NEUMANN) then
        if(n==1) fbc(:, n, :) = -dm%fbcy_qw(:, n, :) / fbcy_c4c(:, n, :) * framp
        if(n==2) fbc(:, n, :) =  dm%fbcy_qw(:, n, :) / fbcy_c4c(:, n, :) * framp
      else
        fbc(:, n, :) = ZERO
      end if
    end do
    return
  end subroutine
!==============================================================================
  !> Fill z-face thermal boundary values from the thermal state.
  !> - ibc (in): Boundary-condition IDs for the z faces.
  !> - dm (in): Domain descriptor.
  !> - fbc (out): Face-boundary thermal values.
  !> - tm (in): Thermal state.
  subroutine get_fbcz_iTh(ibc, dm, fbc, tm, opt_k)
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    integer, intent(in) :: ibc(2)
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm
    real(WP), intent(in), optional :: opt_k(:, :, :)
    real(WP), intent(out) :: fbc(dm%dccp%zsz(1), dm%dccp%zsz(2), 4)

    integer :: n
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4
    real(WP) :: framp

    fbcz_cc4 = ONE
    framp = ONE
    if(ibc(1) == IBC_NEUMANN .or. &
       ibc(2) == IBC_NEUMANN) then
      if(.not. present(opt_k)) &
      call Print_error_msg("opt_k is required for Neumann BC in get_fbcz_iTh.")
      call extract_dirichlet_fbcz(fbcz_cc4, opt_k, dm%dccp)
      if(tm%is_use_qw_ramp) framp = get_qw_ramp_factor(tm%iteration, tm%istt_qw_ramp, tm%iend_qw_ramp)
    end if

    fbc = ZERO
    do n = 1, 2
      if(ibc(n) == IBC_DIRICHLET) then
        fbc(:, :, n) = dm%fbcz_ftp(:, :, n)%t
      else if(ibc(n) == IBC_NEUMANN) then
        fbc(:, :, n) = -dm%fbcz_qw(:, :, n) / fbcz_cc4(:, :, n) * framp
      else
        fbc(:, :, n) = ZERO
      end if
    end do
    return
  end subroutine


!==============================================================================


!==============================================================================
end module
