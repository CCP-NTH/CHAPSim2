!> Divergence and mass-conservation diagnostics.
!>
!> Provides divergence operators for flow and generic vector fields, including
!> support for staggered decompositions and element-wise mass-conservation
!> checks used by monitor and debug output.
module continuity_eq_mod
  use decomp_2d
  use operations

  public :: Get_divergence_vector
  public :: Get_divergence_flow
  public :: Get_divergence_vel_x2z
  public :: Check_element_mass_conservation
contains
!==============================================================================
!==============================================================================
!> To calculate divergence of (rho * u) or divergence of (u)
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - div (out): div(q) or div(g)
!> - d (in): domain
!_______________________________________________________________________________
  !> Compute divergence of the active flow mass-flux or velocity field.
  !> - fl (in): Flow state containing velocity or mass-flux components.
  !> - div (out): Divergence field on cell centres.
  !> - dm (in): Domain descriptor.
  subroutine Get_divergence_flow(fl, div, dm)
    use cylindrical_rn_mod
    use parameters_constant_mod
    use solver_tools_mod
    use udf_type_mod
    implicit none

    type(t_domain), intent(in) :: dm
    type(t_flow),    intent(in) :: fl
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent (out) :: div

    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)):: qx
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)):: qy
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)):: qz

    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div0
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: div0_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: div0_zpencil

    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: qy_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: qz_ypencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: qz_zpencil

    real(WP), dimension(4,                dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx
    real(WP), dimension(dm%dcpc%ysz(1), 4,                dm%dcpc%ysz(3)) :: fbcy
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4                ) :: fbcz
    integer :: iacc_prj(NDIM)

!------------------------------------------------------------------------------
!   A thermal run takes the divergence of the mass flux g, not of the velocity q,
!   so the boundary values must be the g ones too: fbc_g = fbc_q * rho_boundary.
!   The two only coincide where the boundary density is one or the boundary value
!   is zero - a no-slip wall, or an inlet at the reference temperature - and they
!   part company at a heated outlet, which feeds wrong values straight into the
!   Poisson right-hand side. The ibc_* index arrays are shared between q and g
!   and stay as they are; this mirrors eq_momentum2:701/740/771.
!
!   Latent at the moment, deliberately fixed anyway: the boundary row of the P2C
!   first derivative multiplies the ghost by d1rP2C(1,2,..) = b/3, and b = 0 at
!   CD2 (basics_operations2:750-752), so no fbc value reaches the result while that
!   direction's iacc_prj is CD2. It becomes live as soon as a non-periodic direction
!   runs above CD2 - a thermal inlet/outlet case at iaccuracy >= CD4.
!------------------------------------------------------------------------------
    if(dm%is_thermo) then
      qx = fl%gx
      qy = fl%gy
      qz = fl%gz
      fbcx = dm%fbcx_gx
      fbcy = dm%fbcy_gy
      fbcz = dm%fbcz_gz
    else
      qx = fl%qx
      qy = fl%qy
      qz = fl%qz
      fbcx = dm%fbcx_qx
      fbcy = dm%fbcy_qy
      fbcz = dm%fbcz_qz
    end if

    div = ZERO
    ! One scheme per direction, shared with the pressure gradient in eq_momentum2 and the
    ! Poisson wavenumbers in poisson_1stderivcomp_fft2d. See get_projection_accuracy.
    iacc_prj = get_projection_accuracy(dm)
!------------------------------------------------------------------------------
! operation in x pencil, dqx/dx
!------------------------------------------------------------------------------
    div0 = ZERO
    call Get_x_1der_P2C_3D(qx, div0, dm, iacc_prj(1), dm%ibcx_qx(:), fbcx)
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
!------------------------------------------------------------------------------
! operation in y pencil, dqy/dy * (1/r)
!------------------------------------------------------------------------------
    qy_ypencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(qy, qy_ypencil, dm%dcpc)
    call Get_y_1der_P2C_3D(qy_ypencil, div0_ypencil, dm, iacc_prj(2), dm%ibcy_qy(:), fbcy)
    call transpose_y_to_x(div0_ypencil, div0, dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
!------------------------------------------------------------------------------
! operation in z pencil, dw/dz * (1/r)
!------------------------------------------------------------------------------
    qz_ypencil = ZERO
    qz_zpencil = ZERO
    div0_zpencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(qz,         qz_ypencil, dm%dccp)
    call transpose_y_to_z(qz_ypencil, qz_zpencil, dm%dccp)
    call Get_z_1der_P2C_3D(qz_zpencil, div0_zpencil, dm, iacc_prj(3), dm%ibcz_qz, fbcz)
    call transpose_z_to_y(div0_zpencil, div0_ypencil, dm%dccc)
    call transpose_y_to_x(div0_ypencil, div0,         dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)

    return
  end subroutine

!==============================================================================
!==============================================================================
!> To calculate divergence of (rho * u) or divergence of (u)
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - ux (in): ux or gx
!> - uy (in): uy or gy
!> - uz (in): uz or gz
!> - div (out): div(u) or div(g)
!> - d (in): domain
!_______________________________________________________________________________
  !> Compute divergence of vector components on staggered layouts.
  !> - ux (in): x-component on x-staggered layout.
  !> - uy (in): y-component on y-staggered layout.
  !> - uz (in): z-component on z-staggered layout.
  !> - div (out): Divergence field on cell centres.
  !> - dm (in): Domain descriptor.
  !> - opt_fbcx/opt_fbcy/opt_fbcz (in): Boundary planes of the vector being
  !>   differenced. Optional; the velocity planes are used when they are absent.
  subroutine Get_divergence_vector(ux, uy, uz, div, dm, opt_fbcx, opt_fbcy, opt_fbcz)
    use cylindrical_rn_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none

    type(t_domain), intent (in) :: dm
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in ) :: ux
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in ) :: uy
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in ) :: uz
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)), intent (out) :: div
    real(WP), dimension(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in ), optional :: opt_fbcx
    real(WP), dimension(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)), intent (in ), optional :: opt_fbcy
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2),              4), intent (in ), optional :: opt_fbcz

    real(WP), dimension(             4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx
    real(WP), dimension(dm%dcpc%ysz(1),              4, dm%dcpc%ysz(3)) :: fbcy
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2),              4) :: fbcz

    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div0
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: div0_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: div0_zpencil

    real(WP), dimension(dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: uy_ypencil
    real(WP), dimension(dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: uz_ypencil
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3)) :: uz_zpencil
    integer :: iacc_prj(NDIM)

    div = ZERO
!------------------------------------------------------------------------------
!   Both callers of this routine are on either side of the same FFT Poisson solve:
!   eq_mhd:593 builds div(u x B) as the right-hand side for the electric potential,
!   and eq_mhd:669 checks div(j) afterwards. So the differencing here has to be the
!   one the solver inverts, not the one the physics terms ask for - the same
!   requirement Get_divergence_flow has. See get_projection_accuracy.
!------------------------------------------------------------------------------
    iacc_prj = get_projection_accuracy(dm)
!------------------------------------------------------------------------------
!   The boundary *topology* below is necessarily the velocity's: ux/uy/uz share the
!   staggering and the domain sides of qx/qy/qz, so the ibc flags carry over. The
!   boundary *values* do not. For u x B and for j the velocity planes are simply a
!   different field - at an inlet dm%fbcx_qx is the prescribed u_x profile, and at
!   the pipe axis dm%fbcy_qy holds the qy mirror, neither of which is the boundary
!   data of the vector being differenced here. Callers that pass something other
!   than a velocity must therefore supply opt_fbc*; the defaults below are correct
!   only for a velocity. This is dormant at CD2, where the P2C first derivative at
!   a boundary cell reads no ghost, and becomes an O(1) error at CD4 and above.
!------------------------------------------------------------------------------
    if(present(opt_fbcx)) then
      fbcx = opt_fbcx
    else
      fbcx = dm%fbcx_qx
    end if
    if(present(opt_fbcy)) then
      fbcy = opt_fbcy
    else
      fbcy = dm%fbcy_qy
    end if
    if(present(opt_fbcz)) then
      fbcz = opt_fbcz
    else
      fbcz = dm%fbcz_qz
    end if
!------------------------------------------------------------------------------
! operation in x pencil, du/dx
!------------------------------------------------------------------------------
    div0 = ZERO
    call Get_x_1der_P2C_3D(ux, div0, dm, iacc_prj(1), dm%ibcx_qx(:), fbcx)
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
    !write(*,*) 'div, x', div0(1, 1, 1), div0(2, 2, 2), div0(8, 8, 8)!, div0(16, 8, 8), div0(32, 8, 8)
!------------------------------------------------------------------------------
! operation in y pencil, dqy/dy * (1/r)
!------------------------------------------------------------------------------
    uy_ypencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(uy, uy_ypencil, dm%dcpc)
    call Get_y_1der_P2C_3D(uy_ypencil, div0_ypencil, dm, iacc_prj(2), dm%ibcy_qy(:), fbcy)
    call transpose_y_to_x(div0_ypencil, div0, dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
    !write(*,*) 'div, y', div0(1, 1, 1), div0(2, 2, 2), div0(8, 8, 8)!, div0(16, 8, 8), div0(32, 8, 8)
!------------------------------------------------------------------------------
! operation in z pencil, dw/dz * (1/r)
!------------------------------------------------------------------------------
    uz_ypencil = ZERO
    uz_zpencil = ZERO
    div0_zpencil = ZERO
    div0_ypencil = ZERO
    div0 = ZERO
    call transpose_x_to_y(uz,         uz_ypencil, dm%dccp)
    call transpose_y_to_z(uz_ypencil, uz_zpencil, dm%dccp)
    call Get_z_1der_P2C_3D(uz_zpencil, div0_zpencil, dm, iacc_prj(3), dm%ibcz_qz(:), fbcz)
    call transpose_z_to_y(div0_zpencil, div0_ypencil, dm%dccc)
    call transpose_y_to_x(div0_ypencil, div0,         dm%dccc)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0, dm%dccc, dm%rci, 1, IPENCIL(1))
    div(:, :, :) = div(:, :, :) + div0(:, :, :)
    !write(*,*) 'div, z', div0(1, 1, 1), div0(2, 2, 2), div0(8, 8, 8)!, div0(16, 8, 8), div0(32, 8, 8)
    !write(*,*) 'divall', div0(1, 1, 1), div(8, 8, 8)
    return
  end subroutine

!==============================================================================
!==============================================================================
!> To calculate divergence of (rho * u) or divergence of (u)
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - ux (in): ux or gx
!> - uy (in): uy or gy
!> - uz (in): uz or gz
!> - div (out): div(u) or div(g)
!> - d (in): domain
!_______________________________________________________________________________
  subroutine Get_divergence_vel_x2z(ux, uy, uz, div_zpencil_ggg, dm)
    use cylindrical_rn_mod
    use decomp_extended_mod
    use parameters_constant_mod
    use poisson_interface_mod
    use transpose_extended_mod
    use udf_type_mod
    implicit none

    type(t_domain), intent (in) :: dm
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in) :: ux
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in) :: uy
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in) :: uz
    real(WP), dimension(dm%dccc%zst(1) : dm%dccc%zen(1), &
                        dm%dccc%zst(2) : dm%dccc%zen(2), &
                        dm%dccc%zst(3) : dm%dccc%zen(3)), intent (out) :: div_zpencil_ggg

    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div0
    real(WP), dimension(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3)) :: div0_ypencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: div0_zpencil
    real(WP), dimension(dm%dccc%yst(1) : dm%dccc%yen(1), &
                        dm%dccc%yst(2) : dm%dccc%yen(2), &
                        dm%dccc%ysz(3))                  :: div0_ypencil_ggl
    real(WP), dimension(dm%dccc%yst(1) : dm%dccc%yen(1), &
                        dm%dccc%yst(2) : dm%dccc%yen(2), &
                        dm%dccc%ysz(3))                  :: div_ypencil_ggl
    real(WP), dimension(dm%dccc%zst(1) : dm%dccc%zen(1), &
                        dm%dccc%zst(2) : dm%dccc%zen(2), &
                        dm%dccc%zst(3) : dm%dccc%zen(3)) :: div0_zpencil_ggg

    real(WP), dimension(dm%dcpc%ysz(1),                  dm%dcpc%ysz(2), dm%dcpc%ysz(3)) :: uy_ypencil
    !real(WP), dimension(dm%dccp%yst(1) : dm%dccp%yen(1), dm%dccp%ysz(2), dm%dccp%ysz(3)) :: uz_ypencil_ggl

    real(WP), dimension(dm%dccp%ysz(1),                  dm%dccp%ysz(2),                  dm%dccp%ysz(3)) :: uz_ypencil
    real(WP), dimension(dm%dccp%zsz(1),                  dm%dccp%zsz(2),                  dm%dccp%zsz(3)) :: uz_zpencil

!------------------------------------------------------------------------------
! operation in x pencil, du/dx
!------------------------------------------------------------------------------
    div0 = ZERO
    div0_ypencil_ggl = ZERO
    div_ypencil_ggl = ZERO
    call Get_x_1der_P2C_3D(ux, div0, dm, dm%iAccuracy, dm%ibcx_qx(:), dm%fbcx_qx)
    call transpose_x_to_y(div0, div0_ypencil_ggl, dm%dccc)
    div_ypencil_ggl = div0_ypencil_ggl
!------------------------------------------------------------------------------
! operation in y pencil, dv/dy * (1/r)
!------------------------------------------------------------------------------
    uy_ypencil = ZERO
    div0_ypencil = ZERO
    div0_ypencil_ggl = ZERO
    call transpose_x_to_y(uy, uy_ypencil, dm%dcpc)
    call Get_y_1der_P2C_3D(uy_ypencil, div0_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
    call ypencil_index_lgl2ggl(div0_ypencil, div0_ypencil_ggl, dm%dccc)
    div_ypencil_ggl = div_ypencil_ggl + div0_ypencil_ggl
    call transpose_y_to_z(div_ypencil_ggl, div_zpencil_ggg, dm%dccc)
!------------------------------------------------------------------------------
! operation in z pencil, dw/dz * (1/r)
!------------------------------------------------------------------------------
    uz_ypencil = ZERO
    uz_zpencil = ZERO
    div0_zpencil = ZERO
    div0_zpencil_ggg = ZERO
    call transpose_x_to_y(uz,         uz_ypencil, dm%dccp)
    call transpose_y_to_z(uz_ypencil, uz_zpencil, dm%dccp)
    call Get_z_1der_P2C_3D(uz_zpencil, div0_zpencil, dm, dm%iAccuracy, dm%ibcz_qz(:), dm%fbcz_qz)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(div0_zpencil, dm%dccc, dm%rci, 1, IPENCIL(3))
    call zpencil_index_llg2ggg(div0_zpencil, div0_zpencil_ggg, dm%dccc)
    div_zpencil_ggg = div_zpencil_ggg + div0_zpencil_ggg

    return
  end subroutine

!==============================================================================
!==============================================================================
!> To calculate divergence of (rho * u) or divergence of (u)
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - ux (in): ux or gx
!> - uy (in): uy or gy
!> - uz (in): uz or gz
!> - div (out): div(u) or div(g)
!> - d (in): domain
!_______________________________________________________________________________
  !> Check and report element-wise mass-conservation residuals.
  !> - fl (in): Flow state to check.
  !> - dm (in): Domain descriptor.
  !> - opt_isub (in): Optional substep index for diagnostics.
  !> - opt_str (in): Optional label appended to diagnostic messages.
  subroutine Check_element_mass_conservation(fl, dm, opt_isub, opt_str)
    use cylindrical_rn_mod
    use input_general_mod
    use math_mod
    use mpi_mod
    use parameters_constant_mod
    use precision_mod
    use solver_tools_mod
    use udf_type_mod
    use wtformat_mod
    !use visualisation_field_mod
    use find_max_min_ave_mod
    use typeconvert_mod
    implicit none

    type(t_domain), intent( in) :: dm
    type(t_flow),   intent( inout) :: fl
    integer, intent(in), optional :: opt_isub
    character(*), intent(in), optional :: opt_str

    character(32) :: str
    integer :: n, nlayer, isub
    real(WP) :: mm(2), mm_projected(2), mass_balance(8), safety_mass_residual
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div, div_projected, drhodt
    !----------------------------------------------------------------
    ! safe-proof
    !----------------------------------------------------------------
    if(present(opt_str)) then
      str = trim(opt_str)//'_iter_'//int2str(fl%iteration)
    else
      str = 'at iter = '//int2str(fl%iteration)
    end if

    if(present(opt_isub)) then
      isub = opt_isub
    else
      isub = 0
    end if
    !----------------------------------------------------------------
    ! Calculate mass conservation residual
    !----------------------------------------------------------------
    ! $d\rho / dt$ at cell centre
    if (dm%is_thermo) then
      drhodt = fl%drhodt
    else
      drhodt = ZERO
    end if
    !
    ! $d(\rho u_i)) / dx_i $ at cell centre
    div = ZERO
    call Get_divergence_flow(fl, div, dm)
    div = div + drhodt
    if(dm%icoordinate == ICYLINDRICAL .and. any(dm%is_conv_outlet(:))) then
      ! The cylindrical zero-mode correction represented by amplitude C is C/r^2.
      div_projected = -fl%poisson_projected_source_amplitude
      call multiple_cylindrical_rn(div_projected, dm%dccc, dm%rci, 2, IPENCIL(1))
      div_projected = div + div_projected
    else
      ! The Cartesian zero-mode correction represented by amplitude C is uniform.
      div_projected = div - fl%poisson_projected_source_amplitude
    end if
    !
!#ifdef DEBUG_STEPS
    !if(MOD(fl%iteration, dm%visu_nfre) == 0) &
    !call write_visu_any3darray(div, 'masserror', 'debug', dm%dccc, dm, fl%iteration)
!#endif
    !----------------------------------------------------------------
    ! Find Max. mass conservation residual
    !----------------------------------------------------------------
    n = dm%dccc%xsz(1)
    !mm0 = fl%mcon(1)
    fl%mcon = ZERO
    fl%mcon_projected = ZERO
    mm = ZERO
    mm_projected = ZERO
    !
    if(dm%is_periodic(1)) then
      nlayer = 0
    else
      nlayer = 4
      call Find_max_min_3d(div(1:nlayer, :, :), opt_abs='ABS', opt_calc='MAXI', &
            opt_work=mm, opt_name="Physical- Mass Consv. (inlet  4):")
      fl%mcon(2) = mm(2)
      call Find_max_min_3d(div_projected(1:nlayer, :, :), opt_abs='ABS', opt_calc='MAXI', &
            opt_work=mm_projected, opt_name="Projected Mass Consv. (inlet  4):")
      fl%mcon_projected(2) = mm_projected(2)
      call Find_max_min_3d(div(n-nlayer+1:n, :, :), opt_abs='ABS', opt_calc='MAXI', &
            opt_work=mm, opt_name="Physical- Mass Consv. (outlet 4):")
      fl%mcon(3) = mm(2)

      call Find_max_min_3d(div_projected(n-nlayer+1:n, :, :), opt_abs='ABS', opt_calc='MAXI', &
            opt_work=mm_projected, opt_name="Projected Mass Consv. (outlet 4):")
      fl%mcon_projected(3) = mm_projected(2)
    end if
    call Find_max_min_3d(div(nlayer+1:n-nlayer, :, :), opt_abs='ABS', opt_calc='MAXI', &
        opt_work=mm, opt_name="Physical- Mass Consv. (bulk    ):")
    fl%mcon(1) = mm(2)
    call Find_max_min_3d(div_projected(nlayer+1:n-nlayer, :, :), opt_abs='ABS', opt_calc='MAXI', &
        opt_work=mm_projected, opt_name="Projected Mass Consv. (bulk    ):")
    fl%mcon_projected(1) = mm_projected(2)
    call check_global_mass_balance(mass_balance, fl%drhodt, dm)
    fl%tt_mass_change = mass_balance(8)
    if(any(dm%is_conv_outlet(:))) then
      safety_mass_residual = fl%mcon_projected(1)
    else
      safety_mass_residual = fl%mcon(1)
    end if
    !fl%mcon(4) = safe_divide(fl%mcon(1)-mm0, dabs(mm0))
    !if(nrank==0) write(*, '(A,1F9.2,A)') ' ', fl%mcon(4)*100.0_WP, '%'
    ! terminate code once too large
    if(nrank == 0) then
      if(safety_mass_residual > 2.0_WP .and. fl%iteration > 10000 ) &
      call Print_error_msg("Mass conservation error exceeds tolerance. Terminate run and investigate.")
    end if
    if (nrank == 0) then
      write (*, wrtfmt1el) 'global mass flux imbalance :', fl%tt_mass_change
      write (*, wrtfmt1el) 'physical Poisson compatibility defect:', fl%physical_poisson_compatibility_defect
      write (*, wrtfmt1el) 'explicit uniform Poisson-source correction:', fl%uniform_poisson_source_correction
      write (*, wrtfmt1el) 'Poisson projected-source amplitude:', fl%poisson_projected_source_amplitude
      write (*, wrtfmt1el) 'Poisson zero-mode projection (scaled solver RHS):', fl%poisson_zero_mode_rhs_projection
    end if
    !----------------------------------------------------------------
    ! turn on numerical tricks based on mass conservation residual
    !----------------------------------------------------------------
    if(fl%iteration >= 1 .and. isub > 0) then
      if(dm%is_conv_outlet(1) .or. dm%is_conv_outlet(3)) then
        if(.not. is_damping_drhodt) then
          if(safety_mass_residual > 1.0_WP) then
            is_damping_drhodt = .true.
            if(nrank==0) call Print_warning_msg('drho/dt damping function is on.')
          end if
        end if
      else
        if(.not. is_global_mass_correction) then
          if(abs(fl%tt_mass_change) > 1.0e-2_WP) then
            is_global_mass_correction = .true. ! scaled convective b.c. has already met this.
            if(nrank==0) call Print_warning_msg('is_global_mass_correction is True for RHS of Pression Poisson Eq.')
          end if
        end if
      end if
    end if
    return
  end subroutine Check_element_mass_conservation

end module continuity_eq_mod
