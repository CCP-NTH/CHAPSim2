!> Energy equation and variable-property update routines.
!>
!> This module advances the thermal/enthalpy equation, updates density,
!> viscosity, conductivity, and related property fields, and computes density
!> time-derivative information needed by variable-property flow.
module eq_energy_mod
  use decomp_2d
  use operations
  use precision_mod, only : WP
  use wrt_debug_field_mod
  implicit none
!------------------------------------------------------------------------------
! Kays' correlation for the subgrid turbulent Prandtl number,
!   Pr_sgs = PR_SGS_INF + PR_SGS_COEF / Pe_sgs,   Pe_sgs = Pr * mu_sgs / mu
! where Pr and mu are the local *molecular* values. Pe_sgs -> 0 at a resolved
! wall, where WALE gives mu_sgs ~ y^3, so the correlation diverges there and is
! clipped to its high-Peclet limit below TVISC_FLOOR. The clip is harmless
! because the subgrid flux it multiplies is itself proportional to mu_sgs.
!------------------------------------------------------------------------------
  real(WP), parameter :: PR_SGS_INF  = 0.85_WP
  real(WP), parameter :: PR_SGS_COEF = 0.7_WP
  real(WP), parameter :: TVISC_FLOOR = 1.0E-12_WP

  private :: Compute_energy_rhs
  private :: Calculate_energy_fractional_step
  private :: add_sgs_enthalpy_flux
  public  :: Calculate_drhodt
  public  :: Update_thermal_properties
  public  :: Solve_energy_eq
  public  :: PR_SGS_INF
contains
!==============================================================================
  !> Compute the density time derivative used by variable-property flow.
  !> - fl (inout): Flow state containing density history and derivative fields.
  !> - dm (in): Domain descriptor.
  !> - isub (in): Substep index.
  subroutine Calculate_drhodt(fl, dm, isub)
    use find_max_min_ave_mod
    use parameters_constant_mod
    use solver_tools_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: isub
    type(t_flow), intent(inout) :: fl
    !real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ), intent(in)  :: dens, densm1
    !real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ), intent(out) :: drhodt
    !real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: div
    !real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: div0
    real(WP) :: maxdrhodt!, d1_bulk, d0_bulk
    logical :: do_projection
    !
    ! Default value
    fl%drhodt = ZERO
    if( .not. dm%is_thermo) return
    if (.not. is_RK_proj(isub)) return
    ! thermal field is half time step ahead of the velocity field
    ! -----*-----$-----*-----$-----*-----$-----*-----$-----*-----$-----*
    !           d_(i-1)     d_i   u_i    d_(i+1)
    ! select case (dm%iTimeScheme)
    !   case (ITIME_EULER) ! 1st order
    !     fl%drhodt = fl%dDens - fl%dDens0
    !     fl%drhodt = fl%drhodt / dm%dt
    !   case (ITIME_AB2) ! 2nd order
    !     fl%drhodt = fl%dDens - fl%dDensm2
    !     fl%drhodt = fl%drhodt / (TWO * dm%dt)
    !   case (ITIME_RK3, ITIME_RK3_CN) ! 3rd order
    !     fl%drhodt = -fl%dDensm2 + SIX * fl%dDens0 - THREE * fl%dDens
    !     fl%drhodt = fl%drhodt / (dm%dt * SIX)
    !   case default
    !     fl%drhodt = fl%dDens - fl%dDens0
    !     fl%drhodt = fl%drhodt / dm%dt
    ! end select
    !------------------------------------------------------------
    ! Compute drho/dt
    !------------------------------------------------------------
    !----------------------------------------------------------
    ! Finite-difference form, per RK sub-step:
    !   drho/dt = (rho^k - rho^{k-1}) / (tAlpha(k) * dt)
    ! dDens0 is re-taken at every sub-step (see Solve_energy_eq), so the
    ! backward difference spans the sub-step interval tAlpha(k)*dt, not the
    ! full step dt. The resulting per-sub-step mass balance
    !   rho^k - rho^{k-1} + tAlpha(k)*dt*div(g^k) = 0
    ! telescopes over k = 1..3 with sum(tAlpha) = 1 to the conservative
    ! full-step balance. tAlpha = 1 for AB2, so that scheme is unaffected.
    !----------------------------------------------------------
    fl%drhodt = fl%dDens - fl%dDens0
    ! Apply time scaling (common to both formulations)
    fl%drhodt = fl%drhodt / (dm%tAlpha(isub) * dm%dt)
    !------------------------------------------------------------
    ! damp drho/dt near in/out b.c.
    !------------------------------------------------------------
    if(is_damping_drhodt) call damping_drhodt(fl%drhodt, dm)

    return
  end subroutine Calculate_drhodt
!==============================================================================
  !> Refresh thermal-property fields from the current thermal state.
  !> - dens (out): Density field.
  !> - visc (out): Dynamic-viscosity field.
  !> - tm (inout): Thermal state containing temperature and property fields.
  !> - dm (in): Domain descriptor.
  !> - opt_tVisc (in): LES eddy viscosity mu_sgs/(rho0 u0 L0), cell centred. Supply
  !>   it together with opt_ren to refresh tm%prSgs; omit both and the subgrid
  !>   Prandtl number is left untouched, which is what the restart path wants.
  !> - opt_ren (in): Current bulk Reynolds number, needed to put opt_tVisc and the
  !>   molecular viscosity on the same scale.
  subroutine Update_thermal_properties(dens, visc, tm, dm, opt_tVisc, opt_ren)
    use boundary_conditions_mod
    use cylindrical_rn_mod
    use operations
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_thermo), intent(inout) :: tm
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ), intent(inout) :: dens, visc
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ), intent(in), optional :: opt_tVisc
    real(WP), intent(in), optional :: opt_ren
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: dh_pcc, d_pcc
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: dh_ypencil, d_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: dh_cpc_ypencil, d_cpc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: dh_zpencil, d_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: dh_ccp_zpencil, d_ccp_zpencil

    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    integer :: i, j, k
    logical  :: is_update_prsgs
    real(WP) :: pe_sgs
    type(t_fluidThermoProperty) :: ftp

    is_update_prsgs = present(opt_tVisc) .and. present(opt_ren) .and. allocated(tm%prSgs)
!------------------------------------------------------------------------------
!   main field
!------------------------------------------------------------------------------
    do k = 1, dm%dccc%xsz(3)
      do j = 1, dm%dccc%xsz(2)
        do i = 1, dm%dccc%xsz(1)
          ftp%rhoh = tm%rhoh(i, j, k)
          ftp%d = dens(i, j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          tm%hEnth(i, j, k) = ftp%h
          tm%tTemp(i, j, k) = ftp%T
          tm%kCond(i, j, k) = ftp%k
          tm%eCond(i, j, k) = ftp%sigma_e
          dens(i, j, k) = ftp%d
          visc(i, j, k) = ftp%m
          if(is_update_prsgs) then
            !------------------------------------------------------------------
            ! Kays' correlation, assembled entirely in code units.
            !   Pr      = Pr0 * ftp%Pr, because ftp%m, ftp%cp and ftp%k are each
            !             normalised by their own reference value, so the stored
            !             ftp%Pr = ftp%m*ftp%cp/ftp%k is Pr/Pr0.
            !   mu_sgs  = rho0*u0*L0 * tVisc and mu = mu0 * ftp%m, so
            !             mu_sgs/mu = Re * tVisc / ftp%m with Re = rho0*u0*L0/mu0.
            !             That is the same scaling eq_momentum2 relies on when it
            !             forms visc = mVisc + ren*tVisc.
            ! Hence Pe_sgs = Pr * mu_sgs/mu = Pr0*ftp%Pr * ren*tVisc/ftp%m.
            !------------------------------------------------------------------
            if(opt_tVisc(i, j, k) < TVISC_FLOOR) then
              tm%prSgs(i, j, k) = PR_SGS_INF
            else
              pe_sgs = fluidparam%ftp0ref%Pr * ftp%Pr * opt_ren * opt_tVisc(i, j, k) / ftp%m
              tm%prSgs(i, j, k) = PR_SGS_INF + PR_SGS_COEF / pe_sgs
            end if
          end if
        end do
      end do
    end do

!------------------------------------------------------------------------------
!  BC - x
!------------------------------------------------------------------------------
  if( dm%ibcx_Tm(1) == IBC_NEUMANN .or. &
      dm%ibcx_Tm(2) == IBC_NEUMANN) then
    call Get_x_midp_C2P_3D(tm%rhoh, dh_pcc, dm, dm%iAccuracy, dm%ibcx_ftp) ! exterpolation, check
    call Get_x_midp_C2P_3D(dens,d_pcc, dm, dm%iAccuracy, dm%ibcx_ftp)
    if(dm%ibcx_Tm(1) == IBC_NEUMANN .and. &
       dm%dpcc%xst(1) == 1) then
      do j = 1, size(dm%fbcx_ftp, 2)
        do k = 1, size(dm%fbcx_ftp, 3)
          ftp%rhoh = dh_pcc(1, j, k)
          ftp%d = d_pcc(1, j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcx_ftp(1, j, k) = ftp
          dm%fbcx_ftp(3, j, k) = ftp
        end do
      end do
    end if
    if(dm%ibcx_Tm(2) == IBC_NEUMANN .and. &
       dm%dpcc%xen(1) == dm%np(1)) then
      do j = 1, size(dm%fbcx_ftp, 2)
        do k = 1, size(dm%fbcx_ftp, 3)
          ftp%rhoh = dh_pcc(dm%np(1), j, k)
          ftp%d = d_pcc(dm%np(1), j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcx_ftp(2, j, k) = ftp
          dm%fbcx_ftp(4, j, k) = ftp
        end do
      end do
    end if

  end if
!------------------------------------------------------------------------------
!  BC - y
!------------------------------------------------------------------------------
  if( dm%ibcy_Tm(1) == IBC_NEUMANN .or. &
      dm%ibcy_Tm(2) == IBC_NEUMANN) then
    ! No fbc is supplied to the C2P interpolations below, matching the x- and
    ! z-blocks above. The wall thermodynamic state under an imposed-qw wall is
    ! unknown and has to be reconstructed from the interior; omitting fbc makes
    ! reduce_bc_to_interp degrade the b.c. to IBC_INTRPL, i.e. a one-sided
    ! extrapolation. Passing fbcy_c4c instead would return the supplied wall
    ! value verbatim, because ibcy_ftp is IBC_DIRICHLET for Neumann-T (see
    ! bc_general.f90), so the write-back below would be an identity and the wall
    ! state would stay frozen at tm%ftp_ini for the whole run.
    call transpose_x_to_y(tm%rhoh, dh_ypencil, dm%dccc)
    call Get_y_midp_C2P_3D(dh_ypencil, dh_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp)
    if(dm%icase == ICASE_PIPE) then
      fbcy_c4c(:,:,:) = dm%fbcy_ftp(:,:,:)%rhoh
      call axis_mirror_fbcy(dh_cpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    end if
    call transpose_x_to_y(dens, d_ypencil, dm%dccc)
    call Get_y_midp_C2P_3D(d_ypencil, d_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp)
    if(dm%icase == ICASE_PIPE) then
      fbcy_c4c(:,:,:) = dm%fbcy_ftp(:,:,:)%d
      call axis_mirror_fbcy(d_cpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    end if

    if(dm%ibcy_Tm(1) == IBC_NEUMANN .and. &
       dm%dcpc%yst(2) == 1) then
      do i = 1, size(dm%fbcy_ftp, 1)
        do k = 1, size(dm%fbcy_ftp, 3)
          ftp%rhoh = dh_cpc_ypencil(i, 1, k)
          ftp%d = d_cpc_ypencil(i, 1, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcy_ftp(i, 1, k) = ftp
          dm%fbcy_ftp(i, 3, k) = ftp
        end do
      end do
    end if
    if(dm%ibcy_Tm(2) == IBC_NEUMANN .and. &
       dm%dcpc%yen(2) == dm%np(2)) then
      do i = 1, size(dm%fbcy_ftp, 1)
        do k = 1, size(dm%fbcy_ftp, 3)
          ftp%rhoh = dh_cpc_ypencil(i, dm%np(2), k)
          ftp%d = d_cpc_ypencil(i, dm%np(2), k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcy_ftp(i, 2, k) = ftp
          dm%fbcy_ftp(i, 4, k) = ftp
        end do
      end do
    end if
  end if
!------------------------------------------------------------------------------
!  BC - z
!------------------------------------------------------------------------------
  if( dm%ibcz_Tm(1) == IBC_NEUMANN .or. &
      dm%ibcz_Tm(2) == IBC_NEUMANN) then
    call transpose_x_to_y(tm%rhoh, dh_ypencil, dm%dccc)
    call transpose_y_to_z(dh_ypencil, dh_zpencil, dm%dccc)
    call Get_z_midp_C2P_3D(dh_zpencil, dh_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp) ! exterpolation, check

    call transpose_x_to_y(dens, d_ypencil, dm%dccc)
    call transpose_y_to_z(d_ypencil, d_zpencil, dm%dccc)
    call Get_z_midp_C2P_3D(d_zpencil, d_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp) ! exterpolation, check

    if(dm%ibcz_Tm(1) == IBC_NEUMANN .and. &
       dm%dccp%zst(3) == 1) then
      do j = 1, size(dm%fbcz_ftp, 2)
        do i = 1, size(dm%fbcz_ftp, 1)
          ftp%rhoh = dh_ccp_zpencil(i, j, 1)
          ftp%d = d_ccp_zpencil(i, j, 1)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcz_ftp(i, j, 1) = ftp
          dm%fbcz_ftp(i, j, 3) = ftp
        end do
      end do
    end if
    if(dm%ibcz_Tm(2) == IBC_NEUMANN .and. &
       dm%dccp%zen(3) == dm%np(3)) then
      do j = 1, size(dm%fbcz_ftp, 2)
        do i = 1, size(dm%fbcz_ftp, 1)
          ftp%rhoh = dh_ccp_zpencil(i, j, dm%np(3))
          ftp%d = d_ccp_zpencil(i, j, dm%np(3))
          call ftp_refresh_thermal_properties_from_DH(ftp)
          dm%fbcz_ftp(i, j, 2) = ftp
          dm%fbcz_ftp(i, j, 4) = ftp
        end do
      end do
    end if
  end if

  return
  end subroutine Update_thermal_properties
!==============================================================================
  subroutine Calculate_energy_fractional_step(rhs0, rhs1, dtmp, dm, isub)
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: rhs0, rhs1
    integer,  intent(in) :: isub

    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    integer :: i, j, k

    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        do i = 1, dtmp%xsz(1)

      ! add explicit terms : convection+viscous rhs
          rhs_explicit_current = rhs1(i, j, k) ! not (*dt)
          rhs_explicit_last    = rhs0(i, j, k) ! not (*dt)
          rhs_total = dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last
          rhs0(i, j, k) = rhs_explicit_current
      ! times the time step
          rhs1(i, j, k) = dm%dt * rhs_total ! * dt
        end do
      end do
    end do

    return
  end subroutine
!==============================================================================
  !> Add the LES subgrid-scale enthalpy flux to the energy right-hand side.
  !>
  !> Term, in dimensional form:
  !>     d/dx_i [ (mu_sgs / Pr_sgs) * dh/dx_i ]
  !>
  !> Non-dimensionalisation (derived here, not copied from the molecular term -
  !> the two carry different coefficients and look deceptively alike):
  !>   x_i = L0 x*,  h = cp0 T0 h*,  t = (L0/u0) t*,  rho = rho0 rho*
  !> The equation is advanced as d(rho* h*)/dt* + d(u*_i rho* h*)/dx*_i = RHS*,
  !> so every RHS term is divided by rho0 u0 cp0 T0 / L0. The subgrid term is
  !>     (mu_sgs cp0 T0 / L0^2) d/dx*_i [ (1/Pr_sgs) dh*/dx*_i ]
  !> and dividing gives the prefactor mu_sgs / (rho0 u0 L0). WALE stores exactly
  !> that group in fl%tVisc - it is the same scaling eq_momentum2 relies on when
  !> it forms visc = mVisc + ren*tVisc, mVisc being mu/mu0 - so in code units
  !>     RHS_sgs = d/dx*_i [ (tVisc / Pr_sgs) dh*/dx*_i ]
  !> with no 1/Re and no 1/Pr. By contrast the molecular term carries k/k0 and
  !> therefore has to restore k0/(rho0 u0 cp0 L0) = 1/(Re*Pr) = tm%rPrRen. Using
  !> rPrRen here, or leaving it out there, is a silent O(Re*Pr) scaling error.
  !>
  !> Wall treatment: the subgrid coefficient is forced to zero on every boundary
  !> face that is not periodic or an interior (pipe-axis) cut, rather than being
  !> extrapolated from the first cell. For wall-resolved LES mu_sgs ~ y^3 as the
  !> wall is approached, so the subgrid heat flux vanishes there and the whole
  !> wall flux is molecular; extrapolating a nonzero coefficient onto the wall
  !> face would invent subgrid transport exactly where the model says there is
  !> none. Because the coefficient multiplies dh/dn face by face, this also makes
  !> the prescribed-q_w and prescribed-T_w wall balances come out unchanged.
  !> This is a wall-resolved assumption; a wall-modelled LES would supply a
  !> nonzero subgrid wall flux from the thermal wall model instead.
  !>
  !> - tVisc (in): WALE eddy viscosity mu_sgs/(rho0 u0 L0), cell centred, x-pencil.
  !> - tm (inout): Thermal state; tm%ene_rhs is accumulated into.
  !> - dm (in): Domain descriptor.
  subroutine add_sgs_enthalpy_flux(tVisc, tm, dm)
    use boundary_conditions_mod
    use cylindrical_rn_mod
    use operations
    use regression_test_mod, only : record_sgs_coef_face, record_sgs_coef_bc, &
                                    record_sgs_enthalpy_flux
    use thermo_info_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in)    :: dm
    type(t_thermo), intent(inout) :: tm
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ), intent(in) :: tVisc

    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil

    ! tVisc/Pr_sgs, cell centred. Only the y pencil needs a dedicated array: in x
    ! and z the coefficient is dead before the generic accc_?pencil scratch of the
    ! same shape is first written, so it rides in that, as elsewhere in this file.
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: csgs_ccc_ypencil
    ! the same, interpolated onto the three face sets
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: csgs_pcc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: csgs_cpc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: csgs_ccp_zpencil

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: hEnth_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: hEnth_ccc_zpencil

    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4

    integer :: ibcx_sgs(2), ibcy_sgs(2), ibcz_sgs(2)
!------------------------------------------------------------------------------
!   the subgrid coefficient and the boundary codes it is interpolated with
!------------------------------------------------------------------------------
    accc_xpencil = tVisc / tm%prSgs   ! the subgrid coefficient, x pencil
    ! The boundary class of the coefficient is read off the nominal VELOCITY bc,
    ! not off ibc*_ftp: temperature is Dirichlet at an isothermal wall and at a
    ! prescribed inflow alike, so it cannot tell a wall from a flow-through
    ! plane. A wall gets IBC_DIRICHLET with fbc = 0 (no subgrid enthalpy flux
    ! through a no-slip wall for wall-resolved LES, since nu_sgs ~ y^3); an
    ! inlet or outlet gets IBC_NEUMANN with fbc = 0, a zero-normal-gradient
    ! closure that retains the incoming subgrid transport instead of switching
    ! it off. See get_ibc_for_sgs_coef_c2p. The fbc arrays below are zero in
    ! both cases, which is why they are simply set to ZERO.
    call get_ibc_for_sgs_coef_c2p(dm%ibcx_nominal, ibcx_sgs)
    call get_ibc_for_sgs_coef_c2p(dm%ibcy_nominal, ibcy_sgs)
    call get_ibc_for_sgs_coef_c2p(dm%ibcz_nominal, ibcz_sgs)

    call transpose_x_to_y (accc_xpencil,      csgs_ccc_ypencil,  dm%dccc)
    call transpose_y_to_z (csgs_ccc_ypencil,  accc_zpencil,      dm%dccc)
    call transpose_x_to_y (tm%hEnth,          hEnth_ccc_ypencil, dm%dccc)
    call transpose_y_to_z (hEnth_ccc_ypencil, hEnth_ccc_zpencil, dm%dccc)
!------------------------------------------------------------------------------
! sgs-x-e, x-pencil : d ( c_pcc * d(h)/dx ) / dx
!------------------------------------------------------------------------------
    !------coefficient on the x faces, zero on a wall------
    fbcx_4cc = ZERO
    ! CD2 for the coefficient: it is the only C2P interpolation here that cannot
    ! undershoot (A = I, both weights +1/2), so a non-negative cell coefficient
    ! stays non-negative on the face. The enthalpy gradient below keeps the
    ! configured accuracy. See the subgrid block in eq_momentum2 for the full
    ! argument and the second-order tradeoff this accepts.
    call Get_x_midp_C2P_3D(accc_xpencil,     csgs_pcc_xpencil, dm, IACCU_CD2, ibcx_sgs, fbcx_4cc)
    !------bulk------
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%h
    call Get_x_1der_C2P_3D(tm%hEnth, apcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp, fbcx_4cc)
    apcc_xpencil = apcc_xpencil * csgs_pcc_xpencil
    call record_sgs_enthalpy_flux(apcc_xpencil)
    !------B.C.------
    ! The flux carries the same boundary class as the molecular one (ebcx_difu):
    ! both are a scalar gradient times a scalar coefficient, so they share the
    ! periodic/interior/wall structure. The wall slot it extracts is identically
    ! zero here, because csgs_pcc_xpencil is zero on that face.
    call extract_dirichlet_fbcx(fbcx_4cc, apcc_xpencil, dm%dpcc)
    !------PDE------
    call Get_x_1der_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, ebcx_difu, fbcx_4cc)
    tm%ene_rhs = tm%ene_rhs + accc_xpencil
!------------------------------------------------------------------------------
! sgs-y-e, y-pencil : d ( r * c_cpc * d(h)/dy ) / dy * 1/r
!------------------------------------------------------------------------------
    !------coefficient on the y faces, zero on a wall------
    fbcy_c4c = ZERO
    ! In a pipe the lower y side is IBC_INTERIOR, so slots 1 and 3 are the two
    ! axis ghosts of the cell-centred input. The coefficient is a scalar, hence
    ! even parity. The wall slots stay at the zero set above.
    if(dm%icase == ICASE_PIPE) &
      call axis_mirror_fbcy(csgs_ccc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dccc, &
                            is_ynode = .false., is_odd = .false.)
    call Get_y_midp_C2P_3D(csgs_ccc_ypencil, csgs_cpc_ypencil, dm, IACCU_CD2, ibcy_sgs, fbcy_c4c)
    if(dm%icase == ICASE_PIPE) &
      call axis_mirror_fbcy(csgs_cpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, &
                            is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    !------bulk------
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%h
    if(dm%icase == ICASE_PIPE) &
      call axis_mirror_fbcy(hEnth_ccc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dccc, &
                            is_ynode = .false., is_odd = .false.)
    call Get_y_1der_C2P_3D(hEnth_ccc_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_c4c)
    ! dh/dr is odd across the axis, as dT/dr is in the molecular block
    if(dm%icase == ICASE_PIPE) &
      call axis_mirror_fbcy(acpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, &
                            is_ynode = .true., is_odd = .true., &
                            axis_mode = AXIS_RECON_M1, assign_axis_to_var = .true., nr = 0, opt_dz = dm%h(3))
    acpc_ypencil = acpc_ypencil * csgs_cpc_ypencil
    call record_sgs_enthalpy_flux(acpc_ypencil)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rp, 1, IPENCIL(2))
    !------B.C.------
    call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
    !------PDE------
    call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, ebcy_difu, fbcy_c4c)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
!------------------------------------------------------------------------------
! sgs-z-e, z-pencil : d ( 1/r * c_ccp * d(h)/dz ) / dz * 1/r
!------------------------------------------------------------------------------
    !------coefficient on the z faces, zero on a wall------
    fbcz_cc4 = ZERO
    call Get_z_midp_C2P_3D(accc_zpencil,     csgs_ccp_zpencil, dm, IACCU_CD2, ibcz_sgs, fbcz_cc4)
    !------bulk------
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%h
    call Get_z_1der_C2P_3D(hEnth_ccc_zpencil, accp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp, fbcz_cc4)
    accp_zpencil = accp_zpencil * csgs_ccp_zpencil
    call record_sgs_enthalpy_flux(accp_zpencil)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    !------B.C.------
    call extract_dirichlet_fbcz(fbcz_cc4, accp_zpencil, dm%dccp)
    !------PDE------
    call Get_z_1der_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, ebcz_difu, fbcz_cc4)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_zpencil, dm%dccc, dm%rci, 1, IPENCIL(3))
!------------------------------------------------------------------------------
! back to x-pencil
!------------------------------------------------------------------------------
    call transpose_z_to_y(accc_zpencil, csgs_ccc_ypencil, dm%dccc)
    accc_ypencil = accc_ypencil + csgs_ccc_ypencil
    call transpose_y_to_x(accc_ypencil, accc_xpencil, dm%dccc)
    tm%ene_rhs = tm%ene_rhs + accc_xpencil
!------------------------------------------------------------------------------
! regression gates on the three enthalpy-flux coefficient face sets. Each one is
! passed in the pencil aligned with its stagger direction, so planes 1 and n are
! the global boundary planes. See the accumulator block in regression_test_mod.
!------------------------------------------------------------------------------
    call record_sgs_coef_face(csgs_pcc_xpencil)
    call record_sgs_coef_face(csgs_cpc_ypencil)
    call record_sgs_coef_face(csgs_ccp_zpencil)
    call record_sgs_coef_bc(csgs_pcc_xpencil, 1, ibcx_sgs)
    call record_sgs_coef_bc(csgs_cpc_ypencil, 2, ibcy_sgs)
    call record_sgs_coef_bc(csgs_ccp_zpencil, 3, ibcz_sgs)

    return
  end subroutine add_sgs_enthalpy_flux
!==============================================================================
  subroutine Compute_energy_rhs(gx, gy, gz, tm, dm, isub, opt_tVisc)
    use boundary_conditions_mod
    use cylindrical_rn_mod
    use operations
    use thermo_info_mod
    use udf_type_mod
    use wrt_debug_field_mod
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(inout) :: tm
    integer,        intent(in) :: isub
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ), intent(in) :: gx
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ), intent(in) :: gy
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ), intent(in) :: gz
    ! LES eddy viscosity; present only for an LES run, see add_sgs_enthalpy_flux
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ), intent(in), optional :: opt_tVisc
    ! local variables
    real(WP), dimension( dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3) ) :: accc_xpencil
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: apcc_xpencil
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: acpc_xpencil
    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: accc_ypencil
    real(WP), dimension( dm%dccp%ysz(1), dm%dccp%ysz(2), dm%dccp%ysz(3) ) :: accp_ypencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: acpc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: accc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil

    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: gz_ccp_zpencil

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: hEnth_pcc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: hEnth_cpc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: hEnth_ccp_zpencil

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: Ttemp_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: Ttemp_ccc_zpencil

    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: kCond_pcc_xpencil
    real(WP), dimension( dm%dcpc%ysz(1), dm%dcpc%ysz(2), dm%dcpc%ysz(3) ) :: kCond_cpc_ypencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: kCond_ccp_zpencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: kCond_ccc_zpencil

    real(WP), dimension( dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3) ) :: ene_rhs_ccc_ypencil
    real(WP), dimension( dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3) ) :: ene_rhs_ccc_zpencil

    real(WP), dimension( 4, dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: fbcx_4cc
    real(WP), dimension( dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3) ) :: fbcy_c4c
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), 4 ) :: fbcz_cc4

    integer  :: n, i, j, k
    integer  :: mbc(1:2, 1:3)
!------------------------------------------------------------------------------
!    h --> h_pcc
!      --> h_ypencil --> h_cpc_ypencil
!                    --> h_zpencil --> h_ccp_zpencil
!------------------------------------------------------------------------------
    fbcx_4cc = MAXP
    fbcy_c4c = MAXP
    fbcz_cc4 = MAXP
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%h
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%h
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%h
    call Get_x_midp_C2P_3D(tm%hEnth, hEnth_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp(:), fbcx_4cc ) ! for d(g_x h_pcc))/dy
    call transpose_x_to_y (tm%hEnth, accc_ypencil, dm%dccc)                     !accc_ypencil = hEnth_ypencil
    call Get_y_midp_C2P_3D(accc_ypencil, hEnth_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp(:), fbcy_c4c)! for d(g_y h_cpc)/dy
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(hEnth_cpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    end if
    call transpose_y_to_z (accc_ypencil, accc_zpencil, dm%dccc) !ccc_zpencil = hEnth_zpencil
    call Get_z_midp_C2P_3D(accc_zpencil, hEnth_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp(:), fbcz_cc4) ! for d(g_z h_ccp)/dz
!------------------------------------------------------------------------------
!    k --> k_pcc
!      --> k_ypencil --> k_cpc_ypencil
!                    --> k_zpencil --> k_ccp_zpencil
!------------------------------------------------------------------------------
    fbcx_4cc = MAXP
    fbcy_c4c = MAXP
    fbcz_cc4 = MAXP
    fbcx_4cc(:, :, :) = dm%fbcx_ftp(:, :, :)%k
    fbcy_c4c(:, :, :) = dm%fbcy_ftp(:, :, :)%k
    fbcz_cc4(:, :, :) = dm%fbcz_ftp(:, :, :)%k
    call Get_x_midp_C2P_3D(tm%kCond, kCond_pcc_xpencil, dm, dm%iAccuracy, dm%ibcx_ftp(:), fbcx_4cc) ! for d(k_pcc * (dT/dx) )/dx
    call transpose_x_to_y (tm%kCond, accc_ypencil, dm%dccc)  ! for k d2(T)/dy^2
    call Get_y_midp_C2P_3D(accc_ypencil,  kCond_cpc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp(:), fbcy_c4c)
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(kCond_cpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .false., &
                            axis_mode = AXIS_RECON_M0, assign_axis_to_var = .true., nr = 0)
    end if
    call transpose_y_to_z (accc_ypencil,  kCond_ccc_zpencil, dm%dccc)
    call Get_z_midp_C2P_3D(kCond_ccc_zpencil, kCond_ccp_zpencil, dm, dm%iAccuracy, dm%ibcz_ftp(:), fbcz_cc4)
!------------------------------------------------------------------------------
!    T --> T_ypencil --> T_zpencil
!------------------------------------------------------------------------------
    call transpose_x_to_y (tm%Ttemp,      Ttemp_ccc_ypencil, dm%dccc)   ! for k d2(T)/dy^2
    call transpose_y_to_z (Ttemp_ccc_ypencil, Ttemp_ccc_zpencil, dm%dccc)   ! for k d2(T)/dz^2
!==============================================================================
! the RHS of energy equation : convection terms
!==============================================================================
    tm%ene_rhs          = ZERO
    ene_rhs_ccc_ypencil = ZERO
    ene_rhs_ccc_zpencil = ZERO
!------------------------------------------------------------------------------
! conv-x-e, x-pencil : d (gx * h_pcc) / dx
!------------------------------------------------------------------------------
    !------bulk------
    apcc_xpencil = - gx * hEnth_pcc_xpencil
    !------b.c.------
    ! Extracted unconditionally: the P2C stencil differentiates apcc_xpencil, so its
    ! boundary data must come from that same array, whatever the velocity b.c. is.
    ! Gating on is_fbcx_velo_required (a *velocity* predicate) left fbcx_4cc = MAXP
    ! whenever no velocity component has a Dirichlet/interior x-b.c., and
    ! buildup_ghost_cells_P then formed fp(0) = 2*MAXP - fi(2).
    call extract_dirichlet_fbcx(fbcx_4cc, apcc_xpencil, dm%dpcc)
    !------PDE------
    call Get_x_1der_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, ebcx_conv, fbcx_4cc)
    tm%ene_rhs = tm%ene_rhs + accc_xpencil

#ifdef DEBUG_STEPS
    write(*,*) 'conx-e', accc_xpencil(4, 1:4, 4)
#endif
!------------------------------------------------------------------------------
! conv-y-e, y-pencil : d (gy * h_cpc) / dy  * (1/r)
!------------------------------------------------------------------------------
    !------bulk------
    call transpose_x_to_y(gy, acpc_ypencil,   dm%dcpc)   ! for d(g_y h)/dy
    acpc_ypencil = - acpc_ypencil * hEnth_cpc_ypencil
    !------b.c.------
    call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
    !------PDE------
    call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, ebcy_conv, fbcy_c4c)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
    ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil

#ifdef DEBUG_STEPS
    write(*,*) 'cony-e', accc_ypencil(4, 1:4, 4)
#endif
!------------------------------------------------------------------------------
! conv-z-e, z-pencil : d (gz * h_ccp) / dz   * (1/r)
!------------------------------------------------------------------------------
    !------bulk------
    call transpose_x_to_y(gz,           accp_ypencil,     dm%dccp)   ! intermediate, accp_ypencil = gz_ypencil
    call transpose_y_to_z(accp_ypencil, gz_ccp_zpencil,   dm%dccp)   ! for d(g_z h)/dz
    accp_zpencil = - gz_ccp_zpencil * hEnth_ccp_zpencil
    ! if(dm%icoordinate == ICYLINDRICAL) &
    ! call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    call extract_dirichlet_fbcz(fbcz_cc4, accp_zpencil, dm%dccp)
    !------PDE------
    call Get_z_1der_P2C_3D( accp_zpencil, accc_zpencil, dm, dm%iAccuracy, ebcz_conv, fbcz_cc4)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_zpencil, dm%dccc, dm%rci, 1, IPENCIL(3))
    ene_rhs_ccc_zpencil = ene_rhs_ccc_zpencil + accc_zpencil

#ifdef DEBUG_STEPS
    write(*,*) 'conz-e', accc_zpencil(4, 1:4, 4)
#endif
!==============================================================================
! the RHS of energy equation : diffusion terms
!==============================================================================
!------------------------------------------------------------------------------
! diff-x-e, d ( k_pcc * d (T) / dx ) dx
!------------------------------------------------------------------------------
    !------bulk------
    call get_fbcx_iTh(dm%ibcx_Tm, dm, fbcx_4cc, tm, opt_k=kCond_pcc_xpencil)
    call Get_x_1der_C2P_3D(tm%tTemp, apcc_xpencil, dm, dm%iAccuracy, dm%ibcx_Tm, fbcx_4cc )
    apcc_xpencil = apcc_xpencil * kCond_pcc_xpencil
    !------B.C.------
    call extract_dirichlet_fbcx(fbcx_4cc, apcc_xpencil, dm%dpcc)
    !------PDE------f
    call Get_x_1der_P2C_3D(apcc_xpencil, accc_xpencil, dm, dm%iAccuracy, ebcx_difu, fbcx_4cc)
    tm%ene_rhs = tm%ene_rhs + accc_xpencil * tm%rPrRen
#ifdef DEBUG_STEPS
    write(*,*) 'difx-e', accc_xpencil(4, 1:4, 4)
#endif
!------------------------------------------------------------------------------
! diff-y-e, d ( r * k_cpc * d (T) / dy ) dy * 1/r
!------------------------------------------------------------------------------
    !------bulk------
    call get_fbcy_iTh(dm%ibcy_Tm, dm, fbcy_c4c, tm, opt_k=kCond_cpc_ypencil)
    ! get_fbcy_iTh only fills the Dirichlet/Neumann wall slots and leaves the rest at
    ! zero, but in a pipe the lower y side is IBC_INTERIOR: slots 1 and 3 are read as
    ! the two axis ghosts of the C2P input, which is cell-centred in y. T is a scalar,
    ! so it mirrors across the axis with even parity. Wall slots 2 and 4 are untouched.
    if(dm%icase == ICASE_PIPE) &
      call axis_mirror_fbcy(Ttemp_ccc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dccc, &
                            is_ynode = .false., is_odd = .false.)
    call Get_y_1der_C2P_3D(tTemp_ccc_ypencil, acpc_ypencil, dm, dm%iAccuracy, dm%ibcy_Tm, fbcy_c4c)
    if(dm%icase == ICASE_PIPE) then
      call axis_mirror_fbcy(acpc_ypencil, IPENCIL(2), fbcy_c4c, dm%knc_sym, dm%dcpc, is_ynode = .true., is_odd = .true., &
                            axis_mode = AXIS_RECON_M1, assign_axis_to_var = .true., nr = 0, opt_dz = dm%h(3))
    end if
    acpc_ypencil = acpc_ypencil * kCond_cpc_ypencil
#ifdef DEBUG_STEPS
    write(*,*) 'diy-dT', acpc_ypencil(4, 1:4, 4)
    write(*,*) 'dify-k', kCond_cpc_ypencil(4, 1:4, 4)
#endif
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(acpc_ypencil, dm%dcpc, dm%rp, 1, IPENCIL(2))
    !------B.C.------
    call extract_dirichlet_fbcy(fbcy_c4c, acpc_ypencil, dm%dcpc, dm, is_reversed = .true.)
    !------PDE------
    call Get_y_1der_P2C_3D(acpc_ypencil, accc_ypencil, dm, dm%iAccuracy, ebcy_difu, fbcy_c4c) ! check, dirichlet, r treatment
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_ypencil, dm%dccc, dm%rci, 1, IPENCIL(2))
    ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil * tm%rPrRen

#ifdef DEBUG_STEPS
    write(*,*) 'dify-e', accc_ypencil(4, 1:4, 4)
#endif
!------------------------------------------------------------------------------
! diff-z-e, d (1/r* k_ccp * d (T) / dz ) / dz * 1/r
!------------------------------------------------------------------------------
    !------bulk------
    call get_fbcz_iTh(dm%ibcz_Tm, dm, fbcz_cc4, tm, opt_k=kCond_ccp_zpencil)
    call Get_z_1der_C2P_3D(tTemp_ccc_zpencil, accp_zpencil, dm, dm%iAccuracy, dm%ibcz_Tm, fbcz_cc4 )
    accp_zpencil = accp_zpencil * kCond_ccp_zpencil
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accp_zpencil, dm%dccp, dm%rci, 1, IPENCIL(3))
    call extract_dirichlet_fbcz(fbcz_cc4, accp_zpencil, dm%dccp)
    !------PDE------
    call Get_z_1der_P2C_3D(accp_zpencil, accc_zpencil, dm, dm%iAccuracy, ebcz_difu, fbcz_cc4)
    if(dm%icoordinate == ICYLINDRICAL) &
    call multiple_cylindrical_rn(accc_zpencil, dm%dccc, dm%rci, 1, IPENCIL(3))
    ene_rhs_ccc_zpencil = ene_rhs_ccc_zpencil + accc_zpencil * tm%rPrRen

#ifdef DEBUG_STEPS
    write(*,*) 'difz-e', accc_zpencil(4, 1:4, 4)
#endif
!==============================================================================
! all convert into x-pencil
!==============================================================================
    call transpose_z_to_y(ene_rhs_ccc_zpencil, accc_ypencil, dm%dccc)
    ene_rhs_ccc_ypencil = ene_rhs_ccc_ypencil + accc_ypencil
    call transpose_y_to_x(ene_rhs_ccc_ypencil, accc_xpencil, dm%dccc)
    tm%ene_rhs = tm%ene_rhs + accc_xpencil
!==============================================================================
! the RHS of energy equation : LES subgrid-scale enthalpy flux
!==============================================================================
    if(present(opt_tVisc)) call add_sgs_enthalpy_flux(opt_tVisc, tm, dm)
!==============================================================================
! time approaching
!==============================================================================
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(tm%tTemp,   dm%dccc, tm%iteration, isub, 'T@bf stepping') ! debug_ww
    call wrt_3d_pt_debug(tm%ene_rhs, dm%dccc, tm%iteration, isub, 'energy_rhs@bf stepping') ! debug_ww
    write(*,*) 'rhs-e', tm%ene_rhs(1, 1:4, 1)
#endif
    call Calculate_energy_fractional_step(tm%ene_rhs0, tm%ene_rhs, dm%dccc, dm, isub)
    return
  end subroutine Compute_energy_rhs

!==============================================================================
!==============================================================================
  !> Advance the energy equation for one substep.
  !> - fl (inout): Flow state coupled to thermal properties.
  !> - tm (inout): Thermal state.
  !> - dm (inout): Domain descriptor.
  !> - isub (in): Substep index.
  subroutine Solve_energy_eq(fl, tm, dm, isub)
    use bc_convective_outlet_mod
    use boundary_conditions_mod
    use convert_primary_conservative_mod
    use find_max_min_ave_mod
    use solver_tools_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    ! arguments
    type(t_domain), intent(inout)    :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP) :: uxdx, rhoh(2)
    integer :: j, k
    real(WP), dimension( dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3) ) :: gx!, ux
    real(WP), dimension( dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3) ) :: gy!, uy
    real(WP), dimension( dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3) ) :: gz!, uz
    !logical :: do_backup_density, do_backup_viscosity
    !
    ! set up flow info based on different time stepping
    gx = fl%gx
    gy = fl%gy
    gz = fl%gz
    ! backup density every RK stage
    fl%dDens0 = fl%dDens
    ! compute b.c. info from convective b.c. if specified.
    call compute_convective_outlet_thermo(tm, dm, isub)
    !
    if(tm%is_rhoh_compensated) then
      call Get_volumetric_average_3d(dm, dm%dccc, tm%rhoh, rhoh(1), SPACE_AVERAGE)
    end if
    ! calculate rhs of energy equation
    if(dm%LES_model /= ILES_NONE) then
      call Compute_energy_rhs(gx, gy, gz, tm, dm, isub, opt_tVisc = fl%tVisc)
    else
      call Compute_energy_rhs(gx, gy, gz, tm, dm, isub)
    end if
    !  update rho * h
    tm%rhoh = tm%rhoh + tm%ene_rhs
    !
    if(tm%is_rhoh_compensated) then
      call Get_volumetric_average_3d(dm, dm%dccc, tm%rhoh, rhoh(2), SPACE_AVERAGE)
      tm%rhoh = tm%rhoh - (rhoh(2) - rhoh(1))
    end if
    !  update other properties from rho * h for domain + b.c.
    !  For an LES run this also refreshes tm%prSgs from the properties just
    !  looked up and the eddy viscosity WALE left behind in the previous momentum
    !  substep. That one-substep lag is inherent to the loop order in chapsim.f90
    !  (energy, then Lorentz force, then momentum, and WALE runs inside momentum)
    !  and is the sequence the model is specified against. On the very first
    !  substep the lagged field is the one initialise_flow_fields seeded from the
    !  initial condition, so the subgrid flux is defined from the start.
    if(dm%LES_model /= ILES_NONE) then
      call Update_thermal_properties(fl%dDens, fl%mVisc, tm, dm, &
                                     opt_tVisc = fl%tVisc, opt_ren = fl%ren)
    else
      call Update_thermal_properties(fl%dDens, fl%mVisc, tm, dm)
    end if
    if (dm%icase == ICASE_PIPE) call update_fbcy_cc_thermo_halo(tm, dm)

  return
  end subroutine

end module eq_energy_mod
