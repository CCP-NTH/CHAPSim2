module bc_convective_outlet_mod
  use decomp_2d
  use parameters_constant_mod
  use udf_type_mod
  implicit none

  private :: get_convective_outlet_velocity
  private :: calculate_fbcx_convective_outlet
  private :: calculate_fbcz_convective_outlet
  !public  :: update_flow_from_dyn_fbcx
  private  :: enforce_domain_mass_balance_dyn_fbc
  !private :: enforce_domain_energy_balance_dyn_fbc
  private :: compute_fbcx_convective_outlet_flow
  private :: compute_fbcz_convective_outlet_flow
  public  :: compute_convective_outlet_thermo
  private :: report_outlet_backflow
  public  :: restore_fbcx_outlet_thermo_bc
  public  :: compute_convective_outlet_flow
  public  :: write_outlet_split_state
  private :: read_outlet_split_state
  !----------------------------------------------------------------------------
  ! outlet_mass_correction = split: sum of the stage corrections R over the
  ! current step (accumulating) and over the previous complete step. Only state
  ! of the method; written to 1_data/domain<id>_outlet_split_<iter>.dat with each
  ! checkpoint and read back at a restart (absent file: S = 0 for one step).
  !----------------------------------------------------------------------------
  real(WP), save :: split_rsum_acc = 0.0_WP, split_rsum_prev = 0.0_WP
  logical,  save :: split_acc_complete = .false., split_has_prev = .false., split_init_done = .false.
!------------------------------------------------------------------------------
! The convective-outlet velocity correction is commented out below, along with
! these three declarations. It was written to test whether the convective outlet
! also needs an explicit velocity correction, and was never enabled; nothing in
! src/ has ever called it. Kept rather than deleted because it is the only
! outlet local-divergence fix and the only per-substep
! Check_element_mass_conservation in the code, so it is worth having on hand.
! If it is ever revived, note that its update carries a spurious tAlpha(isub) -
! the divergence it corrects is already a per-substep quantity - and that
! correct_fbcz_convective_outlet_flow is an empty stub.
!------------------------------------------------------------------------------
!  private :: correct_fbcx_convective_outlet_flow
!  private :: correct_fbcz_convective_outlet_flow
!  public  :: correct_convective_outlet_flow


  contains

!==============================================================================
  subroutine get_convective_outlet_velocity(dm, uxdx)
    use math_mod
    use print_msg_mod
    use wtformat_mod
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: uxdx
    ! local variables
    real(WP) :: uxmax, uxmin, uxmax_work, uxmin_work, uintf, dx
    integer :: i, j, k, nsz(3)
    ! conditions
    if(.not. dm%is_conv_outlet(1) .and. &
       .not. dm%is_conv_outlet(3)) return
    if(dm%is_conv_outlet(1) .and. &
       dm%is_conv_outlet(3)) call Print_error_msg("Not supported.")

    uxmax = MINP
    uxmin = MAXP
    !
    if(dm%is_conv_outlet(1)) then
      ! x - convective velocity
      do k = 1, size(dm%fbcx_qx, 3)
        do j = 1, size(dm%fbcx_qx, 2)
          uintf = dm%fbcx_qx(2, j, k)
          uintf = abs_wp(uintf)
          if(uintf > uxmax) uxmax = uintf
          if(uintf < uxmin) uxmin = uintf
        end do
      end do
      dx = dm%h1r(1)
    else if(dm%is_conv_outlet(3)) then
      ! y - convective velocity
      do j = 1, size(dm%fbcz_qz, 2)
        do i = 1, size(dm%fbcz_qz, 1)
          uintf = dm%fbcz_qz(i, j, 2)
          uintf = abs_wp(uintf)
          if(uintf > uxmax) uxmax = uintf
          if(uintf < uxmin) uxmin = uintf
        end do
      end do
      dx = dm%h1r(3)
    else
      call Print_error_msg("Not supported.")
    end if
    ! find the global max/min
    call MPI_ALLREDUCE(uxmax, uxmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    call MPI_ALLREDUCE(uxmin, uxmin_work, 1, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)
    ! calc convective velocity
    uxdx = HALF * (uxmax_work + uxmin_work)
    uxdx = uxdx * dx
    !uxdx = max(uxdx, ZERO) ! in case of negative velocity, set to zero to avoid numerical instability.
#ifdef DEBUG_STEPS
    if(nrank == 0) write(*, '(10X, A, 3ES13.5)') 'convective outlet velocity Max., Min., Ave = ', &
      uxmax_work, uxmin_work, HALF * (uxmax_work + uxmin_work)
#endif

    return
  end subroutine
!==============================================================================
  subroutine calculate_fbcx_convective_outlet(fbcx_var, uxdx, fbc_rhs0, var_xpencil, dm, isub)
    ! all based on x pencil
    ! dphi/dt + ux * dphi/dx = 0
    ! data storage:
    ! qx,  -----|-----||-----|
    !          qx     bc2   bc4
    ! qy,  --x--|--x--||--x--|
    !       qy    qy  bc2 bc4
    !
    ! The gradient is evaluated as (bc4 - var)/L. Because bc4 is set below as
    ! the reflection of var about bc2, bc4 = 2*bc2 - var, the span L is twice
    ! the distance from var to the boundary:
    !   qx (x-p-node): var = qx(np-1) sits 1 dx  from bc2 -> L = 2 dx
    !   qy/qz (x-c-node): var = q(nc) sits dx/2  from bc2 -> L = 1 dx
    ! The caller therefore has to pass uxdx = u_c/dx scaled by dx/L, i.e.
    ! uxdx*HALF for the p-node component and uxdx for the c-node components.
    !
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    real(WP), dimension(:, :, :), intent(inout), contiguous :: fbcx_var
    real(WP), dimension(:, :),    intent(inout), contiguous :: fbc_rhs0
    real(WP), dimension(:, :),    intent(in),    contiguous :: var_xpencil
    real(WP), intent(in) :: uxdx
    integer, intent(in)  :: isub
    ! local variables
    integer :: j, k
    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    !
    if(.not. dm%is_conv_outlet(1)) return
    !
    do k = 1, size(fbcx_var, 3)
      do j = 1, size(fbcx_var, 2)
      ! add explicit terms : convection rhs
        rhs_explicit_current = fbcx_var(4, j, k) - var_xpencil(j, k) ! at cell centre for ux, and bc point for otherse
        rhs_explicit_current = - rhs_explicit_current * uxdx
        rhs_explicit_last    = fbc_rhs0(j, k)
        rhs_total = ( dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last ) * dm%dt
        fbc_rhs0(j, k) = rhs_explicit_current
      ! calculate updated b.c. values
        fbcx_var(2, j, k) = fbcx_var(2, j, k) + rhs_total
        fbcx_var(4, j, k) = TWO * fbcx_var(2, j, k) - var_xpencil(j, k)
      end do
    end do
    return
  end subroutine
!==============================================================================
  subroutine calculate_fbcz_convective_outlet(fbcz_var, uzdz, fbc_rhs0, var2d_zpencil, dm, isub)
    ! all based on z pencil
    ! dphi/dt + ux * dphi/dx = 0
    ! data storage:
    ! qz,  -----|-----||-----|
    !          qz     bc2   bc4
    ! qy,  --x--|--x--||--x--|
    !       qy    qy  bc2 bc4
    !
    ! Same span argument as calculate_fbcx_convective_outlet: bc4 is the
    ! reflection of var about bc2, so the caller passes uzdz*HALF for the
    ! z-p-node component (qz) and uzdz for the z-c-node components (qx, qy).
    !
    implicit none
    ! arguments
    type(t_domain), intent(in) :: dm
    real(WP), dimension(:, :, :), intent(inout), contiguous :: fbcz_var
    real(WP), dimension(:, :),    intent(inout), contiguous :: fbc_rhs0
    real(WP), dimension(:, :),    intent(in),    contiguous :: var2d_zpencil
    real(WP), intent(in) :: uzdz
    integer, intent(in)  :: isub
    ! local variables
    integer :: i, j
    real(WP) :: rhs_explicit_current, rhs_explicit_last, rhs_total
    !
    if(.not. dm%is_conv_outlet(3)) return
    !
    do j = 1, size(fbcz_var, 2)
      do i = 1, size(fbcz_var, 1)
      ! add explicit terms : convection rhs
        rhs_explicit_current = fbcz_var(i, j, 4) - var2d_zpencil(i, j) ! at cell centre for ux, and bc point for otherse
        rhs_explicit_current = - rhs_explicit_current * uzdz
        rhs_explicit_last    = fbc_rhs0(i, j)
        rhs_total = ( dm%tGamma(isub) * rhs_explicit_current + &
                      dm%tZeta (isub) * rhs_explicit_last ) * dm%dt
        fbc_rhs0(i, j) = rhs_explicit_current
      ! calculate updated b.c. values
        fbcz_var(i, j, 2) = fbcz_var(i, j, 2) + rhs_total
        fbcz_var(i, j, 4) = TWO * fbcz_var(i, j, 2) - var2d_zpencil(i, j)
      end do
    end do
    return
  end subroutine
!==============================================================================
  ! subroutine update_dyn_fbcx_from_flow(dm, ux, uy, uz, fbcx1, fbcx2, fbcx3)
  !   use print_msg_mod
  !   implicit none
  !   type(t_domain), intent(in) :: dm
  !   real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in) :: ux
  !   real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in) :: uy
  !   real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in) :: uz
  !   real(WP), dimension(4,              dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (inout) :: fbcx1
  !   real(WP), dimension(4,              dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (inout) :: fbcx2
  !   real(WP), dimension(4,              dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (inout) :: fbcx3

  !   if( .not. dm%is_conv_outlet) return

  !   ! x - pencil
  !   if(dm%is_conv_outlet(1)) then
  !     !fbcx1(2, :, :) = ux(dm%dpcc%xsz(1), :, :)
  !     fbcx1(4, :, :) = fbcx1(2, :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 2) == IBC_CONVECTIVE) then
  !     fbcx2(4, :, :) = TWO * fbcx2(2, :, :) - uy(dm%dcpc%xsz(1), :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 3) == IBC_CONVECTIVE) then
  !     fbcx3(4, :, :) = TWO * fbcx3(2, :, :) - uz(dm%dccp%xsz(1), :, :)
  !   end if

  !   return
  ! end subroutine


!==============================================================================
  ! subroutine update_flow_from_dyn_fbcx(dm, ux, uy, uz, fbcx1, fbcx2, fbcx3)
  !   use udf_type_mod
  !   use parameters_constant_mod
  !   use print_msg_mod
  !   implicit none
  !   type(t_domain), intent(in) :: dm
  !   real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (inout) :: ux
  !   real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (inout) :: uy
  !   real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (inout) :: uz
  !   real(WP), dimension(4,              dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent (in)    :: fbcx1
  !   real(WP), dimension(4,              dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent (in)    :: fbcx2
  !   real(WP), dimension(4,              dm%dccp%xsz(2), dm%dccp%xsz(3)), intent (in)    :: fbcx3

  !   if( .not. dm%is_conv_outlet) return

  !   ! x - pencil
  !   if(dm%is_conv_outlet(1)) then
  !     ux(dm%dpcc%xsz(1), :, :) = fbcx1(2, :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 2) == IBC_CONVECTIVE) then
  !     uy(dm%dcpc%xsz(1), :, :) = TWO * fbcx2(2, :, :) - fbcx2(4, :, :)
  !   end if
  !   if(dm%ibcx_nominal(2, 3) == IBC_CONVECTIVE) then
  !     uz(dm%dccp%xsz(1), :, :) =  TWO * fbcx3(2, :, :) - fbcx3(4, :, :)
  !   end if


  !   return
  ! end subroutine
  !==============================================================================
  subroutine enforce_domain_mass_balance_dyn_fbc(drhodt, dm, isub, iter)
    use bc_dirichlet_mod
    use cylindrical_rn_mod
    use find_max_min_ave_mod
    use solver_tools_mod
    use wtformat_mod
    implicit none
    real(WP), dimension(:,:,:), intent(in) :: drhodt
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: isub, iter
    type(DECOMP_INFO) :: dtmp
    real(WP) :: scale, mass_imbalance(8)
    real(WP) :: rslow, rfast
    REAL(WP) :: rsign(3)
    real(WP) :: area_x(2), dflux
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: ones_fbcx
    !
    ! CASE 2 (inlet/outlet, supercritical) -- ASSUMPTION: net dilatation is
    ! PHYSICAL (mdot_in - mdot_out = d/dt integral(rho) dV). We enforce global
    ! mass balance by scaling the outlet mass-flux BC so that:
    !   mdot_out = mdot_in - integral(drhodt) dV
    ! The Poisson solver handles any remaining discrete zero-mode mismatch
    ! separately; this routine preserves the physical global mass balance.
    ! LIMIT: no P0(t) is introduced; rho = rho(T) on the isobaric table.
    !
    ! only 1 direction could be convective outlet
    if(.not. dm%is_conv_outlet(1) .and. &
       .not. dm%is_conv_outlet(3)) return
    if(dm%is_conv_outlet(2)) call Print_warning_msg('is_conv_outlet=y is not supported')
    ! check mass im-balance before correction
    call check_global_mass_balance(mass_imbalance, drhodt, dm)

    !
    ! vsum(rho) + sum(rhou_xi * Syz) -   sum(rhou_xo * Syz) +
    !             sum(rhou_yi * Sxz) -   sum(rhou_yo * Sxz) +
    !             sum(rhou_zi * Sxy) -   sum(rhou_zo * Sxy) = R
    ! vsum(rho) + sum(rhou_xi * Syz) - A*sum(rhou_xo * Syz) +
    !             sum(rhou_yi * Sxz) - A*sum(rhou_yo * Sxz) +
    !             sum(rhou_zi * Sxy) - A*sum(rhou_zo * Sxy) = 0
    ! A = 1+R/(Foux'+Fouy'+Fouz')
    rsign(:) = ZERO
    if(dm%is_conv_outlet(1)) rsign(1) = ONE
    if(dm%is_conv_outlet(2)) rsign(2) = ONE
    if(dm%is_conv_outlet(3)) rsign(3) = ONE
    scale = ONE + mass_imbalance(8)/ &
            (mass_imbalance(2)*rsign(1) + &
             mass_imbalance(4)*rsign(2) + &
             mass_imbalance(6)*rsign(3))
#ifdef DEBUG_STEPS
    if(nrank == 0) then
      write(*,*) 'global mass imbalance before correction', mass_imbalance(8)
      write(*,'(9ES18.9)') scale, mass_imbalance(1:8)
    end if
#endif
    ! split: R_k = S_k + F_k. S_k = alpha_k * sum_j R_j(previous step) is the slow
    ! part (the previous step's total correction shared by the sub-step lengths
    ! alpha_k dt) and is applied proportionally, g_b <- g_b (1 + S_k/mdot_out);
    ! the stage-periodic remainder F_k = R_k - S_k is applied as a uniform
    ! mass-flux increment F_k/A_out. sum_out A_f g_b(new) = mdot_out + R_k exactly.
    ! Before a complete previous step is known (start, or a restart without the
    ! state file) S_k = 0 and the whole R_k is applied uniformly.
    if(ioutlet_mass_correction == IOUTLET_MASS_SPLIT .and. dm%is_conv_outlet(1)) then
      if(.not. split_init_done) then
        call read_outlet_split_state(dm, iter - 1)
        split_init_done = .true.
      end if
      if(isub == 1) then
        split_has_prev = split_acc_complete
        split_rsum_prev = split_rsum_acc
        split_rsum_acc = ZERO
        split_acc_complete = .false.
      end if
      rslow = ZERO
      if(split_has_prev) rslow = dm%tAlpha(isub) * split_rsum_prev
      rfast = mass_imbalance(8) - rslow
      ones_fbcx = ONE
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, ones_fbcx, area_x, SPACE_INTEGRAL, 'fbcx')
      dflux = rfast / area_x(2)
      if(dm%is_thermo) then
        dm%fbcx_gx(2, :, :) = dm%fbcx_gx(2, :, :) * (ONE + rslow / mass_imbalance(2)) + dflux
        dm%fbcx_qx(2, :, :) = dm%fbcx_gx(2, :, :) / dm%fbcx_ftp(2, :, :)%d
      else
        dm%fbcx_qx(2, :, :) = dm%fbcx_qx(2, :, :) * (ONE + rslow / mass_imbalance(2)) + dflux
      end if
      split_rsum_acc = split_rsum_acc + mass_imbalance(8)
      if(isub == dm%nsubitr) split_acc_complete = .true.
      return
    end if
    ! uniform_flux: add the same mass-flux increment R/A_out to every x-outlet
    ! face. A_out is the outlet-face area integral with the weights of
    ! check_global_mass_balance (dy * r dtheta), so that
    !   sum_out A_f (g_b + R/A_out) = mdot_out + R
    ! closes the stage balance exactly, as the scaling does. Only the profile
    ! of the correction differs: uniform in mass flux instead of proportional
    ! to g_b (rho*u in thermal runs, u otherwise).
    if(ioutlet_mass_correction == IOUTLET_MASS_UNIFORM_FLUX .and. dm%is_conv_outlet(1)) then
      ones_fbcx = ONE
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, ones_fbcx, area_x, SPACE_INTEGRAL, 'fbcx')
      dflux = mass_imbalance(8) / area_x(2)
      if(dm%is_thermo) then
        dm%fbcx_gx(2, :, :) = dm%fbcx_gx(2, :, :) + dflux
        dm%fbcx_qx(2, :, :) = dm%fbcx_gx(2, :, :) / dm%fbcx_ftp(2, :, :)%d
      else
        dm%fbcx_qx(2, :, :) = dm%fbcx_qx(2, :, :) + dflux
      end if
      return
    end if
    ! scale qx/gx convective b.c.
    if(dm%is_conv_outlet(1)) then
      if(dm%is_thermo) then
        dm%fbcx_gx(2, :, :) = dm%fbcx_gx(2, :, :) * scale
        dm%fbcx_qx(2, :, :) = dm%fbcx_gx(2, :, :) / dm%fbcx_ftp(2, :, :)%d
      else
        dm%fbcx_qx(2, :, :) = dm%fbcx_qx(2, :, :) * scale
      end if
    end if
    !
    ! scale qz/gz convective b.c.
    if(dm%is_conv_outlet(3)) then
      if(dm%is_thermo) then
        dm%fbcz_gz(:, :, 2) = dm%fbcz_gz(:, :, 2) * scale
        dm%fbcz_qz(:, :, 2) = dm%fbcz_gz(:, :, 2) / dm%fbcz_ftp(:, :, 2)%d
      else
        dm%fbcz_qz(:, :, 2) = dm%fbcz_qz(:, :, 2) * scale
      end if
    end if
    ! Verify: imbalance after correction should be near zero.
#ifdef DEBUG_STEPS
    call check_global_mass_balance(mass_imbalance, drhodt, dm)
    if(nrank == 0) then
      write(*,*) 'global mass imbalance after correction', mass_imbalance(8)
      write(*,'(9ES18.9)') scale, mass_imbalance(1:8)
    end if
#endif

    return
  end subroutine enforce_domain_mass_balance_dyn_fbc

!==============================================================================
  subroutine compute_fbcx_convective_outlet_flow(fl, dm, isub)
    use bc_dirichlet_mod
    use convert_primary_conservative_mod
    implicit none
    ! arguments
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP), dimension(dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a0cc
    real(WP), dimension(dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a0pc
    real(WP), dimension(dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a0cp
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a4cc
    real(WP), dimension(4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a4pc
    real(WP), dimension(4, dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a4cp
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2)) :: acc0
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2)) :: apc0
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2)) :: acp0
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2), 4) :: acc4
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4) :: apc4
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) :: acp4
    real(WP) :: uxdx
    integer :: i
    ! condition + default x-pencil
    if(.not. dm%is_conv_outlet(1)) return
    ! assign proper variables to calculate convective bc.
    if ( .not. dm%is_thermo) then
      a0cc = fl%qx(dm%dpcc%xsz(1)-1, :, :) ! qx at np-1
      a0pc = fl%qy(dm%dcpc%xsz(1),   :, :) ! qy at nc
      a0cp = fl%qz(dm%dccp%xsz(1),   :, :) ! qz at nc
      a4cc = dm%fbcx_qx
      a4pc = dm%fbcx_qy
      a4cp = dm%fbcx_qz
    else
      a0cc = fl%gx(dm%dpcc%xsz(1)-1, :, :)
      a0pc = fl%gy(dm%dcpc%xsz(1),   :, :)
      a0cp = fl%gz(dm%dccp%xsz(1),   :, :)
      a4cc = dm%fbcx_gx
      a4pc = dm%fbcx_gy
      a4cp = dm%fbcx_gz
    end if
    ! get u_c/dx
    call get_convective_outlet_velocity(dm, uxdx)
    ! update b.c.
    call calculate_fbcx_convective_outlet(a4cc, uxdx*HALF, fl%fbcx_a0cc_rhs0, a0cc, dm, isub)
    call calculate_fbcx_convective_outlet(a4pc, uxdx,     fl%fbcx_a0pc_rhs0, a0pc, dm, isub)
    call calculate_fbcx_convective_outlet(a4cp, uxdx,     fl%fbcx_a0cp_rhs0, a0cp, dm, isub)
    ! fbc data back
    if ( .not. dm%is_thermo) then
      dm%fbcx_qx = a4cc
      dm%fbcx_qy = a4pc
      dm%fbcx_qz = a4cp
      fl%qx(dm%dpcc%xsz(1), :, :) = dm%fbcx_qx(2, :, :)
    else
      dm%fbcx_gx = a4cc
      dm%fbcx_gy = a4pc
      dm%fbcx_gz = a4cp
      fl%gx(dm%dpcc%xsz(1), :, :) = dm%fbcx_gx(2, :, :)
    end if

    return
  end subroutine

!==============================================================================
! Never called - see the note at the top of this module.
!==============================================================================
!  subroutine correct_fbcx_convective_outlet_flow(fl, dm, isub)
!    use bc_dirichlet_mod
!    use continuity_eq_mod
!    use convert_primary_conservative_mod
!    implicit none
!    ! arguments
!    type(t_flow),   intent(inout) :: fl
!    type(t_domain), intent(inout) :: dm
!    integer,        intent(in)    :: isub
!
!    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: div
!    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: apcc
!    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a4cc
!    integer :: npx, j, k
!    ! condition + default x-pencil
!    if(.not. dm%is_conv_outlet(1)) return
!    !
!    div = ZERO
!    call Get_divergence_flow(fl, div, dm)
!    !
!    npx = dm%dpcc%xen(1)
!    if (dm%is_thermo) then
!      div = fl%drhodt + div
!      apcc = fl%gx
!    else
!      apcc = fl%qx
!    end if
!    ! (rho*u)^real = (rho*u)^previsional - mass_error * dx ( second order only for B.C.)
!
!    do k = 1, dm%dpcc%xsz(3)
!      do j = 1, dm%dpcc%xsz(2)
!        apcc(npx, j, k) = apcc(npx, j, k) - dm%tAlpha(isub) * div(npx-1, j, k) * dm%h(1)
!        a4cc(2, j, k) = apcc(npx, j, k)
!        a4cc(4, j, k) = TWO * apcc(npx, j, k) - apcc(npx-1, j, k)
!      end do
!    end do
!    ! back
!    if (dm%is_thermo) then
!      fl%gx(npx, :, :)=apcc(npx, :, :)
!      dm%fbcx_gx(2, :, :) = a4cc(2, :, :)
!      dm%fbcx_gx(4, :, :) = a4cc(4, :, :)
!    else
!      fl%qx(npx, :, :)=apcc(npx, :, :)
!      dm%fbcx_qx(2, :, :) = a4cc(2, :, :)
!      dm%fbcx_qx(4, :, :) = a4cc(4, :, :)
!    end if
!
!    call Check_element_mass_conservation(fl, dm, opt_isub=isub)
!
!    return
!  end subroutine
!==============================================================================
  subroutine compute_fbcz_convective_outlet_flow(fl, dm, isub)
    use bc_dirichlet_mod
    use convert_primary_conservative_mod
    use transpose_extended_mod
    implicit none
    ! arguments
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP), dimension( dm%dpcc%zsz(1), dm%dpcc%zsz(2), dm%dpcc%zsz(3) ) :: apcc_zpencil
    real(WP), dimension( dm%dcpc%zsz(1), dm%dcpc%zsz(2), dm%dcpc%zsz(3) ) :: acpc_zpencil
    real(WP), dimension( dm%dccp%zsz(1), dm%dccp%zsz(2), dm%dccp%zsz(3) ) :: accp_zpencil
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2))    :: acc0
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2))    :: apc0
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2))    :: acp0
    real(WP), dimension(dm%dpcc%zsz(1), dm%dpcc%zsz(2), 4) :: acc4
    real(WP), dimension(dm%dcpc%zsz(1), dm%dcpc%zsz(2), 4) :: apc4
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) :: acp4
    real(WP), dimension(dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a0cc
    real(WP), dimension(dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a0pc
    real(WP), dimension(dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a0cp
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: a4cc
    real(WP), dimension(4, dm%dcpc%xsz(2), dm%dcpc%xsz(3)) :: a4pc
    real(WP), dimension(4, dm%dccp%xsz(2), dm%dccp%xsz(3)) :: a4cp
    real(WP) :: uzdz
    integer :: i
    ! condition
    if(.not. dm%is_conv_outlet(3)) return
    ! assign proper variables to calculate convective bc.
    if ( .not. dm%is_thermo) then
      call transpose_to_z_pencil(fl%qx, apcc_zpencil, dm%dpcc, IPENCIL(1))
      call transpose_to_z_pencil(fl%qy, acpc_zpencil, dm%dcpc, IPENCIL(1))
      call transpose_to_z_pencil(fl%qz, accp_zpencil, dm%dccp, IPENCIL(1))
      apc0 = apcc_zpencil(:, :, dm%dpcc%zsz(3)  )
      acp0 = acpc_zpencil(:, :, dm%dcpc%zsz(3)  )
      acc0 = accp_zpencil(:, :, dm%dccp%zsz(3)-1)
      apc4 = dm%fbcz_qx
      acp4 = dm%fbcz_qy
      acc4 = dm%fbcz_qz
    else
      call transpose_to_z_pencil(fl%gx, apcc_zpencil, dm%dpcc, IPENCIL(1))
      call transpose_to_z_pencil(fl%gy, acpc_zpencil, dm%dcpc, IPENCIL(1))
      call transpose_to_z_pencil(fl%gz, accp_zpencil, dm%dccp, IPENCIL(1))
      apc0 = apcc_zpencil(:, :, dm%dpcc%zsz(3)  )
      acp0 = acpc_zpencil(:, :, dm%dcpc%zsz(3)  )
      acc0 = accp_zpencil(:, :, dm%dccp%zsz(3)-1)
      apc4 = dm%fbcz_gx
      acp4 = dm%fbcz_gy
      acc4 = dm%fbcz_gz
    end if
    ! get u_c/dz
    call get_convective_outlet_velocity(dm, uzdz)
    ! update b.c.
    call calculate_fbcz_convective_outlet(apc4, uzdz,     fl%fbcz_apc0_rhs0, apc0, dm, isub)
    call calculate_fbcz_convective_outlet(acp4, uzdz,     fl%fbcz_acp0_rhs0, acp0, dm, isub)
    call calculate_fbcz_convective_outlet(acc4, uzdz*HALF, fl%fbcz_acc0_rhs0, acc0, dm, isub)
    ! fbc data back
    if ( .not. dm%is_thermo) then
      dm%fbcz_qx = apc4
      dm%fbcz_qy = acp4
      dm%fbcz_qz = acc4
      accp_zpencil(:, :, 1) = dm%fbcz_qz(:, :, 2)
      call transpose_from_z_pencil(accp_zpencil, fl%qz, dm%dccp, IPENCIL(1))
    else
      dm%fbcz_gx = apc4
      dm%fbcz_gy = acp4
      dm%fbcz_gz = acc4
      accp_zpencil(:, :, dm%dccp%zsz(3)) = dm%fbcz_gz(:, :, 2)
      call transpose_from_z_pencil(accp_zpencil, fl%gz, dm%dccp, IPENCIL(1))
    end if
    return
  end subroutine
  !==============================================================================
  ! Never called, and never written - see the note at the top of this module.
  !==============================================================================
!  subroutine correct_fbcz_convective_outlet_flow(fl, dm, isub)
!    use bc_dirichlet_mod
!    use convert_primary_conservative_mod
!    implicit none
!    ! arguments
!    type(t_flow),   intent(inout) :: fl
!    type(t_domain), intent(inout) :: dm
!    integer,        intent(in)    :: isub
!
!    ! To add ...
!
!    return
!  end subroutine
!==============================================================================
  subroutine compute_convective_outlet_flow(fl, dm, isub)
    use bc_dirichlet_mod
    use convert_primary_conservative_mod
    implicit none
    ! arguments
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    !
    if(.not. dm%is_conv_outlet(1) .and. &
      .not. dm%is_conv_outlet(3)) return
    !
    if(dm%is_conv_outlet(1)) call compute_fbcx_convective_outlet_flow(fl, dm, isub)
    if(dm%is_conv_outlet(3)) call compute_fbcz_convective_outlet_flow(fl, dm, isub)
    !
    call enforce_domain_mass_balance_dyn_fbc(fl%drhodt, dm, isub, fl%iteration)
    !
    if ( .not. dm%is_thermo) then
      call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
    else
      call enforce_velo_from_fbc(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcy_gy, dm%fbcz_gz)
      call convert_primary_conservative(dm, fl%dDens, IG2Q, IBND)
    end if
    return
  end subroutine

  !==============================================================================
  ! Never called - see the note at the top of this module.
  !==============================================================================
!  subroutine correct_convective_outlet_flow(fl, dm, isub)
!    use bc_dirichlet_mod
!    use convert_primary_conservative_mod
!    implicit none
!    ! arguments
!    type(t_flow),   intent(inout) :: fl
!    type(t_domain), intent(inout) :: dm
!    integer,        intent(in)    :: isub
!    !
!    if(.not. dm%is_conv_outlet(1) .and. &
!      .not. dm%is_conv_outlet(3)) return
!
!    if(dm%is_conv_outlet(1)) call correct_fbcx_convective_outlet_flow(fl, dm, isub)
!    if(dm%is_conv_outlet(3)) call correct_fbcz_convective_outlet_flow(fl, dm, isub)
!
!    return
!  end subroutine
!==============================================================================
  subroutine update_fbcx_convective_outlet_thermo(tm, dm, isub, opt_dens_hat)
    use thermo_info_mod
    implicit none
    ! arguments
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! interior transported density rho_hat^{k-1} (Bc); absent for lagged_legacy
    real(WP), dimension(:, :, :), intent(in), optional :: opt_dens_hat
    ! local variables
    real(WP) :: uxdx
    integer :: j, k, n
    real(WP), dimension(4, dm%dccc%xsz(2), dm%dccc%xsz(3)) :: a4cc_xpencil
    real(WP), dimension(   dm%dccc%xsz(2), dm%dccc%xsz(3)) :: a0cc_xpencil
    real(WP), dimension(4, dm%dccc%xsz(2), dm%dccc%xsz(3)) :: d4cc_xpencil
    real(WP), dimension(   dm%dccc%xsz(2), dm%dccc%xsz(3)) :: d0cc_xpencil
    ! conditions
    if ( .not. dm%is_thermo) return
    if ( .not. dm%is_conv_outlet(1)) return
    if ( dm%ibcx_nominal(2, 5) /= IBC_CONVECTIVE) return
    ! get u_c/dx
    call get_convective_outlet_velocity(dm, uxdx)
    ! assign proper variables to calculate convective bc.
    a4cc_xpencil = dm%fbcx_ftp(:, :, :)%rhoh
    a0cc_xpencil = tm%rhoh(dm%dccc%xsz(1), :, :)
    ! update b.c.
    call calculate_fbcx_convective_outlet(a4cc_xpencil, uxdx, tm%fbcx_rhoh_rhs0, a0cc_xpencil, dm, isub)
    !call enforce_domain_energy_balance_dyn_fbc(fl, dm) ! todo-check necessary?
    ! update other properties
    dm%fbcx_ftp(:, :, :)%rhoh = a4cc_xpencil
    if(present(opt_dens_hat)) then
      !------------------------------------------------------------------------
      ! Bc outlet. rho*h and rho_hat both obey d(phi)/dt + U_c d(phi)/dx = 0 at
      ! the outlet plane; they are advanced with the same discrete operator, the
      ! same U_c and the same RK3 weights (separate histories), from the interior
      ! rho*h^{k-1} and rho_hat^{k-1}. The face (slot 2) and ghost (slot 4) states
      ! are then recovered with h = (rho h)/rho_hat, rho = rho_EOS(h), exactly as
      ! in the interior (ftp%d carries rho_hat into the recovery). The refresh
      ! returns rho = rho_EOS(h) and rhoh = rho_EOS h; the advected rho*h is put
      ! back so that the next stage starts from H_out = rho_hat_out h_out, like
      ! the interior H (otherwise H_out is reset by (rho_EOS - rho_hat) h).
      !------------------------------------------------------------------------
      ! fbcx_ftp%d is final here (the flow restart, read after the thermal
      ! one, may restore it), so an unset rho_hat_out is initialised now.
      if(.not. tm%is_fbcx_dhat_valid) then
        tm%fbcx_dhat(:, :, :) = dm%fbcx_ftp(:, :, :)%d
        tm%fbcx_dhat_rhs0(:, :) = ZERO
        tm%is_fbcx_dhat_valid = .true.
      end if
      d4cc_xpencil = tm%fbcx_dhat
      d0cc_xpencil = opt_dens_hat(dm%dccc%xsz(1), :, :)
      call calculate_fbcx_convective_outlet(d4cc_xpencil, uxdx, tm%fbcx_dhat_rhs0, d0cc_xpencil, dm, isub)
      !------------------------------------------------------------------------
      ! thermal_outlet_pair = zero_gradient: the face and ghost take the last
      ! interior cell's (rho*h, rho_hat)^{k-1}, so h_b = h(nc) and the outlet
      ! enthalpy flux g_b h_b is the upwind outflow flux. The last-cell balances
      ! H: dH = -dt [g_b h(nc) - g_w h_w]/dx + ...,  rho_hat: d rho_hat = -dt [g_b - g_w]/dx
      ! (through D.g = -rho_t) give rho_hat dh = -dt g_w (h_w - h(nc))/dx + ...: the outflow
      ! carries no face state of its own, and a uniform h stays uniform. The
      ! convective update above is overwritten and its rhs histories are zeroed.
      ! Backflow faces (g_b < 0) take the same last-cell state, i.e. the inflowing
      ! fluid carries h(nc); they are counted and reported (diagnostic).
      !------------------------------------------------------------------------
      if(is_thermal_outlet_zero_gradient) then
        do k = 1, size(a4cc_xpencil, 3)
          do j = 1, size(a4cc_xpencil, 2)
            a4cc_xpencil(2, j, k) = a0cc_xpencil(j, k)
            a4cc_xpencil(4, j, k) = a0cc_xpencil(j, k)
            d4cc_xpencil(2, j, k) = d0cc_xpencil(j, k)
            d4cc_xpencil(4, j, k) = d0cc_xpencil(j, k)
          end do
        end do
        tm%fbcx_rhoh_rhs0(:, :) = ZERO
        tm%fbcx_dhat_rhs0(:, :) = ZERO
        dm%fbcx_ftp(:, :, :)%rhoh = a4cc_xpencil
        if(isub == 1) call report_outlet_backflow(tm, dm)
      end if
      tm%fbcx_dhat = d4cc_xpencil
      do k = 1, size(dm%fbcx_ftp, 3)
        do j = 1, size(dm%fbcx_ftp, 2)
          do n = 2, 4, 2
            dm%fbcx_ftp(n, j, k)%d = tm%fbcx_dhat(n, j, k)
            call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(n, j, k))
            dm%fbcx_ftp(n, j, k)%rhoh = a4cc_xpencil(n, j, k)
          end do
        end do
      end do
    else
    do j = 1, size(dm%fbcx_ftp, 2)
      do k = 1, size(dm%fbcx_ftp, 3)
        call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(2, j, k))
        call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(4, j, k))
      end do
    end do
    end if

    return
  end subroutine

  !==============================================================================
  !> thermal_outlet_pair = zero_gradient: count outlet faces with inflow (g_b < 0)
  !> once per step and report them on rank 0 (first occurrence, then every 1000
  !> steps while backflow persists). Diagnostic only; the face state is unchanged.
  subroutine report_outlet_backflow(tm, dm)
    use mpi_mod
    use wtformat_mod
    implicit none
    type(t_thermo), intent(in) :: tm
    type(t_domain), intent(in) :: dm
    integer :: j, k, nglb
    integer, save :: nreported = 0
    real(WP) :: rloc, rglb   ! face count carried as a real: same buffer type as the module's other reductions
    rloc = ZERO
    do k = 1, size(dm%fbcx_gx, 3)
      do j = 1, size(dm%fbcx_gx, 2)
        if(dm%fbcx_gx(2, j, k) < ZERO) rloc = rloc + ONE
      end do
    end do
    call mpi_allreduce(rloc, rglb, 1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
    nglb = nint(rglb)
    if(nglb > 0 .and. nrank == 0) then
      if(nreported == 0 .or. mod(tm%iteration, 1000) == 0) then
        write(*, '(A, I8, A, I10)') ' thermal outlet (zero_gradient): backflow faces = ', nglb, &
          ' at iteration ', tm%iteration
        nreported = nreported + 1
      end if
    end if
    return
  end subroutine report_outlet_backflow
  !==============================================================================
  !> Bc restart: rebuild the convective x-outlet thermal state (slots 2, 4)
  !> from the restored advected rho*h (fbcx_ftp%rhoh) and rho_hat (fbcx_dhat),
  !> exactly as at the end of update_fbcx_convective_outlet_thermo.
  subroutine restore_fbcx_outlet_thermo_bc(tm, dm)
    use thermo_info_mod
    implicit none
    type(t_thermo), intent(in)    :: tm
    type(t_domain), intent(inout) :: dm
    real(WP) :: rhoh_adv
    integer :: j, k, n

    if ( .not. dm%is_thermo) return
    if ( .not. dm%is_conv_outlet(1)) return
    if ( dm%ibcx_nominal(2, 5) /= IBC_CONVECTIVE) return
    if ( .not. tm%is_fbcx_dhat_valid) return
    do k = 1, size(dm%fbcx_ftp, 3)
      do j = 1, size(dm%fbcx_ftp, 2)
        do n = 2, 4, 2
          rhoh_adv = dm%fbcx_ftp(n, j, k)%rhoh
          dm%fbcx_ftp(n, j, k)%d = tm%fbcx_dhat(n, j, k)
          call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(n, j, k))
          dm%fbcx_ftp(n, j, k)%rhoh = rhoh_adv
        end do
      end do
    end do

    return
  end subroutine restore_fbcx_outlet_thermo_bc
  !==============================================================================
  subroutine update_fbcz_convective_outlet_thermo(tm, dm, isub)
    use thermo_info_mod
    use transpose_extended_mod
    implicit none
    ! arguments
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    ! local variables
    real(WP) :: uzdz
    integer :: i, j
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), 4)              :: acc4_zpencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: accc_zpencil
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2))                 :: acc0_zpencil
    ! condition
    if ( .not. dm%is_thermo) return
    if ( .not. dm%is_conv_outlet(3)) return
    if ( dm%ibcz_nominal(2, 5) /= IBC_CONVECTIVE) return
    ! get u_c/dz or u_c/d_theta
    call get_convective_outlet_velocity(dm, uzdz)
    ! assign proper variables to calculate convective bc.
    call transpose_to_z_pencil(tm%rhoh, accc_zpencil, dm%dccc, IPENCIL(1))
    acc0_zpencil = accc_zpencil(:, :, dm%dccc%zsz(3))
    acc4_zpencil = dm%fbcz_ftp(:, :, :)%rhoh
    ! update b.c.
    call calculate_fbcz_convective_outlet(acc4_zpencil, uzdz, tm%fbcz_rhoh_rhs0, acc0_zpencil, dm, isub)
    !!call enforce_domain_energy_balance_dyn_fbc(fl, dm) ! todo-check necessary?
    ! update other properties
    dm%fbcz_ftp(:, :, :)%rhoh = acc4_zpencil
    do j = 1, size(dm%fbcz_ftp, 2)
      do i = 1, size(dm%fbcz_ftp, 1)
        call ftp_refresh_thermal_properties_from_DH(dm%fbcz_ftp(i, j, 2))
        call ftp_refresh_thermal_properties_from_DH(dm%fbcz_ftp(i, j, 4))
      end do
    end do

    return
  end subroutine

  !==============================================================================
  subroutine compute_convective_outlet_thermo(tm, dm, isub, opt_dens_hat)
    use udf_type_mod
    implicit none
    ! arguments
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(inout) :: dm
    integer,        intent(in)    :: isub
    real(WP), dimension(:, :, :), intent(in), optional :: opt_dens_hat
    !
    if(.not. dm%is_conv_outlet(1) .and. &
       .not. dm%is_conv_outlet(3)) return
    !
    if (dm%is_conv_outlet(1)) then
      if(present(opt_dens_hat)) then
        call update_fbcx_convective_outlet_thermo(tm, dm, isub, opt_dens_hat)
      else
        call update_fbcx_convective_outlet_thermo(tm, dm, isub)
      end if
    end if
    if (dm%is_conv_outlet(3)) then
      call update_fbcz_convective_outlet_thermo(tm, dm, isub)
    end if

    return
  end subroutine


  !==============================================================================
  !> outlet_mass_correction = split: write the state (the summed stage corrections of
  !> the step just completed) next to the flow checkpoint of iteration iter.
  subroutine write_outlet_split_state(dm, iter)
    use io_files_mod, only : dir_data
    use typeconvert_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer :: u
    if(ioutlet_mass_correction /= IOUTLET_MASS_SPLIT) return
    if(nrank /= 0) return
    open(newunit = u, file = trim(dir_data)//'/domain'//trim(int2str(dm%idom))//'_outlet_split_'// &
         trim(int2str(iter))//'.dat', status = 'replace', action = 'write')
    write(u, '(I12,ES26.17E3,L3)') iter, split_rsum_acc, split_acc_complete
    close(u)
  end subroutine write_outlet_split_state
  !==============================================================================
  !> outlet_mass_correction = split: read the state of iteration iter (restart). If the
  !> file is absent the previous-step correction is unknown and S = 0 for one step.
  subroutine read_outlet_split_state(dm, iter)
    use io_files_mod, only : dir_data
    use print_msg_mod
    use typeconvert_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    character(len=256) :: fname
    logical :: exists
    integer :: u, it_file, ios
    real(WP) :: rsum
    logical :: complete
    if(iter <= 0) return
    fname = trim(dir_data)//'/domain'//trim(int2str(dm%idom))//'_outlet_split_'//trim(int2str(iter))//'.dat'
    inquire(file = trim(fname), exist = exists)
    if(.not. exists) then
      if(nrank == 0) call Print_warning_msg('outlet_mass_correction = split: no state file '//trim(fname)// &
        '; the first step applies the whole correction uniformly.')
      return
    end if
    open(newunit = u, file = trim(fname), status = 'old', action = 'read')
    read(u, *, iostat = ios) it_file, rsum, complete
    close(u)
    if(ios /= 0 .or. it_file /= iter) call Print_error_msg('outlet_mass_correction = split: unreadable state file '// &
      trim(fname))
    split_rsum_acc = rsum
    split_acc_complete = complete
  end subroutine read_outlet_split_state

end module
