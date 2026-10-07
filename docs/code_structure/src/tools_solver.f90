module solver_tools_mod
  implicit none
  ! procedure
  private
  public  :: Check_cfl_convection
  public  :: Check_cfl_diffusion
  !
  public  :: Update_Re
  public  :: Update_PrGr
  public  :: Calculate_vis_sponge
  public  :: Get_pipe_active_tripping_factor
  public  :: Apply_pipe_active_tripping
  !public  :: Calculate_xz_mean_yprofile
  public  :: Adjust_to_xzmean_zero
  !public  :: Get_volumetric_average_3d ! not used anymore
  public  :: get_fbcx_ftp_4pc
  !
  public :: check_global_mass_balance
  public :: is_RK_proj
  !public :: check_global_energy_balance

  public :: damping_drhodt
contains
!==============================================================================
  !> Return the active-tripping amplitude factor based on solver iteration count.
  function Get_pipe_active_tripping_factor(fl) result(ftrip)
    use math_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none

    type(t_flow), intent(in) :: fl
    real(WP) :: ftrip
    real(WP) :: ramp_start_iter, xi
    integer  :: ntrip

    ftrip = ZERO
    if(.not. fl%is_active_tripping) return

    ntrip = max(1, fl%initReTo)
    ramp_start_iter = HALF * real(ntrip, WP)

    if(fl%iteration <= 0 .or. fl%iteration > ntrip) then
      ftrip = ZERO
    else if(real(fl%iteration, WP) <= ramp_start_iter) then
      ftrip = ONE
    else
      xi = (real(fl%iteration, WP) - ramp_start_iter) / max(real(ntrip, WP) - ramp_start_iter, ONE)
      xi = min(ONE, max(ZERO, xi))
      ftrip = HALF * (ONE + cos_wp(PI * xi))
    end if

    return
  end function Get_pipe_active_tripping_factor
!==============================================================================
  !> Apply the cylindrical active-tripping perturbation to the current velocity field.
  !>
  !> The routine adds a low-order, finite-amplitude disturbance to cylindrical
  !> wall-bounded flow using internal empirical defaults for mode numbers and
  !> amplitude ratios. For annular flow the radial envelope is rescaled so that
  !> it vanishes at both walls, while for pipe flow it reduces to the usual
  !> radius-based shape. It is intended for initial triggering and early-time
  !> maintenance of turbulence, after which the pressure-correction step removes
  !> the divergence introduced by the forcing.
  !>
  !> - fl (inout): Flow state. On entry it contains the current velocity field;
  !>   on exit `qx`, `qy`, and `qz` include the active-tripping perturbation.
  !> - dm (inout): Domain descriptor. Provides mesh/geometry information and is
  !>   passed through boundary/halo updates required after the perturbation.
  !> - amp_scale (in): Scalar multiplier for the tripping strength at the current
  !>   call, typically supplied by the active-tripping schedule.
  !> - relax_time (in): Relaxation time scale used to convert the nominal
  !>   perturbation amplitude into an incremental per-step forcing magnitude.
  subroutine Apply_pipe_active_tripping(fl, dm, amp_scale, relax_time)
    use boundary_conditions_mod
    use math_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none

    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    real(WP),       intent(in)    :: amp_scale
    real(WP),       intent(in)    :: relax_time

    integer :: i, j, k, ii, jj, kk
    real(WP) :: axial_wavenum, theta_phase, x_phase, radial_env, amp_relax
    real(WP) :: amp_cross, amp_axial
    real(WP) :: ur, ux, uth, rr, eta, radial_width
    type(DECOMP_INFO) :: dtmp
    integer, parameter :: m_azi = 3
    integer, parameter :: nk_axial = 1

    if(dm%icoordinate /= ICYLINDRICAL) return
    if(.not. fl%is_active_tripping) return
    if(abs(amp_scale) <= MINP) return

    amp_cross = abs(fl%noiselevel)
    amp_axial = amp_cross * 0.5_WP
    if(amp_cross <= MINP) return
    radial_width = dm%lyt - dm%lyb
    if(radial_width <= MINP) return

    axial_wavenum = real(nk_axial, WP) * TWOPI / dm%lxx
    amp_relax = amp_scale
    if(relax_time > MINP) amp_relax = amp_scale * min(ONE, dm%dt / relax_time)

    dtmp = dm%dpcc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      theta_phase = real(kk - 1, WP) * dm%h(3) + HALF * dm%h(3)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        rr = dm%yc(jj)
        eta = min(ONE, max(ZERO, (rr - dm%lyb) / radial_width))
        radial_env = max(ZERO, eta * (ONE - eta))
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          x_phase = axial_wavenum * (real(ii - 1, WP) * dm%h(1))
          ux = amp_axial * radial_env * cos_wp(real(m_azi, WP) * theta_phase) * cos_wp(x_phase)
          fl%qx(i, j, k) = fl%qx(i, j, k) + amp_relax * ux
        end do
      end do
    end do

    dtmp = dm%dcpc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      theta_phase = real(kk - 1, WP) * dm%h(3) + HALF * dm%h(3)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        rr = dm%yp(jj)
        eta = min(ONE, max(ZERO, (rr - dm%lyb) / radial_width))
        radial_env = max(ZERO, eta * (ONE - eta) * (ONE - eta))
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          x_phase = axial_wavenum * (dm%h(1) * (real(ii - 1, WP) + HALF))
          ur = amp_cross * radial_env * sin_wp(real(m_azi, WP) * theta_phase) * sin_wp(x_phase)
          fl%qy(i, j, k) = fl%qy(i, j, k) + amp_relax * (rr * ur)
        end do
      end do
    end do

    dtmp = dm%dccp
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      theta_phase = real(kk - 1, WP) * dm%h(3)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        rr = dm%yc(jj)
        eta = min(ONE, max(ZERO, (rr - dm%lyb) / radial_width))
        radial_env = max(ZERO, eta * (ONE - eta) * (ONE - eta))
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          x_phase = axial_wavenum * (dm%h(1) * (real(ii - 1, WP) + HALF))
          uth = amp_cross * radial_env * cos_wp(real(m_azi, WP) * theta_phase) * sin_wp(x_phase)
          fl%qz(i, j, k) = fl%qz(i, j, k) + amp_relax * uth
        end do
      end do
    end do

    call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)
    if(dm%icase == ICASE_PIPE) call update_fbcy_cc_flow_halo(fl, dm)

    return
  end subroutine Apply_pipe_active_tripping
!==============================================================================
  !> The main code for initialising flow variables
!> This subroutine is called once in initialise_chapsim.
!>
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
! ==========================================================================================================
  subroutine Update_Re(iter, fl)
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    integer,     intent(in ) :: iter
    type(t_flow),   intent(inout) :: fl
  !------------------------------------------------------------------------------
  !  1/Re
  !------------------------------------------------------------------------------
    if(fl%is_active_tripping) then
      fl%rre = ONE / fl%ren
    else if(iter < fl%initReTo) then
      fl%rre = ONE / fl%reninit
    else
      fl%rre = ONE / fl%ren
    end if

    return
  end subroutine Update_Re
!==============================================================================
  subroutine Update_PrGr(fl, tm)
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm


    real(WP) :: u0, rtmp, gravity_norm

!------------------------------------------------------------------------------
!  1/(Re*Pr)
!------------------------------------------------------------------------------
    tm%rPrRen = fl%rre * fluidparam%ftp0ref%k / fluidparam%ftp0ref%m / fluidparam%ftp0ref%cp
!------------------------------------------------------------------------------
!  gravity force
!------------------------------------------------------------------------------
    u0 = ONE / fl%rre * fluidparam%ftp0ref%m / fluidparam%ftp0ref%d / tm%ref_l0
    tm%phy_time = tm%time * tm%ref_l0 / u0
    rtmp = tm%ref_l0 / u0 / u0 * GRAVITY
    fl%fgravity = ZERO
    gravity_norm = sqrt(sum(fl%igravity(1:NDIM) * fl%igravity(1:NDIM)))
    if(gravity_norm > MINP) then
      fl%fgravity(1:NDIM) = rtmp * fl%igravity(1:NDIM) / gravity_norm
    end if

    if(nrank==0) then
      ! Print once, on the first step this run actually solves. The counter is
      ! incremented before the substeps, so on a continuation that step is
      ! iterfrom + 1, not iterfrom.
      if(fl%iteration == 1 .or. fl%iteration == fl%iterfrom + 1) then
        call Print_debug_mid_msg("The reference (dimensional) are")
        write (*, wrtfmt1e) 'Reynolds Number:', fl%ren
        write (*, wrtfmt1e) 'Prandtl Number:',  fluidparam%ftp0ref%Pr
        write (*, wrtfmt1r) 'Length(s):',       tm%ref_l0
        write (*, wrtfmt1r) 'Velocity(m/s):',   fl%ren * fluidparam%ftp0ref%m / fluidparam%ftp0ref%d / tm%ref_l0
        write (*, wrtfmt3r) 'Gravity unit vector:', fl%igravity(1), fl%igravity(2), fl%igravity(3)
        write (*, wrtfmt1r) 'Gravity coefficient gL0/U0^2:', rtmp
        write (*, wrtfmt3r) 'Nondim gravity vector:', fl%fgravity(1), fl%fgravity(2), fl%fgravity(3)
      end if
    end if

    return
  end subroutine Update_PrGr
!==============================================================================
  subroutine Calculate_vis_sponge(fl, dm)
    use math_mod
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    !
    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl
    !
    real(WP) :: x_start, Ls, vis, coeff, hx
    real(WP) :: x, xi, x_offset
    integer  :: i, nx, n
    logical  :: has_sponge

    if(dm%outlet_sponge_layer(1) <= MINP) return
    ! --- sponge params ---
    Ls = dm%outlet_sponge_layer(1)    ! sponge length
    !
    hx      = dm%h(1)
    x_start = dm%lxx - Ls
    !
    vis = ONE / dm%outlet_sponge_layer(2)
    coeff   = vis / TWO           ! = 1/(2*mu)
    !
    do n = 1, 2
      if (n == 1) then
        !Cell-centre array (ccc): global index nx
        fl%rre_sponge_c = ZERO
        nx       = dm%dccc%xsz(1)
        x_offset = hx / TWO
      else
        !Node  array (pcc) : global index nx
        fl%rre_sponge_p = ZERO
        nx       = dm%dpcc%xsz(1)
        x_offset = ZERO
      end if

      do i = 1, nx
        x = REAL(i - 1, WP) * hx + x_offset
        if (x < x_start) cycle

        xi = (x - x_start) / Ls
        if(n==1) then
          fl%rre_sponge_c(i) = coeff * (ONE - COS_WP(PI * xi))
        else
          fl%rre_sponge_p(i) = coeff * (ONE - COS_WP(PI * xi))
        end if
      end do
    end do

    return
  end subroutine Calculate_vis_sponge
!==============================================================================
!> The main code for initialising flow variables
!>
!> not changing storage position, exclude b.c. values, for example, developing
!> flow.
!> MPI : x-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - none (inout): NA
!==============================================================================
!   subroutine Calculate_xz_mean_yprofile(var, dtmp, n, varxz_work1)
!     use mpi_mod
!     use udf_type_mod
!     use parameters_constant_mod
!     use io_files_mod
!     implicit none
!     type(DECOMP_INFO), intent(in) :: dtmp
!     real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(in)  :: var ! x-pencil default
!     integer,  intent(in)  :: n
!     real(WP), dimension(n), optional, intent(out) :: varxz_work1

!     real(wp) :: varxz( n )
!     integer :: jj, i, j, k
!     integer :: nk, ni!, nk_work, ni_work
!     real(WP) :: varxz_work(n)
!     !------------------------------------------------------------------------------
!     !   Default X-pencil
!     !------------------------------------------------------------------------------
!     varxz = ZERO
!     varxz_work = ZERO
!     do j = 1, dtmp%xsz(2)
!       nk = 0
!       ni = 0
!       jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
!       do k = 1, dtmp%xsz(3)
!         nk = nk + 1
!         do i = 1, dtmp%xsz(1)
!           ni = ni + 1
!           varxz(jj) = varxz(jj) + var(i, j, k) !
!         end do
!       end do
!       varxz(jj) = varxz(jj) / real(nk * ni, wp)
!     end do


!     !call mpi_barrier(MPI_COMM_WORLD, ierror)
!     !call mpi_allreduce(ni, ni_work, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
!     !call mpi_allreduce(nk, nk_work, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
!     call mpi_allreduce(varxz, varxz_work, n, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
!     varxz_work = varxz_work / real(p_col * p_col, wp)
!     if(PRESENT(varxz_work1)) varxz_work1 = varxz_work

! #ifdef DEBUG_STEPS
!     if (nrank == 0) then
!       open(121, file = trim(dir_chkp)//'/check_calculate_xz_mean_yprofile.dat', position="append")
!       do j = 1, dtmp%xsz(2)
!         jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
!         write(121, *) jj, varxz_work(jj)
!       end do
!     end if
! #endif


!     return
!   end subroutine
!==============================================================================
! MPI : x-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
! ==========================================================================================================
  subroutine Adjust_to_xzmean_zero(var, dtmp, n, varxz)
    use io_files_mod
    use mpi_mod
    use udf_type_mod
    implicit none
    type(DECOMP_INFO),  intent(in) :: dtmp
    integer,            intent(in) :: n
    real(WP), dimension(n), intent(in) :: varxz
    real(WP), dimension(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3)), intent(inout) :: var

    integer :: jj, i, j, k

    do j = 1, dtmp%xsz(2)
      jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
      do k = 1, dtmp%xsz(3)
        do i = 1, dtmp%xsz(1)
          var(:, j, :) = var(:, j, :) - varxz(jj)
        end do
      end do
    end do

#ifdef DEBUG_STEPS
    open(121, file = trim(dir_chkp)//'/check_adjust_to_xzmean_zero.dat', position="append")
    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        do i = 1, dtmp%xsz(1)
          write(121, *) k, j, i, var(i, j, k)
        end do
      end do
    end do
    close(121)
#endif

    return
  end subroutine
! ==========================================================================================================
  subroutine Check_cfl_diffusion(fl, dm, opt_tm)
    use mpi_mod
    use parameters_constant_mod
    use print_msg_mod
    use udf_type_mod
    use wtformat_mod

    implicit none

    type(t_flow),   intent(in) :: fl
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in), optional :: opt_tm

    real(WP), parameter :: DIFF_SAFETY = 0.50_WP
    real(WP), parameter :: AXIS_R_FLOOR_COEFF = 0.50_WP

    real(WP) :: cfl_diff_mom, cfl_diff_ene
    real(WP) :: cfl_diff_mom_work, cfl_diff_ene_work
    real(WP) :: dtmax_mom_work, dtmax_ene_work
    real(WP) :: dtmax_diff, dt_suggest_diff

    real(WP) :: rsp(3), rsp_local(3)
    real(WP) :: var(8), var_work(8)
    real(WP) :: rdxyz2, rtmp, dyi
    real(WP) :: r_cc, r_eff, r_floor
    real(WP) :: inv_dx2, inv_dy2, inv_dz2
    real(WP) :: max_x, max_y, max_z
    real(WP) :: max_x_work, max_y_work, max_z_work

    integer :: i, j, k, jj
    integer :: j_lim_local, j_lim_work
    real(WP) :: r_lim_local, r_lim_work

    cfl_diff_mom = ZERO
    cfl_diff_ene = ZERO

    max_x = ZERO
    max_y = ZERO
    max_z = ZERO

    j_lim_local = -1
    r_lim_local = ZERO

    rsp(1) = dm%h2r(1)
    rsp(2) = dm%h2r(2)
    rsp(3) = dm%h2r(3)

    if(dm%is_thermo .and. (.not. present(opt_tm))) &
      call Print_error_msg('Input error for subroutine: Check_cfl_diffusion')

    ! Radius floor only used for cylindrical diagnostic/timestep estimate.
    ! This avoids an unrealistically severe explicit diffusion estimate
    ! from the coordinate singularity at r = 0.
    r_floor = AXIS_R_FLOOR_COEFF * dm%h(2)

    do j = 1, dm%dccc%xsz(2)

      jj = dm%dccc%xst(2) + j - 1

      rsp_local(1) = rsp(1)
      rsp_local(2) = rsp(2)
      rsp_local(3) = rsp(3)

      if(dm%is_stretching(2)) then
        dyi = dm%yMappingcc(jj, 1) / dm%h(2)
        rsp_local(2) = dyi * dyi
      end if

      if(dm%icoordinate == ICYLINDRICAL) then
        r_cc = ONE / dm%rci(jj)
        r_eff = max(r_cc, r_floor)
        rsp_local(3) = dm%h2r(3) / (r_eff * r_eff)
      end if

      inv_dx2 = rsp_local(1)
      inv_dy2 = rsp_local(2)
      inv_dz2 = rsp_local(3)

      rdxyz2 = inv_dx2 + inv_dy2 + inv_dz2

      do k = 1, dm%dccc%xsz(3)
        do i = 1, dm%dccc%xsz(1)

          ! Momentum diffusion
          if(dm%is_thermo) then
            rtmp = rdxyz2 * fl%mVisc(i, j, k)
          else
            rtmp = rdxyz2
          end if

          if(rtmp > cfl_diff_mom) then
            cfl_diff_mom = rtmp
            j_lim_local = jj
            if(dm%icoordinate == ICYLINDRICAL) then
              r_lim_local = ONE / dm%rci(jj)
            else
              r_lim_local = ZERO
            end if
          end if

          ! Directional diagnostic, using same viscosity scaling as momentum
          if(dm%is_thermo) then
            max_x = max(max_x, inv_dx2 * fl%mVisc(i, j, k))
            max_y = max(max_y, inv_dy2 * fl%mVisc(i, j, k))
            max_z = max(max_z, inv_dz2 * fl%mVisc(i, j, k))
          else
            max_x = max(max_x, inv_dx2)
            max_y = max(max_y, inv_dy2)
            max_z = max(max_z, inv_dz2)
          end if

          ! Energy diffusion
          if(dm%is_thermo) then
            rtmp = rdxyz2 * opt_tm%kCond(i, j, k)
            cfl_diff_ene = max(cfl_diff_ene, rtmp)
          end if

        end do
      end do

    end do

    var(1) = cfl_diff_mom
    var(2) = cfl_diff_ene
    var(3) = max_x
    var(4) = max_y
    var(5) = max_z
    var(6) = real(j_lim_local, WP)
    var(7) = r_lim_local
    var(8) = ZERO

    call mpi_allreduce(var, var_work, 5, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)

    cfl_diff_mom_work = var_work(1) * TWO * dm%dt * fl%rre
    dtmax_mom_work    = ONE / (TWO * fl%rre * var_work(1))

    max_x_work = var_work(3)
    max_y_work = var_work(4)
    max_z_work = var_work(5)

    ! j/r location is only approximate here because MPI_MAX is not linked
    ! to the rank that produced cfl_diff_mom. For exact location, use MPI_MAXLOC.
    j_lim_work = nint(var_work(6))
    r_lim_work = var_work(7)

    dtmax_diff = dtmax_mom_work

    if(dm%is_thermo) then
      cfl_diff_ene_work = var_work(2) * TWO * dm%dt * opt_tm%rPrRen
      dtmax_ene_work    = ONE / (TWO * opt_tm%rPrRen * var_work(2))
      dtmax_diff = min(dtmax_mom_work, dtmax_ene_work)
    end if

    dt_suggest_diff = DIFF_SAFETY * dtmax_diff

    if(nrank == 0) then

      write (*, wrtfmt2e) "Momentum diffu. number & max. dt:", &
                          cfl_diff_mom_work, dtmax_mom_work

      if(dm%is_thermo) then
        write (*, wrtfmt2e) "Energy diffu. number & max. dt:", &
                            cfl_diff_ene_work, dtmax_ene_work
      end if

      !write (*, wrtfmt1e) "Suggested dt from explicit diffusion:", dt_suggest_diff
      !write (*, wrtfmt3e) "Momentum directional inv. length^2 max x/y/z:", &
      !                    max_x_work, max_y_work, max_z_work
      !if(dm%icoordinate == ICYLINDRICAL) then
        !write(*,*) "Cylindrical diffusion estimate uses axis radius floor:", r_floor
        !write(*,*) "Approx. limiting j and physical r:", j_lim_work, r_lim_work
      !end if

      if(cfl_diff_mom_work > ONE) then
        call Print_warning_msg("Warning: Momentum diffusion number is larger than 1. Numerical instability could occur.")
        write(*,*) 'Please reduce dt below ', dtmax_mom_work
        write(*,*) 'Recommended dt with safety factor = ', DIFF_SAFETY * dtmax_mom_work
        write(*,*) 'Or consider increasing the mesh size / using implicit diffusion.'
      end if

      if(dm%is_thermo) then
        if(cfl_diff_ene_work > ONE) then
          call Print_warning_msg("Warning: Energy diffusion number is larger than 1. Numerical instability could occur.")
          write(*,*) 'Please reduce dt below ', dtmax_ene_work
          write(*,*) 'Recommended dt with safety factor = ', DIFF_SAFETY * dtmax_ene_work
          write(*,*) 'Or consider increasing the mesh size / using implicit diffusion.'
        end if
      end if

    end if

    return
  end subroutine Check_cfl_diffusion
!==============================================================================
!> to check CFL for convection terms
!> CFL = u^x/dx + v^y/dy + w^z/dz < limit
!> MPI : x-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!>
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
! ==========================================================================================================
  subroutine Check_cfl_convection(u, v, w, dm)
    use decomp_2d
    use find_max_min_ave_mod
    use math_mod, only: abs_wp
    use operations
    use parameters_constant_mod
    use udf_type_mod
    use wtformat_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    real(WP), dimension(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3)), intent(in) :: u
    real(WP), dimension(dm%dcpc%xsz(1), dm%dcpc%xsz(2), dm%dcpc%xsz(3)), intent(in) :: v
    real(WP), dimension(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3)), intent(in) :: w

    real(WP) :: var_xpencil (dm%dccc%xsz(1), &
                             dm%dccc%xsz(2), &
                             dm%dccc%xsz(3))
    real(WP) :: var_ypencil (dm%dccc%ysz(1), &
                             dm%dccc%ysz(2), &
                             dm%dccc%ysz(3))
    real(WP) :: var_zpencil (dm%dccc%zsz(1), &
                             dm%dccc%zsz(2), &
                             dm%dccc%zsz(3))
    real(WP) :: accc_xpencil (dm%dccc%xsz(1), &
                             dm%dccc%xsz(2), &
                             dm%dccc%xsz(3))
    real(WP) :: accc_ypencil (dm%dccc%ysz(1), &
                             dm%dccc%ysz(2), &
                             dm%dccc%ysz(3))
    real(WP) :: accc_zpencil (dm%dccc%zsz(1), &
                             dm%dccc%zsz(2), &
                             dm%dccc%zsz(3))
    real(WP) ::   v_ypencil (dm%dcpc%ysz(1), &
                             dm%dcpc%ysz(2), &
                             dm%dcpc%ysz(3))
    real(WP) ::   w_ypencil (dm%dccp%ysz(1), &
                             dm%dccp%ysz(2), &
                             dm%dccp%ysz(3))
    real(WP) ::   w_zpencil (dm%dccp%zsz(1), &
                             dm%dccp%zsz(2), &
                             dm%dccp%zsz(3))
    real(WP) :: dtmax
    real(wp) :: cfl(2)
    real(wp) :: cfl_target, dt_suggest, dt_min_factor, dt_max_factor
    integer :: j
!------------------------------------------------------------------------------
! Initialisation
!------------------------------------------------------------------------------
    var_xpencil = ZERO
    var_ypencil = ZERO
    var_zpencil = ZERO
    accc_xpencil = ZERO
    accc_ypencil = ZERO
    accc_zpencil = ZERO
!------------------------------------------------------------------------------
! X-pencil : u_ccc / dx
!------------------------------------------------------------------------------
    call Get_x_midp_P2C_3D(u, accc_xpencil, dm, dm%iAccuracy, dm%ibcx_qx, dm%fbcx_qx)
    var_xpencil = abs_wp(accc_xpencil) * dm%h1r(1)
!------------------------------------------------------------------------------
! Y-pencil : v_ccc / dy / r
!------------------------------------------------------------------------------
    call transpose_x_to_y(var_xpencil, var_ypencil, dm%dccc)
    call transpose_x_to_y(v,             v_ypencil, dm%dcpc)
    call Get_y_midp_P2C_3D(v_ypencil, accc_ypencil, dm, dm%iAccuracy, dm%ibcy_qy, dm%fbcy_qy)
    accc_ypencil = abs_wp(accc_ypencil) * dm%h1r(2)
    if(dm%is_stretching(2)) then
      do j = 1, dm%dccc%ysz(2)
        accc_ypencil(:, j, :) = accc_ypencil(:, j, :) * dm%yMappingcc(j, 1)
      end do
    end if
    if(dm%icoordinate == ICYLINDRICAL) then
      do j = 1, dm%dccc%ysz(2)
        accc_ypencil(:, j, :) = accc_ypencil(:, j, :) * dm%rci(j)
      end do
    end if
    var_ypencil = var_ypencil +  accc_ypencil
!------------------------------------------------------------------------------
! Z-pencil : w_ccc / dz / r
!------------------------------------------------------------------------------
    call transpose_y_to_z(var_ypencil, var_zpencil, dm%dccc)
    call transpose_x_to_y(w,             w_ypencil, dm%dccp)
    if(dm%icoordinate == ICYLINDRICAL) then
      do j = 1, dm%dccp%ysz(2)
        w_ypencil(:, j, :) = w_ypencil(:, j, :) * dm%rci(j)
      end do
    end if
    call transpose_y_to_z(w_ypencil,     w_zpencil, dm%dccp)
    call Get_z_midp_P2C_3D(w_zpencil, accc_zpencil, dm, dm%iAccuracy, dm%ibcz_qz, dm%fbcz_qz)
    var_zpencil = var_zpencil + abs_wp(accc_zpencil) * dm%h1r(3)
!------------------------------------------------------------------------------
! Z-pencil : Find the maximum
!------------------------------------------------------------------------------
    var_zpencil = var_zpencil * dm%dt
    call Find_max_min_3d(var_zpencil, opt_calc='MAXI', opt_work=cfl)
    dtmax = dm%dt/cfl(2)
    if(nrank == 0) then
      write (*, wrtfmt2e) "CFL No. & max. dt:", cfl(2), dtmax
    end if

    ! if(cfl(2) > TWO) then
    !   dm%dt = dm%dt / REAL(ceiling(cfl(2)/ 5.0_WP) * 5, WP)
    !   if(nrank == 0) then
    !     call Print_warning_msg("Warning: CFL is larger than 1.")
    !     write(*, wrtfmt1e) 'dt reduced to ', dm%dt
    !   end if
    ! end if

    cfl_target = 1.0_WP
    dt_min_factor = 0.8_WP
    dt_max_factor = 1.05_WP

    if (cfl(2) > cfl_target) then
      dt_suggest = dm%dt * cfl_target / cfl(2)
      dt_suggest = max(dt_suggest, dt_min_factor * dm%dt)
      dt_suggest = min(dt_suggest, dt_max_factor * dm%dt)
      dm%dt = dt_suggest
    endif

    return
  end subroutine
!==============================================================================
!>to calculate:
!>         fo = integral over x and z directions
!> This is based only y-direction stretching.
!> TODO: Here is 2nd order Trapezoid Method. Need to improve! Check!
!------------------------------------------------------------------------------
!> Scope:  mpi    called-freq    xdomain     module
!>         all    needed         specified   pubic
!------------------------------------------------------------------------------
!> MPI :
!>     default x-pencil
!>     working in : y-pencil
!>  (y) ^_____ _____ ______
!>      |_____|_____|______|
!>      |_____|_____|______|__> (z)
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
! ==========================================================================================================
!   subroutine Get_volumetric_average_3d(is_ynp, ibcy, fbcy, dm, dtmp, var, fo_work)
!     use mpi_mod
!     use udf_type_mod
!     use parameters_constant_mod
!     use operations
!     use decomp_2d
!     use wtformat_mod
!     implicit none
!     type(t_domain),  intent(in) :: dm
!     logical,           intent(in) :: is_ynp
!     integer,           intent(in) :: ibcy(2)
!     real(WP),          intent(in) :: fbcy(:, :, :)
!     type(DECOMP_INFO), intent(in) :: dtmp
!     real(WP),          intent(in) :: var(:, :, :)
!     real(WP),          intent(out):: fo_work

!     real(WP), dimension( dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3) )  :: var_ypencil
!     real(WP), allocatable   :: vcp_ypencil(:, :, :)
!     real(WP)   :: vol, fo, vol_work
!     integer :: i, j, k, noy, jp

! ! #ifdef DEBUG_STEPS
! !     if(nrank == 0) then
! !       if(present(str)) then
! !         call Print_debug_inline_msg("Calculating volumeric average of "//trim(str)//" in 3-D ...")
! !       else
! !         call Print_debug_inline_msg("Calculating volumeric average in 3-D ...")
! !       end if
! !     end if
! ! #endif

!     if(.not. dm%is_stretching(2) ) then
!       vol = ZERO
!       fo  = ZERO
!       do k = 1, dtmp%xsz(3)
!         do j = 1, dtmp%xsz(2)
!           do i = 1, dtmp%xsz(1)
!             fo = fo + var(i, j, k)
!             vol = vol + ONE
!           end do
!         end do
!       end do

!     else
!     !------------------------------------------------------------------------------
!     !   transpose to y pencil. Default is x-pencil.
!     !------------------------------------------------------------------------------
!       var_ypencil = ZERO

!       call transpose_x_to_y(var, var_ypencil, dtmp)
!       !------------------------------------------------------------------------------
!       !   In Y-pencil now
!       !------------------------------------------------------------------------------
!       if( is_ynp )then
!         !------------------------------------------------------------------------------
!         !   if variable is stored in y-nodes, extend them to y-cell centres (P2C)
!         !   for example, uy.
!         !------------------------------------------------------------------------------
!         if( dm%is_periodic(2) ) then
!           noy = dtmp%ysz(2)
!         else
!           noy = dtmp%ysz(2) - 1
!         end if

!         allocate( vcp_ypencil(dtmp%ysz(1), noy, dtmp%ysz(3)) )
!         vcp_ypencil = ZERO

!         call Get_y_midp_P2C_3D(var_ypencil, vcp_ypencil, dm, dm%iAccuracy, ibcy, fbcy)

!         fo = ZERO
!         vol = ZERO
!         do k = 1, dtmp%ysz(3)
!           do i = 1, dtmp%ysz(1)
!             do j = 1, noy
!               !------------------------------------------------------------------------------
!               !       j'    j'+1
!               !      _|__.__|_
!               !         j
!               !------------------------------------------------------------------------------
!               jp = j + 1
!               if( dm%is_periodic(2) .and. jp > dtmp%ysz(2)) jp = 1
!               fo = fo + &
!                   ( var_ypencil(i, jp, k) + vcp_ypencil(i, j, k) ) * &
!                   ( dm%yp(j + 1) - dm%yc(j) ) * HALF + &
!                   ( var_ypencil(i, j,     k) + vcp_ypencil(i, j, k) ) * &
!                   ( dm%yc(j    ) - dm%yp(j) ) * HALF
!               vol = vol + ( dm%yp(j + 1) - dm%yp(j) )
!             end do
!           end do
!         end do
!         deallocate(vcp_ypencil)
!       else
!         !------------------------------------------------------------------------------
!         !   if variable is not stored in y-nodes, extends them to y-nodes. C2P
!         !   for example, ux, density, etc.
!         !------------------------------------------------------------------------------
!         if( dm%is_periodic(2) ) then
!           noy = dtmp%ysz(2)
!         else
!           noy = dtmp%ysz(2) + 1
!         end if
!         allocate( vcp_ypencil(dtmp%ysz(1), noy, dtmp%ysz(3)) )
!         vcp_ypencil = ZERO
!         call Get_y_midp_C2P_3D(var_ypencil, vcp_ypencil, dm, dm%iAccuracy, ibcy, fbcy)

!         fo = ZERO
!         vol = ZERO
!         do k = 1, dtmp%ysz(3)
!           do i = 1, dtmp%ysz(1)
!             do j = 1, dtmp%ysz(2)
!               !------------------------------------------------------------------------------
!               !      j'    j'+1
!               !      _|__.__|_
!               !         j
!               !------------------------------------------------------------------------------
!               jp = j + 1
!               if( dm%is_periodic(2) .and. jp > noy) jp = 1
!               ! method 1: 2nd order
!               ! fo = fo + &
!               !     ( vcp_ypencil(i, jp, k) + var_ypencil(i, j, k) ) * &
!               !     ( dm%yp(j + 1) - dm%yc(j) ) * HALF + &
!               !     ( var_ypencil(i, j,     k) + var_ypencil(i, j, k) ) * &
!               !     ( dm%yc(j    ) - dm%yp(j) ) * HALF
!               ! method 2: 1st order, same as CHAPSim1
!               fo = fo + vcp_ypencil(i, j, k)*(dm%yp(j + 1) - dm%yp(j))
!               vol = vol + ( dm%yp(j + 1) - dm%yp(j) )
!             end do
!           end do
!         end do
!         deallocate(vcp_ypencil)
!       end if

!     end if


!     call mpi_barrier(MPI_COMM_WORLD, ierror)
!     call mpi_allreduce( fo,  fo_work, 1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
!     call mpi_allreduce(vol, vol_work, 1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
!     fo_work = fo_work / vol_work

! #ifdef DEBUG_STEPS
!     if(nrank == 0 ) then
!       write (*, wrtfmt1e) " volumetric average :", fo_work
!     end if
! #endif

!     return
!   end subroutine Get_volumetric_average_3d

  !==============================================================================
  !==============================================================================
  subroutine get_fbcx_ftp_4pc(fbcx_ftp_4cc, fbcx_ftp_4pc, dm)
    use operations
    use parameters_constant_mod
    use print_msg_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)), intent(in)  :: fbcx_ftp_4cc
    real(WP), dimension(dm%d4pc%xsz(1), dm%d4pc%xsz(2), dm%d4pc%xsz(3)), intent(out) :: fbcx_ftp_4pc
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_4cc
    real(WP), dimension(dm%d4cc%ysz(1), dm%d4cc%ysz(2), dm%d4cc%ysz(3)) :: a4cc_ypencil
    real(WP), dimension(dm%d4pc%ysz(1), dm%d4pc%ysz(2), dm%d4pc%ysz(3)) :: a4pc_ypencil
    real(WP), dimension(dm%d4pc%xsz(1), dm%d4pc%xsz(2), dm%d4pc%xsz(3)) :: a4pc_xpencil
    integer :: i, j, k!, ibcy(2)
    real(WP) :: fbc


    if(dm%ibcx_ftp(2) == IBC_DIRICHLET) then
      fbcx_4cc(:, :, :) = fbcx_ftp_4cc(:, :, :)
      call transpose_x_to_y(fbcx_4cc, a4cc_ypencil, dm%d4cc)
      do i = 1, dm%d4pc%ysz(1)
        do k = 1, dm%d4pc%ysz(3)
          j = 1
          a4pc_ypencil(i, j, k) = (THREE * a4cc_ypencil(i, j, k) - a4cc_ypencil(i, j+1, k))/TWO
          j= dm%d4pc%ysz(2)
          if(j<=2) call Print_error_msg('get_fbcx_ftp_4pc decomposition error')
          a4pc_ypencil(i, j, k) = (THREE * a4cc_ypencil(i, j-1, k) - a4cc_ypencil(i, j-2, k))/TWO
          do j = 2, dm%d4pc%ysz(2)-1
            a4pc_ypencil(i, j, k) = (a4cc_ypencil(i, j-1, k) + a4cc_ypencil(i, j, k))/TWO
          end do
        end do
      end do
      ! ibcy = IBC_INTRPL
      ! call Get_y_midp_C2P_3D(a4cc_ypencil, a4pc_ypencil, dm, dm%iAccuracy, dm%ibcy_ftp, fbcy_44c)
       call transpose_y_to_x(a4pc_ypencil, a4pc_xpencil, dm%d4pc)
       fbcx_ftp_4pc(:, :, :) = a4pc_xpencil(:, :, :)
    else
      fbcx_ftp_4pc(2, :, :) = MAXP
    end if

    if(dm%ibcx_ftp(1) == IBC_DIRICHLET) then
      fbc = fbcx_ftp_4cc(1, 1, 1)
      fbcx_ftp_4pc(1, :, :) = fbc ! check
    else
      fbcx_ftp_4pc(1, :, :) = MAXP
    end if

    ! write(*,*) '1-', fbcx_ftp_4pc(1, :, :)
    ! write(*,*) '2-', fbcx_ftp_4pc(2, :, :)
    ! write(*,*) '3-', fbcx_ftp_4pc(3, :, :)
    ! write(*,*) '4-', fbcx_ftp_4pc(4, :, :)

    return
  end subroutine

  !==============================================================================
  subroutine check_global_mass_balance(mass_imbalance, drhodt, dm)
    use find_max_min_ave_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    real(WP), dimension(:,:,:), intent(in) :: drhodt
    type(t_domain), intent(in) :: dm
    real(WP), intent(out) :: mass_imbalance(8)
    !
    real(WP), dimension(4, dm%dpcc%xsz(2), dm%dpcc%xsz(3)) :: fbcx
    real(WP), dimension(dm%dcpc%ysz(1), 4, dm%dcpc%ysz(3)) :: fbcy
    real(WP), dimension(dm%dccp%zsz(1), dm%dccp%zsz(2), 4) :: fbcz
    real(WP) :: intg_m, intg_fbcx(2), intg_fbcy(2), intg_fbcz(2)
    !-----------------------------------------------------------------
    ! mass balance = density change + net mass flux through boundaries
    !-----------------------------------------------------------------
    ! density change introduced mass change = integral_volume(drho/dt) unit = kg/m3/s m3 = kg/s
    if(dm%is_thermo) then
      call Get_volumetric_average_3d(dm, dm%dccc, drhodt, intg_m, SPACE_INTEGRAL, 'drhodt')
    else
      intg_m = ZERO
    end if
    !-----------------------------------------------------------------
    ! mass flux through b.c. = integral_surface (mass flux), unit = kg/m3 m/s m2 = kg/s
    !-----------------------------------------------------------------
    ! x-bc
    if(dm%ibcx_qx(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
        fbcx = dm%fbcx_gx
      else
        fbcx = dm%fbcx_qx
      end if
      call Get_area_average_2d_for_fbcx(dm, dm%dpcc, fbcx, intg_fbcx, SPACE_INTEGRAL, 'fbcx')
    else
      intg_fbcx = ZERO
    end if
    ! y-bc
    if(dm%ibcy_qy(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
        fbcy = dm%fbcy_gy
      else
        fbcy = dm%fbcy_qy
      end if
      call Get_area_average_2d_for_fbcy(dm, dm%dcpc, fbcy, intg_fbcy, SPACE_INTEGRAL, 'fbcy', is_rf=.true.)
    else
      intg_fbcy = ZERO
    end if
    ! z-bc
    if(dm%ibcz_qz(1)/=IBC_PERIODIC)then
      if(dm%is_thermo) then
        fbcz = dm%fbcz_gz
      else
        fbcz = dm%fbcz_qz
      end if
      call Get_area_average_2d_for_fbcz(dm, dm%dccp, fbcz, intg_fbcz, SPACE_INTEGRAL, 'fbcz')
    else
      intg_fbcz = ZERO
    end if
    ! mass change rate, kg/s
    mass_imbalance(1:2) = intg_fbcx(1:2)
    mass_imbalance(3:4) = intg_fbcy(1:2)
    mass_imbalance(5:6) = intg_fbcz(1:2)
    mass_imbalance(7)   = intg_m
    !-----------------------------------------------------------------
    ! Global residual of the conservative continuity equation.
    ! Integrating  d(rho)/dt + div(rho u) = 0  over the domain gives
    !   integral_V d(rho)/dt dV + (mdot_out - mdot_in) = 0,
    ! so the residual carries -intg_m against the inflow-minus-outflow
    ! convention used for slots 1..6 (slot 1 = inflow, slot 2 = outflow).
    ! enforce_domain_mass_balance_dyn_fbc drives this to zero via
    ! scale = 1 + R/mdot_out, which then yields the required
    ! mdot_out = mdot_in - intg_m. Getting the sign of intg_m wrong here
    ! leaves the Poisson source incompatible by exactly 2*intg_m, and the
    ! singular all-Neumann system silently discards that component.
    !-----------------------------------------------------------------
    mass_imbalance(8)   = -intg_m + &
                          intg_fbcx(1) - intg_fbcx(2) + &
                          intg_fbcy(1) - intg_fbcy(2) + &
                          intg_fbcz(1) - intg_fbcz(2)
    !
    ! if(nrank==0) then
    !   write(*, '(4X, A, 4ES13.5)') 'mass balance check(-/+x, -/+y):', mass_imbalance(1:4)
    !   write(*, '(4X, A, 4ES13.5)') 'mass balance check(-/+z, m, s):', mass_imbalance(5:8)
    ! end if
    return
  end subroutine

 !==============================================================================
  subroutine damping_drhodt(accc_xpencil, dm)
    use parameters_constant_mod
    use transpose_extended_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(inout) :: accc_xpencil(:, :, :)
    integer :: i, k
    real(WP), dimension(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3)) :: accc_zpencil
    !
    if(.not. dm%is_thermo) return
    if(.not. is_damping_drhodt) return
    !
    if(dm%is_conv_outlet(1)) then
      do i = 1, dm%dccc%xsz(1)
          accc_xpencil(i,:,:) = accc_xpencil(i,:,:) * (ONE - dm%xdamping(i))
      end do
    end if
    !
    if(dm%is_conv_outlet(3)) then
      call transpose_to_z_pencil(accc_xpencil, accc_zpencil, dm%dccc, IPENCIL(1))
      do k = 1, dm%dccc%zsz(3)
        accc_zpencil(:,:,k) = accc_zpencil(:,:,k) * (ONE - dm%zdamping(k) )
      end do
    end if
    return
  end subroutine

!==============================================================================
  pure function is_RK_proj ( isub ) result(d)
    use parameters_constant_mod, only: is_single_RK_projection, ITIME_RK3
    implicit none
    integer, intent(in) :: isub
    logical :: d
    d = (.not. is_single_RK_projection) .or. (isub == ITIME_RK3)
  end function
  !
end module
