!------------------------------------------------------------------------------
!                      CHAPSim version 2.2.0
!                      --------------------------
! This file is part of CHAPSim, a general-purpose CFD tool.
!
! This program is free software; you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation; either version 3 of the License, or (at your option) any later
! version.
!
! This program is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
! details.
!
! You should have received a copy of the GNU General Public License along with
! this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
! Street, Fifth Floor, Boston, MA 02110-1301, USA.
!------------------------------------------------------------------------------
!> Flow and thermal field allocation and initialisation.
!>
!> This module allocates solver state arrays and constructs initial conditions
!> from restart files, random perturbations, Poiseuille profiles, inlet data,
!> prescribed constants, and Taylor-Green vortex functions.
module flow_thermo_initialiasation
  use print_msg_mod
  use solver_tools_mod
  use vars_df_mod
  implicit none

  public  :: Allocate_flow_variables
  public  :: Allocate_thermo_variables
  private :: Generate_poiseuille_flow_profile
  private :: Generate_random_field

  private :: initialise_poiseuille_flow
  private :: initialise_flow_from_given_values
  private :: initialise_solid_body_rotation
  private :: initialise_vortexgreen_2dflow
  private :: initialise_vortexgreen_3dflow
  private  :: initialise_vortexgreen_3dflow_thermo

  public  :: Validate_TGV2D_error
  public  :: initialise_flow_fields
  public  :: initialise_thermo_fields


contains
!==============================================================================
!> Allocate flow and thermal variables.
!------------------------------------------------------------------------------
!> Scope:  mpi    called-freq    xdomain     module
!>         all    once           specified   private
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - none (in): NA
!> - none (out): NA
!==============================================================================
  !> Allocate flow-variable arrays for the current domain decomposition.
  !> - fl (inout): Flow state receiving allocated arrays.
  !> - dm (in): Domain descriptor.
  subroutine Allocate_flow_variables (fl, dm)
    use mpi_mod
    use parameters_constant_mod
    implicit none

    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl

    if(nrank == 0) call Print_debug_start_msg("Allocating flow variables ...")
    !------------------------------------------------------------------------------
    ! default : x pencil.
    ! varaible index is LOCAL. means 1:xsize(1)
    !------------------------------------------------------------------------------
    call alloc_x(fl%qx,      dm%dpcc) ; fl%qx = ZERO
    call alloc_x(fl%qy,      dm%dcpc) ; fl%qy = ZERO
    call alloc_x(fl%qz,      dm%dccp) ; fl%qz = ZERO

    call alloc_x(fl%pres,    dm%dccc) ; fl%pres = ZERO
    call alloc_x(fl%pcor,    dm%dccc) ; fl%pcor = ZERO
    call alloc_z(fl%pcor_zpencil_ggg,    dm%dccc, .true.) ; fl%pcor_zpencil_ggg = ZERO

    call alloc_x(fl%mx_rhs,  dm%dpcc) ; fl%mx_rhs = ZERO
    call alloc_x(fl%my_rhs,  dm%dcpc) ; fl%my_rhs = ZERO
    call alloc_x(fl%mz_rhs,  dm%dccp) ; fl%mz_rhs = ZERO

    call alloc_x(fl%mx_rhs0, dm%dpcc) ; fl%mx_rhs0 = ZERO
    call alloc_x(fl%my_rhs0, dm%dcpc) ; fl%my_rhs0 = ZERO
    call alloc_x(fl%mz_rhs0, dm%dccp) ; fl%mz_rhs0 = ZERO
    call alloc_x(fl%drhodt,  dm%dccc) ; fl%drhodt  = ZERO
    fl%mcon = ZERO
    fl%mcon_projected = ZERO
    fl%tt_mass_change = ZERO
    fl%total_mass = ZERO
    fl%total_mass_reference = ZERO
    fl%total_mass_drift = ZERO
    fl%tt_kinetic_energy = ZERO
    fl%physical_poisson_compatibility_defect = ZERO
    fl%uniform_poisson_source_correction = ZERO
    fl%poisson_projected_source_amplitude = ZERO
    fl%poisson_zero_mode_rhs_projection = ZERO
    fl%max_strain_rate_mag2_init = ZERO
    if(dm%LES_model /= ILES_NONE) then
      call alloc_x(fl%tVisc, dm%dccc) ; fl%tVisc = ZERO
    end if

    if(dm%is_conv_outlet(1)) then
      allocate (fl%fbcx_a0cc_rhs0(dm%dpcc%xsz(2), dm%dpcc%xsz(3))); fl%fbcx_a0cc_rhs0 = ZERO
      allocate (fl%fbcx_a0pc_rhs0(dm%dcpc%xsz(2), dm%dcpc%xsz(3))); fl%fbcx_a0pc_rhs0 = ZERO
      allocate (fl%fbcx_a0cp_rhs0(dm%dccp%xsz(2), dm%dccp%xsz(3))); fl%fbcx_a0cp_rhs0 = ZERO
    end if
    if(dm%is_conv_outlet(3)) then
      allocate (fl%fbcz_apc0_rhs0(dm%dpcc%zsz(1), dm%dpcc%zsz(2))); fl%fbcz_apc0_rhs0 = ZERO
      allocate (fl%fbcz_acp0_rhs0(dm%dcpc%zsz(1), dm%dcpc%zsz(2))); fl%fbcz_acp0_rhs0 = ZERO
      allocate (fl%fbcz_acc0_rhs0(dm%dccp%zsz(1), dm%dccp%zsz(2))); fl%fbcz_acc0_rhs0 = ZERO
    end if

    if(dm%is_thermo) then
      call alloc_x(fl%gx,      dm%dpcc) ; fl%gx = ZERO
      call alloc_x(fl%gy,      dm%dcpc) ; fl%gy = ZERO
      call alloc_x(fl%gz,      dm%dccp) ; fl%gz = ZERO
      call alloc_x(fl%dDens,   dm%dccc) ; fl%dDens = ONE
      call alloc_x(fl%mVisc,   dm%dccc) ; fl%mVisc = ONE
      call alloc_x(fl%dDens0, dm%dccc)  ; fl%dDens0 = ONE
    end if
    if(dm%outlet_sponge_layer(1) > MINP) then
      allocate (fl%rre_sponge_c(dm%dccc%xsz(1))); fl%rre_sponge_c = ZERO
      allocate (fl%rre_sponge_p(dm%dpcc%xsz(1))); fl%rre_sponge_p = ZERO
      call Calculate_vis_sponge(fl, dm)
    end if

    if(nrank == 0) call Print_debug_end_msg()
    return

  end subroutine Allocate_flow_variables
  !==============================================================================
  !> Allocate thermal-variable arrays for the current domain decomposition.
  !> - tm (inout): Thermal state receiving allocated arrays.
  !> - dm (in): Domain descriptor.
  subroutine Allocate_thermo_variables (tm, dm)
    use eq_energy_mod, only : PR_SGS_INF
    use mpi_mod
    use parameters_constant_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none

    type(t_domain), intent(in)    :: dm
    type(t_thermo), intent(inout) :: tm

    if(.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_start_msg("Allocating thermal variables ...")
    !------------------------------------------------------------------------------
    ! default : x pencil.
    ! varaible index is LOCAL. means 1:xsize(1)
    !------------------------------------------------------------------------------
    call alloc_x(tm%rhoh,     dm%dccc) ; tm%rhoh    = ZERO
    call alloc_x(tm%hEnth,    dm%dccc) ; tm%hEnth = ZERO
    call alloc_x(tm%kCond,    dm%dccc) ; tm%kCond = ONE
    call alloc_x(tm%eCond,    dm%dccc) ; tm%eCond = ONE
    call alloc_x(tm%tTemp,    dm%dccc) ; tm%tTemp = ONE
    call alloc_x(tm%ene_rhs,  dm%dccc) ; tm%ene_rhs = ZERO
    call alloc_x(tm%ene_rhs0, dm%dccc) ; tm%ene_rhs0 = ZERO
    ! Only a thermal LES run carries a subgrid Prandtl number. Seeded at the
    ! high-Peclet limit of Kays' correlation, which is also the value the
    ! correlation is clipped to wherever mu_sgs is negligible.
    if(dm%LES_model /= ILES_NONE) then
      call alloc_x(tm%prSgs,  dm%dccc) ; tm%prSgs = PR_SGS_INF
    end if

    if(dm%is_conv_outlet(1)) then
      allocate (tm%fbcx_rhoh_rhs0(dm%dccc%xsz(2), dm%dccc%xsz(3))); tm%fbcx_rhoh_rhs0 = ZERO
    end if
    if(dm%is_conv_outlet(3)) then
      allocate (tm%fbcz_rhoh_rhs0(dm%dccc%zsz(1), dm%dccc%xsz(2))); tm%fbcz_rhoh_rhs0 = ZERO
    end if

    if(nrank == 0) call Print_debug_end_msg()
    return

  end subroutine Allocate_thermo_variables
  !==============================================================================
  !> Generate a flow profile for Poiseuille flow in channel or pipe.
  !------------------------------------------------------------------------------
  !> Scope:  mpi    called-freq    xdomain     module
  !>         all    once           specified   private
  !------------------------------------------------------------------------------
  ! Arguments
  !------------------------------------------------------------------------------
  !  mode           name          role
  !------------------------------------------------------------------------------
  ! !==============================================================================
  !> Generate a random perturbation field for flow initialisation.
  !> - fl (inout): Flow state receiving random perturbations.
  !> - dm (in): Domain descriptor.
  subroutine Generate_random_field(fl, dm)
    use boundary_conditions_mod
    use math_mod
    use mpi_mod
    use parameters_constant_mod
    use random_number_generation_mod
    !use flatten_index_mod
    !use visualisation_field_mod
    use find_max_min_ave_mod
    use wrt_debug_field_mod
    use wtformat_mod
    implicit none
    type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl

    integer :: i, j, k! local id
    integer :: ii, jj, kk ! global id
    integer :: n, nsz
    real(WP) :: rd, lownoise, rnd
    real(WP) :: envelope
    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_inline_msg("Generating random field ...")
    !------------------------------------------------------------------------------
    !   Initialisation in x pencil
    !------------------------------------------------------------------------------
    fl%pres(:, :, :) = ZERO
    fl%pcor(:, :, :) = ZERO
    fl%qx(:, :, :) = ZERO
    fl%qy(:, :, :) = ZERO
    fl%qz(:, :, :) = ZERO
    nsz = dm%np(1) * dm%np(2) * dm%np(3)

    do n = 1, NDIM

      if(n == 1) then
        dtmp = dm%dpcc
      else if(n == 2) then
        dtmp = dm%dcpc
      else if(n == 3) then
        dtmp = dm%dccp
      else
      end if

!     random field from 0 to 1
      do k = 1, dtmp%xsz(3)
        kk = dtmp%xst(3) + k - 1
        do j = 1, dtmp%xsz(2)
          jj = dtmp%xst(2) + j - 1
          lownoise = fl%noiselevel
          do i = 1, dtmp%xsz(1)
            ii = dtmp%xst(1) + i - 1
            ! Method 1: using fortran random number generator
            !call generate_random11_fortran (ii, jj, kk, n, dtmp%xsz(1), dtmp%ysz(2), r)

            ! Method 2: Stateless index hash for GPU: avoids seed-neighbor correlation striping.
            !call generate_random11_stateless_hash(ii, jj, kk, n, rd)

            ! Method 3: Integer-only stateless hash (CPU/GPU consistent), then whiten via LCG.
            ! call generate_random11_LCG_hash(ii, jj, kk, n, rd)

            ! Method 4: 64-bit XOR / Mix Hash (Stateless, CPU/GPU consistent)
            call generate_random11_mixhash(ii, jj, kk, n, rd)

            if(n == 2) then
              envelope = get_random_field_envelope(dm, dm%yp(jj))
            else
              envelope = get_random_field_envelope(dm, dm%yc(jj))
            end if

            if(n == 1) fl%qx(i, j, k) = lownoise * envelope * rd
            if(n == 2) fl%qy(i, j, k) = lownoise * envelope * rd * dm%rp(jj)
            if(n == 3) fl%qz(i, j, k) = lownoise * envelope * rd
          end do
        end do
      end do

    end do

    !     for dirichelt, the perturbation velocity should be zero.
    call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcy_qy, dm%fbcz_qz)

    if(nrank == 0) call Print_debug_inline_msg("Max/min velocity for generated random velocities:")
    call Find_max_min_3d(fl%qx, opt_name="qx")
    call Find_max_min_3d(fl%qy, opt_name="qy")
    call Find_max_min_3d(fl%qz, opt_name="qz")
! to validate the random number generated is MPI processor independent.
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%qx, dm%dpcc,   fl%iteration, 0, 'qx@af radm') ! debug_ww
    call wrt_3d_pt_debug(fl%qy, dm%dcpc,   fl%iteration, 0, 'qy@af radm') ! debug_ww
    call wrt_3d_pt_debug(fl%qz, dm%dccp,   fl%iteration, 0, 'qz@af radm') ! debug_ww
    call wrt_3d_pt_debug(fl%pres, dm%dccc, fl%iteration, 0, 'pr@af radm') ! debug_ww
#endif


    return
  contains
    function get_random_field_envelope(dm, yy) result(envelope)
      type(t_domain), intent(in) :: dm
      real(WP),       intent(in) :: yy
      real(WP)                   :: envelope

      select case(dm%icase)
      case (ICASE_CHANNEL)
        envelope = ONE - yy * yy
      case (ICASE_PIPE)
        envelope = yy * (ONE - yy)
      case (ICASE_ANNULAR)
        envelope = (yy - dm%lyb) * (ONE - yy)
      case default
        envelope = ONE
      end select
      envelope = max(ZERO, envelope)

      return
    end function get_random_field_envelope
  end subroutine

  !==============================================================================
  !> Generate a flow profile for Poiseuille flow in channel or pipe.
  !------------------------------------------------------------------------------
  !> Scope:  mpi    called-freq    xdomain     module
  !>         all    once           specified   private
  !------------------------------------------------------------------------------
  ! Arguments
  !------------------------------------------------------------------------------
  !  mode           name          role
  !------------------------------------------------------------------------------
  !> - d (in): domain
  !> - ux_1c1 (out): u(yc), velocity profile along wall-normal direction
  !==============================================================================
  subroutine Generate_poiseuille_flow_profile(dm, u_xy)
    use io_files_mod
    use math_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none

    type(t_domain), intent(in)  :: dm
    real(WP),       intent(out) :: u_xy(:, :)

    real(WP) :: ay, by, yy, ymax, ymin
    real(WP) :: ax, bx, xx, xmax, xmin
    real(WP) :: fx, fy, c
    integer :: pf_unit
    integer :: i, j

    if(nrank == 0) call Print_debug_inline_msg("Generate poiseuille flow profile ...")

    u_xy = ZERO

    ymax = dm%yp( dm%np_geo(2) )
    ymin = dm%yp( 1 )
    if (dm%icase == ICASE_CHANNEL) then
      ay = (ymax - ymin) * HALF
      by = (ymax + ymin) * HALF
      c = ONEPFIVE
    else if (dm%icase == ICASE_DUCT) then
      xmax = dm%lxx
      xmin = ZERO
      ay = (ymax - ymin) * HALF
      by = (ymax + ymin) * HALF
      ax = (xmax - xmin) * HALF
      bx = (xmax + xmin) * HALF
      c = NINE / FOUR
    else if (dm%icase == ICASE_PIPE) then
      ay = (ymax - ymin)
      by = ZERO
      c = TWO
    else if (dm%icase == ICASE_ANNULAR) then
      ay = (ymax - ymin) * HALF
      by = (ymax + ymin) * HALF
      c = TWO
    else
      ay = (ymax - ymin) * HALF
      by = ZERO
      c = ONEPFIVE
    end if

    do i = 1, dm%nc(1)
      if (dm%icase == ICASE_DUCT) then
        xx = dm%h(1) * (real(i - 1, WP) + HALF)
        fx = ONE - ((xx - bx)/ax)**2
      else
        fx = ONE
      end if
      do j = 1, dm%nc(2)
        yy = dm%yc(j)
        fy = ONE - ((yy - by)/ay)**2
        u_xy(i, j) = c * fx * fy
      end do
    end do
    !------------------------------------------------------------------------------
    !   Y-pencil : write out velocity profile
    !------------------------------------------------------------------------------
    if(nrank == 0 .and. .not. is_IO_off) then
      open ( newunit = pf_unit,     &
              file    = trim(dir_chkp)//'/check_poiseuille_ux_profile.dat', &
              status  = 'replace',         &
              action  = 'write')
      write(pf_unit, '(A)') "#xc, yc, u_xy"
      do j = 1, dm%nc(2)
        yy = dm%yc(j)
        if (dm%icase == ICASE_DUCT) then
          do i = 1, dm%nc(1)
            xx = dm%h(1) * (real(i - 1, WP) + HALF)
            write(pf_unit, '(3ES15.7)') xx, yy, u_xy(i, j)
          end do
        else
          write(pf_unit, '(1I3.1, 2ES15.7)') j, yy, u_xy(1, j)
        end if
      end do
      close(pf_unit)
    end if

    return
  end subroutine Generate_poiseuille_flow_profile

  !==============================================================================
  !> initialise Poiseuille flow in channel or pipe.
  !------------------------------------------------------------------------------
  !> Scope:  mpi    called-freq    xdomain     module
  !>         all    once           specified   private
  !------------------------------------------------------------------------------
  ! Arguments
  !------------------------------------------------------------------------------
  !  mode           name          role
  !------------------------------------------------------------------------------
  !> - d (in): domain
  !> - f (out): flow
  !==============================================================================
  subroutine initialise_poiseuille_flow(fl, dm)
    use boundary_conditions_mod
    use convert_primary_conservative_mod
    use find_max_min_ave_mod
    use input_general_mod
    use io_files_mod
    use io_restart_mod
    use parameters_constant_mod
    use udf_type_mod
    use wrt_debug_field_mod
    use wtformat_mod
    implicit none
    type(t_domain),intent(inout) :: dm
    type(t_flow), intent(inout) :: fl
    integer :: pf_unit
    integer :: i, j, k, jj
    real(WP) :: ubulk
    real(WP) :: u_xy(dm%nc(1), dm%nc(2))
    real(WP) :: ux(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3))
    real(WP) :: uz(dm%dccp%xsz(1), dm%dccp%xsz(2), dm%dccp%xsz(3))
    real(WP) :: ux_ypencil(dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3))
    character(2) :: str


    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_start_msg("Initialising Poiseuille flow field ...")
    !------------------------------------------------------------------------------
    !   x-pencil : to get Poiseuille profile for all ranks
    !------------------------------------------------------------------------------
    u_xy = ZERO
    call Generate_poiseuille_flow_profile (dm, u_xy)
    !------------------------------------------------------------------------------
    !   x-pencil : to add profile to ux (default: x streamwise)
    !------------------------------------------------------------------------------
    if(dm%icase == ICASE_DUCT) then
      dtmp = dm%dccp
      do i = 1, dtmp%xsz(1)
        do j = 1, dtmp%xsz(2)
          jj = dtmp%xst(2) + j - 1
          do k = 1, dtmp%xsz(3)
            fl%qz(i, j, k) =  fl%qz(i, j, k) + u_xy(i, jj)
          end do
        end do
      end do
      if(dm%is_thermo) then
        call convert_primary_conservative (dm, fl%dDens, IQ2G, IALL, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)
        uz = fl%gz
        str = 'gz'
      else
        uz = fl%qz
        str = 'qz'
      end if
      call Get_volumetric_average_3d(dm, dm%dccp, uz, ubulk, SPACE_AVERAGE, str)
      if(nrank == 0) &
      write(*, wrtfmt1e) "The initial, [original] bulk "//str//" = ", ubulk
      uz = uz / ubulk
      if(dm%is_thermo) then
        fl%gz = uz
        call convert_primary_conservative(dm, fl%dDens, IG2Q, IALL, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)
      else
        fl%qz = uz
      end if
      call Get_volumetric_average_3d(dm, dm%dccp, uz, ubulk, SPACE_AVERAGE, str)
      if(nrank == 0) &
      write(*, wrtfmt1e) "The initial, [scaled] bulk "//str//" = ", ubulk
    else
      dtmp = dm%dpcc
      do i = 1, dtmp%xsz(1)
        do j = 1, dtmp%xsz(2)
          jj = dtmp%xst(2) + j - 1
          do k = 1, dtmp%xsz(3)
            fl%qx(i, j, k) =  fl%qx(i, j, k) + u_xy(1, jj)
          end do
        end do
      end do
      if(dm%is_thermo) then
        call convert_primary_conservative (dm, fl%dDens, IQ2G, IALL, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)
        ux = fl%gx
        str = 'gx'
      else
        ux = fl%qx
        str = 'qx'
      end if
      call Get_volumetric_average_3d(dm, dm%dpcc, ux, ubulk, SPACE_AVERAGE, str)
      if(nrank == 0) &
      write(*, wrtfmt1e) "The initial, [original] bulk "//str//" = ", ubulk
      ux = ux / ubulk
      if(dm%is_thermo) then
        fl%gx = ux
        call convert_primary_conservative(dm, fl%dDens, IG2Q, IALL, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)
      else
        fl%qx = ux
      end if
      call Get_volumetric_average_3d(dm, dm%dpcc, ux, ubulk, SPACE_AVERAGE, str)
      if(nrank == 0) &
      write(*, wrtfmt1e) "The initial, [scaled] bulk "//str//" = ", ubulk
    end if

    ! to do : to add a scaling for turbulence generator inlet scaling, u = u * m / rho
    !------------------------------------------------------------------------------
    !   some checking
    !------------------------------------------------------------------------------
    ! if(dm%ibcx_nominal(1, 1) == IBC_PROFILE1D) then
    !   call initialise_fbcx_given_profile(dm%fbcx_qx, ux_xy, dm%dpcc%xst(2), 'qx')
    ! end if
    if(dm%ibcx_nominal(1, 1) == IBC_DATABASE) then
      call extract_dirichlet_fbcx(dm%fbcx_qx, fl%qx, dm%dpcc)
      call extract_dirichlet_fbcx(dm%fbcx_qy, fl%qy, dm%dcpc)
      call extract_dirichlet_fbcx(dm%fbcx_qz, fl%qz, dm%dccp)
    else if(dm%ibcx_nominal(1, 1) == IBC_POISEUILLE .and. dm%icase /= ICASE_DUCT) then
      dtmp = dm%dpcc
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        do k = 1, dtmp%xsz(3)
          dm%fbcx_qx(1, j, k) =  u_xy(1, jj)
          dm%fbcx_qy(1, j, k) = ZERO
          dm%fbcx_qz(1, j, k) = ZERO
        end do
      end do
      dm%fbcx_qx(2, :, :) = dm%fbcx_qx(1, :, :)
      dm%fbcx_qy(2, :, :) = dm%fbcx_qy(1, :, :)
      dm%fbcx_qz(2, :, :) = dm%fbcx_qz(1, :, :)
    else if(dm%ibcx_nominal(1, 1) == IBC_POISEUILLE .and. dm%icase == ICASE_DUCT) then
      dtmp = dm%dccp
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        do i = 1, dtmp%xsz(1)
          dm%fbcz_qx(i, j, 1) =  u_xy(i, jj)
          dm%fbcz_qy(i, j, 1) = ZERO
          dm%fbcz_qz(i, j, 1) = ZERO
        end do
      end do
      dm%fbcz_qx(:, :, 2) = dm%fbcz_qx(:, :, 1)
      dm%fbcz_qy(:, :, 2) = dm%fbcz_qy(:, :, 1)
      dm%fbcz_qz(:, :, 2) = dm%fbcz_qz(:, :, 1)
    else
    end if
    !if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine  initialise_poiseuille_flow
  !==============================================================================
  !==============================================================================
  subroutine initialise_flow_from_given_values(fl)
    use boundary_conditions_mod
    use parameters_constant_mod, only: ZERO
    use precision_mod, only: WP
    use udf_type_mod, only: t_domain
    implicit none
    !type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl

    if(nrank == 0) call Print_debug_inline_msg("Initialising flow field with given values...")
    !------------------------------------------------------------------------------
    !   x-pencil : update values
    !------------------------------------------------------------------------------
    fl%qx(:, :, :) = fl%qx(:, :, :) + fl%init_velo3d(1)
    fl%qy(:, :, :) = fl%qy(:, :, :) + fl%init_velo3d(2)
    fl%qz(:, :, :) = fl%qz(:, :, :) + fl%init_velo3d(3)
    !------------------------------------------------------------------------------
    !   x-pencil : apply b.c.
    !------------------------------------------------------------------------------

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

!==============================================================================
  !> Initialise a cylindrical domain with rigid-body rotation about the axis,
  !> optionally superposed on a uniform axial velocity:
  !>
  !>     u_x = init_velo3d(1),   u_r = 0,   u_theta = init_velo3d(3) * r
  !>
  !> This is an exact zero-strain state. In the physical (x, r, theta) tensor
  !>     L_theta,r = du_theta/dr               = +Omega
  !>     L_r,theta = (1/r)du_r/dtheta - u_th/r = -Omega
  !> so S_ij = 0 identically on any grid, while every other component vanishes
  !> because u_x is uniform and u_r is zero. It is therefore the sharpest
  !> available check on the cylindrical velocity-gradient assembly used by the
  !> LES model: an assembly that differentiates the stored qy = r*u_r instead of
  !> u_r, or that drops the -u_theta/r metric term, returns S_r,theta = Omega/2
  !> and so S_ij S_ij = Omega^2/2 instead of zero.
  !>
  !> For the discrete strain to vanish to round-off the mesh must be uniform in r
  !> (a CD2 derivative reproduces a function linear in *physical* r exactly only
  !> under a linear mapping) and the wall must move with the fluid, i.e.
  !> ifbcy_u = ..,4,..,init_velo3d(1) and ifbcy_w = ..,4,..,Omega*r_wall.
  !>
  !> - fl (inout): Flow state receiving the rotation profile.
  !> - dm (in): Domain descriptor.
  subroutine initialise_solid_body_rotation(fl, dm)
    use parameters_constant_mod, only: ZERO
    use precision_mod, only: WP
    use udf_type_mod, only: t_domain, t_flow
    implicit none
    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl

    integer :: j, k, jj

    if(nrank == 0) call Print_debug_inline_msg("Initialising flow field with solid-body rotation...")

    fl%pres(:, :, :) = ZERO
    fl%qx(:, :, :) = fl%init_velo3d(1)
    fl%qy(:, :, :) = ZERO               ! qy = r * u_r, and u_r = 0
    ! qz = u_theta lives at cell centres in y, hence dm%rc and the dccp y-offset.
    do k = 1, dm%dccp%xsz(3)
      do j = 1, dm%dccp%xsz(2)
        jj = dm%dccp%xst(2) + j - 1
        fl%qz(:, j, k) = fl%init_velo3d(3) * dm%rc(jj)
      end do
    end do

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

  !==============================================================================
  subroutine initialise_flow_from_given_inlet(fl, dm)
    use boundary_conditions_mod
    use parameters_constant_mod, only: ZERO
    use precision_mod, only: WP
    use udf_type_mod, only: t_domain

    implicit none
    type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl

    integer :: i, j, k, ii, jj, kk

    if(nrank == 0) call Print_debug_inline_msg("Initialising flow field with given profile...")
    !------------------------------------------------------------------------------
    !   x-pencil : update values
    !------------------------------------------------------------------------------
    do k = 1, dm%dpcc%xsz(3)
      kk = dm%dpcc%xst(3) + k - 1
      do j = 1, dm%dpcc%xsz(2)
        jj = dm%dpcc%xst(2) + j - 1 !local2global_yid(j, dm%dpcc)
        do i = 1, dm%dpcc%xsz(1)
          ii = dm%dpcc%xst(1) + i - 1
          fl%qx(i, j, k) = fl%qx(i, j, k) + dm%fbcx_qx(1, j, k)
        end do
      end do
    end do

    do k = 1, dm%dcpc%xsz(3)
      kk = dm%dcpc%xst(3) + k - 1
      do j = 1, dm%dcpc%xsz(2)
        jj = dm%dcpc%xst(2) + j - 1 !local2global_yid(j, dm%dcpc)
        do i = 1, dm%dcpc%xsz(1)
          ii = dm%dcpc%xst(1) + i - 1
          fl%qy(i, j, k) = fl%qy(i, j, k) + dm%fbcx_qy(1, j, k)
        end do
      end do
    end do

    do k = 1, dm%dccp%xsz(3)
      kk = dm%dccp%xst(3) + k - 1
      do j = 1, dm%dccp%xsz(2)
        jj = dm%dccp%xst(2) + j - 1 !(j, dm%dccp)
        do i = 1, dm%dccp%xsz(1)
          ii = dm%dccp%xst(1) + i - 1
          fl%qz(i, j, k) = fl%qz(i, j, k) + dm%fbcx_qz(1, j, k)
        end do
      end do
    end do
    !------------------------------------------------------------------------------
    !   x-pencil : apply b.c.
    !------------------------------------------------------------------------------

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

  !==============================================================================
  !> Initialise all flow fields according to the selected input mode.
  !> - fl (inout): Flow state to initialise.
  !> - dm (inout): Domain descriptor.
  subroutine initialise_flow_fields(fl, dm, opt_tm)
    use boundary_conditions_mod
    use continuity_eq_mod
    use convert_primary_conservative_mod
    use find_max_min_ave_mod
    use io_restart_mod
    use les_mod, only : calculate_les_wale
    use parameters_constant_mod
    use solver_tools_mod
    use statistics_mod
    use udf_type_mod
    use visualisation_field_mod
    use wrt_debug_field_mod
    use wtformat_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    type(t_flow), intent(inout)   :: fl
    type(t_thermo), intent(inout), optional :: opt_tm

    real(WP) :: velo(3)
    logical  :: is_thermo_fresh

    if(nrank == 0) call Print_debug_start_msg("Initialise flow fields ...")
  !------------------------------------------------------------------------------
  ! to set up Re. The Reynolds ramp is a function of where the run starts on the
  ! timeline, which is dm%iteration_start - not of which checkpoint was read.
  ! The two differ under restart_clock=reset.
  !------------------------------------------------------------------------------
    call Update_Re(dm%iteration_start, fl)
  !------------------------------------------------------------------------------
  ! initialise primary variables
  !------------------------------------------------------------------------------
    fl%time = ZERO
    fl%iteration = 0

    if(fl%inittype == INIT_RESTART) then
      !------------------------------------------------------------------------
      ! With a freshly initialised thermal field, the restart flow field is an
      ! initial condition rather than a continuation; it is read accordingly.
      !------------------------------------------------------------------------
      is_thermo_fresh = .false.
      if(dm%is_thermo .and. present(opt_tm)) is_thermo_fresh = (opt_tm%inittype /= INIT_RESTART)
      call read_instantaneous_flow(fl, dm, opt_is_initial=is_thermo_fresh)
      call restore_flow_variables_from_restart(fl, dm)
      ! The time-averaged fields are not read here. They are restored, together
      ! with their sample count, by init_stats_flow in post_statistics.f90,
      ! which runs earlier - from Buildup_mpi_domain_decomposition, where the
      ! accumulators are allocated.

    else if (fl%inittype == INIT_RANDOM) then
      call Generate_random_field(fl, dm)

    else if (fl%inittype == INIT_INLET) then

      if(dm%is_read_xinlet) then
        call read_instantaneous_xinlet(fl, dm, opt_iter=1)
      else
        call Generate_random_field(fl, dm)
      end if
      call initialise_flow_from_given_inlet(fl, dm)

    else if (fl%inittype == INIT_GVCONST) then
      call Generate_random_field(fl, dm)
      call initialise_flow_from_given_values(fl)

    else if (fl%inittype == INIT_POISEUILLE) then
      if(fl%is_active_tripping) then
        fl%pres(:, :, :) = ZERO
        fl%pcor(:, :, :) = ZERO
        fl%qx(:, :, :) = ZERO
        fl%qy(:, :, :) = ZERO
        fl%qz(:, :, :) = ZERO
        call Apply_pipe_active_tripping(fl, dm, ONE, ZERO)
      else
        call Generate_random_field(fl, dm)
      end if
      call initialise_poiseuille_flow(fl, dm)

    else if (fl%inittype == INIT_FUNCTION) then
      if (dm%icase == ICASE_TGV2D) then
        call initialise_vortexgreen_2dflow (fl, dm)
      else if (dm%icase == ICASE_TGV3D) then
        call initialise_vortexgreen_3dflow (fl, dm)
      else if (dm%icase == ICASE_BURGERS) then
        !call initialise_burgers_flow      (fl, dm)
      else if (dm%icoordinate == ICYLINDRICAL) then
        call initialise_solid_body_rotation(fl, dm)
      else
      end if
    else
    end if
!------------------------------------------------------------------------------
! One clock per run (see the restart_clock block in Read_input_parameters).
! This is the last field initialiser to run - initialise_thermo_fields is
! called before it from chapsim.f90 - so this is the first point at which both
! fields exist and the clock can be fixed for the whole run without a later
! restart read overwriting it. The dead reset that used to sit at the end of
! initialise_thermo_fields was trying to do exactly this, from the wrong place.
!
! The flow carries the run time because it is the field that has just read its
! checkpoint metadata. On a continuation with a fresh thermal field, the
! thermal field inherits that time instead of starting at zero, so the two
! fields report one physical time rather than two.
!------------------------------------------------------------------------------
    fl%iteration = dm%iteration_start
    if(dm%restart_clock == RESTART_CLOCK_RESET) fl%time = ZERO
    if(present(opt_tm)) then
      opt_tm%iteration = dm%iteration_start
      opt_tm%time      = fl%time
    end if
!------------------------------------------------------------------------------
! A reset treats the checkpoint as an initial condition, so the stored
! time-integration history belongs to a trajectory this run does not continue.
! Drop it and let the AB2 startup branch rebuild it, which is what
! rebuild_compact_flow_restart does for restart_history_mode=compact.
!------------------------------------------------------------------------------
    if(dm%restart_clock == RESTART_CLOCK_RESET) then
      if(fl%inittype == INIT_RESTART) then
        fl%mx_rhs0(:, :, :) = ZERO
        fl%my_rhs0(:, :, :) = ZERO
        fl%mz_rhs0(:, :, :) = ZERO
        fl%is_compact_restart_startup = .true.
      end if
      if(present(opt_tm)) then
        if(opt_tm%inittype == INIT_RESTART) opt_tm%ene_rhs0(:, :, :) = ZERO
      end if
    end if

    if(nrank == 0) call Print_debug_inline_msg("Max/Min [velocity] for real initial flow field:")
    call Find_max_min_3d(fl%qx, opt_name="qx")
    call Find_max_min_3d(fl%qy, opt_name="qy")
    call Find_max_min_3d(fl%qz, opt_name="qz")

    if(dm%is_thermo) then
      if(fl%inittype /= INIT_RESTART) then
        call convert_primary_conservative (dm, fl%dDens, IQ2G, IALL, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)
      else
        !------------------------------------------------------------------------
        ! A thermal restart stores both q and g, so the interior pair comes
        ! verbatim off disk and must not be recomputed - rebuilding g = q*rho
        ! would perturb the discretely divergence-free state that was written.
        ! Only the boundary planes need rebuilding, because fbc*_g* is not part
        ! of the checkpoint. This has to happen here and not in
        ! restore_thermo_variables_from_restart: initialise_thermo_fields runs
        ! before initialise_flow_fields (chapsim.f90), so at that point neither
        ! the restart flow field nor any rescaling of it has been applied yet.
        ! With a freshly initialised thermal field, the interior g = q*rho is
        ! rebuilt from the new density by read_instantaneous_flow instead.
        !------------------------------------------------------------------------
        call convert_primary_conservative (dm, fl%dDens, IQ2G, IBND)
      end if
      !call update_dyn_fbcx_from_flow(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcx_gy, dm%fbcx_gz)
      !call convert_primary_conservative(fl%dDens, dm, itag=IG2Q, iloc=IALL)
      if(nrank == 0) call Print_debug_inline_msg("Max/Min [mass flux] for real initial flow field:")
      call Find_max_min_3d(fl%gx, opt_name="gx")
      call Find_max_min_3d(fl%gy, opt_name="gy")
      call Find_max_min_3d(fl%gz, opt_name="gz")
    end if

#ifdef DEBUG_STEPS
    !call wrt_3d_pt_debug(fl%qx, dm%dpcc,   fl%iteration, 0, 'qx@bf inoutlet') ! debug_ww
    !call wrt_3d_pt_debug(fl%qy, dm%dcpc,   fl%iteration, 0, 'qy@bf inoutlet') ! debug_ww
    !call wrt_3d_pt_debug(fl%qz, dm%dccp,   fl%iteration, 0, 'qz@bf inoutlet') ! debug_ww
    !call wrt_3d_pt_debug(fl%pres, dm%dccc, fl%iteration, 0, 'pr@bf inoutlet') ! debug_ww
#endif

    !call update_dyn_fbcx_from_flow(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcx_qy, dm%fbcx_qz)
    !call enforce_domain_mass_balance_dyn_fbc(fl%drhodt, dm)
!------------------------------------------------------------------------------
! to initialise pressure correction term
!------------------------------------------------------------------------------
    fl%pcor(:, :, :) = ZERO
    ! to set up halo b.c. for cylindrical pipe
    if(dm%icase == ICASE_PIPE) call update_fbcy_cc_flow_halo(fl, dm)
!------------------------------------------------------------------------------
! Seed the LES eddy viscosity from the initial field, and record max(S_ij S_ij)
! on that same field as a regression gate (see t_flow%max_strain_rate_mag2_init).
! The first momentum substep recomputes tVisc before using it, so this does not
! change the solution; it only gives the 'init' visualisation a defined tVisc and
! gives the gate a field that has not been advanced yet.
!------------------------------------------------------------------------------
    if(dm%LES_model == ILES_WALE) then
      call calculate_les_wale(fl, dm, opt_max_strain_rate_mag2 = fl%max_strain_rate_mag2_init)
      if(nrank == 0) write(*, wrtfmt1e) '  max S_ij S_ij of the initial field :', fl%max_strain_rate_mag2_init
    end if

    if(dm%is_thermo) then
      call Get_volumetric_average_3d(dm, dm%dccc, fl%dDens, fl%total_mass_reference, &
                                     SPACE_INTEGRAL, 'initial total mass')
    else
      fl%total_mass_reference = dm%vol
    end if
    fl%total_mass = fl%total_mass_reference
    fl%total_mass_drift = ZERO

    call Check_element_mass_conservation(fl, dm, 0, opt_str='initial')
    if(.not. is_IO_off) call write_visu_flow(fl, dm, 'init')

    if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine

  !==============================================================================
  !> Initialise all thermal fields according to the selected input mode.
  !> - tm (inout): Thermal state to initialise.
  !> - fl (inout): Flow state coupled to thermal properties.
  !> - dm (inout): Domain descriptor.
  subroutine initialise_thermo_fields(tm, fl, dm)
    use boundary_conditions_mod
    use eq_energy_mod
    use io_restart_mod
    use parameters_constant_mod
    use statistics_mod
    use thermo_info_mod
    use udf_type_mod
    use visualisation_field_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm

    integer :: i

    if(.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_start_msg("Initialise thermo fields ...")
!------------------------------------------------------------------------------
! to set up Fr etc, require update flow Re first
!------------------------------------------------------------------------------
    call Update_Re(dm%iteration_start, fl)
    call Update_PrGr(fl, tm)
!------------------------------------------------------------------------------
! initialise primary variables
!------------------------------------------------------------------------------
    if(tm%inittype == INIT_RESTART) then
      call read_instantaneous_thermo  (tm, fl, dm)
      call restore_thermo_variables_from_restart(fl, tm, dm)
      !------------------------------------------------------------------------
      ! With a freshly initialised flow field, the restart thermal field is an
      ! initial condition: its energy RHS histories were accumulated against
      ! another flow field, so they are dropped.
      !------------------------------------------------------------------------
      if(fl%inittype /= INIT_RESTART) then
        tm%ene_rhs0(:, :, :) = ZERO
        if(dm%is_conv_outlet(1)) tm%fbcx_rhoh_rhs0(:, :) = ZERO
        if(nrank == 0) call Print_note_msg( &
          'Thermal restart with a freshly initialised flow field: the restart thermal field is used '// &
          'as an initial condition and its energy RHS history is dropped.')
      end if
      ! Time-averaged thermal fields are restored by init_stats_thermo; see the
      ! same note in initialise_flow_fields.
    else
      call initialise_thermal_properties (fl, tm, dm)
      if (dm%icase == ICASE_TGV3D) then
        call initialise_vortexgreen_3dflow_thermo(fl, tm, dm)
        call ftp_refresh_thermal_properties_from_T_undim_3Dtm(fl, tm, dm)
      end if
      tm%time = ZERO
    end if
!------------------------------------------------------------------------------
! The thermal field joins the run clock whichever branch produced it, so the
! 'init' visualisation written below already carries the run's iteration
! number. initialise_flow_fields runs after this one and sets the same clock on
! the flow, and reconciles tm%time with the flow's restart time; see the
! restart_clock block in Read_input_parameters for why the clock is one
! run-level quantity rather than a per-field one.
!------------------------------------------------------------------------------
    tm%iteration = dm%iteration_start
    if(dm%restart_clock == RESTART_CLOCK_RESET) tm%time = ZERO

    fl%dDens0(:, :, :) = fl%dDens(:, :, :)
    if(nrank == 0) call Print_debug_mid_msg("update_fbcy_cc_thermo_halo ...")
    if (dm%icase == ICASE_PIPE) call update_fbcy_cc_thermo_halo(tm, dm)

    if(.not. is_IO_off) then
      if(nrank == 0) call Print_debug_mid_msg("write_visu_thermo ...")
      call write_visu_thermo(tm, fl, dm, 'init')
    end if

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==============================================================================
!==============================================================================
!> initialise Vortex Green flow
!>
!> This subroutine is called locally once.
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - d (in): domain
!> - f (out): flow
!_______________________________________________________________________________
  subroutine  initialise_vortexgreen_2dflow(fl, dm)
    use math_mod
    use parameters_constant_mod!, only : HALF, ZERO, SIXTEEN, TWO
    use udf_type_mod

    implicit none
    type(t_domain), intent(in ) :: dm
    type(t_flow), intent(inout) :: fl
    real(WP) :: xc, yc
    real(WP) :: xp, yp
    integer :: i, j, ii, jj
    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_inline_msg("Initialising vortexgreen 2dflow ...")
!------------------------------------------------------------------------------
!   ux in x-pencil
!------------------------------------------------------------------------------
    dtmp = dm%dpcc
    do j = 1, dtmp%xsz(2)
      jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
      yc = dm%yc(jj)
      do i = 1, dtmp%xsz(1)
        ii = dtmp%xst(1) + i - 1
        xp = dm%h(1) * real(ii - 1, WP)
        fl%qx(i, j, :) =  sin_wp ( xp ) * cos_wp ( yc )
      end do
    end do
!------------------------------------------------------------------------------
!   uy in x-pencil
!------------------------------------------------------------------------------
    dtmp = dm%dcpc
    do j = 1, dtmp%xsz(2)
      jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
      yp = dm%yp(jj)
      do i = 1, dtmp%xsz(1)
        ii = dtmp%xst(1) + i - 1
        xc = dm%h(1) * (real(ii - 1, WP) + HALF)
        fl%qy(i, j, :) = -cos_wp ( xc ) * sin_wp ( yp )
      end do
    end do
!------------------------------------------------------------------------------
!   uz in x-pencil
!------------------------------------------------------------------------------
    fl%qz(:, :, :) =  ZERO
!------------------------------------------------------------------------------
!   p in x-pencil
!------------------------------------------------------------------------------
    fl%pres(:, :, :) =  ZERO
    ! dtmp = dm%dccc
    ! do j = 1, dtmp%xsz(2)
    !   jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
    !   yc = dm%yc(jj)
    !   do i = 1, dtmp%xsz(1)
    !     ii = dtmp%xst(1) + i - 1
    !     xc = dm%h(1) * (real(ii - 1, WP) + HALF)
    !     p(i, j, :)= ( cos_wp(TWO * xc) + sin(TWO * yc) ) * QUARTER
    !   end do
    ! end do

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine initialise_vortexgreen_2dflow
!==============================================================================
!==============================================================================
  subroutine  Validate_TGV2D_error(fl, dm)
    use io_files_mod
    use math_mod
    use parameters_constant_mod
    use udf_type_mod

    !use iso_fortran_env
    implicit none

    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl
    integer :: k, i, j, ii, jj!, kk
    real(wp) :: uerr, ue, uc, verr, perr
    real(wp) :: xc, yc, xp, yp
    real(wp) :: uerrmax, verrmax, perrmax
    real(wp) :: perr_work, perrmax_work
    real(wp) :: uerr_work, uerrmax_work
    real(wp) :: verr_work, verrmax_work

    type(DECOMP_INFO) :: dtmp
    character( len = 128) :: filename
    integer :: outputunit

!------------------------------------------------------------------------------
!   X-pencil : Find Max. error of ux
!------------------------------------------------------------------------------
    if(nrank == 0) call Print_debug_inline_msg("Validat TGV2D error ...")

    dtmp = dm%dpcc
    uerr = ZERO
    uerrmax = ZERO
    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !(j, dtmp)
        yc = dm%yc(jj)
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          xp = dm%h(1) * real(ii - 1, WP)
          uc = fl%qx(i, j, k)
          ue = sin_wp ( xp ) * cos_wp ( yc ) * exp(- TWO * fl%rre * fl%time)
          uerr = uerr + (uc - ue)**2
          if(abs_wp(uc - ue) > uerrmax) uerrmax = abs_wp(uc - ue)
        end do
      end do
    end do
    call mpi_barrier(MPI_COMM_WORLD, ierror)
    call mpi_allreduce(uerr,    uerr_work,    1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
    call mpi_allreduce(uerrmax, uerrmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    uerr_work = uerr_work / real(dm%np(1), wp) / real(dm%nc(2), wp) / real(dm%nc(3), wp)
    uerr_work = sqrt_wp(uerr_work)
!------------------------------------------------------------------------------
!   X-pencil : Find Max. error of uy
!------------------------------------------------------------------------------
    dtmp = dm%dcpc
    verr = ZERO
    verrmax = ZERO
    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
        yp = dm%yp(jj)
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          xc = dm%h(1) * (real(ii - 1, WP) + HALF)
          uc = fl%qy(i, j, k)
          ue = - cos_wp ( xc ) * sin_wp ( yp ) * exp(- TWO * fl%rre * fl%time)
          verr = verr + (uc - ue)**2
          if(abs_wp(uc - ue) > verrmax) verrmax = abs_wp(uc - ue)
        end do
      end do
    end do
    call mpi_barrier(MPI_COMM_WORLD, ierror)
    call mpi_allreduce(verr,    verr_work,    1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
    call mpi_allreduce(verrmax, verrmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    verr_work = verr_work / real(dm%nc(1), wp) / real(dm%np(2), wp) / real(dm%nc(3), wp)
    verr_work = sqrt_wp(verr_work)
!------------------------------------------------------------------------------
!   X-pencil : Find Max. error of p
!------------------------------------------------------------------------------
    dtmp = dm%dccc
    perr = ZERO
    perrmax = ZERO
    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
        yc = dm%yc(jj)
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          xc = dm%h(1) * (real(ii - 1, WP) + HALF)
          uc = fl%pres(i, j, k)
          ue = ( cos_wp ( TWO * xc ) + sin_wp ( TWO * yc ) ) * QUARTER * (exp(- TWO * fl%rre * fl%time))**2
          perr = perr + (uc - ue)**2
          if(abs_wp(uc - ue) > perrmax) perrmax = abs_wp(uc - ue)
        end do
      end do
    end do
    call mpi_barrier(MPI_COMM_WORLD, ierror)
    call mpi_allreduce(perr,    perr_work,    1, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
    call mpi_allreduce(perrmax, perrmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    perr_work = perr_work / real(dm%nc(1), wp) / real(dm%nc(2), wp) / real(dm%nc(3), wp)
    perr_work = sqrt_wp(perr_work)
!------------------------------------------------------------------------------
!   X-pencil : write data in rank=0
!------------------------------------------------------------------------------
    if(nrank == 0) then
      filename = 'Validation_TGV2d.dat'
      if(.not.file_exists(trim(filename))) then
        open(newunit = outputunit, file = trim(filename), action = "write", status = "new")
        write(outputunit, '(A)') 'Time, SD(u), SD(v), SD(p)'
      else
        open(newunit = outputunit, file = trim(filename), action = "write", status = "old", position="append")
      end if
      write(outputunit, '(1F10.4, 6ES17.7E3)') fl%time, uerr_work, verr_work, perr_work, &
            uerrmax_work, verrmax_work, perrmax_work
      close(outputunit)
    end if

    if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine
!==============================================================================
!==============================================================================
!> initialise Vortex Green flow
!>
!> This subroutine is called locally once.
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - d (in): domain
!> - f (out): flow
!_______________________________________________________________________________
  subroutine  initialise_vortexgreen_3dflow(fl, dm)
    use math_mod
    use parameters_constant_mod!, only : HALF, ZERO, SIXTEEN, TWO, PI
    use udf_type_mod

    implicit none
    type(t_domain), intent(in ) :: dm
    type(t_flow), intent(inout) :: fl
    real(WP) :: xc, yc, zc
    real(WP) :: xp, yp, zp
    integer :: i, j, k, ii, jj, kk
    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_inline_msg("Initialising Taylor Green Vortex flow field ...")
!------------------------------------------------------------------------------
!   ux in x-pencil
!------------------------------------------------------------------------------
    dtmp = dm%dpcc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      zc = dm%h(3) * (real(kk - 1, WP) + HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
        yc = dm%yc(jj)
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          xp = dm%h(1) * real(ii - 1, WP)
          fl%qx(i, j, k) =  sin_wp ( xp ) * cos_wp ( yc ) * cos_wp ( zc )
          !write(*,*) k, j, i, fl%qx(i,j,k)
        end do
      end do
    end do
!------------------------------------------------------------------------------
!   uy in x-pencil
!------------------------------------------------------------------------------
    dtmp = dm%dcpc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      zc = dm%h(3) * (real(kk - 1, WP) + HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !(j, dtmp)
        yp = dm%yp(jj)
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          xc = dm%h(1) * (real(ii - 1, WP) + HALF)
          fl%qy(i, j, k) = -cos_wp ( xc ) * sin_wp ( yp ) * cos_wp ( zc )
        end do
      end do
    end do
!------------------------------------------------------------------------------
!   uz in x-pencil
!------------------------------------------------------------------------------
    !uz(:, :, :) =  ZERO
    dtmp = dm%dccp
    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        do i = 1, dtmp%xsz(1)
          fl%qz(i, j, k) = zero
        end do
      end do
    end do
!------------------------------------------------------------------------------
!   p in x-pencil
!------------------------------------------------------------------------------
    dtmp = dm%dccc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      zc = dm%h(3) * (real(kk - 1, WP) + HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
        yc = dm%yc(jj)
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          xc = dm%h(1) * (real(ii - 1, WP) + HALF)
          fl%pres(i, j, k)= ONE / SIXTEEN * ( cos_wp(TWO * xc) + cos_wp(TWO * yc) ) * &
                      (cos_wp(TWO * zc) + TWO)
        end do
      end do
    end do

    if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine initialise_vortexgreen_3dflow
  !==============================================================================
  subroutine  initialise_vortexgreen_3dflow_thermo(fl, tm, dm)
    use math_mod
    use parameters_constant_mod!, only : HALF, ZERO, SIXTEEN, TWO, PI
    use udf_type_mod

    implicit none
    type(t_domain), intent(in ) :: dm
    type(t_flow), intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    real(WP) :: xc, yc, zc
    real(WP) :: xp, yp, zp
    real(WP) :: ux, uy, uz
    integer :: i, j, k, ii, jj, kk
    type(DECOMP_INFO) :: dtmp
    integer, parameter :: i_ini_T = 1 ! 1 = isothermal T perturbation, 2 = T proportional to Kinetic Energy

    if(nrank == 0) call Print_debug_inline_msg("Initialising Taylor Green Vortex thermo field ...")


    if( .not. dm%is_thermo) return

    dtmp = dm%dccc
    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      zc = dm%h(3) * (real(kk - 1, WP) + HALF)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
        yc = dm%yc(jj)
        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          xc = dm%h(1) * (real(ii - 1, WP) + HALF)
          ! Method 1:
          if(i_ini_T == 1) then ! isothermal perturbation
            tm%Ttemp(i, j, k)= ONE + fl%noiselevel * cos_wp(xc) * cos_wp(yc) * cos_wp(zc)
          else if(i_ini_T == 2) then ! T proportional to kinetic energy
            ux =  sin_wp ( xc ) * cos_wp ( yc ) * cos_wp ( zc )
            uy = -cos_wp ( xc ) * sin_wp ( yc ) * cos_wp ( zc )
            uz = ZERO
            tm%Ttemp(i, j, k)= ONE + fl%noiselevel * (ux * ux + uy * uy + uz * uz)
          else
            tm%Ttemp(i, j, k)= ONE
          end if
        end do
      end do
    end do

    if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine

end module flow_thermo_initialiasation
