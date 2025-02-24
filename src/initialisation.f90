!----------------------------------------------------------------------------------------------------------
!                      CHAPSim version 2.0.0
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
!----------------------------------------------------------------------------------------------------------
module flow_thermo_initialiasation
  use vars_df_mod
  use solver_tools_mod
  use print_msg_mod
  implicit none

  public  :: Allocate_flow_variables
  public  :: Allocate_thermo_variables
  private :: Generate_poiseuille_flow_profile
  private :: Generate_random_field

  private :: initialise_poiseuille_flow
  private :: initialise_flow_from_given_values
  private :: initialise_vortexgreen_2dflow
  private :: initialise_vortexgreen_3dflow

  public  :: Validate_TGV2D_error
  public  :: initialise_flow_fields
  public  :: initialise_thermo_fields

contains
!==========================================================================================================
!> \brief Allocate flow and thermal variables.     
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi    called-freq    xdomain     module
!>         all    once           specified   private
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                         
!----------------------------------------------------------------------------------------------------------
!> \param[in]     none          NA
!> \param[out]    none          NA
!==========================================================================================================
  subroutine Allocate_flow_variables (fl, dm)
    use parameters_constant_mod
    use mpi_mod
    implicit none

    type(t_domain), intent(in)    :: dm
    type(t_flow),   intent(inout) :: fl

    if(nrank == 0) call Print_debug_start_msg("Allocating flow variables ...")
    !----------------------------------------------------------------------------------------------------------
    ! default : x pencil. 
    ! varaible index is LOCAL. means 1:xsize(1)
    !----------------------------------------------------------------------------------------------------------
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

    if(dm%is_conv_outlet) then 
      allocate (fl%fbcx_qx_rhs0(dm%dpcc%xsz(2), dm%dpcc%xsz(3))); fl%fbcx_qx_rhs0 = ZERO
      allocate (fl%fbcx_qy_rhs0(dm%dcpc%xsz(2), dm%dcpc%xsz(3))); fl%fbcx_qy_rhs0 = ZERO
      allocate (fl%fbcx_qz_rhs0(dm%dccp%xsz(2), dm%dccp%xsz(3))); fl%fbcx_qz_rhs0 = ZERO
    end if

    if(dm%is_thermo) then
      call alloc_x(fl%gx,      dm%dpcc) ; fl%gx = ZERO
      call alloc_x(fl%gy,      dm%dcpc) ; fl%gy = ZERO
      call alloc_x(fl%gz,      dm%dccp) ; fl%gz = ZERO
      call alloc_x(fl%dDens,   dm%dccc) ; fl%dDens = ONE
      call alloc_x(fl%mVisc,   dm%dccc) ; fl%mVisc = ONE
      call alloc_x(fl%drhodt,  dm%dccc) ; fl%drhodt = ZERO

      call alloc_x(fl%dDensm1, dm%dccc) ; fl%dDensm1 = ONE
      call alloc_x(fl%dDensm2, dm%dccc) ; fl%dDensm2 = ONE

      call alloc_x(fl%gx0,      dm%dpcc) ; fl%gx0 = ZERO
      call alloc_x(fl%gy0,      dm%dcpc) ; fl%gy0 = ZERO
      call alloc_x(fl%gz0,      dm%dccp) ; fl%gz0 = ZERO

      if(dm%is_conv_outlet) then 
        allocate (fl%fbcx_gx_rhs0(dm%dpcc%xsz(2), dm%dpcc%xsz(3))); fl%fbcx_gx_rhs0 = ZERO
        allocate (fl%fbcx_gy_rhs0(dm%dcpc%xsz(2), dm%dcpc%xsz(3))); fl%fbcx_gy_rhs0 = ZERO
        allocate (fl%fbcx_gz_rhs0(dm%dccp%xsz(2), dm%dccp%xsz(3))); fl%fbcx_gz_rhs0 = ZERO
      end if

    end if

    if(nrank == 0) call Print_debug_end_msg
    return

  end subroutine Allocate_flow_variables
  !==========================================================================================================
  subroutine Allocate_thermo_variables (tm, dm)
    use parameters_constant_mod
    use mpi_mod
    use udf_type_mod
    use thermo_info_mod
    implicit none

    type(t_domain), intent(in)    :: dm
    type(t_thermo), intent(inout) :: tm

    if(.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_start_msg("Allocating thermal variables ...")
    !----------------------------------------------------------------------------------------------------------
    ! default : x pencil. 
    ! varaible index is LOCAL. means 1:xsize(1)
    !----------------------------------------------------------------------------------------------------------
    call alloc_x(tm%rhoh,     dm%dccc) ; tm%rhoh    = ZERO
    call alloc_x(tm%hEnth,    dm%dccc) ; tm%hEnth = ZERO
    call alloc_x(tm%kCond,    dm%dccc) ; tm%kCond = ONE
    call alloc_x(tm%tTemp,    dm%dccc) ; tm%tTemp = ONE
    call alloc_x(tm%ene_rhs,  dm%dccc) ; tm%ene_rhs = ZERO
    call alloc_x(tm%ene_rhs0, dm%dccc) ; tm%ene_rhs0 = ZERO

    if(dm%is_conv_outlet) then 
      allocate (tm%fbcx_rhoh_rhs0(dm%dccc%xsz(2), dm%dccc%xsz(3))); tm%fbcx_rhoh_rhs0 = ZERO
    end if

    if(nrank == 0) call Print_debug_end_msg
    return

  end subroutine Allocate_thermo_variables
  !==========================================================================================================
  !> \brief Generate a flow profile for Poiseuille flow in channel or pipe.     
  !---------------------------------------------------------------------------------------------------------- 
  !> Scope:  mpi    called-freq    xdomain     module
  !>         all    once           specified   private
  !----------------------------------------------------------------------------------------------------------
  ! Arguments
  !----------------------------------------------------------------------------------------------------------
  !  mode           name          role                                           
  !----------------------------------------------------------------------------------------------------------
  !> \param[in]     
  !> \param[out]    
  !==========================================================================================================
  subroutine Generate_random_field(dm, fl)
    use random_number_generation_mod
    use parameters_constant_mod
    use mpi_mod
    use math_mod
    use boundary_conditions_mod
    use flatten_index_mod
    use io_visualisation_mod
    use wtformat_mod
    use find_max_min_ave_mod
    use wrt_debug_field_mod
    type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    
    integer :: seed
    integer :: i, j, k! local id
    integer :: n, nsz  
    integer :: ii, jj, kk ! global id
    integer :: seed0 = 123456
    real(WP) :: rd, lownoise
    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_mid_msg("Generating random field ...")
    !----------------------------------------------------------------------------------------------------------
    !   Initialisation in x pencil
    !----------------------------------------------------------------------------------------------------------
    seed = 0
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
          jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
          !if( ( ONE - abs_wp(dm%yp(jj)) ) .LT. QUARTER) then
            !lownoise = fl%noiselevel * fl%noiselevel
          !else
            lownoise = fl%noiselevel
          !end if
          do i = 1, dtmp%xsz(1)
            ii = i
            seed = flatten_index(ii, jj, kk, dtmp%xsz(1), dtmp%ysz(2)) + seed0 * n
            call initialise_random_number ( seed )
            call Generate_r_random( -ONE, ONE, rd)
            if(n == 1) fl%qx(i, j, k) = lownoise * rd
            if(n == 2) fl%qy(i, j, k) = lownoise * rd * dm%rp(jj)
            if(n == 3) fl%qz(i, j, k) = lownoise * rd * dm%rc(jj)
          end do
        end do
      end do

    end do

    !     for dirichelt, the perturbation velocity should be zero.
    call enforce_velo_from_fbc(dm, fl%qx, fl%qy, fl%qz)

    if(nrank == 0) Call Print_debug_mid_msg(" Max/min velocity for generated random velocities:")
    call Find_max_min_absvar3d(fl%qx, "qx", wrtfmt2e)
    call Find_max_min_absvar3d(fl%qy, "qy", wrtfmt2e)
    call Find_max_min_absvar3d(fl%qz, "qz", wrtfmt2e)
! to validate the random number generated is MPI processor independent.
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%qx, dm%dpcc,   fl%iteration, 0, 'qx@af radm') ! debug_ww
    call wrt_3d_pt_debug(fl%qy, dm%dcpc,   fl%iteration, 0, 'qy@af radm') ! debug_ww
    call wrt_3d_pt_debug(fl%qz, dm%dccp,   fl%iteration, 0, 'qz@af radm') ! debug_ww
    call wrt_3d_pt_debug(fl%pres, dm%dccc, fl%iteration, 0, 'pr@af radm') ! debug_ww
#endif


    return
  end subroutine

  !==========================================================================================================
  !> \brief Generate a flow profile for Poiseuille flow in channel or pipe.     
  !---------------------------------------------------------------------------------------------------------- 
  !> Scope:  mpi    called-freq    xdomain     module
  !>         all    once           specified   private
  !----------------------------------------------------------------------------------------------------------
  ! Arguments
  !----------------------------------------------------------------------------------------------------------
  !  mode           name          role                                           
  !----------------------------------------------------------------------------------------------------------
  !> \param[in]     d             domain
  !> \param[out]    ux_1c1          u(yc), velocity profile along wall-normal direction
  !==========================================================================================================
  subroutine Generate_poiseuille_flow_profile(dm, ux_1c1)
    use parameters_constant_mod
    use udf_type_mod
    use math_mod
    use io_files_mod
    implicit none

    type(t_domain), intent(in)  :: dm
    real(WP),       intent(out) :: ux_1c1(:)
    
    real(WP):: a, b, c, yy, ymax, ymin
    integer :: pf_unit
    integer :: j
    
    if(nrank == 0) call Print_debug_mid_msg("Generate poiseuille flow profile ...")

    ux_1c1 (:) = ZERO

    ymax = dm%yp( dm%np_geo(2) )
    ymin = dm%yp( 1 )
    if (dm%icase == ICASE_CHANNEL) then
      a = (ymax - ymin) * HALF
      b = ZERO
      c = ONEPFIVE
    else if (dm%icase == ICASE_PIPE) then
      a = (ymax - ymin)
      b = ZERO
      c = TWO
    else if (dm%icase == ICASE_ANNULAR) then
      a = (ymax - ymin) * HALF
      b = (ymax + ymin) * HALF
      c = TWO
    else 
      a = (ymax - ymin) * HALF
      b = ZERO
      c = ONEPFIVE
    end if

    do j = 1, dm%nc(2)
      yy = dm%yc(j)
      ux_1c1(j) = ( ONE - ( (yy - b)**2 ) / a / a ) * c
    end do

    !----------------------------------------------------------------------------------------------------------
    !   Y-pencil : write out velocity profile
    !----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then
      open ( newunit = pf_unit,     &
              file    = trim(dir_chkp)//'/check_poiseuille_ux_profile.dat', &
              status  = 'replace',         &
              action  = 'write')
      write(pf_unit, '(A)') "#id,  yc, ux_laminar, ux_real"
      do j = 1, dm%nc(2)
        write(pf_unit, '(1I3.1, 2ES15.7)') j, dm%yc(j), ux_1c1(j)
      end do
      close(pf_unit)
    end if

    return
  end subroutine Generate_poiseuille_flow_profile

  !==========================================================================================================
  !> \brief initialise Poiseuille flow in channel or pipe.     
  !---------------------------------------------------------------------------------------------------------- 
  !> Scope:  mpi    called-freq    xdomain     module
  !>         all    once           specified   private
  !----------------------------------------------------------------------------------------------------------
  ! Arguments
  !----------------------------------------------------------------------------------------------------------
  !  mode           name          role                                           
  !----------------------------------------------------------------------------------------------------------
  !> \param[in]     d             domain
  !> \param[out]    f             flow
  !==========================================================================================================
  subroutine initialise_poiseuille_flow(dm, fl)
    use input_general_mod
    use udf_type_mod
    use boundary_conditions_mod
    use parameters_constant_mod
    use wtformat_mod
    use io_files_mod
    use io_restart_mod
    use convert_primary_conservative_mod
    use find_max_min_ave_mod
    use wrt_debug_field_mod
    implicit none
    type(t_domain),intent(inout) :: dm
    type(t_flow), intent(inout) :: fl
    integer :: pf_unit
    integer :: i, j, k, jj
    real(WP) :: ubulk
    real(WP) :: ux_1c1(dm%nc(2))
    real(WP) :: ux(dm%dpcc%xsz(1), dm%dpcc%xsz(2), dm%dpcc%xsz(3))
    real(WP) :: ux_ypencil(dm%dpcc%ysz(1), dm%dpcc%ysz(2), dm%dpcc%ysz(3))
    character(2) :: str
    

    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_start_msg("initialising Poiseuille flow field ...")
    !----------------------------------------------------------------------------------------------------------
    !   x-pencil : to get Poiseuille profile for all ranks
    !----------------------------------------------------------------------------------------------------------
    ux_1c1(:) = ZERO
    call Generate_poiseuille_flow_profile (dm, ux_1c1)
    !----------------------------------------------------------------------------------------------------------
    !   x-pencil : to add profile to ux (default: x streamwise)
    !----------------------------------------------------------------------------------------------------------
    dtmp = dm%dpcc
    do i = 1, dtmp%xsz(1)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
        do k = 1, dtmp%xsz(3)
          fl%qx(i, j, k) =  fl%qx(i, j, k) + ux_1c1(jj)
        end do
      end do
    end do
#ifdef DEBUG_STEPS
    call wrt_3d_pt_debug(fl%qx, dm%dpcc,   fl%iteration, 0, 'qx@af init') ! debug_ww
    call wrt_3d_pt_debug(fl%qy, dm%dcpc,   fl%iteration, 0, 'qy@af init') ! debug_ww
    call wrt_3d_pt_debug(fl%qz, dm%dccp,   fl%iteration, 0, 'qz@af init') ! debug_ww
    call wrt_3d_pt_debug(fl%pres, dm%dccc, fl%iteration, 0, 'pr@af init') ! debug_ww
#endif 
    !----------------------------------------------------------------------------------------------------------
    !   x-pencil : Ensure the mass flow rate is 1.
    !----------------------------------------------------------------------------------------------------------
    if(dm%is_thermo) then
      call convert_primary_conservative (fl, dm, IQ2G)
      ux = fl%gx
      str = 'gx'
    else
      ux = fl%qx
      str = 'qx'
    end if

    if(nrank == 0) Call Print_debug_mid_msg(" Max/min velocity for generated initial velocities:")
    call Find_max_min_absvar3d(fl%qx, "qx", wrtfmt2e)
    call Find_max_min_absvar3d(fl%qy, "qy", wrtfmt2e)
    call Find_max_min_absvar3d(fl%qz, "qz", wrtfmt2e)

    call Get_volumetric_average_3d_for_var_xcx(dm, dm%dpcc, ux, ubulk, LF3D_VOL_AVE, str)
    if(nrank == 0) then
      write(*, wrtfmt1e) "The initial, [original] bulk "//str//" = ", ubulk
    end if

    ux(:, :, :) = ux(:, :, :) / ubulk
    ux_1c1 = ux_1c1 / ubulk
    if(dm%is_thermo) then
      fl%gx = ux
      call convert_primary_conservative(fl, dm, IG2Q)
    else
      fl%qx = ux
    end if

    call Get_volumetric_average_3d_for_var_xcx(dm, dm%dpcc, ux, ubulk, LF3D_VOL_AVE, str)
    if(nrank == 0) then
      write(*, wrtfmt1e) "The initial, [scaled] bulk "//str//" = ", ubulk
    end if
    if(nrank == 0) call Print_debug_mid_msg(" Maximum [velocity] for real initial flow field:")
    call Find_max_min_absvar3d(fl%qx, "qx", wrtfmt2e)
    call Find_max_min_absvar3d(fl%qy, "qy", wrtfmt2e)
    call Find_max_min_absvar3d(fl%qz, "qz", wrtfmt2e)

    if(dm%is_thermo) then
      if(nrank == 0) call Print_debug_mid_msg(" Maximum [mass flux] for real initial flow field:")
      call Find_max_min_absvar3d(fl%gx, "gx", wrtfmt2e)
      call Find_max_min_absvar3d(fl%gy, "gy", wrtfmt2e)
      call Find_max_min_absvar3d(fl%gz, "gz", wrtfmt2e)
    end if

    ! to do : to add a scaling for turbulence generator inlet scaling, u = u * m / rho

    !----------------------------------------------------------------------------------------------------------
    !   some checking
    !----------------------------------------------------------------------------------------------------------
    call transpose_x_to_y(ux, ux_ypencil, dm%dpcc)
    if(dm%ibcx_nominal(1, 1) == IBC_PROFILE1D) then
      call initialise_fbcx_given_profile(dm%fbcx_qx, ux_1c1, dm%dpcc%xst(2), 'qx')
    end if
    if(dm%ibcx_nominal(1, 1) == IBC_DATABASE .and. &
       dm%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
      call extract_dirichlet_fbcx(dm%fbcx_qx, fl%qx, dm%dpcc)
      call extract_dirichlet_fbcx(dm%fbcx_qy, fl%qy, dm%dcpc)
      call extract_dirichlet_fbcx(dm%fbcx_qz, fl%qz, dm%dccp)
    end if
    
    !if(nrank == 0) call Print_debug_end_msg

    return
  end subroutine  initialise_poiseuille_flow
  !==========================================================================================================
  !==========================================================================================================
  subroutine initialise_flow_from_given_values(fl)
    use udf_type_mod, only: t_domain
    use precision_mod, only: WP
    use parameters_constant_mod, only: ZERO
    use boundary_conditions_mod
    implicit none
    !type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    
    if(nrank == 0) call Print_debug_mid_msg("initialising flow field with given values...")
    !----------------------------------------------------------------------------------------------------------
    !   x-pencil : update values
    !----------------------------------------------------------------------------------------------------------
    fl%qx(:, :, :) = fl%qx(:, :, :) + fl%init_velo3d(1)
    fl%qy(:, :, :) = fl%qy(:, :, :) + fl%init_velo3d(2)
    fl%qz(:, :, :) = fl%qz(:, :, :) + fl%init_velo3d(3)
    !----------------------------------------------------------------------------------------------------------
    !   x-pencil : apply b.c.
    !----------------------------------------------------------------------------------------------------------

    if(nrank == 0) call Print_debug_end_msg
    return
  end subroutine

!==========================================================================================================
  !==========================================================================================================
  subroutine initialise_flow_from_given_inlet(dm, fl)
    use udf_type_mod, only: t_domain
    use precision_mod, only: WP
    use parameters_constant_mod, only: ZERO
    use boundary_conditions_mod
    
    implicit none
    type(t_domain),  intent(in) :: dm
    type(t_flow), intent(inout) :: fl

    integer :: i, j, k, ii, jj, kk
    
    if(nrank == 0) call Print_debug_mid_msg("initialising flow field with given profile...")
    !----------------------------------------------------------------------------------------------------------
    !   x-pencil : update values
    !----------------------------------------------------------------------------------------------------------
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
    !----------------------------------------------------------------------------------------------------------
    !   x-pencil : apply b.c.
    !----------------------------------------------------------------------------------------------------------

    if(nrank == 0) call Print_debug_end_msg
    return
  end subroutine

  !==========================================================================================================
  subroutine initialise_flow_fields(fl, dm)
    use udf_type_mod
    use parameters_constant_mod
    use io_restart_mod
    use io_visualisation_mod
    use wtformat_mod
    use solver_tools_mod
    use continuity_eq_mod
    use boundary_conditions_mod
    use statistics_mod
    use convert_primary_conservative_mod
    use wrt_debug_field_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    type(t_flow), intent(inout)   :: fl

    real(WP) :: velo(3)

    if(nrank == 0) call Print_debug_start_msg("initialise flow fields ...")
  !----------------------------------------------------------------------------------------------------------
  ! to set up Re
  !----------------------------------------------------------------------------------------------------------
    call Update_Re(fl%iterfrom, fl)
  !----------------------------------------------------------------------------------------------------------
  ! initialise primary variables
  !----------------------------------------------------------------------------------------------------------
    fl%time = ZERO
    fl%iteration = 0

    if(fl%inittype == INIT_RESTART) then
      fl%iteration = fl%iterfrom
      fl%time = real(fl%iterfrom, WP) * dm%dt 
      call read_instantanous_flow(fl, dm)
      call restore_flow_variables_from_restart(fl, dm)
      !call read_statistics_flow(fl, dm)
      
    else if (fl%inittype == INIT_INTERPL) then

    else if (fl%inittype == INIT_RANDOM) then
      call Generate_random_field(dm, fl)

    else if (fl%inittype == INIT_INLET) then
      call Generate_random_field(dm, fl)
      call initialise_flow_from_given_inlet(dm, fl)

    else if (fl%inittype == INIT_GVCONST) then
      call Generate_random_field(dm, fl)
      call initialise_flow_from_given_values(fl)

    else if (fl%inittype == INIT_POISEUILLE) then
      call Generate_random_field(dm, fl)
      call initialise_poiseuille_flow(dm, fl)

    else if (fl%inittype == INIT_FUNCTION) then
      if (dm%icase == ICASE_TGV2D) then
        call initialise_vortexgreen_2dflow (dm, fl)
      else if (dm%icase == ICASE_TGV3D) then
        call initialise_vortexgreen_3dflow (dm, fl)
      else if (dm%icase == ICASE_BURGERS) then
        !call initialise_burgers_flow      (dm, fl)
      else
      end if
    else
    end if
!----------------------------------------------------------------------------------------------------------
! to initialise pressure correction term
!----------------------------------------------------------------------------------------------------------
    if(dm%is_thermo) then
      call convert_primary_conservative (fl, dm, IQ2G)
      !call update_dyn_fbcx_from_flow(dm, fl%gx, fl%gy, fl%gz, dm%fbcx_gx, dm%fbcx_gy, dm%fbcx_gz)
      !call convert_primary_conservative(fl, dm, IG2Q)
    end if
  
#ifdef DEBUG_STEPS
    !call wrt_3d_pt_debug(fl%qx, dm%dpcc,   fl%iteration, 0, 'qx@bf inoutlet') ! debug_ww
    !call wrt_3d_pt_debug(fl%qy, dm%dcpc,   fl%iteration, 0, 'qy@bf inoutlet') ! debug_ww
    !call wrt_3d_pt_debug(fl%qz, dm%dccp,   fl%iteration, 0, 'qz@bf inoutlet') ! debug_ww
    !call wrt_3d_pt_debug(fl%pres, dm%dccc, fl%iteration, 0, 'pr@bf inoutlet') ! debug_ww
#endif 
  
    !call update_dyn_fbcx_from_flow(dm, fl%qx, fl%qy, fl%qz, dm%fbcx_qx, dm%fbcx_qy, dm%fbcx_qz)
    !call enforce_domain_mass_balance_dyn_fbc(fl, dm)
!----------------------------------------------------------------------------------------------------------
! to initialise pressure correction term
!----------------------------------------------------------------------------------------------------------
    fl%pcor(:, :, :) = ZERO 

    call Check_element_mass_conservation(fl, dm, 0, 'initial') 
    call write_visu_flow(fl, dm, 'init')

    if(nrank == 0) call Print_debug_end_msg

    return
  end subroutine

  !==========================================================================================================
  subroutine initialise_thermo_fields(tm, fl, dm)
    use udf_type_mod
    use parameters_constant_mod
    use eq_energy_mod
    use thermo_info_mod
    use io_restart_mod
    use statistics_mod
    use io_visualisation_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm

    integer :: i

    if(.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_start_msg("initialise thermo fields ...")  
!----------------------------------------------------------------------------------------------------------
! to set up Fr etc, require update flow Re first
!----------------------------------------------------------------------------------------------------------
    call Update_Re(fl%iterfrom, fl)
    call Update_PrGr(fl, tm) 
!----------------------------------------------------------------------------------------------------------
! initialise primary variables
!----------------------------------------------------------------------------------------------------------
    if(tm%inittype == INIT_RESTART) then
      tm%iteration = tm%iterfrom
      tm%time = real(tm%iterfrom, WP) * dm%dt 
      call read_instantanous_thermo  (tm, dm)
      call restore_thermo_variables_from_restart(fl, tm, dm)
      call read_statistics_thermo(tm, dm)
    else if (tm%inittype == INIT_INTERPL) then
    else
      call initialise_thermal_properties (fl, tm)
      tm%time = ZERO
      tm%iteration = 0
    end if

    fl%dDensm1(:, :, :) = fl%dDens(:, :, :)
    fl%dDensm2(:, :, :) = fl%dDens(:, :, :)
 
    call write_visu_thermo(tm, fl, dm, 'init')

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
!> \brief initialise Vortex Green flow
!>
!> This subroutine is called locally once.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     d             domain
!> \param[out]    f             flow
!_______________________________________________________________________________
  subroutine  initialise_vortexgreen_2dflow(dm, fl)
    use parameters_constant_mod!, only : HALF, ZERO, SIXTEEN, TWO
    use udf_type_mod
    use math_mod
    
    implicit none
    type(t_domain), intent(in ) :: dm
    type(t_flow), intent(inout) :: fl
    real(WP) :: xc, yc
    real(WP) :: xp, yp
    integer :: i, j, ii, jj
    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_mid_msg("initialising vortexgreen 2dflow ...")
!----------------------------------------------------------------------------------------------------------
!   ux in x-pencil
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
!   uy in x-pencil
!---------------------------------------------------------------------------------------------------------- 
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
!----------------------------------------------------------------------------------------------------------
!   uz in x-pencil
!---------------------------------------------------------------------------------------------------------- 
    fl%qz(:, :, :) =  ZERO
!----------------------------------------------------------------------------------------------------------
!   p in x-pencil
!----------------------------------------------------------------------------------------------------------
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
    
    if(nrank == 0) call Print_debug_end_msg
    return
  end subroutine initialise_vortexgreen_2dflow
!==========================================================================================================
!==========================================================================================================
  subroutine  Validate_TGV2D_error(fl, dm)
    use parameters_constant_mod
    use udf_type_mod
    use math_mod
    use io_files_mod
    
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

!----------------------------------------------------------------------------------------------------------
!   X-pencil : Find Max. error of ux
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) call Print_debug_mid_msg("Validat TGV2D error ...")

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
!----------------------------------------------------------------------------------------------------------
!   X-pencil : Find Max. error of uy
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
!   X-pencil : Find Max. error of p
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
!   X-pencil : write data in rank=0
!----------------------------------------------------------------------------------------------------------
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

    if(nrank == 0) call Print_debug_end_msg

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
!> \brief initialise Vortex Green flow
!>
!> This subroutine is called locally once.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     d             domain
!> \param[out]    f             flow
!_______________________________________________________________________________
  subroutine  initialise_vortexgreen_3dflow(dm, fl)
    use parameters_constant_mod!, only : HALF, ZERO, SIXTEEN, TWO, PI
    use udf_type_mod
    use math_mod
    
    implicit none
    type(t_domain), intent(in ) :: dm
    type(t_flow), intent(inout) :: fl
    real(WP) :: xc, yc, zc
    real(WP) :: xp, yp, zp
    integer :: i, j, k, ii, jj, kk
    type(DECOMP_INFO) :: dtmp

    if(nrank == 0) call Print_debug_mid_msg("initialising Taylor Green Vortex flow field ...")
!----------------------------------------------------------------------------------------------------------
!   ux in x-pencil
!---------------------------------------------------------------------------------------------------------- 
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
          !write(*,*) k, j, i, sin_wp ( xp ) , cos_wp ( yc ) , cos_wp ( zc ), ux(i,j,k)
        end do
      end do
    end do
!----------------------------------------------------------------------------------------------------------
!   uy in x-pencil
!----------------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------------
!   uz in x-pencil
!---------------------------------------------------------------------------------------------------------- 
    !uz(:, :, :) =  ZERO
    dtmp = dm%dccp
    do k = 1, dtmp%xsz(3)
      do j = 1, dtmp%xsz(2)
        do i = 1, dtmp%xsz(1)
          fl%qz(i, j, k) = zero
        end do
      end do
    end do
!----------------------------------------------------------------------------------------------------------
!   p in x-pencil
!----------------------------------------------------------------------------------------------------------
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
          fl%pres(i, j, k)= ONE / SIXTEEN * ( cos(TWO * xc) + cos(TWO * yc) ) * &
                      (cos(TWO * zc) + TWO)
        end do
      end do
    end do

    if(nrank == 0) call Print_debug_end_msg
    
    return
  end subroutine initialise_vortexgreen_3dflow

end module flow_thermo_initialiasation
