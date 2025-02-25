!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>                      CHAPSim version 2.0.0
!>                      --------------------------
!> This file is part of CHAPSim, a general-purpose CFD tool.
!>
!> This program is free software; you can redistribute it and/or modify it under
!> the terms of the GNU General Public License as published by the Free Software
!> Foundation; either version 3 of the License, or (at your option) any later
!> version.
!>
!> This program is distributed in the hope that it will be useful, but WITHOUT
!> ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
!> FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
!> details.
!>
!> You should have received a copy of the GNU General Public License along with
!> this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
!> Street, Fifth Floor, Boston, MA 02110-1301, USA.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!==========================================================================================================
!> \file chapsim.f90
!> \brief the main program.
!> \author Wei Wang, wei.wang@stfc.ac.uk
!> \date 
!==========================================================================================================
program chapsim
  implicit none

  call initialise_chapsim
  call Solve_eqs_iteration
  call Finalise_chapsim
  
end program
!==========================================================================================================
!> \brief Initialisation and preprocessing of geometry, mesh and tools
!> This subroutine is called at beginning of the main program
!==========================================================================================================
subroutine initialise_chapsim
  use boundary_conditions_mod
  use code_performance_mod
  use continuity_eq_mod
  use domain_decomposition_mod
  use io_files_mod
  use flow_thermo_initialiasation
  use geometry_mod
  use input_general_mod
  use mpi_mod
  use operations
  use poisson_interface_mod
  use solver_tools_mod
  use thermo_info_mod
  use io_visualisation_mod
  use eq_momentum_mod
  use wrt_debug_field_mod
  use mhd_mod
  implicit none
  integer :: i

!----------------------------------------------------------------------------------------------------------
! initialisation of mpi, nrank, nproc
!----------------------------------------------------------------------------------------------------------
  call create_directory
  call call_cpu_time(CPU_TIME_CODE_START, 0, 0)
  call initialise_mpi
!----------------------------------------------------------------------------------------------------------
! reading input parameters
!----------------------------------------------------------------------------------------------------------
  call Read_input_parameters
!----------------------------------------------------------------------------------------------------------
! build up geometry information, mesh size, grid spacing/spacing, streching factor etc... 
!----------------------------------------------------------------------------------------------------------
  do i = 1, nxdomain
    call Buildup_geometry_mesh_info(domain(i))
  end do
!----------------------------------------------------------------------------------------------------------
! build up operation coefficients for all x-subdomains
!----------------------------------------------------------------------------------------------------------
  call Prepare_LHS_coeffs_for_operations
!----------------------------------------------------------------------------------------------------------
! test all the operations.
!----------------------------------------------------------------------------------------------------------  
#ifdef DEBUG_ALGO
  call Test_algorithms()
  call Print_warning_msg(" === The solver will stop as per the user's request in <Test_algorithms> === ")
  stop 'End of Test_algorithms'
#endif
!----------------------------------------------------------------------------------------------------------
! build up domain decomposition
!----------------------------------------------------------------------------------------------------------
  call Buildup_mpi_domain_decomposition
!----------------------------------------------------------------------------------------------------------
! build up fft basic info
!----------------------------------------------------------------------------------------------------------
  do i = 1, nxdomain
    if(nrank == 0 ) call Print_debug_start_msg("initialising Poisson solver ...")
    call initialise_fft(domain(i))
    if(nrank == 0 ) call Print_debug_end_msg
  end do
!----------------------------------------------------------------------------------------------------------
! build up thermo_mapping_relations, independent of any domains
!----------------------------------------------------------------------------------------------------------
  do i = 1, nxdomain
    if (domain(i)%is_thermo) then
      call Buildup_thermo_mapping_relations(thermo(i))
      exit
    end if
  end do
!----------------------------------------------------------------------------------------------------------
! build up boundary condition
!----------------------------------------------------------------------------------------------------------
  do i = 1, nxdomain
    if(domain(i)%is_thermo) then
      call Convert_thermal_input_2undim(thermo(i), domain(i))
      call allocate_fbc_thermo(domain(i)) 
      call initialise_fbc_thermo_given(thermo(i), domain(i)) 
    end if
    call allocate_fbc_flow(domain(i)) 
    call initialise_fbc_flow_given(domain(i)) 
  end do
!----------------------------------------------------------------------------------------------------------
! initialise flow and thermo fields
!----------------------------------------------------------------------------------------------------------
  do i = 1, nxdomain
    call Allocate_flow_variables (flow(i), domain(i))
    if(domain(i)%is_thermo) then
      call Allocate_thermo_variables (thermo(i), domain(i))
      call initialise_thermo_fields(thermo(i), flow(i), domain(i))
    end if
    call initialise_flow_fields(flow(i), domain(i))
    if(domain(i)%is_mhd) then
      call initialise_mhd(flow(i), mhd(i), domain(i))
      call compute_Lorentz_force(flow(i), mhd(i), domain(i))
    end if
    call Solve_momentum_eq(flow(i), domain(i), 0)
    call Check_element_mass_conservation(flow(i), domain(i), 0, 'init-div-free') 
    call write_visu_flow(flow(i), domain(i))
    if(domain(i)%is_mhd) call write_visu_mhd(mhd(i), flow(i), domain(i))
    if(domain(i)%is_thermo)call write_visu_thermo(thermo(i), flow(i), domain(i))
#ifdef DEBUG_STEPS
    if(domain(i)%is_thermo) then
      call wrt_3d_pt_debug(flow(i)%gx, domain(i)%dpcc, flow(i)%iteration, 0, 'gx@bf solv') ! debug_ww
      call wrt_3d_pt_debug(flow(i)%gy, domain(i)%dcpc, flow(i)%iteration, 0, 'gy@bf solv') ! debug_ww
      call wrt_3d_pt_debug(flow(i)%gz, domain(i)%dccp, flow(i)%iteration, 0, 'gz@bf solv') ! debug_ww
    end if

    call wrt_3d_pt_debug(flow(i)%qx,   domain(i)%dpcc, flow(i)%iteration, 0, 'qx@bf solv') ! debug_ww
    call wrt_3d_pt_debug(flow(i)%qy,   domain(i)%dcpc, flow(i)%iteration, 0, 'qy@bf solv') ! debug_ww
    call wrt_3d_pt_debug(flow(i)%qz,   domain(i)%dccp, flow(i)%iteration, 0, 'qz@bf solv') ! debug_ww
    call wrt_3d_pt_debug(flow(i)%pres, domain(i)%dccc, flow(i)%iteration, 0, 'pr@bf solv') ! debug_ww
    !STOP  "STOP after initialisation"
    flow(i)%nIterFlowEnd = 10
#endif 
  end do
!----------------------------------------------------------------------------------------------------------
! update interface values for multiple domain
!----------------------------------------------------------------------------------------------------------
  do i = 1, nxdomain - 1
    call update_fbc_2dm_flow_halo(domain(i), flow(i), domain(i+1), flow(i+1))
    if(domain(i)%is_thermo) call update_fbc_2dm_thermo_halo(domain(i), thermo(i), domain(i+1), thermo(i+1))
  end do

#ifdef DEBUG_STEPS  
  !call test_poisson(domain(1)) ! debug_ww
  !call Print_warning_msg(" === The solver will stop as per the user's request. === ")
  !stop
#endif

  return
end subroutine initialise_chapsim

!==========================================================================================================
!==========================================================================================================
!> \brief solve the governing equations in iteration
!>
!> This subroutine is the main solver. 
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     none          NA
!> \param[out]    none          NA
!_______________________________________________________________________________
subroutine Solve_eqs_iteration
  !use iso_fortran_env
  use solver_tools_mod!,   only : Check_cfl_diffusion, Check_cfl_convection
  use continuity_eq_mod
  !use poisson_mod
  use eq_energy_mod
  use eq_momentum_mod
  use flow_thermo_initialiasation 
  use code_performance_mod
  use thermo_info_mod
  use vars_df_mod
  use input_general_mod
  use mpi_mod
  use wtformat_mod
  use io_visualisation_mod
  use io_monitor_mod
  use io_tools_mod
  use io_restart_mod
  use statistics_mod
  use typeconvert_mod
  use boundary_conditions_mod
  use find_max_min_ave_mod
  use mhd_mod
  implicit none

  logical, allocatable :: is_flow  (:)
  logical, allocatable :: is_thermo(:)
  integer :: i
  integer :: iter, isub
  integer :: iteration
  integer :: niter

  
  !==========================================================================================================
  ! flow advancing/marching iteration/time control
  !==========================================================================================================
  iteration = HUGE(0)
  niter     = 0
  do i = 1, nxdomain
     if( flow(i)%iteration    < iteration) iteration = flow(i)%iteration
     if( flow(i)%nIterFlowEnd > niter)     niter     = flow(i)%nIterFlowEnd
     if( domain(i)%is_thermo) then
       if (thermo(i)%iteration      < iteration) iteration = thermo(i)%iteration
       if (thermo(i)%nIterThermoEnd > niter)     niter     = thermo(i)%nIterThermoEnd
     end if
  end do

  allocate(is_flow  (nxdomain)) 
  allocate(is_thermo(nxdomain)) 
  is_flow   = .false.
  is_thermo = .false.


  call call_cpu_time(CPU_TIME_STEP_START, iteration, niter)

  if(nrank == 0) call Print_debug_start_msg("Solving the governing equations ...")

  do iter = iteration + 1, niter
    if( mod(iter, cpu_nfre) == 0) call call_cpu_time(CPU_TIME_ITER_START, iteration, niter, iter)
    
    !==========================================================================================================
    ! Solver preparation for each domain
    !==========================================================================================================
    do i = 1, nxdomain
      !----------------------------------------------------------------------------------------------------------
      !      setting up 1/re, 1/re/prt, gravity, etc
      !----------------------------------------------------------------------------------------------------------
      call Update_Re(iter, flow(i))
      !----------------------------------------------------------------------------------------------------------
      !     setting up thermo solver
      !----------------------------------------------------------------------------------------------------------
      if(domain(i)%is_thermo) then
        call Update_PrGr(flow(i), thermo(i))
        if ( (iter >= thermo(i)%nIterThermoStart) .and. (iter <= thermo(i)%nIterThermoEnd)) then
          is_thermo(i) = .true.
          if (nrank == 0) write(*, wrtfmt1r) "thermal field physical time (s) = ", thermo(i)%time
          thermo(i)%time = thermo(i)%time  + domain(i)%dt
          thermo(i)%iteration = thermo(i)%iteration + 1
        end if
      end if
      !----------------------------------------------------------------------------------------------------------
      !      setting up flow solver
      !----------------------------------------------------------------------------------------------------------
      if ( (iter >= flow(i)%nIterFlowStart) .and. (iter <=flow(i)%nIterFlowEnd)) then
        is_flow(i) = .true.
        if (nrank == 0) write(*, wrtfmt1r) "flow field physical time (s) = ", flow(i)%time
        flow(i)%time = flow(i)%time + domain(i)%dt
        flow(i)%iteration = flow(i)%iteration + 1
        call Check_cfl_diffusion (flow(i), domain(i))
        call Check_cfl_convection(flow(i)%qx, flow(i)%qy, flow(i)%qz, domain(i))
      end if

      !----------------------------------------------------------------------------------------------------------
      !  append and write out outlet data every real-iteration (not RK sub)
      !----------------------------------------------------------------------------------------------------------
      if(is_flow(i) .and. domain(i)%is_record_xoutlet) then
        call write_instantanous_xoutlet(flow(i), domain(i))
      end if
      !----------------------------------------------------------------------------------------------------------
      ! to read instantanous inlet from database, real, not sub-RK
      !----------------------------------------------------------------------------------------------------------
      if(domain(i)%is_read_xinlet) call read_instantanous_xinlet(flow(i), domain(i))     
    end do
    !==========================================================================================================
    !  main solver, domain coupling in each sub-iteration (check)
    !==========================================================================================================
    do isub = 1, domain(1)%nsubitr
      !----------------------------------------------------------------------------------------------------------
      ! update interface values for multiple domain
      !----------------------------------------------------------------------------------------------------------
      do i = 1, nxdomain - 1
        if(is_flow(i))   call update_fbc_2dm_flow_halo  (domain(i), flow(i),   domain(i+1), flow(i+1))
        if(is_thermo(i)) call update_fbc_2dm_thermo_halo(domain(i), thermo(i), domain(i+1), thermo(i+1))
      end do
      do i = 1, nxdomain
        if(is_thermo(i)) call Solve_energy_eq  (flow(i), thermo(i), domain(i), isub)
        if(domain(i)%is_mhd) call compute_Lorentz_force(flow(i), mhd(i), domain(i))
        if(is_flow(i))   call Solve_momentum_eq(flow(i), domain(i), isub)
      end do
#ifdef DEBUG_STEPS
      !call Print_warning_msg(" === The solver will stop as per the user's request. === ")
      !stop
#endif
    end do

    do i = 1, nxdomain - 1
      call update_fbc_2dm_flow_halo(domain(i), flow(i), domain(i+1), flow(i+1))
      if(domain(i)%is_thermo) call update_fbc_2dm_thermo_halo(domain(i), thermo(i), domain(i+1), thermo(i+1))
    end do
    
    !==========================================================================================================
    ! result post-processing for each domain
    !==========================================================================================================
    do i = 1, nxdomain
      !----------------------------------------------------------------------------------------------------------
      !  validation for each time step
      !----------------------------------------------------------------------------------------------------------
      if(nrank == 0) call Print_debug_mid_msg("For domain id = "//trim(int2str(i)))
      if(is_flow(i)) then
        if(is_thermo(i)) then
          call Find_max_min_3d(thermo(i)%tTemp, "T : ", wrtfmt2e)
          call Find_max_min_3d(thermo(i)%rhoh,  "rhoh:", wrtfmt2e)
        end if
        call Find_max_min_3d(flow(i)%qx, "qx: ", wrtfmt2e)
        call Find_max_min_3d(flow(i)%qy, "qy: ", wrtfmt2e)
        call Find_max_min_3d(flow(i)%qz, "qz: ", wrtfmt2e)
        call Check_element_mass_conservation(flow(i), domain(i), iter) 
      end if
      !----------------------------------------------------------------------------------------------------------
      !  update statistics
      !----------------------------------------------------------------------------------------------------------
      if (iter > domain(i)%stat_istart .and. is_flow(i)) then
        call update_statistics_flow(flow(i), domain(i))
      end if
      if(domain(i)%is_thermo .and. is_thermo(i)) then
        if (iter > domain(i)%stat_istart) then
          call update_statistics_thermo(thermo(i), domain(i))
        end if
      end if
      !----------------------------------------------------------------------------------------------------------
      !  monitoring 
      !----------------------------------------------------------------------------------------------------------
      !if(domain(i)%icase == ICASE_TGV2D) call Validate_TGV2D_error (flow(i), domain(i))
      if((.not. domain(i)%is_thermo) .and. is_flow(i)) then
        call write_monitor_bulk(flow(i), domain(i))
        call write_monitor_probe(flow(i), domain(i))
      end if
      if(domain(i)%is_thermo .and. is_thermo(i)) then
        call write_monitor_bulk(flow(i), domain(i), thermo(i))
        call write_monitor_probe(flow(i), domain(i), thermo(i))
      end if
      !----------------------------------------------------------------------------------------------------------
      !  write out check point data for restart
      !----------------------------------------------------------------------------------------------------------
      if (mod(iter, domain(i)%ckpt_nfre) == 0) then
        if(is_flow(i)) then
          call write_instantanous_flow(flow(i), domain(i))
          if(iter > domain(i)%stat_istart) call write_statistics_flow(flow(i), domain(i))
        end if
        if(domain(i)%is_thermo .and. is_thermo(i)) then
          call write_instantanous_thermo(thermo(i), domain(i))
          if(iter > domain(i)%stat_istart) call write_statistics_thermo(thermo(i), domain(i))
        end if
      end if
      !----------------------------------------------------------------------------------------------------------
      ! write data for visualisation
      !----------------------------------------------------------------------------------------------------------
      if(MOD(iter, domain(i)%visu_nfre) == 0) then
        if(is_flow(i)) call write_visu_flow(flow(i), domain(i))
        if(domain(i)%is_mhd) call write_visu_mhd(mhd(i), flow(i), domain(i))
        if(domain(i)%is_thermo .and. is_thermo(i)) then
          call write_visu_thermo(thermo(i), flow(i), domain(i))
        end if
        if(iter > domain(i)%stat_istart ) then
          if(is_flow(i)) call write_visu_stats_flow(flow(i), domain(i))
          if(domain(i)%is_thermo .and. is_thermo(i)) then
            call write_visu_stats_thermo(thermo(i), domain(i))
          end if
        end if
      end if

    end do ! domain

    if( mod(iter, cpu_nfre) == 0) call call_cpu_time(CPU_TIME_ITER_END, iteration, niter, iter)

  end do ! iteration

  call call_cpu_time(CPU_TIME_STEP_END, iteration, niter)
  
  return
end subroutine Solve_eqs_iteration


subroutine Finalise_chapsim
  use mpi_mod
  use code_performance_mod
  implicit none

  call call_cpu_time(CPU_TIME_CODE_END, 0, 0)
  call Finalise_mpi()
  return
end subroutine
