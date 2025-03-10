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
!==========================================================================================================
!> \file geometry.f90
!>
!> \brief Building up the geometry and mesh information.
!>
!==========================================================================================================
module geometry_mod
  use vars_df_mod, only : domain
  use precision_mod
  use print_msg_mod
  implicit none
  
  !private
  private :: Buildup_grid_mapping_1D_3fmd
  private :: Buildup_grid_mapping_1D_tanh
  private :: Buildup_grid_mapping_1D_powerlaw
  public  :: Buildup_geometry_mesh_info
  
contains

  subroutine Buildup_grid_mapping_1D_powerlaw (str, n, dm, y, mp, opt_yp)
  ! Powerlaw is not a suitable mesh stretching method, but it's easily degraded to uniform
  ! this is only for debug of stretching grids. 
    use math_mod
    use udf_type_mod
    use typeconvert_mod
    use parameters_constant_mod
    implicit none
    character(len = *), intent(in) :: str
    integer,            intent(in) :: n
    type(t_domain),     intent(in) :: dm
    real(WP), optional, intent(in) :: opt_yp(:)
    real(WP), intent( out )        :: y(n)
    real(WP), intent( out )        :: mp(n, 3)
    integer :: j
    real(WP) :: eta_shift
    real(WP) :: eta_delta
    real(WP) :: alpha, beta, gamma, delta 
    
    real(WP) :: mm, ymin, ymax, ff
    real(WP), dimension(n) :: eta

    if(dm%mstret /= MSTRET_POWL) then 
      error stop 'Grid stretching method is not MSTRET_POWL.'
    end if
    if(nrank == 0) call Print_debug_inline_msg("Buildup_grid_mapping_1D_powerlaw for "//trim(str))
    !----------------------------------------------------------------------------------------------------------
    ! note: (1)if both yc and yp are calculated using the stretching function,
    !          yc_i /= (yp_i + yp_i+1)
    !       (2)if yc_i /= (yp_i + yp_i+1), the mapping function for yc is unknown explicitly.
    !----------------------------------------------------------------------------------------------------------
    eta_shift = ZERO
    eta_delta = ONE
    if ( trim( str ) == 'nd' ) then
      eta_shift = ZERO
      eta_delta = ONE / real( n - 1, WP )
    else if ( trim( str ) == 'cl' ) then
      eta_shift = ONE / ( real(n, WP) ) * HALF
      eta_delta = ONE / real( n, WP )
    else 
      call Print_error_msg('Grid stretching location not defined.')
    end if
    !----------------------------------------------------------------------------------------------------------
    ! to build up the computational domain \eta \in [0, 1] uniform mesh
    !----------------------------------------------------------------------------------------------------------
    eta(1) = ZERO + eta_shift

    do j = 2, n
      eta(j) = eta(1) + real(j - 1, WP) * eta_delta
    end do
    !write(*,*) 'eta', eta
    !----------------------------------------------------------------------------------------------------------
    ! to build up the physical domain y stretching grids based on Eq(53) of Leizet2009JCP
    ! and to build up the derivates based on Eq(53) and (47) in Leizet2009JCP
    !----------------------------------------------------------------------------------------------------------
    ymin = ZERO
    ymax = ZERO
    if (dm%istret == ISTRET_NO) then
      do j = 1, n
        y(j) = eta(j)
        y(j) = y(j) * (dm%lyt - dm%lyb) + dm%lyb
        mp(j, 1) = ONE
        mp(j, 2) = ONE
        mp(j, 3) = ONE
      end do
      return
    else if (dm%istret == ISTRET_CENTRE) then
       call Print_error_msg('Grid stretching flag is not valid.')
    else if (dm%istret == ISTRET_2SIDES) then
      ymin = ZERO
      ymax = ONE
    else if (dm%istret == ISTRET_BOTTOM) then
      ymin = ZERO
      ymax = ONE
    else if (dm%istret == ISTRET_TOP) then
      ymin = ZERO
      ymax = ONE
    else
      call Print_error_msg('Grid stretching flag is not valid.')
    end if

    beta = dm%rstret
    do j = 1, n
      !----------------------------------------------------------------------------------------------------------
      ! y \in [-1, 1] or [0, 1]
      !----------------------------------------------------------------------------------------------------------
      y(j) = eta(j)**int(beta)
      mp(j, 1) = beta * eta(j)**(int(beta)-1)
      if(mp(j, 1) < MINP .and. mp(j, 1) > MAXN) then
        mp(j, 1) = ONE
        if(nrank==0) call Print_warning_msg('Th mapping function for '//trim(str)//' at j = '//trim(int2str(j))//' is adjusted.')
      else
        mp(j, 1) = ONE / mp(j, 1)
      end if
      !----------------------------------------------------------------------------------------------------------
      ! y \in [lyb, lyt]
      !----------------------------------------------------------------------------------------------------------
      ff = (dm%lyt - dm%lyb) / (ymax - ymin)
      y(j) = (y(j) - ymin) * ff + dm%lyb
      mp(j, 1) = mp(j, 1) / ff
      !----------------------------------------------------------------------------------------------------------
      ! (1/h')^2
      !----------------------------------------------------------------------------------------------------------
      mp(j, 2) = mp(j, 1) * mp(j, 1)
      !mp(j, 3) is not used. 
    end do

    return
  end subroutine

  subroutine Buildup_grid_mapping_1D_tanh (str, n, dm, y, mp, opt_yp)
    use math_mod
    use udf_type_mod
    use parameters_constant_mod
    use typeconvert_mod
    implicit none
    character(len = *), intent(in) :: str
    integer,            intent(in) :: n
    type(t_domain),     intent(in) :: dm
    real(WP), optional, intent(in) :: opt_yp(:)
    real(WP), intent( out )        :: y(n)
    real(WP), intent( out )        :: mp(n, 3)
    integer :: j
    real(WP) :: eta_shift
    real(WP) :: eta_delta
    real(WP) :: alpha, beta, gamma, delta
    
    real(WP) :: mm, ymin, ymax, ff
    real(WP), dimension(n) :: eta

    if(dm%mstret /= MSTRET_TANH) then 
      error stop 'Grid stretching method is not MSTRET_TANH.'
    end if
    if(nrank == 0) call Print_debug_inline_msg("Buildup_grid_mapping_1D_tanh for "//trim(str))
    !----------------------------------------------------------------------------------------------------------
    ! note: (1)if both yc and yp are calculated using the stretching function,
    !          yc_i /= (yp_i + yp_i+1)
    !       (2)if yc_i /= (yp_i + yp_i+1), the mapping function for yc is unknown explicitly.
    !----------------------------------------------------------------------------------------------------------
    eta_shift = ZERO
    eta_delta = ONE
    if ( trim( str ) == 'nd' ) then
      eta_shift = ZERO
      eta_delta = ONE / real( n - 1, WP )
    else if ( trim( str ) == 'cl' ) then
      eta_shift = ONE / ( real(n, WP) ) * HALF
      eta_delta = ONE / real( n, WP )
    else 
      call Print_error_msg('Grid stretching location not defined.')
    end if
    !----------------------------------------------------------------------------------------------------------
    ! to build up the computational domain \eta \in [0, 1] uniform mesh
    !----------------------------------------------------------------------------------------------------------
    eta(1) = ZERO + eta_shift

    do j = 2, n
      eta(j) = eta(1) + real(j - 1, WP) * eta_delta
    end do
    !----------------------------------------------------------------------------------------------------------
    ! to build up the physical domain y stretching grids based on Eq(53) of Leizet2009JCP
    ! and to build up the derivates based on Eq(53) and (47) in Leizet2009JCP
    !----------------------------------------------------------------------------------------------------------
    gamma = ONE
    delta = ZERO
    ymin = ZERO
    ymax = ZERO
    if (dm%istret == ISTRET_NO) then
      do j = 1, n
        y(j) = eta(j)
        y(j) = y(j) * (dm%lyt - dm%lyb) + dm%lyb
        mp(j, 1) = ONE
        mp(j, 2) = ONE
        mp(j, 3) = ONE
      end do
      return
    else if (dm%istret == ISTRET_CENTRE) then
       call Print_error_msg('Grid stretching flag is not valid.')
    else if (dm%istret == ISTRET_2SIDES) then
      gamma = HALF
      delta = HALF
      ymin = -ONE
      ymax = ONE
    else if (dm%istret == ISTRET_BOTTOM) then
      gamma = -ONE
      delta = ONE
      ymin = ZERO
      ymax = ONE
    else if (dm%istret == ISTRET_TOP) then
      gamma = ONE
      delta = ZERO
      ymin = ZERO
      ymax = ONE
    else
      call Print_error_msg('Grid stretching flag is not valid.')
    end if

    beta = dm%rstret * TWENTY
    mm = tanh_wp(beta * gamma)
    do j = 1, n
      !----------------------------------------------------------------------------------------------------------
      ! y \in [-1, 1] or [0, 1]
      !----------------------------------------------------------------------------------------------------------
      if (present(opt_yp) .and. trim( str ) == 'cl') then
        y(j) = ( opt_yp(j) + opt_yp(j + 1)) * HALF
      else
        y(j) = tanh_wp(beta * (eta(j) - delta)) / mm
      end if
      mp(j, 1) = ONE - y(j) * y(j) * mm * mm
      if(mp(j, 1) < MINP .and. mp(j, 1) > MAXN) then
        mp(j, 1) = ONE
        if(nrank==0) call Print_warning_msg('Th mapping function for '//trim(str)//' at j = '//trim(int2str(j))//' is adjusted.')
      else
        mp(j, 1) = mm/beta / mp(j, 1)
      end if
      !----------------------------------------------------------------------------------------------------------
      ! y \in [lyb, lyt]
      !----------------------------------------------------------------------------------------------------------
      ff = (dm%lyt - dm%lyb) / (ymax - ymin)
      y(j) = (y(j) - ymin) * ff + dm%lyb
      mp(j, 1) = mp(j, 1) / ff
      !----------------------------------------------------------------------------------------------------------
      ! (1/h')^2
      !----------------------------------------------------------------------------------------------------------
      mp(j, 2) = mp(j, 1) * mp(j, 1)
      !mp(j, 3) is not used. 
    end do

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
!> \brief Building up the mesh mapping relation between physical domain and mesh
!>  to a computational domain and mesh.   
!>
!> This subroutine is used locally for 1D only.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     str          string to indicate mapping of cell centre or nodes
!> \param[in]     n            number of mapping points
!> \param[out]    y            the physical coordinate array
!> \param[out]    mp           the mapping relations for 1st and 2nd deriviatives
!_______________________________________________________________________________
  subroutine Buildup_grid_mapping_1D_3fmd (str, n, dm, y, mp, opt_yp)
    use math_mod
    use udf_type_mod
    use parameters_constant_mod
    implicit none
    character(len = *), intent(in) :: str
    integer,            intent(in) :: n
    type(t_domain),     intent(in) :: dm
    real(WP), optional, intent(in) :: opt_yp(:)
    real(WP), intent( out )        :: y(n)
    real(WP), intent( out )        :: mp(n, 3)
    integer :: j
    real(WP) :: eta_shift
    real(WP) :: eta_delta
    
    real(WP) :: cc, dd, ee, st1, st2, mm
    real(WP), dimension(n) :: eta
    real(WP) :: alpha, beta, gamma, delta

    if(dm%mstret /= MSTRET_3FMD) then 
      error stop 'Grid stretching method is not MSTRET_3FMD.'
    end if
    if(nrank == 0) call Print_debug_inline_msg("Buildup_grid_mapping_1D_3fmd for "//trim(str))
    !----------------------------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------------------------
    eta_shift = ZERO
    eta_delta = ONE
    if ( trim( str ) == 'nd' ) then
      eta_shift = ZERO
      eta_delta = ONE / real( n - 1, WP )
    else if ( trim( str ) == 'cl' ) then
      eta_shift = ONE / ( real(n, WP) ) * HALF
      eta_delta = ONE / real( n, WP )
    else 
      call Print_error_msg('Grid stretching location not defined.')
    end if
    
    !----------------------------------------------------------------------------------------------------------
    ! to build up the computational domain \eta \in [0, 1] uniform mesh
    !----------------------------------------------------------------------------------------------------------
    eta(1) = ZERO + eta_shift

    do j = 2, n
      eta(j) = eta(1) + real(j - 1, WP) * eta_delta
    end do
    !----------------------------------------------------------------------------------------------------------
    ! to build up the physical domain y stretching grids based on Eq(53) of Leizet2009JCP
    ! and to build up the derivates based on Eq(53) and (47) in Leizet2009JCP
    !----------------------------------------------------------------------------------------------------------
    gamma = ONE
    delta = ZERO
    if (dm%istret == ISTRET_NO) then
      do j = 1, n
        y(j) = eta(j)
        y(j) = y(j) * (dm%lyt - dm%lyb) + dm%lyb
        mp(j, 1) = ONE
        mp(j, 2) = ONE
        mp(j, 3) = ONE
      end do
      return
    else if (dm%istret == ISTRET_CENTRE) then
      gamma = ONE
      delta = ZERO
    else if (dm%istret == ISTRET_2SIDES) then
      gamma = ONE
      delta = HALF
    else if (dm%istret == ISTRET_BOTTOM) then
      gamma = HALF
      delta = HALF
    else if (dm%istret == ISTRET_TOP) then
      gamma = HALF
      delta = ZERO
    else
      call Print_error_msg('Grid stretching flag is not valid.')
    end if

    beta = dm%rstret
    alpha =  ( -ONE + sqrt_wp( ONE + FOUR * PI * PI * beta * beta ) ) / beta * HALF

    cc = sqrt_wp( alpha * beta + ONE ) / sqrt_wp( beta )
    dd = cc / sqrt_wp( alpha )
    ee = cc * sqrt_wp( alpha )

    st1 = (ONE   - TWO * delta) / gamma * HALF
    st2 = (THREE - TWO * delta) / gamma * HALF

    do j = 1, n
      mm = PI * (gamma * eta(j) + delta)
      !----------------------------------------------------------------------------------------------------------
      ! y \in [0, 1]
      !----------------------------------------------------------------------------------------------------------
      if (present(opt_yp) .and. trim( str ) == 'cl') then
        y(j) = ( opt_yp(j) + opt_yp(j + 1)) * HALF
      else
        y(j) = atan_wp ( dd * tan_wp( mm ) ) - &
               atan_wp ( dd * tan_wp( PI * delta) ) + &
               PI * ( heaviside_step( eta(j) - st1 ) + heaviside_step( eta(j) - st2 ) )
        y(j) = ONE / (gamma * ee) * y(j)
        if ( trim( str ) == 'nd' .and. j == 1) y(j) = ZERO
        if ( trim( str ) == 'nd' .and. j == n) y(j) = ONE
      end if
      !----------------------------------------------------------------------------------------------------------
      ! y \in [lyb, lyt]
      !----------------------------------------------------------------------------------------------------------
      y(j) = y(j) * (dm%lyt - dm%lyb) + dm%lyb
      !----------------------------------------------------------------------------------------------------------
      ! 1/h' = d\eta/dy
      !----------------------------------------------------------------------------------------------------------
      mp(j, 1) = (alpha / PI + sin_wp(mm) * sin_wp(mm) / PI / beta)  / (dm%lyt - dm%lyb)
      !----------------------------------------------------------------------------------------------------------
      ! (1/h')^2
      !----------------------------------------------------------------------------------------------------------
      mp(j, 2) = mp(j, 1) * mp(j, 1)
      !----------------------------------------------------------------------------------------------------------
      ! -h"/(h'^3) = 1/h' * [ d(1/h') / d\eta]
      !----------------------------------------------------------------------------------------------------------
      mp(j, 3) = gamma / (dm%lyt - dm%lyb) / beta * sin_wp(TWO * mm) * mp(j, 1)

    end do

    return
  end subroutine Buildup_grid_mapping_1D_3fmd
!==========================================================================================================
  subroutine Buildup_geometry_mesh_info (dm)
    use mpi_mod
    use math_mod
    use parameters_constant_mod
    use udf_type_mod
    use typeconvert_mod
    use mpi_mod
    use wtformat_mod
    use io_files_mod
    use find_max_min_ave_mod
    implicit none

    type(t_domain), intent(inout) :: dm
    

    integer  :: j
    integer  :: wrt_unit
    real(WP) :: dyp, dyn, ddy
    real(WP) :: dy(dm%nc(2))
    if(nrank == 0) call Print_debug_start_msg("Initialising domain geometric ...")

    !----------------------------------------------------------------------------------------------------------
    ! set up node number in geometry domain
    !----------------------------------------------------------------------------------------------------------
    do j = 1, NDIM
      dm%np_geo(j) = dm%nc(j) + 1 
    end do
    !----------------------------------------------------------------------------------------------------------
    ! set up node number in computational domain
    !----------------------------------------------------------------------------------------------------------
    do j = 1, NDIM
      if ( dm%is_periodic(j) ) then
        dm%np(j) = dm%nc(j)
      else 
        dm%np(j) = dm%np_geo(j)
      end if
    end do
    !----------------------------------------------------------------------------------------------------------
    ! set dx, dz for uniform grids
    !----------------------------------------------------------------------------------------------------------
    dm%h(1) = dm%lxx / real(dm%nc(1), WP)
    dm%h(3) = dm%lzz / real(dm%nc(3), WP)
    dm%h(2) = (dm%lyt - dm%lyb) / real(dm%nc(2), WP) ! default, uniform
    !----------------------------------------------------------------------------------------------------------
    ! allocate  variables for mapping physical domain to computational domain
    !----------------------------------------------------------------------------------------------------------
    allocate ( dm%yp( dm%np_geo(2) ) ) ! yp(1:np_geo)
    allocate ( dm%yc( dm%nc    (2) ) ) ! yc(1:nc)
    dm%yp(:) = ZERO
    dm%yc(:) = ZERO

    allocate ( dm%yMappingpt( dm%np_geo(2), 3 ) )
    allocate ( dm%yMappingcc( dm%nc    (2), 3 ) )
    dm%yMappingpt(:, :) = ONE
    dm%yMappingcc(:, :) = ONE
    if(dm%is_stretching(2)) then
      dm%h(2) = ONE / real(dm%nc(2), WP) ! updated for computational domain, check
      if(dm%mstret == MSTRET_3FMD) then
        ! stretching in only given function to provide a limited modes for a fast 3D FFT
        call Buildup_grid_mapping_1D_3fmd ('nd', dm%np_geo(2), dm, dm%yp(:), dm%yMappingpt(:, :))
        call Buildup_grid_mapping_1D_3fmd ('cl', dm%nc(2),     dm, dm%yc(:), dm%yMappingcc(:, :))
      else if(dm%mstret == MSTRET_TANH) then
        call Buildup_grid_mapping_1D_tanh ('nd', dm%np_geo(2), dm, dm%yp(:), dm%yMappingpt(:, :))
        call Buildup_grid_mapping_1D_tanh ('cl', dm%nc(2),     dm, dm%yc(:), dm%yMappingcc(:, :))
      else if(dm%mstret == MSTRET_POWL) then
        call Buildup_grid_mapping_1D_powerlaw('nd', dm%np_geo(2), dm, dm%yp(:), dm%yMappingpt(:, :))
        call Buildup_grid_mapping_1D_powerlaw('cl', dm%nc(2),     dm, dm%yc(:), dm%yMappingcc(:, :))
      else
        call Print_error_msg('Stretching grid is not supported for 2DECOMP_3DFFT')
      end if
      
    else
      dm%h(2) = (dm%lyt - dm%lyb) / real(dm%nc(2), WP)
      do j = 1, dm%np_geo(2)
        dm%yp(j) = real(j - 1, WP) * dm%h(2) + dm%lyb
      end do
      do j = 1, dm%nc(2)
        dm%yc(j) = real(j - 1, WP) * dm%h(2) + dm%h(2) * HALF + dm%lyb
      end do
    end if
!----------------------------------------------------------------------------------------------------------
! set 1/dx, 1/(dx)^2
!----------------------------------------------------------------------------------------------------------
    do j = 1, NDIM
      dm%h2r(j) = ONE / (dm%h(j) * dm%h(j))
      dm%h1r(j) = ONE / dm%h(j)
    end do

!----------------------------------------------------------------------------------------------------------
! allocate  cylindrical radius
!----------------------------------------------------------------------------------------------------------
    allocate ( dm%rpi( dm%np_geo(2) ) )
    allocate ( dm%rci( dm%nc    (2) ) )
    allocate ( dm%rp ( dm%np_geo(2) ) )
    allocate ( dm%rc ( dm%nc    (2) ) )
    if(dm%icoordinate == ICARTESIAN) then
      dm%rp(:) = ONE
      dm%rc(:) = ONE
      dm%rpi(:) = ONE
      dm%rci(:) = ONE
    else if(dm%icoordinate == ICYLINDRICAL) then 
      dm%rp(1 : dm%np_geo(2)) = dm%yp(1 : dm%np_geo(2))
      dm%rc(1 : dm%nc(2)) = dm%yc(1 : dm%nc(2))

      if(dabs( dm%yp(1) ) < MINP) then
        dm%rpi(1) = MAXP
      else 
        dm%rpi(1) = ONE / dm%yp(1)
      end if
      dm%rpi(2 : dm%np_geo(2)) = ONE / dm%yp(2 : dm%np_geo(2))
      dm%rci(:) = ONE / dm%yc(:)
!----------------------------------------------------------------------------------------------------------
! set up z-interior extention cells for pipe flow, zpencil only
!----------------------------------------------------------------------------------------------------------
      allocate (dm%knc_sym(dm%nc(3)))
      do j = 1, dm%nc(3)
        dm%knc_sym(j) = j + dm%nc(3)/2
        if(dm%knc_sym(j) > dm%nc(3)) dm%knc_sym(j) = dm%knc_sym(j) - dm%nc(3)
      end do
    end if
!----------------------------------------------------------------------------------------------------------
! print out data 
!----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then
      !write (*, wrtfmt1i) '------For the domain-x------ ', dm%idom
      write (*, wrtfmt3l) 'is periodic in xyz :', dm%is_periodic(1:NDIM)
      write (*, wrtfmt3i) 'geometry number of nodes     in xyz :', dm%np_geo(1:NDIM)
      write (*, wrtfmt3i) 'calculation number of cells  in xyz :', dm%nc(1:NDIM)
      write (*, wrtfmt3i) 'calculation number of points in xyz :', dm%np(1:NDIM)
      write (*, wrtfmt3r) 'grid spacing in x, z: ', dm%h(1), dm%h(3)
      write (*, wrtfmt1e) 'grid spacing in y(geometric     uniform) :', (dm%lyt - dm%lyb) / real(dm%nc(2), WP)
      write (*, wrtfmt1e) 'grid spacing in y(computational uniform) :', dm%h(2)
    end if
    !----------------------------------------------------------------------------------------------------------
    ! print out data for debugging
    !----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then

    !----------------------------------------------------------------------------------------------------------
    ! validate the mesh mapping function using the chain rule: dy = d(h(s)) = h'(s) ds
    !----------------------------------------------------------------------------------------------------------
      open(newunit = wrt_unit, file = trim(dir_chkp)//'/check_mesh_mapping.dat', action = "write", status = "replace")
      write(wrt_unit, *) 'index, dyn(numerical dy), dyp (physical dy), diff'
      dyn = dm%h(2)
      dy = ZERO
      do j = 2, dm%nc(2)
        if(dm%is_stretching(2)) &
        dyn = dm%h(2) / dm%yMappingcc(j, 1)
        dyp = dm%yp(j+1) - dm%yp(j)
        dy(j) = dyp
        ddy = dabs(dyn - dyp)
        write(wrt_unit, *) j, dyn, dyp, ddy
        !write(wrt_unit, *) j, dm%h(2) / dm%yMappingcc(j, 1), dm%yp(j+1) - dm%yp(j), dm%h(2) / dm%yMappingpt(j, 1), dm%yc(j) - dm%yc(j-1)
      end do
      close(wrt_unit)
      !call Find_max_min_1d(dy, 'dy')

      open(newunit = wrt_unit, file = trim(dir_chkp)//'/check_mesh_yp.dat', action = "write", status = "replace")
      write(wrt_unit, *) 'index, yp, rp, rpi'
      do j = 1, dm%np_geo(2)
        write (wrt_unit, *) j, dm%yp(j), dm%rp(j), dm%rpi(j)
      end do
      close(wrt_unit)
      
      open(newunit = wrt_unit, file = trim(dir_chkp)//'/check_mesh_yc.dat', action = "write", status = "replace")
      write(wrt_unit, *) 'index, yc, growth, rc, rci'
      ddy = ZERO
      do j = 1, dm%nc(2)
        if (j<=(dm%nc(2)-1)) then
          dyn = dm%yp(j+2)-dm%yp(j+1)
          dyp = dm%yp(j+1)-dm%yp(j)
          ddy = abs_wp( dyn - dyp) / min(dyn, dyp)
        end if
        write (wrt_unit, *) j, dm%yc(j), ddy, dm%rc(j), dm%rci(j)
      end do
      close(wrt_unit)

      if(dm%icoordinate == ICYLINDRICAL) then
        open(newunit = wrt_unit, file = trim(dir_chkp)//'/check_mesh_ksym.dat', action = "write", status = "replace")
        write(wrt_unit, *) 'knc, knc_sym'
        do j = 1, dm%nc(3)
          write (wrt_unit, *) j, dm%knc_sym(j)
        end do
        close(wrt_unit)
      end if
    end if

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine  Buildup_geometry_mesh_info

end module geometry_mod

