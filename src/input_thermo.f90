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
!> \file input_thermo.f90
!>
!> \brief Reading the input parameters from the given file and building up the
!> relationships between properties.
!>
!==========================================================================================================
module thermo_info_mod
  use parameters_constant_mod
  use udf_type_mod
  use wtformat_mod
  use mpi_mod
  use print_msg_mod
  implicit none

  integer, save :: N_FUNC2TABLE = 1024
  logical :: is_ftplist_dim
  type(t_fluidThermoProperty), save, allocatable, dimension(:) :: ftplist
  
  private :: buildup_property_relations_from_table
  private :: buildup_property_relations_from_function
  private :: ftplist_check_monotonicity_DH_of_HorT
  private :: Buildup_fluidparam
  private :: ftplist_sort_t_small2big
  private :: Write_thermo_property

  private :: ftp_is_T_in_scope
  private :: ftp_get_thermal_properties_dimensional_from_T
  private :: ftp_refresh_thermal_properties_from_T_undim
  public  :: ftp_refresh_thermal_properties_from_T_undim_3D
  private :: ftp_refresh_thermal_properties_from_H
  private :: ftp_convert_undim_to_dim
  private :: ftp_print
  public  :: ftp_refresh_thermal_properties_from_DH

  public  :: Buildup_thermo_mapping_relations
  public  :: initialise_thermal_properties
  public  :: Convert_thermal_input_2undim
  
contains
!==========================================================================================================
!==========================================================================================================
!> \brief Defination of a procedure in the type t_fluidThermoProperty.
!>  to check the temperature limitations.     
!>
!> This subroutine is called as required to check the temperature
!> of given element is within the given limits as a single phase
!> flow.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[inout]  this          a cell element with udf property
!_______________________________________________________________________________
  subroutine ftp_is_T_in_scope ( this )
    type( t_fluidThermoProperty ), intent( in ) :: this
    integer :: nlist

    nlist = fluidparam%nlist
    
    if(fluidparam%ipropertyState == IPROPERTY_TABLE) then

      if ( ( this%t < ftplist(1)%t     )  .OR. &
           ( this%t > ftplist(nlist)%t ) ) then
        write(*, wrtfmt3r) 'this T, low T, high T:', this%t, ftplist(1)%t, ftplist(nlist)%t
        write(*, wrtfmt3r) 'this rhoh, low rhoh, high rhoh', this%rhoh, fluidparam%dhmin, fluidparam%dhmax
        stop 'temperature exceeds specified range.'
      end if
    end if

    if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then
      if ( ( this%t < ( fluidparam%TM0 / fluidparam%ftp0ref%t ) ) .OR. &
           ( this%t > ( fluidparam%TB0 / fluidparam%ftp0ref%t ) ) ) then 
        write(*, wrtfmt3r) 'this T, low T, high T:', this%t, fluidparam%TM0 / fluidparam%ftp0ref%t, fluidparam%TB0 / fluidparam%ftp0ref%t
        write(*, wrtfmt3r) 'this rhoh, low rhoh, high rhoh', this%rhoh, fluidparam%dhmin, fluidparam%dhmax
        stop 'temperature exceeds specified range.'
      end if
    end if

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
!> \brief Defination of a procedure in the type t_fluidThermoProperty.
!>  to update the thermal properties based on the known temperature.     
!>
!> This subroutine is called as required to update all thermal properties from
!> the known temperature (dimensional or dimensionless).
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[inout]  this          a cell element with udf property
!> \param[in]     dim           an optional indicator of dimensional
!>                              /dimensionless T. Exsiting of \ref dim indicates
!>                              the known/given T is dimensional. Otherwise, not.
!_______________________________________________________________________________
  subroutine ftp_get_thermal_properties_dimensional_from_T (this)
    type(t_fluidThermoProperty), intent(inout) :: this
    
    integer :: i1, i2, im
    real(WP) :: d1, dm
    real(WP) :: w1, w2
    real(WP) :: t1, dummy

    if(.not. is_ftplist_dim) call Print_error_msg("Error. Please provide dimensional thermal property.")

    if(fluidparam%ipropertyState == IPROPERTY_TABLE) then 
      i1 = 1
      i2 = fluidparam%nlist
      do while ( (i2 - i1) > 1)
        im = i1 + (i2 - i1) / 2
        d1 = ftplist(i1)%t - this%t
        dm = ftplist(im)%t - this%t
        if ( (d1 * dm) > MINP ) then
          i1 = im
        else
          i2 = im
        end if
      end do

      w1 = (ftplist(i2)%t - this%t) / (ftplist(i2)%t - ftplist(i1)%t) 
      w2 = ONE - w1

      this%d = w1 * ftplist(i1)%d + w2 * ftplist(i2)%d
      this%m = w1 * ftplist(i1)%m + w2 * ftplist(i2)%m
      this%k = w1 * ftplist(i1)%k + w2 * ftplist(i2)%k
      this%h = w1 * ftplist(i1)%h + w2 * ftplist(i2)%h
      this%b = w1 * ftplist(i1)%b + w2 * ftplist(i2)%b
      this%cp = w1 * ftplist(i1)%cp + w2 * ftplist(i2)%cp
      this%rhoh = this%d * this%h

    else if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then 
      
      t1 = this%t
  
      ! D = density = f(T)
      this%d = fluidparam%CoD(0) + &
               fluidparam%CoD(1) * t1
  
      ! K = thermal conductivity = f(T)
      this%k = fluidparam%CoK(0) + &
               fluidparam%CoK(1) * t1 + &
               fluidparam%CoK(2) * t1**2
  
      ! Cp = f(T)
      this%cp = fluidparam%CoCp(-2) * t1**(-2) + & 
                fluidparam%CoCp(-1) * t1**(-1) + &
                fluidparam%CoCp(0) + &
                fluidparam%CoCp(1) * t1 + &
                fluidparam%CoCp(2) * t1**2
  
      ! H = entropy = f(T)
      this%h = fluidparam%Hm0 + &
               fluidparam%CoH(-1) * (ONE / t1 - ONE / fluidparam%TM0) + &
               fluidparam%CoH(0) + &
               fluidparam%CoH(1) * (t1    - fluidparam%TM0) + &
               fluidparam%CoH(2) * (t1**2 - fluidparam%TM0**2) + &
               fluidparam%CoH(3) * (t1**3 - fluidparam%TM0**3)
  
      ! B = f(T)
      this%b = ONE / (fluidparam%CoB - t1)
  
      ! dynamic viscosity = f(T)
      select case (fluidparam%ifluid)
        ! unit: T(Kelvin), M(Pa S)
        case (ILIQUID_SODIUM)
          dummy = EXP( CoM_Na(-1) / t1 + &
                       CoM_Na(0) + &
                       CoM_Na(1) * LOG(t1) )
        case (ILIQUID_LEAD)
          dummy = CoM_Pb(0)  * EXP (CoM_Pb(-1) / t1)
        case (ILIQUID_BISMUTH)
          dummy = CoM_Bi(0)  * EXP (CoM_Bi(-1) / t1)
        case (ILIQUID_LBE)
          dummy = CoM_LBE(0) * EXP (CoM_LBE(-1) / t1)
        case default
          dummy = EXP( CoM_Na(-1) / t1 + &
                       CoM_Na(0) + &
                       CoM_Na(1) * LOG(t1) )
      end select
      this%m = dummy
      this%rhoh = this%d * this%h
    else
      call Print_error_msg ("Error.")
    end if
    return
  end subroutine ftp_get_thermal_properties_dimensional_from_T
!==========================================================================================================
!==========================================================================================================
  subroutine ftp_refresh_thermal_properties_from_T_undim ( this )
    type(t_fluidThermoProperty), intent(inout) :: this

    type(t_fluidThermoProperty) :: ftp0ref
    integer :: i1, i2, im
    real(WP) :: d1, dm
    real(WP) :: w1, w2
    real(WP) :: t1, dummy

    !call Print_debug_start_msg("ftp_refresh_thermal_properties_from_T_undim")
    if(fluidparam%ipropertyState == IPROPERTY_TABLE) then 
      i1 = 1
      i2 = fluidparam%nlist
      do while ( (i2 - i1) > 1)
        im = i1 + (i2 - i1) / 2
        d1 = ftplist(i1)%t - this%t
        dm = ftplist(im)%t - this%t
        if ( (d1 * dm) > MINP ) then
          i1 = im
        else
          i2 = im
        end if
      end do

      w1 = (ftplist(i2)%t - this%t) / (ftplist(i2)%t - ftplist(i1)%t) 
      w2 = ONE - w1

      this%d = w1 * ftplist(i1)%d + w2 * ftplist(i2)%d
      this%m = w1 * ftplist(i1)%m + w2 * ftplist(i2)%m
      this%k = w1 * ftplist(i1)%k + w2 * ftplist(i2)%k
      this%h = w1 * ftplist(i1)%h + w2 * ftplist(i2)%h
      this%b = w1 * ftplist(i1)%b + w2 * ftplist(i2)%b
      this%cp = w1 * ftplist(i1)%cp + w2 * ftplist(i2)%cp
      this%rhoh = this%d * this%h

    else if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then 
      
      ftp0ref =fluidparam%ftp0ref
      ! convert undim to dim 
      t1 = this%t * ftp0ref%t

      ! D = density = f(T)
      dummy = fluidparam%CoD(0) + &
              fluidparam%CoD(1) * t1
      this%d = dummy / ftp0ref%d
  
      ! K = thermal conductivity = f(T)
      dummy = fluidparam%CoK(0) + &
              fluidparam%CoK(1) * t1 + &
              fluidparam%CoK(2) * t1**2
      this%k = dummy / ftp0ref%k
  
      ! Cp = f(T)
      dummy = fluidparam%CoCp(-2) * t1**(-2) + &
              fluidparam%CoCp(-1) * t1**(-1) + &
              fluidparam%CoCp(0) + &
              fluidparam%CoCp(1) * t1 + &
              fluidparam%CoCp(2) * t1**2
      this%cp = dummy / ftp0ref%cp
  
      ! H = entropy = f(T)
      dummy = fluidparam%Hm0 + &
              fluidparam%CoH(-1) * (ONE / t1 - ONE / fluidparam%TM0) + &
              fluidparam%CoH(0) + &
              fluidparam%CoH(1) * (t1    - fluidparam%TM0) + &
              fluidparam%CoH(2) * (t1**2 - fluidparam%TM0**2) + &
              fluidparam%CoH(3) * (t1**3 - fluidparam%TM0**3)
      this%h = (dummy - ftp0ref%h) / (ftp0ref%cp * ftp0ref%t)
  
      ! B = f(T)
      dummy = ONE / (fluidparam%CoB - t1)
      this%b = dummy / ftp0ref%b
  
      ! dynamic viscosity = f(T)
      select case (fluidparam%ifluid)
        ! unit: T(Kelvin), M(Pa S)
        case (ILIQUID_SODIUM)
          dummy = EXP( CoM_Na(-1) / t1 + CoM_Na(0) + CoM_Na(1) * LOG(t1) )
        case (ILIQUID_LEAD)
          dummy = CoM_Pb(0) * EXP (CoM_Pb(-1) / t1)
        case (ILIQUID_BISMUTH)
          dummy = CoM_Bi(0) * EXP (CoM_Bi(-1) / t1)
        case (ILIQUID_LBE)
          dummy = CoM_LBE(0) * EXP (CoM_LBE(-1) / t1)
        case default
          dummy = EXP( CoM_Na(-1) / t1 + CoM_Na(0) + CoM_Na(1) * LOG(t1) )
      end select
      this%m = dummy / ftp0ref%m
      this%rhoh = this%d * this%h
    else
      this%t  = ONE
      this%d  = ONE
      this%m  = ONE
      this%k  = ONE
      this%cp = ONE
      this%b  = ONE
      this%h  = ZERO
      this%rhoh = ZERO
    end if
    return
  end subroutine ftp_refresh_thermal_properties_from_T_undim
  !==========================================================================================================
!==========================================================================================================
  subroutine ftp_refresh_thermal_properties_from_T_undim_3D ( ftp3d )
    type(t_fluidThermoProperty), intent(inout) :: ftp3d(:, :, :)
    integer :: i, j, k

    do i = 1, size(ftp3d, 1)
      do j = 1, size(ftp3d, 2)
        do k = 1, size(ftp3d, 3)
            call ftp_refresh_thermal_properties_from_T_undim(ftp3d(i, j, k))
        end do
      end do
    end do 

    return
  end subroutine ftp_refresh_thermal_properties_from_T_undim_3D
!==========================================================================================================
!==========================================================================================================
!> \brief Defination of a procedure in the type t_fluidThermoProperty.
!>  to update the thermal properties based on the known enthalpy.     
!>
!> This subroutine is called as required to update all thermal properties from
!> the known enthalpy (dimensionless only).
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[inout]  this          a cell element with udf property
!_______________________________________________________________________________
  subroutine ftp_refresh_thermal_properties_from_H(this)
    type(t_fluidThermoProperty), intent(inout) :: this

    integer :: i1, i2, im
    real(WP) :: d1, dm
    real(WP) :: w1, w2

    i1 = 1
    i2 = fluidparam%nlist

    do while ( (i2 - i1) > 1)
      im = i1 + (i2 - i1) / 2
      d1 = ftplist(i1)%h - this%h
      dm = ftplist(im)%h - this%h
      if ( (d1 * dm) > MINP ) then
        i1 = im
      else
        i2 = im
      end if
    end do

    w1 = (ftplist(i2)%h - this%h) / (ftplist(i2)%h - ftplist(i1)%h) 
    w2 = ONE - w1

    this%d  = w1 * ftplist(i1)%d  + w2 * ftplist(i2)%d
    this%m  = w1 * ftplist(i1)%m  + w2 * ftplist(i2)%m
    this%k  = w1 * ftplist(i1)%k  + w2 * ftplist(i2)%k
    this%t  = w1 * ftplist(i1)%t  + w2 * ftplist(i2)%t
    this%b  = w1 * ftplist(i1)%b  + w2 * ftplist(i2)%b
    this%cp = w1 * ftplist(i1)%cp + w2 * ftplist(i2)%cp
    !this%rhoh = this%d * this%h
    return
  end subroutine ftp_refresh_thermal_properties_from_H
!==========================================================================================================
!==========================================================================================================
!> \brief Defination of a procedure in the type t_fluidThermoProperty.
!>  to update the thermal properties based on the known enthalpy per unit mass.     
!>
!> This subroutine is called as required to update all thermal properties from
!> the known enthalpy per unit mass (dimensionless only).
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[inout]  this          a cell element with udf property
!_______________________________________________________________________________
  subroutine ftp_refresh_thermal_properties_from_DH(this)
    type(t_fluidThermoProperty), intent(inout) :: this

    integer :: i1, i2, im
    real(WP) :: d1, dm
    real(WP) :: w1, w2

    if(is_ftplist_dim) call Print_error_msg("Error. Please provide undimentional variables.")


    if(this%rhoh < fluidparam%dhmin) then
      if(nrank == 0) then 
        write(*, wrtfmt2r) 'this%rhoh < fluidparam%dhmin', this%rhoh, fluidparam%dhmin
        call Print_error_msg("rho*h is out of range.")
      end if
      this%rhoh = fluidparam%dhmin
    else if(this%rhoh > fluidparam%dhmax) then
      if(nrank == 0) then 
        write(*, wrtfmt2r) 'this%rhoh > fluidparam%dhmax', this%rhoh, fluidparam%dhmax
        call Print_error_msg("rho*h is out of range.")
      end if
      this%rhoh = fluidparam%dhmax
    end if

    i1 = 1
    i2 = fluidparam%nlist

    do while ( (i2 - i1) > 1)
      im = i1 + (i2 - i1) / 2
      d1 = ftplist(i1)%rhoh - this%rhoh
      dm = ftplist(im)%rhoh - this%rhoh
      if ( (d1 * dm) > MINP ) then
        i1 = im
      else
        i2 = im
      end if
    end do

    w1 = (ftplist(i2)%rhoh - this%rhoh) / (ftplist(i2)%rhoh - ftplist(i1)%rhoh) 
    w2 = ONE - w1
    
    if(fluidparam%ipropertyState == IPROPERTY_TABLE) then 
      this%h = w1 * ftplist(i1)%h + w2 * ftplist(i2)%h
      call ftp_refresh_thermal_properties_from_H(this)
      this%h = this%rhoh / this%d
      call ftp_refresh_thermal_properties_from_H(this)
    else if (fluidparam%ipropertyState == IPROPERTY_FUNCS) then 
      this%t = w1 * ftplist(i1)%t + w2 * ftplist(i2)%t
      call ftp_refresh_thermal_properties_from_T_undim(this)
    else  
      STOP 'Error. No such option of ipropertyState.'
    end if
    return
  end subroutine ftp_refresh_thermal_properties_from_DH
!==========================================================================================================
!==========================================================================================================
!> \brief Defination of a procedure in the type t_fluidThermoProperty.
!>  to print out the thermal properties at the given element.     
!>
!> This subroutine is called as required to print out thermal properties 
!> for degbugging.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     this          a cell element with udf property
!> \param[in]     unit          
!> \param[in]     iotype        
!> \param[in]     v_list        
!> \param[out]    iostat        
!> \param[inout]  iomsg         
!_______________________________________________________________________________
  subroutine ftp_print(this, unit, iotype, v_list, iostat, iomsg)
    type(t_fluidThermoProperty), intent(in) :: this
    integer, intent(in)                 :: unit
    character(len = *), intent(in)      :: iotype
    integer, intent(in)                 :: v_list(:)
    integer, intent(out)                :: iostat
    character(len = *), intent(inout)   :: iomsg

    integer                             :: i_pass

    iostat = 0
    iomsg = ""
    
    do i_pass = 1, 1
      !write(unit, *, iostat = iostat, iomsg = iomsg) 'thermalProperty'
      !if(iostat /= 0) exit
      if(iotype(1:2) == 'DT' .and. len(iotype) > 2) &
        write(unit, *, iostat = iostat, iomsg = iomsg) iotype(3:)
      if(iostat /= 0) exit

      write(unit, *, iostat = iostat, iomsg = iomsg) &
      this%h, this%t, this%d, this%m, this%k, this%cp, this%b, this%rhoh
      
      if(iostat /= 0) exit
    end do 

    if(iostat /= 0) then
      write (*, "(A)") "print error : " // trim(iomsg)
      write (*, "(A, I0)") "  iostat : ", iostat
    end if
    return
  end subroutine ftp_print
!==========================================================================================================
!> \brief Sort out the user given thermal property table based on the temperature
!>  from small to big.
!>
!> This subroutine is called locally once reading in the given thermal table.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]  list         the thermal table element array
!==========================================================================================================
  subroutine ftplist_sort_t_small2big
    integer :: i, n, k
    real(WP) :: buf

    n = fluidparam%nlist

    do i = 1, n
      k = minloc( ftplist(i:n)%t, dim = 1) + i - 1

      buf = ftplist(i)%t
      ftplist(i)%t = ftplist(k)%t
      ftplist(k)%t = buf

      buf = ftplist(i)%d
      ftplist(i)%d = ftplist(k)%d
      ftplist(k)%d = buf

      buf = ftplist(i)%m
      ftplist(i)%m = ftplist(k)%m
      ftplist(k)%m = buf

      buf = ftplist(i)%k
      ftplist(i)%k = ftplist(k)%k
      ftplist(k)%k = buf

      buf = ftplist(i)%b
      ftplist(i)%b = ftplist(k)%b
      ftplist(k)%b = buf

      buf = ftplist(i)%cp
      ftplist(i)%cp = ftplist(k)%cp
      ftplist(k)%cp = buf

      buf = ftplist(i)%h
      ftplist(i)%h = ftplist(k)%h
      ftplist(k)%h = buf

      buf = ftplist(i)%rhoh
      ftplist(i)%rhoh = ftplist(k)%rhoh
      ftplist(k)%rhoh = buf

    end do

    return
  end subroutine ftplist_sort_t_small2big
!==========================================================================================================
!==========================================================================================================
!> \brief Check the monotonicity of the $\rho h$ along $h$ and $T$
!>
!> This subroutine is called locally once building up the thermal property
!> relationships. Non-monotonicity could happen in fluids at supercritical
!> pressure when inproper reference temperature is given.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[inout]  none          NA
!_______________________________________________________________________________
  subroutine ftplist_check_monotonicity_DH_of_HorT
    integer :: i
    real(WP) :: ddh1, dt1, dh1
    real(WP) :: ddh2, dt2, dh2
    real(WP) :: ddt, ddh

    if(nrank /= 0 ) return
    do i = 2, fluidparam%nlist - 1
        ddh1 = ftplist(i)%rhoh - ftplist(i - 1)%rhoh
        dt1  = ftplist(i)%t  - ftplist(i - 1)%t
        dh1  = ftplist(i)%h  - ftplist(i - 1)%h

        ddh2 = ftplist(i + 1)%rhoh - ftplist(i)%rhoh
        dt2  = ftplist(i + 1)%t  - ftplist(i)%t
        dh2  = ftplist(i + 1)%h  - ftplist(i)%h

        ddt = ddh1 / dt1 * ddh2 / dt2 
        ddh = ddh1 / dh1 * ddh2 / dh2

        if (ddt < MINP .and. nrank == 0) then
          call Print_warning_msg('The relation (rho * h) = FUNCTION (T) is not monotonicity.') 
          write(*, wrtfmt1r) ' This occurs from T(K) = ', ftplist(i)%t * fluidparam%ftp0ref%t
          call Print_warning_msg('If this temperature locates in-between your interested range, please try to increase your reference temeprature.')
        end if
        if (ddh < MINP .and. nrank == 0) then
          call Print_warning_msg('The relation (rho * h) = FUNCTION (H) is not monotonicity.') 
          write(*, wrtfmt1r) ' This occurs from H(J/KG) = ', \
          ftplist(i)%h  * fluidparam%ftp0ref%t * fluidparam%ftp0ref%cp + fluidparam%ftp0ref%h
          call Print_warning_msg('If this H locates in-between your interested range, please try to increase your reference temeprature.')
        end if

    end do
    return
  end subroutine ftplist_check_monotonicity_DH_of_HorT
!==========================================================================================================
!> \brief Building up the thermal property relations from the given table.
!! This subroutine is called once after reading the table.
!! [mpi] all ranks
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[in]     ref_T0          reference temperature
!==========================================================================================================
  subroutine buildup_property_relations_from_table
    integer, parameter :: IOMSG_LEN = 200
    character(len = IOMSG_LEN) :: iotxt
    integer :: ioerr, inputUnit
    character(len = 80) :: str
    real(WP) :: rtmp
    integer :: i
    !----------------------------------------------------------------------------------------------------------
    ! to read given table of thermal properties, dimensional
    !----------------------------------------------------------------------------------------------------------
    open ( newunit = inputUnit,     &
           file    = fluidparam%inputProperty, &
           status  = 'old',         &
           action  = 'read',        &
           iostat  = ioerr,         &
           iomsg   = iotxt)
    if(ioerr /= 0) then
      !write (*, *) 'Problem openning : ', fluidparam%inputProperty, ' for reading.'
      !write (*, *) 'Message: ', trim (iotxt)
      error stop 'Problem openning fluidparam%inputProperty for reading.'
    end if

    fluidparam%nlist = 0
    read(inputUnit, *, iostat = ioerr) str
    do
      read(inputUnit, *, iostat = ioerr) rtmp, rtmp, rtmp, rtmp, &
      rtmp, rtmp, rtmp, rtmp
      if(ioerr /= 0) exit
      fluidparam%nlist = fluidparam%nlist + 1
    end do
    rewind(inputUnit)
    !----------------------------------------------------------------------------------------------------------
    ! to read given table of thermal properties, dimensional
    !----------------------------------------------------------------------------------------------------------
    allocate ( ftplist (fluidparam%nlist) )

    read(inputUnit, *, iostat = ioerr) str
    do i = 1, fluidparam%nlist
      read(inputUnit, *, iostat = ioerr) rtmp, ftplist(i)%h, ftplist(i)%t, ftplist(i)%d, &
      ftplist(i)%m, ftplist(i)%k, ftplist(i)%cp, ftplist(i)%b
      ftplist(i)%rhoh = ftplist(i)%d * ftplist(i)%h
    end do
    close(inputUnit)
    !----------------------------------------------------------------------------------------------------------
    ! to sort input date (dimensional) based on Temperature (small to big)
    !----------------------------------------------------------------------------------------------------------
    call ftplist_sort_t_small2big 
    !----------------------------------------------------------------------------------------------------------
    ! to update reference of thermal properties
    !----------------------------------------------------------------------------------------------------------
    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftp0ref)
    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftpini)
    !----------------------------------------------------------------------------------------------------------
    ! to unify/undimensionalize the table of thermal property
    !----------------------------------------------------------------------------------------------------------
    do i = 1, fluidparam%nlist
      ftplist(i)%t  = ftplist(i)%t  / fluidparam%ftp0ref%t
      ftplist(i)%d  = ftplist(i)%d  / fluidparam%ftp0ref%d
      ftplist(i)%m  = ftplist(i)%m  / fluidparam%ftp0ref%m
      ftplist(i)%k  = ftplist(i)%k  / fluidparam%ftp0ref%k
      ftplist(i)%b  = ftplist(i)%b  / fluidparam%ftp0ref%b
      ftplist(i)%cp = ftplist(i)%cp / fluidparam%ftp0ref%cp
      ftplist(i)%h  = (ftplist(i)%h - fluidparam%ftp0ref%h) / fluidparam%ftp0ref%t / fluidparam%ftp0ref%cp
      ftplist(i)%rhoh = ftplist(i)%d * ftplist(i)%h
    end do

    call ftplist_check_monotonicity_DH_of_HorT

    i = minloc( ftplist(1:fluidparam%nlist)%rhoh, dim = 1)
    fluidparam%dhmin = ftplist(i)%rhoh ! undim
    i = maxloc( ftplist(1:fluidparam%nlist)%rhoh, dim = 1)
    fluidparam%dhmax = ftplist(i)%rhoh ! undim

    is_ftplist_dim = .false.

    return
  end subroutine buildup_property_relations_from_table
!==========================================================================================================
  subroutine ftp_convert_undim_to_dim(ftp_undim, ftp_dim)
    type(t_fluidThermoProperty), intent(in) :: ftp_undim
    type(t_fluidThermoProperty), intent(out) :: ftp_dim

    ftp_dim%t  = ftp_undim%t  * fluidparam%ftp0ref%t
    ftp_dim%d  = ftp_undim%d  * fluidparam%ftp0ref%d
    ftp_dim%m  = ftp_undim%m  * fluidparam%ftp0ref%m
    ftp_dim%k  = ftp_undim%k  * fluidparam%ftp0ref%k
    ftp_dim%b  = ftp_undim%b  * fluidparam%ftp0ref%b
    ftp_dim%cp = ftp_undim%cp * fluidparam%ftp0ref%cp
    ftp_dim%h  = ftp_undim%h  * fluidparam%ftp0ref%t * fluidparam%ftp0ref%cp + fluidparam%ftp0ref%h
    ftp_dim%rhoh = ftp_dim%d    * ftp_dim%h
    return
  end subroutine
!==========================================================================================================
!==========================================================================================================

!> \brief Building up the thermal property relations from defined relations.
!>
!> This subroutine is called once after defining the relations.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]  none          NA
!==========================================================================================================
  subroutine buildup_property_relations_from_function
    integer :: i

    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftp0ref)
    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftpini)
    
    fluidparam%nlist = N_FUNC2TABLE
    allocate ( ftplist (fluidparam%nlist) )
    do i = 1, fluidparam%nlist
      ftplist(i)%t = ( fluidparam%TM0 + (fluidparam%Tb0 - fluidparam%TM0) * real(i, WP) / real(fluidparam%nlist, WP) ) &
                     / fluidparam%ftp0ref%t ! undimensional
      call ftp_refresh_thermal_properties_from_T_undim(ftplist(i))
    end do

    call ftplist_check_monotonicity_DH_of_HorT

    i = minloc( ftplist(1:fluidparam%nlist)%rhoh, dim = 1)
    fluidparam%dhmin = ftplist(i)%rhoh
    i = maxloc( ftplist(1:fluidparam%nlist)%rhoh, dim = 1)
    fluidparam%dhmax = ftplist(i)%rhoh

    is_ftplist_dim = .false.

    return
  end subroutine buildup_property_relations_from_function
!==========================================================================================================
!==========================================================================================================
!> \brief Write out the rebuilt thermal property relations.
!>
!> This subroutine is called for testing.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[inout]  none          NA
!_______________________________________________________________________________
  subroutine Write_thermo_property
    type(t_fluidThermoProperty) :: ftp
    type(t_fluidThermoProperty) :: ftp_dim
    integer :: n, i
    real(WP) :: dhmax, dhmin
    integer :: ftp_unit1, ftp_unit2

    if (nrank /= 0) return

    ! dhmin = MAXP
    ! dhmax = MINP
    ! if(fluidparam%ipropertyState == IPROPERTY_TABLE) then
    !   do i = 1, fluidparam%nlist
    !     if(ftplist(i)%rhoh < dhmin) dhmin = ftplist(i)%rhoh
    !     if(ftplist(i)%rhoh > dhmax) dhmax = ftplist(i)%rhoh
    !   end do
    !   !dhmin = dhmin + TRUNCERR
    !   !dhmax = dhmax - TRUNCERR

    ! else if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then 
    !   ftp%t  = fluidparam%TB0 / fluidparam%ftp0ref%t
    !   call ftp_refresh_thermal_properties_from_T_undim(ftp)
    !   dhmin1 = ftp%rhoh

    !   ftp%t  = fluidparam%TM0 / fluidparam%ftp0ref%t
    !   call ftp_refresh_thermal_properties_from_T_undim(ftp)
    !   dhmax1 = ftp%rhoh
      
    !   dhmin = dmin1( dhmin1, dhmax1) + TRUNCERR
    !   dhmax = dmax1( dhmin1, dhmax1) - TRUNCERR

    ! else
    !   dhmin = MAXP
    !   dhmax = MINP
    ! end if
    
    open (newunit = ftp_unit1, file = 'check_ftplist_undim.dat')
    write(ftp_unit1, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Cp, Texpansion B, rho*h'
    open (newunit = ftp_unit2, file = 'check_ftplist_dim.dat')
    write(ftp_unit2, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Cp, Texpansion B, rho*h'

    do i = 1, fluidparam%nlist
      write(ftp_unit1, '(8ES13.5)') ftplist(i)%h, ftplist(i)%t, ftplist(i)%d, ftplist(i)%m, ftplist(i)%k, ftplist(i)%cp, ftplist(i)%b, ftplist(i)%rhoh
      call ftp_convert_undim_to_dim(ftplist(i), ftp_dim)  
      write(ftp_unit2, '(8ES13.5)') ftp_dim%h, ftp_dim%t, ftp_dim%d, ftp_dim%m, ftp_dim%k, ftp_dim%cp, ftp_dim%b, ftp_dim%rhoh
    end do
    close (ftp_unit1)
    close (ftp_unit2)


    n = 128
    dhmin = fluidparam%dhmin + TRUNCERR
    dhmax = fluidparam%dhmax - TRUNCERR
    open (newunit = ftp_unit1, file = 'check_ftp_from_dh_undim.dat')
    write(ftp_unit1, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Cp, Texpansion B, rho*h'
    open (newunit = ftp_unit2, file = 'check_ftp_from_dh_dim.dat')
    write(ftp_unit2, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Cp, Texpansion B, rho*h'
    do i = 1, n
      ftp%rhoh = dhmin + (dhmax - dhmin) * real(i - 1, WP) / real(n - 1, WP)
      !write(*,*) ftp%rhoh
      call ftp_refresh_thermal_properties_from_DH(ftp)
      call ftp_is_T_in_scope(ftp)
      write(ftp_unit1, '(8ES13.5)') ftp%h, ftp%t, ftp%d, ftp%m, ftp%k, ftp%cp, ftp%b, ftp%rhoh
      call ftp_convert_undim_to_dim(ftp, ftp_dim)  
      write(ftp_unit2, '(8ES13.5)') ftp_dim%h, ftp_dim%t, ftp_dim%d, ftp_dim%m, ftp_dim%k, ftp_dim%cp, ftp_dim%b, ftp_dim%rhoh
    end do
    close (ftp_unit1)
    close (ftp_unit2)

    if (nrank == 0 ) then
      call Print_debug_start_msg("The reference thermal properties (dim) are")
      write (*, wrtfmt1r) '  Temperature(K):',              fluidparam%ftp0ref%t
      write (*, wrtfmt1r) '  Density(Kg/m3):',              fluidparam%ftp0ref%d
      write (*, wrtfmt1e) '  Dynamic Viscosity(Pa-s):',     fluidparam%ftp0ref%m
      write (*, wrtfmt1r) '  Thermal Conductivity(W/m-K):', fluidparam%ftp0ref%k
      write (*, wrtfmt1r) '  Cp(J/Kg/K):',                  fluidparam%ftp0ref%cp
      write (*, wrtfmt1e) '  Enthalphy(J):',                fluidparam%ftp0ref%h
      write (*, wrtfmt1e) '  mass enthaphy(Kg J/m3):',      fluidparam%ftp0ref%rhoh

      call Print_debug_start_msg("The initial thermal properties (dim) are")
      write (*, wrtfmt1r) '  Temperature(K):',              fluidparam%ftpini%t
      write (*, wrtfmt1r) '  Density(Kg/m3):',              fluidparam%ftpini%d
      write (*, wrtfmt1e) '  Dynamic Viscosity(Pa-s):',     fluidparam%ftpini%m
      write (*, wrtfmt1r) '  Thermal Conductivity(W/m-K):', fluidparam%ftpini%k
      write (*, wrtfmt1r) '  Cp(J/Kg/K):',                  fluidparam%ftpini%cp
      write (*, wrtfmt1e) '  Enthalphy(J):',                fluidparam%ftpini%h
      write (*, wrtfmt1e) '  mass enthaphy(Kg J/m3):',      fluidparam%ftpini%rhoh
      call Print_debug_mid_msg("The range of the property table (undim)")
      write (*, wrtfmt2r) '  rho*h(Kg J/m3)',                  fluidparam%dhmin, fluidparam%dhmax
    end if

    return
  end subroutine Write_thermo_property
!==========================================================================================================
!> \brief Identify table or equations for thermal properties based on input
!>  fluid material.
!>
!> This subroutine is called once in setting up thermal relations.
!> [mpi] all ranks
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]  none          NA
!==========================================================================================================
  subroutine Buildup_fluidparam(tm)
    type(t_thermo), intent(in) :: tm

    if(nrank == 0) call Print_debug_start_msg("initialising thermal parameters ...")

    is_ftplist_dim = .true.
    fluidparam%ifluid    = tm%ifluid
    fluidparam%ftp0ref%t = tm%ref_T0  ! dim
    fluidparam%ftpini%t  = tm%init_T0 ! dim

    !----------------------------------------------------------------------------------------------------------
    ! get given file name or coefficients 
    !----------------------------------------------------------------------------------------------------------
    select case (fluidparam%ifluid)
    case (ISCP_WATER)
      fluidparam%ipropertyState = IPROPERTY_TABLE
      fluidparam%inputProperty = TRIM(INPUT_SCP_WATER)

    case (ISCP_CO2)
      fluidparam%ipropertyState = IPROPERTY_TABLE
      fluidparam%inputProperty = TRIM(INPUT_SCP_CO2)

    case (ILIQUID_SODIUM)
      fluidparam%nlist = N_FUNC2TABLE
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_Na
      fluidparam%TB0 = TB0_Na
      fluidparam%HM0 = HM0_Na
      fluidparam%CoD(0:1) = CoD_Na(0:1)
      fluidparam%CoK(0:2) = CoK_Na(0:2)
      fluidparam%CoB = CoB_Na
      fluidparam%CoCp(-2:2) = CoCp_Na(-2:2)
      fluidparam%CoH(-1:3) = CoH_Na(-1:3)
      fluidparam%CoM(-1:1) = CoM_Na(-1:1)

    case (ILIQUID_LEAD)
      fluidparam%nlist = N_FUNC2TABLE
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_Pb
      fluidparam%TB0 = TB0_Pb
      fluidparam%HM0 = HM0_Pb
      fluidparam%CoD(0:1) = CoD_Pb(0:1)
      fluidparam%CoK(0:2) = CoK_Pb(0:2)
      fluidparam%CoB = CoB_Pb
      fluidparam%CoCp(-2:2) = CoCp_Pb(-2:2)
      fluidparam%CoH(-1:3) = CoH_Pb(-1:3)
      fluidparam%CoM(-1:1) = CoM_Pb(-1:1)

    case (ILIQUID_BISMUTH)
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_BI
      fluidparam%TB0 = TB0_BI
      fluidparam%HM0 = HM0_BI
      fluidparam%CoD(0:1) = CoD_BI(0:1)
      fluidparam%CoK(0:2) = CoK_BI(0:2)
      fluidparam%CoB = CoB_BI
      fluidparam%CoCp(-2:2) = CoCp_BI(-2:2)
      fluidparam%CoH(-1:3) = CoH_BI(-1:3)
      fluidparam%CoM(-1:1) = CoM_BI(-1:1)

    case (ILIQUID_LBE)
      fluidparam%nlist = N_FUNC2TABLE
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_LBE
      fluidparam%TB0 = TB0_LBE
      fluidparam%HM0 = HM0_LBE
      fluidparam%CoD(0:1) = CoD_LBE(0:1)
      fluidparam%CoK(0:2) = CoK_LBE(0:2)
      fluidparam%CoB = CoB_LBE
      fluidparam%CoCp(-2:2) = CoCp_LBE(-2:2)
      fluidparam%CoH(-1:3) = CoH_LBE(-1:3)
      fluidparam%CoM(-1:1) = CoM_LBE(-1:1)

    case default
      fluidparam%nlist = N_FUNC2TABLE
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_Na
      fluidparam%TB0 = TB0_Na
      fluidparam%HM0 = HM0_Na
      fluidparam%CoD(0:1) = CoD_Na(0:1)
      fluidparam%CoK(0:2) = CoK_Na(0:2)
      fluidparam%CoB = CoB_Na
      fluidparam%CoCp(-2:2) = CoCp_Na(-2:2)
      fluidparam%CoH(-1:3) = CoH_Na(-1:3)
      fluidparam%CoM(-1:1) = CoM_Na(-1:1)
    end select


    if(nrank == 0) call Print_debug_end_msg
    return
  end subroutine Buildup_fluidparam

!==========================================================================================================
  subroutine Convert_thermal_input_2undim (tm, dm)
    type(t_domain),   intent(inout) :: dm
    type(t_thermo),   intent(inout) :: tm

    character(16) :: filename1 = 'pf1d_T1y_dim.dat'
    character(18) :: filename2 = 'pf1d_T1y_undim.dat'
    integer :: n, i
    integer, parameter :: IOMSG_LEN = 200
    character(len = IOMSG_LEN) :: iotxt
    integer :: ioerr, inputUnit, outputUnit
    character(len = 80) :: str
    
    real(WP) :: rtmp1, rtmp2

    
    if(.not. dm%is_thermo) return

    !----------------------------------------------------------------------------------------------------------
    !   for x-pencil
    !   scale the given thermo b.c. in dimensional to undimensional
    !----------------------------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------------------------
    ! x-bc
    !----------------------------------------------------------------------------------------------------------
    do n = 1, 2
      if( dm%ibcx_Tm(n) == IBC_DIRICHLET ) then
        dm%fbcx_const(n, 5) = dm%fbcx_const(n, 5)/tm%ref_T0  ! undim
      end if
      if (dm%ibcx_Tm(n) == IBC_NEUMANN) then
        dm%fbcx_const(n, 5) = dm%fbcx_const(n, 5) * tm%ref_l0 / fluidparam%ftp0ref%k / fluidparam%ftp0ref%t 
      end if
    end do 

    !----------------------------------------------------------------------------------------------------------
    ! y-bc
    !----------------------------------------------------------------------------------------------------------
    do n = 1, 2
      if( dm%ibcy_Tm(n) == IBC_DIRICHLET ) then
        dm%fbcy_const(n, 5) = dm%fbcy_const(n, 5)/tm%ref_T0  ! undim
      end if
      if (dm%ibcy_Tm(n) == IBC_NEUMANN) then
        dm%fbcy_const(n, 5) = dm%fbcy_const(n, 5) * tm%ref_l0 / fluidparam%ftp0ref%k / fluidparam%ftp0ref%t 
      end if
    end do 
    !----------------------------------------------------------------------------------------------------------
    ! z-bc
    !----------------------------------------------------------------------------------------------------------
    do n = 1, 2
      if( dm%ibcz_Tm(n) == IBC_DIRICHLET ) then
        dm%fbcz_const(n, 5) = dm%fbcz_const(n, 5)/tm%ref_T0  ! undim
      end if
      if (dm%ibcz_Tm(n) == IBC_NEUMANN) then
        dm%fbcz_const(n, 5) = dm%fbcz_const(n, 5) * tm%ref_l0 / fluidparam%ftp0ref%k / fluidparam%ftp0ref%t 
      end if
    end do 

    !
    if(dm%ibcx_Tm(1) == IBC_PROFILE1D .and. nrank == 0) then
      open ( newunit = inputUnit,     &
           file    = trim(filename1), &
           status  = 'old',         &
           action  = 'read',        &
           iostat  = ioerr,         &
           iomsg   = iotxt)
      open ( newunit = outputUnit,     &
           file    = trim(filename2), &
           status  = 'new',         &
           action  = 'write',        &
           iostat  = ioerr,         &
           iomsg   = iotxt)
      if(ioerr /= 0) then
        write(*,*) 'Problem opening: ', trim(filename1)
        error stop
      end if

      n = 0
      read(inputUnit, *, iostat = ioerr) str

      do
        read(inputUnit, *, iostat = ioerr) rtmp1, rtmp2
        if(ioerr /= 0) exit
        n = n + 1
      end do
      rewind(inputUnit)

      read(inputUnit, *, iostat = ioerr) str
      do i = 1, n
        read(inputUnit, *, iostat = ioerr)   rtmp1, rtmp2
        write(outputUnit, *, iostat = ioerr) rtmp1, rtmp2/tm%ref_T0
      end do
      close(inputUnit)
      close(outputUnit)
    end if
    
    return
  end subroutine

!==========================================================================================================
!> \brief Initialise thermal variables if ithermo = 1.     
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi    called-freq    xdomain     module
!>         all    once           specified   private
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]  fl   flow type
!> \param[inout]  tm   thermo type
!==========================================================================================================
  subroutine initialise_thermal_properties (fl, tm)
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    
    if(nrank == 0) call Print_debug_mid_msg("initialise thermal variables ...")
    !----------------------------------------------------------------------------------------------------------
    !   initialise thermal fields
    !----------------------------------------------------------------------------------------------------------
    if(nrank == 0) then
      call Print_debug_start_msg("The initial thermal properties (undim) are")
      write (*, wrtfmt1r) '  Temperature:',          tm%ftp_ini%t
      write (*, wrtfmt1r) '  Density:',              tm%ftp_ini%d
      write (*, wrtfmt1r) '  Dynamic Viscosity:',    tm%ftp_ini%m
      write (*, wrtfmt1r) '  Thermal Conductivity:', tm%ftp_ini%k
      write (*, wrtfmt1r) '  Cp:',                   tm%ftp_ini%cp
      write (*, wrtfmt1r) '  Enthalphy:',            tm%ftp_ini%h
      write (*, wrtfmt1r) '  mass enthaphy:',        tm%ftp_ini%rhoh
    end if

    fl%dDens(:, :, :) = tm%ftp_ini%d
    fl%mVisc(:, :, :) = tm%ftp_ini%m
    tm%rhoh (:, :, :) = tm%ftp_ini%rhoh
    tm%hEnth(:, :, :) = tm%ftp_ini%h
    tm%kCond(:, :, :) = tm%ftp_ini%k
    tm%tTemp(:, :, :) = tm%ftp_ini%t

    fl%dDensm2(:, :, :) = fl%dDensm1(:, :, :)
    fl%dDensm1(:, :, :) = fl%dDens(:, :, :)

    

    if(nrank == 0) call Print_debug_end_msg
    return
  end subroutine initialise_thermal_properties

!==========================================================================================================
!> \brief Initialise thermal variables if ithermo = 1.     
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi    called-freq    xdomain     module
!>         all    once           specified   private
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[inout]  fl   flow type
!> \param[inout]  tm   thermo type
!==========================================================================================================
!   subroutine get_bc_tdm (dm) ! apply once
!     use parameters_constant_mod
!     implicit none
!     type(t_flow),   intent(inout) :: fl
!     type(t_thermo), intent(inout) :: tm
    
!     type(t_fluidThermoProperty) :: ftpx, ftpy, ftpz

!     do k = 1, size(dm%fbcx_var, 3)
!       do j = 1, size(dm%fbcx_var, 2)
!         do i = 1, size(dm%fbcx_var, 1)
! !----------------------------------------------------------------------------------------------------------
! !         update density and viscousity at b.c.
! !         for temperature bc, heat flux bc (to chdck)
! !----------------------------------------------------------------------------------------------------------
!           ftpx%t = dm%fbcx_var(i, j, k, 5)
!           ftpy%t = dm%fbcy_var(i, j, k, 5)
!           ftpz%t = dm%fbcz_var(i, j, k, 5)
!           call ftp_refresh_thermal_properties_from_T_undim(ftpx)
!           call ftp_refresh_thermal_properties_from_T_undim(ftpy)
!           call ftp_refresh_thermal_properties_from_T_undim(ftpz)

!           dm%fbcx_var(i, j, k, 9)  = ftpx%d
!           dm%fbcx_var(i, j, k, 10) = ftpx%m
          
!         end do
!       end do
!     end do

!     return
!   end subroutine apply_bc_thermmal_properties

!==========================================================================================================
!==========================================================================================================
!> \brief The main code for thermal property initialisation.
!> Scope:  mpi    called-freq    xdomain
!>         all    once           all
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[inout]  none          NA
!_______________________________________________________________________________
  subroutine Buildup_thermo_mapping_relations(tm)
    type(t_thermo), intent(inout) :: tm

    if(nrank == 0) call Print_debug_start_msg("initialising thermal mapping relations ...")
    call Buildup_fluidparam(tm)
    if (fluidparam%ipropertyState == IPROPERTY_TABLE) call buildup_property_relations_from_table
    if (fluidparam%ipropertyState == IPROPERTY_FUNCS) call buildup_property_relations_from_function
    call Write_thermo_property
    tm%ftp_ini%t = tm%init_T0 / tm%ref_T0 ! already undim
    call ftp_refresh_thermal_properties_from_T_undim(tm%ftp_ini)

    if(nrank == 0) call Print_debug_end_msg
    return
  end subroutine Buildup_thermo_mapping_relations

end module thermo_info_mod



