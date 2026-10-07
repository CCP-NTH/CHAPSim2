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
!==============================================================================
!> Source file input_thermo.f90.
!>
!> Reading the input parameters from the given file and building up the
!> relationships between properties.
!>
!==============================================================================
module thermo_info_mod
  use mpi_mod
  use parameters_constant_mod
  use print_msg_mod
  use udf_type_mod
  use wtformat_mod
  implicit none

  integer, save :: N_FUNC2TABLE = 1024
  logical :: is_ftplist_dim
  type(t_fluidThermoProperty), save, allocatable, dimension(:) :: ftplist

  private :: buildup_property_relations_from_table
  private :: buildup_property_relations_from_function
  private :: ftplist_check_monotonicity_DH_of_HorT
  private :: ftplist_check_positive_properties
  private :: Buildup_fluidparam
  private :: restrict_property_range
  private :: check_T_in_property_range
  private :: ftplist_sort_t_small2big
  private :: Write_thermo_property

  private :: ftp_is_T_in_scope
  private :: ftp_get_thermal_properties_dimensional_from_T
  private :: ftp_refresh_thermal_properties_from_T_undim
  public  :: ftp_refresh_thermal_properties_from_T_undim_3Dftp
  public  :: ftp_refresh_thermal_properties_from_T_undim_3Dtm
  public :: ftp_refresh_thermal_properties_from_H
  public  :: ftp_refresh_thermal_properties_from_H_3Dftp
  private :: ftp_convert_undim_to_dim
  private :: ftp_print
  public  :: ftp_refresh_thermal_properties_from_DH
  public  :: ftp_refresh_thermal_properties_from_DH_3Dtm

  public  :: Buildup_thermo_mapping_relations
  public  :: initialise_thermal_properties
  public  :: Convert_thermal_input_2undim
  public  :: get_qw_ramp_factor

contains
!==============================================================================
!==============================================================================
!> Defination of a procedure in the type t_fluidThermoProperty.
!>  to check the temperature limitations.
!>
!> This subroutine is called as required to check the temperature
!> of given element is within the given limits as a single phase
!> flow.
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - this (inout): a cell element with udf property
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
        call Print_error_msg('temperature exceeds specified range.')
      end if
    end if

    if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then
      ! TP0min/TP0max, not TM0/TB0: a correlation is not guaranteed to hold over
      ! the whole liquid range, so the table interval can be narrower than
      ! melting-to-boiling. Name the property that binds the end being violated.
      if ( this%t < ( fluidparam%TP0min / fluidparam%ftp0ref%t ) ) then
        write(*, wrtfmt3r) 'this T, low T, high T:', this%t, fluidparam%TP0min / fluidparam%ftp0ref%t, fluidparam%TP0max / fluidparam%ftp0ref%t
        write(*, wrtfmt3r) 'this rhoh, low rhoh, high rhoh', this%rhoh, fluidparam%dhmin, fluidparam%dhmax
        call Print_error_msg('temperature is below the supported property range, set by '// &
                             trim(fluidparam%TP0minsrc)//'.')
      else if ( this%t > ( fluidparam%TP0max / fluidparam%ftp0ref%t ) ) then
        write(*, wrtfmt3r) 'this T, low T, high T:', this%t, fluidparam%TP0min / fluidparam%ftp0ref%t, fluidparam%TP0max / fluidparam%ftp0ref%t
        write(*, wrtfmt3r) 'this rhoh, low rhoh, high rhoh', this%rhoh, fluidparam%dhmin, fluidparam%dhmax
        call Print_error_msg('temperature is above the supported property range, set by '// &
                             trim(fluidparam%TP0maxsrc)//'.')
      end if
    end if

    return
  end subroutine
!==============================================================================
!==============================================================================
!> Defination of a procedure in the type t_fluidThermoProperty.
!>  to update the thermal properties based on the known temperature.
!>
!> This subroutine is called as required to update all thermal properties from
!> the known temperature (dimensional or dimensionless).
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - this (inout): a cell element with udf property
!> - dim (in): an optional indicator of dimensional
!>                              /dimensionless T. Exsiting of dim indicates
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
      this%sigma_e = w1 * ftplist(i1)%sigma_e + w2 * ftplist(i2)%sigma_e
      this%h = w1 * ftplist(i1)%h + w2 * ftplist(i2)%h
      this%b = w1 * ftplist(i1)%b + w2 * ftplist(i2)%b
      this%cp = w1 * ftplist(i1)%cp + w2 * ftplist(i2)%cp

    else if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then

      t1 = this%t

      ! D = density = f(T)
      select case (fluidparam%ifluid)
        case (ILIQUID_LITHIUM)
          this%d = fluidparam%CoD(0) + &
                   fluidparam%CoD(1) * t1 + &
                   fluidparam%CoD(2) * (1.0_WP - t1 / fluidparam%CoD(3))**(fluidparam%CoD(4))
        case default
          this%d = fluidparam%CoD(0) + &
                   fluidparam%CoD(1) * t1
      end select

      ! K = thermal conductivity = f(T)
      this%k = fluidparam%CoK(0) + &
               fluidparam%CoK(1) * t1 + &
               fluidparam%CoK(2) * t1**2

      ! Electrical conductivity = f(T)
      this%sigma_e = ONE

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

      ! B = thermal expansion coefficient = -(1 / rho) * drho/dT = f(T)
      ! The tabulated form 1 / (CoB - T) is that definition rewritten for a
      ! density that is linear in T: with rho = CoD(0) + CoD(1) * T it gives
      ! CoB = -CoD(0) / CoD(1) exactly. Li has a non-linear density, so it has
      ! to be differentiated instead.
      select case (fluidparam%ifluid)
        case (ILIQUID_LITHIUM)
          this%b = -( fluidparam%CoD(1) - fluidparam%CoD(2) * fluidparam%CoD(4) * &
                      (ONE - t1 / fluidparam%CoD(3))**(fluidparam%CoD(4) - ONE) / &
                      fluidparam%CoD(3) ) / this%d
        case default
          this%b = ONE / (fluidparam%CoB - t1)
      end select

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
        case (ILIQUID_LITHIUM)
          dummy = EXP( CoM_Li(-1) + CoM_Li(0) * LOG(t1) + (CoM_Li(1) / t1) )
        case (ILIQUID_FLIBE)
          dummy = CoM_FLiBe(0) * EXP (CoM_FLiBe(-1) / t1)
        case (ILIQUID_PBLI)
          ! Arrhenius, KfK-4144 (1986). CoM_PbLi(-1) is the activation temperature
          ! Ea/Ru, not a polynomial coefficient; see parameters_constant_mod.
          dummy = CoM_PbLi(0) * EXP (CoM_PbLi(-1) / t1)
        case default
          dummy = ZERO ! never used; Print_error_msg terminates the run
          call Print_error_msg('No dynamic-viscosity correlation is defined for this ifluid.')
      end select
      this%m = dummy
    else
      call Print_error_msg ("Error.")
    end if
    !
    this%rhoh = this%d * this%h
    this%alpha = this%k / this%d / this%cp
    this%Pr = this%m * this%cp /this%k
    !
    return
  end subroutine ftp_get_thermal_properties_dimensional_from_T
!==============================================================================
!==============================================================================
  subroutine ftp_refresh_thermal_properties_from_T_undim ( this )
    type(t_fluidThermoProperty), intent(inout) :: this

    type(t_fluidThermoProperty) :: ftp0ref
    integer :: i1, i2, im
    real(WP) :: d1, dm
    real(WP) :: w1, w2
    real(WP) :: t1, dummy
    real(WP) :: drho_dt, dh_dt

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
      this%sigma_e = w1 * ftplist(i1)%sigma_e + w2 * ftplist(i2)%sigma_e
      this%h = w1 * ftplist(i1)%h + w2 * ftplist(i2)%h
      this%b = w1 * ftplist(i1)%b + w2 * ftplist(i2)%b
      this%cp = w1 * ftplist(i1)%cp + w2 * ftplist(i2)%cp

    else if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then

      ftp0ref =fluidparam%ftp0ref
      ! convert undim to dim
      t1 = this%t * ftp0ref%t

      ! D = density = f(T)
      select case (fluidparam%ifluid)
        case (ILIQUID_LITHIUM)
          dummy = fluidparam%CoD(0) + &
                  fluidparam%CoD(1) * t1 + &
                  fluidparam%CoD(2) * (1.0_WP - t1 / fluidparam%CoD(3))**(fluidparam%CoD(4))
          this%d = dummy / ftp0ref%d
          drho_dt = (fluidparam%CoD(1) - fluidparam%CoD(2) * fluidparam%CoD(4) * &
                    (1.0_WP - t1 / fluidparam%CoD(3))**(fluidparam%CoD(4) - 1.0_WP) / &
                    fluidparam%CoD(3)) * ftp0ref%t / ftp0ref%d
        case default
          dummy = fluidparam%CoD(0) + &
                  fluidparam%CoD(1) * t1
          this%d = dummy / ftp0ref%d
          drho_dt = fluidparam%CoD(1) * ftp0ref%t / ftp0ref%d
      end select

      ! K = thermal conductivity = f(T)
      dummy = fluidparam%CoK(0) + &
              fluidparam%CoK(1) * t1 + &
              fluidparam%CoK(2) * t1**2
      this%k = dummy / ftp0ref%k

      ! Electrical conductivity = f(T)
      dummy = ONE
      this%sigma_e = dummy / ftp0ref%sigma_e

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
      dh_dt =        -fluidparam%CoH(-1) / t1**2 + &
                      fluidparam%CoH(1) +          &
                TWO * fluidparam%CoH(2) * t1 +     &
              THREE * fluidparam%CoH(3) * t1**2
      dh_dt = dh_dt / ftp0ref%cp
      ! B = f(T). See ftp_get_thermal_properties_dimensional_from_T for why Li
      ! cannot use the 1 / (CoB - T) form. drho_dt above is already divided by
      ! ftp0ref%d and multiplied by ftp0ref%t, so dividing by ftp0ref%t here
      ! returns a dimensional beta, as the default branch also produces.
      select case (fluidparam%ifluid)
        case (ILIQUID_LITHIUM)
          dummy = -drho_dt / (ftp0ref%t * this%d)
        case default
          dummy = ONE / (fluidparam%CoB - t1)
      end select
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
        case (ILIQUID_LITHIUM)
          dummy = EXP( CoM_Li(-1) + CoM_Li(0) * LOG(t1) + (CoM_Li(1) / t1) )
        case (ILIQUID_FLIBE)
          dummy = CoM_FLiBe(0) * EXP (CoM_FLiBe(-1) / t1)
        case (ILIQUID_PBLI)
          ! Arrhenius, KfK-4144 (1986). CoM_PbLi(-1) is the activation temperature
          ! Ea/Ru, not a polynomial coefficient; see parameters_constant_mod.
          dummy = CoM_PbLi(0) * EXP (CoM_PbLi(-1) / t1)
        case default
          dummy = ZERO ! never used; Print_error_msg terminates the run
          call Print_error_msg('No dynamic-viscosity correlation is defined for this ifluid.')
      end select
      this%m = dummy / ftp0ref%m
    else
      this%t  = ONE
      this%d  = ONE
      this%m  = ONE
      this%k  = ONE
      this%sigma_e = ONE
      this%cp = ONE
      this%b  = ONE
      this%h  = ZERO
    end if

    this%rhoh = this%d * this%h
    this%alpha = this%k / this%d / this%cp
    this%Pr = this%m * this%cp /this%k

    return
  end subroutine ftp_refresh_thermal_properties_from_T_undim
!==============================================================================
  subroutine ftp_refresh_thermal_properties_from_T_undim_3Dftp( ftp3d )
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
  end subroutine
  !==============================================================================
  subroutine ftp_refresh_thermal_properties_from_T_undim_3Dtm(fl, tm, dm)
    ! arguments
    type(t_domain), intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    ! local
    integer :: i, j, k
    type(t_fluidThermoProperty) :: ftp

    do k = 1, dm%dccc%xsz(3)
      do j = 1, dm%dccc%xsz(2)
        do i = 1, dm%dccc%xsz(1)
          ftp%t = tm%tTemp(i, j, k)
          call ftp_refresh_thermal_properties_from_T_undim(ftp)
          tm%rhoh (i, j, k) = ftp%rhoh
          tm%hEnth(i, j, k) = ftp%h
          tm%kCond(i, j, k) = ftp%k
          tm%eCond(i, j, k) = ftp%sigma_e
          fl%dDens(i, j, k) = ftp%d
          fl%mVisc(i, j, k) = ftp%m
        end do
      end do
    end do

    return
  end subroutine
!==============================================================================
  subroutine ftp_refresh_thermal_properties_from_DH_3Dtm(fl, tm, dm)
    ! arguments
    type(t_domain), intent(in) :: dm
    type(t_flow), intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    ! local
    integer :: i, j, k
    type(t_fluidThermoProperty) :: ftp

    do k = 1, dm%dccc%xsz(3)
      do j = 1, dm%dccc%xsz(2)
        do i = 1, dm%dccc%xsz(1)
          ftp%rhoh = tm%rhoh(i, j, k)
          ftp%d = fl%dDens(i, j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          tm%tTemp(i, j, k) = ftp%t
          tm%hEnth(i, j, k) = ftp%h
          tm%kCond(i, j, k) = ftp%k
          tm%eCond(i, j, k) = ftp%sigma_e
          fl%dDens(i, j, k) = ftp%d
          fl%mVisc(i, j, k) = ftp%m
        end do
      end do
    end do

    return
  end subroutine
!==============================================================================
!==============================================================================
!> Defination of a procedure in the type t_fluidThermoProperty.
!>  to update the thermal properties based on the known enthalpy.
!>
!> This subroutine is called as required to update all thermal properties from
!> the known enthalpy (dimensionless only).
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - this (inout): a cell element with udf property
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
    this%sigma_e = w1 * ftplist(i1)%sigma_e + w2 * ftplist(i2)%sigma_e
    this%t  = w1 * ftplist(i1)%t  + w2 * ftplist(i2)%t
    this%b  = w1 * ftplist(i1)%b  + w2 * ftplist(i2)%b
    this%cp = w1 * ftplist(i1)%cp + w2 * ftplist(i2)%cp

    this%rhoh = this%d * this%h
    this%alpha = this%k / this%d / this%cp
    this%Pr = this%m * this%cp /this%k

    return
  end subroutine ftp_refresh_thermal_properties_from_H
!==============================================================================
  subroutine ftp_refresh_thermal_properties_from_H_3Dftp( ftp3d )
    type(t_fluidThermoProperty), intent(inout) :: ftp3d(:, :, :)
    integer :: i, j, k

    do i = 1, size(ftp3d, 1)
      do j = 1, size(ftp3d, 2)
        do k = 1, size(ftp3d, 3)
            call ftp_refresh_thermal_properties_from_H(ftp3d(i, j, k))
        end do
      end do
    end do
    return
  end subroutine
!==============================================================================
!==============================================================================
!> Defination of a procedure in the type t_fluidThermoProperty.
!>  to update the thermal properties based on the known enthalpy per unit mass.
!>
!> This subroutine is called as required to update all thermal properties from
!> the known enthalpy per unit mass (dimensionless only).
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - this (inout): a cell element with udf property
!_______________________________________________________________________________
  subroutine ftp_refresh_thermal_properties_from_DH(this)
    type(t_fluidThermoProperty), intent(inout) :: this

    integer :: i1, i2, im
    real(WP) :: d1, dm
    real(WP) :: w1, w2, res

    if(is_ftplist_dim) call Print_error_msg("Error. Please provide undimentional variables.")


    if(this%rhoh < fluidparam%dhmin) then
      if(nrank == 0) then
        write(*, wrtfmt2e) 'this%rhoh < fluidparam%dhmin', this%rhoh, fluidparam%dhmin
        call Print_error_msg("rho*h is out of range.")
      end if
      this%rhoh = fluidparam%dhmin
    else if(this%rhoh > fluidparam%dhmax) then
      if(nrank == 0) then
        write(*, wrtfmt2e) 'this%rhoh > fluidparam%dhmax', this%rhoh, fluidparam%dhmax
        call Print_error_msg("rho*h is out of range.")
      end if
      this%rhoh = fluidparam%dhmax
    end if

    if(fluidparam%ipropertyState == IPROPERTY_TABLE) then
      !this%h = w1 * ftplist(i1)%h + w2 * ftplist(i2)%h
      res = MAXP
      im = 1
      !do while (res > 1e-6_WP .or. im <= 5)
      do im = 1, 2
        !res = this%h
        this%h = this%rhoh / this%d
        !res = dabs(res - this%h)
        call ftp_refresh_thermal_properties_from_H(this)
        !im = im + 1
      end do
    else if (fluidparam%ipropertyState == IPROPERTY_FUNCS) then
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
      this%t = w1 * ftplist(i1)%t + w2 * ftplist(i2)%t
      call ftp_refresh_thermal_properties_from_T_undim(this)
    else
      call Print_error_msg('No such option of ipropertyState.')
    end if
    return
  end subroutine ftp_refresh_thermal_properties_from_DH
!==============================================================================
!==============================================================================
!> Defination of a procedure in the type t_fluidThermoProperty.
!>  to print out the thermal properties at the given element.
!>
!> This subroutine is called as required to print out thermal properties
!> for degbugging.
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - this (in): a cell element with udf property
!> - unit (in): !> - iotype (in): !> - v_list (in): !> - iostat (out): !> - iomsg (inout): !_______________________________________________________________________________
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
!==============================================================================
!> Sort out the user given thermal property table based on the temperature
!>  from small to big.
!>
!> This subroutine is called locally once reading in the given thermal table.
!>
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - list (inout): the thermal table element array
!==============================================================================
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

      buf = ftplist(i)%sigma_e
      ftplist(i)%sigma_e = ftplist(k)%sigma_e
      ftplist(k)%sigma_e = buf

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
!==============================================================================
!==============================================================================
!> Check the monotonicity of the rho h along h and T
!>
!> This subroutine is called locally once building up the thermal property
!> relationships. Non-monotonicity could happen in fluids at supercritical
!> pressure when inproper reference temperature is given.
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - none (inout): NA
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
          write(*, wrtfmt1e) ' This occurs from H(J/KG) = ', &
          ftplist(i)%h  * fluidparam%ftp0ref%t * fluidparam%ftp0ref%cp + fluidparam%ftp0ref%h
          call Print_warning_msg('If this H locates in-between your interested range, please try to increase your reference temeprature.')
        end if

    end do
    return
  end subroutine ftplist_check_monotonicity_DH_of_HorT
!==============================================================================
!> Check that every property in the freshly built table is physically positive.
!>
!> A polynomial correlation fitted over one temperature window can go negative
!> when it is evaluated outside that window, and nothing downstream notices: the
!> table is built once and then interpolated, so a negative density, viscosity,
!> conductivity or heat capacity would propagate into the solver as a plausible
!> looking number. This stops the run at the point where the bad value is
!> created, and names the fluid, the property and the temperature.
!> [mpi] all ranks, so every rank stops together.
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - none (inout): NA
!==============================================================================
  subroutine ftplist_check_positive_properties
    integer :: i
    character(len = 2) :: badname
    real(WP) :: badvalue, badt

    badname = ''
    do i = 1, fluidparam%nlist
      if    (ftplist(i)%d  <= ZERO) then
        badname = 'd '; badvalue = ftplist(i)%d  * fluidparam%ftp0ref%d
      else if(ftplist(i)%m  <= ZERO) then
        badname = 'm '; badvalue = ftplist(i)%m  * fluidparam%ftp0ref%m
      else if(ftplist(i)%k  <= ZERO) then
        badname = 'k '; badvalue = ftplist(i)%k  * fluidparam%ftp0ref%k
      else if(ftplist(i)%cp <= ZERO) then
        badname = 'cp'; badvalue = ftplist(i)%cp * fluidparam%ftp0ref%cp
      else
        cycle
      end if
      badt = ftplist(i)%t * fluidparam%ftp0ref%t
      exit
    end do

    if(trim(badname) /= '') then
      if(nrank == 0) then
        write(*, wrtfmt1i) 'ifluid = ',                   fluidparam%ifluid
        write(*, wrtfmt1r) 'T(K) of the bad value = ',     badt
        write(*, wrtfmt2r) 'property range T(K) low, high:', fluidparam%TP0min, fluidparam%TP0max
        write(*, wrtfmt2s) 'the low  end of that range is set by: ', trim(fluidparam%TP0minsrc)
        write(*, wrtfmt2s) 'the high end of that range is set by: ', trim(fluidparam%TP0maxsrc)
        write(*, wrtfmt1e) 'the non-physical value = ',    badvalue
      end if
      call Print_error_msg('The property correlation of '//trim(badname)// &
           ' is not positive over the whole property temperature range. '// &
           'Its fit is being extrapolated beyond where it holds.')
    end if

    return
  end subroutine ftplist_check_positive_properties
!==============================================================================
!> Building up the thermal property relations from the given table.
!! This subroutine is called once after reading the table.
!! [mpi] all ranks
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - ref_T0 (in): reference temperature
!==============================================================================
  subroutine buildup_property_relations_from_table
    use math_mod
    implicit none
    integer, parameter :: IOMSG_LEN = 200
    character(len = IOMSG_LEN) :: iotxt
    integer :: ioerr, inputUnit
    character(len = 80) :: str
    real(WP) :: rtmp
    integer :: i
    !------------------------------------------------------------------------------
    ! to read given table of thermal properties, dimensional
    !------------------------------------------------------------------------------
    open ( newunit = inputUnit,     &
           file    = fluidparam%inputProperty, &
           status  = 'old',         &
           action  = 'read',        &
           iostat  = ioerr,         &
           iomsg   = iotxt)
    if(ioerr /= 0) then
      !write (*, *) 'Problem openning : ', fluidparam%inputProperty, ' for reading.'
      !write (*, *) 'Message: ', trim (iotxt)
      call Print_error_msg('Problem openning fluidparam%inputProperty for reading.')
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
    !------------------------------------------------------------------------------
    ! to read given table of thermal properties, dimensional
    !------------------------------------------------------------------------------
    allocate ( ftplist (fluidparam%nlist) )

    read(inputUnit, *, iostat = ioerr) str
    do i = 1, fluidparam%nlist
      read(inputUnit, *, iostat = ioerr) rtmp, ftplist(i)%h, ftplist(i)%t, ftplist(i)%d, &
      ftplist(i)%m, ftplist(i)%k, ftplist(i)%cp, ftplist(i)%b
      ftplist(i)%sigma_e = ONE
      ftplist(i)%rhoh = ftplist(i)%d * ftplist(i)%h
    end do
    close(inputUnit)
    !------------------------------------------------------------------------------
    ! to sort input date (dimensional) based on Temperature (small to big)
    !------------------------------------------------------------------------------
    call ftplist_sort_t_small2big
    !------------------------------------------------------------------------------
    ! to update reference of thermal properties
    !------------------------------------------------------------------------------
    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftp0ref)
    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftpini)
    !------------------------------------------------------------------------------
    ! to unify/undimensionalize the table of thermal property
    !------------------------------------------------------------------------------
    do i = 1, fluidparam%nlist
      ftplist(i)%t  = ftplist(i)%t  / fluidparam%ftp0ref%t
      ftplist(i)%d  = ftplist(i)%d  / fluidparam%ftp0ref%d
      ftplist(i)%m  = ftplist(i)%m  / fluidparam%ftp0ref%m
      ftplist(i)%k  = ftplist(i)%k  / fluidparam%ftp0ref%k
      ftplist(i)%sigma_e = ftplist(i)%sigma_e / fluidparam%ftp0ref%sigma_e
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
!==============================================================================
  subroutine ftp_convert_undim_to_dim(ftp_undim, ftp_dim)
    type(t_fluidThermoProperty), intent(in) :: ftp_undim
    type(t_fluidThermoProperty), intent(out) :: ftp_dim

    ftp_dim%t  = ftp_undim%t  * fluidparam%ftp0ref%t
    ftp_dim%d  = ftp_undim%d  * fluidparam%ftp0ref%d
    ftp_dim%m  = ftp_undim%m  * fluidparam%ftp0ref%m
    ftp_dim%k  = ftp_undim%k  * fluidparam%ftp0ref%k
    ftp_dim%sigma_e = ftp_undim%sigma_e * fluidparam%ftp0ref%sigma_e
    ftp_dim%b  = ftp_undim%b  * fluidparam%ftp0ref%b
    ftp_dim%cp = ftp_undim%cp * fluidparam%ftp0ref%cp
    ftp_dim%h  = ftp_undim%h  * fluidparam%ftp0ref%t * fluidparam%ftp0ref%cp + fluidparam%ftp0ref%h
    ftp_dim%rhoh = ftp_dim%d    * ftp_dim%h
    return
  end subroutine
!==============================================================================
!==============================================================================

!> Building up the thermal property relations from defined relations.
!>
!> This subroutine is called once after defining the relations.
!>
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - none (inout): NA
!==============================================================================
  subroutine buildup_property_relations_from_function
    use math_mod
    implicit none
    integer :: i
    real(WP) :: rhoh(N_FUNC2TABLE), d(N_FUNC2TABLE)

    ! Reject a run whose reference or initial state sits outside the interval the
    ! correlations support, before anything is built from it -- and before the
    ! correlations are evaluated at all. Order matters: Li's density carries
    ! (1 - T / CoD(3))**CoD(4), so a T above CoD(3) = 3500 K raises a negative
    ! base to a fractional power, which the debug build's -ffpe-trap=invalid
    ! turns into a SIGFPE with no message. That is measured, not inferred: with
    ! these two calls moved below the two evaluations, lithium at ref_t0 = tini =
    ! 3600 K dies on signal 8 with no diagnostic, and in this order it stops on
    ! the range message instead. Both %t fields are set from input in
    ! Buildup_fluidparam, so the check needs nothing the evaluation produces.
    ! tests/tools/run_fluid_property_tests.py guards the ordering.
    ! Only these two states are checked here; a wall temperature or a developing
    ! field can still leave the interval, and the table inversion catches that.
    call check_T_in_property_range(fluidparam%ftp0ref%t, 'the reference temperature ref_t0')
    call check_T_in_property_range(fluidparam%ftpini%t,  'the initial temperature tini')

    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftp0ref)
    call ftp_get_thermal_properties_dimensional_from_T(fluidparam%ftpini)

    if(nrank == 0) then
      write(*, wrtfmt2r) 'property table T(K) range, low and high:', fluidparam%TP0min, fluidparam%TP0max
      write(*, wrtfmt2s) 'the low  end of that range is set by: ', trim(fluidparam%TP0minsrc)
      write(*, wrtfmt2s) 'the high end of that range is set by: ', trim(fluidparam%TP0maxsrc)
    end if

    fluidparam%nlist = N_FUNC2TABLE
    allocate ( ftplist (fluidparam%nlist) )
    do i = 1, fluidparam%nlist
      ftplist(i)%t = ( fluidparam%TP0min + (fluidparam%TP0max - fluidparam%TP0min) * real(i, WP) / real(fluidparam%nlist, WP) ) &
                     / fluidparam%ftp0ref%t ! undimensional
      call ftp_refresh_thermal_properties_from_T_undim(ftplist(i))
    end do

    call ftplist_check_positive_properties
    call ftplist_check_monotonicity_DH_of_HorT

    i = minloc( ftplist(1:fluidparam%nlist)%rhoh, dim = 1)
    fluidparam%dhmin = ftplist(i)%rhoh
    i = maxloc( ftplist(1:fluidparam%nlist)%rhoh, dim = 1)
    fluidparam%dhmax = ftplist(i)%rhoh

    is_ftplist_dim = .false.

    return
  end subroutine buildup_property_relations_from_function
!==============================================================================
!==============================================================================
!> Write out the rebuilt thermal property relations.
!>
!> This subroutine is called for testing.
!>
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - none (inout): NA
!_______________________________________________________________________________
  subroutine Write_thermo_property
    use io_files_mod
    implicit none
    !type(t_fluidThermoProperty) :: ftp
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

    open (newunit = ftp_unit1, file = trim(dir_chkp)//'/check_ftplist_undim.dat')
    write(ftp_unit1, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Econductivity sigma_e, Cp, Texpansion B, rho*h'
    open (newunit = ftp_unit2, file = trim(dir_chkp)//'/check_ftplist_dim.dat')
    write(ftp_unit2, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Econductivity sigma_e, Cp, Texpansion B, rho*h'

    do i = 1, fluidparam%nlist
      write(ftp_unit1, '(9ES13.5)') ftplist(i)%h, ftplist(i)%t, ftplist(i)%d, ftplist(i)%m, &
        ftplist(i)%k, ftplist(i)%sigma_e, ftplist(i)%cp, ftplist(i)%b, ftplist(i)%rhoh
      call ftp_convert_undim_to_dim(ftplist(i), ftp_dim)
      write(ftp_unit2, '(9ES13.5)') ftp_dim%h, ftp_dim%t, ftp_dim%d, ftp_dim%m, &
        ftp_dim%k, ftp_dim%sigma_e, ftp_dim%cp, ftp_dim%b, ftp_dim%rhoh
    end do
    close (ftp_unit1)
    close (ftp_unit2)


    n = 128
    dhmin = fluidparam%dhmin + MINP
    dhmax = fluidparam%dhmax - MINP
    !write(*, *) 'dhmin, dhmax', dhmin, dhmax
    ! open (newunit = ftp_unit1, file = trim(dir_chkp)//'/check_ftp_from_dh_undim.dat')
    ! write(ftp_unit1, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Cp, Texpansion B, rho*h, drhoh_drho'
    ! write(ftp_unit1, *) '# dhmax = ', dhmax, ' dhmin = ', dhmin
    ! open (newunit = ftp_unit2, file = trim(dir_chkp)//'/check_ftp_from_dh_dim.dat')
    ! write(ftp_unit2, *) '# Enthalpy H, Temperature T, Density D, DViscosity M, Tconductivity K, Cp, Texpansion B, rho*h'
    ! do i = 1, n
    !   ftp%rhoh = dhmin + (dhmax - dhmin) * real(i - 1, WP) / real(n - 1, WP)
    !   !write(*,*) ftp%rhoh
    !   call ftp_refresh_thermal_properties_from_DH(ftp)
    !   call ftp_is_T_in_scope(ftp)
    !   write(ftp_unit1, '(8ES13.5)') ftp%h, ftp%t, ftp%d, ftp%m, ftp%k, ftp%cp, ftp%b, ftp%rhoh, ftp%drhoh_drho
    !   call ftp_convert_undim_to_dim(ftp, ftp_dim)
    !   write(ftp_unit2, '(8ES13.5)') ftp_dim%h, ftp_dim%t, ftp_dim%d, ftp_dim%m, ftp_dim%k, ftp_dim%cp, ftp_dim%b, ftp_dim%rhoh
    ! end do
    ! close (ftp_unit1)
    ! close (ftp_unit2)

    if (nrank == 0 ) then
      call Print_debug_mid_msg("The range of the property table (undim)")
      write (*, wrtfmt2e) 'rho*h(Kg J/m3):',              fluidparam%dhmin, fluidparam%dhmax
      call Print_debug_mid_msg("The reference thermal properties (dimensional) are")
      write (*, wrtfmt1r) 'Temperature(K):',              fluidparam%ftp0ref%t
      write (*, wrtfmt1r) 'Density(Kg/m3):',              fluidparam%ftp0ref%d
      write (*, wrtfmt1e) 'Dynamic Viscosity(Pa-s):',     fluidparam%ftp0ref%m
      write (*, wrtfmt1r) 'Thermal Conductivity(W/m-K):', fluidparam%ftp0ref%k
      write (*, wrtfmt1r) 'Electrical Conductivity:',      fluidparam%ftp0ref%sigma_e
      write (*, wrtfmt1r) 'Cp(J/Kg/K):',                  fluidparam%ftp0ref%cp
      write (*, wrtfmt1e) 'Enthalphy(J):',                fluidparam%ftp0ref%h
      write (*, wrtfmt1e) 'mass enthaphy(Kg J/m3):',      fluidparam%ftp0ref%rhoh
      write (*, wrtfmt1e) 'thermal diffusivity(m2/s):',   fluidparam%ftp0ref%alpha
      write (*, wrtfmt1e) 'Prandtl Number:',              fluidparam%ftp0ref%Pr

      call Print_debug_mid_msg("The initial thermal properties (dimensional) are")
      write (*, wrtfmt1r) 'Temperature(K):',              fluidparam%ftpini%t
      write (*, wrtfmt1r) 'Density(Kg/m3):',              fluidparam%ftpini%d
      write (*, wrtfmt1e) 'Dynamic Viscosity(Pa-s):',     fluidparam%ftpini%m
      write (*, wrtfmt1r) 'Thermal Conductivity(W/m-K):', fluidparam%ftpini%k
      write (*, wrtfmt1r) 'Electrical Conductivity:',      fluidparam%ftpini%sigma_e
      write (*, wrtfmt1r) 'Cp(J/Kg/K):',                  fluidparam%ftpini%cp
      write (*, wrtfmt1e) 'Enthalphy(J):',                fluidparam%ftpini%h
      write (*, wrtfmt1e) 'mass enthaphy(Kg J/m3):',      fluidparam%ftpini%rhoh
      write (*, wrtfmt1e) 'thermal diffusivity(m2/s):',   fluidparam%ftpini%alpha
      write (*, wrtfmt1e) 'Prandtl Number:',              fluidparam%ftpini%Pr
    end if

    return
  end subroutine Write_thermo_property
!==============================================================================
!> Identify table or equations for thermal properties based on input
!>  fluid material.
!>
!> This subroutine is called once in setting up thermal relations.
!> [mpi] all ranks
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - none (inout): NA
!==============================================================================
  subroutine Buildup_fluidparam(tm)
    type(t_thermo), intent(in) :: tm
    logical :: exist

    if(nrank == 0) call Print_debug_inline_msg("Initialising thermal parameters ...")

    is_ftplist_dim = .true.
    fluidparam%ifluid    = tm%ifluid
    fluidparam%ftp0ref%t = tm%ref_T0  ! dim
    fluidparam%ftpini%t  = tm%init_T0 ! dim

    !------------------------------------------------------------------------------
    ! get given file name or coefficients
    !------------------------------------------------------------------------------
    select case (fluidparam%ifluid)
    case (ISCP_WATER)
      fluidparam%ipropertyState = IPROPERTY_TABLE
      inquire(file = trim(INPUT_SCP_WATER), exist = exist)
      if(.not. exist) call Print_error_msg("NPUT_SCP_WATER does not exist!")
      fluidparam%inputProperty = TRIM(INPUT_SCP_WATER)

    case (ISCP_CO2)
      fluidparam%ipropertyState = IPROPERTY_TABLE
      inquire(file = trim(INPUT_SCP_CO2), exist = exist)
      if(.not. exist) call Print_error_msg("INPUT_SCP_CO2 does not exist!")
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

      case (ILIQUID_LITHIUM)
      fluidparam%nlist = N_FUNC2TABLE
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_Li
      fluidparam%TB0 = TB0_Li
      fluidparam%HM0 = HM0_Li
      fluidparam%CoD(0:4) = CoD_Li(0:4)
      fluidparam%CoK(0:2) = CoK_Li(0:2)
      fluidparam%CoB = CoB_Li
      fluidparam%CoCp(-2:2) = CoCp_Li(-2:2)
      fluidparam%CoH(-1:3) = CoH_Li(-1:3)
      fluidparam%CoM(-1:1) = CoM_Li(-1:1)

      case (ILIQUID_FLIBE)
      fluidparam%nlist = N_FUNC2TABLE
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_FLiBe
      fluidparam%TB0 = TB0_FLiBe
      fluidparam%HM0 = HM0_FLiBe
      fluidparam%CoD(0:1) = CoD_FLiBe(0:1)
      fluidparam%CoK(0:2) = CoK_FLiBe(0:2)
      fluidparam%CoB = CoB_FLiBe
      fluidparam%CoCp(-2:2) = CoCp_FLiBe(-2:2)
      fluidparam%CoH(-1:3) = CoH_FLiBe(-1:3)
      fluidparam%CoM(-1:1) = CoM_FLiBe(-1:1)

      case (ILIQUID_PBLI)
      fluidparam%nlist = N_FUNC2TABLE
      fluidparam%ipropertyState = IPROPERTY_FUNCS
      fluidparam%TM0 = TM0_PbLi
      fluidparam%TB0 = TB0_PbLi
      fluidparam%HM0 = HM0_PbLi
      fluidparam%CoD(0:1) = CoD_PbLi(0:1)
      fluidparam%CoK(0:2) = CoK_PbLi(0:2)
      fluidparam%CoB = CoB_PbLi
      fluidparam%CoCp(-2:2) = CoCp_PbLi(-2:2)
      fluidparam%CoH(-1:3) = CoH_PbLi(-1:3)
      fluidparam%CoM(-1:1) = CoM_PbLi(-1:1)

    case (ILIQUID_WATER)
      ! The enumerator exists and the input parser accepts 'water', but no
      ! property correlation for ordinary liquid water was ever added here.
      ! This used to fall through to the default branch below and silently load
      ! liquid sodium, while the log still announced "Liquid Water".
      call Print_error_msg("ifluid = water (ordinary liquid water) has no property "// &
                           "correlation in this code. Use scp_water for the NIST table.")

    case default
      write(*, wrtfmt1i) 'ifluid = ', fluidparam%ifluid
      call Print_error_msg("Unknown ifluid. No thermal property relation is defined for it.")
    end select

    !------------------------------------------------------------------------------
    ! Temperature interval over which the property correlations are evaluated.
    !
    ! The property table is a single common interval, so it is the intersection of
    ! the phase range (melting to boiling, where the material is a liquid at all)
    ! with the validity range of every correlation that holds over less than that.
    ! The two are different kinds of limit and are kept apart: the phase range is
    ! the starting point, each correlation narrows it, and whichever one binds is
    ! recorded by name so the diagnostics can say what restricts the run.
    !
    ! The table path takes its range from the data file instead.
    !------------------------------------------------------------------------------
    if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then
      fluidparam%TP0min = fluidparam%TM0
      fluidparam%TP0max = fluidparam%TB0
      fluidparam%TP0minsrc = 'melting point'
      fluidparam%TP0maxsrc = 'boiling point'

      select case (fluidparam%ifluid)
        case (ILIQUID_PBLI)
          ! The PbLi cp and viscosity fits carry recorded ranges; the density and
          ! conductivity fits carry none, so they do not narrow the table. The
          ! viscosity is what actually binds here -- its 625 K cuts below cp's
          ! 800 K, and cp's 508 K is already TM0 -- but both are declared so that
          ! the binding end is decided by the data rather than by which call was
          ! written. See TMUmin_PbLi / TMUmax_PbLi for why the viscosity interval
          ! is an implementation policy rather than an established validity range.
          call restrict_property_range(TCPmin_PbLi, TCPmax_PbLi, 'specific heat (KfK-4144)')
          call restrict_property_range(TMUmin_PbLi, TMUmax_PbLi, 'dynamic viscosity (KfK-4144)')
        case default
          ! No correlation range is recorded for any other fluid here. That is an
          ! absence of information, not a guarantee that the fits reach TB0.
      end select
    end if

    return
  end subroutine Buildup_fluidparam

!==============================================================================
!> \brief Narrow the common property-table interval by one correlation's own
!> validity range, remembering which correlation binds each end.
!------------------------------------------------------------------------------
  subroutine restrict_property_range(tlow, thigh, propname)
    real(WP),         intent(in) :: tlow
    real(WP),         intent(in) :: thigh
    character(len = *), intent(in) :: propname

    if(tlow  > fluidparam%TP0min) then
      fluidparam%TP0min    = tlow
      fluidparam%TP0minsrc = propname
    end if
    if(thigh < fluidparam%TP0max) then
      fluidparam%TP0max    = thigh
      fluidparam%TP0maxsrc = propname
    end if

    return
  end subroutine restrict_property_range

!==============================================================================
!> \brief Stop the run if a dimensional temperature lies outside the interval
!> the property correlations support, naming the property that sets that end.
!------------------------------------------------------------------------------
  subroutine check_T_in_property_range(tdim, whatname)
    real(WP),           intent(in) :: tdim
    character(len = *), intent(in) :: whatname

    if(tdim >= fluidparam%TP0min .and. tdim <= fluidparam%TP0max) return

    if(nrank == 0) then
      write(*, wrtfmt1i) 'ifluid = ', fluidparam%ifluid
      write(*, wrtfmt1r) 'the requested T(K) = ', tdim
      write(*, wrtfmt2r) 'supported property range T(K), low and high:', fluidparam%TP0min, fluidparam%TP0max
      write(*, wrtfmt2s) 'the low  end of that range is set by: ', trim(fluidparam%TP0minsrc)
      write(*, wrtfmt2s) 'the high end of that range is set by: ', trim(fluidparam%TP0maxsrc)
    end if

    if(tdim < fluidparam%TP0min) then
      call Print_error_msg(whatname//' is below the supported property range, set by '// &
                           trim(fluidparam%TP0minsrc)//'.')
    else
      call Print_error_msg(whatname//' is above the supported property range, set by '// &
                           trim(fluidparam%TP0maxsrc)//'.')
    end if

    return
  end subroutine check_T_in_property_range

!==============================================================================
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

    !------------------------------------------------------------------------------
    !   for x-pencil
    !   scale the given thermo b.c. in dimensional to undimensional
    !------------------------------------------------------------------------------
    !------------------------------------------------------------------------------
    ! x-bc
    !------------------------------------------------------------------------------
    do n = 1, 2
      if( dm%ibcx_Tm(n) == IBC_DIRICHLET ) then
        dm%fbcx_const(n, 5) = dm%fbcx_const(n, 5)/tm%ref_T0  ! undim
      end if
      if (dm%ibcx_Tm(n) == IBC_NEUMANN) then
        dm%fbcx_const(n, 5) = dm%fbcx_const(n, 5) * tm%ref_l0 / fluidparam%ftp0ref%k / fluidparam%ftp0ref%t
      end if
    end do

    !------------------------------------------------------------------------------
    ! y-bc
    !------------------------------------------------------------------------------
    do n = 1, 2
      if( dm%ibcy_Tm(n) == IBC_DIRICHLET ) then
        dm%fbcy_const(n, 5) = dm%fbcy_const(n, 5)/tm%ref_T0  ! undim
      end if
      if (dm%ibcy_Tm(n) == IBC_NEUMANN) then
        dm%fbcy_const(n, 5) = dm%fbcy_const(n, 5) * tm%ref_l0 / fluidparam%ftp0ref%k / fluidparam%ftp0ref%t
      end if
    end do
    !------------------------------------------------------------------------------
    ! z-bc
    !------------------------------------------------------------------------------
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
        str = 'Problem opening: '//trim(filename1)
        call Print_error_msg(trim(str))
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

!==============================================================================
  subroutine initialise_thermal_properties(fl, tm, dm)
    use find_max_min_ave_mod
    use random_number_generation_mod
    use udf_type_mod
    implicit none
    !
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm
    type(t_domain), intent(in)    :: dm
    type(DECOMP_INFO)             :: dtmp

    integer  :: i, j, k, ii, jj, kk
    integer  :: nxinl, nxout, nxouts, nxoute
    integer  :: n
    real(WP) :: Ts, rd, lownoise, s
    real(WP) :: rhoh_bulk(2)
    logical  :: is_zerograd_lower
    character(len = 80) :: str

    if (nrank == 0) call Print_debug_start_msg("Initialise thermal variables ...")

    dtmp = dm%dccc
    !--------------------------------------------------------------------------------------------------------
    ! Initialise temperature field - no perburbation
    !--------------------------------------------------------------------------------------------------------
    select case (tm%inittype)
    case (INIT_GVCONST)
      if (nrank == 0) then
        call Print_debug_mid_msg("The initial thermal properties (undim) are")
        write (*, wrtfmt1r) '  Temperature:',          tm%ftp_ini%t
        write (*, wrtfmt1r) '  Density:',              tm%ftp_ini%d
        write (*, wrtfmt1r) '  Dynamic Viscosity:',    tm%ftp_ini%m
        write (*, wrtfmt1r) '  Thermal Conductivity:', tm%ftp_ini%k
        write (*, wrtfmt1r) '  Electrical Conductivity:', tm%ftp_ini%sigma_e
        write (*, wrtfmt1r) '  Cp:',                   tm%ftp_ini%cp
        write (*, wrtfmt1r) '  Enthalphy:',            tm%ftp_ini%h
        write (*, wrtfmt1r) '  mass enthaphy:',        tm%ftp_ini%rhoh
      end if
      tm%tTemp = tm%ftp_ini%t
    case (INIT_GVBCLN, INIT_GVBCSMOOTH)
!--------------------------------------------------------------------------------------------------------
!     Interpolate across y between the two side temperatures, so the run starts
!     nearer a developed field than a uniform one does. Where the lower side is
!     not Dirichlet - a pipe axis, or an adiabatic wall - there is no prescribed
!     value to start from, so init_T0 is used there. That removes the grid-scale
!     wall jump a uniform field would leave, which otherwise sets the initial
!     wall heat flux and, in a closed periodic domain, the whole (0,0)-mode
!     Poisson compatibility defect: on pipe_scp_periodic_Tw it is worth a factor
!     of 559.
!
!     With s = (y - lyb) / (lyt - lyb), the two shapes are
!       INIT_GVBCLN     T = Ts + s   * (T2 - Ts)
!       INIT_GVBCSMOOTH T = Ts + s^2 * (T2 - Ts)
!     and they differ only where the lower side is non-Dirichlet. There the
!     quadratic has dT/dy = 0 at s = 0, which is exactly the condition that side
!     imposes - symmetry at a pipe axis, zero flux at an adiabatic wall. The
!     linear profile violates it: a cone in r is not regular on the axis, and
!     its continuous Laplacian goes as 1/r there. The discrete first cell keeps
!     that finite and the r-weighting keeps it out of the volume mean, so the
!     linear profile is usable, but the quadratic is the correct shape.
!     With Dirichlet on both sides there is no gradient condition to satisfy and
!     a parabola through two prescribed endpoints is underdetermined, so the two
!     shapes deliberately coincide - linear is already regular in a channel, and
!     is the exact steady conduction solution in Cartesian coordinates.
!--------------------------------------------------------------------------------------------------------
      if (dm%ibcy_Tm(2) == IBC_DIRICHLET) then
        if (dm%ibcy_Tm(1) == IBC_DIRICHLET) then
          Ts = dm%fbcy_const(1, 5)
          is_zerograd_lower = .false.
        else
          Ts = tm%ftp_ini%t
          is_zerograd_lower = (tm%inittype == INIT_GVBCSMOOTH)
        end if
        do j = 1, dtmp%xsz(2)
          jj = dtmp%xst(2) + j - 1
          s = (dm%yc(jj) - dm%lyb) / (dm%lyt - dm%lyb)
          if (is_zerograd_lower) s = s * s
          tm%tTemp(:, j, :) = s * (dm%fbcy_const(2, 5) - Ts) + Ts
        end do
      else
!       Unreachable: input_general downgrades to INIT_GVCONST on exactly this
!       condition. Kept so the two cannot drift apart silently.
        tm%tTemp = tm%ftp_ini%t
      end if
    case default
      tm%tTemp = tm%ftp_ini%t
    end select
    !--------------------------------------------------------------------------------------------------------
    ! Refresh thermal properties from temperature
    !--------------------------------------------------------------------------------------------------------
    call ftp_refresh_thermal_properties_from_T_undim_3Dtm(fl, tm, dm)

    if(dm%ibcx_Tm(1) == IBC_PERIODIC .and. &
       dm%ibcx_Tm(2) == IBC_PERIODIC) then
      if(dm%ibcy_Tm(1) == IBC_NEUMANN .or. &
         dm%ibcy_Tm(2) == IBC_NEUMANN) then
        ! get bulk energy
        str = 'rhoh'
        call Get_volumetric_average_3d(dm, dm%dccc, tm%rhoh, rhoh_bulk(1), SPACE_AVERAGE, trim(str))
        if(nrank == 0) &
        write(*, wrtfmt1e) "The initial, [original] bulk "//trim(str)//" = ", rhoh_bulk(1)
      end if
    end if
    !--------------------------------------------------------------------------------------------------------
    ! Add low-level random perturbation to temperature field
    !--------------------------------------------------------------------------------------------------------
    lownoise = fl%noiselevel*2.0e-4_WP
    if (abs(lownoise) > MINP) then
      if(nrank == 0) call Print_debug_inline_msg("Add low-level random perturbation to temperature field ...")
      do k = 1, dtmp%xsz(3)
        kk = dtmp%xst(3) + k - 1
        do j = 1, dtmp%xsz(2)
          jj = dtmp%xst(2) + j - 1
          do i = 1, dtmp%xsz(1)
            ii = dtmp%xst(1) + i - 1
            call generate_random11_mixhash(ii, jj, kk, n, rd)
            tm%tTemp(i, j, k) = tm%tTemp(i, j, k) * (ONE + lownoise * rd)
          end do
        end do
      end do
    end if
    !--------------------------------------------------------------------------------------------------------
    ! Refresh thermal properties from temperature
    !--------------------------------------------------------------------------------------------------------
    call ftp_refresh_thermal_properties_from_T_undim_3Dtm(fl, tm, dm)
    !
    if(dm%ibcx_Tm(1) == IBC_PERIODIC .and. &
       dm%ibcx_Tm(2) == IBC_PERIODIC) then
      if(dm%ibcy_Tm(1) == IBC_NEUMANN .or. &
         dm%ibcy_Tm(2) == IBC_NEUMANN) then
        ! get bulk energy
        str = 'rhoh'
        call Get_volumetric_average_3d(dm, dm%dccc, tm%rhoh, rhoh_bulk(2), SPACE_AVERAGE, trim(str))
        if(nrank == 0) &
        write(*, wrtfmt1e) "The initial, [real] bulk "//trim(str)//" = ", rhoh_bulk(2)
        ! Preserve the original Neumann-case bulk rhoh by removing only the
        ! perturbation-induced mean offset.  Multiplicative rescaling is singular
        ! when the initial bulk rhoh is zero.
        tm%rhoh = tm%rhoh - (rhoh_bulk(2) - rhoh_bulk(1))
        call Get_volumetric_average_3d(dm, dm%dccc, tm%rhoh, rhoh_bulk(2), SPACE_AVERAGE, trim(str))
        if(nrank == 0) &
        write(*, wrtfmt1e) "The initial, [adjust] bulk "//trim(str)//" = ", rhoh_bulk(2)
        call ftp_refresh_thermal_properties_from_DH_3Dtm(fl, tm, dm)
      end if
    end if
    !--------------------------------------------------------------------------------------------------------
    ! Apply thermo buffer layers in x direction
    !--------------------------------------------------------------------------------------------------------
    nxinl  = 0
    nxout  = 0
    nxouts = 0
    nxoute = 0
    if ((tm%thermo_buffer_layer(1) - dm%h(1)) > MINP) then
      nxinl = floor(tm%thermo_buffer_layer(1) * dm%h1r(1))
      if(nrank == 0) call Print_debug_inline_msg ("Configuring thermo buffer layers at the inlet...")
    end if

    if ((tm%thermo_buffer_layer(2) - dm%h(1)) > MINP) then
      nxout  = floor(tm%thermo_buffer_layer(2) * dm%h1r(1))
      nxouts = dtmp%xsz(1) - nxout
      nxoute = dtmp%xsz(1)
      if(nrank == 0) call Print_debug_inline_msg ("Configuring thermo buffer layers at the outlet...")
    end if
    if (nxinl > 0) then
      fl%dDens(1:nxinl, :, :) = ONE
      fl%mVisc(1:nxinl, :, :) = ONE
      tm%rhoh (1:nxinl, :, :) = ZERO
      tm%hEnth(1:nxinl, :, :) = ZERO
      tm%kCond(1:nxinl, :, :) = ONE
      tm%eCond(1:nxinl, :, :) = ONE
      tm%tTemp(1:nxinl, :, :) = ONE
    end if
    if (nxout > 0) then
      fl%dDens(nxouts:nxoute, :, :) = ONE
      fl%mVisc(nxouts:nxoute, :, :) = ONE
      tm%rhoh (nxouts:nxoute, :, :) = ZERO
      tm%hEnth(nxouts:nxoute, :, :) = ZERO
      tm%kCond(nxouts:nxoute, :, :) = ONE
      tm%eCond(nxouts:nxoute, :, :) = ONE
      tm%tTemp(nxouts:nxoute, :, :) = ONE
    end if

    if (nrank == 0) call Print_debug_end_msg()

    return
  end subroutine initialise_thermal_properties

!==============================================================================
!> Initialise thermal variables if ithermo = 1.
!------------------------------------------------------------------------------
!> Scope:  mpi    called-freq    xdomain     module
!>         all    once           specified   private
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - fl (inout): flow type
!> - tm (inout): thermo type
!==============================================================================
!   subroutine get_bc_tdm (dm) ! apply once
!     use parameters_constant_mod
!     implicit none
!     type(t_flow),   intent(inout) :: fl
!     type(t_thermo), intent(inout) :: tm

!     type(t_fluidThermoProperty) :: ftpx, ftpy, ftpz

!     do k = 1, size(dm%fbcx_var, 3)
!       do j = 1, size(dm%fbcx_var, 2)
!         do i = 1, size(dm%fbcx_var, 1)
! !------------------------------------------------------------------------------
! !         update density and viscousity at b.c.
! !         for temperature bc, heat flux bc (to chdck)
! !------------------------------------------------------------------------------
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

!==============================================================================
!==============================================================================
!> The main code for thermal property initialisation.
!> Scope:  mpi    called-freq    xdomain
!>         all    once           all
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - none (inout): NA
!_______________________________________________________________________________
  subroutine Buildup_thermo_mapping_relations(tm)
    type(t_thermo), intent(inout) :: tm

    if(nrank == 0) call Print_debug_start_msg("Initialising thermal mapping relations ...")
    call Buildup_fluidparam(tm)
    if (fluidparam%ipropertyState == IPROPERTY_TABLE) call buildup_property_relations_from_table
    if (fluidparam%ipropertyState == IPROPERTY_FUNCS) call buildup_property_relations_from_function
    call Write_thermo_property
    tm%ftp_ini%t = tm%init_T0 / tm%ref_T0 ! already undim
    call ftp_refresh_thermal_properties_from_T_undim(tm%ftp_ini)

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine Buildup_thermo_mapping_relations

  function get_qw_ramp_factor(iter, istt, iend) result(framp)
    use math_mod
    use parameters_constant_mod
    implicit none
    integer, intent(in) :: iter, istt, iend
    real(WP) :: framp
    real(WP) :: xi

    if(iter < istt) then
      framp = ZERO
    else if (iter > iend) then
      framp = ONE
    else
      xi = real(iter - istt, WP) / real(iend - istt, WP)
      framp = HALF * (ONE - COS_WP(ACOS_WP(-ONE) * xi)) ! half cosine
    end if
  end function

end module thermo_info_mod
