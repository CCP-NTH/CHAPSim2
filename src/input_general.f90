!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!==========================================================================================================
!> \file input_general.f90
!> \brief Reading the input parameters from the given file.
!> \author Wei Wang wei.wang@stfc.ac.uk
!> \date 11-05-2022, checked.
!==========================================================================================================
module input_general_mod
  use print_msg_mod
  use parameters_constant_mod
  implicit none

  logical :: is_prerun, is_postprocess

  public  :: Read_input_parameters
  private :: get_name_case
  private :: get_name_cs
  private :: get_name_mesh
  private :: get_name_iacc
  private :: get_name_initial
  private :: get_name_fluid
  private :: get_name_fft
  private :: get_name_mstret
contains
!==========================================================================================================
  function get_name_case(icase) result(str)
    integer, intent(in) :: icase
    character(72) :: str

    select case(icase)
    case ( ICASE_DUCT) 
      str = 'ICASE_DUCT'
    case ( ICASE_OTHERS) 
      str = 'ICASE_OTHERS'
    case ( ICASE_CHANNEL )
      str = 'Channel flow'
    case ( ICASE_PIPE )
      str = 'Pipe flow'
    case ( ICASE_ANNULAR )
      str = 'Annular flow'
    case ( ICASE_TGV2D )
      str = '2D Taylor Green Vortex'
    case ( ICASE_TGV3D )
      str = '3D Taylor Green Vortex'
    case ( ICASE_BURGERS )
      str = 'Burgers flow'
    case ( ICASE_ALGTEST )
      str = 'Analytical test'
    case default
      call Print_error_msg('The required case type is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_cs(ics) result(str)
    integer, intent(in) :: ics
    character(72) :: str

    select case(ics)
    case ( ICARTESIAN) 
      str = 'Cartesian coordinate system'
    case ( ICYLINDRICAL )
      str = 'Cylindrical coordinate system'
    case default
      call Print_error_msg('The required coordinate system is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_mesh(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str

    select case(ist)
    case ( ISTRET_NO) 
      str = 'Uniform mesh without stretching'
    case ( ISTRET_CENTRE)
      str = 'Mesh clusted towards centre of y-domain'
    case ( ISTRET_2SIDES)
      str = 'Mesh clusted towards two sides of y-domain'
    case ( ISTRET_BOTTOM)
      str = 'Mesh clusted towards the bottom of y-domain'
    case ( ISTRET_TOP)
      str = 'Mesh clusted towards the top of y-domain'
    case default
      call Print_error_msg('The required mesh stretching is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_mstret(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str

    select case(ist)
    case ( MSTRET_3FMD) 
      str = 'Stretched mesh has only 3 Fourier modes. Suitable for 3-D FFT.'
    case ( MSTRET_TANH)
      str = 'Stretched mesh follows tanh.'
    case ( MSTRET_POWL)
      str = 'Stretched mesh follows powerlaw.'
    case default
      call Print_warning_msg('The required mesh stretching method is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_fft(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str

    select case(ist)
    case ( FFT_2DECOMP_3DFFT) 
      str = '3-D FFT using 2DECOMP&FFT'
    case ( FFT_FISHPACK_2DFFT)
      str = '2-D FFT using Fishpack FFT'
    case default
      call Print_error_msg('The required FFT lib is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_iacc(iacc) result(str)
    integer, intent(in) :: iacc
    character(72) :: str

    select case(iacc)
    case ( IACCU_CD2) 
      str = '2nd order Centrail Difference'
    case ( IACCU_CD4)
      str = '4th order Central Difference'
    case ( IACCU_CP4)
      str = '4th order Compact Scheme'
    case ( IACCU_CP6)
      str = '6th order Compact Scheme'
    case default
      call Print_error_msg('The required numerical scheme is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_initial(irst) result(str)
    integer, intent(in) :: irst
    character(72) :: str

    select case(irst)
    case ( INIT_RESTART) 
      str = 'Initialised from restart'
    case ( INIT_RANDOM)
      str = 'Initialised from random numbers'
    case ( INIT_INLET)
      str = 'Initialised from inlet'
    case ( INIT_GVCONST)
      str = 'Initialised from given values'
    case ( INIT_POISEUILLE)
      str = 'Initialised from a poiseuille flow'
    case ( INIT_FUNCTION)
      str = 'Initialised from a given function'
    case default
      call Print_error_msg('The required initialisation method is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_fluid(ifl) result(str)
    integer, intent(in) :: ifl
    character(72) :: str

    select case(ifl)
    case ( ISCP_WATER) 
      str = 'Supercritical water'
    case ( ISCP_CO2)
      str = 'Supercritical CO2'
    case ( ILIQUID_BISMUTH)
      str = 'Liquid Bismuth'
    case ( ILIQUID_LBE)
      str = 'Liquid LBE'
    case ( ILIQUID_LEAD)
      str = 'Liquid Lead'
    case ( ILIQUID_SODIUM)
      str = 'Liquid Sodium'
    case ( ILIQUID_WATER)
      str = 'Liquid Water'
    case ( ILIQUID_LITHIUM)
      str = 'Liquid Lithium'
    case default
      call Print_error_msg('The required flow medium is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
  function get_name_drivenforce(ifl) result(str)
    integer, intent(in) :: ifl
    character(72) :: str

    select case(ifl)
    case ( IDRVF_NO) 
      str = 'no external driven force'
    case ( IDRVF_X_MASSFLUX)
      str = 'constant mass flux driven in x-direction'
    case ( IDRVF_X_TAUW)
      str = 'constant skin friction driven in x-direction'
    case ( IDRVF_X_DPDX)
      str = 'pressure gradient driven in x-direction'
    case ( IDRVF_Z_MASSFLUX)
      str = 'constant mass flux driven in z-direction'
    case ( IDRVF_Z_TAUW)
      str = 'constant skin friction driven in z-direction'
    case ( IDRVF_Z_DPDZ)
      str = 'pressure gradient driven in z-direction'
    case default
      call Print_error_msg('The required flow-driven method is not supported.')
    end select
    str = ' '//trim(adjustl(str))

    return
  end function
!==========================================================================================================
!> \brief Reading the input parameters from the given file.     
!! Scope:  mpi    called-freq    xdomain
!!         all    once           all
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           
!----------------------------------------------------------------------------------------------------------
!> \param[in]     none          NA
!> \param[out]    none          NA
!==========================================================================================================
  subroutine Read_input_parameters
    use wtformat_mod
    use mpi_mod
    use parameters_constant_mod
    use vars_df_mod
    use thermo_info_mod
    use boundary_conditions_mod
    use code_performance_mod
    use EvenOdd_mod
    implicit none
    character(len = 18) :: flinput = 'input_chapsim.ini'
    integer, parameter :: IOMSG_LEN = 200
    character(len = IOMSG_LEN) :: iotxt
    integer :: ioerr, inputUnit
    integer  :: slen

    character(len = 80) :: secname
    character(len = 80) :: varname
    integer  :: itmp
    real(WP) :: rtmp
    real(WP), allocatable :: rtmpx(:)
    integer, allocatable  :: itmpx(:)
    integer :: i, j, m, n
    logical :: is_tmp
    logical :: is_any_energyeq
    
    if(nrank == 0) then
      call Print_debug_start_msg("CHAPSim2.0 Starts ...")
      write (*, wrtfmt1i) 'The precision is REAL * ', WP
      write (*, wrtfmt1l) 'is_strong_coupling : ', is_strong_coupling
      write (*, wrtfmt1l) 'is_drhodt_chain : ', is_drhodt_chain
      if((.not.is_strong_coupling) .and. is_drhodt_chain) call Print_warning_msg("For a loose coupling, an Euler drho/dt is recommended.")
      if(is_strong_coupling .and. (.not.is_drhodt_chain)) call Print_warning_msg("For a strong coupling, a drho/dt chain calc is recommended.")
    end if
    is_any_energyeq = .false.

    !----------------------------------------------------------------------------------------------------------
    ! open file
    !----------------------------------------------------------------------------------------------------------
    open ( newunit = inputUnit, &
           file    = flinput, &
           status  = 'old', &
           action  = 'read', &
           iostat  = ioerr, &
           iomsg   = iotxt )
    if(ioerr /= 0) then
      ! write (*, *) 'Problem openning : ', flinput, ' for reading.'
      ! write (*, *) 'Message: ', trim (iotxt)
      call Print_error_msg('Error in opening the input file: input_chapsim.ini')
    end if

    if(nrank == 0) &
    call Print_debug_start_msg("Reading General Parameters from "//flinput//" ...")
    !----------------------------------------------------------------------------------------------------------
    ! reading input
    !----------------------------------------------------------------------------------------------------------
    do 
      !----------------------------------------------------------------------------------------------------------
      ! reading headings/comments
      !----------------------------------------------------------------------------------------------------------
      read(inputUnit, '(a)', iostat = ioerr) secname
      slen = len_trim(secname)
      if (ioerr /=0 ) exit
      if ( (secname(1:1) == ';') .or. &
           (secname(1:1) == '#') .or. &
           (secname(1:1) == ' ') .or. &
           (slen == 0) ) then
        cycle
      end if
      if(nrank == 0) call Print_debug_mid_msg("Reading "//secname(1:slen))
      !----------------------------------------------------------------------------------------------------------
      ! [ioparams]
      !----------------------------------------------------------------------------------------------------------
      if ( secname(1:slen) == '[process]' ) then
        read(inputUnit, *, iostat = ioerr) varname, is_prerun
        read(inputUnit, *, iostat = ioerr) varname, is_postprocess
        if(nrank == 0) then
          write (*, wrtfmt1l) 'is_prerun :', is_prerun
          write (*, wrtfmt1l) 'is_postprocess :', is_postprocess
        end if 
      !----------------------------------------------------------------------------------------------------------
      ! [decomposition]
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[decomposition]' ) then
        read(inputUnit, *, iostat = ioerr) varname, nxdomain
        read(inputUnit, *, iostat = ioerr) varname, p_row
        read(inputUnit, *, iostat = ioerr) varname, p_col
        if (nxdomain /= 1 .and. nrank == 0) call Print_error_msg("Set up nxdomain = 1.")
        allocate( domain (nxdomain) )
        allocate(   flow (nxdomain) )
        allocate( itmpx(nxdomain) ); itmpx = 0
        allocate( rtmpx(nxdomain) ); rtmpx = ZERO
        domain(:)%is_thermo = .false.
        domain(:)%icht = 0
        domain(:)%is_mhd = .false.

        do i = 1, nxdomain
          domain(i)%idom = i
        end do

        if(nrank == 0) then
          call Print_note_msg('if p_row = p_col = 0, the system will employ a default, automatic domain decomposition strategy.')
          write (*, wrtfmt1i) 'x-dir domain number             :', nxdomain
          write (*, wrtfmt1i) 'y-dir domain number (mpi Row)   :', p_row
          write (*, wrtfmt1i) 'z-dir domain number (mpi Column):', p_col
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [domain]
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[domain]' ) then

        read(inputUnit, *, iostat = ioerr) varname, domain(1)%icase
        domain(:)%icase = domain(1)%icase

        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%lxx

        read(inputUnit, *, iostat = ioerr) varname, domain(1)%lyt
        domain(:)%lyt = domain(1)%lyt

        read(inputUnit, *, iostat = ioerr) varname, domain(1)%lyb
        domain(:)%lyb = domain(1)%lyb

        read(inputUnit, *, iostat = ioerr) varname, domain(1)%lzz
        domain(:)%lzz = domain(1)%lzz

        !----------------------------------------------------------------------------------------------------------
        !     restore domain size to default if not set properly
        !----------------------------------------------------------------------------------------------------------
        do i = 1, nxdomain
          if (domain(i)%icase == ICASE_CHANNEL) then
            domain(i)%lyb = - ONE
            domain(i)%lyt = ONE
          else if (domain(i)%icase == ICASE_DUCT) then
            domain(i)%lyb = - ONE
            domain(i)%lyt = ONE
          else if (domain(i)%icase == ICASE_PIPE) then
            domain(i)%lyb = ZERO
            domain(i)%lyt = ONE
            domain(i)%lzz = TWOPI
          else if (domain(i)%icase == ICASE_ANNULAR) then
            domain(i)%lyt = ONE
            domain(i)%lzz = TWOPI
          else if (domain(i)%icase == ICASE_TGV2D .or. domain(i)%icase == ICASE_TGV3D) then
            domain(i)%lxx = TWOPI
            domain(i)%lzz = TWOPI
            domain(i)%lyt =   PI
            domain(i)%lyb = - PI
          else if (domain(i)%icase == ICASE_BURGERS) then
            domain(i)%lxx = TWO
            domain(i)%lzz = TWO
            domain(i)%lyt = TWO
            domain(i)%lyb = ZERO
          else if (domain(i)%icase == ICASE_ALGTEST) then
            domain(i)%lxx = TWOPI
            domain(i)%lzz = TWOPI
            domain(i)%lyt = TWOPI
            domain(i)%lyb = ZERO
          else 
            ! do nothing...
          end if

          !----------------------------------------------------------------------------------------------------------
          ! coordinates type
          !----------------------------------------------------------------------------------------------------------
          if (domain(i)%icase == ICASE_PIPE) then
            domain(i)%icoordinate = ICYLINDRICAL
          else if (domain(i)%icase == ICASE_ANNULAR) then
            domain(i)%icoordinate = ICYLINDRICAL
          else 
            domain(i)%icoordinate = ICARTESIAN
          end if

        end do

        
        if(nrank == 0) then

          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt2s) 'current icase id :', get_name_case(domain(i)%icase)
            write (*, wrtfmt2s) 'current coordinates system :', get_name_cs(domain(i)%icoordinate)
            write (*, wrtfmt1r) 'scaled length in x-direction :', domain(i)%lxx
            write (*, wrtfmt1r) 'scaled length in y-direction :', domain(i)%lyt - domain(i)%lyb
            if((domain(i)%lyt - domain(i)%lyb) < ZERO) call Print_error_msg("Y length is smaller than zero.")
            write (*, wrtfmt1r) 'scaled length in z-direction :', domain(i)%lzz
          end do
        end if
        
      !----------------------------------------------------------------------------------------------------------
      ! [boundary] 
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[bc]' ) then

        do i = 1, nxdomain
          read(inputUnit, *, iostat = ioerr) varname, domain(i)%ibcx_nominal(1:2, 1), domain(i)%fbcx_const(1:2, 1)
          read(inputUnit, *, iostat = ioerr) varname, domain(i)%ibcx_nominal(1:2, 2), domain(i)%fbcx_const(1:2, 2)
          read(inputUnit, *, iostat = ioerr) varname, domain(i)%ibcx_nominal(1:2, 3), domain(i)%fbcx_const(1:2, 3)
          read(inputUnit, *, iostat = ioerr) varname, domain(i)%ibcx_nominal(1:2, 4), domain(i)%fbcx_const(1:2, 4)
          read(inputUnit, *, iostat = ioerr) varname, domain(i)%ibcx_nominal(1:2, 5), domain(i)%fbcx_const(1:2, 5) ! dimensional
        end do

        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcy_nominal(1:2, 1), domain(1)%fbcy_const(1:2, 1)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcy_nominal(1:2, 2), domain(1)%fbcy_const(1:2, 2)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcy_nominal(1:2, 3), domain(1)%fbcy_const(1:2, 3)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcy_nominal(1:2, 4), domain(1)%fbcy_const(1:2, 4)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcy_nominal(1:2, 5), domain(1)%fbcy_const(1:2, 5) ! dimensional

        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcz_nominal(1:2, 1), domain(1)%fbcz_const(1:2, 1)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcz_nominal(1:2, 2), domain(1)%fbcz_const(1:2, 2)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcz_nominal(1:2, 3), domain(1)%fbcz_const(1:2, 3)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcz_nominal(1:2, 4), domain(1)%fbcz_const(1:2, 4)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ibcz_nominal(1:2, 5), domain(1)%fbcz_const(1:2, 5) ! dimensional

        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%idriven
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%drvfc   

        do i = 2, nxdomain
          domain(i)%ibcy_nominal(:, :) = domain(1)%ibcy_nominal(:, :)
          domain(i)%ibcz_nominal(:, :) = domain(1)%ibcz_nominal(:, :)
          domain(i)%fbcy_const(:, :) = domain(1)%fbcy_const(:, :)
          domain(i)%fbcz_const(:, :) = domain(1)%fbcz_const(:, :)
        end do

        do i = 1, nxdomain
          domain(i)%is_periodic(:) = .false.
          do m = 1, 3
            if(domain(i)%ibcx_nominal(1, m) == IBC_PERIODIC .or. &
               domain(i)%ibcx_nominal(2, m) == IBC_PERIODIC) then
               domain(i)%ibcx_nominal(1:2, m) = IBC_PERIODIC
               domain(i)%is_periodic(1) = .true.
            end if
            if(domain(i)%ibcy_nominal(1, m) == IBC_PERIODIC .or. &
               domain(i)%ibcy_nominal(2, m) == IBC_PERIODIC) then
               domain(i)%ibcy_nominal(1:2, m) = IBC_PERIODIC
               domain(i)%is_periodic(2) = .true.
            end if
            if(domain(i)%ibcz_nominal(1, m) == IBC_PERIODIC .or. &
               domain(i)%ibcz_nominal(2, m) == IBC_PERIODIC) then
               domain(i)%ibcz_nominal(1:2, m) = IBC_PERIODIC
               domain(i)%is_periodic(3) = .true.
            end if
          end do

          if (domain(i)%icase == ICASE_PIPE) then
            domain(i)%ibcy_nominal(1, :) = IBC_INTERIOR
            domain(i)%ibcy_nominal(1, 2) = IBC_INTERIOR !IBC_DIRICHLET
            domain(i)%fbcx_const(1, 2) = ZERO
            domain(i)%is_periodic(2) = .false.
          end if
          if(domain(i)%ibcx_nominal(1, 1) == IBC_DATABASE) then
            domain(i)%ibcx_nominal(1, 2:3) = IBC_DATABASE
            domain(i)%ibcx_nominal(1, 4:5) = IBC_NEUMANN
          end if
          !if(domain(i)%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
          !  domain(i)%ibcx_nominal(2, 2:3) = IBC_CONVECTIVE
            !domain(i)%ibcx_nominal(2, 4) = IBC_NEUMANN
            !domain(i)%ibcx_nominal(2, 5) = IBC_NEUMANN !IBC_CONVECTIVE, to check!
          !end if
          !----------------------------------------------------------------------------------------------------------
          ! to exclude non-resonable input
          !----------------------------------------------------------------------------------------------------------
          domain(i)%is_conv_outlet = .false.
          do m = 1, NBC
            if(domain(i)%ibcx_nominal(2, m) == IBC_PROFILE1D) call Print_error_msg(" This BC IBC_PROFILE1D is not supported.")
            do n = 1, 2
              if(domain(i)%ibcx_nominal(n, m) >  IBC_OTHERS   ) call Print_error_msg(" This xBC is not suported.")
              if(domain(i)%ibcy_nominal(n, m) >  IBC_OTHERS   ) call Print_error_msg(" This yBC is not suported.")
              if(domain(i)%ibcz_nominal(n, m) >  IBC_OTHERS   ) call Print_error_msg(" This zBC is not suported.")
              if(domain(i)%ibcy_nominal(n, m) == IBC_PROFILE1D) call Print_error_msg(" This yBC IBC_PROFILE1D is not supported.")
              if(domain(i)%ibcz_nominal(n, m) == IBC_PROFILE1D) call Print_error_msg(" This zBC IBC_PROFILE1D is not supported.")
            end do
            if(domain(i)%ibcx_nominal(2, m) == IBC_CONVECTIVE) domain(i)%is_conv_outlet(1) = .true.
            if(domain(i)%ibcy_nominal(2, m) == IBC_CONVECTIVE) call Print_error_msg(" Convective Outlet in Y direction is not supported.")
            if(domain(i)%ibcz_nominal(2, m) == IBC_CONVECTIVE) domain(i)%is_conv_outlet(3) = .true.
          end do 
        end do

        do i = 1, nxdomain
          if(domain(i)%icase /= ICASE_CHANNEL .and. &
             domain(i)%icase /= ICASE_ANNULAR  .and. &
             domain(i)%icase /= ICASE_PIPE ) then
            flow(i)%idriven = IDRVF_NO
          end if
          
          if(domain(i)%ibcx_nominal(1, 1) /= IBC_PERIODIC .or. &
             domain(i)%ibcx_nominal(2, 1) /= IBC_PERIODIC) then 
            flow(i)%idriven = IDRVF_NO
          end if
        end do

        if(nrank == 0) then
          do i = 1, nxdomain
            write (*, wrtfmt2s) 'flow driven force type :', get_name_drivenforce(flow(i)%idriven)
            if(flow(i)%idriven /= IDRVF_NO .and. &
               flow(i)%idriven /= IDRVF_X_MASSFLUX .and. &
               flow(i)%idriven /= IDRVF_Z_MASSFLUX) then
              write (*, wrtfmt1r) 'flow driven force(cf):', flow(i)%drvfc        
            end if
          end do
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [mesh] 
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[mesh]' ) then
        read(inputUnit, *, iostat = ioerr) varname, domain(1:nxdomain)%nc(1)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%nc(2)
        domain(:)%nc(2) = domain(1)%nc(2)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%nc(3)
        domain(:)%nc(3) = domain(1)%nc(3)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%istret
        domain(:)%istret = domain(1)%istret
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%mstret, domain(1)%rstret
        domain(:)%rstret = domain(1)%rstret
        domain(:)%mstret = domain(1)%mstret
        !read(inputUnit, *, iostat = ioerr) varname, domain(1)%ifft_lib
        domain(:)%ifft_lib = FFT_2DECOMP_3DFFT
        !domain(:)%ifft_lib = FFT_FISHPACK_2DFFT ! for testing only, hidden option


        do i = 1, nxdomain
          domain(i)%fft_skip_c2c(:) = .false.
          if(domain(i)%icoordinate == ICYLINDRICAL) then
            if (.not. is_even(domain(i)%nc(3))) domain(i)%nc(3) = domain(i)%nc(3) + 1
            domain(i)%fft_skip_c2c(2) = .true.
          end if
          if (.not. domain(i)%fft_skip_c2c(2)) domain(i)%mstret = MSTRET_3FMD
          !----------------------------------------------------------------------------------------------------------
          !     stretching
          !----------------------------------------------------------------------------------------------------------
          domain(i)%is_stretching(:) = .false.
          if(domain(i)%istret /= ISTRET_NO) domain(i)%is_stretching(2) = .true.
          
          if (domain(i)%icase == ICASE_CHANNEL .and. &
              domain(i)%istret /= ISTRET_2SIDES .and. &
              domain(i)%istret /= ISTRET_NO ) then

            if(nrank == 0) call Print_warning_msg ("Grids are neither uniform nor two-side clustered.")
          
          else if (domain(i)%icase == ICASE_PIPE .and. &
                   domain(i)%istret /= ISTRET_TOP) then

            if(nrank == 0) call Print_warning_msg ("Grids are not near-wall clustered.")

          else if (domain(i)%icase == ICASE_ANNULAR .and. &
                   domain(i)%istret /= ISTRET_2SIDES .and. &
                   domain(i)%istret /= ISTRET_NO) then

            if(nrank == 0) call Print_warning_msg ("Grids are neither uniform nor two-side clustered.")

          else if (domain(i)%icase == ICASE_TGV2D .or. &
                   domain(i)%icase == ICASE_TGV3D .or. &
                   domain(i)%icase == ICASE_ALGTEST) then

            if(domain(i)%istret /= ISTRET_NO .and. nrank == 0) &
            call Print_warning_msg ("Grids are clustered.")

          else 
            ! do nothing...
          end if
          !
          if(.not. domain(i)%fft_skip_c2c(2)) domain(i)%mstret   = MSTRET_3FMD
        end do

        if(nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1i) 'mesh cell number - x :', domain(i)%nc(1)
            write (*, wrtfmt1i) 'mesh cell number - y :', domain(i)%nc(2)
            write (*, wrtfmt1i) 'mesh cell number - z :', domain(i)%nc(3)
            write (*, wrtfmt2s) 'FFT lib :', get_name_fft(domain(i)%ifft_lib)
            write (*, wrtfmt3l) 'is mesh stretching in xyz :', domain(i)%is_stretching(1:3)
            write (*, wrtfmt2s) 'mesh y-stretching type :', get_name_mesh(domain(i)%istret)
            if(domain(i)%istret /= ISTRET_NO) then
              write (*, wrtfmt1r) 'mesh y-stretching factor :', domain(i)%rstret
              write (*, wrtfmt2s) 'mesh y-stretching method :', get_name_mstret(domain(i)%mstret)
            end if
          end do
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [timestepping]
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[scheme]' ) then
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%dt
        domain(:)%dt = domain(1)%dt
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%iTimeScheme
        domain(:)%iTimeScheme = domain(1)%iTimeScheme
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%iAccuracy
        domain(:)%iAccuracy = domain(1)%iAccuracy
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%iviscous
        domain(:)%iviscous = domain(1)%iviscous
        ! some schemes are still testing, check >>>
        if(domain(1)%icase == ICASE_PIPE) then
          domain(1)%iAccuracy = IACCU_CD2
        end if ! 

        if(domain(1)%icase == ICASE_CHANNEL) then
          if (domain(1)%iAccuracy == IACCU_CP4 .or.  &
              domain(1)%iAccuracy == IACCU_CP6) then
            domain(1)%iAccuracy = IACCU_CD4
          end if
        end if
        ! some schemes are still testing, check <<<

        if(domain(1)%iAccuracy == IACCU_CD2 .or. &
           domain(1)%iAccuracy == IACCU_CD4) then
          domain(:)%is_compact_scheme = .false.
        else if (domain(1)%iAccuracy == IACCU_CP4 .or. &
                 domain(1)%iAccuracy == IACCU_CP6) then
          domain(:)%is_compact_scheme = .true.
        else
          call Print_error_msg("Input error for numerical schemes.")
        end if

        if(nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1e) 'physical time step(dt, unit = second) :', domain(i)%dt
            write (*, wrtfmt1i) 'time marching scheme :', domain(i)%iTimeScheme
            write (*, wrtfmt2s) 'current spatial accuracy scheme :', get_name_iacc(domain(i)%iAccuracy)
            write (*, wrtfmt1i) 'viscous term treatment  :', domain(i)%iviscous
          end do
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [flow]
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[flow]' ) then

        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%inittype
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%iterfrom
        read(inputUnit, *, iostat = ioerr) varname, flow(1)%init_velo3d(1:3)
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%noiselevel
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%reninit
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%initReTo
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%ren

        do i = 1, nxdomain
          if(flow(i)%inittype /= INIT_RESTART) flow(i)%iterfrom = 0
          flow(i)%init_velo3d(1:3) = flow(1)%init_velo3d(1:3)
          if(flow(i)%inittype == INIT_RESTART) flow(i)%reninit = flow(i)%ren
          if(domain(i)%ibcx_nominal(1, 1) /= IBC_PERIODIC .or. &
            domain(i)%ibcx_nominal(2, 1) /= IBC_PERIODIC) then 
            flow(i)%reninit = flow(i)%ren
          end if
        end do

        
        

        if( nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            call print_note_msg("The Reynolds number is based on half channel hight or radius of a pipe.")
            write (*, wrtfmt2s) 'flow initial type :', get_name_initial(flow(i)%inittype)
            write (*, wrtfmt1i) 'iteration starting from :', flow(i)%iterfrom
            if(flow(i)%inittype == INIT_GVCONST) then
            write (*, wrtfmt3r) 'initial velocity u, v, w :', flow(i)%init_velo3d(1:3)
            end if
            write (*, wrtfmt1r) 'Initial velocity influction level :', flow(i)%noiselevel
            write (*, wrtfmt1r) 'Initial Reynolds No. :', flow(i)%reninit
            write (*, wrtfmt1i) 'Iteration for initial Reynolds No.:', flow(i)%initReTo
            write (*, wrtfmt1r) 'flow Reynolds number :', flow(i)%ren
          end do
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [thermo] 
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[thermo]' )  then 
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%is_thermo
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%icht
        read(inputUnit, *, iostat = ioerr) varname,   flow(1 : nxdomain)%igravity

        if(ANY(domain(:)%is_thermo)) is_any_energyeq = .true.
        if(is_any_energyeq) allocate( thermo(nxdomain) )
        
        read(inputUnit, *, iostat = ioerr) varname, itmp
        if(is_any_energyeq) thermo(1 : nxdomain)%ifluid = itmp
        read(inputUnit, *, iostat = ioerr) varname, rtmp
        if(is_any_energyeq) thermo(1 : nxdomain)%ref_l0 = rtmp
        read(inputUnit, *, iostat = ioerr) varname, rtmp
        if(is_any_energyeq) thermo(1 : nxdomain)%ref_T0 = rtmp
        
        read(inputUnit, *, iostat = ioerr) varname, itmp
        if(is_any_energyeq) thermo(1 : nxdomain)%inittype  = itmp
        read(inputUnit, *, iostat = ioerr) varname, itmp
        if(is_any_energyeq) thermo(1 : nxdomain)%iterfrom = itmp
        read(inputUnit, *, iostat = ioerr) varname, rtmpx(1: nxdomain)
        if(is_any_energyeq) thermo(1 : nxdomain)%init_T0 = rtmpx(1: nxdomain)
        
        if(is_any_energyeq .and. nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1l) 'is thermal field solved ?', domain(i)%is_thermo
            write (*, wrtfmt1l) 'is CHT solved ?', domain(i)%icht
            write (*, wrtfmt1i) 'gravity direction ', flow(i)%igravity
            write (*, wrtfmt2s) 'fluid medium :' , get_name_fluid(thermo(i)%ifluid)
            write (*, wrtfmt1r) 'reference length (m) :', thermo(i)%ref_l0
            write (*, wrtfmt1r) 'reference temperature (K) :', thermo(i)%ref_T0
            write (*, wrtfmt1i) 'thermo field initial type :', thermo(i)%inittype
            write (*, wrtfmt1i) 'iteration starting from :', thermo(i)%iterfrom
            write (*, wrtfmt1r) 'initial temperature (K) :', thermo(i)%init_T0
          end do
        else if(nrank == 0) then
         call Print_note_msg ('Thermal field is not considered. ')
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [mhd] 
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[mhd]' )  then 
        read(inputUnit, *, iostat = ioerr) varname, domain(1:nxdomain)%is_mhd
        if(domain(1)%is_mhd) then
          allocate (mhd(nxdomain))
          read(inputUnit, *, iostat = ioerr) varname, mhd(1)%is_NStuart, mhd(1)%NStuart
          read(inputUnit, *, iostat = ioerr) varname, mhd(1)%is_NHartmn, mhd(1)%NHartmn
          read(inputUnit, *, iostat = ioerr) varname, mhd(1)%B_static(1:3)
          if( (     mhd(1)%is_NStuart  .and.       mhd(1)%is_NHartmn) .or. &
            ( (.not.mhd(1)%is_NStuart) .and. (.not.mhd(1)%is_NHartmn)) ) &
          call Print_error_msg('Please provide either Stuart Number or Hartmann Number')
          mhd(1)%iterfrom = flow(1)%iterfrom
        else
          read(inputUnit, *, iostat = ioerr) varname, is_tmp, rtmp
          read(inputUnit, *, iostat = ioerr) varname, is_tmp, rtmp
          read(inputUnit, *, iostat = ioerr) varname, rtmp, rtmp, rtmp
        end if
        if(domain(1)%is_mhd .and. nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1l) 'is thermal field solved?', domain(i)%is_mhd
            if(mhd(1)%is_NStuart) &
            write (*, wrtfmt1r) 'given Stuart Number :', mhd(i)%NStuart
            if(mhd(1)%is_NHartmn) &
            write (*, wrtfmt1r) 'given Hartmann Number :', mhd(i)%NHartmn
            write (*, wrtfmt3r) 'Static Magnetic field :', mhd(i)%B_static(1:3)
          end do
        else if(nrank == 0) then
         call Print_note_msg(' MHD is not considered. ')
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [simcontrol]
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[simcontrol]' ) then
        read(inputUnit, *, iostat = ioerr) varname,   flow(1 : nxdomain)%nIterFlowStart
        read(inputUnit, *, iostat = ioerr) varname,   flow(1 : nxdomain)%nIterFlowEnd
        read(inputUnit, *, iostat = ioerr) varname,   itmpx(1:nxdomain)
        if(is_any_energyeq) thermo(1 : nxdomain)%nIterThermoStart = itmpx(1:nxdomain)
        read(inputUnit, *, iostat = ioerr) varname,   itmpx(1:nxdomain)
        if(is_any_energyeq) thermo(1 : nxdomain)%nIterThermoEnd = itmpx(1:nxdomain)

        if( nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1i) 'flow simulation starting from :', flow(i)%nIterFlowStart
            write (*, wrtfmt1i) 'flow simulation ending   at   :', flow(i)%nIterFlowEnd
            if(is_any_energyeq) then
            write (*, wrtfmt1i) 'thermal simulation starting from :', thermo(i)%nIterThermoStart
            write (*, wrtfmt1i) 'thermal simulation ending   at   :', thermo(i)%nIterThermoEnd
            end if
          end do
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [ioparams]
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[io]' ) then
        read(inputUnit, *, iostat = ioerr) varname, cpu_nfre
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%ckpt_nfre
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%visu_idim
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%visu_nfre
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%visu_nskip(1:3)
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%stat_istart
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%stat_nskip(1:3)
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%is_record_xoutlet, domain(1)%is_read_xinlet
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%ndbfre, domain(1)%ndbstart, domain(1)%ndbend
        
        do i = 1, nxdomain
          domain(:)%ndbend = (domain(1)%ndbend - domain(1)%ndbstart + 1)/domain(1)%ndbfre * domain(1)%ndbfre + domain(1)%ndbstart - 1
          domain(i)%visu_nskip(1:3) = domain(1)%visu_nskip(1:3)
          domain(i)%stat_nskip(1:3) = domain(1)%stat_nskip(1:3) 
          if(domain(i)%is_stretching(2)) domain(i)%visu_nskip(2) = 1
          if(domain(i)%is_stretching(2)) domain(i)%stat_nskip(2) = 1
        end do

        if( nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1i) 'data check freqency :', domain(i)%ckpt_nfre
            write (*, wrtfmt1i) 'visu data dimensions :', domain(i)%visu_idim
            write (*, wrtfmt1i) 'visu data written freqency :', domain(i)%visu_nfre
            write (*, wrtfmt3i) 'visu data skips in xyz :', domain(i)%visu_nskip(1:3)
            write (*, wrtfmt1i) 'statistics written from :', domain(i)%stat_istart
            write (*, wrtfmt3i) 'statistics skips in xyz :', domain(i)%stat_nskip(1:3)
            write (*, wrtfmt1l) 'recording outlet plane? :', domain(1)%is_record_xoutlet
            write (*, wrtfmt1l) 'reading inlet plane? :', domain(1)%is_read_xinlet
            write (*, wrtfmt1i) 'reading/recording plane freqency :', domain(1)%ndbfre
            write (*, wrtfmt2i) 'reading/recording plane period (start-end):', domain(1)%ndbstart, domain(1)%ndbend
            if(domain(1)%is_record_xoutlet .and. domain(1)%ndbstart < domain(1)%stat_istart) &
            call Print_warning_msg('recording outlet plane data before starting statistics!')
          end do
        end if
      !----------------------------------------------------------------------------------------------------------
      ! [probe]
      !----------------------------------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[probe]' ) then
        do i = 1, nxdomain
          read(inputUnit, *, iostat = ioerr) varname, itmp
          domain(i)%proben = itmp
          if(domain(i)%proben > 0) then
            allocate( domain(i)%probexyz(3, itmp))
            !if( nrank == 0) !write (*, wrtfmt1i) '------For the domain-x------ ', i
            do j = 1, domain(i)%proben
              read(inputUnit, *, iostat = ioerr) varname, domain(i)%probexyz(1:3, j) 
              if(domain(i)%probexyz(1, j) > domain(i)%lxx) then
                call Print_warning_msg('probed points x > lx_max, adjusted.')
                domain(i)%probexyz(1, j) = domain(i)%lxx / real(domain(i)%proben + 1, WP) * real(j, WP)
              end if
              if(domain(i)%probexyz(2, j) > domain(i)%lyt .or. domain(i)%probexyz(2, j) < domain(i)%lyb) then
                call Print_warning_msg('probed points y not in (lyb, lyt), adjusted.')
                domain(i)%probexyz(2, j) = (domain(i)%lyt - domain(i)%lyb) / real(domain(i)%proben + 1, WP) * real(j, WP)
              end if
              if(domain(i)%probexyz(3, j) > domain(i)%lzz) then
                call Print_warning_msg('probed points z > lz_max, adjusted.')
                domain(i)%probexyz(3, j) = domain(i)%lzz / real(domain(i)%proben + 1, WP) * real(j, WP)
              end if
              if( nrank == 0) write (*, wrtfmt3r) 'probed points x, y, z :', domain(i)%probexyz(1:3, j) 
            end do 
          end if
        end do
      else
        exit
      end if
    end do
    !----------------------------------------------------------------------------------------------------------
    ! end of reading, clearing dummies
    !----------------------------------------------------------------------------------------------------------
    if(.not.IS_IOSTAT_END(ioerr)) &
    call Print_error_msg( 'Problem reading '//flinput // &
    'in Subroutine: '// "Read_general_input")

    close(inputUnit)

    if(allocated(itmpx)) deallocate(itmpx)
    if(allocated(rtmpx)) deallocate(rtmpx)
    !----------------------------------------------------------------------------------------------------------
    ! convert the input dimensional temperature/heat flux into undimensional
    !----------------------------------------------------------------------------------------------------------
    do i = 1, nxdomain
      if(.not. is_any_energyeq) then
        domain(i)%ibcx_nominal(1:2, 5) = domain(i)%ibcx_nominal(1:2, 1)
        domain(i)%ibcy_nominal(1:2, 5) = domain(i)%ibcy_nominal(1:2, 2)
        domain(i)%ibcz_nominal(1:2, 5) = domain(i)%ibcz_nominal(1:2, 3)
      end if
      call config_calc_basic_ibc(domain(i))
      call config_calc_eqs_ibc(domain(i))
    end do 
    !----------------------------------------------------------------------------------------------------------
    ! set up constant for time step marching 
    !----------------------------------------------------------------------------------------------------------
    do i = 1, nxdomain

      !option 1: Kim & Moin 1982
      ! domain(i)%sigma1p = ZERO
      ! domain(i)%sigma2p = ONE

      !option 2: to set up pressure treatment, for O(dt^2)
      !domain(i)%sigma1p = ONE
      !domain(i)%sigma2p = HALF
  
      !option 3: to set up pressure treatment, for O(dt)
      domain(i)%sigma1p = ONE
      domain(i)%sigma2p = ONE
  
      if(domain(i)%iTimeScheme == ITIME_RK3     .or. &
         domain(i)%iTimeScheme == ITIME_RK3_CN) then
        
        domain(i)%nsubitr = 3
        domain(i)%tGamma(0) = ONE
        domain(i)%tGamma(1) = EIGHT / FIFTEEN
        domain(i)%tGamma(2) = FIVE / TWELVE
        domain(i)%tGamma(3) = THREE * QUARTER
  
        domain(i)%tZeta (0) = ZERO
        domain(i)%tZeta (1) = ZERO
        domain(i)%tZeta (2) = - SEVENTEEN / SIXTY
        domain(i)%tZeta (3) = - FIVE / TWELVE
  
      else if (domain(i)%iTimeScheme == ITIME_AB2) then !Adams-Bashforth
  
        domain(i)%nsubitr = 1
        domain(i)%tGamma(0) = ONE
        domain(i)%tGamma(1) = ONEPFIVE
        domain(i)%tGamma(2) = ZERO
        domain(i)%tGamma(3) = ZERO
  
        domain(i)%tZeta (0) = ZERO
        domain(i)%tZeta (1) = -HALF
        domain(i)%tZeta (2) = ZERO
        domain(i)%tZeta (3) = ZERO
  
      else 
  
        domain(i)%nsubitr = 0
        domain(i)%tGamma(:) = ZERO
        domain(i)%tZeta (:) = ZERO
  
      end if 
      
      domain(i)%tAlpha(0:3) = domain(i)%tGamma(0:3) + domain(i)%tZeta(0:3)

    end do

    if(nrank == 0) call Print_debug_end_msg()

    return
  end subroutine

end module


!==========================================================================================================
!==========================================================================================================
module apx_prerun_mod
  use parameters_constant_mod
  use math_mod
  use input_general_mod
  use wtformat_mod
  use mpi_mod
  implicit none


  real(WP), parameter :: dxplus_max = 10.0_WP
  real(WP), parameter :: dzplus_max = 5.0_WP
  real(WP), parameter :: dyplus_max = 1.0_WP
  real(WP), parameter :: Cflmax = 0.714_WP
  real(WP), parameter :: Ctmmax = 0.1_WP
  real(WP), save :: dymax, dymin, rmin, rmax, Re_tau, u_tau

  private :: solve_Prandtl_vonKarman_eq_for_cf
  private :: estimate_skin_friction_factor

  public :: estimate_temporal_resolution
  public :: estimate_spacial_resolution

contains

!==========================================================================================================
  subroutine solve_Prandtl_vonKarman_eq_for_cf(cf, Re, icase)
    implicit none
    real(WP), intent(in)  :: Re
    integer, intent(in)   :: icase
    real(WP), intent(out) :: cf
    real(8) :: Cf_new, f, df, tol, a, b
    integer :: i, max_iter


    if (icase == ICASE_ANNULAR .or. icase == ICASE_PIPE) then
      a = 2.0_WP
      b = -0.8_WP
    else if (icase == ICASE_CHANNEL) then
      a = 2.12_WP
      b = -0.65_WP
    else
      a = 2.12_WP
      b = -0.65_WP
    end if

    ! Initial guess for Cf
    Cf = 0.005d0

    ! Convergence criteria
    tol = 1.0d-6
    max_iter = 50

    ! Iterative Newton-Raphson method
    do i = 1, max_iter
      f = 1.0d0 / sqrt(Cf) - a * log10(Re * sqrt(Cf)) + b
      df = -0.5d0 / (Cf**1.5d0) - (a / (log(10.0d0) * (Re * sqrt(Cf)) * 2.0d0 * sqrt(Cf)))
      ! Update Cf
      Cf_new = Cf - f / df
      ! Check for convergence
      if (abs(Cf_new - Cf) < tol) then
        exit
      end if
      Cf = Cf_new
    end do
    return

  end subroutine
!==========================================================================================================
  subroutine estimate_skin_friction_factor(cf, Re, icase)
    implicit none
    real(WP), intent(in)  :: Re
    integer, intent(in)   :: icase
    real(WP), intent(out) :: cf

    if(icase == ICASE_PIPE .or. &
       icase == ICASE_ANNULAR) then
      if(Re < 2300.0_WP) then
        ! laminar
        cf = 64.0_WP/Re
      else if (Re < 3.0e4_WP) then
        ! the Blasius relation
        cf = 0.316_WP * Re**(-0.25_WP)
      else if (Re < 1.0e6_WP) then
        ! the McAdmas relation
        cf = 0.814_WP * Re**(-0.2_WP)
      else
        !  the Prandtl–von Kármán equation
        call solve_Prandtl_vonKarman_eq_for_cf(cf, Re, icase)
      end if
    else if (icase == ICASE_CHANNEL) then
      if(Re < 1.0e4_WP) then
        ! laminar
        cf = 0.079_WP * Re**(-0.25_WP)
      else
       !  the Prandtl–von Kármán equation
       call solve_Prandtl_vonKarman_eq_for_cf(cf, Re, icase)
      end if
    else 
      cf = MAXP
    end if
    return
  end subroutine
!==========================================================================================================
  subroutine estimate_spacial_resolution(fl, dm, opt_mh)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),  intent(in)  :: fl
    type(t_mhd),   intent(in), optional :: opt_mh
    ! local variables
    real(WP) :: dx_max, dy_max, dz_max
    real(WP) :: cf, dy1, dy2, dy3, dy32, dy33
    real(WP) :: yplus1, yplus2, yplus3, dxplus, dzplus, dzplus2
    real(WP) :: ha_bl, ha_bl_plus
    real(WP) :: growth_rate1, growth_rate2
    integer  :: n_pnts_ha
    integer  :: nx_min, ny_min, nz_min
    
    ! excludes non-wall bcs
    if(nrank /= 0) return
    if(dm%icase /= ICASE_PIPE .and. &
       dm%icase /= ICASE_ANNULAR .and. &
       dm%icase /= ICASE_CHANNEL) return
    call Print_note_msg("DNS MESH RESOLUTION ASSESSMENT")
    call Print_note_msg("Recommended values based on empirical correlations in [apx_prerun_mod]")
    call Print_debug_mid_msg("Domain Length Check")
    write(*, wrtfmt2r) 'Streamwise (x): Current | Recom. min:', dm%lxx, TWOPI
    if(dm%icase == ICASE_CHANNEL) then
      write(*, '(A)') '  Note: Lx ≥ 2π for channel flow (≈4π preferred for large-scale structures)'
    else if(dm%icase == ICASE_PIPE) then
      write(*, '(A)') '  Note: Lx ≥ 2π for pipe flow (≈8-10 pipe diameters)'
    end if
    if(dm%icoordinate == ICARTESIAN) then
      write(*, wrtfmt2r) 'Spanwise (z):   Current | Recom. min:', dm%lzz, PI
      write(*, '(A)') '  Note: Lz ≥ π for adequate spanwise correlation'
    end if
    ! estimate Re_tau and u_tau
    rmax = ONE
    rmin = ONE
    if(dm%icoordinate == ICYLINDRICAL) then 
      rmin = dm%yc(1)
      rmax = dm%yp(dm%np(2))
    end if
    call estimate_skin_friction_factor(cf, fl%ren, dm%icase)
    Re_tau = fl%ren * sqrt_wp(cf/TWO)
    if(dm%icase == ICASE_PIPE) Re_tau = Re_tau / TWO
    u_tau = Re_tau/fl%ren
    call Print_debug_mid_msg("Flow Parameters:")
    write(*, wrtfmt1r) 'Friction Reynolds number (Re_tau):', Re_tau
    write(*, wrtfmt1r) 'Friction velocity (u_tau)        :', u_tau
    write(*, wrtfmt1r) 'Skin friction coefficient (cf)   :', cf
    ! calcualte current mesh spacing in normal units
    dy1 = dm%yp(2) - dm%yp(1)
    dy2 = dm%yp(dm%np(2)/2) - dm%yp(dm%np(2)/2-1)
    dy3 = dm%yp(dm%np(2)) - dm%yp(dm%np(2)-1)
    dy32 = dm%yp(dm%np(2)-1) - dm%yp(dm%np(2)-2)
    dy33 = dm%yp(dm%np(2)-2) - dm%yp(dm%np(2)-3)
    ! cacluate mesh spacing in wall units
    yplus1 = Re_tau * dy1
    yplus2 = Re_tau * dy2
    yplus3 = Re_tau * dy3
    dxplus = Re_tau * ( dm%h(1) )
    dzplus = Re_tau * ( dm%h(3) ) * rmax
    if(dm%icoordinate == ICYLINDRICAL) then 
      dzplus2 = Re_tau * ( dm%h(3) ) * rmin
    end if
    !
    call Print_debug_mid_msg("Current Mesh Resolution (wall units)")
    write(*, '(A)')    '    Wall-normal direction (y):'
    write(*, wrtfmt1r) '    Δy+ at near wall (j=1)         :', yplus1
    write(*, wrtfmt1r) '    Δy+ at far wall (j=np)         :', yplus3
    write(*, wrtfmt1r) '    Δy+ at center (j=np/2)         :', yplus2
    !
    ! calculate grid stretching rates
    growth_rate1 = (abs_wp(dy33 - dy32) + MIN(dy33, dy32)) / MIN(dy33, dy32)
    growth_rate2 = (abs_wp(dy32 - dy3 ) + MIN(dy32, dy3) ) / MIN(dy32, dy3)
    call Print_debug_mid_msg("Grid Stretching Assessment")
    write(*, wrtfmt2r) 'Growth rate at wall (layers 2-3 and 3-4):', growth_rate1, growth_rate2
    write(*, '(A)')    '  Recommended: Growth rate < 1.2-1.3 for DNS'
    if(growth_rate1 > 1.3_WP .or. growth_rate2 > 1.3_WP) then
      call Print_warning_msg("Grid stretching is too aggressive! Reduce stretching factor.")
      write(*, '(A)') '    High stretching can cause numerical errors and inaccurate statistics'
    else if(growth_rate1 > 1.2_WP .or. growth_rate2 > 1.2_WP) then
      write(*, '(A)') '  ⚠ Caution: Growth rate approaching upper limit'
    else
      write(*, '(A)') '  ✓ Grid stretching is acceptable'
    end if
    !
    if(yplus1 > ONE) then
      call Print_warning_msg("Wall spacing too large! Δy+ should be ≤ 1.0 for DNS")
      write(*, '(A,F6.2,A)') '    Current Δy+ = ', yplus1, ' → Increase Ny or adjust stretching'
    else if(yplus1 < 0.5_WP) then
      write(*, '(A)') '  ✓ Excellent wall resolution (Δy+ < 0.5)'
    else
      write(*, '(A)') '  ✓ Acceptable wall resolution (Δy+ ≤ 1.0)'
    end if
    ! MHD
    if (dm%is_mhd) then
      if(.not. present(opt_mh)) call Print_error_msg("Error. Opt_mhd is required.")
      ha_bl = 1.0_WP / opt_mh%NHartmn
      ha_bl_plus = ha_bl * Re_tau
      n_pnts_ha = count( dm%yp <= (ha_bl - 1.0_WP) )
      write(*, '(A)') repeat('-', 80)
      call Print_debug_mid_msg("At the current mesh configuration: MHD")
      write(*, wrtfmt1r) 'MHD boundary layer thickness (δ_Ha)      :', ha_bl
      write(*, wrtfmt1r) 'MHD boundary layer thickness (δ_Ha+)     :', ha_bl_plus
      write(*, wrtfmt1i) 'Grid points in MHD boundary layer        :', n_pnts_ha
      if (n_pnts_ha < 10) then
        call Print_warning_msg("Insufficient grid points in MHD boundary layer. Recommend at least 10 points.")
      end if
    end if
    ! Calculate recommended minimum mesh resolution
    dymax = MAX(dy1, dy2, dy3)
    dymin = MIN(dy1, dy2, dy3)
    !
    dx_max = dxplus_max / Re_tau
    dz_max = dzplus_max / Re_tau / rmax
    dy_max = dyplus_max / Re_tau
    if (dm%is_thermo) then
      write(*,*) 'pr', fluidparam%ftp0ref%Pr
      dy_max = MIN(dyplus_max, ONE/fluidparam%ftp0ref%Pr)  / Re_tau
    end if
    !
    nx_min = ceiling(dm%lxx/dx_max)
    nz_min = ceiling(dm%lzz/dz_max)
    ny_min = ceiling(dm%nc(2) * dymin / dy_max)
    !
    write(*, '(A)') ''
    write(*, '(A)')      '  Streamwise direction (x):'
    write(*, wrtfmt1r)   '    Δx+                            :', dxplus
    write(*, '(A,F6.1)') '    Recommended: Δx+ ≤ ', dxplus_max
    
    write(*, '(A)') ''
    write(*, '(A)')      '  Spanwise direction (z):'
    write(*, wrtfmt1r)   '    Δz+ (at outer wall)            :', dzplus
    if(dm%icoordinate == ICYLINDRICAL) then
      write(*, wrtfmt1r) '    Δz+ (at inner wall)            :', dzplus2
    end if
    write(*, '(A,F6.1)') '    Recommended: Δz+ ≤ ', dzplus_max
    write(*, '(A)') repeat('-', 80)
    !
    call Print_debug_mid_msg("Mesh Resolution Summary")
    write(*, '(A)') '  Current mesh:'
    write(*, wrtfmt3i) '    Grid points (Nx, Ny, Nz)      :', dm%nc(1), dm%nc(2), dm%nc(3)
    write(*, wrtfmt1il)'    Total cells                    :', dm%nc(1) * dm%nc(2) * dm%nc(3)
    
    write(*, '(A)') ''
    write(*, '(A)') '  Recommended minimum mesh for DNS:'
    write(*, wrtfmt3i) '    Grid points (Nx, Ny, Nz)      :', nx_min, ny_min, nz_min
    write(*, wrtfmt1il)'    Total cells                    :', nx_min * ny_min * nz_min
    
    ! Mesh adequacy check
    if(dm%nc(1) >= nx_min .and. dm%nc(2) >= ny_min .and. dm%nc(3) >= nz_min) then
      write(*, '(A)') '  ✓ Current mesh meets minimum DNS resolution requirements'
    else
      call Print_warning_msg("Current mesh is below recommended DNS resolution!")
      if(dm%nc(1) < nx_min) write(*, '(A,I0,A,I0)') '    Increase Nx: ', dm%nc(1), ' → ', nx_min
      if(dm%nc(2) < ny_min) write(*, '(A,I0,A,I0)') '    Increase Ny: ', dm%nc(2), ' → ', ny_min
      if(dm%nc(3) < nz_min) write(*, '(A,I0,A,I0)') '    Increase Nz: ', dm%nc(3), ' → ', nz_min
    end if
    
    write(*, '(A)') repeat('=', 80)
    write(*, '(A)') ''

  return 
  end subroutine

!==========================================================================================================
  subroutine estimate_temporal_resolution(fl, dm)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),  intent(in)  :: fl

    real(WP) :: dt_max_cfl1, dt_max_cfl2, dt_max_phy, dxyz_max, dt_min
    real(WP) :: t_flth
    integer :: nt_cur, nt_est

    if(nrank /= 0) return
    if(dm%icase /= ICASE_PIPE .and. &
       dm%icase /= ICASE_ANNULAR .and. &
       dm%icase /= ICASE_CHANNEL) return

    ! dt limits
    dt_max_cfl1 = Cflmax * dm%h(1) / TWO
    dxyz_max = ONE / (dm%h(1)**2) + ONE /(dymin**2) + ONE/((rmin * dm%h(3))**2)
    dt_max_cfl2 = fl%ren / TWO / dxyz_max
    !\Delta t \approx 0.1 \frac{h^2}{\nu Re_{\tau}^2}
    dt_max_phy = Ctmmax *(fl%ren/Re_tau/Re_tau)! * dymin / dymax
    dt_min = MIN(dt_max_cfl1, dt_max_cfl2, dt_max_phy)
    call Print_debug_mid_msg("Estimating the temporal resolution (based on isothermal flow)")
    write(*, wrtfmt1e) 'current dt :', dm%dt
    write(*, wrtfmt1e) 'dt_max (convection CFL  ) :', dt_max_cfl1
    write(*, wrtfmt1e) 'dt_max (diffusion  CFL  ) :', dt_max_cfl2
    write(*, wrtfmt1e) 'dt_max (Kolmogorov limit) :', dt_max_phy
    write(*, wrtfmt1e) 'dt_max (dt+ = 0.1) :', 0.1_WP * fl%ren/Re_tau/Re_tau

    ! iteration 
    t_flth = dm%lxx / 1.2_wp 
    nt_cur = ceiling(t_flth / dm%dt)
    nt_est = ceiling(t_flth / dt_min)
    
    call Print_debug_mid_msg("Estimating the required time steps")
    write(*, wrtfmt1r)     'flow throught time :', t_flth
    write(*, wrtfmt1il1r)  '1-flthr iter. at the estimated dtmax   :', nt_est, dt_min
    write(*, wrtfmt1il1r)  '1-flthr iter. at the      current dt   :', nt_cur, dm%dt
    write(*, wrtfmt1il1r)  'rec.[25]-flthr iter. for statistics    :', nt_cur * 25, dm%dt
    if(dm%is_record_xoutlet .or. dm%is_read_xinlet) &
    write(*, wrtfmt1il1r)  'rec. [5]-flthr iter. for db recording  :', nt_cur * 5, dm%dt
    write(*, *)  "Note: Statistics can start from any iteration when using running average postprocessing. Otherwise:"
    write(*, wrtfmt1il1r)  'rec.[6]-flthr iter. before statistics  :', nt_cur * 6,  dm%dt

    call Print_debug_mid_msg("folder structure")
    write(*, *)  '1_data: all raw data(.bin), time averaged data(.bin) and time-space averaged data (.dat)'
    write(*, *)  '2_visu: visulisation script (.xdmf)'
    write(*, *)  '3_monitor: monitored bulk properties, probed points, and mass conservation'
    write(*, *)  '4_check: check mesh grid distribution, initial velocity profiles'

    call Print_debug_start_msg()
    return
  end subroutine


end module
