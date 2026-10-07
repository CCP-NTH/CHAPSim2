!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!==============================================================================
!> Source file input_general.f90.
!> Reading the input parameters from the given file.
!> Author: Wei Wang wei.wang@stfc.ac.uk
!> Date: 11-05-2022, checked.
!==============================================================================
!> Small input-parsing utility helpers.
module util_mod
contains
    !> Convert a character string to an integer and return the I/O status.
    !> - str (in): Input string.
    !> - ioerr (out): Fortran read status.
    !> Return: Parsed integer value when conversion succeeds.
    function int_from_string(str, ioerr)
        character(len=*), intent(in) :: str
        integer, intent(out) :: ioerr
        integer :: int_from_string
        read(str, *, iostat=ioerr) int_from_string
    end function int_from_string
end module util_mod
!==============================================================================
!> Read and validate the main CHAPSim input file.
!>
!> This module owns the user-facing `input_chapsim.ini` interpretation: process
!> switches, geometry, mesh, flow, thermal, MHD, boundary-condition, scheme, I/O,
!> and statistics settings. It also prints readable names for selected integer
!> IDs so run logs are easier to audit.
module input_general_mod
  use parameters_constant_mod
  use print_msg_mod
  implicit none
  logical :: is_prerun, is_postprocess
  logical :: input_section_reached_eof
  integer, parameter :: INPUT_SECTION_MAX = 128
  public  :: Read_input_parameters
  public  :: Read_input_parameters_target
  private :: apply_case_geometry_defaults
  private :: apply_mesh_stretching_defaults
  private :: get_name_case
  private :: get_name_cs
  private :: get_name_mesh
  private :: get_name_iacc
  private :: get_name_initial
  private :: get_name_fluid
  private :: get_name_fft
  private :: get_name_mstret
  private :: get_name_poisson_y_method
  private :: get_name_existing_output_policy
  private :: get_name_restart_data_layout
  private :: get_name_restart_history_mode
  private :: get_name_restart_clock
  private :: get_name_stat_visu_mode
  private :: open_clean_input_file
  private :: is_input_data_line
  private :: read_input_section
  private :: split_input_key_value
  private :: extract_input_value
  private :: lowercase
  private :: section_find_key
  private :: section_get_integer
  private :: section_get_integer_array
  private :: section_get_real
  private :: section_get_real_array
  private :: section_get_logical
  private :: read_optional_input_line
  private :: parse_gravity_vector
  private :: parse_existing_output_policy
  private :: parse_restart_data_layout
  private :: parse_restart_history_mode
  private :: parse_restart_clock
  private :: parse_stat_visu_mode
  private :: parse_icase
  private :: parse_initfl
  private :: parse_ifluid
  private :: parse_initm
  private :: parse_istret
  private :: parse_rstret
  private :: parse_iaccuracy
  private :: parse_idriven
  private :: parse_itimescheme
  private :: parse_iviscous
  private :: parse_LES_model
contains
!==============================================================================
  subroutine open_clean_input_file(filename, inputUnit)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(out) :: inputUnit
    character(len=512) :: input_line
    character(len=200) :: iotxt
    integer :: sourceUnit
    integer :: ioerr
    open(newunit=sourceUnit, file=trim(filename), status='old', action='read', &
         iostat=ioerr, iomsg=iotxt)
    if(ioerr /= 0) then
      call Print_error_msg('Error in opening the input file: '//trim(filename))
    end if
    open(newunit=inputUnit, status='scratch', action='readwrite', iostat=ioerr, &
         iomsg=iotxt)
    if(ioerr /= 0) then
      close(sourceUnit)
      call Print_error_msg('Error in preparing a clean copy of '//trim(filename))
    end if
    do
      read(sourceUnit, '(A)', iostat=ioerr) input_line
      if(ioerr /= 0) exit
      if(is_input_data_line(input_line)) write(inputUnit, '(A)') trim(input_line)
    end do
    close(sourceUnit)
    rewind(inputUnit)
    return
  end subroutine open_clean_input_file
!==============================================================================
  logical function is_input_data_line(input_line)
    implicit none
    character(len=*), intent(in) :: input_line
    character(len=len(input_line)) :: clean_line
    clean_line = adjustl(input_line)
    is_input_data_line = .false.
    if(len_trim(clean_line) == 0) return
    if(clean_line(1:1) == '#') return
    if(clean_line(1:1) == ';') return
    is_input_data_line = .true.
    return
  end function is_input_data_line
!==============================================================================
  subroutine read_input_section(inputUnit, section_name, keys, values, nentry)
    implicit none
    integer, intent(in) :: inputUnit
    character(len=*), intent(in) :: section_name
    character(len=*), intent(out) :: keys(:)
    character(len=*), intent(out) :: values(:)
    integer, intent(out) :: nentry
    character(len=512) :: input_line
    character(len=512) :: clean_line
    integer :: ioerr
    keys = ''
    values = ''
    nentry = 0
    do
      read(inputUnit, '(A)', iostat=ioerr) input_line
      if(ioerr /= 0) then
        input_section_reached_eof = .true.
        exit
      end if
      clean_line = adjustl(input_line)
      if(len_trim(clean_line) == 0) cycle
      if(clean_line(1:1) == '[') then
        backspace(inputUnit)
        exit
      end if
      if(nentry >= size(keys)) &
      call Print_error_msg('Too many entries in input section '//trim(section_name))
      nentry = nentry + 1
      call split_input_key_value(input_line, keys(nentry), values(nentry))
      if(len_trim(keys(nentry)) == 0) &
      call Print_error_msg('Failed to parse an input key in section '//trim(section_name))
    end do
    return
  end subroutine read_input_section
!==============================================================================
  subroutine split_input_key_value(input_line, key, value)
    implicit none
    character(len=*), intent(in) :: input_line
    character(len=*), intent(out) :: key
    character(len=*), intent(out) :: value
    character(len=512) :: clean_line
    integer :: eqpos
    integer :: sep
    key = ''
    value = ''
    clean_line = adjustl(input_line)
    eqpos = index(clean_line, '=')
    if(eqpos > 0) then
      key = adjustl(clean_line(1:eqpos - 1))
      value = adjustl(clean_line(eqpos + 1:))
    else
      sep = scan(clean_line, ' '//char(9))
      if(sep > 0) then
        key = adjustl(clean_line(1:sep - 1))
        value = adjustl(clean_line(sep + 1:))
      else
        key = adjustl(clean_line)
      end if
    end if
    key = trim(key)
    value = trim(value)
    return
  end subroutine split_input_key_value
!==============================================================================
  subroutine extract_input_value(input_text, value)
    implicit none
    character(len=*), intent(in) :: input_text
    character(len=*), intent(out) :: value
    character(len=512) :: key

    call split_input_key_value(input_text, key, value)
    if(len_trim(value) == 0) value = trim(key)

    return
  end subroutine extract_input_value
!==============================================================================
  function lowercase(input_text) result(output_text)
    implicit none
    character(len=*), intent(in) :: input_text
    character(len=len(input_text)) :: output_text
    integer :: i
    integer :: code

    output_text = input_text
    do i = 1, len(input_text)
      code = iachar(input_text(i:i))
      if(code >= iachar('A') .and. code <= iachar('Z')) then
        output_text(i:i) = achar(code + iachar('a') - iachar('A'))
      end if
    end do

    return
  end function lowercase
!==============================================================================
  integer function section_find_key(keys, nentry, key_name) result(idx)
    implicit none
    character(len=*), intent(in) :: keys(:)
    integer, intent(in) :: nentry
    character(len=*), intent(in) :: key_name
    integer :: i
    idx = 0
    do i = 1, nentry
      if(trim(keys(i)) == trim(key_name)) then
        idx = i
        return
      end if
    end do
    return
  end function section_find_key
!==============================================================================
  subroutine section_get_integer(keys, values, nentry, section_name, key_name, value)
    implicit none
    character(len=*), intent(in) :: keys(:)
    character(len=*), intent(in) :: values(:)
    integer, intent(in) :: nentry
    character(len=*), intent(in) :: section_name
    character(len=*), intent(in) :: key_name
    integer, intent(out) :: value
    integer :: idx
    integer :: ioerr
    idx = section_find_key(keys, nentry, key_name)
    if(idx == 0) &
    call Print_error_msg('Missing required key '//trim(key_name)//' in section '//trim(section_name))
    read(values(idx), *, iostat=ioerr) value
    if(ioerr /= 0) &
    call Print_error_msg('Invalid integer value for key '//trim(key_name)//' in section '//trim(section_name))
    return
  end subroutine section_get_integer
!==============================================================================
  subroutine section_get_integer_array(keys, values, nentry, section_name, key_name, value)
    implicit none
    character(len=*), intent(in) :: keys(:)
    character(len=*), intent(in) :: values(:)
    integer, intent(in) :: nentry
    character(len=*), intent(in) :: section_name
    character(len=*), intent(in) :: key_name
    integer, intent(out) :: value(:)
    integer :: idx
    integer :: ioerr
    idx = section_find_key(keys, nentry, key_name)
    if(idx == 0) &
    call Print_error_msg('Missing required key '//trim(key_name)//' in section '//trim(section_name))
    read(values(idx), *, iostat=ioerr) value
    if(ioerr /= 0) &
    call Print_error_msg('Invalid integer array value for key '//trim(key_name)//' in section '//trim(section_name))
    return
  end subroutine section_get_integer_array
!==============================================================================
  subroutine section_get_real(keys, values, nentry, section_name, key_name, value)
    implicit none
    character(len=*), intent(in) :: keys(:)
    character(len=*), intent(in) :: values(:)
    integer, intent(in) :: nentry
    character(len=*), intent(in) :: section_name
    character(len=*), intent(in) :: key_name
    real(WP), intent(out) :: value
    integer :: idx
    integer :: ioerr
    idx = section_find_key(keys, nentry, key_name)
    if(idx == 0) &
    call Print_error_msg('Missing required key '//trim(key_name)//' in section '//trim(section_name))
    read(values(idx), *, iostat=ioerr) value
    if(ioerr /= 0) &
    call Print_error_msg('Invalid real value for key '//trim(key_name)//' in section '//trim(section_name))
    return
  end subroutine section_get_real
!==============================================================================
  subroutine section_get_real_array(keys, values, nentry, section_name, key_name, value)
    implicit none
    character(len=*), intent(in) :: keys(:)
    character(len=*), intent(in) :: values(:)
    integer, intent(in) :: nentry
    character(len=*), intent(in) :: section_name
    character(len=*), intent(in) :: key_name
    real(WP), intent(out) :: value(:)
    integer :: idx
    integer :: ioerr
    idx = section_find_key(keys, nentry, key_name)
    if(idx == 0) &
    call Print_error_msg('Missing required key '//trim(key_name)//' in section '//trim(section_name))
    read(values(idx), *, iostat=ioerr) value
    if(ioerr /= 0) &
    call Print_error_msg('Invalid real array value for key '//trim(key_name)//' in section '//trim(section_name))
    return
  end subroutine section_get_real_array
!==============================================================================
  subroutine section_get_logical(keys, values, nentry, section_name, key_name, value)
    implicit none
    character(len=*), intent(in) :: keys(:)
    character(len=*), intent(in) :: values(:)
    integer, intent(in) :: nentry
    character(len=*), intent(in) :: section_name
    character(len=*), intent(in) :: key_name
    logical, intent(out) :: value
    integer :: idx
    integer :: ioerr
    idx = section_find_key(keys, nentry, key_name)
    if(idx == 0) &
    call Print_error_msg('Missing required key '//trim(key_name)//' in section '//trim(section_name))
    read(values(idx), *, iostat=ioerr) value
    if(ioerr /= 0) &
    call Print_error_msg('Invalid logical value for key '//trim(key_name)//' in section '//trim(section_name))
    return
  end subroutine section_get_logical
!==============================================================================
  subroutine read_optional_input_line(inputUnit, expected_name, input_line, is_present)
    implicit none
    integer,          intent(in)  :: inputUnit
    character(len=*), intent(in)  :: expected_name
    character(len=*), intent(out) :: input_line
    logical,          intent(out) :: is_present
    character(len=80) :: varname
    integer :: ioerr
    is_present = .false.
    input_line = ''
    read(inputUnit, '(A)', iostat = ioerr) input_line
    if(ioerr /= 0) return
    read(input_line, *, iostat = ioerr) varname
    if(ioerr == 0) then
      if(trim(varname) == trim(expected_name) .or. &
         trim(varname) == trim(expected_name)//'=') then
        is_present = .true.
        return
      end if
    end if
    backspace(inputUnit)
    return
  end subroutine read_optional_input_line
!==============================================================================
  subroutine parse_existing_output_policy(input_line, policy)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: policy
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('overwrite', '0')
      policy = OUTPUT_POLICY_OVERWRITE
    case('skip', '1')
      policy = OUTPUT_POLICY_SKIP
    case('rename_existing', 'rename', '2')
      policy = OUTPUT_POLICY_RENAME_EXISTING
    case default
      call Print_error_msg( &
        'Invalid existing_output_policy. Supported values: overwrite, skip, rename_existing.')
    end select
    return
  end subroutine parse_existing_output_policy
!==============================================================================
  subroutine parse_restart_data_layout(input_line, layout)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: layout
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('per_field', 'per-field', '0')
      layout = RESTART_LAYOUT_PER_FIELD
    case('bundled', 'bundle', '1')
      layout = RESTART_LAYOUT_BUNDLED
    case default
      call Print_error_msg( &
        'Invalid restart data layout. Supported values: per_field, bundled.')
    end select
    return
  end subroutine parse_restart_data_layout
!==============================================================================
  subroutine parse_restart_history_mode(input_line, mode)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: mode
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('exact', '0')
      mode = RESTART_HISTORY_EXACT
    case('compact', '1')
      mode = RESTART_HISTORY_COMPACT
    case default
      call Print_error_msg( &
        'Invalid restart_history_mode. Supported values: exact, compact.')
    end select
    return
  end subroutine parse_restart_history_mode
!==============================================================================
  subroutine parse_restart_clock(input_line, mode)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: mode
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('continue', 'continuation', '0')
      mode = RESTART_CLOCK_CONTINUE
    case('reset', 'initial', '1')
      mode = RESTART_CLOCK_RESET
    case default
      call Print_error_msg( &
        'Invalid restart_clock. Supported values: continue, reset.')
    end select
    return
  end subroutine parse_restart_clock
!==============================================================================
  subroutine parse_stat_visu_mode(input_line, mode)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: mode
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('all', '0')
      mode = STAT_VISU_MODE_ALL
    case('tsp_only', 'tsp-only', 'tsp', '1')
      mode = STAT_VISU_MODE_TSP_ONLY
    case default
      call Print_error_msg( &
        'Invalid stat_visu_mode. Supported values: all, tsp_only.')
    end select
    return
  end subroutine parse_stat_visu_mode
!==============================================================================
  subroutine parse_icase(input_line, icase)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: icase
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('channel', '1')
      icase = ICASE_CHANNEL
    case('pipe', '2')
      icase = ICASE_PIPE
    case('annular', '3')
      icase = ICASE_ANNULAR
    case('tgv3d', '4')
      icase = ICASE_TGV3D
    case('duct', '5')
      icase = ICASE_DUCT
    case('tgv2d', '6')
      icase = ICASE_TGV2D
    case('burgers', '7')
      icase = ICASE_BURGERS
    case('algtest', '8')
      icase = ICASE_ALGTEST
    case('others', '0')
      icase = ICASE_OTHERS
    case default
      call Print_error_msg( &
        'Invalid icase. Supported values: channel, pipe, annular, tgv3d, duct, tgv2d, burgers, algtest, others.')
    end select
    return
  end subroutine parse_icase
!==============================================================================
!> \brief Parse one side of an electrical boundary condition from [mhd].
!>
!> The electric potential carries its own boundary condition rather than
!> inheriting the pressure one. The two coincide in every case shipped today, but
!> they are independent physics: an insulating wall (j.n = 0) and a conducting
!> wall (ep = const) differ, and neither is implied by the pressure BC. Omitting
!> the key gives EBC_INHERIT, which reproduces the historical behaviour exactly.
!==============================================================================
  subroutine parse_ebc_side(value, ebc)
    implicit none
    character(len=*), intent(in) :: value
    integer, intent(out) :: ebc
    select case(trim(lowercase(adjustl(value))))
    case('inherit', '-1')
      ebc = EBC_INHERIT
    case('insulating', '1')
      ebc = EBC_INSULATING
    case('conducting', '2')
      ebc = EBC_CONDUCTING
    case('periodic', '3')
      ebc = EBC_PERIODIC
    case default
      call Print_error_msg( &
        'Invalid electrical bc. Supported values: inherit, insulating, conducting, periodic.')
    end select
    return
  end subroutine parse_ebc_side
!==============================================================================
  subroutine parse_ebc(keys, values, nentry, section_name, key_name, ebc)
    implicit none
    character(len=*), intent(in) :: keys(:)
    character(len=*), intent(in) :: values(:)
    integer,          intent(in) :: nentry
    character(len=*), intent(in) :: section_name
    character(len=*), intent(in) :: key_name
    integer,       intent(inout) :: ebc(2)
    character(len=80) :: str(2)
    integer :: idx, ioerr, n
    idx = section_find_key(keys, nentry, key_name)
    if(idx == 0) return ! key absent: keep the EBC_INHERIT default
    read(values(idx), *, iostat = ioerr) str(1), str(2)
    if(ioerr /= 0) call Print_error_msg( &
      'Key '//trim(key_name)//' in section '//trim(section_name)//' needs two values, one per side.')
    do n = 1, 2
      call parse_ebc_side(str(n), ebc(n))
    end do
    return
  end subroutine parse_ebc
!==============================================================================
  subroutine parse_initfl(input_line, initfl)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: initfl
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('restart', '0')
      initfl = INIT_RESTART
    case('random', '2')
      initfl = INIT_RANDOM
    case('inlet', '3')
      initfl = INIT_INLET
    case('const', 'gvconst', '4')
      initfl = INIT_GVCONST
    case('poiseuille', '5')
      initfl = INIT_POISEUILLE
    case('function', '6')
      initfl = INIT_FUNCTION
    case default
      call Print_error_msg( &
        'Invalid initfl. Supported values: restart, random, inlet, const, poiseuille, function.')
    end select
    return
  end subroutine parse_initfl
!==============================================================================
  subroutine parse_ifluid(input_line, ifluid)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: ifluid
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('scp_water', '1')
      ifluid = ISCP_WATER
    case('scp_co2', '2')
      ifluid = ISCP_CO2
    case('sodium', '3')
      ifluid = ILIQUID_SODIUM
    case('lead', '4')
      ifluid = ILIQUID_LEAD
    case('bismuth', '5')
      ifluid = ILIQUID_BISMUTH
    case('lbe', '6')
      ifluid = ILIQUID_LBE
    case('water', '7')
      ! The enumerator is reserved but no property correlation for ordinary
      ! liquid water exists. Rejected here rather than in Buildup_fluidparam,
      ! where it used to fall through to liquid sodium without a word.
      call Print_error_msg( &
        'ifluid = water (ordinary liquid water) is not implemented. Use scp_water for the NIST water table.')
    case('lithium', '8')
      ifluid = ILIQUID_LITHIUM
    case('flibe', '9')
      ifluid = ILIQUID_FLIBE
    case('pbli', '10')
      ifluid = ILIQUID_PBLI
    case default
      call Print_error_msg( &
        'Invalid ifluid. Supported values: scp_water, scp_co2, sodium, lead, bismuth, lbe, lithium, flibe, pbli.')
    end select
    return
  end subroutine parse_ifluid
!==============================================================================
  subroutine parse_initm(input_line, initm)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: initm
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('restart', '0')
      initm = INIT_RESTART
    case('const', 'gvconst', '4')
      initm = INIT_GVCONST
    case('function', '6')
      initm = INIT_FUNCTION
    case('linear', 'gvbcln', '7')
      initm = INIT_GVBCLN
    case('smooth', 'gvbcsmooth', '8')
      initm = INIT_GVBCSMOOTH
    case default
      call Print_error_msg( &
        'Invalid inittm. Supported values: restart, const, function, linear, smooth.')
    end select
    return
  end subroutine parse_initm
!==============================================================================
  subroutine parse_istret(input_line, istret)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: istret
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('no', 'none', '0')
      istret = ISTRET_NO
    case('centre', 'center', '1')
      istret = ISTRET_CENTRE
    case('2sides', 'twosides', '2')
      istret = ISTRET_2SIDES
    case('bottom', '3')
      istret = ISTRET_BOTTOM
    case('top', '4')
      istret = ISTRET_TOP
    case default
      call Print_error_msg( &
        'Invalid istret. Supported values: no, centre, 2sides, bottom, top.')
    end select
    return
  end subroutine parse_istret
!==============================================================================
  subroutine parse_rstret(input_line, mstret, rstret)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: mstret
    real(WP), intent(out) :: rstret
    character(len=512) :: value
    character(len=80) :: mstret_str
    integer :: ioerr
    call extract_input_value(input_line, value)
    read(value, *, iostat=ioerr) mstret_str, rstret
    if(ioerr /= 0) call Print_error_msg( &
      'Invalid rstret. Expected: rstret= <method> <factor>. Methods: uniform, 3fmd, tanh, powl.')
    select case(trim(lowercase(adjustl(mstret_str))))
    case('none', 'uniform', '0')
      mstret = MSTRET_NONE
    case('3fmd', '1')
      mstret = MSTRET_3FMD
    case('tanh', '2')
      mstret = MSTRET_TANH
    case('powl', 'powerlaw', '3')
      mstret = MSTRET_POWL
    case default
      call Print_error_msg( &
        'Invalid rstret stretching method. Supported values: uniform, 3fmd, tanh, powl.')
    end select
    return
  end subroutine parse_rstret
!==============================================================================
  subroutine parse_iaccuracy(input_line, iaccuracy)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: iaccuracy
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('cd2', '1')
      iaccuracy = IACCU_CD2
    case('cd4', '2')
      iaccuracy = IACCU_CD4
    case('cp4', '3')
      iaccuracy = IACCU_CP4
    case('cp6', '4')
      iaccuracy = IACCU_CP6
    case default
      call Print_error_msg( &
        'Invalid iaccuracy. Supported values: cd2, cd4, cp4, cp6.')
    end select
    return
  end subroutine parse_iaccuracy
!==============================================================================
  subroutine parse_idriven(input_line, idriven)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: idriven
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('no', 'none', '0')
      idriven = IDRVF_NO
    case('x_massflux', 'x_mf', '1')
      idriven = IDRVF_X_MASSFLUX
    case('x_tauw', '2')
      idriven = IDRVF_X_TAUW
    case('x_dpdx', '3')
      idriven = IDRVF_X_DPDX
    case('z_massflux', 'z_mf', '4')
      idriven = IDRVF_Z_MASSFLUX
    case('z_tauw', '5')
      idriven = IDRVF_Z_TAUW
    case('z_dpdz', '6')
      idriven = IDRVF_Z_DPDZ
    case default
      call Print_error_msg( &
        'Invalid idriven. Supported values: no, x_massflux, x_tauw, x_dpdx, z_massflux, z_tauw, z_dpdz.')
    end select
    return
  end subroutine parse_idriven
!==============================================================================
  subroutine parse_itimescheme(input_line, itimescheme)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: itimescheme
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('euler', '0')
      itimescheme = ITIME_EULER
    case('ab2', '1')
      itimescheme = ITIME_AB2
    case('rk3_cn', '2')
      itimescheme = ITIME_RK3_CN
    case('rk3', '3')
      itimescheme = ITIME_RK3
    case default
      call Print_error_msg( &
        'Invalid itimescheme. Supported values: euler, ab2, rk3_cn, rk3.')
    end select
    return
  end subroutine parse_itimescheme
!==============================================================================
  subroutine parse_iviscous(input_line, iviscous)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: iviscous
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('explicit', '1')
      iviscous = IVIS_EXPLICIT
    case('semi_implicit', 'semimplt', '2')
      iviscous = IVIS_SEMIMPLT
    case default
      call Print_error_msg( &
        'Invalid iviscous. Supported values: explicit, semi_implicit.')
    end select
    return
  end subroutine parse_iviscous
!==============================================================================
  subroutine parse_LES_model(input_line, LES_model)
    implicit none
    character(len=*), intent(in) :: input_line
    integer, intent(out) :: LES_model
    character(len=80) :: value
    call extract_input_value(input_line, value)
    select case(trim(lowercase(adjustl(value))))
    case('none', 'dns', 'no', '0')
      LES_model = ILES_NONE
    case('wale', '1')
      LES_model = ILES_WALE
    case default
      call Print_error_msg( &
        'Invalid LES_model. Supported values: none, wale.')
    end select
    return
  end subroutine parse_LES_model
!==============================================================================
  subroutine parse_gravity_vector(line, gravity_vector)
    use udf_type_mod
    implicit none
    character(len=*), intent(in)  :: line
    real(WP),         intent(out) :: gravity_vector(NDIM)
    character(len=80) :: key
    integer  :: ioerr
    real(WP) :: gravity_norm
    real(WP) :: vector_read(NDIM)
    gravity_vector = ZERO
    vector_read = ZERO
    read(line, *, iostat=ioerr) key, vector_read(1:NDIM)
    if(ioerr == 0) then
      gravity_vector = vector_read
      gravity_norm = sqrt(sum(gravity_vector * gravity_vector))
      if(gravity_norm > MINP) gravity_vector = gravity_vector / gravity_norm
      return
    end if
    call Print_error_msg('Failed to read gravity vector. Use igravity= gx,gy,gz.')
    return
  end subroutine parse_gravity_vector
!==============================================================================
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
!==============================================================================
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
!==============================================================================
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
!==============================================================================
  function get_name_mstret(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case ( MSTRET_NONE)
      str = 'Uniform mesh; no stretching method.'
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
!==============================================================================
  function get_name_poisson_y_method(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case ( IPOISSON_Y_AUTO)
      str = 'auto'
    case ( IPOISSON_Y_FFT)
      str = 'fft'
    case ( IPOISSON_Y_TDMA)
      str = 'tdma'
    case default
      call Print_error_msg('The requested Poisson y-direction method is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
  function get_name_existing_output_policy(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case (OUTPUT_POLICY_OVERWRITE)
      str = 'overwrite'
    case (OUTPUT_POLICY_SKIP)
      str = 'skip'
    case (OUTPUT_POLICY_RENAME_EXISTING)
      str = 'rename_existing'
    case default
      call Print_error_msg('The requested existing-output policy is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
  function get_name_restart_data_layout(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case (RESTART_LAYOUT_PER_FIELD)
      str = 'per_field'
    case (RESTART_LAYOUT_BUNDLED)
      str = 'bundled'
    case default
      call Print_error_msg('The requested restart-data layout is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
  function get_name_restart_history_mode(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case (RESTART_HISTORY_EXACT)
      str = 'exact'
    case (RESTART_HISTORY_COMPACT)
      str = 'compact'
    case default
      call Print_error_msg('The requested restart-history mode is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
  function get_name_restart_clock(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case (RESTART_CLOCK_CONTINUE)
      str = 'continue'
    case (RESTART_CLOCK_RESET)
      str = 'reset'
    case default
      call Print_error_msg('The requested restart-clock mode is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
  function get_name_stat_visu_mode(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case (STAT_VISU_MODE_ALL)
      str = 'all'
    case (STAT_VISU_MODE_TSP_ONLY)
      str = 'tsp_only'
    case default
      call Print_error_msg('The requested visualised-statistics mode is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
  function get_name_fft(ist) result(str)
    integer, intent(in) :: ist
    character(72) :: str
    select case(ist)
    case ( FFT_2DECOMP_3DFFT)
      str = 'FFT using 2DECOMP&FFT'
    case ( FFT_FISHPACK_2DFFT)
      str = 'FFT using Fishpack FFT'
    case default
      call Print_error_msg('The required FFT lib is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
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
!==============================================================================
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
    case ( INIT_GVBCLN)
      str = 'Initialised from linear interpolation of given BCs'
    case ( INIT_GVBCSMOOTH)
      str = 'Initialised from smooth interpolation of given BCs'
    case default
      call Print_error_msg('The required initialisation method is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
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
    case ( ILIQUID_FLIBE)
      str = 'Liquid FLiBe (2:1 LiF:BeF2)'
    case (ILIQUID_PBLI )
      str = 'Liquid PbLi Eutectic'
    case default
      call Print_error_msg('The required flow medium is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
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
!==============================================================================
  function get_name_LES_model(LES_model) result(str)
    integer, intent(in) :: LES_model
    character(72) :: str
    select case(LES_model)
    case ( ILES_NONE )
      str = 'No LES model; DNS'
    case ( ILES_WALE )
      str = 'WALE'
    case default
      call Print_error_msg('The required LES model is not supported.')
    end select
    str = ' '//trim(adjustl(str))
    return
  end function
!==============================================================================
!> Reading the input parameters from the given file.
!! Scope:  mpi    called-freq    xdomain
!!         all    once           all
!------------------------------------------------------------------------------
! Arguments
!------------------------------------------------------------------------------
!  mode           name          role
!------------------------------------------------------------------------------
!> - none (in): NA
!> - none (out): NA
!==============================================================================
  !> Read, validate, and distribute all input parameters.
  !>
  !> The routine reads `input_chapsim.ini`, applies case-specific consistency
  !> rules, updates domain/flow/thermal/MHD descriptors, and broadcasts the
  !> interpreted configuration to all MPI ranks.
  subroutine Read_input_parameters
    use boundary_conditions_mod
    use code_performance_mod
    use EvenOdd_mod
    use mpi_mod
    use parameters_constant_mod
    use thermo_info_mod
    use util_mod
    use vars_df_mod
    use wtformat_mod
    implicit none
    character(len = 18) :: flinput = 'input_chapsim.ini'
    integer :: ioerr, inputUnit
    integer  :: slen
    character(len = 80) :: secname
    character(len = 80) :: varname, varvalue
    character(len = 256) :: input_line
    character(len = 80) :: section_keys(INPUT_SECTION_MAX)
    character(len = 256) :: section_values(INPUT_SECTION_MAX)
    integer  :: itmp
    real(WP) :: rtmp, diff, best_diff
    real(WP) :: gravity_vector(NDIM)
    real(WP), allocatable :: rtmpx(:)
    integer, allocatable  :: itmpx(:)
    integer :: i, j, m, n, D, S, ibuf
    integer :: section_n
    integer :: section_idx
    logical :: is_tmp
    logical :: has_reninit
    logical :: has_optional_line
    logical :: is_any_energyeq
    if(nrank == 0) then
      call Print_debug_start_msg("CHAPSim2.0 Starts ...")
      write (*, wrtfmt1i) 'The precision is REAL * ', WP
    end if
    ! default
    is_any_energyeq = .false.
    is_single_RK_projection = .false.
    is_damping_drhodt = .false.
    is_global_mass_correction = .false.
    input_section_reached_eof = .false.
    !------------------------------------------------------------------------------
    ! open file
    !------------------------------------------------------------------------------
    call open_clean_input_file(flinput, inputUnit)
    if(nrank == 0) &
    call Print_debug_start_msg("Reading General Parameters from "//flinput//" ...")
    !------------------------------------------------------------------------------
    ! reading input
    !------------------------------------------------------------------------------
    do
      if(input_section_reached_eof) exit
      !------------------------------------------------------------------------------
      ! reading headings/comments
      !------------------------------------------------------------------------------
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
      !------------------------------------------------------------------------------
      ! [ioparams]
      !------------------------------------------------------------------------------
      if ( secname(1:slen) == '[process]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        call section_get_logical(section_keys, section_values, section_n, secname(1:slen), 'is_prerun', is_prerun)
        call section_get_logical(section_keys, section_values, section_n, secname(1:slen), 'is_postprocess', is_postprocess)
        if(nrank == 0) then
          write (*, wrtfmt1l) 'is_prerun :', is_prerun
          write (*, wrtfmt1l) 'is_postprocess :', is_postprocess
        end if
      !------------------------------------------------------------------------------
      ! [decomposition]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[decomposition]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        nxdomain = 1
        section_idx = section_find_key(section_keys, section_n, 'nxdomain')
        if(section_idx > 0) then
          read(section_values(section_idx), *, iostat = ioerr) nxdomain
          if(ioerr /= 0) nxdomain = 1
          ioerr = 0
        end if
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), 'p_row', p_row)
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), 'p_col', p_col)
        if (nxdomain /= 1 .and. nrank == 0) call Print_error_msg("Set up nxdomain = 1.")
        allocate( domain (nxdomain) )
        allocate(   flow (nxdomain) )
        allocate( itmpx(nxdomain) ); itmpx = 0
        allocate( rtmpx(nxdomain) ); rtmpx = ZERO
        domain(:)%is_thermo = .false.
        domain(:)%icht = 0
        domain(:)%is_mhd = .false.
        domain(:)%visu_precision = VISU_PRECISION_SINGLE
        domain(:)%restart_data_layout_read = RESTART_LAYOUT_PER_FIELD
        domain(:)%restart_data_layout_write = RESTART_LAYOUT_PER_FIELD
        domain(:)%restart_history_mode = RESTART_HISTORY_EXACT
        domain(:)%restart_clock = RESTART_CLOCK_CONTINUE
        domain(:)%iteration_start = 0
        domain(:)%reset_unit_massflux = .false.
        domain(:)%ipoisson_y_method = IPOISSON_Y_AUTO
        flow(:)%is_active_tripping = .false.
        flow(:)%is_compact_restart_startup = .false.
        domain(:)%LES_model = ILES_NONE
        do i = 1, nxdomain
          domain(i)%idom = i
          flow(i)%igravity = ZERO
        end do
        if(nrank == 0) then
          call Print_note_msg('if p_row = p_col = 0, the system will employ a default, automatic domain decomposition strategy.')
          write (*, wrtfmt1i) 'x-dir domain number             :', nxdomain
          write (*, wrtfmt1i) 'y-dir domain number (mpi Row)   :', p_row
          write (*, wrtfmt1i) 'z-dir domain number (mpi Column):', p_col
        end if
      !------------------------------------------------------------------------------
      ! [domain]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[domain]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        section_idx = section_find_key(section_keys, section_n, 'icase')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key icase in section '//trim(secname(1:slen)))
        call parse_icase(section_values(section_idx), domain(1)%icase)
        domain(:)%icase = domain(1)%icase
        call section_get_real_array(section_keys, section_values, section_n, secname(1:slen), &
                                    'lxx', domain(1 : nxdomain)%lxx)
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), &
                              'lyt', domain(1)%lyt)
        domain(:)%lyt = domain(1)%lyt
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), &
                              'lyb', domain(1)%lyb)
        domain(:)%lyb = domain(1)%lyb
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), &
                              'lzz', domain(1)%lzz)
        domain(:)%lzz = domain(1)%lzz
        !------------------------------------------------------------------------------
        !     restore domain size to default if not set properly
        !------------------------------------------------------------------------------
        do i = 1, nxdomain
          call apply_case_geometry_defaults(domain(i))
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
      !------------------------------------------------------------------------------
      ! [boundary]
      !------------------------------------------------------------------------------
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
        call read_optional_input_line(inputUnit, 'idriven', input_line, has_optional_line)
        if(.not. has_optional_line) &
        call Print_error_msg('Missing required key idriven in section '//trim(secname(1:slen)))
        call parse_idriven(input_line, flow(1)%idriven)
        flow(2 : nxdomain)%idriven = flow(1)%idriven
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
            domain(i)%ibcx_nominal(1, 4) = IBC_NEUMANN
            !domain(i)%ibcx_nominal(1, 5) = IBC_DIRICHLET
          end if
          !if(domain(i)%ibcx_nominal(2, 1) == IBC_CONVECTIVE) then
          !  domain(i)%ibcx_nominal(2, 2:3) = IBC_CONVECTIVE
            !domain(i)%ibcx_nominal(2, 4) = IBC_NEUMANN
            !domain(i)%ibcx_nominal(2, 5) = IBC_NEUMANN !IBC_CONVECTIVE, to check!
          !end if
          !------------------------------------------------------------------------------
          ! to exclude non-resonable input
          !------------------------------------------------------------------------------
          domain(i)%is_conv_outlet = .false.
          do m = 1, NBC
            if(domain(i)%ibcx_nominal(2, m) == IBC_PROFILE1D) &
              call Print_error_msg(" This BC IBC_PROFILE1D is not supported.")
            do n = 1, 2
              if(domain(i)%ibcx_nominal(n, m) >  IBC_OTHERS   ) &
                call Print_error_msg(" This xBC is not suported.")
              if(domain(i)%ibcy_nominal(n, m) >  IBC_OTHERS   ) &
                call Print_error_msg(" This yBC is not suported.")
              if(domain(i)%ibcz_nominal(n, m) >  IBC_OTHERS   ) &
                call Print_error_msg(" This zBC is not suported.")
              if(domain(i)%ibcy_nominal(n, m) == IBC_PROFILE1D) &
                call Print_error_msg(" This yBC IBC_PROFILE1D is not supported.")
              if(domain(i)%ibcz_nominal(n, m) == IBC_PROFILE1D) &
                call Print_error_msg(" This zBC IBC_PROFILE1D is not supported.")
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
          if(domain(i)%ibcx_nominal(1, 1) == IBC_PERIODIC .or. &
             domain(i)%ibcx_nominal(2, 1) == IBC_PERIODIC) then
            if(flow(i)%idriven == IDRVF_NO) then
              if(nrank==0) &
              call Print_warning_msg("Check if a flow driven force is required for periodic flow.")
            end if
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
      !------------------------------------------------------------------------------
      ! [mesh]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[mesh]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), &
                                 'ncx', domain(1)%nc(1))
        domain(:)%nc(1) = domain(1)%nc(1)
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), &
                                 'ncy', domain(1)%nc(2))
        domain(:)%nc(2) = domain(1)%nc(2)
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), &
                                 'ncz', domain(1)%nc(3))
        domain(:)%nc(3) = domain(1)%nc(3)
        section_idx = section_find_key(section_keys, section_n, 'istret')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key istret in section '//trim(secname(1:slen)))
        call parse_istret(section_values(section_idx), domain(1)%istret)
        domain(:)%istret = domain(1)%istret
        section_idx = section_find_key(section_keys, section_n, 'rstret')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key rstret in section '//trim(secname(1:slen)))
        call parse_rstret('rstret='//trim(section_values(section_idx)), domain(1)%mstret, domain(1)%rstret)
        domain(:)%rstret = domain(1)%rstret
        domain(:)%mstret = domain(1)%mstret
        section_idx = section_find_key(section_keys, section_n, 'poisson_y_method')
        if(section_idx > 0) then
          varvalue = section_values(section_idx)
          select case(trim(lowercase(adjustl(varvalue))))
          case('auto', '0')
            domain(:)%ipoisson_y_method = IPOISSON_Y_AUTO
          case('fft', '1')
            domain(:)%ipoisson_y_method = IPOISSON_Y_FFT
          case('tdma', '2')
            domain(:)%ipoisson_y_method = IPOISSON_Y_TDMA
          case default
            call Print_error_msg('Invalid poisson_y_method. Supported values: auto, fft, tdma.')
          end select
        end if
        !read(inputUnit, *, iostat = ioerr) varname, domain(1)%ifft_lib
        domain(:)%ifft_lib = FFT_2DECOMP_3DFFT
        !domain(:)%ifft_lib = FFT_FISHPACK_2DFFT ! for testing only, hidden option
        do i = 1, nxdomain
          call apply_mesh_stretching_defaults(domain(i))
        end do
        if(nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1i) 'mesh cell number - x :', domain(i)%nc(1)
            write (*, wrtfmt1i) 'mesh cell number - y :', domain(i)%nc(2)
            write (*, wrtfmt1i) 'mesh cell number - z :', domain(i)%nc(3)
            write (*, wrtfmt3l) 'is mesh stretching in xyz :', domain(i)%is_stretching(1:3)
            write (*, wrtfmt2s) 'mesh y-stretching type :', get_name_mesh(domain(i)%istret)
            if(domain(i)%istret /= ISTRET_NO) then
              write (*, wrtfmt1r) 'mesh y-stretching factor :', domain(i)%rstret
              write (*, wrtfmt2s) 'mesh y-stretching method :', get_name_mstret(domain(i)%mstret)
            end if
          end do
        end if
      !------------------------------------------------------------------------------
      ! [timestepping]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[scheme]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), &
                              'dt', domain(1)%dt)
        domain(:)%dt = domain(1)%dt
        section_idx = section_find_key(section_keys, section_n, 'itimescheme')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key itimescheme in section '//trim(secname(1:slen)))
        call parse_itimescheme(section_values(section_idx), domain(1)%iTimeScheme)
        domain(:)%iTimeScheme = domain(1)%iTimeScheme
        section_idx = section_find_key(section_keys, section_n, 'iaccuracy')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key iaccuracy in section '//trim(secname(1:slen)))
        call parse_iaccuracy(section_values(section_idx), domain(1)%iAccuracy)
        domain(:)%iAccuracy = domain(1)%iAccuracy
        domain(1)%iviscous = IVIS_EXPLICIT
        section_idx = section_find_key(section_keys, section_n, 'iviscous')
        if(section_idx > 0) then
          call parse_iviscous(section_values(section_idx), domain(1)%iviscous)
        end if
        domain(:)%iviscous = domain(1)%iviscous
        domain(1)%outlet_sponge_layer(1:2) = ZERO
        section_idx = section_find_key(section_keys, section_n, 'out_sponge_L_Re')
        if(section_idx > 0) then
          read(section_values(section_idx), *, iostat = ioerr) domain(1)%outlet_sponge_layer(1:2)
          if(ioerr /= 0) then
            domain(1)%outlet_sponge_layer(1:2) = ZERO
          end if
          ioerr = 0
        end if
        do i = 2, nxdomain
          domain(i)%outlet_sponge_layer(1:2) = domain(1)%outlet_sponge_layer(1:2)
        end do
        !----------------------------------------------------------------------
        ! Cylindrical used to be forced down to CD2 here, which made iaccuracy
        ! a no-op in a pipe or annulus. It no longer is: convection, diffusion
        ! and energy honour the requested order, while the *projection* still
        ! runs at CD2 on its own. Cylindrical always implies fft_skip_c2c(2)
        ! (IPOISSON_Y_AUTO and _TDMA both set it, _FFT errors out), and that
        ! flag independently pins the divergence (eq_continuity), the pressure
        ! gradient (eq_momentum2) and the Poisson wavenumbers
        ! (poisson_1stderivcomp_fft2d) to IACCU_CD2. So div, grad and the
        ! Poisson operator stay mutually consistent and the projection stays
        ! exact, but the pressure itself is only second order: raising
        ! iaccuracy in a pipe buys accuracy in the transport terms, not in the
        ! pressure. Raising the radial TDMA solver is a separate job.
        !----------------------------------------------------------------------
        !if(domain(1)%icase == ICASE_CHANNEL) then
          !if (domain(1)%iAccuracy == IACCU_CP4 .or.  &
          !    domain(1)%iAccuracy == IACCU_CP6) then
          !  domain(1)%iAccuracy = IACCU_CD4
          !end if
        !end if
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
            write (*, wrtfmt1e) 'physical time step(dt) :', domain(i)%dt
            write (*, wrtfmt1e) 'time steps required for one flow-through', domain(i)%lxx/domain(i)%dt
            write (*, wrtfmt1i) 'time marching scheme :', domain(i)%iTimeScheme
            write (*, wrtfmt2s) 'current spatial accuracy scheme :', get_name_iacc(domain(i)%iAccuracy)
            write (*, wrtfmt1i) 'viscous term treatment  :', domain(i)%iviscous
            if(domain(1)%outlet_sponge_layer(1) > MINP) then
              write (*, wrtfmt2r) 'outlet sponge layer thickness :', domain(i)%outlet_sponge_layer(1)
              write (*, wrtfmt2r) 'outlet sponge layer strength (Re):', domain(i)%outlet_sponge_layer(2)
            end if
          end do
        end if
      !------------------------------------------------------------------------------
      ! [flow]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[flow]' ) then
        call read_optional_input_line(inputUnit, 'initfl', input_line, has_optional_line)
        if(.not. has_optional_line) &
        call Print_error_msg('Missing required key initfl in section '//trim(secname(1:slen)))
        call parse_initfl(input_line, itmp)
        flow(1 : nxdomain)%inittype = itmp
        flow(1 : nxdomain)%iterfrom = 0
        call read_optional_input_line(inputUnit, 'irestartfrom', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, flow(1 : nxdomain)%iterfrom
          if(ioerr /= 0) then
            flow(1 : nxdomain)%iterfrom = 0
            if(any(flow(1 : nxdomain)%inittype == INIT_RESTART)) &
              call Print_error_msg('irestartfrom is required when initfl = 0.')
          end if
          ioerr = 0
        else if(any(flow(1 : nxdomain)%inittype == INIT_RESTART)) then
          call Print_error_msg('irestartfrom is required when initfl = 0.')
        end if
        flow(1)%init_velo3d(1:3) = ZERO
        call read_optional_input_line(inputUnit, 'veloinit', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, flow(1)%init_velo3d(1:3)
          if(ioerr /= 0) then
            flow(1)%init_velo3d(1:3) = ZERO
            if(any(flow(1 : nxdomain)%inittype == INIT_GVCONST)) &
              call Print_error_msg('veloinit is required when initfl = 4.')
          end if
          ioerr = 0
        else if(any(flow(1 : nxdomain)%inittype == INIT_GVCONST)) then
          call Print_error_msg('veloinit is required when initfl = 4.')
        end if
        flow(1 : nxdomain)%noiselevel = ZERO
        call read_optional_input_line(inputUnit, 'noiselevel', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, flow(1 : nxdomain)%noiselevel
          if(ioerr /= 0) then
            flow(1 : nxdomain)%noiselevel = ZERO
            if(any(flow(1 : nxdomain)%inittype == INIT_RANDOM) .or. &
               any(flow(1 : nxdomain)%inittype == INIT_POISEUILLE)) &
              call Print_error_msg('noiselevel is required when initfl = 2 or 5.')
          end if
          ioerr = 0
        else if(any(flow(1 : nxdomain)%inittype == INIT_RANDOM) .or. &
                any(flow(1 : nxdomain)%inittype == INIT_POISEUILLE)) then
          call Print_error_msg('noiselevel is required when initfl = 2 or 5.')
        end if
        flow(1 : nxdomain)%is_active_tripping = .false.
        call read_optional_input_line(inputUnit, 'is_active_tripping', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, flow(1 : nxdomain)%is_active_tripping
          if(ioerr /= 0) then
            flow(1 : nxdomain)%is_active_tripping = .false.
          end if
          ioerr = 0
        end if
        has_reninit = .false.
        call read_optional_input_line(inputUnit, 'reni', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, flow(1 : nxdomain)%reninit
          if(ioerr /= 0) then
            flow(1 : nxdomain)%reninit = ZERO
          else
            has_reninit = .true.
          end if
          ioerr = 0
        end if
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%initReTo
        read(inputUnit, *, iostat = ioerr) varname, flow(1 : nxdomain)%ren
        if(.not. has_reninit) flow(1 : nxdomain)%reninit = flow(1 : nxdomain)%ren
        do i = 1, nxdomain
          if(flow(i)%inittype /= INIT_RESTART) flow(i)%iterfrom = 0
          flow(i)%init_velo3d(1:3) = flow(1)%init_velo3d(1:3)
          if(domain(i)%icoordinate /= ICYLINDRICAL .and. flow(i)%is_active_tripping) then
            if(nrank == 0) call Print_warning_msg("Active tripping is only applied to cylindrical coordinates. It is disabled for this case.")
            flow(i)%is_active_tripping = .false.
          end if
          !if(flow(i)%inittype == INIT_RESTART) flow(i)%reninit = flow(i)%ren
          ! if(domain(i)%ibcx_nominal(1, 1) /= IBC_PERIODIC .or. &
          !   domain(i)%ibcx_nominal(2, 1) /= IBC_PERIODIC) then
          !   flow(i)%reninit = flow(i)%ren
          ! end if
        end do
        if( nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            call print_note_msg("The Reynolds number is based on half channel hight or radius of a pipe.")
            write (*, wrtfmt2s) 'flow field initial type :', get_name_initial(flow(i)%inittype)
            if(flow(i)%inittype == INIT_RESTART) then
              write (*, wrtfmt1i) 'restarting from :', flow(i)%iterfrom
            end if
            if(flow(i)%inittype == INIT_GVCONST) then
              write (*, wrtfmt3r) 'initial velocity u, v, w :', flow(i)%init_velo3d(1:3)
            end if
            write (*, wrtfmt1r) 'Initial velocity influction level :', flow(i)%noiselevel
            write (*, wrtfmt1l) 'is active tripping enabled?', flow(i)%is_active_tripping
            write (*, wrtfmt1r) 'Initial Reynolds No. :', flow(i)%reninit
            if(flow(i)%is_active_tripping) then
              write (*, wrtfmt1i) 'Active-tripping duration (iter):', flow(i)%initReTo
            else
              write (*, wrtfmt1i) 'Iteration for initial Reynolds No.:', flow(i)%initReTo
            end if
            write (*, wrtfmt1r) 'flow Reynolds number :', flow(i)%ren
          end do
        end if
      !------------------------------------------------------------------------------
      ! [thermo]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[thermo]' )  then
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%is_thermo
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%icht
        read(inputUnit, '(A)', iostat = ioerr) input_line
        if(ioerr /= 0) call Print_error_msg('Failed to read igravity line.')
        call parse_gravity_vector(input_line, gravity_vector)
        flow(1)%igravity = gravity_vector
        do i = 2, nxdomain
          flow(i)%igravity = flow(1)%igravity
        end do
        if(ANY(domain(:)%is_thermo)) is_any_energyeq = .true.
        if(is_any_energyeq) allocate( thermo(nxdomain) )
        call read_optional_input_line(inputUnit, 'ifluid', input_line, has_optional_line)
        if(.not. has_optional_line) &
        call Print_error_msg('Missing required key ifluid in section '//trim(secname(1:slen)))
        call parse_ifluid(input_line, itmp)
        if(is_any_energyeq) thermo(1 : nxdomain)%ifluid = itmp
        read(inputUnit, *, iostat = ioerr) varname, rtmp
        if(is_any_energyeq) thermo(1 : nxdomain)%ref_l0 = rtmp
        read(inputUnit, *, iostat = ioerr) varname, rtmp
        if(is_any_energyeq) thermo(1 : nxdomain)%ref_T0 = rtmp
        call read_optional_input_line(inputUnit, 'inittm', input_line, has_optional_line)
        if(.not. has_optional_line) &
        call Print_error_msg('Missing required key inittm in section '//trim(secname(1:slen)))
        call parse_initm(input_line, itmp)
        if(is_any_energyeq) thermo(1 : nxdomain)%inittype  = itmp
        read(inputUnit, *, iostat = ioerr) varname, itmp
        if(is_any_energyeq) thermo(1 : nxdomain)%iterfrom = itmp
        read(inputUnit, *, iostat = ioerr) varname, rtmpx(1: nxdomain)
        if(is_any_energyeq) thermo(1 : nxdomain)%init_T0 = rtmpx(1: nxdomain)
        read(inputUnit, *, iostat = ioerr) varname, rtmp, diff
        if(is_any_energyeq) thermo(1 : nxdomain)%thermo_buffer_layer(1) = rtmp
        if(is_any_energyeq) thermo(1 : nxdomain)%thermo_buffer_layer(2) = diff
        is_tmp = .false.
        i = 0
        j = 0
        call read_optional_input_line(inputUnit, 'qw_ramp', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, is_tmp, i, j
          if(ioerr /= 0) then
            is_tmp = .false.
            i = 0
            j = 0
          end if
          ioerr = 0
        end if
        if(is_any_energyeq) thermo(1 : nxdomain)%is_use_qw_ramp = is_tmp
        if(is_any_energyeq) thermo(1 : nxdomain)%istt_qw_ramp = i
        if(is_any_energyeq) thermo(1 : nxdomain)%iend_qw_ramp = j
        do i = 1, nxdomain
          ! iterfrom selects which checkpoint to read, so it is meaningless unless
          ! this field restarts. The flow is cleared the same way below the [flow]
          ! section; without the matching clear here a leftover irestartfrom in a
          ! non-restart [thermo] block would be picked up as a run-clock origin.
          if(is_any_energyeq) then
            if(thermo(i)%inittype /= INIT_RESTART) thermo(i)%iterfrom = 0
          end if
          if( domain(i)%ibcx_nominal(1, 5) == IBC_DIRICHLET ) then
            domain(i)%fbcx_const(1, 5) = thermo(i)%init_T0
          end if
        end do
        if(is_any_energyeq .and. nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1l) 'is thermal field solved ?', domain(i)%is_thermo
            write (*, wrtfmt1l) 'is CHT solved ?', domain(i)%icht
            write (*, wrtfmt3r) 'gravity unit vector :', flow(i)%igravity(1), &
              flow(i)%igravity(2), flow(i)%igravity(3)
            write (*, wrtfmt2s) 'fluid medium :', get_name_fluid(thermo(i)%ifluid)
            write (*, wrtfmt1r) 'reference length (m) :', thermo(i)%ref_l0
            write (*, wrtfmt1r) 'reference temperature (K) :', thermo(i)%ref_T0
            write (*, wrtfmt2s) 'thermo field initial type :', get_name_initial(thermo(i)%inittype)
            if(thermo(i)%inittype == INIT_RESTART) then
              write (*, wrtfmt1i) 'restarting from :', thermo(i)%iterfrom
            end if
            if(thermo(i)%inittype == INIT_GVCONST) then
              write (*, wrtfmt1r) 'initial temperature (K) :', thermo(i)%init_T0
            end if
            write (*, wrtfmt2r) 'inlet  thermal buffer length (lx/L0):', thermo(i)%thermo_buffer_layer(1)
            write (*, wrtfmt2r) 'outlet thermal buffer length (lx/L0):', thermo(i)%thermo_buffer_layer(2)
            write (*, wrtfmt1l) 'is a ramp b.c. heat flux qw enabled?',  thermo(i)%is_use_qw_ramp
            if(thermo(i)%is_use_qw_ramp) then
              write (*, wrtfmt1i) 'b.c. qw ramp starts from :', thermo(i)%istt_qw_ramp
              write (*, wrtfmt1i) 'b.c. qw ramp ends at :', thermo(i)%iend_qw_ramp
            end if
          end do
        else if(nrank == 0) then
         call Print_note_msg ('Thermal field is not considered. ')
        end if
      !------------------------------------------------------------------------------
      ! [mhd]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[mhd]' )  then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        call section_get_logical(section_keys, section_values, section_n, secname(1:slen), 'imhd_xdom', domain(1)%is_mhd)
        domain(1:nxdomain)%is_mhd = domain(1)%is_mhd
        if(domain(1)%is_mhd) then
          allocate (mhd(nxdomain))
          section_idx = section_find_key(section_keys, section_n, 'NStuart')
          if(section_idx == 0) call Print_error_msg('Missing required key NStuart in section '//trim(secname(1:slen)))
          read(section_values(section_idx), *, iostat = ioerr) mhd(1)%is_NStuart, mhd(1)%NStuart
          if(ioerr /= 0) call Print_error_msg('Invalid values for key NStuart in section '//trim(secname(1:slen)))
          section_idx = section_find_key(section_keys, section_n, 'NHartmn')
          if(section_idx == 0) call Print_error_msg('Missing required key NHartmn in section '//trim(secname(1:slen)))
          read(section_values(section_idx), *, iostat = ioerr) mhd(1)%is_NHartmn, mhd(1)%NHartmn
          if(ioerr /= 0) call Print_error_msg('Invalid values for key NHartmn in section '//trim(secname(1:slen)))
          call section_get_real_array(section_keys, section_values, section_n, secname(1:slen), 'B_static', mhd(1)%B_static(1:3))
          if( (     mhd(1)%is_NStuart  .and.       mhd(1)%is_NHartmn) .or. &
            ( (.not.mhd(1)%is_NStuart) .and. (.not.mhd(1)%is_NHartmn)) ) &
          call Print_error_msg('Please provide either Stuart Number or Hartmann Number')
          mhd(1)%iterfrom = flow(1)%iterfrom
          !------------------------------------------------------------------------------
          ! Electrical boundary condition for the electric potential. Optional; when a
          ! key is absent the side keeps EBC_INHERIT and initialise_mhd copies the
          ! pressure BC, which is what every input written before this key did.
          !------------------------------------------------------------------------------
          mhd(1)%ebcx_nominal(:) = EBC_INHERIT
          mhd(1)%ebcy_nominal(:) = EBC_INHERIT
          mhd(1)%ebcz_nominal(:) = EBC_INHERIT
          call parse_ebc(section_keys, section_values, section_n, secname(1:slen), 'ebcx', mhd(1)%ebcx_nominal)
          call parse_ebc(section_keys, section_values, section_n, secname(1:slen), 'ebcy', mhd(1)%ebcy_nominal)
          call parse_ebc(section_keys, section_values, section_n, secname(1:slen), 'ebcz', mhd(1)%ebcz_nominal)
          ! Only mhd(1) is filled from the input, as for every other key in this
          ! section. Propagate the electrical BC explicitly so that an unsupported
          ! multi-domain run cannot pick up an uninitialised code and resolve it to
          ! a boundary condition nobody asked for.
          do i = 2, nxdomain
            mhd(i)%ebcx_nominal(:) = mhd(1)%ebcx_nominal(:)
            mhd(i)%ebcy_nominal(:) = mhd(1)%ebcy_nominal(:)
            mhd(i)%ebcz_nominal(:) = mhd(1)%ebcz_nominal(:)
          end do
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
      !------------------------------------------------------------------------------
      ! [les]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[les]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        section_idx = section_find_key(section_keys, section_n, 'LESmode')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key LESmode in section '//trim(secname(1:slen)))
        call parse_LES_model(section_values(section_idx), domain(1)%LES_model)
        domain(:)%LES_model = domain(1)%LES_model
        if(domain(1)%LES_model /= ILES_NONE .and. nrank == 0) then
          do i = 1, nxdomain
            write (*, wrtfmt2s) 'LES model:', get_name_LES_model(domain(i)%LES_model)
          end do
        else if(domain(1)%LES_model /= ILES_NONE) then
          do i = 1, nxdomain
            select case(domain(i)%LES_model)
            case(ILES_WALE)
            case default
              call Print_error_msg('The required LES model is not supported.')
            end select
          end do
        else if(nrank == 0) then
          call Print_note_msg(' LES is not enabled; DNS mode. ')
        end if
      !------------------------------------------------------------------------------
      ! [simcontrol]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[simcontrol]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        call section_get_integer_array(section_keys, section_values, section_n, secname(1:slen), &
                                       'niterflowfirst', flow(1 : nxdomain)%nIterFlowStart)
        call section_get_integer_array(section_keys, section_values, section_n, secname(1:slen), &
                                       'niterflowlast', flow(1 : nxdomain)%nIterFlowEnd)
        call section_get_integer_array(section_keys, section_values, section_n, secname(1:slen), &
                                       'niterthermofirst', itmpx(1:nxdomain))
        if(is_any_energyeq) thermo(1 : nxdomain)%nIterThermoStart = itmpx(1:nxdomain)
        call section_get_integer_array(section_keys, section_values, section_n, secname(1:slen), &
                                       'niterthermolast', itmpx(1:nxdomain))
        if(is_any_energyeq) thermo(1 : nxdomain)%nIterThermoEnd = itmpx(1:nxdomain)
        section_idx = section_find_key(section_keys, section_n, 'restart_clock')
        if(section_idx > 0) then
          call parse_restart_clock(section_values(section_idx), domain(1)%restart_clock)
          domain(:)%restart_clock = domain(1)%restart_clock
        end if
        ! Runtime override via environment
        call get_environment_variable("CHAPSIM_NITER", varname, status=ioerr)
        if(ioerr == 0) then
            itmp = int_from_string(trim(varname), ioerr)
            if(ioerr == 0) then
                flow(:)%nIterFlowEnd = itmp
                if(is_any_energyeq) thermo(:)%nIterThermoEnd = itmp
                ! optional: reset start iteration for smoke test
                flow(:)%nIterFlowStart = 1
                if(is_any_energyeq) thermo(:)%nIterThermoStart = 1
            end if
        end if
        !
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
      !------------------------------------------------------------------------------
      ! [ioparams]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[io]' ) then
        read(inputUnit, *, iostat = ioerr) varname, cpu_nfre
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%ckpt_nfre
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%visu_idim
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%visu_nfre
        domain(1)%visu_nskip(1:3) = 1
        call read_optional_input_line(inputUnit, 'visu_nskip', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, domain(1)%visu_nskip(1:3)
          if(ioerr /= 0) then
            domain(1)%visu_nskip(1:3) = 1
          end if
          ioerr = 0
        end if
        read(inputUnit, *, iostat = ioerr) varname, domain(1 : nxdomain)%stat_istart
        read(inputUnit, *, iostat = ioerr) varname, domain(1)%stat_level
        domain(1)%stat_nskip(1:3) = 1
        call read_optional_input_line(inputUnit, 'stat_nskip', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, domain(1)%stat_nskip(1:3)
          if(ioerr /= 0) then
            domain(1)%stat_nskip(1:3) = 1
          end if
          ioerr = 0
        end if
        domain(1)%stat_visu_nfre = domain(1)%visu_nfre
        call read_optional_input_line(inputUnit, 'stat_visu_nfre', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, domain(1)%stat_visu_nfre
          if(ioerr /= 0 .or. domain(1)%stat_visu_nfre <= 0) then
            call Print_warning_msg('Invalid stat_visu_nfre; using visu_nfre for visualised statistics.')
            domain(1)%stat_visu_nfre = domain(1)%visu_nfre
          end if
          ioerr = 0
        end if
        domain(1)%stat_visu_mode = STAT_VISU_MODE_ALL
        call read_optional_input_line(inputUnit, 'stat_visu_mode', input_line, has_optional_line)
        if(has_optional_line) then
          call parse_stat_visu_mode(input_line, domain(1)%stat_visu_mode)
        end if
        domain(1)%is_record_xoutlet = .false.
        domain(1)%is_read_xinlet = .false.
        call read_optional_input_line(inputUnit, 'is_record_xoutlet_read_xinlet', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, domain(1)%is_record_xoutlet, domain(1)%is_read_xinlet
          if(ioerr /= 0) then
            domain(1)%is_record_xoutlet = .false.
            domain(1)%is_read_xinlet = .false.
          end if
          ioerr = 0
        end if
        domain(1)%ndbfre = 0
        domain(1)%ndbstart = 0
        domain(1)%ndbend = 0
        domain(1)%ndb_file_offset = 0
        call read_optional_input_line(inputUnit, 'ndbfre_ndbstart_ndbend', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, domain(1)%ndbfre, domain(1)%ndbstart, domain(1)%ndbend
          if(ioerr /= 0) then
            domain(1)%ndbfre = 0
            domain(1)%ndbstart = 0
            domain(1)%ndbend = 0
          end if
          ioerr = 0
        end if
        call read_optional_input_line(inputUnit, 'ndb_file_offset', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, domain(1)%ndb_file_offset
          if(ioerr /= 0 .or. domain(1)%ndb_file_offset < 0) then
            call Print_error_msg('Invalid ndb_file_offset. Supported values are non-negative integers.')
          end if
          ioerr = 0
        end if
        domain(1)%existing_output_policy = OUTPUT_POLICY_OVERWRITE
        call read_optional_input_line(inputUnit, 'existing_output_policy', input_line, has_optional_line)
        if(has_optional_line) then
          call parse_existing_output_policy(input_line, domain(1)%existing_output_policy)
        end if
        domain(:)%existing_output_policy = domain(1)%existing_output_policy
        domain(1)%restart_data_layout_read = RESTART_LAYOUT_PER_FIELD
        domain(1)%restart_data_layout_write = RESTART_LAYOUT_PER_FIELD
        call read_optional_input_line(inputUnit, 'restart_data_layout', input_line, has_optional_line)
        if(has_optional_line) then
          call parse_restart_data_layout(input_line, domain(1)%restart_data_layout_read)
          domain(1)%restart_data_layout_write = domain(1)%restart_data_layout_read
        end if
        call read_optional_input_line(inputUnit, 'restart_data_layout_read', input_line, has_optional_line)
        if(has_optional_line) then
          call parse_restart_data_layout(input_line, domain(1)%restart_data_layout_read)
        end if
        call read_optional_input_line(inputUnit, 'restart_data_layout_write', input_line, has_optional_line)
        if(has_optional_line) then
          call parse_restart_data_layout(input_line, domain(1)%restart_data_layout_write)
        end if
        domain(:)%restart_data_layout_read = domain(1)%restart_data_layout_read
        domain(:)%restart_data_layout_write = domain(1)%restart_data_layout_write
        domain(1)%restart_history_mode = RESTART_HISTORY_EXACT
        call read_optional_input_line(inputUnit, 'restart_history_mode', input_line, has_optional_line)
        if(has_optional_line) then
          call parse_restart_history_mode(input_line, domain(1)%restart_history_mode)
        end if
        domain(:)%restart_history_mode = domain(1)%restart_history_mode
        domain(:)%ndb_file_offset = domain(1)%ndb_file_offset
        domain(1)%reset_unit_massflux = .false.
        call read_optional_input_line(inputUnit, 'reset_unit_massflux', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, domain(1)%reset_unit_massflux
          if(ioerr /= 0) then
            call Print_error_msg('Invalid reset_unit_massflux. Supported values: true, false.')
          end if
          ioerr = 0
        end if
        domain(:)%reset_unit_massflux = domain(1)%reset_unit_massflux
        call read_optional_input_line(inputUnit, 'visu_precision', input_line, has_optional_line)
        if(has_optional_line) then
          read(input_line, *, iostat = ioerr) varname, itmp
          if(ioerr == 0) then
            domain(1:nxdomain)%visu_precision = itmp
          end if
          ioerr = 0
        end if
        if(domain(1)%visu_precision /= VISU_PRECISION_SINGLE .and. &
           domain(1)%visu_precision /= VISU_PRECISION_DOUBLE) then
          call Print_warning_msg('Unsupported visu_precision; using single precision visualisation output.')
          domain(1:nxdomain)%visu_precision = VISU_PRECISION_SINGLE
        end if
        if(domain(1)%is_record_xoutlet .or. &
          domain(1)%is_read_xinlet) then
          if(domain(1)%ndbfre <= 0) &
            call Print_error_msg('ndbfre must be positive when recording or reading inlet/outlet database.')
          if(domain(1)%ndbend < domain(1)%ndbstart) then
            domain(1)%ndbend = domain(1)%ndbstart + domain(1)%ndbfre - 1
          end if
          !----------------------------------------------------------------------
          ! Clamping the window to the end of this run is only meaningful when
          ! RECORDING: no plane can be written after the last iteration. When
          ! READING, ndbstart/ndbend describe a database written by a previous
          ! run, whose length has nothing to do with this one - clamping it can
          ! push ndbend below ndbstart and leave an empty window.
          !----------------------------------------------------------------------
          if(domain(1)%is_record_xoutlet) then
            if(domain(1)%ndbend > flow(1)%nIterFlowEnd) then
              domain(1)%ndbend = flow(1)%nIterFlowEnd
            end if
          end if
          itmp = domain(1)%ndbstart - 1 + ((domain(1)%ndbend - domain(1)%ndbstart + 1) / domain(1)%ndbfre) * domain(1)%ndbfre
          domain(1)%ndbend =min(domain(1)%ndbend, itmp)
          !----------------------------------------------------------------------
          ! An empty window gives zero stored planes, and the readers divide by
          ! that count. Fail with a message rather than a floating-point trap.
          !----------------------------------------------------------------------
          if(domain(1)%ndbend < domain(1)%ndbstart) &
            call Print_error_msg('The inlet/outlet database window is empty: ndbend < ndbstart after '// &
              'alignment to ndbfre. Check ndbfre_ndbstart_ndbend, and for a recording run check that '// &
              'ndbstart+ndbfre-1 does not exceed the last flow iteration.')
        end if
        domain(1)%visu_nskip(1:3) = 1 ! This is a temporary solution to wait for features from 2decomp lib.
        do i = 1, nxdomain
          if(domain(1)%ndbfre/=0) &
          domain(:)%ndbend = (domain(1)%ndbend - domain(1)%ndbstart + 1)/domain(1)%ndbfre * domain(1)%ndbfre + domain(1)%ndbstart - 1
          domain(i)%ndbbuf = 1
          if(domain(i)%ndbfre > 0) then
            do ibuf = min(domain(i)%ndbfre, domain(i)%nc(1)), 1, -1
              if(mod(domain(i)%ndbfre, ibuf) == 0) then
                domain(i)%ndbbuf = ibuf
                exit
              end if
            end do
          end if
          domain(i)%visu_nskip(1:3) = domain(1)%visu_nskip(1:3)
          domain(i)%stat_nskip(1:3) = domain(1)%stat_nskip(1:3)
          domain(i)%stat_visu_nfre = domain(1)%stat_visu_nfre
          domain(i)%stat_visu_mode = domain(1)%stat_visu_mode
          domain(i)%restart_data_layout_read = domain(1)%restart_data_layout_read
          domain(i)%restart_data_layout_write = domain(1)%restart_data_layout_write
          domain(i)%restart_history_mode = domain(1)%restart_history_mode
          domain(i)%reset_unit_massflux = domain(1)%reset_unit_massflux
          if(domain(i)%stat_visu_mode == STAT_VISU_MODE_TSP_ONLY) then
            if(count(domain(i)%is_periodic(1:3)) == 0) &
              call Print_error_msg('stat_visu_mode=tsp_only requires at least one periodic direction.')
            if(count(domain(i)%is_periodic(1:3)) == 3) &
              call Print_warning_msg('stat_visu_mode=tsp_only is selected for an all-periodic domain, ' // &
                                     'but current tsp_avg visualisation does not write bulk-only statistics.')
          end if
          if(domain(i)%is_stretching(2)) domain(i)%visu_nskip(2) = 1
          if(domain(i)%is_stretching(2)) domain(i)%stat_nskip(2) = 1
          !
          do n = 1, 3
            if((domain(i)%visu_nskip(n) < 1) .or. &
               (domain(i)%visu_nskip(n) > (domain(i)%nc(n)+1))) then
               domain(i)%visu_nskip(n) = 1
            end if
            if(domain(i)%visu_nskip(n) > 1) then
              D = domain(i)%nc(n) - 1
              S = 1
              best_diff = abs(domain(i)%visu_nskip(n) - 1)
              do j = 1, D
                if (mod(D, j) == 0) then
                    diff = abs(domain(i)%visu_nskip(n) - j)
                    if (diff < best_diff) then
                        best_diff = diff
                        S = j
                    end if
                end if
                domain(i)%visu_nskip(n) = S
              end do
            end if
          end do
        end do
        !
        if( nrank == 0) then
          do i = 1, nxdomain
            !write (*, wrtfmt1i) '------For the domain-x------ ', i
            write (*, wrtfmt1i) 'data check freqency :', domain(i)%ckpt_nfre
            write (*, wrtfmt2s) 'existing output policy :', &
              get_name_existing_output_policy(domain(i)%existing_output_policy)
            write (*, wrtfmt2s) 'restart input layout :', &
              get_name_restart_data_layout(domain(i)%restart_data_layout_read)
            write (*, wrtfmt2s) 'restart output layout :', &
              get_name_restart_data_layout(domain(i)%restart_data_layout_write)
            write (*, wrtfmt2s) 'restart history mode :', &
              get_name_restart_history_mode(domain(i)%restart_history_mode)
            write (*, wrtfmt1l) 'reset unit mass flux on restart? :', domain(i)%reset_unit_massflux
            write (*, wrtfmt1i) 'visu data dimensions :', domain(i)%visu_idim
            write (*, wrtfmt1i) 'visu data written freqency :', domain(i)%visu_nfre
            write (*, wrtfmt1i) 'visu data precision bytes :', domain(i)%visu_precision
            write (*, wrtfmt3i) 'visu data skips in xyz :', domain(i)%visu_nskip(1:3)
            write (*, wrtfmt1i) 'statistics written from :', domain(i)%stat_istart
            write (*, wrtfmt1i) 'visualised statistics written freqency :', domain(i)%stat_visu_nfre
            write (*, wrtfmt2s) 'visualised statistics mode :', get_name_stat_visu_mode(domain(i)%stat_visu_mode)
            write (*, wrtfmt3i) 'statistics skips in xyz :', domain(i)%stat_nskip(1:3)
            write (*, wrtfmt1l) 'recording outlet plane? :', domain(1)%is_record_xoutlet
            write (*, wrtfmt1l) 'reading inlet plane? :', domain(1)%is_read_xinlet
            write (*, wrtfmt1i) 'reading/recording plane freqency :', domain(1)%ndbfre
            write (*, wrtfmt1i) 'reading/recording plane buffer :', domain(1)%ndbbuf
            write (*, wrtfmt1i) 'reading/recording plane file offset :', domain(1)%ndb_file_offset
            write (*, wrtfmt2i) 'reading/recording plane period (start-end):', domain(1)%ndbstart, domain(1)%ndbend
            if(domain(1)%is_record_xoutlet .and. domain(1)%ndbstart < domain(1)%stat_istart) &
            call Print_warning_msg('recording outlet plane data before starting statistics!')
          end do
        end if
      !------------------------------------------------------------------------------
      ! [probe]
      !------------------------------------------------------------------------------
      else if ( secname(1:slen) == '[probe]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        do i = 1, nxdomain
          call section_get_integer(section_keys, section_values, section_n, secname(1:slen), 'npp', domain(i)%proben)
          if(domain(i)%proben > 0) then
            allocate( domain(i)%probexyz(3, domain(i)%proben))
            !if( nrank == 0) !write (*, wrtfmt1i) '------For the domain-x------ ', i
            do j = 1, domain(i)%proben
              write(varname, '(A,I0)') 'pt', j
              call section_get_real_array(section_keys, section_values, section_n, secname(1:slen), &
                                          trim(varname), domain(i)%probexyz(1:3, j))
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
    !------------------------------------------------------------------------------
    ! end of reading, clearing dummies
    !------------------------------------------------------------------------------
    if(.not. input_section_reached_eof .and. .not.IS_IOSTAT_END(ioerr)) &
    call Print_error_msg( 'Problem reading '//flinput // &
    'in Subroutine: '// "Read_general_input")
    close(inputUnit)
    !------------------------------------------------------------------------------
    ! cross session conditions
    !------------------------------------------------------------------------------
    do i = 1, nxdomain
      if(domain(i)%ibcx_nominal(1, 1) /= IBC_PERIODIC .or. &
         domain(i)%ibcx_nominal(2, 1) /= IBC_PERIODIC) then
        flow(i)%reninit = flow(i)%ren
      end if
    end do
    if(is_any_energyeq) then
      do i = 1, nxdomain
        thermo(i)%is_rhoh_compensated = .false.
!------------------------------------------------------------------------------
!       Any initialisation other than a restart or a uniform field interpolates
!       from the y boundary values, so it needs a prescribed wall temperature to
!       interpolate towards. Only the upper side is required: the lower endpoint
!       falls back to init_T0 - input_thermo:1296 - which is what a pipe needs,
!       where the lower side is the axis (IBC_INTERIOR) and never Dirichlet.
!       This condition must stay identical to the one tested at
!       input_thermo:1293, otherwise the interpolating init types reach that
!       select case with no branch to take and leave tTemp unassigned.
!------------------------------------------------------------------------------
        if(thermo(i)%inittype /= INIT_RESTART .and. &
           domain(i)%ibcy_nominal(2, 5) /= IBC_DIRICHLET) then
           thermo(i)%inittype = INIT_GVCONST
        end if
!------------------------------------------------------------------------------
!       INIT_GVBCLN and INIT_GVBCSMOOTH interpolate the initial temperature
!       between the two y side values. That is a good starting field for a
!       streamwise-periodic channel, annulus or pipe, where nothing else
!       prescribes the temperature. It is wrong
!       for an inlet-outlet run, where the inlet plane does: the interpolated
!       interior does not match the inlet temperature, so the first thermal
!       substep sees a step in density across the whole inlet, and
!       drhodt = (dDens - dDens0) / (tAlpha * dt) - eq_energy:73 - divides that
!       step by a timestep and puts it into the Poisson right-hand side. It is
!       a one-off inconsistency in the initial condition, not a transient: on
!       channel_scp_inout_Tw the global pressure drop steps to 1.7e5 at the
!       first thermal substep and stays flat there, against -1.1e2 from a
!       uniform start. Fall back to a uniform field, which matches the inlet -
!       it matches because line 1692 forces a Dirichlet inlet temperature to
!       equal init_T0, so the two always agree. If that coupling is ever
!       relaxed, a mismatch would reinstate the same inlet-plane density step.
!------------------------------------------------------------------------------
        if((thermo(i)%inittype == INIT_GVBCLN .or. &
            thermo(i)%inittype == INIT_GVBCSMOOTH) .and. &
           (domain(i)%ibcx_nominal(1, 1) /= IBC_PERIODIC .or. &
            domain(i)%ibcx_nominal(2, 1) /= IBC_PERIODIC)) then
           thermo(i)%inittype = INIT_GVCONST
           if(nrank == 0) call Print_warning_msg("A wall-to-wall interpolated &
             &initial temperature is incompatible with an inlet-outlet &
             &configuration; a uniform initial field is used instead.")
        end if
        if(domain(i)%ibcx_nominal(1, 1) == IBC_PERIODIC .and. &
           domain(i)%ibcx_nominal(2, 1) == IBC_PERIODIC) then
           if(domain(i)%ibcy_nominal(1, 5) == IBC_NEUMANN .or. &
              domain(i)%ibcy_nominal(2, 5) == IBC_NEUMANN) then
              thermo(i)%is_rhoh_compensated = .true.
           end if
        end if
      end do
!------------------------------------------------------------------------------
!     Report the initialisation type that will actually be used. The summary at
!     line 1705 is printed before the two downgrades above, so on their own the
!     log would name the type asked for in the input file rather than the one
!     the solver ran with.
!------------------------------------------------------------------------------
      if(nrank == 0) then
        do i = 1, nxdomain
          write (*, wrtfmt2s) 'thermo field initial type (effective) :', &
            get_name_initial(thermo(i)%inittype)
        end do
      end if
    end if
    if(domain(1)%is_periodic(1)) then
      domain(1)%outlet_sponge_layer(1:2) = ZERO
    end if
    if(is_any_energyeq) then
      if(domain(1)%is_periodic(1)) &
      thermo(1)%thermo_buffer_layer(:) = ZERO
    end if
    select case(domain(1)%ipoisson_y_method)
    case(IPOISSON_Y_AUTO)
      if((.not. domain(1)%is_periodic(2)) .and. is_any_energyeq) then
        domain(:)%fft_skip_c2c(2) = .true.
      end if
      if(domain(1)%icoordinate == ICYLINDRICAL) then
        domain(:)%fft_skip_c2c(2) = .true.
      end if
    case(IPOISSON_Y_FFT)
      if(domain(1)%icoordinate == ICYLINDRICAL) then
        call Print_error_msg('poisson_y_method=fft is not supported for cylindrical coordinates.')
      end if
      !-----------------------------------------------------------------
      ! An all-Neumann pressure system is singular, so the Poisson source
      ! must satisfy the compatibility condition
      !   integral_V (div(g_hat) + d(rho)/dt) dV = 0,
      ! which is global mass conservation. The callers that establish it -
      ! enforce_domain_mass_balance_dyn_fbc for inlet/outlet and
      ! is_global_mass_correction for closed/periodic systems - both work
      ! with the PHYSICAL volume-weighted mean (Get_volumetric_average_3d).
      !
      ! Only the y-TDMA path removes the same functional: its left-null
      ! vector null_weight(j) ~ Jc(j)/rc(j) applied to the r^2-scaled source
      ! reproduces exactly integral_V f dV. The full-FFT path performs no
      ! range projection at all on a stretched mesh - the y operator is
      ! pentadiagonal after matrice_refinement, and the singular mode is
      ! only guarded against division by zero in inversion5_v1. The caller
      ! therefore corrects one component while the solver discards another,
      ! and the mismatch enters the continuity residual directly because
      ! d(rho)/dt is an O(1) source rather than an O(dt) one.
      !
      ! Isothermal flow escapes this only because its source mean is zero
      ! to roundoff, leaving nothing to discard.
      !-----------------------------------------------------------------
      if(is_any_energyeq .and. (.not. domain(1)%is_periodic(2))) then
        call Print_error_msg('poisson_y_method=fft is not valid for a thermal flow with a '// &
          'non-periodic y direction: the full-FFT path discards the singular mode instead '// &
          'of projecting the source onto the operator range, so the nonzero mean of '// &
          'd(rho)/dt + div(g_hat) is lost. Use poisson_y_method=tdma.')
      end if
      !-----------------------------------------------------------------
      ! matrice_refinement builds the pentadiagonal spectral-stretching
      ! operator only for istret = centre or 2sides; any other stretched
      ! mapping leaves its coefficients unset.
      !-----------------------------------------------------------------
      if((.not. domain(1)%is_periodic(2)) .and. &
         domain(1)%istret /= ISTRET_NO     .and. &
         domain(1)%istret /= ISTRET_CENTRE .and. &
         domain(1)%istret /= ISTRET_2SIDES) then
        call Print_error_msg('poisson_y_method=fft supports only istret = no, centre or '// &
          '2sides; matrice_refinement has no pentadiagonal construction for the requested '// &
          'stretching.')
      end if
      if(is_any_energyeq .and. any(domain(1)%is_conv_outlet(:)) .and. nrank == 0) then
        call Print_warning_msg( &
          'poisson_y_method=fft for thermal inlet/outlet flow is experimental and not yet validated.')
      end if
      if(domain(1)%istret /= ISTRET_NO .and. &
         domain(1)%mstret /= MSTRET_3FMD .and. nrank == 0) then
        call Print_warning_msg( &
          'poisson_y_method=fft requires spectral stretching; requested rstret method is overridden.')
      end if
    case(IPOISSON_Y_TDMA)
      if(domain(1)%is_periodic(2)) then
        call Print_error_msg('poisson_y_method=tdma requires a non-periodic y direction.')
      end if
      domain(:)%fft_skip_c2c(2) = .true.
    case default
      call Print_error_msg('The requested Poisson y-direction method is not supported.')
    end select
    if (.not. domain(1)%fft_skip_c2c(2)) domain(:)%mstret = MSTRET_3FMD
    !-----------------------------------------------------------------
    ! One clock per run.
    !
    ! Each field owns two different integers. iterfrom says which checkpoint to
    ! read; iteration says where the field sits on the run timeline. They have
    ! always been conflated, because a restart read assigns iteration = iterfrom.
    ! That breaks as soon as the fields are initialised differently: restarting
    ! the flow from step N while the thermal field starts fresh left the thermal
    ! clock at 0, so the solver loop ran N steps in which only the thermal field
    ! advanced, every output file carried a per-field iteration number that
    ! disagreed with the other field, and the flow stayed frozen throughout.
    !
    ! dm%iteration_start is the single timeline origin that every field adopts,
    ! whichever way it was initialised. iterfrom keeps its original meaning, and
    ! the two are now allowed to differ - which is exactly what reset does.
    !
    !   restart_clock = continue (default)
    !     The restarting field's checkpoint iteration becomes the run clock. A
    !     field that does not restart is injected fresh at that same iteration,
    !     so the loop never spins through steps that solve nothing. When more
    !     than one field restarts they must come from the same checkpoint.
    !   restart_clock = reset
    !     The checkpoint is an initial condition and nothing more. The clock
    !     starts at iteration 0 and time 0, the stored time-integration history
    !     is dropped, and the stored statistics are not read. This is the mode
    !     for seeding a run from a precursor - an isothermal field used to start
    !     a heated run - where carrying the precursor's step index, physical
    !     time and running averages forward would be wrong.
    !-----------------------------------------------------------------
    do i = 1, nxdomain
      itmp = 0
      is_tmp = .false.         ! has any field of this domain been restarted?
      if(flow(i)%inittype == INIT_RESTART) then
        itmp = flow(i)%iterfrom
        is_tmp = .true.
      end if
      if(is_any_energyeq) then
        if(domain(i)%is_thermo .and. thermo(i)%inittype == INIT_RESTART) then
          if(is_tmp .and. thermo(i)%iterfrom /= itmp) &
            call Print_error_msg('The flow and the thermal field restart from different '// &
              'checkpoints. A run advances on one timeline, so both must restart from the '// &
              'same iteration. Either align the two irestartfrom values, or initialise one '// &
              'of the fields with a non-restart inittype.')
          itmp = thermo(i)%iterfrom
          is_tmp = .true.
        end if
      end if
      if(domain(i)%restart_clock == RESTART_CLOCK_RESET) itmp = 0
      domain(i)%iteration_start = itmp
      !
      ! A field whose last iteration is at or before the run start never passes
      ! its gate in the solver loop, so it stays frozen for the whole run while
      ! the other one advances. That is almost never intended, and it is silent.
      if(flow(i)%nIterFlowEnd <= domain(i)%iteration_start .and. nrank == 0) &
        call Print_warning_msg('niterflowlast is not beyond the iteration this run starts '// &
          'from, so the flow field will never be advanced. Note that niterflowfirst and '// &
          'niterflowlast are absolute iteration numbers, not offsets from the restart.')
      if(is_any_energyeq .and. nrank == 0) then
        if(domain(i)%is_thermo) then
          if(thermo(i)%nIterThermoEnd <= domain(i)%iteration_start) &
            call Print_warning_msg('niterthermolast is not beyond the iteration this run '// &
              'starts from, so the thermal field will never be advanced.')
        end if
      end if
      if(domain(i)%restart_clock == RESTART_CLOCK_RESET .and. is_tmp .and. nrank == 0) &
        call Print_note_msg('restart_clock=reset: the restart field is used as an initial '// &
          'condition only. The run clock starts at iteration 0 and time 0, the stored '// &
          'momentum and energy RHS history is discarded, and the stored statistics are not '// &
          'read. niterflowfirst, niterthermofirst, niterflowlast, niterthermolast, '// &
          'stat_istart, ndbstart, ndbend, initReTo and the qw ramp bounds are all absolute '// &
          'iteration numbers and are therefore read on the new clock, so a run that used to '// &
          'start at irestartfrom+1 now has to start at 1.')
    end do
    if(nrank == 0) then
      do i = 1, nxdomain
        write (*, wrtfmt2s) 'restart clock mode :', get_name_restart_clock(domain(i)%restart_clock)
        write (*, wrtfmt1i) 'run clock starts from iteration :', domain(i)%iteration_start
      end do
    end if
    !-----------------------------------------------------------------
    ! restart_history_mode = compact drops the stored time-integration
    ! history and rebuilds what it can from the remaining fields. For an
    ! isothermal run the only loss is the momentum RHS history, which the
    ! AB2 startup branch absorbs. A thermal run would additionally have to
    ! rebuild rho, mu, T, h, k and sigma from rhoh through the property
    ! table; that inverse lookup is not bit-reproducible, so a restart
    ! would not continue the same trajectory. Reject the mode outright -
    ! at write time as well as read time, so no unusable compact thermal
    ! checkpoint can be produced.
    !-----------------------------------------------------------------
    if(is_any_energyeq) then
      do i = 1, nxdomain
        if(domain(i)%restart_history_mode == RESTART_HISTORY_COMPACT) &
          call Print_error_msg('restart_history_mode=compact is not supported for a thermal flow: '// &
            'the thermal properties cannot be rebuilt reproducibly from rhoh. Use '// &
            'restart_history_mode=exact.')
      end do
    end if
    !-----------------------------------------------------------------
    ! The FFT Poisson solver turns each non-periodic direction into a
    ! cosine transform by an even-mirror reordering of the cell-centred
    ! field (the nx/2, ny/2, nz/2 loops in poisson_1stderivcomp_fft2d).
    ! That reordering, e.g.
    !   do i = 1,      n/2 ; rw1b(i) = rw1(2*i-1)    ; end do
    !   do i = n/2+1,  n   ; rw1b(i) = rw1(2*n-2*i+2); end do
    ! is a permutation of 1..n only for even n; with odd n it reads one
    ! index past the end and never reads the last point, which corrupts
    ! the pressure silently. y is exempt when the y-FFT is replaced by
    ! the TDMA path, which solves the direction directly and never mirrors.
    !-----------------------------------------------------------------
    do i = 1, nxdomain
      do j = 1, NDIM
        if(domain(i)%is_periodic(j)) cycle
        if(j == 2 .and. domain(i)%fft_skip_c2c(2)) cycle
        if(.not. is_even(domain(i)%nc(j))) &
          call Print_error_msg('A non-periodic direction solved by the FFT Poisson solver '// &
            'requires an even cell count, because the solver mirrors that direction.')
      end do
    end do
    !
    ! Case 1 (closed pressure system, supercritical): periodicity is a NUMERICAL
    ! DEVICE mimicking fully-developed flow, NOT a sealed vessel. The (0,0)
    ! Poisson mode has no solution unless the complete Poisson source has zero
    ! volume mean. is_global_mass_correction makes only that local source compatible.
    ! Enable it proactively so the constraint is satisfied from the first projection
    ! step, not only after the adaptive trigger in Check_element_mass_conservation.
    ! It does not alter density or conserve the global density integral. Constant
    ! wall-heat-flux thermal mean compensation is handled separately by
    ! is_rhoh_compensated when that option is active.
    if(domain(1)%is_periodic(1) .and. is_any_energyeq .and. &
       .not. any(domain(1)%is_conv_outlet(:))) then
      is_global_mass_correction = .true.
      if(nrank == 0) call Print_warning_msg( &
        'Closed periodic+thermo pressure system: compatible Poisson source correction enabled.')
    end if
    !
    is_single_RK_projection = .false.
    if(is_any_energyeq) then
    !   if(domain(1)%ibcx_nominal(2,1)==IBC_CONVECTIVE) then
         if(is_single_RK_projection) &
         call Print_warning_msg('is_single_RK_projection on could introduce very high pressure.')
    !     !is_damping_drhodt = .true.
    !   end if
      ! Calculate_drhodt forms (rho^k - rho^{k-1})/(tAlpha(k)*dt) with dDens0
      ! re-taken every sub-step. With a single projection at isub = 3 the
      ! backward difference would cover only the last sub-step while the
      ! projection has to absorb the whole step, so drho/dt would be wrong.
      ! Block the combination until Calculate_drhodt/dDens0 handle it.
      if(is_single_RK_projection .and. (domain(1)%iTimeScheme == ITIME_RK3 .or. &
                                        domain(1)%iTimeScheme == ITIME_RK3_CN)) &
        call Print_error_msg('is_single_RK_projection is not supported with RK3 + energy equation: '// &
                             'drho/dt for the Poisson source assumes a projection at every sub-step.')
    end if
    if( nrank == 0) then
      do i = 1, nxdomain
        write (*, *) '  ----- FFT Solver -----'
        write (*, wrtfmt2s) 'FFT lib :', get_name_fft(domain(i)%ifft_lib)
        write (*, wrtfmt2s) 'requested Poisson y-method :', &
          get_name_poisson_y_method(domain(i)%ipoisson_y_method)
        if(domain(i)%fft_skip_c2c(2)) then
          write (*, wrtfmt2s) 'effective Poisson y-method :', 'tdma'
        else
          write (*, wrtfmt2s) 'effective Poisson y-method :', 'fft'
        end if
        write (*, wrtfmt3l) '3-D FFT skiping any direction? ', domain(i)%fft_skip_c2c(:)
        write (*, *) '  ----- Numerical treatments (optional, could change once in sim.) -----'
        write (*, wrtfmt1l) 'is_single_RK_projection ?', is_single_RK_projection
        write (*, wrtfmt1l) 'is_damping_drhodt ?', is_damping_drhodt
        write (*, wrtfmt1l) 'is_global_mass_correction ?', is_global_mass_correction
      end do
    end if
    !------------------------------------------------------------------------------
    if(allocated(itmpx)) deallocate(itmpx)
    if(allocated(rtmpx)) deallocate(rtmpx)
    !------------------------------------------------------------------------------
    ! convert the input dimensional temperature/heat flux into undimensional
    !------------------------------------------------------------------------------
    do i = 1, nxdomain
      if(.not. is_any_energyeq) then
        domain(i)%ibcx_nominal(1:2, 5) = domain(i)%ibcx_nominal(1:2, 1)
        domain(i)%ibcy_nominal(1:2, 5) = domain(i)%ibcy_nominal(1:2, 2)
        domain(i)%ibcz_nominal(1:2, 5) = domain(i)%ibcz_nominal(1:2, 3)
      end if
      call config_calc_basic_ibc(domain(i))
      call config_calc_eqs_ibc(domain(i))
    end do
    !------------------------------------------------------------------------------
    ! set up constant for time step marching
    !------------------------------------------------------------------------------
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
!==============================================================================
!> \brief Restore the case-driven geometry defaults on one domain.
!>
!> The canonical non-dimensionalisation fixes the cross-stream extent for every
!> built-in case (half-height 1 for a channel, radius 1 for a pipe, 2*pi in the
!> azimuthal direction, ...), so whatever lxx/lyb/lyt/lzz the input file carries
!> is overwritten here. The icase -> icoordinate mapping lives in the same place
!> because it is driven by the same choice.
!>
!> Shared by the primary input reader and by the mesh-mapping target reader so
!> the two cannot drift apart - one input file must describe one mesh whichever
!> reader consumes it.
!>
!> - dm (inout): domain descriptor; %icase must already be set.
  subroutine apply_case_geometry_defaults(dm)
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm

    if (dm%icase == ICASE_CHANNEL) then
      dm%lyb = - ONE
      dm%lyt = ONE
    else if (dm%icase == ICASE_DUCT) then
      dm%lyb = - ONE
      dm%lyt = ONE
    else if (dm%icase == ICASE_PIPE) then
      dm%lyb = ZERO
      dm%lyt = ONE
      dm%lzz = TWOPI
    else if (dm%icase == ICASE_ANNULAR) then
      dm%lyt = ONE
      dm%lzz = TWOPI
    else if (dm%icase == ICASE_TGV2D .or. dm%icase == ICASE_TGV3D) then
      dm%lxx = TWOPI
      dm%lzz = TWOPI
      dm%lyt =   PI
      dm%lyb = - PI
    else if (dm%icase == ICASE_BURGERS) then
      dm%lxx = TWO
      dm%lzz = TWO
      dm%lyt = TWO
      dm%lyb = ZERO
    else if (dm%icase == ICASE_ALGTEST) then
      dm%lxx = TWOPI
      dm%lzz = TWOPI
      dm%lyt = TWOPI
      dm%lyb = ZERO
    else
      ! do nothing...
    end if
    !------------------------------------------------------------------------------
    ! coordinates type
    !------------------------------------------------------------------------------
    if (dm%icase == ICASE_PIPE) then
      dm%icoordinate = ICYLINDRICAL
    else if (dm%icase == ICASE_ANNULAR) then
      dm%icoordinate = ICYLINDRICAL
    else
      dm%icoordinate = ICARTESIAN
    end if
    dm%fft_skip_c2c(:) = .false.

    return
  end subroutine apply_case_geometry_defaults
!==============================================================================
!> \brief Apply the mesh-count and stretching defaults on one domain.
!>
!> A cylindrical mesh needs an even azimuthal cell count so that each cell has
!> an exact partner across the axis; the axis reconstruction pairs (k, k+ncz/2).
!>
!> Shared by the primary input reader and by the mesh-mapping target reader -
!> see apply_case_geometry_defaults.
!>
!> - dm (inout): domain descriptor; %icase, %icoordinate, %nc and %istret must
!>   already be set.
  subroutine apply_mesh_stretching_defaults(dm)
    use EvenOdd_mod
    use mpi_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm

    if(dm%icoordinate == ICYLINDRICAL) then
      if (.not. is_even(dm%nc(3))) dm%nc(3) = dm%nc(3) + 1
    end if
    !------------------------------------------------------------------------------
    !     stretching
    !------------------------------------------------------------------------------
    dm%is_stretching(:) = .false.
    if(dm%istret /= ISTRET_NO) dm%is_stretching(2) = .true.
    if (dm%icase == ICASE_CHANNEL .and. &
        dm%istret /= ISTRET_2SIDES .and. &
        dm%istret /= ISTRET_NO ) then
      if(nrank == 0) call Print_warning_msg ("Grids are neither uniform nor two-side clustered.")
    else if (dm%icase == ICASE_PIPE .and. &
             dm%istret /= ISTRET_TOP) then
      if(nrank == 0) call Print_warning_msg ("Grids are not near-wall clustered.")
    else if (dm%icase == ICASE_ANNULAR .and. &
             dm%istret /= ISTRET_2SIDES .and. &
             dm%istret /= ISTRET_NO) then
      if(nrank == 0) call Print_warning_msg ("Grids are neither uniform nor two-side clustered.")
    else if (dm%icase == ICASE_TGV2D .or. &
             dm%icase == ICASE_TGV3D .or. &
             dm%icase == ICASE_ALGTEST) then
      if(dm%istret /= ISTRET_NO .and. nrank == 0) &
      call Print_warning_msg ("Grids are clustered.")
    else
      ! do nothing...
    end if

    return
  end subroutine apply_mesh_stretching_defaults
!==============================================================================
!> \brief Read the target-mesh description used by the mesh-mapping pre-run.
!>
!> The pre-run (`is_prerun`) interpolates the running solution onto the mesh
!> declared in a secondary input file (`input_chapsim_tgt.ini`) and writes it as
!> a restart. Only `[domain]` and `[mesh]` are meaningful for that; every other
!> section is consumed and discarded, so the target file may be - and normally
!> is - a complete copy of a regular input file.
!>
!> The keyword grammar and the defaults are the primary reader's, by
!> construction: the same file must describe the same mesh whether it is fed in
!> as `input_chapsim.ini` or as the interpolation target. Reading these keys by
!> hand is what let the target reader rot when the inputs moved from numeric
!> codes to keywords.
!>
!> Multi-domain targets are not supported; the first value of a per-domain key
!> is taken.
!>
!> - dm (inout): target domain descriptor; only the geometry and mesh members
!>   are set here. The caller owns everything else.
!> - flinput (in): path to the target input file.
  subroutine Read_input_parameters_target(dm, flinput)
    use mpi_mod
    use udf_type_mod
    use wtformat_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    character(len = *), intent(in) :: flinput

    integer :: ioerr, inputUnit
    integer :: slen
    character(len = 80)  :: secname
    character(len = 80)  :: section_keys(INPUT_SECTION_MAX)
    character(len = 256) :: section_values(INPUT_SECTION_MAX)
    integer :: section_n
    integer :: section_idx
    logical :: has_domain, has_mesh

    has_domain = .false.
    has_mesh   = .false.
    input_section_reached_eof = .false.

    call open_clean_input_file(flinput, inputUnit)
    if(nrank == 0) &
    call Print_debug_start_msg("Reading target mesh parameters from "//trim(flinput)//" ...")

    do
      if(input_section_reached_eof) exit
      read(inputUnit, '(a)', iostat = ioerr) secname
      if (ioerr /= 0) exit
      slen = len_trim(secname)
      if (slen == 0) cycle
      if (secname(1:1) /= '[') cycle
      if ( secname(1:slen) == '[domain]' ) then
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        section_idx = section_find_key(section_keys, section_n, 'icase')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key icase in section [domain] of '//trim(flinput))
        call parse_icase(section_values(section_idx), dm%icase)
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), 'lxx', dm%lxx)
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), 'lyt', dm%lyt)
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), 'lyb', dm%lyb)
        call section_get_real(section_keys, section_values, section_n, secname(1:slen), 'lzz', dm%lzz)
        call apply_case_geometry_defaults(dm)
        has_domain = .true.
      else if ( secname(1:slen) == '[mesh]' ) then
        if(.not. has_domain) &
        call Print_error_msg('Section [mesh] precedes [domain] in '//trim(flinput)// &
                             '; the mesh defaults depend on icase.')
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), 'ncx', dm%nc(1))
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), 'ncy', dm%nc(2))
        call section_get_integer(section_keys, section_values, section_n, secname(1:slen), 'ncz', dm%nc(3))
        section_idx = section_find_key(section_keys, section_n, 'istret')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key istret in section [mesh] of '//trim(flinput))
        call parse_istret(section_values(section_idx), dm%istret)
        section_idx = section_find_key(section_keys, section_n, 'rstret')
        if(section_idx == 0) &
        call Print_error_msg('Missing required key rstret in section [mesh] of '//trim(flinput))
        call parse_rstret('rstret='//trim(section_values(section_idx)), dm%mstret, dm%rstret)
        dm%ifft_lib = FFT_2DECOMP_3DFFT
        call apply_mesh_stretching_defaults(dm)
        has_mesh = .true.
      else
        ! not needed by the interpolation; consume the body so the next header is reached
        call read_input_section(inputUnit, secname(1:slen), section_keys, section_values, section_n)
      end if
    end do
    close(inputUnit)

    if(.not. has_domain) &
    call Print_error_msg('Missing section [domain] in '//trim(flinput))
    if(.not. has_mesh) &
    call Print_error_msg('Missing section [mesh] in '//trim(flinput))

    if(nrank == 0) then
      write (*, wrtfmt2s) 'target icase id :', get_name_case(dm%icase)
      write (*, wrtfmt2s) 'target coordinates system :', get_name_cs(dm%icoordinate)
      write (*, wrtfmt1r) 'target scaled length in x-direction :', dm%lxx
      write (*, wrtfmt1r) 'target scaled length in y-direction :', dm%lyt - dm%lyb
      write (*, wrtfmt1r) 'target scaled length in z-direction :', dm%lzz
      write (*, wrtfmt1i) 'target mesh cell number - x :', dm%nc(1)
      write (*, wrtfmt1i) 'target mesh cell number - y :', dm%nc(2)
      write (*, wrtfmt1i) 'target mesh cell number - z :', dm%nc(3)
      write (*, wrtfmt2s) 'target mesh y-stretching type :', get_name_mesh(dm%istret)
      if(dm%istret /= ISTRET_NO) then
        write (*, wrtfmt1r) 'target mesh y-stretching factor :', dm%rstret
        write (*, wrtfmt2s) 'target mesh y-stretching method :', get_name_mstret(dm%mstret)
      end if
      call Print_debug_end_msg()
    end if
    if((dm%lyt - dm%lyb) < ZERO) call Print_error_msg("Target y length is smaller than zero.")

    return
  end subroutine Read_input_parameters_target
end module
!==============================================================================
!==============================================================================
!> Approximate pre-run estimates for DNS mesh and time-step suitability.
!>
!> The routines in this module provide empirical checks for wall-bounded cases,
!> including approximate skin-friction estimates, wall-unit spacing, Hartmann
!> layer spacing, and CFL-related time-step guidance.
module apx_prerun_mod
  use input_general_mod
  use math_mod
  use mpi_mod
  use parameters_constant_mod
  use wtformat_mod
  implicit none
  real(WP), parameter :: dxplus_max = 10.0_WP
  real(WP), parameter :: dzplus_max = 5.0_WP
  real(WP), parameter :: dyplus_max = 1.0_WP
  real(WP), parameter :: Cflmax = 0.714_WP
  real(WP), parameter :: Ctmmax = 0.1_WP
  real(WP), save :: dymax, dymin, rmin, rmax, Re_tau, u_tau
  private :: solve_Prandtl_vonKarman_eq_for_cf
  private :: estimate_skin_friction_factor
  private :: estimate_friction_and_retau
  private :: local_growth_rate
  private :: count_points_near_wall
  public :: estimate_temporal_resolution
  public :: estimate_spacial_resolution
contains
!==============================================================================
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
!==============================================================================
  subroutine estimate_skin_friction_factor(cf, Re, icase)
    implicit none
    real(WP), intent(in)  :: Re
    integer, intent(in)   :: icase
    real(WP), intent(out) :: cf
    if(icase == ICASE_PIPE .or. &
       icase == ICASE_ANNULAR) then
      if(Re < 2300.0_WP) then
        ! Laminar Darcy friction factor for internal flow.
        cf = 64.0_WP/Re
      else if (Re < 3.0e4_WP) then
        ! Blasius Darcy friction factor.
        cf = 0.316_WP * Re**(-0.25_WP)
      else if (Re < 1.0e6_WP) then
        ! McAdams Darcy friction factor.
        cf = 0.814_WP * Re**(-0.2_WP)
      else
        ! Prandtl-von Karman skin-friction estimate.
        call solve_Prandtl_vonKarman_eq_for_cf(cf, Re, icase)
      end if
    else if (icase == ICASE_CHANNEL) then
      if(Re < 1.0e4_WP) then
        ! Turbulent channel skin-friction estimate.
        cf = 0.079_WP * Re**(-0.25_WP)
      else
       ! Prandtl-von Karman skin-friction estimate.
       call solve_Prandtl_vonKarman_eq_for_cf(cf, Re, icase)
      end if
    else
      cf = MAXP
    end if
    return
  end subroutine
!==============================================================================
  subroutine estimate_friction_and_retau(fl, dm, cf_skin, f_darcy, Re_corr)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),  intent(in)  :: fl
    real(WP), intent(out) :: cf_skin, f_darcy, Re_corr
    real(WP) :: cf_est, Re_diameter

    cf_skin = MAXP
    f_darcy = MAXP
    Re_corr = fl%ren

    if(dm%icase == ICASE_PIPE) then
      ! Pipe correlations use diameter Reynolds number; fl%ren is radius-based.
      Re_diameter = TWO * fl%ren
      Re_corr = Re_diameter
      call estimate_skin_friction_factor(cf_est, Re_diameter, dm%icase)
      f_darcy = cf_est
      cf_skin = f_darcy / FOUR
      Re_tau = fl%ren * sqrt_wp(f_darcy / EIGHT)
    else if(dm%icase == ICASE_ANNULAR) then
      ! Approximate annular check: use hydraulic diameter based on gap width.
      Re_corr = TWO * fl%ren * (dm%lyt - dm%lyb) / MAX(dm%lyt, MINP)
      call estimate_skin_friction_factor(cf_est, Re_corr, dm%icase)
      f_darcy = cf_est
      cf_skin = f_darcy / FOUR
      Re_tau = fl%ren * sqrt_wp(f_darcy / EIGHT)
    else if(dm%icase == ICASE_CHANNEL) then
      Re_corr = fl%ren
      call estimate_skin_friction_factor(cf_est, fl%ren, dm%icase)
      cf_skin = cf_est
      f_darcy = FOUR * cf_skin
      Re_tau = fl%ren * sqrt_wp(cf_skin / TWO)
    else
      Re_tau = MAXP
    end if

    u_tau = Re_tau / fl%ren
    return
  end subroutine
!==============================================================================
  pure real(WP) function local_growth_rate(dya, dyb)
    implicit none
    real(WP), intent(in) :: dya, dyb
    if(MIN(dya, dyb) <= MINP) then
      local_growth_rate = MAXP
    else
      local_growth_rate = MAX(dya, dyb) / MIN(dya, dyb)
    end if
    return
  end function
!==============================================================================
  pure integer function count_points_near_wall(y, wall, dist)
    implicit none
    real(WP), intent(in) :: y(:), wall, dist
    count_points_near_wall = count(abs_wp(y - wall) <= dist)
    return
  end function
!==============================================================================
  !> Estimate whether the current mesh is adequate for wall-bounded DNS.
  !> - fl (in): Flow configuration.
  !> - dm (in): Domain and mesh descriptor.
  !> - opt_mh (in): Optional MHD configuration for Hartmann-layer checks.
  subroutine estimate_spacial_resolution(fl, dm, opt_mh)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),  intent(in)  :: fl
    type(t_mhd),   intent(in), optional :: opt_mh
    ! local variables
    real(WP) :: dx_max, dy_max, dz_max
    real(WP) :: cf_skin, f_darcy, Re_corr
    real(WP) :: dy_low1, dy_low2, dy_high1, dy_high2, dy_mid
    real(WP) :: yplus_low, yplus_high, yplus_mid, dxplus, dzplus, dzplus_inner
    real(WP) :: ha_bl, ha_bl_plus
    real(WP) :: growth_low, growth_high
    integer  :: n_pnts_ha, n_pnts_ha_low, n_pnts_ha_high
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
      write(*, '(A)') '  Note: Lx >= 2pi for channel flow (approx. 4pi preferred for large-scale structures)'
    else if(dm%icase == ICASE_PIPE) then
      write(*, '(A)') '  Note: Lx >= 2pi for pipe flow (approx. 8-10 pipe diameters)'
    end if
    if(dm%icoordinate == ICARTESIAN) then
      write(*, wrtfmt2r) 'Spanwise (z):   Current | Recom. min:', dm%lzz, PI
      write(*, '(A)') '  Note: Lz >= pi for adequate spanwise correlation'
    end if
    ! estimate Re_tau and u_tau
    rmax = ONE
    rmin = ONE
    if(dm%icoordinate == ICYLINDRICAL) then
      rmin = dm%yc(1)
      rmax = dm%yp(dm%np(2))
    end if
    call estimate_friction_and_retau(fl, dm, cf_skin, f_darcy, Re_corr)
    call Print_debug_mid_msg("Flow Parameters:")
    write(*, wrtfmt1r) 'Friction Reynolds number (Re_tau):', Re_tau
    write(*, wrtfmt1r) 'Friction velocity (u_tau)        :', u_tau
    write(*, wrtfmt1r) 'Skin-friction coefficient (Cf)   :', cf_skin
    if(dm%icase == ICASE_PIPE .or. dm%icase == ICASE_ANNULAR) then
      write(*, wrtfmt1r) 'Darcy friction factor estimate   :', f_darcy
      write(*, wrtfmt1r) 'Correlation Reynolds number      :', Re_corr
      if(dm%icase == ICASE_ANNULAR) &
        write(*, '(A)') '  Note: annular Re_tau is a hydraulic-diameter estimate; inner/outer wall shear can differ.'
    end if
    ! calculate current mesh spacing in normal units. yp(:) is node distance from the lower wall/axis.
    dy_low1  = dm%yp(2) - dm%yp(1)
    dy_low2  = dm%yp(3) - dm%yp(2)
    dy_high1 = dm%yp(dm%np(2)) - dm%yp(dm%np(2)-1)
    dy_high2 = dm%yp(dm%np(2)-1) - dm%yp(dm%np(2)-2)
    dy_mid   = dm%yp(dm%np(2)/2) - dm%yp(dm%np(2)/2-1)
    yplus_low  = Re_tau * dy_low1
    yplus_high = Re_tau * dy_high1
    yplus_mid  = Re_tau * dy_mid
    dxplus = Re_tau * dm%h(1)
    dzplus = Re_tau * dm%h(3)
    dzplus_inner = dzplus
    if(dm%icoordinate == ICYLINDRICAL) then
      dzplus = Re_tau * dm%h(3) * rmax
      dzplus_inner = Re_tau * dm%h(3) * rmin
    end if
    !
    call Print_debug_mid_msg("Current Mesh Resolution (wall units)")
    write(*, '(A)')    '    Wall-normal/radial direction:'
    if(dm%icase == ICASE_PIPE) then
      write(*, wrtfmt1r) '    Delta r+ at pipe wall              :', yplus_high
      write(*, wrtfmt1r) '    Delta r+ near pipe axis            :', yplus_low
      write(*, wrtfmt1r) '    Delta r+ at radial middle          :', yplus_mid
    else if(dm%icase == ICASE_ANNULAR) then
      write(*, wrtfmt1r) '    Delta r+ at inner wall             :', yplus_low
      write(*, wrtfmt1r) '    Delta r+ at outer wall             :', yplus_high
      write(*, wrtfmt1r) '    Delta r+ at gap middle             :', yplus_mid
    else
      write(*, wrtfmt1r) '    Delta y+ at lower wall             :', yplus_low
      write(*, wrtfmt1r) '    Delta y+ at upper wall             :', yplus_high
      write(*, wrtfmt1r) '    Delta y+ at channel centre         :', yplus_mid
    end if
    !
    ! calculate grid stretching rates
    growth_low = local_growth_rate(dy_low1, dy_low2)
    growth_high = local_growth_rate(dy_high1, dy_high2)
    call Print_debug_mid_msg("Grid Stretching Assessment")
    if(dm%icase == ICASE_PIPE) then
      write(*, wrtfmt2r) 'Growth rate near axis | near wall:', growth_low, growth_high
    else if(dm%icase == ICASE_ANNULAR) then
      write(*, wrtfmt2r) 'Growth rate inner wall | outer wall:', growth_low, growth_high
    else
      write(*, wrtfmt2r) 'Growth rate lower wall | upper wall:', growth_low, growth_high
    end if
    write(*, '(A)')    '  Recommended: Growth rate < 1.2-1.3 for DNS'
    if(growth_low > 1.3_WP .or. growth_high > 1.3_WP) then
      call Print_warning_msg("Grid stretching is too aggressive! Reduce stretching factor.")
      write(*, '(A)') '    High stretching can cause numerical errors and inaccurate statistics'
    else if(growth_low > 1.2_WP .or. growth_high > 1.2_WP) then
      write(*, '(A)') '  Caution: Growth rate approaching upper limit'
    else
      write(*, '(A)') '  Grid stretching is acceptable'
    end if
    !
    if(dm%icase == ICASE_PIPE) then
      if(yplus_high > ONE) then
        call Print_warning_msg("Pipe wall spacing too large! Delta r+ should be <= 1.0 for DNS")
        write(*, '(A,F6.2,A)') '    Current wall Delta r+ = ', yplus_high, ' -> Increase Ny or adjust stretching'
      else
        write(*, '(A)') '  Pipe wall resolution is acceptable'
      end if
    else if(MAX(yplus_low, yplus_high) > ONE) then
      call Print_warning_msg("Wall spacing too large! Delta y+/r+ should be <= 1.0 for DNS")
      write(*, '(A,F6.2,A)') '    Current maximum wall spacing = ', MAX(yplus_low, yplus_high), &
                             ' -> Increase Ny or adjust stretching'
    else
      write(*, '(A)') '  Wall resolution is acceptable'
    end if
    ! MHD
    if (dm%is_mhd) then
      if(.not. present(opt_mh)) call Print_error_msg("Error. Opt_mhd is required.")
      ha_bl = ONE / opt_mh%NHartmn
      ha_bl_plus = ha_bl * Re_tau
      n_pnts_ha_low = 0
      n_pnts_ha_high = count_points_near_wall(dm%yp, dm%lyt, ha_bl)
      if(dm%icase == ICASE_CHANNEL .or. dm%icase == ICASE_ANNULAR) &
        n_pnts_ha_low = count_points_near_wall(dm%yp, dm%lyb, ha_bl)
      n_pnts_ha = MAX(n_pnts_ha_low, n_pnts_ha_high)
      write(*, '(A)') repeat('-', 80)
      call Print_debug_mid_msg("At the current mesh configuration: MHD")
      write(*, wrtfmt1r) 'MHD boundary layer thickness (delta_Ha)  :', ha_bl
      write(*, wrtfmt1r) 'MHD boundary layer thickness (delta_Ha+):', ha_bl_plus
      if(dm%icase == ICASE_PIPE) then
        write(*, wrtfmt1i) 'Grid points in wall-side Ha layer       :', n_pnts_ha_high
      else
        write(*, wrtfmt2i) 'Grid points in lower/inner | upper/outer Ha layer:', n_pnts_ha_low, n_pnts_ha_high
      end if
      write(*, '(A)') '  Note: this is a wall-distance estimate; exact Hartmann walls depend on B-field orientation.'
      if (n_pnts_ha < 10) then
        call Print_warning_msg("Insufficient grid points in MHD boundary layer. Recommend at least 10 points.")
      end if
    end if
    ! Calculate recommended minimum mesh resolution
    if(dm%icase == ICASE_PIPE) then
      dymax = MAX(dy_high1, dy_mid)
      dymin = MIN(dy_high1, dy_mid)
    else
      dymax = MAX(dy_low1, dy_high1, dy_mid)
      dymin = MIN(dy_low1, dy_high1, dy_mid)
    end if
    !
    dx_max = dxplus_max / Re_tau
    dz_max = dzplus_max / Re_tau
    if(dm%icoordinate == ICYLINDRICAL) dz_max = dzplus_max / Re_tau / rmax
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
    write(*, wrtfmt1r)   '    Delta x+                       :', dxplus
    write(*, '(A,F6.1)') '    Recommended: Delta x+ <= ', dxplus_max
    write(*, '(A)') ''
    if(dm%icoordinate == ICYLINDRICAL) then
      write(*, '(A)')      '  Azimuthal direction (theta):'
      write(*, wrtfmt1r)   '    r Delta theta+ at outer wall   :', dzplus
      if(dm%icase == ICASE_ANNULAR) &
        write(*, wrtfmt1r) '    r Delta theta+ at inner wall   :', dzplus_inner
    else
      write(*, '(A)')      '  Spanwise direction (z):'
      write(*, wrtfmt1r)   '    Delta z+                       :', dzplus
    end if
    write(*, '(A,F6.1)') '    Recommended: <= ', dzplus_max
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
      write(*, '(A)') '  Current mesh meets minimum DNS resolution requirements'
    else
      call Print_warning_msg("Current mesh is below recommended DNS resolution!")
      if(dm%nc(1) < nx_min) write(*, '(A,I0,A,I0)') '    Increase Nx: ', dm%nc(1), ' -> ', nx_min
      if(dm%nc(2) < ny_min) write(*, '(A,I0,A,I0)') '    Increase Ny: ', dm%nc(2), ' -> ', ny_min
      if(dm%nc(3) < nz_min) write(*, '(A,I0,A,I0)') '    Increase Nz: ', dm%nc(3), ' -> ', nz_min
    end if
    write(*, '(A)') repeat('=', 80)
    write(*, '(A)') ''
  return
  end subroutine
!==============================================================================
  !> Estimate time-step and CFL suitability before a production run.
  !> - fl (in): Flow configuration.
  !> - dm (in): Domain and mesh descriptor.
  subroutine estimate_temporal_resolution(fl, dm)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),  intent(in)  :: fl
    real(WP) :: dt_max_cfl1, dt_max_cfl2, dt_max_phy, dxyz_max, dt_min
    real(WP) :: t_flth, dymin_local, r_for_theta, dzmin_local
    real(WP) :: cf_skin, f_darcy, Re_corr
    integer :: nt_cur, nt_est
    if(nrank /= 0) return
    if(dm%icase /= ICASE_PIPE .and. &
       dm%icase /= ICASE_ANNULAR .and. &
       dm%icase /= ICASE_CHANNEL) return
    call estimate_friction_and_retau(fl, dm, cf_skin, f_darcy, Re_corr)
    dymin_local = MINVAL(dm%yp(2:dm%np(2)) - dm%yp(1:dm%np(2)-1))
    r_for_theta = ONE
    if(dm%icoordinate == ICYLINDRICAL) then
      if(dm%icase == ICASE_PIPE) then
        r_for_theta = MAX(dm%lyt, MINP)
      else
        r_for_theta = MAX(dm%lyb, MINP)
      end if
    end if
    dzmin_local = dm%h(3) * r_for_theta
    ! dt limits
    dt_max_cfl1 = Cflmax * MIN(dm%h(1), dymin_local, dzmin_local) / TWO
    dxyz_max = ONE / (dm%h(1)**2) + ONE /(dymin_local**2) + ONE/(dzmin_local**2)
    dt_max_cfl2 = fl%ren / TWO / dxyz_max
    ! Delta t+ ~= 0.1 in wall units.
    dt_max_phy = Ctmmax *(fl%ren/Re_tau/Re_tau)
    dt_min = MIN(dt_max_cfl1, dt_max_cfl2, dt_max_phy)
    call Print_debug_mid_msg("Estimating the temporal resolution (based on isothermal flow)")
    write(*, wrtfmt1e) 'current dt :', dm%dt
    write(*, wrtfmt1e) 'dt_max (convection CFL  ) :', dt_max_cfl1
    write(*, wrtfmt1e) 'dt_max (diffusion  CFL  ) :', dt_max_cfl2
    write(*, wrtfmt1e) 'dt_max (dt+ = 0.1) :', dt_max_phy
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
    write(*, *)  '1_data: restart/checkpoint data and restartable raw solver data'
    write(*, *)  '2_visu: visualisation files in visu_data, visu_xdmf, and visu_mesh'
    write(*, *)  '3_monitor: monitored bulk properties, probed points, and mass conservation'
    write(*, *)  '4_check: check mesh grid distribution, initial velocity profiles'
    call Print_debug_start_msg()
    return
  end subroutine
end module
