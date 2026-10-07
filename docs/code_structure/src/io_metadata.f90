!> Shared text metadata helpers for restartable checkpoint outputs.
module checkpoint_metadata_mod
  use io_files_mod
  use io_tools_mod
  use parameters_constant_mod
  use print_msg_mod
  implicit none

  private
  public :: write_checkpoint_manifest
  public :: read_checkpoint_metadata

contains

!==============================================================================
  subroutine write_checkpoint_manifest(idom, iter, time, dt)
    implicit none
    integer,  intent(in) :: idom
    integer,  intent(in) :: iter
    real(WP), intent(in) :: time
    real(WP), intent(in) :: dt

    character(256) :: manifest_file
    integer :: u
    integer :: ios

    if(nrank /= 0) return

    call generate_pathfile_name(manifest_file, idom, 'checkpoint_meta', dir_data, 'dat', iter)
    open(newunit=u, file=trim(manifest_file), status='replace', action='write', iostat=ios)
    if(ios /= 0) then
      call Print_warning_msg("Could not write checkpoint manifest file: "//trim(manifest_file))
      return
    end if

    write(u, '(A)') 'CHAPSim_checkpoint_v1'
    write(u, '(A,1X,I0)')        'domain',    idom
    write(u, '(A,1X,I0)')        'iteration', iter
    write(u, '(A,1X,ES24.16E3)') 'time',      time
    write(u, '(A,1X,ES24.16E3)') 'dt',        dt

    call write_checkpoint_manifest_entry(u, idom, 'flow_restart', iter)
    call write_checkpoint_manifest_entry(u, idom, 'thermo_restart', iter)
    call write_checkpoint_manifest_entry(u, idom, 'flow_stats', iter)
    call write_checkpoint_manifest_entry(u, idom, 'thermo_stats', iter)
    call write_checkpoint_manifest_entry(u, idom, 'mhd_stats', iter)
    call write_checkpoint_manifest_entry(u, idom, 'xoutlet_database', iter)

    close(u)

    return
  end subroutine write_checkpoint_manifest

!==============================================================================
  subroutine write_checkpoint_manifest_entry(u, idom, group_name, iter)
    implicit none
    integer,      intent(in) :: u
    integer,      intent(in) :: idom
    character(*), intent(in) :: group_name
    integer,      intent(in) :: iter

    character(256) :: bin_file
    character(256) :: meta_file
    character(256) :: bin_name
    character(256) :: meta_name

    call generate_pathfile_name(bin_file, idom, trim(group_name), dir_data, 'bin', iter)
    call generate_pathfile_name(meta_file, idom, trim(group_name)//'_meta', dir_data, 'dat', iter)

    if(.not. file_exists(trim(bin_file))) return
    if(.not. file_exists(trim(meta_file))) return

    write(bin_name, '(A,I0,A,A,A,I0,A)') 'domain', idom, '_', &
      trim(group_name), '_', iter, '.bin'
    write(meta_name, '(A,I0,A,A,A,I0,A)') 'domain', idom, '_', &
      trim(group_name)//'_meta', '_', iter, '.dat'
    write(u, '(A,1X,A,1X,A)') trim(group_name), trim(bin_name), trim(meta_name)

    return
  end subroutine write_checkpoint_manifest_entry

!==============================================================================
  subroutine read_checkpoint_metadata(idom, iter, time, dt, found)
    implicit none
    integer,  intent(in)  :: idom
    integer,  intent(in)  :: iter
    real(WP), intent(out) :: time
    real(WP), intent(out) :: dt
    logical,  intent(out) :: found

    character(256) :: manifest_file
    character(256) :: line
    character(32)  :: label
    integer :: io_unit
    integer :: domain_file
    integer :: iter_file
    integer :: ios
    logical :: manifest_exists

    time = ZERO
    dt = ZERO
    found = .false.
    domain_file = -1
    iter_file = -1

    call generate_pathfile_name(manifest_file, idom, 'checkpoint_meta', dir_data, 'dat', iter)
    inquire(file=trim(manifest_file), exist=manifest_exists)
    if(.not. manifest_exists) return

    open(newunit=io_unit, file=trim(manifest_file), status='old', action='read', iostat=ios)
    if(ios /= 0) then
      if(nrank == 0) call Print_warning_msg("Could not open checkpoint manifest file: "//trim(manifest_file))
      return
    end if

    read(io_unit, '(A)', iostat=ios) line
    if(ios == 0 .and. trim(line) == 'CHAPSim_checkpoint_v1') then
      read(io_unit, *, iostat=ios) label, domain_file
      if(ios == 0 .and. trim(label) /= 'domain') ios = 1
      if(ios == 0) read(io_unit, *, iostat=ios) label, iter_file
      if(ios == 0 .and. trim(label) /= 'iteration') ios = 1
      if(ios == 0) read(io_unit, *, iostat=ios) label, time
      if(ios == 0 .and. trim(label) /= 'time') ios = 1
      if(ios == 0) read(io_unit, *, iostat=ios) label, dt
      if(ios == 0 .and. trim(label) /= 'dt') ios = 1
    else
      ios = 1
    end if
    close(io_unit)

    found = (ios == 0 .and. domain_file == idom .and. iter_file == iter)
    if(.not. found .and. nrank == 0) &
      call Print_warning_msg("Checkpoint manifest file is invalid: "//trim(manifest_file))

    return
  end subroutine read_checkpoint_metadata

end module checkpoint_metadata_mod
