!> Restart and instantaneous-field I/O.
!>
!> This module writes and reads solver state fields used for checkpoint/restart
!> and for inlet/outlet database exchange. It covers flow, thermal, and
!> streamwise plane database files.
module io_restart_mod
  use decomp_2d_io
  use decomp_2d_io_object_mpi
  use checkpoint_metadata_mod, only: read_checkpoint_metadata, write_checkpoint_manifest
  use io_files_mod
  use io_tools_mod
  use parameters_constant_mod
  use print_msg_mod
  use udf_type_mod
  implicit none

  character(len=10), parameter :: io_name = "restart-io"

  public  :: write_instantaneous_flow
  public  :: read_instantaneous_flow
  public  :: restore_flow_variables_from_restart

  public  :: write_instantaneous_thermo
  public  :: read_instantaneous_thermo
  public  :: restore_thermo_variables_from_restart
  public  :: read_les_restart_state
  private :: write_les_restart_state
  private :: les_restart_fields
  private :: les_restart_shapes

  private :: append_instantaneous_xoutlet
  private :: assign_instantaneous_xinlet
  private :: read_instantaneous_xinlet_tinterp
  private :: assign_instantaneous_xinlet_tinterp
  private :: load_xinlet_block_current
  private :: load_xinlet_block_next
  private :: read_xinlet_block
  private :: swap_xinlet_block_buffers
  private :: check_xinlet_database_dt
  private :: check_xinlet_constant_dt
  public  :: write_instantaneous_xoutlet
  public  :: read_instantaneous_xinlet

  !private :: write_instantaneous_plane !not used
  !private :: read_instantaneous_plane !not used
  private :: write_flow_restart_bundle
  private :: read_flow_restart_bundle
  private :: read_flow_restart_bundle_initial
  private :: initialise_flow_from_restart_q
  private :: write_thermo_restart_bundle
  private :: write_thermo_interp_metadata
  ! appended to the stop messages of a missing thermal checkpoint
  character(*), parameter :: THERMO_LABEL_HINT = 'Thermal checkpoints written by builds before 2026-10-30 '// &
    'are labelled by the thermal step count: if the energy equation started after the flow (niterthermofirst '// &
    '> niterflowfirst), their labels are lower than the flow step at which they were written.'
  private :: write_thermo_flow_iteration, check_thermo_flow_iteration
  private :: check_thermo_interp_metadata
  private :: read_thermo_restart_bundle
  private :: write_xoutlet_database_bundle
  private :: read_xoutlet_database_bundle
  private :: read_xoutlet_database_per_field_interp
  private :: ensure_xinlet_database_mesh
  private :: infer_xoutlet_database_mesh
  private :: xoutlet_database_file_elements
  private :: setup_xinlet_database_source_mesh
  private :: read_interp_xinlet_database_field
  private :: interp_xinlet_database_array_yz
  private :: compute_xinlet_database_halo_level
  private :: check_xinlet_database_halo_layout
  private :: bilinear_interp_xinlet_point_halo
  private :: build_xinlet_z_search_coords
  private :: binary_search_loc2index_yz
  private :: get_xinlet_z_coord
  private :: fill_xinlet_z_coords
  private :: xoutlet_database_fields
  private :: xoutlet_database_file_iter
  private :: cleanup_xoutlet_database_bundle_files
  private :: cleanup_xoutlet_database_per_field_files
  private :: remove_xoutlet_per_field_file
  private :: write_bundle_metadata
  private :: validate_bundle_metadata
  private :: check_bundle_file_size
  private :: read_xoutlet_bundle_metadata
  private :: metadata_value
  private :: read_metadata_line
  private :: parse_metadata_int3
  private :: parse_metadata_int
  private :: bundle_shape
  private :: flow_restart_fields
  private :: flow_restart_fields_exact
  private :: flow_restart_fields_compact
  private :: flow_restart_shapes
  private :: flow_restart_shapes_exact
  private :: flow_restart_shapes_compact
  ! needed by io_field_interpolation_mod, which has to build a target restart
  ! that matches exactly what the writers below expect
  public  :: is_restart_history_exact
  private :: ensure_restart_convective_outlet_supported
  private :: write_flow_xoutlet_state_per_field
  private :: read_flow_xoutlet_state_per_field
  private :: write_flow_restart_xoutlet_state
  private :: read_flow_restart_xoutlet_state
  private :: is_same_bits
  private :: legacy_fbcx_policy, check_legacy_fbcx_state, is_true_on_all_ranks
  private :: fbcx_thermal_state_metadata_file, write_fbcx_thermal_state_metadata, read_fbcx_thermal_state_metadata
  private :: write_flow_restart_xoutlet_state_compact
  private :: read_flow_restart_xoutlet_state_compact
  private :: thermo_restart_fields
  private :: thermo_restart_shapes
  private :: rebuild_compact_flow_restart
  public  :: measure_streamwise_bulk
  private :: xoutlet_database_shapes
  private :: xoutlet_database_shapes_from_dims
  private :: configure_xinlet_database_mesh
  private :: read_xoutlet_database_bundle_interp
  private :: infer_xoutlet_database_mesh_from_bundle
  private :: read_interp_xinlet_database_bundle_field
  private :: read_checkpoint_restart_metadata
  private :: remove_legacy_restart_metadata
  private :: warn_if_legacy_metadata_conflicts

  ! Latest permitted change of enthalpy_advection at a thermal restart, as
  ! '<from>-><to> <iteration>' (empty if none). Read from the checkpoint record
  ! or set when enthalpy_advection_restart = allow_switch permits a change, and
  ! written into every later thermal record (provenance).
  character(64), save :: enth_adv_switch_note = ''

contains

!==============================================================================
  function bundle_shape(name, dtmp) result(shape)
    implicit none
    character(*), intent(in) :: name
    type(DECOMP_INFO), intent(in) :: dtmp
    character(128) :: shape

    write(shape, '(A,A,I0,A,I0,A,I0)') trim(name), '=', &
      dtmp%xsz(1), ',', dtmp%ysz(2), ',', dtmp%zsz(3)

    return
  end function bundle_shape
!==============================================================================
  function is_restart_history_exact(dm) result(is_exact)
    implicit none
    type(t_domain), intent(in) :: dm
    logical :: is_exact

    is_exact = (dm%restart_history_mode == RESTART_HISTORY_EXACT)

    return
  end function is_restart_history_exact
!==============================================================================
  function flow_restart_fields(dm) result(fields)
    implicit none
    type(t_domain), intent(in) :: dm
    character(1024) :: fields

    if(is_restart_history_exact(dm)) then
      fields = flow_restart_fields_exact(dm)
    else
      fields = flow_restart_fields_compact(dm)
    end if

    return
  end function flow_restart_fields
!==============================================================================
  ! opt_is_thermo overrides dm%is_thermo, so that a thermal run can recognise
  ! a bundle written by an isothermal run (see read_flow_restart_bundle_exact)
  function flow_restart_fields_exact(dm, opt_is_thermo) result(fields)
    implicit none
    type(t_domain), intent(in) :: dm
    logical, intent(in), optional :: opt_is_thermo
    character(1024) :: fields
    logical :: is_thermo

    is_thermo = dm%is_thermo
    if(present(opt_is_thermo)) is_thermo = opt_is_thermo

    if(is_thermo) then
      fields = 'gx:dpcc,gy:dcpc,gz:dccp,qx:dpcc,qy:dcpc,qz:dccp,pr:dccc,'// &
               'mx_rhs0:dpcc,my_rhs0:dcpc,mz_rhs0:dccp'
    else
      fields = 'qx:dpcc,qy:dcpc,qz:dccp,pr:dccc,'// &
               'mx_rhs0:dpcc,my_rhs0:dcpc,mz_rhs0:dccp'
    end if
    if(dm%is_conv_outlet(1)) then
      if(is_thermo) then
        fields = trim(fields)// &
          ',fbcx_gx:d4cc,fbcx_gy:d4pc,fbcx_gz:d4cp'// &
          ',fbcx_qx:d4cc,fbcx_qy:d4pc,fbcx_qz:d4cp'// &
          ',fbcx_a0cc_rhs0:d1cc,fbcx_a0pc_rhs0:d1pc,fbcx_a0cp_rhs0:d1cp'// &
          ',fbcx_ftp_d:d4cc,fbcx_ftp_rhoh:d4cc'
      else
        fields = trim(fields)// &
          ',fbcx_qx:d4cc,fbcx_qy:d4pc,fbcx_qz:d4cp'// &
          ',fbcx_a0cc_rhs0:d1cc,fbcx_a0pc_rhs0:d1pc,fbcx_a0cp_rhs0:d1cp'
      end if
    end if

    return
  end function flow_restart_fields_exact
!==============================================================================
  function flow_restart_fields_compact(dm) result(fields)
    implicit none
    type(t_domain), intent(in) :: dm
    character(1024) :: fields

    ! compact history is isothermal only - rejected for a thermal flow in Read_input_parameters
    fields = 'qx:dpcc,qy:dcpc,qz:dccp,pr:dccc'
    if(dm%is_conv_outlet(1)) &
      fields = trim(fields)//',fbcx_qx:d4cc,fbcx_qy:d4pc,fbcx_qz:d4cp'

    return
  end function flow_restart_fields_compact
!==============================================================================
  function flow_restart_shapes(dm) result(shapes)
    implicit none
    type(t_domain), intent(in) :: dm
    character(2048) :: shapes

    if(is_restart_history_exact(dm)) then
      shapes = flow_restart_shapes_exact(dm)
    else
      shapes = flow_restart_shapes_compact(dm)
    end if

    return
  end function flow_restart_shapes
!==============================================================================
  ! opt_is_thermo: see flow_restart_fields_exact
  function flow_restart_shapes_exact(dm, opt_is_thermo) result(shapes)
    implicit none
    type(t_domain), intent(in) :: dm
    logical, intent(in), optional :: opt_is_thermo
    character(2048) :: shapes
    logical :: is_thermo

    is_thermo = dm%is_thermo
    if(present(opt_is_thermo)) is_thermo = opt_is_thermo

    if(is_thermo) then
      shapes = trim(bundle_shape('gx', dm%dpcc))//';'// &
               trim(bundle_shape('gy', dm%dcpc))//';'// &
               trim(bundle_shape('gz', dm%dccp))//';'// &
               trim(bundle_shape('qx', dm%dpcc))//';'// &
               trim(bundle_shape('qy', dm%dcpc))//';'// &
               trim(bundle_shape('qz', dm%dccp))//';'// &
               trim(bundle_shape('pr', dm%dccc))//';'// &
               trim(bundle_shape('mx_rhs0', dm%dpcc))//';'// &
               trim(bundle_shape('my_rhs0', dm%dcpc))//';'// &
               trim(bundle_shape('mz_rhs0', dm%dccp))
    else
      shapes = trim(bundle_shape('qx', dm%dpcc))//';'// &
               trim(bundle_shape('qy', dm%dcpc))//';'// &
               trim(bundle_shape('qz', dm%dccp))//';'// &
               trim(bundle_shape('pr', dm%dccc))//';'// &
               trim(bundle_shape('mx_rhs0', dm%dpcc))//';'// &
               trim(bundle_shape('my_rhs0', dm%dcpc))//';'// &
               trim(bundle_shape('mz_rhs0', dm%dccp))
    end if
    if(dm%is_conv_outlet(1)) then
      if(is_thermo) then
        shapes = trim(shapes)//';'// &
                 trim(bundle_shape('fbcx_gx', dm%d4cc))//';'// &
                 trim(bundle_shape('fbcx_gy', dm%d4pc))//';'// &
                 trim(bundle_shape('fbcx_gz', dm%d4cp))//';'// &
                 trim(bundle_shape('fbcx_qx', dm%d4cc))//';'// &
                 trim(bundle_shape('fbcx_qy', dm%d4pc))//';'// &
                 trim(bundle_shape('fbcx_qz', dm%d4cp))//';'// &
                 trim(bundle_shape('fbcx_a0cc_rhs0', dm%d1cc))//';'// &
                 trim(bundle_shape('fbcx_a0pc_rhs0', dm%d1pc))//';'// &
                 trim(bundle_shape('fbcx_a0cp_rhs0', dm%d1cp))//';'// &
                 trim(bundle_shape('fbcx_ftp_d', dm%d4cc))//';'// &
                 trim(bundle_shape('fbcx_ftp_rhoh', dm%d4cc))
      else
        shapes = trim(shapes)//';'// &
                 trim(bundle_shape('fbcx_qx', dm%d4cc))//';'// &
                 trim(bundle_shape('fbcx_qy', dm%d4pc))//';'// &
                 trim(bundle_shape('fbcx_qz', dm%d4cp))//';'// &
                 trim(bundle_shape('fbcx_a0cc_rhs0', dm%d1cc))//';'// &
                 trim(bundle_shape('fbcx_a0pc_rhs0', dm%d1pc))//';'// &
                 trim(bundle_shape('fbcx_a0cp_rhs0', dm%d1cp))
      end if
    end if

    return
  end function flow_restart_shapes_exact
!==============================================================================
  function flow_restart_shapes_compact(dm) result(shapes)
    implicit none
    type(t_domain), intent(in) :: dm
    character(2048) :: shapes

    ! compact history is isothermal only - rejected for a thermal flow in Read_input_parameters
    shapes = trim(bundle_shape('qx', dm%dpcc))//';'// &
             trim(bundle_shape('qy', dm%dcpc))//';'// &
             trim(bundle_shape('qz', dm%dccp))//';'// &
             trim(bundle_shape('pr', dm%dccc))
    if(dm%is_conv_outlet(1)) &
      shapes = trim(shapes)//';'// &
               trim(bundle_shape('fbcx_qx', dm%d4cc))//';'// &
               trim(bundle_shape('fbcx_qy', dm%d4pc))//';'// &
               trim(bundle_shape('fbcx_qz', dm%d4cp))

    return
  end function flow_restart_shapes_compact
!==============================================================================
  subroutine ensure_restart_convective_outlet_supported(dm)
    implicit none
    type(t_domain), intent(in) :: dm

    if(dm%is_conv_outlet(2)) call Print_error_msg("Restart with y convective outlet is not supported.")
    if(dm%is_conv_outlet(3)) call Print_error_msg("Restart with z convective outlet is not supported yet.")

    return
  end subroutine ensure_restart_convective_outlet_supported
!==============================================================================
  subroutine write_flow_restart_xoutlet_state(io, fl, dm)
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(in) :: dm

    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_d
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_rhoh
    real(WP), dimension(dm%d1cc%xsz(1), dm%d1cc%xsz(2), dm%d1cc%xsz(3)) :: fbcx_a0cc_rhs0
    real(WP), dimension(dm%d1pc%xsz(1), dm%d1pc%xsz(2), dm%d1pc%xsz(3)) :: fbcx_a0pc_rhs0
    real(WP), dimension(dm%d1cp%xsz(1), dm%d1cp%xsz(2), dm%d1cp%xsz(3)) :: fbcx_a0cp_rhs0
    integer :: n, j, k

    if(.not. dm%is_conv_outlet(1)) return

    fbcx_a0cc_rhs0(1, :, :) = fl%fbcx_a0cc_rhs0(:, :)
    fbcx_a0pc_rhs0(1, :, :) = fl%fbcx_a0pc_rhs0(:, :)
    fbcx_a0cp_rhs0(1, :, :) = fl%fbcx_a0cp_rhs0(:, :)

    if(dm%is_thermo) then
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_gx, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_gy, opt_decomp=dm%d4pc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_gz, opt_decomp=dm%d4cp)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_a0cc_rhs0, opt_decomp=dm%d1cc)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_a0pc_rhs0, opt_decomp=dm%d1pc)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_a0cp_rhs0, opt_decomp=dm%d1cp)
      do k = 1, dm%d4cc%xsz(3)
        do j = 1, dm%d4cc%xsz(2)
          do n = 1, dm%d4cc%xsz(1)
            fbcx_ftp_d(n, j, k) = dm%fbcx_ftp(n, j, k)%d
            fbcx_ftp_rhoh(n, j, k) = dm%fbcx_ftp(n, j, k)%rhoh
          end do
        end do
      end do
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_ftp_d, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_ftp_rhoh, opt_decomp=dm%d4cc)
    else
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_a0cc_rhs0, opt_decomp=dm%d1cc)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_a0pc_rhs0, opt_decomp=dm%d1pc)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_a0cp_rhs0, opt_decomp=dm%d1cp)
    end if

    return
  end subroutine write_flow_restart_xoutlet_state
!==============================================================================
  ! opt_is_thermo: see flow_restart_fields_exact
  subroutine read_flow_restart_xoutlet_state(io, fl, dm, opt_is_thermo)
    use thermo_info_mod, only: ftp_refresh_thermal_properties_from_DH
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    logical, intent(in), optional :: opt_is_thermo

    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_d
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_rhoh
    real(WP), dimension(dm%d1cc%xsz(1), dm%d1cc%xsz(2), dm%d1cc%xsz(3)) :: fbcx_a0cc_rhs0
    real(WP), dimension(dm%d1pc%xsz(1), dm%d1pc%xsz(2), dm%d1pc%xsz(3)) :: fbcx_a0pc_rhs0
    real(WP), dimension(dm%d1cp%xsz(1), dm%d1cp%xsz(2), dm%d1cp%xsz(3)) :: fbcx_a0cp_rhs0
    integer :: n, j, k
    logical :: is_thermo
    character(16) :: fbcx_state, legacy_policy
    logical :: all_rebuilt
    integer :: nlegacy

    if(.not. dm%is_conv_outlet(1)) return
    fbcx_state = read_fbcx_thermal_state_metadata(dm, fl%iterfrom)
    all_rebuilt = .true.
    nlegacy = 0
    legacy_policy = legacy_fbcx_policy()

    is_thermo = dm%is_thermo
    if(present(opt_is_thermo)) is_thermo = opt_is_thermo

    if(is_thermo) then
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_gx, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_gy, opt_decomp=dm%d4pc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_gz, opt_decomp=dm%d4cp)
      fl%is_fbcx_g_restored = .true.
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_a0cc_rhs0, opt_decomp=dm%d1cc)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_a0pc_rhs0, opt_decomp=dm%d1pc)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_a0cp_rhs0, opt_decomp=dm%d1cp)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_ftp_d, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_ftp_rhoh, opt_decomp=dm%d4cc)
      fl%fbcx_a0cc_rhs0(:, :) = fbcx_a0cc_rhs0(1, :, :)
      fl%fbcx_a0pc_rhs0(:, :) = fbcx_a0pc_rhs0(1, :, :)
      fl%fbcx_a0cp_rhs0(:, :) = fbcx_a0cp_rhs0(1, :, :)
      do k = 1, dm%d4cc%xsz(3)
        do j = 1, dm%d4cc%xsz(2)
          do n = 1, dm%d4cc%xsz(1)
            ! Exact Bc restart: restore_thermo_variables_from_restart (which runs first)
            ! has rebuilt the states of a Neumann-T x side (slots 1, 3 or 2, 4) from
            ! (rho*h, rho_hat), as the time loop builds them. Where that rebuild
            ! reproduces the stored (rho, rho*h) bit for bit it is kept: re-deriving
            ! from the stored pair takes h = rho*h/rho instead of rho*h/rho_hat and does
            ! not round-trip, which made heated inlet/outlet continuations inexact.
            ! Elsewhere (e.g. a mesh-mapped iteration-0 state, which the rebuild does
            ! not reproduce) the stored pair is used as before.
            ! The same holds on a Dirichlet-T side: its state was set from the given
            ! temperature, and on the correlation property path (IPROPERTY_FUNCS)
            ! re-deriving it from (rho, rho*h) through the tabulated inverse does not
            ! return that state.
            ! Provenance (fbcx_thermal_state in the metadata): a 'rebuild' checkpoint keeps the
            ! rebuilt state whichever build wrote it - the bit test below fails across builds
            ! because the property evaluation differs in the last bit, and the stored-pair
            ! re-derivation (h = rho*h/rho) then departs from the time-loop state by the
            ! iterative inverse's tolerance; an 'initial' checkpoint uses the stored pair; a
            ! checkpoint without the key keeps the bit test (see check_legacy_fbcx_state).
            ! The rule applies to Neumann-T and Dirichlet-T sides only. A convective thermal outlet
            ! (ibcx_nominal(2, 5) == IBC_CONVECTIVE, reduced to Neumann in ibcx_Tm) stores the
            ! advected rho*h, which the stored-pair branch below keeps and restore_fbcx_outlet_thermo_bc
            ! then uses with fbcx_dhat; the rebuild extrapolates instead, so it is never kept there.
            if(is_bc_recovery .and. fl%is_bc_state_restored .and. &
               .not. ((n == 2 .or. n == 4) .and. dm%ibcx_nominal(2, 5) == IBC_CONVECTIVE) .and. &
               (dm%ibcx_Tm(2 - mod(n, 2)) == IBC_NEUMANN .or. &
                dm%ibcx_Tm(2 - mod(n, 2)) == IBC_DIRICHLET)) then
              if(trim(fbcx_state) == 'rebuild') cycle
              if(len_trim(fbcx_state) == 0) then
                if(is_same_bits(dm%fbcx_ftp(n, j, k)%d, fbcx_ftp_d(n, j, k)) .and. &
                   is_same_bits(dm%fbcx_ftp(n, j, k)%rhoh, fbcx_ftp_rhoh(n, j, k))) cycle
                if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then
                  nlegacy = nlegacy + 1
                  if(trim(legacy_policy) == 'keep') cycle
                end if
              end if
            end if
            if(.not. ((n == 2 .or. n == 4) .and. dm%ibcx_nominal(2, 5) == IBC_CONVECTIVE)) all_rebuilt = .false.
            dm%fbcx_ftp(n, j, k)%d = fbcx_ftp_d(n, j, k)
            dm%fbcx_ftp(n, j, k)%rhoh = fbcx_ftp_rhoh(n, j, k)
            call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(n, j, k))
            ! Bc convective thermal outlet: the stored rhoh of slots 2, 4 is the
            ! advected rho*h (not rho_EOS h); keep it. The outlet state is
            ! recomputed from (rho*h, rho_hat) by restore_fbcx_outlet_thermo_bc.
            if(is_bc_recovery .and. (n == 2 .or. n == 4) .and. &
               dm%ibcx_nominal(2, 5) == IBC_CONVECTIVE) dm%fbcx_ftp(n, j, k)%rhoh = fbcx_ftp_rhoh(n, j, k)
          end do
        end do
      end do
      call check_legacy_fbcx_state(nlegacy, legacy_policy, fl%iterfrom)
      fl%is_fbcx_thermal_rebuilt = is_true_on_all_ranks(all_rebuilt)
    else
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_a0cc_rhs0, opt_decomp=dm%d1cc)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_a0pc_rhs0, opt_decomp=dm%d1pc)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_a0cp_rhs0, opt_decomp=dm%d1cp)
      fl%fbcx_a0cc_rhs0(:, :) = fbcx_a0cc_rhs0(1, :, :)
      fl%fbcx_a0pc_rhs0(:, :) = fbcx_a0pc_rhs0(1, :, :)
      fl%fbcx_a0cp_rhs0(:, :) = fbcx_a0cp_rhs0(1, :, :)
    end if

    return
  end subroutine read_flow_restart_xoutlet_state
!==============================================================================
  !> CHAPSIM_LEGACY_FBCX_STATE, for checkpoints written before the fbcx_thermal_state key existed:
  !> '' (default: stop where the state cannot be verified), 'keep' (keep the x-boundary states rebuilt
  !> from the restored (rho*h, rho_hat), as for a 'rebuild' checkpoint) or 'rederive' (re-derive them
  !> from the stored (rho, rho*h) pair, the behaviour before the key).
  function legacy_fbcx_policy() result(policy)
    implicit none
    character(16) :: policy
    integer :: l, e
    call get_environment_variable('CHAPSIM_LEGACY_FBCX_STATE', policy, l, e)
    if(e /= 0) policy = ''
    if(len_trim(policy) > 0 .and. trim(policy) /= 'keep' .and. trim(policy) /= 'rederive') &
      call Print_error_msg('CHAPSIM_LEGACY_FBCX_STATE must be keep or rederive.')
  end function legacy_fbcx_policy
!==============================================================================
  !> A checkpoint without fbcx_thermal_state whose stored x-boundary thermal state on the correlation
  !> property path is not reproduced bit for bit by the rebuild cannot be classified: if another build
  !> wrote it after an energy step, re-deriving the state moves it by the iterative inverse's tolerance
  !> (a jump of order 1e-8 in rho*h at the outlet); if it is an initial-state
  !> checkpoint, re-deriving is correct. The run stops on every rank unless the user chooses.
  subroutine check_legacy_fbcx_state(nlegacy, policy, iter)
    use mpi_mod
    implicit none
    integer,      intent(in) :: nlegacy, iter
    character(*), intent(in) :: policy
    integer :: ntot
    character(32) :: str
    call mpi_allreduce(nlegacy, ntot, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
    if(ntot == 0) return
    write(str, '(I0,A,I0)') ntot, ' point(s), checkpoint ', iter
    if(len_trim(policy) == 0) call Print_error_msg('Restart from a checkpoint without fbcx_thermal_state: the '// &
      'x-boundary thermal state of a correlation-property fluid is not reproduced ('//trim(str)//'). Set '// &
      'CHAPSIM_LEGACY_FBCX_STATE=keep if the checkpoint was written after an energy step (any time-loop '// &
      'checkpoint), or =rederive for a checkpoint of the initial state.')
    if(nrank == 0) call Print_warning_msg('Checkpoint without fbcx_thermal_state ('//trim(str)// &
      '): CHAPSIM_LEGACY_FBCX_STATE='//trim(policy)//' applied.')
  end subroutine check_legacy_fbcx_state
!==============================================================================
  !> Logical AND over all ranks. The restored provenance must be one value for the run: rank 0 writes
  !> it to the metadata of the next checkpoint, and with the bit test a point may fall back on any rank.
  !> Reduced as an INTEGER, like the other MPI buffers of this file.
  logical function is_true_on_all_ranks(local)
    use mpi_mod
    implicit none
    logical, intent(in) :: local
    integer :: ilocal, iall
    ilocal = merge(1, 0, local)
    call mpi_allreduce(ilocal, iall, 1, MPI_INTEGER, MPI_MIN, MPI_COMM_WORLD, ierror)
    is_true_on_all_ranks = (iall == 1)
  end function is_true_on_all_ranks
!==============================================================================
  !> Name of the metadata file that carries fbcx_thermal_state: the flow-bundle metadata in the bundled
  !> layout, the sidecar of domainN_flow_xoutlet_<iter>.bin in the per-field layout.
  subroutine fbcx_thermal_state_metadata_file(dm, iter, layout, meta_file)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter, layout
    character(*), intent(out) :: meta_file
    if(layout == RESTART_LAYOUT_BUNDLED) then
      call generate_pathfile_name(meta_file, dm%idom, 'flow_restart_meta', dir_data, 'dat', iter)
    else
      call generate_pathfile_name(meta_file, dm%idom, 'flow_xoutlet_meta', dir_data, 'dat', iter)
    end if
  end subroutine fbcx_thermal_state_metadata_file
!==============================================================================
  !> Records where the x-boundary thermal states (fbcx_ftp) of a heated run with a convective outlet
  !> came from: 'rebuild' when the time loop's property update has built them from (rho*h, rho_hat)
  !> (any checkpoint after an energy step, or one restored from such a checkpoint), 'initial' otherwise
  !> (a checkpoint of the initial state). Bundled layout: appended after field_shapes, so readers that
  !> validate the fixed keys are unaffected. Per-field layout: one-line sidecar file.
  subroutine write_fbcx_thermal_state_metadata(fl, dm, layout)
    implicit none
    type(t_flow),   intent(in) :: fl
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: layout
    character(256) :: meta_file
    integer :: u
    if(nrank /= 0) return
    if(.not. (dm%is_thermo .and. dm%is_conv_outlet(1))) return
    call fbcx_thermal_state_metadata_file(dm, fl%iteration, layout, meta_file)
    if(layout == RESTART_LAYOUT_BUNDLED) then
      open(newunit=u, file=trim(meta_file), status='old', position='append', action='write')
    else
      open(newunit=u, file=trim(meta_file), status='replace', action='write')
    end if
    if(fl%is_fbcx_thermal_rebuilt) then
      write(u, '(A)') 'fbcx_thermal_state rebuild'
    else
      write(u, '(A)') 'fbcx_thermal_state initial'
    end if
    close(u)
  end subroutine write_fbcx_thermal_state_metadata
!==============================================================================
  !> fbcx_thermal_state of the checkpoint of iteration iter in the read layout: 'rebuild', 'initial',
  !> or '' (key or sidecar absent: a checkpoint written before the key existed).
  function read_fbcx_thermal_state_metadata(dm, iter) result(state)
    use mpi_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    character(16) :: state
    character(256) :: meta_file
    character(1024) :: line
    integer :: u, ioerr, code
    state = ''
    call fbcx_thermal_state_metadata_file(dm, iter, dm%restart_data_layout_read, meta_file)
    if(nrank == 0) then
      open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
      if(ioerr == 0) then
        do
          read(u, '(A)', iostat=ioerr) line
          if(ioerr /= 0) exit
          line = adjustl(line)
          if(line(1:19) == 'fbcx_thermal_state ') then
            state = adjustl(line(20:))
            exit
          end if
        end do
        close(u)
      end if
    end if
    ! one value on every rank (the per-point decision of the reader must not depend on the rank),
    ! broadcast as an INTEGER code: 0 absent, 1 rebuild, 2 initial, -1 invalid
    code = 0
    if(trim(state) == 'rebuild') then
      code = 1
    else if(trim(state) == 'initial') then
      code = 2
    else if(len_trim(state) > 0) then
      code = -1
    end if
    call mpi_bcast(code, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
    if(code == -1) call Print_error_msg('Invalid fbcx_thermal_state in '//trim(meta_file))
    state = ''
    if(code == 1) state = 'rebuild'
    if(code == 2) state = 'initial'
  end function read_fbcx_thermal_state_metadata
!==============================================================================
  !> True when a and b have the same bit pattern (the restart exactness test;
  !> unlike a == b it separates +0 and -0 and needs no real equality comparison).
  pure logical function is_same_bits(a, b)
    use precision_mod, only : I15
    implicit none
    real(WP), intent(in) :: a, b
    is_same_bits = (transfer(a, 1_I15) == transfer(b, 1_I15))
  end function is_same_bits
!==============================================================================
  !> Per-field restart layout: the convective x-outlet state (planes, their RHS
  !> histories and, for a thermal run, the stored boundary (rho, rho*h)) is written
  !> as one extra file domain1_flow_xoutlet_<iter>.bin, in exactly the form and by
  !> the same routines as the tail of the flow bundle, so both layouts restore the
  !> same outlet state.
  subroutine write_flow_xoutlet_state_per_field(fl, dm)
    implicit none
    type(t_flow),   intent(in) :: fl
    type(t_domain), intent(in) :: dm
    type(d2d_io_mpi) :: io
    character(256) :: output_files(1)
    logical :: do_write

    if(.not. dm%is_conv_outlet(1)) return
    call generate_pathfile_name(output_files(1), dm%idom, 'flow_xoutlet', dir_data, 'bin', fl%iteration)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'flow outlet state', do_write)
    if(.not. do_write) return
    call io%open(trim(output_files(1)), decomp_2d_write_mode)
    if(is_restart_history_exact(dm)) then
      call write_flow_restart_xoutlet_state(io, fl, dm)
    else
      call write_flow_restart_xoutlet_state_compact(io, fl, dm)
    end if
    call io%close()
    call write_fbcx_thermal_state_metadata(fl, dm, RESTART_LAYOUT_PER_FIELD)
    return
  end subroutine write_flow_xoutlet_state_per_field
!==============================================================================
  !> Per-field restart layout: read the convective x-outlet state written by
  !> write_flow_xoutlet_state_per_field. A per-field checkpoint written before that
  !> file existed is read as before (outlet from its initial state), with a warning.
  subroutine read_flow_xoutlet_state_per_field(fl, dm)
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    type(d2d_io_mpi) :: io
    character(256) :: data_file

    if(.not. dm%is_conv_outlet(1)) return
    call generate_pathfile_name(data_file, dm%idom, 'flow_xoutlet', dir_data, 'bin', fl%iterfrom)
    if(.not. file_exists(trim(data_file))) then
      if(nrank == 0) call Print_warning_msg('Per-field flow restart without the convective-outlet '// &
        'state ('//trim(data_file)//'): the outlet restarts from its initial state; the continuation is NOT exact.')
      return
    end if
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(data_file))
    call io%open(trim(data_file), decomp_2d_read_mode)
    if(is_restart_history_exact(dm)) then
      call read_flow_restart_xoutlet_state(io, fl, dm)
    else
      call read_flow_restart_xoutlet_state_compact(io, fl, dm)
    end if
    call io%close()
    return
  end subroutine read_flow_xoutlet_state_per_field
!==============================================================================
  subroutine write_flow_restart_xoutlet_state_compact(io, fl, dm)
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(in) :: dm

    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_d
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_rhoh
    integer :: n, j, k

    if(.not. dm%is_conv_outlet(1)) return

    if(dm%is_thermo) then
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_gx, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_gy, opt_decomp=dm%d4pc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_gz, opt_decomp=dm%d4cp)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
      do k = 1, dm%d4cc%xsz(3)
        do j = 1, dm%d4cc%xsz(2)
          do n = 1, dm%d4cc%xsz(1)
            fbcx_ftp_d(n, j, k) = dm%fbcx_ftp(n, j, k)%d
            fbcx_ftp_rhoh(n, j, k) = dm%fbcx_ftp(n, j, k)%rhoh
          end do
        end do
      end do
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_ftp_d, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_ftp_rhoh, opt_decomp=dm%d4cc)
    else
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
    end if

    return
  end subroutine write_flow_restart_xoutlet_state_compact
!==============================================================================
  ! opt_is_thermo: see flow_restart_fields_exact
  subroutine read_flow_restart_xoutlet_state_compact(io, fl, dm, opt_is_thermo)
    use thermo_info_mod, only: ftp_refresh_thermal_properties_from_DH
    implicit none
    type(d2d_io_mpi), intent(inout) :: io
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    logical, intent(in), optional :: opt_is_thermo

    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_d
    real(WP), dimension(dm%d4cc%xsz(1), dm%d4cc%xsz(2), dm%d4cc%xsz(3)) :: fbcx_ftp_rhoh
    integer :: n, j, k
    logical :: is_thermo
    character(16) :: fbcx_state, legacy_policy
    logical :: all_rebuilt
    integer :: nlegacy

    if(.not. dm%is_conv_outlet(1)) return
    fbcx_state = read_fbcx_thermal_state_metadata(dm, fl%iterfrom)
    all_rebuilt = .true.
    nlegacy = 0
    legacy_policy = legacy_fbcx_policy()

    is_thermo = dm%is_thermo
    if(present(opt_is_thermo)) is_thermo = opt_is_thermo

    if(is_thermo) then
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_gx, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_gy, opt_decomp=dm%d4pc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_gz, opt_decomp=dm%d4cp)
      fl%is_fbcx_g_restored = .true.
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_ftp_d, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_ftp_rhoh, opt_decomp=dm%d4cc)
      do k = 1, dm%d4cc%xsz(3)
        do j = 1, dm%d4cc%xsz(2)
          do n = 1, dm%d4cc%xsz(1)
            ! Exact Bc restart: restore_thermo_variables_from_restart (which runs first)
            ! has rebuilt the states of a Neumann-T x side (slots 1, 3 or 2, 4) from
            ! (rho*h, rho_hat), as the time loop builds them. Where that rebuild
            ! reproduces the stored (rho, rho*h) bit for bit it is kept: re-deriving
            ! from the stored pair takes h = rho*h/rho instead of rho*h/rho_hat and does
            ! not round-trip, which made heated inlet/outlet continuations inexact.
            ! Elsewhere (e.g. a mesh-mapped iteration-0 state, which the rebuild does
            ! not reproduce) the stored pair is used as before.
            ! The same holds on a Dirichlet-T side: its state was set from the given
            ! temperature, and on the correlation property path (IPROPERTY_FUNCS)
            ! re-deriving it from (rho, rho*h) through the tabulated inverse does not
            ! return that state. Provenance and legacy checkpoints: as in
            ! read_flow_restart_xoutlet_state.
            ! The rule applies to Neumann-T and Dirichlet-T sides only. A convective thermal outlet
            ! (ibcx_nominal(2, 5) == IBC_CONVECTIVE, reduced to Neumann in ibcx_Tm) stores the
            ! advected rho*h, which the stored-pair branch below keeps and restore_fbcx_outlet_thermo_bc
            ! then uses with fbcx_dhat; the rebuild extrapolates instead, so it is never kept there.
            if(is_bc_recovery .and. fl%is_bc_state_restored .and. &
               .not. ((n == 2 .or. n == 4) .and. dm%ibcx_nominal(2, 5) == IBC_CONVECTIVE) .and. &
               (dm%ibcx_Tm(2 - mod(n, 2)) == IBC_NEUMANN .or. &
                dm%ibcx_Tm(2 - mod(n, 2)) == IBC_DIRICHLET)) then
              if(trim(fbcx_state) == 'rebuild') cycle
              if(len_trim(fbcx_state) == 0) then
                if(is_same_bits(dm%fbcx_ftp(n, j, k)%d, fbcx_ftp_d(n, j, k)) .and. &
                   is_same_bits(dm%fbcx_ftp(n, j, k)%rhoh, fbcx_ftp_rhoh(n, j, k))) cycle
                if(fluidparam%ipropertyState == IPROPERTY_FUNCS) then
                  nlegacy = nlegacy + 1
                  if(trim(legacy_policy) == 'keep') cycle
                end if
              end if
            end if
            if(.not. ((n == 2 .or. n == 4) .and. dm%ibcx_nominal(2, 5) == IBC_CONVECTIVE)) all_rebuilt = .false.
            dm%fbcx_ftp(n, j, k)%d = fbcx_ftp_d(n, j, k)
            dm%fbcx_ftp(n, j, k)%rhoh = fbcx_ftp_rhoh(n, j, k)
            call ftp_refresh_thermal_properties_from_DH(dm%fbcx_ftp(n, j, k))
            ! Bc convective thermal outlet: the stored rhoh of slots 2, 4 is the
            ! advected rho*h (not rho_EOS h); keep it. The outlet state is
            ! recomputed from (rho*h, rho_hat) by restore_fbcx_outlet_thermo_bc.
            if(is_bc_recovery .and. (n == 2 .or. n == 4) .and. &
               dm%ibcx_nominal(2, 5) == IBC_CONVECTIVE) dm%fbcx_ftp(n, j, k)%rhoh = fbcx_ftp_rhoh(n, j, k)
          end do
        end do
      end do
      call check_legacy_fbcx_state(nlegacy, legacy_policy, fl%iterfrom)
      fl%is_fbcx_thermal_rebuilt = is_true_on_all_ranks(all_rebuilt)
    else
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qx, opt_decomp=dm%d4cc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qy, opt_decomp=dm%d4pc)
      call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qz, opt_decomp=dm%d4cp)
    end if
    fl%fbcx_a0cc_rhs0 = ZERO
    fl%fbcx_a0pc_rhs0 = ZERO
    fl%fbcx_a0cp_rhs0 = ZERO

    return
  end subroutine read_flow_restart_xoutlet_state_compact
!==============================================================================
  ! A thermal run always stores the full history: compact history is rejected
  ! for is_thermo at input time (Read_input_parameters), so there
  ! is a single thermo restart field set.
  function thermo_restart_fields(dm) result(fields)
    implicit none
    type(t_domain), intent(in) :: dm
    character(1024) :: fields

    fields = thermo_restart_fields_legacy(dm)
    ! Transported-density recovery (Bc) needs rho_hat and the physical rho_t of
    ! the last stage of the previous step (it multiplies gamma_1 at stage 1 and
    ! zeta_2 at stage 2; the rho_t of the stage before multiplies zeta_1 = 0).
    if(is_bc_recovery) fields = trim(fields)//trim(thermo_restart_fields_bc_extra(dm))

    return
  end function thermo_restart_fields
!==============================================================================
  ! Extra thermal restart fields of rhoh_recovery = transported (Bc): rho_hat,
  ! the physical rho_t of the previous step's last stage and, with a convective
  ! x-outlet, the outlet rho_hat face/ghost states and their RK history.
  function thermo_restart_fields_bc_extra(dm) result(fields)
    implicit none
    type(t_domain), intent(in) :: dm
    character(256) :: fields

    fields = ',dens_hat:dccc,drhodt_m1:dccc'
    if(dm%is_conv_outlet(1)) fields = trim(fields)//',fbcx_dhat:d4cc,fbcx_dhat_rhs0:d1cc'

    return
  end function thermo_restart_fields_bc_extra
!==============================================================================
  ! Thermal restart field set as written up to 8d47f18 (and by lagged_legacy).
  function thermo_restart_fields_legacy(dm) result(fields)
    implicit none
    type(t_domain), intent(in) :: dm
    character(1024) :: fields

    fields = 'rhoh:dccc,temp:dccc,ene_rhs0:dccc,dens:dccc,visc:dccc,henth:dccc,kcond:dccc,econd:dccc'
    if(dm%is_conv_outlet(1)) fields = trim(fields)//',fbcx_rhoh_rhs0:d1cc'

    return
  end function thermo_restart_fields_legacy
!==============================================================================
  function thermo_restart_shapes(dm, opt_bc_fields) result(shapes)
    implicit none
    type(t_domain), intent(in) :: dm
    logical, intent(in), optional :: opt_bc_fields
    character(1024) :: shapes

    shapes = trim(bundle_shape('rhoh', dm%dccc))//';'// &
             trim(bundle_shape('temp', dm%dccc))//';'// &
             trim(bundle_shape('ene_rhs0', dm%dccc))//';'// &
             trim(bundle_shape('dens', dm%dccc))//';'// &
             trim(bundle_shape('visc', dm%dccc))//';'// &
             trim(bundle_shape('henth', dm%dccc))//';'// &
             trim(bundle_shape('kcond', dm%dccc))//';'// &
             trim(bundle_shape('econd', dm%dccc))
    if(dm%is_conv_outlet(1)) shapes = trim(shapes)//';'//trim(bundle_shape('fbcx_rhoh_rhs0', dm%d1cc))
    if(present(opt_bc_fields)) then
      if(opt_bc_fields) then
        shapes = trim(shapes)//';'//trim(bundle_shape('dens_hat', dm%dccc))// &
                 ';'//trim(bundle_shape('drhodt_m1', dm%dccc))
        if(dm%is_conv_outlet(1)) shapes = trim(shapes)//';'//trim(bundle_shape('fbcx_dhat', dm%d4cc))// &
                 ';'//trim(bundle_shape('fbcx_dhat_rhs0', dm%d1cc))
      end if
    end if

    return
  end function thermo_restart_shapes
!==============================================================================
  function xoutlet_database_shapes(dm) result(shapes)
    implicit none
    type(t_domain), intent(in) :: dm
    character(512) :: shapes

    shapes = xoutlet_database_shapes_from_dims(dm%ndbbuf, dm%nc(2), dm%nc(3), dm%np(2), dm%np(3))

    return
  end function xoutlet_database_shapes
!==============================================================================
  function xoutlet_database_shapes_from_dims(ndbbuf, nc2, nc3, np2, np3) result(shapes)
    implicit none
    integer, intent(in) :: ndbbuf, nc2, nc3, np2, np3
    character(512) :: shapes

    write(shapes, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)') &
      'outlet1_qx=', ndbbuf, ',', nc2, ',', nc3, &
      ';outlet2_qx=', ndbbuf, ',', nc2, ',', nc3, &
      ';outlet1_qy=', ndbbuf, ',', np2, ',', nc3, &
      ';outlet2_qy=', ndbbuf, ',', np2, ',', nc3, &
      ';outlet1_qz=', ndbbuf, ',', nc2, ',', np3, &
      ';outlet2_qz=', ndbbuf, ',', nc2, ',', np3

    return
  end function xoutlet_database_shapes_from_dims
!==============================================================================
  subroutine write_bundle_metadata(dm, group_name, iter, fields, field_shapes, opt_time, opt_dt, &
                                   opt_iter_start, opt_iter_end, opt_time_end)
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    character(*), intent(in) :: fields
    character(*), intent(in) :: field_shapes
    integer, intent(in) :: iter
    real(WP), intent(in), optional :: opt_time
    real(WP), intent(in), optional :: opt_dt
    integer, intent(in), optional :: opt_iter_start
    integer, intent(in), optional :: opt_iter_end
    real(WP), intent(in), optional :: opt_time_end

    character(256) :: meta_file
    integer :: u
    integer :: precision_bytes
    integer :: iter_start_meta, iter_end_meta
    real(WP) :: time_meta, time_end_meta, dt_meta

    if(nrank /= 0) return

    iter_start_meta = iter
    iter_end_meta = iter
    time_meta = ZERO
    time_end_meta = ZERO
    dt_meta = dm%dt
    if(present(opt_iter_start)) iter_start_meta = opt_iter_start
    if(present(opt_iter_end)) iter_end_meta = opt_iter_end
    if(present(opt_time)) time_meta = opt_time
    time_end_meta = time_meta
    if(present(opt_time_end)) time_end_meta = opt_time_end
    if(present(opt_dt)) dt_meta = opt_dt
    precision_bytes = storage_size(time_meta) / 8

    call generate_pathfile_name(meta_file, dm%idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
    open(newunit=u, file=trim(meta_file), status='replace', action='write')
    write(u, '(A)') 'CHAPSim_bundle_v2'
    write(u, '(A,1X,A)') 'group', trim(group_name)
    write(u, '(A,1X,I0)') 'iter', iter
    write(u, '(A,1X,I0)') 'iter_start', iter_start_meta
    write(u, '(A,1X,I0)') 'iter_end', iter_end_meta
    write(u, '(A,1X,ES24.16E3)') 'time', time_meta
    write(u, '(A,1X,ES24.16E3)') 'time_end', time_end_meta
    write(u, '(A,1X,ES24.16E3)') 'dt', dt_meta
    write(u, '(A,1X,I0)') 'idom', dm%idom
    write(u, '(A,1X,I0)') 'icase', dm%icase
    write(u, '(A,1X,I0)') 'coordinate', dm%icoordinate
    write(u, '(A)') 'layout bundled'
    write(u, '(A,1X,I0)') 'precision_bytes', precision_bytes
    write(u, '(A)') 'endianness native'
    write(u, '(A,3(1X,I0))') 'nc', dm%nc(1), dm%nc(2), dm%nc(3)
    write(u, '(A,3(1X,I0))') 'np', dm%np(1), dm%np(2), dm%np(3)
    write(u, '(A,4(1X,ES24.16E3))') 'length', dm%lxx, dm%lyb, dm%lyt, dm%lzz
    write(u, '(A,2(1X,I0),1X,ES24.16E3)') 'stretch', dm%istret, dm%mstret, dm%rstret
    write(u, '(A,3(1X,L1))') 'periodic', dm%is_periodic(1), dm%is_periodic(2), dm%is_periodic(3)
    write(u, '(A,1X,I0)') 'ndbbuf', dm%ndbbuf
    write(u, '(A,1X,A)') 'fields', trim(fields)
    write(u, '(A,1X,A)') 'field_shapes', trim(field_shapes)
    close(u)

    return
  end subroutine write_bundle_metadata
!==============================================================================
  subroutine read_metadata_line(u, meta_file, expected_key, value)
    implicit none
    integer, intent(in) :: u
    character(*), intent(in) :: meta_file
    character(*), intent(in) :: expected_key
    character(*), intent(out) :: value

    character(1024) :: line, key
    integer :: ioerr, p

    read(u, '(A)', iostat=ioerr) line
    if(ioerr /= 0) call Print_error_msg("Missing metadata key "//trim(expected_key)//" in "//trim(meta_file))

    p = index(trim(line), ' ')
    if(p <= 0) call Print_error_msg("Malformed metadata line in "//trim(meta_file)//": "//trim(line))
    key = adjustl(line(1:p-1))
    value = adjustl(line(p+1:))
    if(trim(key) /= trim(expected_key)) &
    call Print_error_msg("Expected metadata key "//trim(expected_key)//" in "//trim(meta_file)//", got "//trim(key))

    return
  end subroutine read_metadata_line
!==============================================================================
  function metadata_value(meta_file, key_name) result(value)
    implicit none
    character(*), intent(in) :: meta_file
    character(*), intent(in) :: key_name
    character(1024) :: value

    character(1024) :: line, key
    integer :: u, ioerr, p

    value = ''
    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    if(ioerr /= 0) call Print_error_msg("Failed to open bundle metadata file "//trim(meta_file))

    do
      read(u, '(A)', iostat=ioerr) line
      if(ioerr /= 0) exit
      p = index(trim(line), ' ')
      if(p <= 0) cycle
      key = adjustl(line(1:p-1))
      if(trim(key) == trim(key_name)) then
        value = adjustl(line(p+1:))
        close(u)
        return
      end if
    end do
    close(u)

    call Print_error_msg("Metadata key "//trim(key_name)//" not found in "//trim(meta_file))
    return
  end function metadata_value
!==============================================================================
  subroutine parse_metadata_int3(value, vals, key_name, meta_file)
    implicit none
    character(*), intent(in) :: value
    integer, intent(out) :: vals(3)
    character(*), intent(in) :: key_name
    character(*), intent(in) :: meta_file

    integer :: ioerr

    read(value, *, iostat=ioerr) vals(1), vals(2), vals(3)
    if(ioerr /= 0) call Print_error_msg("Invalid integer triplet metadata key "//trim(key_name)// &
      " in "//trim(meta_file))

    return
  end subroutine parse_metadata_int3
!==============================================================================
  subroutine parse_metadata_int(value, val, key_name, meta_file)
    implicit none
    character(*), intent(in) :: value
    integer, intent(out) :: val
    character(*), intent(in) :: key_name
    character(*), intent(in) :: meta_file

    integer :: ioerr

    read(value, *, iostat=ioerr) val
    if(ioerr /= 0) call Print_error_msg("Invalid integer metadata key "//trim(key_name)// &
      " in "//trim(meta_file))

    return
  end subroutine parse_metadata_int
!==============================================================================
  subroutine validate_bundle_metadata(dm, group_name, iter, fields, field_shapes)
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    character(*), intent(in) :: fields
    character(*), intent(in) :: field_shapes
    integer, intent(in) :: iter

    character(256) :: meta_file
    character(1024) :: line, value
    integer :: u, ioerr, iter_read, precision_read
    integer :: nc_read(3), np_read(3)
    integer :: int_read, istret_read, mstret_read
    real(WP) :: length_read(4), rstret_read
    logical :: periodic_read(3)

    call generate_pathfile_name(meta_file, dm%idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
    if(.not. file_exists(trim(meta_file))) &
    call Print_error_msg("The bundle metadata file "//trim(meta_file)//" does not exist.")

    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    if(ioerr /= 0) call Print_error_msg("Failed to open bundle metadata file "//trim(meta_file))

    read(u, '(A)', iostat=ioerr) line
    if(ioerr /= 0 .or. trim(line) /= 'CHAPSim_bundle_v2') &
    call Print_error_msg("Unsupported bundle metadata format in "//trim(meta_file))

    call read_metadata_line(u, meta_file, 'group', value)
    if(trim(value) /= trim(group_name)) &
    call Print_error_msg("Bundle group mismatch in "//trim(meta_file))

    call read_metadata_line(u, meta_file, 'iter', value)
    call parse_metadata_int(value, iter_read, 'iter', meta_file)
    if(iter_read /= iter) &
    call Print_error_msg("Bundle iteration mismatch in "//trim(meta_file))

    call read_metadata_line(u, meta_file, 'iter_start', value)
    call read_metadata_line(u, meta_file, 'iter_end', value)
    call read_metadata_line(u, meta_file, 'time', value)
    call read_metadata_line(u, meta_file, 'time_end', value)
    call read_metadata_line(u, meta_file, 'dt', value)
    call read_metadata_line(u, meta_file, 'idom', value)
    call read_metadata_line(u, meta_file, 'icase', value)
    call parse_metadata_int(value, int_read, 'icase', meta_file)
    if(int_read /= dm%icase) call Print_error_msg("Bundle case (icase) mismatch in "//trim(meta_file))
    call read_metadata_line(u, meta_file, 'coordinate', value)
    call parse_metadata_int(value, int_read, 'coordinate', meta_file)
    if(int_read /= dm%icoordinate) call Print_error_msg("Bundle coordinate-system mismatch in "//trim(meta_file))
    call read_metadata_line(u, meta_file, 'layout', value)
    if(trim(value) /= 'bundled') call Print_error_msg("Bundle layout mismatch in "//trim(meta_file))
    call read_metadata_line(u, meta_file, 'precision_bytes', value)
    ! the writer records storage_size(real(WP)); another value means another build
    ! kind, whose arrays this reader would misinterpret
    call parse_metadata_int(value, precision_read, 'precision_bytes', meta_file)
    if(precision_read /= storage_size(1.0_WP) / 8) &
    call Print_error_msg("Bundle precision mismatch in "//trim(meta_file))
    call read_metadata_line(u, meta_file, 'endianness', value)
    call read_metadata_line(u, meta_file, 'nc', value)
    call parse_metadata_int3(value, nc_read, 'nc', meta_file)
    if(any(nc_read /= dm%nc)) call Print_error_msg("Bundle mesh cell-count mismatch in "//trim(meta_file))
    call read_metadata_line(u, meta_file, 'np', value)
    call parse_metadata_int3(value, np_read, 'np', meta_file)
    if(any(np_read /= dm%np)) call Print_error_msg("Bundle mesh point-count mismatch in "//trim(meta_file))
    !--------------------------------------------------------------------------
    ! Geometry. The arrays carry no coordinates, so a bundle written on a mesh with
    ! the same cell counts but another length, stretching or periodicity would be
    ! read as if it belonged to this mesh. The writer stores the reals with
    ! ES24.16 (an exact round trip); the 1e-12 relative margin only absorbs
    ! hand-edited or reformatted files. rstret is compared only for a stretched y.
    !--------------------------------------------------------------------------
    call read_metadata_line(u, meta_file, 'length', value)
    read(value, *, iostat=ioerr) length_read(1:4)
    if(ioerr /= 0) call Print_error_msg("Invalid length entry in "//trim(meta_file))
    if(.not. (same_real(length_read(1), dm%lxx) .and. same_real(length_read(2), dm%lyb) .and. &
              same_real(length_read(3), dm%lyt) .and. same_real(length_read(4), dm%lzz))) then
      if(nrank == 0) write(*, '(A,4ES24.16E3)') ' bundle length lxx, lyb, lyt, lzz:', length_read(1:4)
      if(nrank == 0) write(*, '(A,4ES24.16E3)') ' input  length lxx, lyb, lyt, lzz:', dm%lxx, dm%lyb, dm%lyt, dm%lzz
      call Print_error_msg("Bundle domain-length mismatch in "//trim(meta_file))
    end if
    call read_metadata_line(u, meta_file, 'stretch', value)
    read(value, *, iostat=ioerr) istret_read, mstret_read, rstret_read
    if(ioerr /= 0) call Print_error_msg("Invalid stretch entry in "//trim(meta_file))
    if(istret_read /= dm%istret) then
      call Print_error_msg("Bundle mesh-stretching mismatch (istret) in "//trim(meta_file))
    else if(dm%istret /= ISTRET_NO) then
      if(mstret_read /= dm%mstret .or. .not. same_real(rstret_read, dm%rstret)) &
      call Print_error_msg("Bundle mesh-stretching mismatch (mstret/rstret) in "//trim(meta_file))
    end if
    call read_metadata_line(u, meta_file, 'periodic', value)
    read(value, *, iostat=ioerr) periodic_read(1:3)
    if(ioerr /= 0) call Print_error_msg("Invalid periodic entry in "//trim(meta_file))
    if(any(periodic_read(1:3) .neqv. dm%is_periodic(1:3))) &
    call Print_error_msg("Bundle periodicity mismatch in "//trim(meta_file))
    call read_metadata_line(u, meta_file, 'ndbbuf', value)
    call read_metadata_line(u, meta_file, 'fields', value)
    if(trim(value) /= trim(fields)) then
      write(*, '(A)') "Bundle metadata fields: "//trim(value)
      write(*, '(A)') "Expected fields: "//trim(fields)
      call Print_error_msg("Bundle field list mismatch in "//trim(meta_file))
    end if
    call read_metadata_line(u, meta_file, 'field_shapes', value)
    if(trim(value) /= trim(field_shapes)) then
      write(*, '(A)') "Bundle metadata field shapes: "//trim(value)
      write(*, '(A)') "Expected field shapes: "//trim(field_shapes)
      call Print_error_msg("Bundle field-shape mismatch in "//trim(meta_file))
    end if

    close(u)

    call check_bundle_file_size(dm, group_name, iter, field_shapes, meta_file)

    return
  contains
    logical function same_real(a, b)
      real(WP), intent(in) :: a, b
      same_real = abs(a - b) <= 1.0e-12_WP * max(abs(a), abs(b), 1.0_WP)
    end function same_real
  end subroutine validate_bundle_metadata
!==============================================================================
  !> Stop unless the .bin of a bundle holds exactly the arrays its validated
  !> metadata lists. The readers take each array at the offset that follows from
  !> field_shapes, and an MPI-IO read past the end of the file is not an error, so
  !> a truncated, padded or differently laid-out file would otherwise be read
  !> without notice (read_one_3d_array makes the same check for a per-field file).
  !> Rank 0 measures the file and broadcasts the size, so that every rank reaches
  !> the same verdict and stops together. Collective: call on all ranks.
  !> - field_shapes (in): 'name=n1,n2,n3;...' as the reader expects it, already
  !>   compared with the metadata by validate_bundle_metadata.
  subroutine check_bundle_file_size(dm, group_name, iter, field_shapes, meta_file)
    use iso_fortran_env, only: int64, real32
    use decomp_2d_constants, only: DEFAULT_OPT_REDUCE_PREC
    use mpi_mod
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: group_name
    integer,      intent(in) :: iter
    character(*), intent(in) :: field_shapes
    character(*), intent(in) :: meta_file

    character(256) :: bin_file
    character(1024) :: msg
    integer(int64) :: expected_bytes, file_bytes, value_bytes
    integer :: n(3), p0, p1, peq, narrays

    ! bytes per stored value, as decomp_2d_write_var/read_var store it: real(WP),
    ! or real32 if 2decomp was built with single-precision output (SAVE_SINGLE)
    value_bytes = int(storage_size(1.0_WP) / 8, int64)
    if(DEFAULT_OPT_REDUCE_PREC) value_bytes = int(storage_size(1.0_real32) / 8, int64)
    expected_bytes = 0_int64
    narrays = 0
    p0 = 1
    do while(p0 <= len_trim(field_shapes))
      p1 = index(field_shapes(p0:), ';')
      if(p1 == 0) then
        p1 = len_trim(field_shapes) + 1
      else
        p1 = p0 + p1 - 1
      end if
      peq = index(field_shapes(p0:p1-1), '=')
      if(peq == 0) call Print_error_msg("Malformed bundle field shape "//field_shapes(p0:p1-1))
      call parse_metadata_int3(field_shapes(p0+peq:p1-1), n, 'field_shapes', meta_file)
      expected_bytes = expected_bytes + int(n(1), int64) * int(n(2), int64) * int(n(3), int64) * value_bytes
      narrays = narrays + 1
      p0 = p1 + 1
    end do

    call generate_pathfile_name(bin_file, dm%idom, trim(group_name), dir_data, 'bin', iter)
    file_bytes = -1_int64
    if(nrank == 0) then
      if(file_exists(trim(bin_file))) inquire(file=trim(bin_file), size=file_bytes)
    end if
    call mpi_bcast(file_bytes, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierror)

    if(file_bytes < 0_int64) &
    call Print_error_msg("The bundle file "//trim(bin_file)//" does not exist or its size cannot be read.")
    if(file_bytes /= expected_bytes) then
      write(msg, '(A,A,I0,A,I0,A,I0,A,A)') trim(bin_file), ' has ', file_bytes, ' bytes, expected ', &
        expected_bytes, ' for the ', narrays, ' arrays listed in ', trim(meta_file)
      if(file_bytes < expected_bytes) then
        call Print_error_msg("Bundle size mismatch (file too short): "//trim(msg))
      else
        call Print_error_msg("Bundle size mismatch (file too long): "//trim(msg))
      end if
    end if

    return
  end subroutine check_bundle_file_size
!==============================================================================
  subroutine warn_if_legacy_metadata_conflicts(idom, keyword, iter, time, dt)
    implicit none
    integer,      intent(in) :: idom
    integer,      intent(in) :: iter
    character(*), intent(in) :: keyword
    real(WP),     intent(in) :: time
    real(WP),     intent(in) :: dt

    real(WP) :: legacy_time
    real(WP) :: legacy_dt
    logical  :: has_legacy

    call read_restart_metadata(idom, keyword, iter, legacy_time, legacy_dt, has_legacy)
    if(.not. has_legacy) return
    if(abs(legacy_time - time) <= max(MINP, MINP * abs(time)) .and. &
       abs(legacy_dt - dt) <= max(MINP, MINP * abs(dt))) return

    if(nrank == 0) call Print_warning_msg("Checkpoint manifest scalar metadata differs from "// &
      "legacy "//trim(keyword)//"; using checkpoint manifest time and dt.")

    return
  end subroutine warn_if_legacy_metadata_conflicts
!==============================================================================
  subroutine read_checkpoint_restart_metadata(idom, keyword, iter, time, dt, found)
    implicit none
    integer,      intent(in)  :: idom
    integer,      intent(in)  :: iter
    character(*), intent(in)  :: keyword
    real(WP),     intent(out) :: time
    real(WP),     intent(out) :: dt
    logical,      intent(out) :: found

    call read_checkpoint_metadata(idom, iter, time, dt, found)
    if(found) then
      call warn_if_legacy_metadata_conflicts(idom, keyword, iter, time, dt)
      return
    end if

    call read_restart_metadata(idom, keyword, iter, time, dt, found)
    if(found) return

    !--------------------------------------------------------------------------
    ! Bundled layout without checkpoint_meta (e.g. only the bundle pair was
    ! copied): the bundle manifest records the same time and dt (ES24.16), so
    ! take them from there rather than estimating time = iterfrom * dt, which is
    ! wrong whenever dt has changed during the run.
    !--------------------------------------------------------------------------
    block
      character(256) :: bundle_meta
      character(1024) :: value
      character(32) :: group_name
      integer :: ios_t, ios_dt
      group_name = ''
      if(trim(keyword) == 'flow_meta')   group_name = 'flow_restart'
      if(trim(keyword) == 'thermo_meta') group_name = 'thermo_restart'
      if(len_trim(group_name) == 0) return
      call generate_pathfile_name(bundle_meta, idom, trim(group_name)//'_meta', dir_data, 'dat', iter)
      if(.not. file_exists(trim(bundle_meta))) return
      value = metadata_value(bundle_meta, 'time')
      read(value, *, iostat=ios_t) time
      value = metadata_value(bundle_meta, 'dt')
      read(value, *, iostat=ios_dt) dt
      found = (ios_t == 0 .and. ios_dt == 0)
      if(found .and. nrank == 0) call Print_note_msg('Restart time and dt taken from the bundle manifest '// &
        trim(bundle_meta)//' (no checkpoint_meta file).')
      if(.not. found) then
        time = ZERO
        dt = ZERO
      end if
    end block

    return
  end subroutine read_checkpoint_restart_metadata
!==============================================================================
  subroutine remove_legacy_restart_metadata(idom, keyword, iter, existing_output_policy)
    implicit none
    integer,      intent(in) :: idom
    integer,      intent(in) :: iter
    character(*), intent(in) :: keyword
    integer,      intent(in) :: existing_output_policy

    character(256) :: legacy_file

    call generate_pathfile_name(legacy_file, idom, trim(keyword), dir_data, 'dat', iter)
    call remove_output_file_if_overwrite(legacy_file, existing_output_policy)

    return
  end subroutine remove_legacy_restart_metadata
!==============================================================================
!==============================================================================
  !> Write instantaneous flow variables for restart.
  !> - fl (in): Flow state to write.
  !> - dm (in): Domain descriptor and I/O mode.
  subroutine write_instantaneous_flow(fl, dm)
    use io_tools_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl

    character(64):: data_flname_path
    character(64):: keyword

    if(nrank == 0) call Print_debug_inline_msg("writing out instantaneous 3d flow data ...")

    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      call write_flow_restart_bundle(fl, dm)
    else if(dm%is_thermo) then
      call write_one_3d_array(fl%gx, 'gx', dm%idom, fl%iteration, dm%dpcc, dm%existing_output_policy)
      call write_one_3d_array(fl%gy, 'gy', dm%idom, fl%iteration, dm%dcpc, dm%existing_output_policy)
      call write_one_3d_array(fl%gz, 'gz', dm%idom, fl%iteration, dm%dccp, dm%existing_output_policy)
      call write_one_3d_array(fl%qx, 'qx', dm%idom, fl%iteration, dm%dpcc, dm%existing_output_policy)
      call write_one_3d_array(fl%qy, 'qy', dm%idom, fl%iteration, dm%dcpc, dm%existing_output_policy)
      call write_one_3d_array(fl%qz, 'qz', dm%idom, fl%iteration, dm%dccp, dm%existing_output_policy)
      call write_one_3d_array(fl%pres, 'pr', dm%idom, fl%iteration, dm%dccc, dm%existing_output_policy)
      if(is_restart_history_exact(dm)) then
        call write_one_3d_array(fl%mx_rhs0, 'mx_rhs0', dm%idom, fl%iteration, dm%dpcc, dm%existing_output_policy)
        call write_one_3d_array(fl%my_rhs0, 'my_rhs0', dm%idom, fl%iteration, dm%dcpc, dm%existing_output_policy)
        call write_one_3d_array(fl%mz_rhs0, 'mz_rhs0', dm%idom, fl%iteration, dm%dccp, dm%existing_output_policy)
      end if
      call write_flow_xoutlet_state_per_field(fl, dm)
      call write_checkpoint_manifest(dm%idom, fl%iteration, fl%time, dm%dt)
      call remove_legacy_restart_metadata(dm%idom, 'flow_meta', fl%iteration, dm%existing_output_policy)
    else
      call write_one_3d_array(fl%qx, 'qx', dm%idom, fl%iteration, dm%dpcc, dm%existing_output_policy)
      call write_one_3d_array(fl%qy, 'qy', dm%idom, fl%iteration, dm%dcpc, dm%existing_output_policy)
      call write_one_3d_array(fl%qz, 'qz', dm%idom, fl%iteration, dm%dccp, dm%existing_output_policy)
      call write_one_3d_array(fl%pres, 'pr', dm%idom, fl%iteration, dm%dccc, dm%existing_output_policy)
      if(is_restart_history_exact(dm)) then
        call write_one_3d_array(fl%mx_rhs0, 'mx_rhs0', dm%idom, fl%iteration, dm%dpcc, dm%existing_output_policy)
        call write_one_3d_array(fl%my_rhs0, 'my_rhs0', dm%idom, fl%iteration, dm%dcpc, dm%existing_output_policy)
        call write_one_3d_array(fl%mz_rhs0, 'mz_rhs0', dm%idom, fl%iteration, dm%dccp, dm%existing_output_policy)
      end if
      call write_flow_xoutlet_state_per_field(fl, dm)
      call write_checkpoint_manifest(dm%idom, fl%iteration, fl%time, dm%dt)
      call remove_legacy_restart_metadata(dm%idom, 'flow_meta', fl%iteration, dm%existing_output_policy)
    end if

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==============================================================================
!==============================================================================
  subroutine write_flow_restart_bundle(fl, dm)
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl

    if(is_restart_history_exact(dm)) then
      call write_flow_restart_bundle_exact(fl, dm)
    else
      call write_flow_restart_bundle_compact(fl, dm)
    end if

    return
  end subroutine write_flow_restart_bundle
!==============================================================================
  subroutine write_flow_restart_bundle_exact(fl, dm)
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    character(256) :: output_files(2)
    logical :: do_write

    call ensure_restart_convective_outlet_supported(dm)

    call generate_pathfile_name(output_files(1), dm%idom, 'flow_restart', dir_data, 'bin', fl%iteration)
    call generate_pathfile_name(output_files(2), dm%idom, 'flow_restart_meta', dir_data, 'dat', fl%iteration)
    bundle_file = output_files(1)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'flow restart bundle', do_write)
    if(.not. do_write) return
    if(nrank == 0) call Print_debug_mid_msg("Writing "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_write_mode)
    if(dm%is_thermo) then
      call decomp_2d_write_var(io, IPENCIL(1), fl%gx, opt_decomp=dm%dpcc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%gy, opt_decomp=dm%dcpc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%gz, opt_decomp=dm%dccp)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    else
      call decomp_2d_write_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    end if
    call decomp_2d_write_var(io, IPENCIL(1), fl%pres, opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), fl%mx_rhs0, opt_decomp=dm%dpcc)
    call decomp_2d_write_var(io, IPENCIL(1), fl%my_rhs0, opt_decomp=dm%dcpc)
    call decomp_2d_write_var(io, IPENCIL(1), fl%mz_rhs0, opt_decomp=dm%dccp)
    call write_flow_restart_xoutlet_state(io, fl, dm)
    call io%close()

    call write_bundle_metadata(dm, 'flow_restart', fl%iteration, &
      flow_restart_fields(dm), flow_restart_shapes(dm), fl%time, dm%dt)
    call write_fbcx_thermal_state_metadata(fl, dm, RESTART_LAYOUT_BUNDLED)
    call write_checkpoint_manifest(dm%idom, fl%iteration, fl%time, dm%dt)
    call remove_legacy_restart_metadata(dm%idom, 'flow_meta', fl%iteration, dm%existing_output_policy)

    return
  end subroutine write_flow_restart_bundle_exact
!==============================================================================
  ! opt_is_initial: the restart flow field is an initial condition for a
  ! thermal run whose thermal field is not restarted (see
  ! read_flow_restart_bundle_initial)
  subroutine read_flow_restart_bundle(fl, dm, opt_is_initial)
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    logical, intent(in), optional :: opt_is_initial

    logical :: is_initial

    is_initial = .false.
    if(present(opt_is_initial)) is_initial = opt_is_initial

    if(is_initial) then
      call read_flow_restart_bundle_initial(fl, dm)
    else if(is_restart_history_exact(dm)) then
      call read_flow_restart_bundle_exact(fl, dm)
    else
      call read_flow_restart_bundle_compact(fl, dm)
    end if

    return
  end subroutine read_flow_restart_bundle
!==============================================================================
  subroutine read_flow_restart_bundle_exact(fl, dm)
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file

    call ensure_restart_convective_outlet_supported(dm)

    call generate_pathfile_name(bundle_file, dm%idom, 'flow_restart', dir_data, 'bin', fl%iterfrom)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The file "//trim(bundle_file)//" does not exist.")
    call validate_bundle_metadata(dm, 'flow_restart', fl%iterfrom, &
      flow_restart_fields(dm), flow_restart_shapes(dm))
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_read_mode)
    if(dm%is_thermo) then
      call decomp_2d_read_var(io, IPENCIL(1), fl%gx, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%gy, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%gz, opt_decomp=dm%dccp)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    else
      call decomp_2d_read_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    end if
    call decomp_2d_read_var(io, IPENCIL(1), fl%pres, opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), fl%mx_rhs0, opt_decomp=dm%dpcc)
    call decomp_2d_read_var(io, IPENCIL(1), fl%my_rhs0, opt_decomp=dm%dcpc)
    call decomp_2d_read_var(io, IPENCIL(1), fl%mz_rhs0, opt_decomp=dm%dccp)
    call read_flow_restart_xoutlet_state(io, fl, dm)
    call io%close()
    fl%is_compact_restart_startup = .false.

    return
  end subroutine read_flow_restart_bundle_exact
!==============================================================================
  !----------------------------------------------------------------------------
  ! Flow restart of a thermal run whose thermal field is initialised afresh.
  ! The two fields do not continue a common trajectory, so this is an
  ! initialisation from a stored velocity field, not a restart, and the bundle
  ! is accepted in any layout a flow restart can be written in:
  !   - thermal, exact history
  !   - isothermal, exact history
  !   - isothermal, compact history
  ! (thermal compact is rejected at input, so it is never written). Only the
  ! primitive state q, pr and the outlet planes fbcx_q* are taken. Whatever
  ! else the bundle holds - g, fbcx_g*, fbcx_ftp, RHS histories - belongs to
  ! the old thermal field and is replaced in initialise_flow_from_restart_q.
  !----------------------------------------------------------------------------
  subroutine read_flow_restart_bundle_initial(fl, dm)
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    character(256) :: meta_file
    character(1024) :: fields_read
    type(t_fluidThermoProperty), allocatable :: fbcx_ftp_initial(:, :, :)
    logical :: is_thermo_read, is_exact_read

    call ensure_restart_convective_outlet_supported(dm)

    call generate_pathfile_name(bundle_file, dm%idom, 'flow_restart', dir_data, 'bin', fl%iterfrom)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The file "//trim(bundle_file)//" does not exist.")

    !--------------------------------------------------------------------------
    ! The layout is identified from the metadata field list; an unrecognised
    ! list falls through to the thermal-exact validation, which reports it.
    !--------------------------------------------------------------------------
    call generate_pathfile_name(meta_file, dm%idom, 'flow_restart_meta', dir_data, 'dat', fl%iterfrom)
    fields_read = ''
    if(file_exists(trim(meta_file))) fields_read = metadata_value(meta_file, 'fields')
    is_thermo_read = .true.
    is_exact_read  = .true.
    if(trim(fields_read) == trim(flow_restart_fields_exact(dm, opt_is_thermo=.false.))) then
      is_thermo_read = .false.
    else if(trim(fields_read) == trim(flow_restart_fields_compact(dm))) then
      is_thermo_read = .false.
      is_exact_read  = .false.
    end if
    if(is_exact_read) then
      call validate_bundle_metadata(dm, 'flow_restart', fl%iterfrom, &
        flow_restart_fields_exact(dm, opt_is_thermo=is_thermo_read), &
        flow_restart_shapes_exact(dm, opt_is_thermo=is_thermo_read))
    else
      call validate_bundle_metadata(dm, 'flow_restart', fl%iterfrom, &
        flow_restart_fields_compact(dm), flow_restart_shapes_compact(dm))
    end if
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(bundle_file))

    ! a thermal bundle would overwrite the outlet thermal state set up for the
    ! new thermal field
    if(is_thermo_read .and. dm%is_conv_outlet(1)) then
      allocate(fbcx_ftp_initial, source=dm%fbcx_ftp)
    end if

    call io%open(trim(bundle_file), decomp_2d_read_mode)
    if(is_thermo_read) then
      call decomp_2d_read_var(io, IPENCIL(1), fl%gx, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%gy, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%gz, opt_decomp=dm%dccp)
    end if
    call decomp_2d_read_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
    call decomp_2d_read_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
    call decomp_2d_read_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    call decomp_2d_read_var(io, IPENCIL(1), fl%pres, opt_decomp=dm%dccc)
    if(is_exact_read) then
      call decomp_2d_read_var(io, IPENCIL(1), fl%mx_rhs0, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%my_rhs0, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%mz_rhs0, opt_decomp=dm%dccp)
      call read_flow_restart_xoutlet_state(io, fl, dm, opt_is_thermo=is_thermo_read)
    else
      call read_flow_restart_xoutlet_state_compact(io, fl, dm, opt_is_thermo=.false.)
    end if
    call io%close()

    if(allocated(fbcx_ftp_initial)) then
      dm%fbcx_ftp = fbcx_ftp_initial
      deallocate(fbcx_ftp_initial)
    end if

    call initialise_flow_from_restart_q(fl, dm)

    return
  end subroutine read_flow_restart_bundle_initial
!==============================================================================
  !----------------------------------------------------------------------------
  ! Complete a flow field read as an initial condition for a thermal run
  ! (thermal field not restarted).
  !
  ! g = rho * u in the interior, rho being the density initialise_thermo_fields
  ! has just set (it runs before initialise_flow_fields). The boundary planes
  ! fbc*_g* follow from the IQ2G/IBND conversion in initialise_flow_fields.
  ! Unless rho is uniform, g = rho*u does not satisfy div(g) = -d(rho)/dt
  ! discretely; the first projection restores it.
  !
  ! The momentum and outlet RHS histories, if read, were accumulated against
  ! another thermal field (or none), so they are dropped and the first step
  ! uses startup history, as for restart_history_mode=compact.
  !----------------------------------------------------------------------------
  subroutine initialise_flow_from_restart_q(fl, dm)
    use convert_primary_conservative_mod, only: convert_primary_conservative
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl

    call convert_primary_conservative(dm, fl%dDens, IQ2G, IBLK, fl%qx, fl%qy, fl%qz, fl%gx, fl%gy, fl%gz)
    fl%mx_rhs0 = ZERO
    fl%my_rhs0 = ZERO
    fl%mz_rhs0 = ZERO
    if(dm%is_conv_outlet(1)) then
      fl%fbcx_a0cc_rhs0 = ZERO
      fl%fbcx_a0pc_rhs0 = ZERO
      fl%fbcx_a0cp_rhs0 = ZERO
    end if
    fl%is_compact_restart_startup = .true.

    if(nrank == 0) call Print_note_msg( &
      'Flow restart with a freshly initialised thermal field: the restart flow field is used as an '// &
      'initial condition. g = rho*u is rebuilt from the current density and the momentum RHS history is dropped.')

    return
  end subroutine initialise_flow_from_restart_q
!==============================================================================
  subroutine write_flow_restart_bundle_compact(fl, dm)
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    character(256) :: output_files(2)
    logical :: do_write

    call ensure_restart_convective_outlet_supported(dm)

    call generate_pathfile_name(output_files(1), dm%idom, 'flow_restart', dir_data, 'bin', fl%iteration)
    call generate_pathfile_name(output_files(2), dm%idom, 'flow_restart_meta', dir_data, 'dat', fl%iteration)
    bundle_file = output_files(1)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'flow restart bundle', do_write)
    if(.not. do_write) return
    if(nrank == 0) call Print_debug_mid_msg("Writing "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_write_mode)
    if(dm%is_thermo) then
      call decomp_2d_write_var(io, IPENCIL(1), fl%gx, opt_decomp=dm%dpcc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%gy, opt_decomp=dm%dcpc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%gz, opt_decomp=dm%dccp)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    else
      call decomp_2d_write_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    end if
    call decomp_2d_write_var(io, IPENCIL(1), fl%pres, opt_decomp=dm%dccc)
    call write_flow_restart_xoutlet_state_compact(io, fl, dm)
    call io%close()

    call write_bundle_metadata(dm, 'flow_restart', fl%iteration, &
      flow_restart_fields(dm), flow_restart_shapes(dm), fl%time, dm%dt)
    call write_fbcx_thermal_state_metadata(fl, dm, RESTART_LAYOUT_BUNDLED)
    call write_checkpoint_manifest(dm%idom, fl%iteration, fl%time, dm%dt)
    call remove_legacy_restart_metadata(dm%idom, 'flow_meta', fl%iteration, dm%existing_output_policy)

    return
  end subroutine write_flow_restart_bundle_compact
!==============================================================================
  subroutine read_flow_restart_bundle_compact(fl, dm)
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file

    call ensure_restart_convective_outlet_supported(dm)

    call generate_pathfile_name(bundle_file, dm%idom, 'flow_restart', dir_data, 'bin', fl%iterfrom)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The file "//trim(bundle_file)//" does not exist.")
    call validate_bundle_metadata(dm, 'flow_restart', fl%iterfrom, &
      flow_restart_fields(dm), flow_restart_shapes(dm))
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_read_mode)
    if(dm%is_thermo) then
      call decomp_2d_read_var(io, IPENCIL(1), fl%gx, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%gy, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%gz, opt_decomp=dm%dccp)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    else
      call decomp_2d_read_var(io, IPENCIL(1), fl%qx, opt_decomp=dm%dpcc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qy, opt_decomp=dm%dcpc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%qz, opt_decomp=dm%dccp)
    end if
    call decomp_2d_read_var(io, IPENCIL(1), fl%pres, opt_decomp=dm%dccc)
    call read_flow_restart_xoutlet_state_compact(io, fl, dm)
    call io%close()

    call rebuild_compact_flow_restart(fl, dm)

    return
  end subroutine read_flow_restart_bundle_compact
!==============================================================================
  subroutine write_thermo_restart_bundle(tm, dm, fl)
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm
    type(t_flow),   intent(in) :: fl

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    character(256) :: output_files(3)
    real(WP), dimension(dm%d1cc%xsz(1), dm%d1cc%xsz(2), dm%d1cc%xsz(3)) :: fbcx_rhoh_rhs0
    logical :: do_write

    call generate_pathfile_name(output_files(1), dm%idom, 'thermo_restart', dir_data, 'bin', tm%iteration)
    call generate_pathfile_name(output_files(2), dm%idom, 'thermo_restart_meta', dir_data, 'dat', tm%iteration)
    ! interpolation-mode sidecar: part of the same output set, so it is skipped,
    ! renamed or overwritten together with the bundle it describes
    call generate_pathfile_name(output_files(3), dm%idom, 'thermo_interp_meta', dir_data, 'dat', tm%iteration)
    bundle_file = output_files(1)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'thermo restart bundle', do_write)
    if(.not. do_write) return
    if(nrank == 0) call Print_debug_mid_msg("Writing "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_write_mode)
    call decomp_2d_write_var(io, IPENCIL(1), tm%rhoh,  opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), tm%tTemp, opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), tm%ene_rhs0, opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), fl%dDens, opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), fl%mVisc, opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), tm%hEnth, opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), tm%kCond, opt_decomp=dm%dccc)
    call decomp_2d_write_var(io, IPENCIL(1), tm%eCond, opt_decomp=dm%dccc)
    if(dm%is_conv_outlet(1)) then
      fbcx_rhoh_rhs0(1, :, :) = tm%fbcx_rhoh_rhs0(:, :)
      call decomp_2d_write_var(io, IPENCIL(1), fbcx_rhoh_rhs0, opt_decomp=dm%d1cc)
    end if
    if(is_bc_recovery) then
      call decomp_2d_write_var(io, IPENCIL(1), fl%dDens_hat, opt_decomp=dm%dccc)
      call decomp_2d_write_var(io, IPENCIL(1), fl%drhodt_m1, opt_decomp=dm%dccc)
      if(dm%is_conv_outlet(1)) then
        call decomp_2d_write_var(io, IPENCIL(1), tm%fbcx_dhat, opt_decomp=dm%d4cc)
        fbcx_rhoh_rhs0(1, :, :) = tm%fbcx_dhat_rhs0(:, :)
        call decomp_2d_write_var(io, IPENCIL(1), fbcx_rhoh_rhs0, opt_decomp=dm%d1cc)
      end if
    end if
    call io%close()

    call write_bundle_metadata(dm, 'thermo_restart', tm%iteration, &
      thermo_restart_fields(dm), thermo_restart_shapes(dm, opt_bc_fields=is_bc_recovery), tm%time, dm%dt)
    call write_thermo_flow_iteration(dm, tm%iteration, fl%iteration, RESTART_LAYOUT_BUNDLED)
    call write_thermo_interp_metadata(dm, tm%iteration)
    call write_checkpoint_manifest(dm%idom, tm%iteration, tm%time, dm%dt)
    call remove_legacy_restart_metadata(dm%idom, 'thermo_meta', tm%iteration, dm%existing_output_policy)

    return
  end subroutine write_thermo_restart_bundle
!==============================================================================
  !> Write the property-interpolation record of a thermal checkpoint,
  !> domain<d>_thermo_interp_meta_<iter>.dat (rank 0):
  !>   CHAPSim_thermo_interp_v1 / domain <d> / iteration <iter> /
  !>   property_interp <linear|pchip>
  !>   [enthalpy_advection <cd2|koren>]   (optional fifth line)
  !>   [enthalpy_advection_switch <from>-><to> <iteration>]   (optional sixth line)
  !> A separate text file, so the existing binary and metadata formats are
  !> unchanged; it is paired with the checkpoint by domain and iteration. The
  !> fifth line is written by a koren run, and by any run that carries a switch
  !> record; the sixth line records the latest permitted change of
  !> enthalpy_advection at a restart. A cd2 run without a switch record writes
  !> neither, so its record is unchanged; readers that predate the lines ignore
  !> them.
  subroutine write_thermo_interp_metadata(dm, iter)
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: iter
    character(256) :: meta_file
    integer :: u, ios
    if(nrank /= 0) return
    call generate_pathfile_name(meta_file, dm%idom, 'thermo_interp_meta', dir_data, 'dat', iter)
    open(newunit=u, file=trim(meta_file), status='replace', action='write', iostat=ios)
    if(ios /= 0) call Print_error_msg("Could not write "//trim(meta_file))
    write(u, '(A)') 'CHAPSim_thermo_interp_v1'
    write(u, '(A,1X,I0)') 'domain', dm%idom
    write(u, '(A,1X,I0)') 'iteration', iter
    if(iprop_interp == IPROP_INTERP_PCHIP) then
      write(u, '(A)') 'property_interp pchip'
    else
      write(u, '(A)') 'property_interp linear'
    end if
    if(len_trim(enth_adv_switch_note) > 0) then
      if(ienth_adv == IENTH_ADV_KOREN) then
        write(u, '(A)') 'enthalpy_advection koren'
      else
        write(u, '(A)') 'enthalpy_advection cd2'
      end if
      write(u, '(A)') 'enthalpy_advection_switch '//trim(enth_adv_switch_note)
    else if(ienth_adv == IENTH_ADV_KOREN) then
      write(u, '(A)') 'enthalpy_advection koren'
    end if
    close(u)
    return
  end subroutine write_thermo_interp_metadata
!==============================================================================
  !> Records the flow step at which a thermal checkpoint is written ('flow_iteration <n>'). Thermal
  !> checkpoints are labelled by tm%iteration, which follows the run clock (chapsim.f90), so the record
  !> equals the label. Builds before 2026-10-30 advanced tm%iteration only while the energy equation was
  !> solved: with niterthermofirst > niterflowfirst the label was lower than the flow step, and a thermal
  !> checkpoint could carry the label of an earlier flow checkpoint with the same stored time; the
  !> record lets a restart detect such a pair. Bundled layout: appended after field_shapes of the thermal
  !> bundle metadata (readers that validate the fixed keys ignore it); per-field layout: one-line
  !> sidecar domain<d>_thermo_clock_<iter>.dat.
  subroutine write_thermo_flow_iteration(dm, iter, flow_iter, layout)
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: iter, flow_iter, layout
    character(256) :: meta_file
    integer :: u
    if(nrank /= 0) return
    if(layout == RESTART_LAYOUT_BUNDLED) then
      call generate_pathfile_name(meta_file, dm%idom, 'thermo_restart_meta', dir_data, 'dat', iter)
      open(newunit=u, file=trim(meta_file), status='old', position='append', action='write')
    else
      call generate_pathfile_name(meta_file, dm%idom, 'thermo_clock', dir_data, 'dat', iter)
      open(newunit=u, file=trim(meta_file), status='replace', action='write')
    end if
    write(u, '(A,1X,I0)') 'flow_iteration', flow_iter
    close(u)
  end subroutine write_thermo_flow_iteration
!==============================================================================
  !> A thermal restart from checkpoint iter must start from the thermal state written at flow step
  !> iter (the flow is restarted from the same label). Stops when the recorded flow step differs: the
  !> energy equation of the run that wrote it started after the flow, so the two checkpoints with
  !> this label hold different steps (with the same stored time), and continuing would couple a
  !> thermal field to a flow field of another step without notice. A checkpoint written before the
  !> record existed cannot be checked: warning.
  subroutine check_thermo_flow_iteration(dm, iter)
    use mpi_mod
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: iter
    character(256) :: meta_file
    character(1024) :: line
    character(64) :: label
    character(160) :: str
    integer :: u, ios, flow_iter
    flow_iter = -1
    if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
      call generate_pathfile_name(meta_file, dm%idom, 'thermo_restart_meta', dir_data, 'dat', iter)
    else
      call generate_pathfile_name(meta_file, dm%idom, 'thermo_clock', dir_data, 'dat', iter)
    end if
    if(nrank == 0) then
      open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ios)
      if(ios == 0) then
        do
          read(u, '(A)', iostat=ios) line
          if(ios /= 0) exit
          line = adjustl(line)
          if(line(1:15) == 'flow_iteration ') then
            read(line, *, iostat=ios) label, flow_iter
            if(ios /= 0) flow_iter = -2
            exit
          end if
        end do
        close(u)
      end if
    end if
    call mpi_bcast(flow_iter, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
    if(flow_iter == -2) call Print_error_msg('Unreadable flow_iteration in '//trim(meta_file))
    if(flow_iter == -1) then
      if(nrank == 0) call Print_warning_msg('Thermal checkpoint without flow_iteration ('//trim(meta_file)// &
        '): it cannot be verified that it was written at flow step '//trim(int2str(iter))//'. If the '// &
        'energy equation of the run that wrote it started after the flow and that run was not restarted '// &
        'since, its thermal state is from a later flow step than the flow checkpoint with the same label.')
      return
    end if
    if(flow_iter /= iter) then
      write(str, '(A,I0,A,I0,A)') 'thermal checkpoint ', iter, ' was written at flow step ', flow_iter, &
        ', flow checkpoint '
      call Print_error_msg('Restart: '//trim(str)//' '//trim(int2str(iter))//' at flow step '// &
        trim(int2str(iter))//'. The energy equation of the run that wrote them started after the '// &
        'flow, and that build labelled thermal checkpoints by the thermal step count. The pair holds two '// &
        'different steps and cannot be continued jointly.')
    end if
  end subroutine check_thermo_flow_iteration
!==============================================================================
  !> Restart policy for the property interpolation (all ranks; rank 0 reads and
  !> broadcasts, so every rank takes the same decision):
  !>   record linear -> run linear: allowed;  record pchip -> run pchip: allowed;
  !>   record and run differ: stop;
  !>   no record (checkpoint written before property_interp existed, i.e. with
  !>   linear interpolation): allowed for linear, as before; stop for pchip;
  !>   unreadable record, or domain/iteration not matching the checkpoint: stop.
  !> A change of interpolation at a restart is rejected because the stored
  !> density and rho_hat belong to the writing mode's property relation: the new
  !> mode would relax rho in one step and keep a permanent rho_hat offset.
  !> Enthalpy advection (optional fifth line; absent = cd2, which includes every
  !> checkpoint written before the key existed), checked after the property
  !> interpolation, whose rules above are not affected:
  !>   same mode: allowed;
  !>   different mode, enthalpy_advection_restart = strict (default): stop;
  !>   different mode, allow_switch: allowed with a warning and recorded in the
  !>     sixth line of later records, unless the statistics would continue across
  !>     the change (stat_level > 0, iteration > stat_istart, run clock
  !>     continued): then stop until the statistics window is reset;
  !>   unrecognised fifth or sixth line: stop.
  !> The stored state does not depend on the face reconstruction, so a switch is
  !> admissible numerically; it is gated because it changes the scheme of a run
  !> (reproducibility) and would mix two schemes in one averaging window.
  subroutine check_thermo_interp_metadata(dm, iter)
    use mpi_mod
    use typeconvert_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer,        intent(in) :: iter
    character(256) :: meta_file
    character(64)  :: label, mode
    character(80)  :: header
    character(16)  :: other
    character(64)  :: note_a, note_b, note_read
    character(8)   :: mode_ckpt, mode_run
    integer :: u, ios, idom_read, iter_read, code, iadv_read
    ! code: 0 no record, 1 linear, 2 pchip, -1 unreadable/invalid, -2 domain/iteration mismatch
    call generate_pathfile_name(meta_file, dm%idom, 'thermo_interp_meta', dir_data, 'dat', iter)
    code = 0
    iadv_read = IENTH_ADV_CD2
    note_read = ''
    if(nrank == 0) then
      if(file_exists(trim(meta_file))) then
        code = -1
        open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ios)
        if(ios == 0) then
          read(u, '(A)', iostat=ios) header
          if(ios == 0 .and. trim(header) == 'CHAPSim_thermo_interp_v1') then
            read(u, *, iostat=ios) label, idom_read
            if(ios == 0 .and. trim(label) == 'domain') then
              read(u, *, iostat=ios) label, iter_read
              if(ios == 0 .and. trim(label) == 'iteration') then
                read(u, *, iostat=ios) label, mode
                if(ios == 0 .and. trim(label) == 'property_interp') then
                  if(idom_read /= dm%idom .or. iter_read /= iter) then
                    code = -2
                  else if(trim(mode) == 'linear') then
                    code = 1
                  else if(trim(mode) == 'pchip') then
                    code = 2
                  end if
                  if(code > 0) then
                    read(u, *, iostat=ios) label, mode
                    if(ios == 0) then
                      if(trim(label) == 'enthalpy_advection' .and. trim(mode) == 'koren') then
                        iadv_read = IENTH_ADV_KOREN
                      else if(.not. (trim(label) == 'enthalpy_advection' .and. trim(mode) == 'cd2')) then
                        code = -1
                      end if
                      if(code > 0) then
                        read(u, *, iostat=ios) label, note_a, note_b
                        if(ios == 0) then
                          if(trim(label) == 'enthalpy_advection_switch') then
                            note_read = trim(note_a)//' '//trim(note_b)
                          else
                            code = -1
                          end if
                        end if
                      end if
                    end if
                  end if
                end if
              end if
            end if
          end if
          close(u)
        end if
      end if
    end if
    call mpi_bcast(code, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
    call mpi_bcast(iadv_read, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
    call mpi_bcast(note_read, len(note_read), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierror)
    select case(code)
    case(-1)
      call Print_error_msg("Unreadable or invalid interpolation record "//trim(meta_file)// &
        " (expected lines: CHAPSim_thermo_interp_v1, domain <d>, iteration <n>, property_interp linear|pchip"// &
        ", optionally enthalpy_advection cd2|koren and enthalpy_advection_switch <from>-><to> <iteration>).")
    case(-2)
      call Print_error_msg("Interpolation record "//trim(meta_file)// &
        " names another domain or iteration; it does not belong to this thermal checkpoint.")
    case(0)
      if(iprop_interp == IPROP_INTERP_PCHIP) call Print_error_msg("Thermal checkpoint at iteration "// &
        trim(int2str(iter))//" has no interpolation record ("//trim(meta_file)//"): it was written "// &
        "before property_interp existed, i.e. with linear interpolation, and cannot be continued with "// &
        "property_interp = pchip. Restart with linear, or start a fresh thermal field (inittm /= restart).")
      if(nrank == 0) call Print_note_msg("Thermal checkpoint has no interpolation record (written before "// &
        "property_interp existed); continuing with linear interpolation, as before.")
    case default
      if((code == 2) .neqv. (iprop_interp == IPROP_INTERP_PCHIP)) then
        other = 'linear'
        if(code == 2) other = 'pchip'
        call Print_error_msg("Thermal checkpoint at iteration "//trim(int2str(iter))//" was written with "// &
          "property_interp = "//trim(other)//", which differs from this run. Changing the property "// &
          "interpolation at a restart is not supported (the stored density belongs to the other "// &
          "interpolation); restart with property_interp = "//trim(other)//", or start a fresh thermal "// &
          "field (inittm /= restart).")
      end if
    end select
    ! enthalpy advection: the provenance of the checkpoint is carried forward
    enth_adv_switch_note = note_read
    if(iadv_read /= ienth_adv) then
      mode_ckpt = 'cd2'
      if(iadv_read == IENTH_ADV_KOREN) mode_ckpt = 'koren'
      mode_run = 'cd2'
      if(ienth_adv == IENTH_ADV_KOREN) mode_run = 'koren'
      if(.not. is_enth_adv_switch_allowed) call Print_error_msg("Thermal checkpoint at iteration "// &
        trim(int2str(iter))//" was advanced with enthalpy_advection = "//trim(mode_ckpt)//"; this run "// &
        "requests enthalpy_advection = "//trim(mode_run)//". Changing the enthalpy advection scheme at a "// &
        "restart requires enthalpy_advection_restart = allow_switch in [thermo]; otherwise restart with "// &
        "enthalpy_advection = "//trim(mode_ckpt)//", or start a fresh thermal field (inittm /= restart).")
      if(dm%stat_level > ISTATL0 .and. iter > dm%stat_istart .and. dm%restart_clock == RESTART_CLOCK_CONTINUE) &
        call Print_error_msg("enthalpy_advection changes from "//trim(mode_ckpt)//" to "//trim(mode_run)// &
        " at the restart from iteration "//trim(int2str(iter))//", but the statistics averaging window "// &
        "(stat_istart = "//trim(int2str(dm%stat_istart))//") would continue across the change. Set "// &
        "stat_istart >= "//trim(int2str(iter))//" to start a new averaging window at the restart (or "// &
        "restart the run clock), or keep enthalpy_advection = "//trim(mode_ckpt)//".")
      enth_adv_switch_note = trim(mode_ckpt)//'->'//trim(mode_run)//' '//trim(int2str(iter))
      if(nrank == 0) call Print_warning_msg("ENTHALPY ADVECTION SCHEME CHANGED AT RESTART: the thermal "// &
        "checkpoint at iteration "//trim(int2str(iter))//" was advanced with enthalpy_advection = "// &
        trim(mode_ckpt)//"; this run continues it with enthalpy_advection = "//trim(mode_run)// &
        " (enthalpy_advection_restart = allow_switch). Recorded as 'enthalpy_advection_switch "// &
        trim(enth_adv_switch_note)//"' in later thermal checkpoint records.")
    end if
    return
  end subroutine check_thermo_interp_metadata
!==============================================================================
  subroutine read_thermo_restart_bundle(tm, fl, dm)
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_thermo), intent(inout) :: tm
    type(t_flow),   intent(inout) :: fl

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    character(256) :: meta_file
    character(1024) :: fields_read
    logical :: has_bc_fields
    real(WP), dimension(dm%d1cc%xsz(1), dm%d1cc%xsz(2), dm%d1cc%xsz(3)) :: fbcx_rhoh_rhs0

    call generate_pathfile_name(bundle_file, dm%idom, 'thermo_restart', dir_data, 'bin', tm%iterfrom)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The file "//trim(bundle_file)//" does not exist. "//THERMO_LABEL_HINT)
    !--------------------------------------------------------------------------
    ! Format identification by the stored field list:
    !   with Bc fields   -> exact continuation of a transported-density run;
    !   legacy list      -> 8d47f18 / lagged_legacy checkpoint. Under Bc it is
    !                       accepted with a warning: rho_hat = rho and a zero
    !                       rho_t history are reconstructed (not exact);
    !   anything else    -> rejected by validate_bundle_metadata.
    !--------------------------------------------------------------------------
    call generate_pathfile_name(meta_file, dm%idom, 'thermo_restart_meta', dir_data, 'dat', tm%iterfrom)
    fields_read = ''
    if(file_exists(trim(meta_file))) fields_read = metadata_value(meta_file, 'fields')
    has_bc_fields = (trim(fields_read) == trim(thermo_restart_fields_legacy(dm))//trim(thermo_restart_fields_bc_extra(dm)))
    if(has_bc_fields) then
      call validate_bundle_metadata(dm, 'thermo_restart', tm%iterfrom, &
        trim(thermo_restart_fields_legacy(dm))//trim(thermo_restart_fields_bc_extra(dm)), &
        thermo_restart_shapes(dm, opt_bc_fields=.true.))
      if(.not. is_bc_recovery .and. nrank == 0) call Print_warning_msg( &
        'Thermal restart written with rhoh_recovery = transported is read by lagged_legacy: '// &
        'rho_hat and rho_t history are ignored.')
    else
      call validate_bundle_metadata(dm, 'thermo_restart', tm%iterfrom, &
        thermo_restart_fields_legacy(dm), thermo_restart_shapes(dm))
      if(is_bc_recovery .and. nrank == 0) call Print_warning_msg( &
        'LEGACY thermal restart (no rho_hat / rho_t history) read with rhoh_recovery = transported: '// &
        'rho_hat = rho and a zero rho_t history are reconstructed. The continuation is NOT exact; '// &
        'the first steps differ from an uninterrupted run at the level of one time step error.')
    end if
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_read_mode)
    call decomp_2d_read_var(io, IPENCIL(1), tm%rhoh,  opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), tm%tTemp, opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), tm%ene_rhs0, opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), fl%dDens, opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), fl%mVisc, opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), tm%hEnth, opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), tm%kCond, opt_decomp=dm%dccc)
    call decomp_2d_read_var(io, IPENCIL(1), tm%eCond, opt_decomp=dm%dccc)
    if(dm%is_conv_outlet(1)) then
      call decomp_2d_read_var(io, IPENCIL(1), fbcx_rhoh_rhs0, opt_decomp=dm%d1cc)
      tm%fbcx_rhoh_rhs0(:, :) = fbcx_rhoh_rhs0(1, :, :)
    end if
    if(has_bc_fields) then
      call decomp_2d_read_var(io, IPENCIL(1), fl%dDens_hat, opt_decomp=dm%dccc)
      call decomp_2d_read_var(io, IPENCIL(1), fl%drhodt_m1, opt_decomp=dm%dccc)
      if(dm%is_conv_outlet(1)) then
        call decomp_2d_read_var(io, IPENCIL(1), tm%fbcx_dhat, opt_decomp=dm%d4cc)
        call decomp_2d_read_var(io, IPENCIL(1), fbcx_rhoh_rhs0, opt_decomp=dm%d1cc)
        tm%fbcx_dhat_rhs0(:, :) = fbcx_rhoh_rhs0(1, :, :)
        tm%is_fbcx_dhat_valid = is_bc_recovery
      end if
      fl%is_bc_state_restored = is_bc_recovery
    end if
    call io%close()

    return
  end subroutine read_thermo_restart_bundle
!==============================================================================
  ! LES state of a heated LES run, stored with the thermal checkpoint.
  !
  ! The energy equation of a substep uses the subgrid viscosity fl%tVisc that
  ! WALE computed in the previous momentum substep, and the subgrid Prandtl
  ! number tm%prSgs that the previous energy substep refreshed from that
  ! viscosity (loop order in chapsim.f90: energy, Lorentz force, momentum; see
  ! Solve_energy_eq). Neither can be rebuilt from the other checkpoint fields:
  ! the last WALE call of a step acted on the velocity at the start of the last
  ! substep, not on the final velocity, and the last Pr_sgs refresh used the
  ! viscosity of the substep before. Both are therefore stored, so that a
  ! continuation repeats the uninterrupted run. An isothermal LES run needs
  ! neither: WALE runs before the momentum equation uses tVisc.
  function les_restart_fields() result(fields)
    implicit none
    character(64) :: fields

    fields = 'tvisc:dccc,prsgs:dccc'

    return
  end function les_restart_fields
!==============================================================================
  function les_restart_shapes(dm) result(shapes)
    implicit none
    type(t_domain), intent(in) :: dm
    character(512) :: shapes

    shapes = trim(bundle_shape('tvisc', dm%dccc))//';'//trim(bundle_shape('prsgs', dm%dccc))

    return
  end function les_restart_shapes
!==============================================================================
  subroutine write_les_restart_state(fl, tm, dm)
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_flow),   intent(in) :: fl
    type(t_thermo), intent(in) :: tm

    type(d2d_io_mpi) :: io
    character(256) :: output_files(2)
    logical :: do_write

    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      call generate_pathfile_name(output_files(1), dm%idom, 'les_restart', dir_data, 'bin', tm%iteration)
      call generate_pathfile_name(output_files(2), dm%idom, 'les_restart_meta', dir_data, 'dat', tm%iteration)
      call prepare_output_file_set(output_files, dm%existing_output_policy, 'LES restart bundle', do_write)
      if(.not. do_write) return
      if(nrank == 0) call Print_debug_mid_msg("Writing "//trim(output_files(1)))
      call io%open(trim(output_files(1)), decomp_2d_write_mode)
      call decomp_2d_write_var(io, IPENCIL(1), fl%tVisc, opt_decomp=dm%dccc)
      call decomp_2d_write_var(io, IPENCIL(1), tm%prSgs, opt_decomp=dm%dccc)
      call io%close()
      call write_bundle_metadata(dm, 'les_restart', tm%iteration, &
        les_restart_fields(), les_restart_shapes(dm), tm%time, dm%dt)
    else
      call write_one_3d_array(fl%tVisc, 'tvisc', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(tm%prSgs, 'prsgs', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
    end if

    return
  end subroutine write_les_restart_state
!==============================================================================
  !> Restore the LES state of a heated LES continuation (write_les_restart_state).
  !> Called by initialise_flow_fields after the initial WALE call, whose tVisc it
  !> replaces. A checkpoint written before the LES state was stored has none:
  !> then tVisc stays the one WALE computed from the restart field and Pr_sgs
  !> keeps its allocation value, the high-Peclet limit, as at a fresh start; the
  !> first energy substep then differs from the uninterrupted run, and the
  !> continuation is not bit-continuous. A warning says so.
  !> - fl (inout): receives tVisc.
  !> - tm (inout): receives prSgs.
  !> - dm (in): Domain descriptor and restart layout.
  subroutine read_les_restart_state(fl, tm, dm)
    use typeconvert_mod, only: int2str
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file, meta_file, tvisc_file, prsgs_file
    logical :: found

    if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
      call generate_pathfile_name(bundle_file, dm%idom, 'les_restart', dir_data, 'bin', tm%iterfrom)
      call generate_pathfile_name(meta_file, dm%idom, 'les_restart_meta', dir_data, 'dat', tm%iterfrom)
      ! explicit control flow: both file queries are always made
      found = file_exists(trim(bundle_file))
      if(file_exists(trim(meta_file))) found = .true.
      if(found) then
        if(.not. file_exists(trim(bundle_file))) &
        call Print_error_msg("The file "//trim(bundle_file)//" does not exist.")
        call validate_bundle_metadata(dm, 'les_restart', tm%iterfrom, les_restart_fields(), les_restart_shapes(dm))
        if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(bundle_file))
        call io%open(trim(bundle_file), decomp_2d_read_mode)
        call decomp_2d_read_var(io, IPENCIL(1), fl%tVisc, opt_decomp=dm%dccc)
        call decomp_2d_read_var(io, IPENCIL(1), tm%prSgs, opt_decomp=dm%dccc)
        call io%close()
      end if
    else
      call generate_pathfile_name(tvisc_file, dm%idom, 'tvisc', dir_data, 'bin', tm%iterfrom)
      call generate_pathfile_name(prsgs_file, dm%idom, 'prsgs', dir_data, 'bin', tm%iterfrom)
      ! explicit control flow: both file queries are always made
      found = file_exists(trim(tvisc_file))
      if(file_exists(trim(prsgs_file))) found = .true.
      if(found) then
        call read_one_3d_array(fl%tVisc, 'tvisc', dm%idom, tm%iterfrom, dm%dccc)
        call read_one_3d_array(tm%prSgs, 'prsgs', dm%idom, tm%iterfrom, dm%dccc)
      end if
    end if

    if(.not. found .and. nrank == 0) call Print_warning_msg( &
      "The checkpoint at iteration "//trim(int2str(tm%iterfrom))//" holds no LES state "// &
      "(written before the subgrid viscosity and Prandtl number were checkpointed, "// &
      "or by field interpolation). "// &
      "The subgrid viscosity is recomputed from the restart field and Pr_sgs starts at "// &
      "its high-Peclet limit, as at a fresh start, so this continuation is not bit-continuous.")

    return
  end subroutine read_les_restart_state
!==============================================================================
!==============================================================================
  !> Write instantaneous thermal variables for restart.
  !> - tm (in): Thermal state to write.
  !> - dm (in): Domain descriptor and I/O mode.
  subroutine write_instantaneous_thermo(tm, fl, dm)
    use thermo_info_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(t_thermo), intent(in) :: tm
    type(t_flow),   intent(in) :: fl

    character(64):: data_flname_path
    character(64):: keyword
    character(256) :: sidecar_file(1)
    logical :: sidecar_kept, do_write
    real(WP), dimension(dm%d1cc%xsz(1), dm%d1cc%xsz(2), dm%d1cc%xsz(3)) :: fbcx_1cc


    if(nrank == 0) call Print_debug_inline_msg("writing out instantaneous 3d thermo data ...")

    if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
      call write_thermo_restart_bundle(tm, dm, fl)
    else
      ! interpolation-mode sidecar: skipped when the rhoh file it describes is
      ! kept (skip policy), otherwise written with the per-field arrays
      call generate_pathfile_name(sidecar_file(1), dm%idom, 'rhoh', dir_data, 'bin', tm%iteration)
      sidecar_kept = .false.
      ! explicit control flow: the file query must not depend on short-circuit evaluation
      if(nrank == 0) then
        if(dm%existing_output_policy == OUTPUT_POLICY_SKIP) sidecar_kept = file_exists(trim(sidecar_file(1)))
      end if
      call mpi_bcast(sidecar_kept, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierror)
      call write_one_3d_array(tm%rhoh,  'rhoh', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(tm%tTemp, 'temp', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(tm%ene_rhs0, 'ene_rhs0', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(fl%dDens, 'dens', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(fl%mVisc, 'visc', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(tm%hEnth, 'henth', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(tm%kCond, 'kcond', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      call write_one_3d_array(tm%eCond, 'econd', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      if(is_bc_recovery) then
        call write_one_3d_array(fl%dDens_hat, 'dens_hat', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
        call write_one_3d_array(fl%drhodt_m1, 'drhodt_m1', dm%idom, tm%iteration, dm%dccc, dm%existing_output_policy)
      end if
      ! convective x-outlet: the thermal outlet state the bundle also stores
      if(dm%is_conv_outlet(1)) then
        fbcx_1cc(1, :, :) = tm%fbcx_rhoh_rhs0(:, :)
        call write_one_3d_array(fbcx_1cc, 'fbcx_rhoh_rhs0', dm%idom, tm%iteration, dm%d1cc, dm%existing_output_policy)
        if(is_bc_recovery) then
          call write_one_3d_array(tm%fbcx_dhat, 'fbcx_dhat', dm%idom, tm%iteration, dm%d4cc, dm%existing_output_policy)
          fbcx_1cc(1, :, :) = tm%fbcx_dhat_rhs0(:, :)
          call write_one_3d_array(fbcx_1cc, 'fbcx_dhat_rhs0', dm%idom, tm%iteration, dm%d1cc, dm%existing_output_policy)
        end if
      end if
      if(.not. sidecar_kept) then
        call generate_pathfile_name(sidecar_file(1), dm%idom, 'thermo_interp_meta', dir_data, 'dat', tm%iteration)
        call prepare_output_file_set(sidecar_file, dm%existing_output_policy, 'thermo interpolation metadata', do_write)
        if(do_write) call write_thermo_interp_metadata(dm, tm%iteration)
        call generate_pathfile_name(sidecar_file(1), dm%idom, 'thermo_clock', dir_data, 'dat', tm%iteration)
        call prepare_output_file_set(sidecar_file, dm%existing_output_policy, 'thermo clock record', do_write)
        if(do_write) call write_thermo_flow_iteration(dm, tm%iteration, fl%iteration, RESTART_LAYOUT_PER_FIELD)
      end if
      call write_checkpoint_manifest(dm%idom, tm%iteration, tm%time, dm%dt)
      call remove_legacy_restart_metadata(dm%idom, 'thermo_meta', tm%iteration, dm%existing_output_policy)
    end if
    ! heated LES: the lagged subgrid state the next energy substep reads
    if(dm%LES_model /= ILES_NONE .and. allocated(tm%prSgs) .and. allocated(fl%tVisc)) &
      call write_les_restart_state(fl, tm, dm)

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine
!==============================================================================
!==============================================================================
  !> Read instantaneous flow variables from restart files.
  !> - fl (inout): Flow state receiving restart fields.
  !> - dm (inout): Domain descriptor.
  !> - opt_is_initial (in): the flow field initialises a thermal run whose
  !>   thermal field is not restarted; see read_flow_restart_bundle_initial.
  subroutine read_instantaneous_flow(fl, dm, opt_is_initial)
    use io_tools_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    logical, intent(in), optional :: opt_is_initial

    character(64):: data_flname
    character(64):: keyword
    real(WP) :: restart_dt
    logical  :: has_metadata
    logical  :: is_initial


    if(nrank == 0) call Print_debug_inline_msg("read instantaneous flow data ...")
    fl%iteration = fl%iterfrom
    call read_checkpoint_restart_metadata(dm%idom, 'flow_meta', fl%iterfrom, fl%time, restart_dt, has_metadata)
    if(.not. has_metadata) then
      fl%time = real(fl%iterfrom, WP) * dm%dt
      if(nrank == 0) call Print_warning_msg("Flow restart metadata was not found. " // &
        "The restart time is estimated as iterfrom * current dt.")
    else if(abs(restart_dt - dm%dt) > max(MINP, MINP * abs(restart_dt))) then
      if(nrank == 0) call Print_warning_msg("Current input dt differs from the flow checkpoint dt. " // &
        "The stored restart time is preserved, and the input dt is used for the next steps.")
    end if

    is_initial = .false.
    if(present(opt_is_initial)) is_initial = opt_is_initial

    if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
      if(nrank == 0) call Print_debug_mid_msg("Restart input layout expects bundled flow restart files.")
      call read_flow_restart_bundle(fl, dm, opt_is_initial=is_initial)
    else if(is_initial) then
      if(nrank == 0) call Print_debug_mid_msg("Restart input layout expects per-field flow restart files.")
      call read_one_3d_array(fl%qx, 'qx', dm%idom, fl%iterfrom, dm%dpcc)
      call read_one_3d_array(fl%qy, 'qy', dm%idom, fl%iterfrom, dm%dcpc)
      call read_one_3d_array(fl%qz, 'qz', dm%idom, fl%iterfrom, dm%dccp)
      call read_one_3d_array(fl%pres, 'pr', dm%idom, fl%iterfrom, dm%dccc)
      ! The bundled initial read (read_flow_restart_bundle_initial) also takes the
      ! outlet planes fbcx_q*. The per-field flow_xoutlet file carries no field list,
      ! so its layout (thermal or isothermal writer, exact or compact) cannot be
      ! identified here and the outlet keeps its initial state: say so.
      if(dm%is_conv_outlet(1) .and. nrank == 0) call Print_warning_msg( &
        'Per-field flow checkpoint used as an initial field with a convective x-outlet: the outlet '// &
        'planes start from their initial state, not from the checkpoint (the bundled layout restores them).')
      call initialise_flow_from_restart_q(fl, dm)
    else
      if(nrank == 0) call Print_debug_mid_msg("Restart input layout expects per-field flow restart files.")
      if(dm%is_thermo) then
        call read_one_3d_array(fl%gx, 'gx', dm%idom, fl%iterfrom, dm%dpcc)
        call read_one_3d_array(fl%gy, 'gy', dm%idom, fl%iterfrom, dm%dcpc)
        call read_one_3d_array(fl%gz, 'gz', dm%idom, fl%iterfrom, dm%dccp)
        call read_one_3d_array(fl%qx, 'qx', dm%idom, fl%iterfrom, dm%dpcc)
        call read_one_3d_array(fl%qy, 'qy', dm%idom, fl%iterfrom, dm%dcpc)
        call read_one_3d_array(fl%qz, 'qz', dm%idom, fl%iterfrom, dm%dccp)
      else
        call read_one_3d_array(fl%qx, 'qx', dm%idom, fl%iterfrom, dm%dpcc)
        call read_one_3d_array(fl%qy, 'qy', dm%idom, fl%iterfrom, dm%dcpc)
        call read_one_3d_array(fl%qz, 'qz', dm%idom, fl%iterfrom, dm%dccp)
      end if
      call read_one_3d_array(fl%pres, 'pr', dm%idom, fl%iterfrom, dm%dccc)
      if(is_restart_history_exact(dm)) then
        call read_one_3d_array(fl%mx_rhs0, 'mx_rhs0', dm%idom, fl%iterfrom, dm%dpcc)
        call read_one_3d_array(fl%my_rhs0, 'my_rhs0', dm%idom, fl%iterfrom, dm%dcpc)
        call read_one_3d_array(fl%mz_rhs0, 'mz_rhs0', dm%idom, fl%iterfrom, dm%dccp)
        call read_flow_xoutlet_state_per_field(fl, dm)
        fl%is_compact_restart_startup = .false.
      else
        call read_flow_xoutlet_state_per_field(fl, dm)
        call rebuild_compact_flow_restart(fl, dm)
      end if
    end if

    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

!==============================================================================
  subroutine rebuild_compact_flow_restart(fl, dm)
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(in)    :: dm

    fl%mx_rhs0 = ZERO
    fl%my_rhs0 = ZERO
    fl%mz_rhs0 = ZERO
    fl%is_compact_restart_startup = .true.

    if(nrank == 0) call Print_warning_msg( &
      'restart_history_mode=compact: momentum RHS history was not read; ' // &
      'the first restarted AB2 momentum step will use startup history.')

    return
  end subroutine rebuild_compact_flow_restart
!==============================================================================
  !> Volume-average the streamwise component that a restart normalises to unity.
  !>
  !> A variable-property run carries conservative momentum, so the quantity that
  !> has to be unity is the bulk mass flux g = rho*u - the same target
  !> initialise_poiseuille_flow uses when it builds a fresh thermal field. An
  !> isothermal run carries the primitive velocity q.
  !> - fl (in): Flow state holding the restored field.
  !> - dm (in): Domain descriptor.
  !> - is_z_streamwise (in): True when the streamwise direction is z (duct).
  !> - bulk_var (out): Name of the averaged component, for logging.
  !> - bulk (out): Volume-averaged streamwise component.
  subroutine measure_streamwise_bulk(fl, dm, is_z_streamwise, bulk_var, bulk)
    use find_max_min_ave_mod
    use parameters_constant_mod
    use solver_tools_mod
    use udf_type_mod
    implicit none
    type(t_flow),   intent(in)  :: fl
    type(t_domain), intent(in)  :: dm
    logical,        intent(in)  :: is_z_streamwise
    character(2),   intent(out) :: bulk_var
    real(WP),       intent(out) :: bulk

    if(is_z_streamwise) then
      if(dm%is_thermo) then
        bulk_var = 'gz'
        call Get_volumetric_average_3d(dm, dm%dccp, fl%gz, bulk, SPACE_AVERAGE, bulk_var)
      else
        bulk_var = 'qz'
        call Get_volumetric_average_3d(dm, dm%dccp, fl%qz, bulk, SPACE_AVERAGE, bulk_var)
      end if
    else
      if(dm%is_thermo) then
        bulk_var = 'gx'
        call Get_volumetric_average_3d(dm, dm%dpcc, fl%gx, bulk, SPACE_AVERAGE, bulk_var)
      else
        bulk_var = 'qx'
        call Get_volumetric_average_3d(dm, dm%dpcc, fl%qx, bulk, SPACE_AVERAGE, bulk_var)
      end if
    end if

    return
  end subroutine measure_streamwise_bulk
!==============================================================================
  !> Reinitialise derived flow variables after restart data is loaded.
  !> - fl (inout): Flow state to refresh.
  !> - dm (in): Domain descriptor.
  subroutine restore_flow_variables_from_restart(fl, dm)
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use boundary_conditions_mod
    use find_max_min_ave_mod
    use mpi_mod
    use solver_tools_mod
    use wtformat_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    real(WP) :: ubulk
    real(WP) :: ubulk_new
    real(WP) :: scale
    real(WP) :: ubulk_tol
    real(WP) :: ubulk_min
    integer  :: istream
    logical  :: is_z_streamwise
    logical  :: is_reset_bulk
    logical  :: has_nonunit_ubulk
    character(2) :: bulk_var

    !--------------------------------------------------------------------------
    ! The streamwise direction is x, except for a duct, which is set up with z
    ! streamwise so that both walls fall in directions the Poisson solver treats
    ! as non-periodic.
    !--------------------------------------------------------------------------
    is_z_streamwise = (dm%icase == ICASE_DUCT)
    istream = 1
    if(is_z_streamwise) istream = 3

    call measure_streamwise_bulk(fl, dm, is_z_streamwise, bulk_var, ubulk)

    if(nrank == 0) then
        call Print_debug_inline_msg("The restarted bulk streamwise velocity/mass flux is:")
        write (*, wrtfmt1e) ' average['//trim(bulk_var)//']_[x,y,z]: ', ubulk
    end if

    !--------------------------------------------------------------------------
    ! Normalising the restored field to unit bulk is only meaningful when the
    ! streamwise direction is periodic, where the bulk is a free constant of the
    ! fully-developed state. With an inlet/outlet it must be refused:
    !   - the bulk is already fixed by the inlet boundary condition, not by the
    !     restored interior field;
    !   - the prescribed fbcx_q*/fbcx_g* inlet planes are not rescaled here, so
    !     scaling the interior alone would step the first interior node away
    !     from the inlet it has to match;
    !   - for a variable-property flow the projection constraint is
    !     div(g) = -d(rho)/dt, and div(c*g) = -c*d(rho)/dt for c /= 1, so the
    !     rescaled field is no longer a valid projection state. Isothermal flow
    !     escapes this only because div(q) = 0 is scale invariant.
    !--------------------------------------------------------------------------
    is_reset_bulk = dm%reset_unit_massflux
    if(dm%reset_unit_massflux .and. (.not. dm%is_periodic(istream))) then
      is_reset_bulk = .false.
      if(nrank == 0) call Print_warning_msg( &
        "reset_unit_massflux is ignored because the streamwise direction is not periodic: "// &
        "the bulk is set by the inlet boundary condition, and rescaling the interior alone "// &
        "would desynchronise it from the prescribed inlet planes.")
    end if

    ubulk_tol = max(1.0E-10_WP, TEN * MINP)
    ubulk_min = sqrt(tiny(ONE))
    has_nonunit_ubulk = (.not. ieee_is_finite(ubulk))
    if(.not. has_nonunit_ubulk) has_nonunit_ubulk = (abs(ubulk - ONE) > ubulk_tol)
    if(has_nonunit_ubulk .and. dm%is_periodic(istream)) then
      if(nrank == 0) then
        call Print_warning_msg("Restored bulk velocity/mass flux is not 1.0.")
        write (*, wrtfmt1e) ' restored ubulk: ', ubulk
      end if
      if(is_reset_bulk) then
        if((.not. ieee_is_finite(ubulk)) .or. abs(ubulk) <= ubulk_min) then
          call Print_error_msg("Cannot reset unit mass flux because restored ubulk is zero, non-finite, or too small.")
        end if
        scale = ONE / ubulk
        if(.not. ieee_is_finite(scale)) then
          call Print_error_msg("Cannot reset unit mass flux because the required scaling factor is non-finite.")
        end if

        !----------------------------------------------------------------------
        ! q and g are scaled by the same factor so that g = q*rho still holds
        ! pointwise; only the measured component decides the factor.
        !----------------------------------------------------------------------
        if(is_z_streamwise) then
          fl%qz = fl%qz * scale
          if(dm%is_thermo) fl%gz = fl%gz * scale
        else
          fl%qx = fl%qx * scale
          if(dm%is_thermo) fl%gx = fl%gx * scale
        end if
        call measure_streamwise_bulk(fl, dm, is_z_streamwise, bulk_var, ubulk_new)

        if(nrank == 0) then
          call Print_warning_msg("reset_unit_massflux = true; rescaled restored streamwise velocity.")
          write (*, wrtfmt1e) ' scale factor: ', scale
          write (*, wrtfmt1e) ' new ubulk: ', ubulk_new
        end if
        if((.not. ieee_is_finite(ubulk_new)) .or. abs(ubulk_new - ONE) > ubulk_tol) then
          call Print_error_msg("Unit mass-flux reset failed; recomputed ubulk is not close to 1.0.")
        end if
      else
        if(nrank == 0) call Print_warning_msg( &
          "Continuing without rescaling because reset_unit_massflux = false.")
      end if
    end if
    !------------------------------------------------------------------------------
    ! to check maximum velocity
    !------------------------------------------------------------------------------
    call Find_max_min_3d(fl%qx, opt_name="qx: ")
    call Find_max_min_3d(fl%qy, opt_name="qy: ")
    call Find_max_min_3d(fl%qz, opt_name="qz: ")
    !------------------------------------------------------------------------------
    ! to set up other parameters for flow only, which will be updated in thermo flow.
    !------------------------------------------------------------------------------
    fl%pcor(:, :, :) = ZERO
    fl%pcor_zpencil_ggg(:, :, :) = ZERO

    return
  end subroutine
!==============================================================================
!==============================================================================
  !> Read instantaneous thermal variables from restart files.
  !> - tm (inout): Thermal state receiving restart fields.
  !> - dm (inout): Domain descriptor.
  subroutine read_instantaneous_thermo(tm, fl, dm)
    use io_tools_mod
    use thermo_info_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm

    character(64):: data_flname
    character(64):: keyword
    real(WP) :: restart_dt
    logical  :: has_metadata
    real(WP), dimension(dm%d1cc%xsz(1), dm%d1cc%xsz(2), dm%d1cc%xsz(3)) :: fbcx_1cc
    logical  :: has_fbcx_dhat

    if (.not. dm%is_thermo) return
    if(nrank == 0) call Print_debug_inline_msg("read instantaneous thermo data ...")

    tm%iteration = tm%iterfrom
    ! the checkpoint's property_interp must match this run (stops otherwise)
    call check_thermo_interp_metadata(dm, tm%iterfrom)
    ! the thermal checkpoint must belong to the flow step it is restarted with (stops otherwise)
    call check_thermo_flow_iteration(dm, tm%iterfrom)
    call read_checkpoint_restart_metadata(dm%idom, 'thermo_meta', tm%iterfrom, tm%time, restart_dt, has_metadata)
    if(.not. has_metadata) then
      tm%time = real(tm%iterfrom, WP) * dm%dt
      if(nrank == 0) call Print_warning_msg("Thermo restart metadata was not found. " // &
        "The restart time is estimated as iterfrom * current dt.")
    else if(abs(restart_dt - dm%dt) > max(MINP, MINP * abs(restart_dt))) then
      if(nrank == 0) call Print_warning_msg("Current input dt differs from the thermo checkpoint dt. " // &
        "The stored restart time is preserved, and the input dt is used for the next steps.")
    end if

    if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
      if(nrank == 0) call Print_debug_mid_msg("Restart input layout expects bundled thermo restart files.")
      call read_thermo_restart_bundle(tm, fl, dm)
    else
      if(nrank == 0) call Print_debug_mid_msg("Restart input layout expects per-field thermo restart files.")
      call generate_pathfile_name(data_flname, dm%idom, 'rhoh', dir_data, 'bin', tm%iteration)
      if(.not. file_exists(trim(data_flname))) &
        call Print_error_msg("The file "//trim(data_flname)//" does not exist. "//THERMO_LABEL_HINT)
      call read_one_3d_array(tm%rhoh,  'rhoh', dm%idom, tm%iteration, dm%dccc)
      call read_one_3d_array(tm%tTemp, 'temp', dm%idom, tm%iteration, dm%dccc)
      call read_one_3d_array(tm%ene_rhs0, 'ene_rhs0', dm%idom, tm%iteration, dm%dccc)
      call read_one_3d_array(fl%dDens, 'dens', dm%idom, tm%iteration, dm%dccc)
      call read_one_3d_array(fl%mVisc, 'visc', dm%idom, tm%iteration, dm%dccc)
      call read_one_3d_array(tm%hEnth, 'henth', dm%idom, tm%iteration, dm%dccc)
      call read_one_3d_array(tm%kCond, 'kcond', dm%idom, tm%iteration, dm%dccc)
      call read_one_3d_array(tm%eCond, 'econd', dm%idom, tm%iteration, dm%dccc)
      if(is_bc_recovery) then
        call generate_pathfile_name(data_flname, dm%idom, 'dens_hat', dir_data, 'bin', tm%iteration)
        if(file_exists(trim(data_flname))) then
          call read_one_3d_array(fl%dDens_hat, 'dens_hat', dm%idom, tm%iteration, dm%dccc)
          call read_one_3d_array(fl%drhodt_m1, 'drhodt_m1', dm%idom, tm%iteration, dm%dccc)
          fl%is_bc_state_restored = .true.
        else if(nrank == 0) then
          call Print_warning_msg('LEGACY per-field thermal restart (no dens_hat) read with '// &
            'rhoh_recovery = transported: rho_hat = rho and a zero rho_t history are reconstructed; '// &
            'the continuation is NOT exact.')
        end if
      end if
      ! convective x-outlet: the thermal outlet state (as in the bundle). A per-field
      ! checkpoint written before these files existed is read as before, with a warning.
      if(dm%is_conv_outlet(1)) then
        call generate_pathfile_name(data_flname, dm%idom, 'fbcx_rhoh_rhs0', dir_data, 'bin', tm%iteration)
        if(file_exists(trim(data_flname))) then
          call read_one_3d_array(fbcx_1cc, 'fbcx_rhoh_rhs0', dm%idom, tm%iteration, dm%d1cc)
          tm%fbcx_rhoh_rhs0(:, :) = fbcx_1cc(1, :, :)
          call generate_pathfile_name(data_flname, dm%idom, 'fbcx_dhat', dir_data, 'bin', tm%iteration)
          ! explicit control flow: the file query must not depend on short-circuit evaluation
          has_fbcx_dhat = file_exists(trim(data_flname))
          if(is_bc_recovery .and. fl%is_bc_state_restored .and. has_fbcx_dhat) then
            call read_one_3d_array(tm%fbcx_dhat, 'fbcx_dhat', dm%idom, tm%iteration, dm%d4cc)
            call read_one_3d_array(fbcx_1cc, 'fbcx_dhat_rhs0', dm%idom, tm%iteration, dm%d1cc)
            tm%fbcx_dhat_rhs0(:, :) = fbcx_1cc(1, :, :)
            tm%is_fbcx_dhat_valid = .true.
          else if(is_bc_recovery .and. fl%is_bc_state_restored .and. nrank == 0) then
            call Print_warning_msg('Per-field thermal restart without fbcx_dhat: the outlet '// &
              'transported density is reconstructed; the continuation is NOT exact.')
          end if
        else if(nrank == 0) then
          call Print_warning_msg('Per-field thermal restart without the convective-outlet state '// &
            '(fbcx_rhoh_rhs0): the thermal outlet restarts from its initial state; the continuation is NOT exact.')
        end if
      end if
    end if


    if(nrank == 0) call Print_debug_end_msg()
    return
  end subroutine

!==============================================================================
!==============================================================================
  !> Write scalar metadata needed to restart with the correct simulation time.
  !> - idom (in): Domain index.
  !> - keyword (in): Metadata file keyword.
  !> - iter (in): Checkpoint iteration.
  !> - time (in): Stored nondimensional solver time.
  !> - dt (in): Time step used when the checkpoint was written.
  subroutine write_restart_metadata(idom, keyword, iter, time, dt)
    implicit none
    integer,      intent(in) :: idom
    integer,      intent(in) :: iter
    character(*), intent(in) :: keyword
    real(WP),     intent(in) :: time
    real(WP),     intent(in) :: dt

    character(128) :: meta_file
    integer :: io_unit
    integer :: ios

    if(nrank /= 0) return

    call generate_pathfile_name(meta_file, idom, trim(keyword), dir_data, 'dat', iter)
    open(newunit=io_unit, file=trim(meta_file), status='replace', action='write', iostat=ios)
    if(ios /= 0) then
      call Print_warning_msg("Could not write restart metadata file: "//trim(meta_file))
      return
    end if

    write(io_unit, '(A,1X,I0)')       'iteration', iter
    write(io_unit, '(A,1X,ES24.16E3)') 'time',      time
    write(io_unit, '(A,1X,ES24.16E3)') 'dt',        dt
    close(io_unit)

    return
  end subroutine

!==============================================================================
!==============================================================================
  !> Read scalar metadata written alongside a restart checkpoint.
  !> - idom (in): Domain index.
  !> - keyword (in): Metadata file keyword.
  !> - iter (in): Expected checkpoint iteration.
  !> - time (out): Stored nondimensional solver time.
  !> - dt (out): Checkpoint time step.
  !> - found (out): True when a matching metadata file was read.
  subroutine read_restart_metadata(idom, keyword, iter, time, dt, found)
    implicit none
    integer,      intent(in)  :: idom
    integer,      intent(in)  :: iter
    character(*), intent(in)  :: keyword
    real(WP),     intent(out) :: time
    real(WP),     intent(out) :: dt
    logical,      intent(out) :: found

    character(128) :: meta_file
    character(32)  :: label
    integer :: io_unit
    integer :: iter_file
    integer :: ios
    logical :: file_exists

    time = ZERO
    dt = ZERO
    found = .false.

    call generate_pathfile_name(meta_file, idom, trim(keyword), dir_data, 'dat', iter)
    inquire(file=trim(meta_file), exist=file_exists)
    if(.not. file_exists) return

    open(newunit=io_unit, file=trim(meta_file), status='old', action='read', iostat=ios)
    if(ios /= 0) return

    read(io_unit, *, iostat=ios) label, iter_file
    if(ios == 0) read(io_unit, *, iostat=ios) label, time
    if(ios == 0) read(io_unit, *, iostat=ios) label, dt
    close(io_unit)

    found = (ios == 0 .and. iter_file == iter)
    if(.not. found .and. nrank == 0) call Print_warning_msg("Restart metadata file is invalid: "//trim(meta_file))

    return
  end subroutine
!==============================================================================
  !> Reinitialise thermal-property and conservative variables after restart.
  !> - fl (inout): Flow state receiving refreshed density/viscosity variables.
  !> - tm (inout): Thermal state used to update properties.
  !> - dm (inout): Domain descriptor.
  subroutine restore_thermo_variables_from_restart(fl, tm, dm)
    use eq_energy_mod
    use solver_tools_mod
    use thermo_info_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    type(t_flow),   intent(inout) :: fl
    type(t_thermo), intent(inout) :: tm

    if (.not. dm%is_thermo) return

    !--------------------------------------------------------------------------
    ! rho, mu, T, h, k and sigma_e are all part of the checkpoint, in both
    ! restart layouts - a thermal run always stores the full history
    ! (Read_input_parameters rejects compact). Update_thermal_
    ! properties is still needed, but only for its second half: it is what
    ! rebuilds the Neumann fbc*_ftp boundary states from rhoh, and those are
    ! not stored. Its first half re-derives the interior properties from
    ! (rhoh, rho) through the property table, and that inverse lookup does not
    ! reproduce the stored values bit-for-bit, so a restart taking them would
    ! not continue the written trajectory. Keep the stored fields.
    !
    ! This used to be done for the bundled layout only, which made a restart
    ! depend on how the checkpoint happened to be laid out on disk. Both
    ! layouts now behave identically.
    !--------------------------------------------------------------------------
    call Rebuild_thermal_boundary_states(fl, tm, dm)

    fl%dDens0(:, :, :) = fl%dDens(:, :, :)

    return
  end subroutine

!==============================================================================
  function xoutlet_database_fields(dm) result(fields)
    implicit none
    type(t_domain), intent(in) :: dm
    character(256) :: fields

    write(fields, '(A,I0,A)') 'ndbbuf=', dm%ndbbuf, &
      ';outlet1_qx:dxcc,outlet2_qx:dxcc,'// &
      'outlet1_qy:dxpc,outlet2_qy:dxpc,'// &
      'outlet1_qz:dxcp,outlet2_qz:dxcp'

    return
  end function xoutlet_database_fields
!==============================================================================
  function xoutlet_database_file_iter(dm, iter) result(file_iter)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer :: file_iter

    file_iter = iter + dm%ndb_file_offset

    return
  end function xoutlet_database_file_iter
!==============================================================================
  subroutine remove_xoutlet_per_field_file(dm, keyword, iter)
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: keyword
    integer, intent(in) :: iter

    character(256) :: stale_file

    call generate_pathfile_name(stale_file, dm%idom, trim(keyword), dir_data, 'bin', iter)
    call remove_output_file_if_overwrite(stale_file, dm%existing_output_policy)

    return
  end subroutine remove_xoutlet_per_field_file
!==============================================================================
  subroutine cleanup_xoutlet_database_per_field_files(dm, iter)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter

    call remove_xoutlet_per_field_file(dm, 'outlet1_qx', iter)
    call remove_xoutlet_per_field_file(dm, 'outlet2_qx', iter)
    call remove_xoutlet_per_field_file(dm, 'outlet1_qy', iter)
    call remove_xoutlet_per_field_file(dm, 'outlet2_qy', iter)
    call remove_xoutlet_per_field_file(dm, 'outlet1_qz', iter)
    call remove_xoutlet_per_field_file(dm, 'outlet2_qz', iter)
    call remove_xoutlet_per_field_file(dm, 'outlet1_pr', iter)
    call remove_xoutlet_per_field_file(dm, 'outlet2_pr', iter)

    return
  end subroutine cleanup_xoutlet_database_per_field_files
!==============================================================================
  subroutine cleanup_xoutlet_database_bundle_files(dm, iter)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter

    character(256) :: stale_file

    call generate_pathfile_name(stale_file, dm%idom, 'xoutlet_database', dir_data, 'bin', iter)
    call remove_output_file_if_overwrite(stale_file, dm%existing_output_policy)
    call generate_pathfile_name(stale_file, dm%idom, 'xoutlet_database_meta', dir_data, 'dat', iter)
    call remove_output_file_if_overwrite(stale_file, dm%existing_output_policy)

    return
  end subroutine cleanup_xoutlet_database_bundle_files
!==============================================================================
  subroutine write_xoutlet_database_bundle(dm, iter)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    character(256) :: output_files(2)
    integer :: db_iter
    integer :: iter_start, iter_end
    real(WP) :: time_start, time_end
    logical :: do_write

    call generate_pathfile_name(output_files(1), dm%idom, 'xoutlet_database', dir_data, 'bin', iter)
    call generate_pathfile_name(output_files(2), dm%idom, 'xoutlet_database_meta', dir_data, 'dat', iter)
    bundle_file = output_files(1)
    call prepare_output_file_set(output_files, dm%existing_output_policy, 'x-outlet database bundle', do_write)
    if(.not. do_write) return
    call cleanup_xoutlet_database_per_field_files(dm, iter)
    if(nrank == 0) call Print_debug_mid_msg("Writing "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_write_mode)
    call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qx_outl1, opt_decomp=dm%dxcc)
    call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qx_outl2, opt_decomp=dm%dxcc)
    call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qy_outl1, opt_decomp=dm%dxpc)
    call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qy_outl2, opt_decomp=dm%dxpc)
    call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qz_outl1, opt_decomp=dm%dxcp)
    call decomp_2d_write_var(io, IPENCIL(1), dm%fbcx_qz_outl2, opt_decomp=dm%dxcp)
    call io%close()

    db_iter = iter - dm%ndb_file_offset
    iter_start = dm%ndbstart + db_iter
    iter_end = iter_start + dm%ndbbuf - 1
    time_start = real(iter_start, WP) * dm%dt
    time_end = real(iter_end, WP) * dm%dt
    call write_bundle_metadata(dm, 'xoutlet_database', iter, &
      xoutlet_database_fields(dm), xoutlet_database_shapes(dm), time_start, dm%dt, &
      opt_iter_start=iter_start, opt_iter_end=iter_end, opt_time_end=time_end)

    return
  end subroutine write_xoutlet_database_bundle
!==============================================================================
  subroutine read_xoutlet_database_bundle(dm, iter)
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: iter

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    integer :: nc_src(3)
    integer :: np_src(3)
    integer :: ndbbuf_src

    call generate_pathfile_name(bundle_file, dm%idom, 'xoutlet_database', dir_data, 'bin', iter)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The file "//trim(bundle_file)//" does not exist.")
    call read_xoutlet_bundle_metadata(dm, iter, nc_src, np_src, ndbbuf_src)
    if(ndbbuf_src /= dm%ndbbuf) &
    call Print_error_msg("Bundled inlet database ndbbuf differs from current replay buffer.")
    if(nc_src(2) /= dm%nc(2) .or. nc_src(3) /= dm%nc(3) .or. &
       np_src(2) /= dm%np(2) .or. np_src(3) /= dm%np(3)) &
    call Print_error_msg("Bundled inlet database cross-section mesh differs from direct-read mesh.")
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_read_mode)
    call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qx_inl1, opt_decomp=dm%dxcc)
    call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qx_inl2, opt_decomp=dm%dxcc)
    call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qy_inl1, opt_decomp=dm%dxpc)
    call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qy_inl2, opt_decomp=dm%dxpc)
    call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qz_inl1, opt_decomp=dm%dxcp)
    call decomp_2d_read_var(io, IPENCIL(1), dm%fbcx_qz_inl2, opt_decomp=dm%dxcp)
    call io%close()

    return
  end subroutine read_xoutlet_database_bundle
!==============================================================================
  subroutine read_xoutlet_bundle_metadata(dm, iter, nc_src, np_src, ndbbuf_src)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer, intent(out) :: nc_src(3)
    integer, intent(out) :: np_src(3)
    integer, intent(out) :: ndbbuf_src

    character(256) :: meta_file
    character(1024) :: line, value
    character(512) :: expected_shapes
    integer :: u, ioerr, iter_read

    call generate_pathfile_name(meta_file, dm%idom, 'xoutlet_database_meta', dir_data, 'dat', iter)
    if(.not. file_exists(trim(meta_file))) &
    call Print_error_msg("The bundle metadata file "//trim(meta_file)//" does not exist.")

    open(newunit=u, file=trim(meta_file), status='old', action='read', iostat=ioerr)
    if(ioerr /= 0) call Print_error_msg("Failed to open bundle metadata file "//trim(meta_file))

    read(u, '(A)', iostat=ioerr) line
    close(u)
    if(ioerr /= 0 .or. trim(line) /= 'CHAPSim_bundle_v2') &
    call Print_error_msg("Unsupported xoutlet database metadata format in "//trim(meta_file))

    value = metadata_value(meta_file, 'group')
    if(trim(value) /= 'xoutlet_database') call Print_error_msg("Bundle group mismatch in "//trim(meta_file))

    value = metadata_value(meta_file, 'iter')
    call parse_metadata_int(value, iter_read, 'iter', meta_file)
    if(iter_read /= iter) call Print_error_msg("Bundle iteration mismatch in "//trim(meta_file))

    value = metadata_value(meta_file, 'layout')
    if(trim(value) /= 'bundled') call Print_error_msg("Bundle layout mismatch in "//trim(meta_file))

    value = metadata_value(meta_file, 'fields')
    if(trim(value) /= trim(xoutlet_database_fields(dm))) then
      write(*, '(A)') "Expected fields: "//trim(xoutlet_database_fields(dm))
      write(*, '(A)') "Found fields:    "//trim(value)
      call Print_error_msg("Bundle field list mismatch in "//trim(meta_file))
    end if

    value = metadata_value(meta_file, 'nc')
    call parse_metadata_int3(value, nc_src, 'nc', meta_file)
    value = metadata_value(meta_file, 'np')
    call parse_metadata_int3(value, np_src, 'np', meta_file)
    value = metadata_value(meta_file, 'ndbbuf')
    call parse_metadata_int(value, ndbbuf_src, 'ndbbuf', meta_file)
    value = metadata_value(meta_file, 'field_shapes')
    expected_shapes = xoutlet_database_shapes_from_dims(ndbbuf_src, nc_src(2), nc_src(3), np_src(2), np_src(3))
    if(trim(value) /= trim(expected_shapes)) &
    call Print_error_msg("Bundle field-shape mismatch in "//trim(meta_file))
    ! the .bin must hold exactly the six planes the validated shapes list (a short
    ! file would otherwise be replayed with stale or zero planes); collective
    call check_bundle_file_size(dm, 'xoutlet_database', iter, expected_shapes, meta_file)

    return
  end subroutine read_xoutlet_bundle_metadata
!==============================================================================
  subroutine configure_xinlet_database_mesh(dm, nc_src, np_src)
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: nc_src(2)
    integer, intent(in) :: np_src(2)

    dm%xinlet_database_nc(:) = nc_src(:)
    dm%xinlet_database_np(:) = np_src(:)
    dm%xinlet_database_interp_yz = &
      nc_src(1) /= dm%nc(2) .or. nc_src(2) /= dm%nc(3) .or. &
      np_src(1) /= dm%np(2) .or. np_src(2) /= dm%np(3)

    if(dm%xinlet_database_interp_yz) then
      call setup_xinlet_database_source_mesh(dm, nc_src, np_src)
      if(nrank == 0 .and. .not. dm%xinlet_database_warning_issued) then
        call Print_warning_msg("Inlet database cross-section mesh differs from the main domain. " // &
          "The same mesh is recommended; inlet interpolation may reduce accuracy.")
        dm%xinlet_database_warning_issued = .true.
      end if
    end if

    dm%xinlet_database_checked = .true.

    return
  end subroutine configure_xinlet_database_mesh
!==============================================================================
  subroutine infer_xoutlet_database_mesh_from_bundle(dm, iter, nc_src, np_src)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer, intent(out) :: nc_src(2)
    integer, intent(out) :: np_src(2)

    integer :: nc_meta(3)
    integer :: np_meta(3)
    integer :: ndbbuf_src

    call read_xoutlet_bundle_metadata(dm, iter, nc_meta, np_meta, ndbbuf_src)
    if(ndbbuf_src /= dm%ndbbuf) &
    call Print_error_msg("Bundled inlet database ndbbuf differs from current replay buffer.")

    nc_src(1) = nc_meta(2)
    nc_src(2) = nc_meta(3)
    np_src(1) = np_meta(2)
    np_src(2) = np_meta(3)

    return
  end subroutine infer_xoutlet_database_mesh_from_bundle
!==============================================================================
  subroutine read_xoutlet_database_bundle_interp(dm, iter)
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: iter

    type(d2d_io_mpi) :: io
    character(256) :: bundle_file
    integer :: nc_src(2)
    integer :: np_src(2)

    call infer_xoutlet_database_mesh_from_bundle(dm, iter, nc_src, np_src)
    if(dm%ndb_sampling_dt > ZERO) call check_xinlet_database_dt(dm, iter)
    if(dm%xinlet_database_checked) then
      if(any(nc_src /= dm%xinlet_database_nc) .or. any(np_src /= dm%xinlet_database_np)) &
      call Print_error_msg("Bundled inlet database mesh changes between database files.")
    else
      call configure_xinlet_database_mesh(dm, nc_src, np_src)
    end if

    if(.not. dm%xinlet_database_interp_yz) then
      call read_xoutlet_database_bundle(dm, iter)
      return
    end if

    call generate_pathfile_name(bundle_file, dm%idom, 'xoutlet_database', dir_data, 'bin', iter)
    if(.not. file_exists(trim(bundle_file))) &
    call Print_error_msg("The file "//trim(bundle_file)//" does not exist.")
    if(nrank == 0) call Print_debug_inline_msg("Reading "//trim(bundle_file))

    call io%open(trim(bundle_file), decomp_2d_read_mode)
    call read_interp_xinlet_database_bundle_field(dm, io, &
      dm%dxcc_inl_src, dm%dxcc, dm%fbcx_qx_inl1, 1, 1)
    call read_interp_xinlet_database_bundle_field(dm, io, &
      dm%dxcc_inl_src, dm%dxcc, dm%fbcx_qx_inl2, 1, 1)
    call read_interp_xinlet_database_bundle_field(dm, io, &
      dm%dxpc_inl_src, dm%dxpc, dm%fbcx_qy_inl1, 2, 1)
    call read_interp_xinlet_database_bundle_field(dm, io, &
      dm%dxpc_inl_src, dm%dxpc, dm%fbcx_qy_inl2, 2, 1)
    call read_interp_xinlet_database_bundle_field(dm, io, &
      dm%dxcp_inl_src, dm%dxcp, dm%fbcx_qz_inl1, 1, 2)
    call read_interp_xinlet_database_bundle_field(dm, io, &
      dm%dxcp_inl_src, dm%dxcp, dm%fbcx_qz_inl2, 1, 2)
    call io%close()

    return
  end subroutine read_xoutlet_database_bundle_interp
!==============================================================================
  subroutine read_interp_xinlet_database_bundle_field(dm, io, dsrc, dtgt, ftgt, yloc, zloc)
    use decomp_2d, only: alloc_x
    implicit none
    type(t_domain), intent(in) :: dm
    type(d2d_io_mpi), intent(inout) :: io
    type(DECOMP_INFO), intent(in) :: dsrc
    type(DECOMP_INFO), intent(in) :: dtgt
    real(WP), intent(inout) :: ftgt(:, :, :)
    integer, intent(in) :: yloc
    integer, intent(in) :: zloc

    real(WP), allocatable :: fsrc(:, :, :)

    call alloc_x(fsrc, dsrc)
    call decomp_2d_read_var(io, IPENCIL(1), fsrc, opt_decomp=dsrc)
    call interp_xinlet_database_array_yz(dm, fsrc, ftgt, dsrc, dtgt, yloc, zloc)
    deallocate(fsrc)

    return
  end subroutine read_interp_xinlet_database_bundle_field
!==============================================================================
  function xoutlet_database_file_elements(dm, keyword, iter) result(nelems)
    use iso_fortran_env, only: int64
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: keyword
    integer, intent(in) :: iter
    integer(int64) :: nelems

    character(256) :: data_file
    integer(int64) :: file_bytes
    integer(int64) :: elem_bytes
    real(WP) :: sample

    call generate_pathfile_name(data_file, dm%idom, trim(keyword), dir_data, 'bin', iter)
    if(.not. file_exists(trim(data_file))) &
    call Print_error_msg("The file "//trim(data_file)//" does not exist.")

    inquire(file=trim(data_file), size=file_bytes)
    elem_bytes = int(storage_size(sample), int64) / 8_int64
    if(elem_bytes <= 0_int64 .or. mod(file_bytes, elem_bytes) /= 0_int64) &
    call Print_error_msg("Invalid binary size for inlet database file "//trim(data_file))
    nelems = file_bytes / elem_bytes

    return
  end function xoutlet_database_file_elements
!==============================================================================
  subroutine infer_xoutlet_database_mesh(dm, iter, nc_src, np_src)
    use iso_fortran_env, only: int64
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter
    integer, intent(out) :: nc_src(2)
    integer, intent(out) :: np_src(2)

    integer(int64) :: nbuf
    integer(int64) :: ncc, npc, ncp
    integer :: ny, nz
    integer :: dy_face_extra, dz_face_extra
    character(256) :: msg

    nbuf = int(dm%ndbbuf, int64)
    ncc = xoutlet_database_file_elements(dm, 'outlet1_qx', iter)
    npc = xoutlet_database_file_elements(dm, 'outlet1_qy', iter)
    ncp = xoutlet_database_file_elements(dm, 'outlet1_qz', iter)

    if(mod(ncc, nbuf) /= 0_int64 .or. mod(npc, nbuf) /= 0_int64 .or. &
       mod(ncp, nbuf) /= 0_int64) then
      call Print_error_msg("Inlet database file sizes are not divisible by ndbbuf.")
    end if
    ncc = ncc / nbuf
    npc = npc / nbuf
    ncp = ncp / nbuf

    dy_face_extra = 1
    dz_face_extra = 1
    if(dm%is_periodic(2)) dy_face_extra = 0
    if(dm%is_periodic(3)) dz_face_extra = 0

    ny = 0
    nz = 0
    if(dy_face_extra == 1) then
      nz = int(npc - ncc)
      if(nz > 0 .and. mod(ncc, int(nz, int64)) == 0_int64) ny = int(ncc / int(nz, int64))
    else if(dz_face_extra == 1) then
      ny = int(ncp - ncc)
      if(ny > 0 .and. mod(ncc, int(ny, int64)) == 0_int64) nz = int(ncc / int(ny, int64))
    else if(ncc == int(dm%nc(2), int64) * int(dm%nc(3), int64)) then
      ny = dm%nc(2)
      nz = dm%nc(3)
    end if

    if(ny <= 0 .or. nz <= 0) then
      write(msg, '(A,I0,A,I0,A,I0,A)') &
        "Cannot infer mismatched inlet database cross-section mesh from file sizes: qx=", &
        ncc, ", qy=", npc, ", qz=", ncp, ". Use a non-periodic y or z direction, or matching mesh."
      call Print_error_msg(trim(msg))
    end if

    if(ncc /= int(ny, int64) * int(nz, int64) .or. &
       npc /= int(ny + dy_face_extra, int64) * int(nz, int64) .or. &
       ncp /= int(ny, int64) * int(nz + dz_face_extra, int64)) then
      call Print_error_msg("Inconsistent qx/qy/qz inlet database sizes; cannot infer one source mesh.")
    end if

    nc_src(1) = ny
    nc_src(2) = nz
    np_src(1) = ny + dy_face_extra
    np_src(2) = nz + dz_face_extra

    return
  end subroutine infer_xoutlet_database_mesh
!==============================================================================
  subroutine setup_xinlet_database_source_mesh(dm, nc_src, np_src)
    use decomp_2d, only: decomp_info_init
    use geometry_mod, only: Buildup_geometry_mesh_info
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: nc_src(2)
    integer, intent(in) :: np_src(2)

    type(t_domain) :: dm_src

    call decomp_info_init(dm%ndbbuf, nc_src(1), nc_src(2), dm%dxcc_inl_src)
    call decomp_info_init(dm%ndbbuf, np_src(1), nc_src(2), dm%dxpc_inl_src)
    call decomp_info_init(dm%ndbbuf, nc_src(1), np_src(2), dm%dxcp_inl_src)

    dm_src%idom = dm%idom
    dm_src%icase = dm%icase
    dm_src%icoordinate = dm%icoordinate
    dm_src%is_periodic(:) = dm%is_periodic(:)
    dm_src%is_stretching(:) = dm%is_stretching(:)
    dm_src%is_thermo = dm%is_thermo
    dm_src%ibcx_nominal(:, :) = dm%ibcx_nominal(:, :)
    dm_src%ibcz_nominal(:, :) = dm%ibcz_nominal(:, :)
    dm_src%lxx = dm%lxx
    dm_src%lyt = dm%lyt
    dm_src%lyb = dm%lyb
    dm_src%lzz = dm%lzz
    dm_src%istret = dm%istret
    dm_src%mstret = dm%mstret
    dm_src%rstret = dm%rstret
    dm_src%nc(:) = dm%nc(:)
    dm_src%nc(2) = nc_src(1)
    dm_src%nc(3) = nc_src(2)
    if(allocated(dm_src%yp)) deallocate(dm_src%yp)
    if(allocated(dm_src%yc)) deallocate(dm_src%yc)
    if(allocated(dm_src%yMappingpt)) deallocate(dm_src%yMappingpt)
    if(allocated(dm_src%yMappingcc)) deallocate(dm_src%yMappingcc)
    if(allocated(dm_src%rpi)) deallocate(dm_src%rpi)
    if(allocated(dm_src%rci)) deallocate(dm_src%rci)
    if(allocated(dm_src%rp)) deallocate(dm_src%rp)
    if(allocated(dm_src%rc)) deallocate(dm_src%rc)
    if(allocated(dm_src%knc_sym)) deallocate(dm_src%knc_sym)
    if(allocated(dm_src%xdamping)) deallocate(dm_src%xdamping)
    if(allocated(dm_src%zdamping)) deallocate(dm_src%zdamping)
    call Buildup_geometry_mesh_info(dm_src)

    if(allocated(dm%xinlet_database_yp_src)) deallocate(dm%xinlet_database_yp_src)
    if(allocated(dm%xinlet_database_yc_src)) deallocate(dm%xinlet_database_yc_src)
    allocate(dm%xinlet_database_yp_src(dm_src%np_geo(2)))
    allocate(dm%xinlet_database_yc_src(dm_src%nc(2)))
    dm%xinlet_database_yp_src(:) = dm_src%yp(:)
    dm%xinlet_database_yc_src(:) = dm_src%yc(:)

    if(allocated(dm_src%yp)) deallocate(dm_src%yp)
    if(allocated(dm_src%yc)) deallocate(dm_src%yc)
    if(allocated(dm_src%yMappingpt)) deallocate(dm_src%yMappingpt)
    if(allocated(dm_src%yMappingcc)) deallocate(dm_src%yMappingcc)
    if(allocated(dm_src%rpi)) deallocate(dm_src%rpi)
    if(allocated(dm_src%rci)) deallocate(dm_src%rci)
    if(allocated(dm_src%rp)) deallocate(dm_src%rp)
    if(allocated(dm_src%rc)) deallocate(dm_src%rc)
    if(allocated(dm_src%knc_sym)) deallocate(dm_src%knc_sym)
    if(allocated(dm_src%xdamping)) deallocate(dm_src%xdamping)
    if(allocated(dm_src%zdamping)) deallocate(dm_src%zdamping)

    return
  end subroutine setup_xinlet_database_source_mesh
!==============================================================================
  subroutine ensure_xinlet_database_mesh(dm, iter)
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: iter

    integer :: nc_src(2)
    integer :: np_src(2)

    if(dm%xinlet_database_checked) return

    call infer_xoutlet_database_mesh(dm, iter, nc_src, np_src)
    call configure_xinlet_database_mesh(dm, nc_src, np_src)

    return
  end subroutine ensure_xinlet_database_mesh
!==============================================================================
  subroutine read_xoutlet_database_per_field_interp(dm, iter)
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: iter

    call ensure_xinlet_database_mesh(dm, iter)
    if(dm%ndb_sampling_dt > ZERO .and. .not. dm%xinlet_ndb_dt_warned) then
      if(nrank == 0) call Print_warning_msg("Per-field inlet database files carry no metadata: "// &
        "ndb_sampling_dt cannot be checked against the recorded sampling interval.")
      dm%xinlet_ndb_dt_warned = .true.
    end if

    if(.not. dm%xinlet_database_interp_yz) then
      call read_one_3d_array(dm%fbcx_qx_inl1, 'outlet1_qx', dm%idom, iter, dm%dxcc)
      call read_one_3d_array(dm%fbcx_qx_inl2, 'outlet2_qx', dm%idom, iter, dm%dxcc)
      call read_one_3d_array(dm%fbcx_qy_inl1, 'outlet1_qy', dm%idom, iter, dm%dxpc)
      call read_one_3d_array(dm%fbcx_qy_inl2, 'outlet2_qy', dm%idom, iter, dm%dxpc)
      call read_one_3d_array(dm%fbcx_qz_inl1, 'outlet1_qz', dm%idom, iter, dm%dxcp)
      call read_one_3d_array(dm%fbcx_qz_inl2, 'outlet2_qz', dm%idom, iter, dm%dxcp)
      return
    end if

    call read_interp_xinlet_database_field(dm, 'outlet1_qx', iter, &
      dm%dxcc_inl_src, dm%dxcc, dm%fbcx_qx_inl1, 1, 1)
    call read_interp_xinlet_database_field(dm, 'outlet2_qx', iter, &
      dm%dxcc_inl_src, dm%dxcc, dm%fbcx_qx_inl2, 1, 1)
    call read_interp_xinlet_database_field(dm, 'outlet1_qy', iter, &
      dm%dxpc_inl_src, dm%dxpc, dm%fbcx_qy_inl1, 2, 1)
    call read_interp_xinlet_database_field(dm, 'outlet2_qy', iter, &
      dm%dxpc_inl_src, dm%dxpc, dm%fbcx_qy_inl2, 2, 1)
    call read_interp_xinlet_database_field(dm, 'outlet1_qz', iter, &
      dm%dxcp_inl_src, dm%dxcp, dm%fbcx_qz_inl1, 1, 2)
    call read_interp_xinlet_database_field(dm, 'outlet2_qz', iter, &
      dm%dxcp_inl_src, dm%dxcp, dm%fbcx_qz_inl2, 1, 2)

    return
  end subroutine read_xoutlet_database_per_field_interp
!==============================================================================
  subroutine read_interp_xinlet_database_field(dm, keyword, iter, dsrc, dtgt, ftgt, yloc, zloc)
    use decomp_2d, only: alloc_x
    implicit none
    type(t_domain), intent(in) :: dm
    character(*), intent(in) :: keyword
    integer, intent(in) :: iter
    type(DECOMP_INFO), intent(in) :: dsrc
    type(DECOMP_INFO), intent(in) :: dtgt
    real(WP), intent(inout) :: ftgt(:, :, :)
    integer, intent(in) :: yloc
    integer, intent(in) :: zloc

    real(WP), allocatable :: fsrc(:, :, :)

    call alloc_x(fsrc, dsrc)
    call read_one_3d_array(fsrc, keyword, dm%idom, iter, dsrc)
    call interp_xinlet_database_array_yz(dm, fsrc, ftgt, dsrc, dtgt, yloc, zloc)
    deallocate(fsrc)

    return
  end subroutine read_interp_xinlet_database_field
!==============================================================================
  subroutine fill_xinlet_z_coords(n, h, loc, z)
    implicit none
    integer, intent(in) :: n
    real(WP), intent(in) :: h
    integer, intent(in) :: loc
    real(WP), intent(out) :: z(n)

    integer :: k

    do k = 1, n
      z(k) = get_xinlet_z_coord(k, h, loc)
    end do

    return
  end subroutine fill_xinlet_z_coords
!==============================================================================
  pure function get_xinlet_z_coord(idx, h, loc) result(z)
    implicit none
    integer, intent(in) :: idx
    real(WP), intent(in) :: h
    integer, intent(in) :: loc
    real(WP) :: z

    if(loc == 1) then
      z = h * (real(idx - 1, WP) + HALF)
    else
      z = h * real(idx - 1, WP)
    end if

    return
  end function get_xinlet_z_coord
!==============================================================================
  !> Source z coordinates used for the inlet database search. In a periodic z
  !> direction the array is extended by one periodic image at each end,
  !>   zsrc_ext = [ zsrc(n) - lzz, zsrc(1:n), zsrc(1) + lzz ],
  !> so that targets between zsrc(n) and lzz (or below zsrc(1)) are bracketed
  !> by the source points across the seam instead of being clamped to the end
  !> value. Bracket index e in zsrc_ext then maps to global source index
  !> e + kshift with kshift = -1 (global 0 and n+1 are the periodic halo cells
  !> filled by update_halo). Non-periodic z: zsrc_ext = zsrc, kshift = 0, i.e.
  !> unchanged behaviour. Interior targets get the same bracket and weights as
  !> without the extension.
  subroutine build_xinlet_z_search_coords(dm, zsrc, zsrc_ext, kshift)
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(in) :: zsrc(:)
    real(WP), allocatable, intent(out) :: zsrc_ext(:)
    integer, intent(out) :: kshift

    integer :: n

    n = size(zsrc)
    if(dm%is_periodic(3)) then
      allocate(zsrc_ext(n + 2))
      zsrc_ext(1) = zsrc(n) - dm%lzz
      zsrc_ext(2:n + 1) = zsrc(1:n)
      zsrc_ext(n + 2) = zsrc(1) + dm%lzz
      kshift = -1
    else
      allocate(zsrc_ext(n))
      zsrc_ext(:) = zsrc(:)
      kshift = 0
    end if

    return
  end subroutine build_xinlet_z_search_coords
!==============================================================================
  subroutine compute_xinlet_database_halo_level(dm, dsrc, dtgt, ysrc, zsrc, yloc, zloc, halo_level)
    use mpi_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dsrc
    type(DECOMP_INFO), intent(in) :: dtgt
    real(WP), intent(in) :: ysrc(:)
    real(WP), intent(in) :: zsrc(:)
    integer, intent(in) :: yloc
    integer, intent(in) :: zloc
    integer, intent(out) :: halo_level

    integer :: j, k, jj, kk
    integer :: jsrc, ksrc
    integer :: min_j, max_j, min_k, max_k
    integer :: halo_local
    integer :: kshift
    real(WP) :: y_target, z_target
    real(WP), allocatable :: zsrc_ext(:)

    call build_xinlet_z_search_coords(dm, zsrc, zsrc_ext, kshift)
    min_j = huge(1)
    max_j = -huge(1)
    min_k = huge(1)
    max_k = -huge(1)

    do k = 1, dtgt%xsz(3)
      kk = dtgt%xst(3) + k - 1
      z_target = get_xinlet_z_coord(kk, dm%h(3), zloc)
      do j = 1, dtgt%xsz(2)
        jj = dtgt%xst(2) + j - 1
        if(yloc == 1) then
          y_target = dm%yc(jj)
        else
          y_target = dm%yp(jj)
        end if
        call binary_search_loc2index_yz(y_target, ysrc, jsrc)
        call binary_search_loc2index_yz(z_target, zsrc_ext, ksrc)
        jsrc = min(jsrc, size(ysrc) - 1)
        ksrc = min(ksrc, size(zsrc_ext) - 1) + kshift   ! global source index (0..n in periodic z)
        min_j = min(min_j, jsrc)
        max_j = max(max_j, jsrc + 1)
        min_k = min(min_k, ksrc)
        max_k = max(max_k, ksrc + 1)
      end do
    end do

    halo_local = 1
    halo_local = max(halo_local, dsrc%xst(2) - min_j)
    halo_local = max(halo_local, max_j - dsrc%xen(2))
    halo_local = max(halo_local, dsrc%xst(3) - min_k)
    halo_local = max(halo_local, max_k - dsrc%xen(3))

    call mpi_allreduce(halo_local, halo_level, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierror)
    halo_level = max(1, halo_level)
    deallocate(zsrc_ext)

    call check_xinlet_database_halo_layout(dm, dsrc, &
      (/ max(0, dsrc%xst(2) - min_j), max(0, max_j - dsrc%xen(2)), &
         max(0, dsrc%xst(3) - min_k), max(0, max_k - dsrc%xen(3)) /), yloc, zloc)

    return
  end subroutine compute_xinlet_database_halo_level
!==============================================================================
  !> Guard for the halo exchange used by the inlet interpolation. update_halo
  !> (2decomp) fills halo layer m on each side only from the adjacent rank, so a
  !> layer is valid only if m does not exceed that neighbour's local extent of the
  !> source block; deeper layers are not filled reliably. need(1:4) are the
  !> source layers this rank reads below/above its block in y and z (global
  !> indices of the bracketing source points, see compute_xinlet_database_halo_level).
  !> The neighbouring block extents are taken from the actual decomposition
  !> (all ranks' source blocks), so uneven distributions, the staggered location
  !> of the field (dsrc) and periodic wrap in z are covered. An incompatible
  !> processor grid stops the run before any interpolated value is used; the grid
  !> is never changed here.
  subroutine check_xinlet_database_halo_layout(dm, dsrc, need, yloc, zloc)
    use mpi_mod
    implicit none
    type(t_domain), intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dsrc
    integer, intent(in) :: need(4)          ! layers needed: y below, y above, z below, z above
    integer, intent(in) :: yloc, zloc

    integer, allocatable :: blk(:, :)
    integer :: mine(4), ext(4), n_src(2), b, d, nfail, nfail_glb
    integer :: need_glb(4), ext_loc(4), ext_glb(4)
    logical, save :: reported(2, 2) = .false.
    character(len=1024) :: msg

    mine = (/ dsrc%xst(2), dsrc%xen(2), dsrc%xst(3), dsrc%xen(3) /)
    allocate(blk(4, 0:nproc - 1))
    call mpi_allgather(mine, 4, MPI_INTEGER, blk, 4, MPI_INTEGER, MPI_COMM_WORLD, ierror)
    n_src(1) = maxval(blk(2, :))
    n_src(2) = maxval(blk(4, :))

    ! extent of the neighbouring source block on each side (0 if there is none)
    ext(:) = 0
    do d = 1, 2
      do b = 0, nproc - 1
        if(blk(2*d, b) == mine(2*d-1) - 1 .or. &
           (mine(2*d-1) == 1 .and. dm%is_periodic(d+1) .and. blk(2*d, b) == n_src(d))) &
          ext(2*d-1) = blk(2*d, b) - blk(2*d-1, b) + 1
        if(blk(2*d-1, b) == mine(2*d) + 1 .or. &
           (mine(2*d) == n_src(d) .and. dm%is_periodic(d+1) .and. blk(2*d-1, b) == 1)) &
          ext(2*d) = blk(2*d, b) - blk(2*d-1, b) + 1
      end do
    end do
    deallocate(blk)

    nfail = 0
    if(any(need(:) > ext(:))) nfail = 1
    call mpi_allreduce(nfail, nfail_glb, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
    where(need(:) > 0)
      ext_loc(:) = ext(:)
    elsewhere
      ext_loc(:) = huge(1)
    end where
    do d = 1, 4      ! element-wise (scalar buffers, as the other reductions in this file)
      call mpi_allreduce(need(d), need_glb(d), 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierror)
      call mpi_allreduce(ext_loc(d), ext_glb(d), 1, MPI_INTEGER, MPI_MIN, MPI_COMM_WORLD, ierror)
    end do
    where(ext_glb(:) == huge(1)) ext_glb(:) = -1        ! -1: no layers needed on that side

    if(nfail_glb > 0) then
      if(nrank == 0) then
        write(msg, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,4(1X,I0),A,4(1X,I0),A)') &
          'Inlet database interpolation: processor grid p_row x p_col = ', p_row, ' x ', p_col, &
          ' is incompatible with the source database cross-section ', n_src(1), ' x ', n_src(2), &
          ' (field location y/z = ', yloc, '/', zloc, '). On ', nfail_glb, &
          ' rank(s) the interpolation needs more halo layers than the adjacent rank holds; '// &
          'max layers needed (y-, y+, z-, z+):', need_glb, &
          '; smallest adjacent source block where layers are needed (-1: none needed):', ext_glb, &
          '. The halo exchange would return unfilled values. Choose p_row/p_col in [decomposition] '// &
          'so that each source block is at least as wide as the layers it must supply '// &
          '(e.g. a split of the database cross-section aligned with the main-grid split).'
        write(*, '(A)') trim(msg)
      end if
      call Print_error_msg("Inlet database interpolation halo check failed (see message above). "// &
                           "The processor grid is not changed automatically.")
    end if

    if(nrank == 0 .and. .not. reported(yloc, zloc)) then
      write(*, '(A,I0,A,I0,A,I0,A,I0,A,4(1X,I0),A)') &
        '   inlet database interpolation halo check passed: p_row x p_col = ', p_row, ' x ', p_col, &
        ', field y/z location ', yloc, '/', zloc, ', max layers needed (y-, y+, z-, z+):', need_glb, &
        ' (each within the adjacent source block)'
      reported(yloc, zloc) = .true.
    end if

    return
  end subroutine check_xinlet_database_halo_layout
!==============================================================================
  subroutine interp_xinlet_database_array_yz(dm, fsrc, ftgt, dsrc, dtgt, yloc, zloc)
    use m_halo, only: update_halo
    implicit none
    type(t_domain), intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dsrc
    type(DECOMP_INFO), intent(in) :: dtgt
    real(WP), intent(in) :: fsrc(:, :, :)
    real(WP), intent(inout) :: ftgt(:, :, :)
    integer, intent(in) :: yloc
    integer, intent(in) :: zloc

    integer :: i, j, k, ii, jj, kk
    integer :: halo_level
    real(WP) :: y_target, z_target, var_target
    real(WP) :: h3_src
    integer :: kshift
    real(WP), allocatable :: ysrc(:), zsrc(:), zsrc_ext(:)
    real(WP), allocatable :: fsrc_halo(:, :, :)

    if(yloc == 1) then
      allocate(ysrc(dm%xinlet_database_nc(1)))
      ysrc(:) = dm%xinlet_database_yc_src(:)
    else
      allocate(ysrc(dm%xinlet_database_np(1)))
      ysrc(:) = dm%xinlet_database_yp_src(1:dm%xinlet_database_np(1))
    end if

    h3_src = dm%lzz / real(dm%xinlet_database_nc(2), WP)
    if(zloc == 1) then
      allocate(zsrc(dm%xinlet_database_nc(2)))
    else
      allocate(zsrc(dm%xinlet_database_np(2)))
    end if
    call fill_xinlet_z_coords(size(zsrc), h3_src, zloc, zsrc)

    call compute_xinlet_database_halo_level(dm, dsrc, dtgt, ysrc, zsrc, yloc, zloc, halo_level)
    ! global-index halo; in a periodic z direction the halo cells 0 and n+1 hold
    ! the periodic images (periodic 2decomp topology), used across the seam
    call update_halo(fsrc, fsrc_halo, halo_level, dsrc, opt_global=.true., opt_pencil=IPENCIL(1))
    call build_xinlet_z_search_coords(dm, zsrc, zsrc_ext, kshift)

    do k = 1, dtgt%xsz(3)
      kk = dtgt%xst(3) + k - 1
      z_target = get_xinlet_z_coord(kk, dm%h(3), zloc)
      do j = 1, dtgt%xsz(2)
        jj = dtgt%xst(2) + j - 1
        if(yloc == 1) then
          y_target = dm%yc(jj)
        else
          y_target = dm%yp(jj)
        end if
        do i = 1, dtgt%xsz(1)
          ii = dtgt%xst(1) + i - 1
          call bilinear_interp_xinlet_point_halo(ii, y_target, z_target, ysrc, zsrc_ext, kshift, &
            lbound(fsrc_halo, 1), lbound(fsrc_halo, 2), lbound(fsrc_halo, 3), &
            fsrc_halo, var_target)
          ftgt(i, j, k) = var_target
        end do
      end do
    end do

    deallocate(fsrc_halo)
    deallocate(ysrc)
    deallocate(zsrc)
    deallocate(zsrc_ext)

    return
  end subroutine interp_xinlet_database_array_yz
!==============================================================================
  !> zsrc is the search array from build_xinlet_z_search_coords: in a periodic
  !> z direction it carries one periodic image at each end and kshift = -1 maps
  !> its bracket index to the global source index (0 and n+1 = halo images).
  subroutine bilinear_interp_xinlet_point_halo(i_src, y_target, z_target, ysrc, zsrc, kshift, &
                                              ilo, jlo, klo, fsrc, fout)
    implicit none
    integer, intent(in) :: i_src
    real(WP), intent(in) :: y_target, z_target
    real(WP), intent(in) :: ysrc(:), zsrc(:)
    integer, intent(in) :: kshift
    integer, intent(in) :: ilo, jlo, klo
    real(WP), intent(in) :: fsrc(ilo:, jlo:, klo:)
    real(WP), intent(out) :: fout

    integer :: jsrc, ksrc, kglb
    real(WP) :: eta, zeta
    real(WP) :: dy, dz
    real(WP) :: c00, c01, c10, c11
    real(WP) :: c0, c1

    call binary_search_loc2index_yz(y_target, ysrc, jsrc)
    call binary_search_loc2index_yz(z_target, zsrc, ksrc)
    jsrc = min(jsrc, size(ysrc) - 1)
    ksrc = min(ksrc, size(zsrc) - 1)
    kglb = ksrc + kshift

    if(i_src < lbound(fsrc, 1) .or. i_src > ubound(fsrc, 1) .or. &
       jsrc < lbound(fsrc, 2) .or. jsrc + 1 > ubound(fsrc, 2) .or. &
       kglb < lbound(fsrc, 3) .or. kglb + 1 > ubound(fsrc, 3)) then
      call Print_error_msg("Inlet database interpolation source halo is too small.")
    end if

    dy = ysrc(jsrc + 1) - ysrc(jsrc)
    dz = zsrc(ksrc + 1) - zsrc(ksrc)
    if(dabs(dy) <= MINP) dy = ONE
    if(dabs(dz) <= MINP) dz = ONE

    eta = (y_target - ysrc(jsrc)) / dy
    zeta = (z_target - zsrc(ksrc)) / dz
    eta = max(ZERO, min(ONE, eta))
    zeta = max(ZERO, min(ONE, zeta))

    c00 = fsrc(i_src, jsrc,     kglb)
    c01 = fsrc(i_src, jsrc,     kglb + 1)
    c10 = fsrc(i_src, jsrc + 1, kglb)
    c11 = fsrc(i_src, jsrc + 1, kglb + 1)

    c0 = c00 * (ONE - zeta) + c01 * zeta
    c1 = c10 * (ONE - zeta) + c11 * zeta
    fout = c0 * (ONE - eta) + c1 * eta

    return
  end subroutine bilinear_interp_xinlet_point_halo
!==============================================================================
  subroutine binary_search_loc2index_yz(x_target, x_array, idx)
    implicit none
    real(WP), intent(in) :: x_target
    real(WP), intent(in) :: x_array(:)
    integer, intent(out) :: idx

    integer :: n, left, right, mid

    n = size(x_array)
    if(n < 2) call Print_error_msg("Inlet database interpolation requires at least two source points.")
    if(x_target <= x_array(1)) then
      idx = 1
      return
    end if
    if(x_target >= x_array(n)) then
      idx = n - 1
      return
    end if

    left = 1
    right = n
    do while(right - left > 1)
      mid = (left + right) / 2
      if(x_target <= x_array(mid)) then
        right = mid
      else
        left = mid
      end if
    end do
    idx = left

    return
  end subroutine binary_search_loc2index_yz
!==============================================================================
!==============================================================================
  subroutine append_instantaneous_xoutlet(fl, dm, niter, iter)
    implicit none
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(inout) :: dm
    integer, intent(out) :: niter, iter

    integer :: db_pos, j, k
    type(DECOMP_INFO) :: dtmp

    ! based on x pencil
    if(.not. dm%is_record_xoutlet) return
    if(fl%iteration < dm%ndbstart) return

    ! ndbfre is the full replay period; ndbbuf is the memory-resident chunk.
    ! The file iteration stores the zero-based database offset of each chunk.
    db_pos = fl%iteration - dm%ndbstart + 1
    niter = mod(db_pos - 1, dm%ndbbuf) + 1
    iter = db_pos - niter
    if(niter == 1) then ! re-initialize at begin of each cycle
      dm%fbcx_qx_outl1 = MAXP
      dm%fbcx_qx_outl2 = MAXP
      dm%fbcx_qy_outl1 = MAXP
      dm%fbcx_qy_outl2 = MAXP
      dm%fbcx_qz_outl1 = MAXP
      dm%fbcx_qz_outl2 = MAXP
    else
      ! do nothing
    end if

    dtmp = dm%dpcc
    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        dm%fbcx_qx_outl1(niter, j, k) = fl%qx(dtmp%xsz(1),   j, k)
        dm%fbcx_qx_outl2(niter, j, k) = fl%qx(dtmp%xsz(1)-1, j, k)
      end do
    end do

    !write(*, *) 'j, fl%qx(1, j, 1), dm%fbcx_qx_outl1(niter, j, 1)'
    ! do j = 1, dm%dpcc%xsz(2)
    !   write(*, *) j, fl%qx(dtmp%xsz(1), j, 1), dm%fbcx_qx_outl1(niter, j, 1)
    ! end do

    dtmp = dm%dcpc
    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        dm%fbcx_qy_outl1(niter, j, k) = fl%qy(dtmp%xsz(1),   j, k)
        dm%fbcx_qy_outl2(niter, j, k) = fl%qy(dtmp%xsz(1)-1, j, k)
      end do
    end do

    dtmp = dm%dccp
    do j = 1, dtmp%xsz(2)
      do k = 1, dtmp%xsz(3)
        dm%fbcx_qz_outl1(niter, j, k) = fl%qz(dtmp%xsz(1),   j, k)
        dm%fbcx_qz_outl2(niter, j, k) = fl%qz(dtmp%xsz(1)-1, j, k)
      end do
    end do

    ! Pressure outlet database is intentionally not recorded by default because
    ! pressure database replay is disabled in read_instantaneous_xinlet.

    return
  end subroutine
! !==============================================================================
!   subroutine write_instantaneous_plane(var, keyword, idom, iter, niter, dtmp)
!     implicit none
!     real(WP), contiguous, intent(in) :: var( :, :, :)
!     type(DECOMP_INFO), intent(in) :: dtmp
!     character(*), intent(in) :: keyword
!     integer, intent(in) :: idom
!     integer, intent(in) :: iter, niter

!     character(64):: data_flname_path

!     call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter)

!     if(nrank==0) write(*, *) 'Write outlet plane data to ['//trim(data_flname_path)//"]"

!     !call decomp_2d_open_io (io_in2outlet, trim(data_flname_path), decomp_2d_write_mode)
!     !call decomp_2d_start_io(io_in2outlet, trim(data_flname_path))!

!     !call decomp_2d_write_outflow(trim(data_flname_path), trim(keyword), niter, var, io_in2outlet, dtmp)
!     !call decomp_2d_write_plane(IPENCIL(1), var, 1, dtmp%xsz(1), trim(data_flname_path), dtmp)
!     call decomp_2d_write_plane(IPENCIL(1), var, data_flname_path, &
!                                 opt_nplanes=niter, &
!                                 opt_decomp = dtmp)
!     !call decomp_2d_end_io(io_in2outlet, trim(data_flname_path))
!     !call decomp_2d_close_io(io_in2outlet, trim(data_flname_path))

!     return
!   end subroutine
!==============================================================================
  subroutine write_instantaneous_xoutlet(fl, dm)
    use io_tools_mod
    implicit none
    type(t_flow), intent(in) :: fl
    type(t_domain), intent(inout) :: dm

    character(64):: data_flname_path
    integer :: idom, niter, iter, j, file_iter

    if(.not. dm%is_record_xoutlet) return
    if(fl%iteration < dm%ndbstart) return

    call append_instantaneous_xoutlet(fl, dm, niter, iter)

    ! Write one memory chunk at a time.  File names use iter = 0, ndbbuf,
    ! 2*ndbbuf, ... over the complete outlet database interval.
    !write(*,*) 'iter, niter', fl%iteration, niter
    if(niter == dm%ndbbuf) then
      if( mod(fl%iteration - dm%ndbstart + 1, dm%ndbbuf) /= 0 .and. nrank == 0) &
      call Print_warning_msg("niter /= dm%ndbbuf, something wrong in writing outlet data")
      file_iter = xoutlet_database_file_iter(dm, iter)
      if(dm%restart_data_layout_write == RESTART_LAYOUT_BUNDLED) then
        call write_xoutlet_database_bundle(dm, file_iter)
      else
        call cleanup_xoutlet_database_bundle_files(dm, file_iter)
        call write_one_3d_array(dm%fbcx_qx_outl1, 'outlet1_qx', dm%idom, file_iter, dm%dxcc, dm%existing_output_policy)
        call write_one_3d_array(dm%fbcx_qx_outl2, 'outlet2_qx', dm%idom, file_iter, dm%dxcc, dm%existing_output_policy)
        call write_one_3d_array(dm%fbcx_qy_outl1, 'outlet1_qy', dm%idom, file_iter, dm%dxpc, dm%existing_output_policy)
        call write_one_3d_array(dm%fbcx_qy_outl2, 'outlet2_qy', dm%idom, file_iter, dm%dxpc, dm%existing_output_policy)
        call write_one_3d_array(dm%fbcx_qz_outl1, 'outlet1_qz', dm%idom, file_iter, dm%dxcp, dm%existing_output_policy)
        call write_one_3d_array(dm%fbcx_qz_outl2, 'outlet2_qz', dm%idom, file_iter, dm%dxcp, dm%existing_output_policy)
      end if
      ! Pressure outlet database is optional and disabled by default.
      !call write_one_3d_array(dm%fbcx_pr_outl1, 'outlet1_pr', dm%idom, iter, dm%dxcc, dm%existing_output_policy)
      !call write_one_3d_array(dm%fbcx_pr_outl2, 'outlet2_pr', dm%idom, iter, dm%dxcc, dm%existing_output_policy)
      !if(nrank == 0) write (*,*) " writing outlet database at ", fl%iteration, 'for iter =', iter -  dm%ndbfre, 'to ', iter
    end if
! #ifdef DEBUG_STEPS
!     write(*,*) 'outlet bc'
!     do j = 1, dm%dpcc%xsz(2)
!       write(*,*) dm%dpcc%xst(2) + j - 1, &
!       dm%fbcx_qx_outl1(niter, j, 1), dm%fbcx_qx_outl2(niter, j, 1)
!     end do
!     write(*,*) 'inlet bc'
!     do j = 1, dm%dpcc%xsz(2)
!       write(*,*) dm%dpcc%xst(2) + j - 1, &
!       dm%fbcx_qx_out1(niter, j, 1), dm%fbcx_qx_out2(niter, j, 1)
!     end do
! #endif
    return
  end subroutine
!==============================================================================
  subroutine assign_instantaneous_xinlet(fl, dm)
    use convert_primary_conservative_mod
    use typeconvert_mod
    implicit none
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(inout) :: dm

    integer :: iter, j, k
    type(DECOMP_INFO) :: dtmp

    ! based on x pencil
    if(.not. dm%is_read_xinlet) return

    iter = max(1, fl%iteration)
    iter = mod(iter-1, dm%ndbbuf) + 1

    !if (nrank == 0) &
    !  call Print_debug_mid_msg('inlet assigned at iteration '//trim(int2str(iter))

    if(dm%ibcx_nominal(1, 1) == IBC_DATABASE) then
      dtmp = dm%dpcc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_qx(1, j, k) = dm%fbcx_qx_inl1(iter, j, k)
          dm%fbcx_qx(3, j, k) = dm%fbcx_qx_inl2(iter, j, k)
          ! check, below
          !fl%qx(1, j, k) = dm%fbcx_qx(1, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_in1 = ', iter, dm%fbcx_qx_inl1(iter, :, 1)
      !if(nrank == 0) write(*,*) 'fbcx_in2 = ', iter, dm%fbcx_qx_inl1(iter, :, 32)
      !if(nrank == 0) write(*,*) 'fbcx_qx1 = ', iter, dm%fbcx_qx(1, :, 1)
      !if(nrank == 0) write(*,*) 'fbcx_qx2 = ', iter, dm%fbcx_qx(1, :, 32)
    end if


    if(dm%ibcx_nominal(1, 2) == IBC_DATABASE) then
      dtmp = dm%dcpc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_qy(1, j, k) = dm%fbcx_qy_inl1(iter, j, k)
          dm%fbcx_qy(3, j, k) = dm%fbcx_qy_inl2(iter, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_qy = ', iter, dm%fbcx_qy(1, :, :)
    end if

    if(dm%ibcx_nominal(1, 3) == IBC_DATABASE) then
      dtmp = dm%dccp
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_qz(1, j, k) = dm%fbcx_qz_inl1(iter, j, k)
          dm%fbcx_qz(3, j, k) = dm%fbcx_qz_inl2(iter, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_qz = ', iter, dm%fbcx_qz(1, :, :)
    end if

    if(dm%ibcx_nominal(1, 4) == IBC_DATABASE .and. &
       allocated(dm%fbcx_pr_inl1) .and. allocated(dm%fbcx_pr_inl2)) then
      dtmp = dm%dccc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          dm%fbcx_pr(1, j, k) = dm%fbcx_pr_inl1(iter, j, k)
          dm%fbcx_pr(3, j, k) = dm%fbcx_pr_inl2(iter, j, k)
        end do
      end do
      !if(nrank == 0) write(*,*) 'fbcx_pr = ', iter, dm%fbcx_pr(1, :, :)
    end if

    if(dm%is_thermo) then
      call convert_primary_conservative(dm, fl%dDens, IQ2G, IBND)
    end if

    return
  end subroutine
! !==============================================================================
!   subroutine read_instantaneous_plane(var, keyword, idom, iter, nfre, dtmp)
!     use decomp_2d_io
!     implicit none
!     real(WP), contiguous, intent(out) :: var( :, :, :)
!     type(DECOMP_INFO), intent(in) :: dtmp
!     character(*), intent(in) :: keyword
!     integer, intent(in) :: idom
!     integer, intent(in) :: iter
!     integer, intent(in) :: nfre

!     character(64):: data_flname_path, flname

!     call generate_pathfile_name(data_flname_path, idom, trim(keyword), dir_data, 'bin', iter, flname)

!     !call decomp_2d_open_io (io_in2outlet, trim(data_flname_path), decomp_2d_read_mode)
!     if(nrank == 0) call Print_debug_inline_msg("Read data on a plane from file: "//trim(data_flname_path))
!     !call decomp_2d_read_inflow(trim(data_flname_path), trim(keyword), nfre, var, io_in2outlet, dtmp)
!     call decomp_2d_read_plane(IPENCIL(1), var, data_flname_path, nfre, &
!                                 opt_decomp = dtmp)

!     !decomp_2d_read_plane(ipencil, var, varname, nplanes, &
!                               !  opt_dirname, &
!                               !  opt_mpi_file_open_info, &
!                               !  opt_mpi_file_set_view_info, &
!                               !  opt_reduce_prec, &
!                               !  opt_decomp, &
!                               !  opt_nb_req, &
!                               !  opt_io)
!     !write(*,*) var
!     !call decomp_2d_close_io(io_in2outlet, trim(data_flname_path))

!     return
!   end subroutine
!==============================================================================
  subroutine read_instantaneous_xinlet(fl, dm, opt_iter)
    use io_tools_mod
    use typeconvert_mod
    implicit none
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer, intent(in), optional :: opt_iter

    character(64):: data_flname_path
    integer :: iter, niter, nblock, nblocks
    integer :: ndb_total


    if(.not. dm%is_read_xinlet) return
    ! ----------------------------------------------------------------------------
    ! ndb_sampling_dt given: the database planes are samples in physical time.
    ! Ratio 1 keeps the step-indexed replay below; ratio > 1 interpolates in time.
    ! ----------------------------------------------------------------------------
    if(dm%ndb_sampling_dt > ZERO) call check_xinlet_constant_dt(dm)
    if(dm%ndb_time_ratio > 1) then
      call read_instantaneous_xinlet_tinterp(fl, dm, opt_iter)
      return
    end if
    ! ----------------------------------------------------------------------------
    ! The database period remains ndbfre, but files are stored as ndbbuf-sized
    ! chunks named by zero-based database offset: 0, ndbbuf, 2*ndbbuf, ...
    ! Inlet replay cycles through the complete [ndbstart, ndbend] database.
    ! ----------------------------------------------------------------------------
    iter = fl%iteration
    if (present(opt_iter)) iter = opt_iter
    ! ----------------------------------------------------------------------------
    ! Only read if current iteration is the first of the memory chunk
    ! ----------------------------------------------------------------------------
    ! The second test is "this is the first step this run solves", which is the
    ! run clock plus one. It is not fl%iterfrom + 1: under restart_clock=reset
    ! the checkpoint index and the clock origin deliberately differ.
    if (mod(iter-1, dm%ndbbuf)==0 .or. iter == (dm%iteration_start+1)) then
      ndb_total = dm%ndbend - dm%ndbstart + 1
      nblocks = ndb_total / dm%ndbbuf
      nblock = mod((iter - 1) / dm%ndbbuf, nblocks)
      niter = dm%ndbbuf * nblock
      if(nrank == 0) &
      call Print_debug_mid_msg('Read inlet database at iteration '//trim(int2str(iter))&
        //' mapped to file name ='//trim(int2str(xoutlet_database_file_iter(dm, niter))))
      if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
        call generate_pathfile_name(data_flname_path, dm%idom, 'xoutlet_database', dir_data, 'bin', &
          xoutlet_database_file_iter(dm, niter))
        if(file_exists(trim(data_flname_path))) then
          if(nrank == 0) call Print_debug_mid_msg("Restart input layout expects bundled inlet/outlet database files.")
          call read_xoutlet_database_bundle_interp(dm, xoutlet_database_file_iter(dm, niter))
        else
          if(nrank == 0) call Print_warning_msg("Bundled inlet/outlet database file was not found; " // &
            "falling back to per-field inlet/outlet database files.")
          call read_xoutlet_database_per_field_interp(dm, xoutlet_database_file_iter(dm, niter))
        end if
      else
        if(nrank == 0) call Print_debug_mid_msg("Restart input layout expects per-field inlet/outlet database files.")
        call read_xoutlet_database_per_field_interp(dm, xoutlet_database_file_iter(dm, niter))
      end if
      !call read_one_3d_array(dm%fbcx_pr_inl1, 'outlet1_pr', dm%idom, niter, dm%dxcc)
      !call read_one_3d_array(dm%fbcx_pr_inl2, 'outlet2_pr', dm%idom, niter, dm%dxcc)
    end if

    ! ----------------------------------------------------------------------------
    ! Assign inlet data for every iteration (after reading block)
    ! ----------------------------------------------------------------------------
    call assign_instantaneous_xinlet(fl, dm)

    return
  end subroutine
!==============================================================================
  !> Inlet database replay with linear interpolation in time
  !> (ndb_sampling_dt = R dt, R = ndb_time_ratio > 1).
  !>
  !> Time convention (unchanged from the step-indexed replay): the step that
  !> advances t_{n-1} -> t_n uses the inflow at its start time t_{n-1}; the plane
  !> stored at database position q (0-based, q = 0 at ndbstart) is the sample at
  !> database time q*ndb_sampling_dt, and database time is the run's physical
  !> time. With the step-start index m = t_{n-1}/dt (an integer on a fixed-dt
  !> run; checked):
  !>   q = m div R,  alpha = mod(m, R)/R,  u = (1 - alpha) u_q + alpha u_{q+1},
  !> so alpha = 0, 1/R, ..., (R-1)/R and the stored plane is used unchanged at
  !> every sample time. With R = 1 this is the step-indexed replay.
  !> Replay: q is taken modulo the database length (ndbend - ndbstart + 1).
  !> The last stored plane and the first are not consecutive samples (cyclic
  !> replay of an uncorrelated record), so no interpolation is done across the
  !> wrap: the last plane is held for the R steps of its interval and the inflow
  !> then jumps to the first plane, as in the step-indexed replay.
  !> Buffers: the block (ndbbuf planes) holding q is in fbcx_q*_inl1/2; when q is
  !> the last plane of its block and alpha > 0, the next block is read into
  !> fbcx_q*_inl1/2_nxt, which becomes the current block at the next sample.
  !> Each block is read once per pass.
  subroutine read_instantaneous_xinlet_tinterp(fl, dm, opt_iter)
    implicit none
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer, intent(in), optional :: opt_iter

    integer :: m, nratio, q, kfrac, ntot, qc, blk, l
    real(WP) :: xm, alpha
    logical :: at_wrap, blend, use_next

    if(present(opt_iter)) then
      m = opt_iter - 1                              ! initialisation from the inlet
    else
      ! called after the clock update of this step: fl%time = t_n, start t_{n-1}
      xm = (fl%time - dm%dt) / dm%dt
      m = nint(xm)
      if(m < 0 .or. abs(xm - real(m, WP)) > 0.01_WP) &
        call Print_error_msg("Inlet database temporal interpolation: the run time is not a "// &
          "multiple of dt (dt changed or inconsistent restart clock).")
    end if
    nratio = dm%ndb_time_ratio
    q = m / nratio
    kfrac = mod(m, nratio)
    ntot = dm%ndbend - dm%ndbstart + 1
    qc = mod(q, ntot)
    blk = qc / dm%ndbbuf
    l = mod(qc, dm%ndbbuf) + 1
    at_wrap = (qc == ntot - 1)
    blend = (kfrac > 0) .and. (.not. at_wrap)      ! between two consecutive samples
    alpha = ZERO
    if(blend) alpha = real(kfrac, WP) / real(nratio, WP)
    use_next = blend .and. (l == dm%ndbbuf)

    call load_xinlet_block_current(dm, blk, qc, kfrac)
    if(use_next) call load_xinlet_block_next(dm, blk + 1)
    call assign_instantaneous_xinlet_tinterp(fl, dm, l, blend, alpha, use_next)

    return
  end subroutine read_instantaneous_xinlet_tinterp
!==============================================================================
  subroutine load_xinlet_block_current(dm, blk, qc, kfrac)
    use typeconvert_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: blk, qc, kfrac

    integer :: itmp

    if(dm%xinlet_blk_cur == blk) return
    if(dm%xinlet_blk_nxt == blk) then
      call swap_xinlet_block_buffers(dm)
      itmp = dm%xinlet_blk_cur
      dm%xinlet_blk_cur = dm%xinlet_blk_nxt
      dm%xinlet_blk_nxt = itmp
    else
      if(nrank == 0) call Print_debug_mid_msg('Read inlet database block for sample '// &
        trim(int2str(qc))//' (+'//trim(int2str(kfrac))//'/'//trim(int2str(dm%ndb_time_ratio))// &
        ' of a sample interval) mapped to file name ='// &
        trim(int2str(xoutlet_database_file_iter(dm, dm%ndbbuf * blk))))
      call read_xinlet_block(dm, blk)
      dm%xinlet_blk_cur = blk
    end if

    return
  end subroutine load_xinlet_block_current
!==============================================================================
  subroutine load_xinlet_block_next(dm, blk)
    use typeconvert_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: blk

    if(dm%xinlet_blk_nxt == blk) return
    if(.not. allocated(dm%fbcx_qx_inl1_nxt)) then   ! once per run
      allocate(dm%fbcx_qx_inl1_nxt, mold = dm%fbcx_qx_inl1)
      allocate(dm%fbcx_qx_inl2_nxt, mold = dm%fbcx_qx_inl2)
      allocate(dm%fbcx_qy_inl1_nxt, mold = dm%fbcx_qy_inl1)
      allocate(dm%fbcx_qy_inl2_nxt, mold = dm%fbcx_qy_inl2)
      allocate(dm%fbcx_qz_inl1_nxt, mold = dm%fbcx_qz_inl1)
      allocate(dm%fbcx_qz_inl2_nxt, mold = dm%fbcx_qz_inl2)
    end if
    if(nrank == 0) call Print_debug_mid_msg('Read next inlet database block mapped to file name ='// &
      trim(int2str(xoutlet_database_file_iter(dm, dm%ndbbuf * blk))))
    ! the readers fill fbcx_q*_inl1/2: read into the next-block storage by swapping
    call swap_xinlet_block_buffers(dm)
    call read_xinlet_block(dm, blk)
    call swap_xinlet_block_buffers(dm)
    dm%xinlet_blk_nxt = blk

    return
  end subroutine load_xinlet_block_next
!==============================================================================
  !> Read database block blk (planes ndbbuf*blk ... ndbbuf*blk + ndbbuf - 1) into
  !> fbcx_q*_inl1/2 through the same bundled / per-field readers (and spatial
  !> interpolation) as the step-indexed replay.
  subroutine read_xinlet_block(dm, blk)
    use io_tools_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: blk

    character(64) :: data_flname_path
    integer :: file_iter

    file_iter = xoutlet_database_file_iter(dm, dm%ndbbuf * blk)
    if(dm%restart_data_layout_read == RESTART_LAYOUT_BUNDLED) then
      call generate_pathfile_name(data_flname_path, dm%idom, 'xoutlet_database', dir_data, 'bin', file_iter)
      if(file_exists(trim(data_flname_path))) then
        call read_xoutlet_database_bundle_interp(dm, file_iter)
      else
        if(nrank == 0) call Print_warning_msg("Bundled inlet/outlet database file was not found; " // &
          "falling back to per-field inlet/outlet database files.")
        call read_xoutlet_database_per_field_interp(dm, file_iter)
      end if
    else
      call read_xoutlet_database_per_field_interp(dm, file_iter)
    end if

    return
  end subroutine read_xinlet_block
!==============================================================================
  subroutine swap_xinlet_block_buffers(dm)
    implicit none
    type(t_domain), intent(inout) :: dm
    real(WP), allocatable :: tmp(:, :, :)

    call move_alloc(dm%fbcx_qx_inl1, tmp); call move_alloc(dm%fbcx_qx_inl1_nxt, dm%fbcx_qx_inl1)
    call move_alloc(tmp, dm%fbcx_qx_inl1_nxt)
    call move_alloc(dm%fbcx_qx_inl2, tmp); call move_alloc(dm%fbcx_qx_inl2_nxt, dm%fbcx_qx_inl2)
    call move_alloc(tmp, dm%fbcx_qx_inl2_nxt)
    call move_alloc(dm%fbcx_qy_inl1, tmp); call move_alloc(dm%fbcx_qy_inl1_nxt, dm%fbcx_qy_inl1)
    call move_alloc(tmp, dm%fbcx_qy_inl1_nxt)
    call move_alloc(dm%fbcx_qy_inl2, tmp); call move_alloc(dm%fbcx_qy_inl2_nxt, dm%fbcx_qy_inl2)
    call move_alloc(tmp, dm%fbcx_qy_inl2_nxt)
    call move_alloc(dm%fbcx_qz_inl1, tmp); call move_alloc(dm%fbcx_qz_inl1_nxt, dm%fbcx_qz_inl1)
    call move_alloc(tmp, dm%fbcx_qz_inl1_nxt)
    call move_alloc(dm%fbcx_qz_inl2, tmp); call move_alloc(dm%fbcx_qz_inl2_nxt, dm%fbcx_qz_inl2)
    call move_alloc(tmp, dm%fbcx_qz_inl2_nxt)

    return
  end subroutine swap_xinlet_block_buffers
!==============================================================================
  !> Inlet planes for the temporal replay: plane l of the current block, blended
  !> with plane l+1 (or plane 1 of the next block when use_next) with weight alpha.
  !> blend = .false. (a sample time, or the held last plane) copies the stored
  !> plane unchanged.
  subroutine assign_instantaneous_xinlet_tinterp(fl, dm, l, blend, alpha, use_next)
    use convert_primary_conservative_mod
    implicit none
    type(t_flow), intent(inout) :: fl
    type(t_domain), intent(inout) :: dm
    integer, intent(in) :: l
    logical, intent(in) :: blend
    real(WP), intent(in) :: alpha
    logical, intent(in) :: use_next

    integer :: j, k
    real(WP) :: w0, w1
    type(DECOMP_INFO) :: dtmp

    w0 = ONE - alpha
    w1 = alpha

    if(dm%ibcx_nominal(1, 4) == IBC_DATABASE .and. &
       allocated(dm%fbcx_pr_inl1) .and. allocated(dm%fbcx_pr_inl2)) &
      call Print_error_msg("Pressure inlet database replay is not supported with ndb_sampling_dt > dt.")

    if(dm%ibcx_nominal(1, 1) == IBC_DATABASE) then
      dtmp = dm%dpcc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          if(.not. blend) then
            dm%fbcx_qx(1, j, k) = dm%fbcx_qx_inl1(l, j, k)
            dm%fbcx_qx(3, j, k) = dm%fbcx_qx_inl2(l, j, k)
          else if(use_next) then
            dm%fbcx_qx(1, j, k) = w0 * dm%fbcx_qx_inl1(l, j, k) + w1 * dm%fbcx_qx_inl1_nxt(1, j, k)
            dm%fbcx_qx(3, j, k) = w0 * dm%fbcx_qx_inl2(l, j, k) + w1 * dm%fbcx_qx_inl2_nxt(1, j, k)
          else
            dm%fbcx_qx(1, j, k) = w0 * dm%fbcx_qx_inl1(l, j, k) + w1 * dm%fbcx_qx_inl1(l + 1, j, k)
            dm%fbcx_qx(3, j, k) = w0 * dm%fbcx_qx_inl2(l, j, k) + w1 * dm%fbcx_qx_inl2(l + 1, j, k)
          end if
        end do
      end do
    end if

    if(dm%ibcx_nominal(1, 2) == IBC_DATABASE) then
      dtmp = dm%dcpc
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          if(.not. blend) then
            dm%fbcx_qy(1, j, k) = dm%fbcx_qy_inl1(l, j, k)
            dm%fbcx_qy(3, j, k) = dm%fbcx_qy_inl2(l, j, k)
          else if(use_next) then
            dm%fbcx_qy(1, j, k) = w0 * dm%fbcx_qy_inl1(l, j, k) + w1 * dm%fbcx_qy_inl1_nxt(1, j, k)
            dm%fbcx_qy(3, j, k) = w0 * dm%fbcx_qy_inl2(l, j, k) + w1 * dm%fbcx_qy_inl2_nxt(1, j, k)
          else
            dm%fbcx_qy(1, j, k) = w0 * dm%fbcx_qy_inl1(l, j, k) + w1 * dm%fbcx_qy_inl1(l + 1, j, k)
            dm%fbcx_qy(3, j, k) = w0 * dm%fbcx_qy_inl2(l, j, k) + w1 * dm%fbcx_qy_inl2(l + 1, j, k)
          end if
        end do
      end do
    end if

    if(dm%ibcx_nominal(1, 3) == IBC_DATABASE) then
      dtmp = dm%dccp
      do j = 1, dtmp%xsz(2)
        do k = 1, dtmp%xsz(3)
          if(.not. blend) then
            dm%fbcx_qz(1, j, k) = dm%fbcx_qz_inl1(l, j, k)
            dm%fbcx_qz(3, j, k) = dm%fbcx_qz_inl2(l, j, k)
          else if(use_next) then
            dm%fbcx_qz(1, j, k) = w0 * dm%fbcx_qz_inl1(l, j, k) + w1 * dm%fbcx_qz_inl1_nxt(1, j, k)
            dm%fbcx_qz(3, j, k) = w0 * dm%fbcx_qz_inl2(l, j, k) + w1 * dm%fbcx_qz_inl2_nxt(1, j, k)
          else
            dm%fbcx_qz(1, j, k) = w0 * dm%fbcx_qz_inl1(l, j, k) + w1 * dm%fbcx_qz_inl1(l + 1, j, k)
            dm%fbcx_qz(3, j, k) = w0 * dm%fbcx_qz_inl2(l, j, k) + w1 * dm%fbcx_qz_inl2(l + 1, j, k)
          end if
        end do
      end do
    end if

    if(dm%is_thermo) then
      call convert_primary_conservative(dm, fl%dDens, IQ2G, IBND)
    end if

    return
  end subroutine assign_instantaneous_xinlet_tinterp
!==============================================================================
  !> ndb_sampling_dt must equal the sampling interval recorded in the bundle
  !> metadata ('dt' line written by write_xoutlet_database_bundle).
  subroutine check_xinlet_database_dt(dm, iter)
    implicit none
    type(t_domain), intent(in) :: dm
    integer, intent(in) :: iter

    character(256) :: meta_file
    character(1024) :: value
    real(WP) :: dt_db
    integer :: ioerr

    call generate_pathfile_name(meta_file, dm%idom, 'xoutlet_database_meta', dir_data, 'dat', iter)
    value = metadata_value(meta_file, 'dt')
    read(value, *, iostat = ioerr) dt_db
    if(ioerr /= 0 .or. .not. (dt_db > ZERO)) &
      call Print_error_msg("No valid dt line in "//trim(meta_file)//" to check ndb_sampling_dt against.")
    if(abs(dt_db - dm%ndb_sampling_dt) > 1.0e-9_WP * dm%ndb_sampling_dt) &
      call Print_error_msg("ndb_sampling_dt differs from the sampling interval dt recorded in "// &
        trim(meta_file)//".")

    return
  end subroutine check_xinlet_database_dt
!==============================================================================
  !> The sample-time lattice is fixed by the dt the ratio was validated with.
  subroutine check_xinlet_constant_dt(dm)
    implicit none
    type(t_domain), intent(in) :: dm

    if(abs(dm%dt - dm%ndb_sampling_dt_run) > 1.0e-12_WP * dm%ndb_sampling_dt_run) &
      call Print_error_msg("dt changed during the run: ndb_sampling_dt requires a constant dt "// &
        "(set CHAPSIM_B3_FIXED_DT=1).")

    return
  end subroutine check_xinlet_constant_dt
end module
!==============================================================================
!==============================================================================
!> Field interpolation support for restarting on a different mesh.
!>
!> Reads source and target domain/mesh descriptors, builds the interpolated
!> target flow and thermal fields, and writes the target-domain restart files
!> used by the two-step mesh-restart workflow.
module io_field_interpolation_mod
  USE precision_mod
  use udf_type_mod
  implicit none

  type(t_domain) :: domain_tgt
  type(t_flow)   :: flow_tgt
  type(t_thermo) :: thermo_tgt
  character(len = 21) :: input_tgt = 'input_chapsim_tgt.ini'

  integer, parameter :: XLOC_CELL = 1, &
                        XLOC_FACE = 2, &
                        YLOC_CELL = 3, &
                        YLOC_FACE = 4, &
                        ZLOC_CELL = 5, &
                        ZLOC_FACE = 6

  private :: binary_search_loc2index
  private :: trilinear_interp_point
  private :: trilinear_interp_point_halo
  private :: setup_extension_mapping
  private :: compute_interp_halo_level
  private :: build_up_interp_target_field_flow
  private :: build_up_interp_target_field_thermo
  private :: preserve_interp_streamwise_bulk
  private :: configure_interp_target_domain
  private :: allocate_interp_target_variables
  private :: interp_fbcx_plane_generic
  private :: interp_fbcx_history_plane
  private :: build_up_interp_target_xoutlet_state
  public  :: output_interp_target_field

  contains
!==============================================================================
  SUBROUTINE binary_search_loc2index(x_target, x_array, idx)
    IMPLICIT NONE
    REAL(WP), INTENT(IN) :: x_target
    REAL(WP), INTENT(IN) :: x_array(:)
    INTEGER, INTENT(OUT) :: idx
    !
    INTEGER :: n, left, right, mid
    !
    n = size(x_array, 1)
    ! Handle boundary cases
    IF (x_target <= x_array(1)) THEN
      idx = 1
      RETURN
    END IF
    IF (x_target >= x_array(n)) THEN
      idx = n - 1
      RETURN
    END IF
    ! Binary search
    left = 1
    right = n
    DO WHILE (right - left > 1)
      mid = (left + right) / 2
      IF (x_target <= x_array(mid)) THEN
        right = mid
      ELSE
        left = mid
      END IF
    END DO
    idx = left
    RETURN
  END SUBROUTINE binary_search_loc2index
!==============================================================================
  SUBROUTINE trilinear_interp_point(x_target, y_target, z_target, &
                                        x_src, y_src, z_src, var_src, &
                                        var_interp)
    USE parameters_constant_mod
    IMPLICIT NONE
    REAL(WP), INTENT(IN) :: x_target, y_target, z_target
    REAL(WP), INTENT(IN) :: x_src(:), y_src(:), z_src(:)
    REAL(WP), INTENT(IN) :: var_src(:, :, :)
    REAL(WP), INTENT(OUT) :: var_interp

    INTEGER :: i_src, j_src, k_src, nx, ny, nz
    REAL(WP) :: xi, eta, zeta
    REAL(WP) :: dx, dy, dz
    REAL(WP) :: c000, c001, c010, c011, c100, c101, c110, c111
    REAL(WP) :: c00, c01, c10, c11, c0, c1

    nx = SIZE(x_src)
    ny = SIZE(y_src)
    nz = SIZE(z_src)

    !-------------------------------------------------------------
    ! Find enclosing cell indices
    !-------------------------------------------------------------
    CALL binary_search_loc2index(x_target, x_src, i_src)
    CALL binary_search_loc2index(y_target, y_src, j_src)
    CALL binary_search_loc2index(z_target, z_src, k_src)

    ! Clamp indices to valid range (ensure i_src+1 <= nx)
    i_src = MIN(i_src, nx-1)
    j_src = MIN(j_src, ny-1)
    k_src = MIN(k_src, nz-1)

    !-------------------------------------------------------------
    ! Compute normalized coordinates within the cell
    !-------------------------------------------------------------
    dx = x_src(i_src+1) - x_src(i_src)
    dy = y_src(j_src+1) - y_src(j_src)
    dz = z_src(k_src+1) - z_src(k_src)

    ! Avoid division by zero
    IF (dabs(dx) <= MINP) dx = 1.0_wp
    IF (dabs(dy) <= MINP) dy = 1.0_wp
    IF (dabs(dz) <= MINP) dz = 1.0_wp

    xi   = (x_target - x_src(i_src)) / dx
    eta  = (y_target - y_src(j_src)) / dy
    zeta = (z_target - z_src(k_src)) / dz

    ! Clamp normalized coordinates to [0,1] to avoid extrapolation outside last cell
    xi   = MAX(0.0_wp, MIN(1.0_wp, xi))
    eta  = MAX(0.0_wp, MIN(1.0_wp, eta))
    zeta = MAX(0.0_wp, MIN(1.0_wp, zeta))

    !-------------------------------------------------------------
    ! Trilinear interpolation
    !-------------------------------------------------------------
    c000 = var_src(i_src  , j_src  , k_src  )
    c001 = var_src(i_src  , j_src  , k_src+1)
    c010 = var_src(i_src  , j_src+1, k_src  )
    c011 = var_src(i_src  , j_src+1, k_src+1)
    c100 = var_src(i_src+1, j_src  , k_src  )
    c101 = var_src(i_src+1, j_src  , k_src+1)
    c110 = var_src(i_src+1, j_src+1, k_src  )
    c111 = var_src(i_src+1, j_src+1, k_src+1)

    ! Interpolate in z
    c00 = c000*(1.0_wp - zeta) + c001*zeta
    c01 = c010*(1.0_wp - zeta) + c011*zeta
    c10 = c100*(1.0_wp - zeta) + c101*zeta
    c11 = c110*(1.0_wp - zeta) + c111*zeta

    ! Interpolate in y
    c0 = c00*(1.0_wp - eta) + c01*eta
    c1 = c10*(1.0_wp - eta) + c11*eta

    ! Interpolate in x
    var_interp = c0*(1.0_wp - xi) + c1*xi
    RETURN
  END SUBROUTINE trilinear_interp_point
!==============================================================================
  SUBROUTINE trilinear_interp_point_halo(x_target, y_target, z_target, &
                                        x_src, y_src, z_src,            &
                                        ilo, jlo, klo, var_src,         &
                                        var_interp)
    USE parameters_constant_mod
    use print_msg_mod
    IMPLICIT NONE
    REAL(WP), INTENT(IN) :: x_target, y_target, z_target
    REAL(WP), INTENT(IN) :: x_src(:), y_src(:), z_src(:)
    INTEGER, INTENT(IN) :: ilo, jlo, klo
    REAL(WP), INTENT(IN) :: var_src(ilo:, jlo:, klo:)
    REAL(WP), INTENT(OUT) :: var_interp

    INTEGER :: i_src, j_src, k_src, nx, ny, nz
    REAL(WP) :: xi, eta, zeta
    REAL(WP) :: dx, dy, dz
    REAL(WP) :: c000, c001, c010, c011, c100, c101, c110, c111
    REAL(WP) :: c00, c01, c10, c11, c0, c1

    nx = SIZE(x_src)
    ny = SIZE(y_src)
    nz = SIZE(z_src)

    CALL binary_search_loc2index(x_target, x_src, i_src)
    CALL binary_search_loc2index(y_target, y_src, j_src)
    CALL binary_search_loc2index(z_target, z_src, k_src)

    i_src = MIN(i_src, nx-1)
    j_src = MIN(j_src, ny-1)
    k_src = MIN(k_src, nz-1)

    if(i_src < lbound(var_src, 1) .or. i_src + 1 > ubound(var_src, 1) .or. &
       j_src < lbound(var_src, 2) .or. j_src + 1 > ubound(var_src, 2) .or. &
       k_src < lbound(var_src, 3) .or. k_src + 1 > ubound(var_src, 3)) then
      call Print_error_msg('Interpolation source halo is too small for requested target point.')
    end if

    dx = x_src(i_src+1) - x_src(i_src)
    dy = y_src(j_src+1) - y_src(j_src)
    dz = z_src(k_src+1) - z_src(k_src)

    IF (dabs(dx) <= MINP) dx = 1.0_wp
    IF (dabs(dy) <= MINP) dy = 1.0_wp
    IF (dabs(dz) <= MINP) dz = 1.0_wp

    xi   = (x_target - x_src(i_src)) / dx
    eta  = (y_target - y_src(j_src)) / dy
    zeta = (z_target - z_src(k_src)) / dz

    xi   = MAX(0.0_wp, MIN(1.0_wp, xi))
    eta  = MAX(0.0_wp, MIN(1.0_wp, eta))
    zeta = MAX(0.0_wp, MIN(1.0_wp, zeta))

    c000 = var_src(i_src  , j_src  , k_src  )
    c001 = var_src(i_src  , j_src  , k_src+1)
    c010 = var_src(i_src  , j_src+1, k_src  )
    c011 = var_src(i_src  , j_src+1, k_src+1)
    c100 = var_src(i_src+1, j_src  , k_src  )
    c101 = var_src(i_src+1, j_src  , k_src+1)
    c110 = var_src(i_src+1, j_src+1, k_src  )
    c111 = var_src(i_src+1, j_src+1, k_src+1)

    c00 = c000*(1.0_wp - zeta) + c001*zeta
    c01 = c010*(1.0_wp - zeta) + c011*zeta
    c10 = c100*(1.0_wp - zeta) + c101*zeta
    c11 = c110*(1.0_wp - zeta) + c111*zeta

    c0 = c00*(1.0_wp - eta) + c01*eta
    c1 = c10*(1.0_wp - eta) + c11*eta

    var_interp = c0*(1.0_wp - xi) + c1*xi
    RETURN
  END SUBROUTINE trilinear_interp_point_halo
!==============================================================================
  subroutine setup_extension_mapping(src_len, tgt_len, tgt_spacing, extend_mode, extend_length)
    use parameters_constant_mod
    implicit none

    real(WP), intent(in)  :: src_len, tgt_len, tgt_spacing
    integer , intent(out) :: extend_mode
    real(WP), intent(out) :: extend_length

    if (src_len > tgt_len) then
      extend_mode = 1
    else if (src_len < tgt_len) then
      extend_mode = 2
    else
      extend_mode = 0
    end if

    extend_length = ZERO
    if (extend_mode == 2) then
      extend_length = MAX((tgt_len - src_len) / FIVE, TWO * tgt_spacing)
    end if

    return
  end subroutine setup_extension_mapping
!==============================================================================
  !> Interpolate source flow fields onto the target mesh.
  !>
  !> Cylindrical note: qy is stored as the radial flux r*u_r, not as u_r, and it
  !> is remapped in that stored form. Interpolating the flux is the deliberate
  !> choice - it is what the radial mass balance is written in, and r*u_r stays
  !> regular on the axis while u_r = qy/r does not - but it means the remap is
  !> linear in r*u_r rather than in u_r, so the two differ at the O(dr^2) level
  !> of the interpolation itself. qz = u_theta and qx = u_x are plain velocities
  !> and need no such caveat.
  !>
  !> - fl_src (in): Source flow field.
  !> - dm_src (in): Source domain descriptor.
  !> - fl_tgt (inout): Target flow field.
  !> - dm_tgt (in): Target domain descriptor.
  subroutine build_up_interp_target_field_flow(fl_src, dm_src, fl_tgt, dm_tgt)
    use io_restart_mod, only: is_restart_history_exact
    use parameters_constant_mod
    use print_msg_mod
    use udf_type_mod
    implicit none
    !
    type(t_domain), intent(in)    :: dm_src
    type(t_flow)  , intent(in)    :: fl_src
    type(t_domain), intent(inout) :: dm_tgt
    type(t_flow)  , intent(inout) :: fl_tgt
    !
    integer  :: imode_x, imode_z, i, k
    real(WP) :: Lbuf_x, Lbuf_z
    real(WP) :: xc_src(dm_src%nc(1)), zc_src(dm_src%nc(3))
    real(WP) :: xp_src(dm_src%np(1)), zp_src(dm_src%np(3))
    !
    if (abs(dm_src%lyt - dm_tgt%lyt) > 1.0e-10_wp) then
      call print_error_msg("build_up_interp_target_field_flow: cross-section mismatch in yt")
    end if
    if (abs(dm_src%lyb - dm_tgt%lyb) > 1.0e-10_wp) then
      call print_error_msg("build_up_interp_target_field_flow: cross-section mismatch in yb")
    end if
    !-----------------------------------------
    ! extension controls
    !-----------------------------------------
    call setup_extension_mapping(dm_src%lxx, dm_tgt%lxx, dm_tgt%h(1), imode_x, Lbuf_x)
    call setup_extension_mapping(dm_src%lzz, dm_tgt%lzz, dm_tgt%h(3), imode_z, Lbuf_z)
    !-----------------------------------------
    ! source coordinates
    !-----------------------------------------
    xc_src = dm_src%h(1) * ([(real(i-1,WP) + HALF, i=1,dm_src%nc(1))])
    xp_src = dm_src%h(1) * ([(real(i-1,WP)       , i=1,dm_src%np(1))])

    zc_src = dm_src%h(3) * ([(real(k-1,WP) + HALF, k=1,dm_src%nc(3))])
    zp_src = dm_src%h(3) * ([(real(k-1,WP)       , k=1,dm_src%np(3))])

    ! qx : x-face, y-center, z-center
    call interp_field_3d_generic(dm_src, dm_tgt,                    &
        xp_src, dm_src%yc, zc_src,                                  &
        fl_src%qx, fl_tgt%qx, dm_src%dpcc, dm_tgt%dpcc,             &
        XLOC_FACE, YLOC_CELL, ZLOC_CELL,                            &
        imode_x, Lbuf_x, imode_z, Lbuf_z)

    ! qy : x-center, y-face, z-center
    call interp_field_3d_generic(dm_src, dm_tgt,                    &
        xc_src, dm_src%yp, zc_src,                                  &
        fl_src%qy, fl_tgt%qy, dm_src%dcpc, dm_tgt%dcpc,             &
        XLOC_CELL, YLOC_FACE, ZLOC_CELL,                            &
        imode_x, Lbuf_x, imode_z, Lbuf_z)

    ! qz : x-center, y-center, z-face
    call interp_field_3d_generic(dm_src, dm_tgt,                    &
        xc_src, dm_src%yc, zp_src,                                  &
        fl_src%qz, fl_tgt%qz, dm_src%dccp, dm_tgt%dccp,             &
        XLOC_CELL, YLOC_CELL, ZLOC_FACE,                            &
        imode_x, Lbuf_x, imode_z, Lbuf_z)

    ! pressure : x-center, y-center, z-center
    call interp_field_3d_generic(dm_src, dm_tgt,                     &
        xc_src, dm_src%yc, zc_src,                                  &
        fl_src%pres, fl_tgt%pres, dm_src%dccc, dm_tgt%dccc,         &
        XLOC_CELL, YLOC_CELL, ZLOC_CELL,                            &
        imode_x, Lbuf_x, imode_z, Lbuf_z)
    !--------------------------------------------------------------------------
    ! Conservative momentum g = rho*u.
    !
    ! g is remapped in its own right rather than rebuilt as q*rho on the target.
    ! g, not q, is the primary field of a variable-property run: the projection
    ! makes g discretely divergence free against -d(rho)/dt, and eq_momentum2
    ! re-derives q from g (IG2Q) at every substep. Rebuilding g on the target
    ! would need the target's full face-interpolation and boundary machinery,
    ! and would still only agree with the remapped g to the O(dx^2) of the
    ! interpolation - which the first projection substep removes anyway.
    !--------------------------------------------------------------------------
    if(dm_tgt%is_thermo) then
      ! gx : x-face, y-center, z-center
      call interp_field_3d_generic(dm_src, dm_tgt,                  &
          xp_src, dm_src%yc, zc_src,                                &
          fl_src%gx, fl_tgt%gx, dm_src%dpcc, dm_tgt%dpcc,           &
          XLOC_FACE, YLOC_CELL, ZLOC_CELL,                          &
          imode_x, Lbuf_x, imode_z, Lbuf_z)
      ! gy : x-center, y-face, z-center
      call interp_field_3d_generic(dm_src, dm_tgt,                  &
          xc_src, dm_src%yp, zc_src,                                &
          fl_src%gy, fl_tgt%gy, dm_src%dcpc, dm_tgt%dcpc,           &
          XLOC_CELL, YLOC_FACE, ZLOC_CELL,                          &
          imode_x, Lbuf_x, imode_z, Lbuf_z)
      ! gz : x-center, y-center, z-face
      call interp_field_3d_generic(dm_src, dm_tgt,                  &
          xc_src, dm_src%yc, zp_src,                                &
          fl_src%gz, fl_tgt%gz, dm_src%dccp, dm_tgt%dccp,           &
          XLOC_CELL, YLOC_CELL, ZLOC_FACE,                          &
          imode_x, Lbuf_x, imode_z, Lbuf_z)
    end if
    !--------------------------------------------------------------------------
    ! AB2 momentum history.
    !
    ! m*_rhs0 holds the previous substep's raw convection+viscous RHS, with no
    ! dt factor (Calculate_momentum_fractional_step), so it is an ordinary
    ! smooth field on the same staggered locations as q and is remapped like
    ! one. Zeroing it instead would leave the first restarted AB2 step
    ! evaluating tGamma*rhs + tZeta*0 - a 1.5x over-step, not a restart. The
    ! remapped history is the source-mesh operator sampled on the target mesh,
    ! which differs from the target-mesh operator at O(dx^2); it enters weighted
    ! by tZeta*dt, so the resulting error is well below the remap's own.
    !--------------------------------------------------------------------------
    if(is_restart_history_exact(dm_tgt)) then
      call interp_field_3d_generic(dm_src, dm_tgt,                  &
          xp_src, dm_src%yc, zc_src,                                &
          fl_src%mx_rhs0, fl_tgt%mx_rhs0, dm_src%dpcc, dm_tgt%dpcc, &
          XLOC_FACE, YLOC_CELL, ZLOC_CELL,                          &
          imode_x, Lbuf_x, imode_z, Lbuf_z)
      call interp_field_3d_generic(dm_src, dm_tgt,                  &
          xc_src, dm_src%yp, zc_src,                                &
          fl_src%my_rhs0, fl_tgt%my_rhs0, dm_src%dcpc, dm_tgt%dcpc, &
          XLOC_CELL, YLOC_FACE, ZLOC_CELL,                          &
          imode_x, Lbuf_x, imode_z, Lbuf_z)
      call interp_field_3d_generic(dm_src, dm_tgt,                  &
          xc_src, dm_src%yc, zp_src,                                &
          fl_src%mz_rhs0, fl_tgt%mz_rhs0, dm_src%dccp, dm_tgt%dccp, &
          XLOC_CELL, YLOC_CELL, ZLOC_FACE,                          &
          imode_x, Lbuf_x, imode_z, Lbuf_z)
    end if

    call preserve_interp_streamwise_bulk(dm_src, fl_src, dm_tgt, fl_tgt)
    return
  end subroutine build_up_interp_target_field_flow
!==============================================================================
  !> Preserve the source bulk streamwise velocity/mass flux after interpolation.
  !>
  !> The measured component is the one a restart normalises to unity - g for a
  !> variable-property run, q otherwise, z-streamwise for a duct - so
  !> measure_streamwise_bulk decides it, exactly as restore_flow_variables_from_
  !> restart does. For a thermal run q and g are scaled by the same factor so
  !> that g = q*rho still holds pointwise.
  subroutine preserve_interp_streamwise_bulk(dm_src, fl_src, dm_tgt, fl_tgt)
    use io_restart_mod, only: measure_streamwise_bulk
    use mpi_mod, only: nrank
    use parameters_constant_mod
    use print_msg_mod
    use udf_type_mod
    implicit none

    type(t_domain), intent(in)    :: dm_src
    type(t_flow)  , intent(in)    :: fl_src
    type(t_domain), intent(in)    :: dm_tgt
    type(t_flow)  , intent(inout) :: fl_tgt

    real(WP) :: bulk_src, bulk_tgt, bulk_chk
    real(WP) :: correction
    real(WP) :: tol
    logical  :: is_z_streamwise
    character(2) :: bulk_var

    is_z_streamwise = (dm_src%icase == ICASE_DUCT)

    call measure_streamwise_bulk(fl_src, dm_src, is_z_streamwise, bulk_var, bulk_src)
    call measure_streamwise_bulk(fl_tgt, dm_tgt, is_z_streamwise, bulk_var, bulk_tgt)

    tol = max(MINP, MINP * abs(bulk_src))
    if(abs(bulk_src) > tol .and. abs(bulk_tgt) > tol) then
      correction = bulk_src / bulk_tgt
      if(is_z_streamwise) then
        fl_tgt%qz = fl_tgt%qz * correction
        if(dm_tgt%is_thermo) fl_tgt%gz = fl_tgt%gz * correction
      else
        fl_tgt%qx = fl_tgt%qx * correction
        if(dm_tgt%is_thermo) fl_tgt%gx = fl_tgt%gx * correction
      end if
    else
      !------------------------------------------------------------------------
      ! Degenerate case: a bulk of zero cannot be restored by scaling. Shift
      ! instead. An additive shift does not commute with g = q*rho, so it is
      ! applied to the measured component only and the pair is left to the
      ! first IG2Q of the restarted run.
      !------------------------------------------------------------------------
      correction = bulk_src - bulk_tgt
      if(is_z_streamwise) then
        if(dm_tgt%is_thermo) then
          fl_tgt%gz = fl_tgt%gz + correction
        else
          fl_tgt%qz = fl_tgt%qz + correction
        end if
      else
        if(dm_tgt%is_thermo) then
          fl_tgt%gx = fl_tgt%gx + correction
        else
          fl_tgt%qx = fl_tgt%qx + correction
        end if
      end if
    end if

    call measure_streamwise_bulk(fl_tgt, dm_tgt, is_z_streamwise, bulk_var, bulk_chk)

    tol = max(1.0e-10_wp, 1.0e-10_wp * abs(bulk_src))
    if(nrank == 0 .and. abs(bulk_chk - bulk_src) > tol) then
      call Print_warning_msg("Interpolated streamwise bulk "//trim(bulk_var)//" was not fully preserved.")
    end if

    return
  end subroutine preserve_interp_streamwise_bulk
!==============================================================================
  !> Interpolate source thermal fields onto the target mesh.
  !>
  !> Only the two independent thermodynamic fields are remapped: rhoh (the
  !> conserved energy variable) and the density that goes with it. Every other
  !> property - T, h, k, sigma_e, mu, and the density itself - is then rebuilt
  !> from the property table by the same (rhoh, d) lookup Update_thermal_
  !> properties uses each step. Remapping them independently would write a
  !> property set that is not on the table's manifold, and
  !> restore_thermo_variables_from_restart trusts the stored set verbatim
  !> rather than re-deriving it, so the inconsistency would survive the restart.
  !>
  !> - tm_src (in): Source thermal field.
  !> - fl_src (in): Source flow field, for the source density.
  !> - dm_src (in): Source domain descriptor.
  !> - tm_tgt (inout): Target thermal field.
  !> - fl_tgt (inout): Target flow field, receiving density and viscosity.
  !> - dm_tgt (in): Target domain descriptor.
  subroutine build_up_interp_target_field_thermo(tm_src, fl_src, dm_src, tm_tgt, fl_tgt, dm_tgt)
    use io_restart_mod, only: is_restart_history_exact
    use parameters_constant_mod
    use print_msg_mod
    use thermo_info_mod, only: ftp_refresh_thermal_properties_from_DH, t_fluidThermoProperty
    use udf_type_mod
    implicit none
    !
    type(t_domain), intent(in)    :: dm_src
    type(t_thermo), intent(in)    :: tm_src
    type(t_flow)  , intent(in)    :: fl_src
    type(t_domain), intent(inout) :: dm_tgt
    type(t_thermo), intent(inout) :: tm_tgt
    type(t_flow)  , intent(inout) :: fl_tgt
    !
    integer  :: imode_x, imode_z, i, j, k
    real(WP) :: Lbuf_x, Lbuf_z
    real(WP) :: xc_src(dm_src%nc(1)), zc_src(dm_src%nc(3))
    real(WP) :: xp_src(dm_src%np(1)), zp_src(dm_src%np(3))
    type(t_fluidThermoProperty) :: ftp
    !
    if (abs(dm_src%lyt - dm_tgt%lyt) > 1.0e-10_wp) then
      call print_error_msg("build_up_interp_target_field_flow: cross-section mismatch in yt")
    end if
    if (abs(dm_src%lyb - dm_tgt%lyb) > 1.0e-10_wp) then
      call print_error_msg("build_up_interp_target_field_flow: cross-section mismatch in yb")
    end if
    !-----------------------------------------
    ! extension controls
    !-----------------------------------------
    call setup_extension_mapping(dm_src%lxx, dm_tgt%lxx, dm_tgt%h(1), imode_x, Lbuf_x)
    call setup_extension_mapping(dm_src%lzz, dm_tgt%lzz, dm_tgt%h(3), imode_z, Lbuf_z)
    !-----------------------------------------
    ! source coordinates
    !-----------------------------------------
    xc_src = dm_src%h(1) * ([(real(i-1,WP) + HALF, i=1,dm_src%nc(1))])
    xp_src = dm_src%h(1) * ([(real(i-1,WP)       , i=1,dm_src%np(1))])

    zc_src = dm_src%h(3) * ([(real(k-1,WP) + HALF, k=1,dm_src%nc(3))])
    zp_src = dm_src%h(3) * ([(real(k-1,WP)       , k=1,dm_src%np(3))])

    ! rhoh : x-center, y-center, z-center
    call interp_field_3d_generic(dm_src, dm_tgt,                     &
        xc_src, dm_src%yc, zc_src,                                  &
        tm_src%rhoh, tm_tgt%rhoh, dm_src%dccc, dm_tgt%dccc,         &
        XLOC_CELL, YLOC_CELL, ZLOC_CELL,                            &
        imode_x, Lbuf_x, imode_z, Lbuf_z)

    ! density : x-center, y-center, z-center. The table lookup below takes it
    ! as the starting point for the inverse (rhoh, d) -> state solve, so it has
    ! to be the remapped field, not a uniform guess.
    call interp_field_3d_generic(dm_src, dm_tgt,                     &
        xc_src, dm_src%yc, zc_src,                                  &
        fl_src%dDens, fl_tgt%dDens, dm_src%dccc, dm_tgt%dccc,       &
        XLOC_CELL, YLOC_CELL, ZLOC_CELL,                            &
        imode_x, Lbuf_x, imode_z, Lbuf_z)

    ! AB2 energy history - see the momentum history in
    ! build_up_interp_target_field_flow for why it is remapped and not zeroed.
    if(is_restart_history_exact(dm_tgt)) then
      call interp_field_3d_generic(dm_src, dm_tgt,                   &
          xc_src, dm_src%yc, zc_src,                                &
          tm_src%ene_rhs0, tm_tgt%ene_rhs0, dm_src%dccc, dm_tgt%dccc, &
          XLOC_CELL, YLOC_CELL, ZLOC_CELL,                          &
          imode_x, Lbuf_x, imode_z, Lbuf_z)
    end if
    !--------------------------------------------------------------------------
    ! Rebuild the dependent properties on the table manifold. Same recipe as
    ! Update_thermal_properties: (rhoh, d) in, the full state out, including a
    ! density corrected onto the table.
    !
    ! rho*h is then stored from that state, rhoh = rho_EOS(h) h (ftp%rhoh), not
    ! kept at its interpolated value: output_interp_target_field sets
    ! rho_hat = rho_EOS, and the interpolated pair (rhoh_i, rho_i) gives
    ! h = rhoh_i/rho_i with rho_i /= rho_EOS(h) wherever the interpolation cuts
    ! across a density gradient, so the first recovery h = rhoh/rho_hat of the
    ! remapped run would jump by h (rho_i - rho_EOS)/rho_EOS
    ! (tests/restart_remap_thermo). This is thermodynamic consistency, not
    ! conservation: local rho*h changes by h (rho_EOS - rho_i), and the
    ! trilinear interpolation is not conservative either.
    !--------------------------------------------------------------------------
    do k = 1, dm_tgt%dccc%xsz(3)
      do j = 1, dm_tgt%dccc%xsz(2)
        do i = 1, dm_tgt%dccc%xsz(1)
          ftp%rhoh = tm_tgt%rhoh(i, j, k)
          ftp%d    = fl_tgt%dDens(i, j, k)
          call ftp_refresh_thermal_properties_from_DH(ftp)
          tm_tgt%rhoh (i, j, k) = ftp%rhoh
          tm_tgt%hEnth(i, j, k) = ftp%h
          tm_tgt%tTemp(i, j, k) = ftp%T
          tm_tgt%kCond(i, j, k) = ftp%k
          tm_tgt%eCond(i, j, k) = ftp%sigma_e
          fl_tgt%dDens(i, j, k) = ftp%d
          fl_tgt%mVisc(i, j, k) = ftp%m
        end do
      end do
    end do

    return
  end subroutine build_up_interp_target_field_thermo
!==============================================================================
  subroutine compute_interp_halo_level(dm_tgt, dtmp_tgt, dtmp_src, xsrc, ysrc, zsrc, &
                                      xloc_tgt, yloc_tgt, zloc_tgt,                  &
                                      extend_mode_x, extend_length_x,                &
                                      extend_mode_z, extend_length_z, halo_level)
    use mpi_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    !
    type(t_domain)   , intent(in)  :: dm_tgt
    type(DECOMP_INFO), intent(in)  :: dtmp_tgt
    type(DECOMP_INFO), intent(in)  :: dtmp_src
    real(WP)         , intent(in)  :: xsrc(:), ysrc(:), zsrc(:)
    integer          , intent(in)  :: xloc_tgt, yloc_tgt, zloc_tgt
    integer          , intent(in)  :: extend_mode_x, extend_mode_z
    real(WP)         , intent(in)  :: extend_length_x, extend_length_z
    integer          , intent(out) :: halo_level
    !
    integer :: i, j, k, ii, jj, kk
    integer :: isrc, jsrc, ksrc
    integer :: min_j, max_j, min_k, max_k
    integer :: halo_local
    real(WP) :: x_target, y_target, z_target
    real(WP) :: x_tgt_eff, z_tgt_eff

    min_j = huge(1)
    max_j = -huge(1)
    min_k = huge(1)
    max_k = -huge(1)

    do k = 1, dtmp_tgt%xsz(3)
      kk = dtmp_tgt%xst(3) + k - 1
      z_target = get_coord_from_loc(3, kk, dm_tgt, zloc_tgt)
      z_tgt_eff = map_coord_to_src_bounds(z_target, zsrc(1), zsrc(size(zsrc)), &
                                          extend_length_z, extend_mode_z)

      do j = 1, dtmp_tgt%xsz(2)
        jj = dtmp_tgt%xst(2) + j - 1
        y_target = get_coord_from_loc(2, jj, dm_tgt, yloc_tgt)

        do i = 1, dtmp_tgt%xsz(1)
          ii = dtmp_tgt%xst(1) + i - 1
          x_target = get_coord_from_loc(1, ii, dm_tgt, xloc_tgt)
          x_tgt_eff = map_coord_to_src_bounds(x_target, xsrc(1), xsrc(size(xsrc)), &
                                              extend_length_x, extend_mode_x)

          call binary_search_loc2index(x_tgt_eff, xsrc, isrc)
          call binary_search_loc2index(y_target,  ysrc, jsrc)
          call binary_search_loc2index(z_tgt_eff, zsrc, ksrc)

          jsrc = min(jsrc, size(ysrc) - 1)
          ksrc = min(ksrc, size(zsrc) - 1)

          min_j = min(min_j, jsrc)
          max_j = max(max_j, jsrc + 1)
          min_k = min(min_k, ksrc)
          max_k = max(max_k, ksrc + 1)
        end do
      end do
    end do

    halo_local = 1
    halo_local = max(halo_local, dtmp_src%xst(2) - min_j)
    halo_local = max(halo_local, max_j - dtmp_src%xen(2))
    halo_local = max(halo_local, dtmp_src%xst(3) - min_k)
    halo_local = max(halo_local, max_k - dtmp_src%xen(3))

    call mpi_allreduce(halo_local, halo_level, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierror)
    halo_level = max(1, halo_level)

    return
  end subroutine compute_interp_halo_level
!==============================================================================
  subroutine interp_field_3d_generic(dm_src, dm_tgt, xsrc, ysrc, zsrc, fsrc, ftgt, dsrc, dtmp, &
                                    xloc_tgt, yloc_tgt, zloc_tgt,                           &
                                    extend_mode_x, extend_length_x,                         &
                                    extend_mode_z, extend_length_z)
    use m_halo, only: update_halo
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    !
    type(t_domain)   , intent(in)    :: dm_src
    type(t_domain)   , intent(in)    :: dm_tgt
    type(DECOMP_INFO), intent(in)    :: dsrc
    type(DECOMP_INFO), intent(in)    :: dtmp
    real(WP)         , intent(in)    :: xsrc(:), ysrc(:), zsrc(:)
    real(WP)         , intent(in)    :: fsrc(:, :, :)
    real(WP)         , intent(inout) :: ftgt(:, :, :)
    integer          , intent(in)    :: xloc_tgt, yloc_tgt, zloc_tgt
    integer          , intent(in)    :: extend_mode_x, extend_mode_z
    real(WP)         , intent(in)    :: extend_length_x, extend_length_z
    !
    integer :: i, j, k, ii, jj, kk
    integer :: halo_level
    real(WP) :: x_target, y_target, z_target
    real(WP) :: x_tgt_eff, z_tgt_eff, var_target
    real(WP), allocatable :: fsrc_halo(:, :, :)

    call compute_interp_halo_level(dm_tgt, dtmp, dsrc, xsrc, ysrc, zsrc, &
                                   xloc_tgt, yloc_tgt, zloc_tgt,         &
                                   extend_mode_x, extend_length_x,       &
                                   extend_mode_z, extend_length_z,       &
                                   halo_level)
    call update_halo(fsrc, fsrc_halo, halo_level, dsrc, opt_global=.true., opt_pencil=IPENCIL(1))

    do k = 1, dtmp%xsz(3)
      kk = dtmp%xst(3) + k - 1
      z_target = get_coord_from_loc(3, kk, dm_tgt, zloc_tgt)
      z_tgt_eff = map_coord_to_src_bounds(z_target, zsrc(1), zsrc(size(zsrc)), &
                                          extend_length_z, extend_mode_z)

      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1
        y_target = get_coord_from_loc(2, jj, dm_tgt, yloc_tgt)

        do i = 1, dtmp%xsz(1)
          ii = dtmp%xst(1) + i - 1
          x_target = get_coord_from_loc(1, ii, dm_tgt, xloc_tgt)

          x_tgt_eff = map_coord_to_src_bounds(x_target, xsrc(1), xsrc(size(xsrc)), &
                                              extend_length_x, extend_mode_x)

          call trilinear_interp_point_halo(x_tgt_eff, y_target, z_tgt_eff, &
                                           xsrc, ysrc, zsrc,                &
                                           lbound(fsrc_halo, 1),            &
                                           lbound(fsrc_halo, 2),            &
                                           lbound(fsrc_halo, 3),            &
                                           fsrc_halo, var_target)

          ftgt(i, j, k) = var_target
        end do
      end do
    end do

    deallocate(fsrc_halo)

  end subroutine interp_field_3d_generic
!==============================================================================
  pure function get_coord_from_loc(dir, idx, dm, loc_type) result(coord_val)
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    !
    integer       , intent(in) :: dir
    integer       , intent(in) :: idx
    type(t_domain), intent(in) :: dm
    integer       , intent(in) :: loc_type
    real(WP)                  :: coord_val
    !
    select case (dir)
    !
    case (1)   ! x
      select case (loc_type)
      case (XLOC_CELL)
        coord_val = dm%h(1) * (real(idx - 1, WP) + HALF)
      case (XLOC_FACE)
        coord_val = dm%h(1) *  real(idx - 1, WP)
      end select
    !
    case (2)   ! y
      select case (loc_type)
      case (YLOC_CELL)
        coord_val = dm%yc(idx)
      case (YLOC_FACE)
        coord_val = dm%yp(idx)
      end select
    !
    case (3)   ! z
      select case (loc_type)
      case (ZLOC_CELL)
        coord_val = dm%h(3) * (real(idx - 1, WP) + HALF)
      case (ZLOC_FACE)
        coord_val = dm%h(3) *  real(idx - 1, WP)
      end select
    !
    end select
    return
  end function get_coord_from_loc
!==============================================================================
  pure function map_coord_to_src_bounds(x_tgt, x_min, x_max, Lbuf, mode) result(x_tgt_eff)
    use parameters_constant_mod
    implicit none

    real(WP), intent(in) :: x_tgt, x_min, x_max, Lbuf
    integer , intent(in) :: mode
    real(WP)             :: x_tgt_eff
    real(WP)             :: dx, Lloc, Luse, epsx

    Lloc = x_max - x_min
    epsx = TEN * epsilon(ONE) * max(ONE, abs(x_max))

    select case (mode)

    case (1)
      ! clamp to last valid source position
      x_tgt_eff = min(max(x_tgt, x_min), x_max - epsx)

    case (2)
      ! repeat last chunk
      if (x_tgt <= x_max) then
        x_tgt_eff = min(max(x_tgt, x_min), x_max - epsx)
      else
        if (Lbuf <= ZERO) then
          x_tgt_eff = x_max - epsx
        else
          Luse = min(Lbuf, Lloc)
          dx   = modulo(x_tgt - x_max, Luse)
          x_tgt_eff = x_max - Luse + dx
          x_tgt_eff = min(max(x_tgt_eff, x_min), x_max - epsx)
        end if
      end if

    case default
      x_tgt_eff = min(max(x_tgt, x_min), x_max - epsx)

    end select

  end function map_coord_to_src_bounds
!==============================================================================
  !> Give the target domain everything the restart writers read off it.
  !>
  !> The target descriptor is built from scratch: `Read_input_parameters_target`
  !> sets only the geometry and the mesh, so every other member still holds its
  !> default-initialised (or, for members without an initialiser, undefined)
  !> value. The writers below are driven almost entirely by `t_domain` flags, so
  !> anything left unset here is read as garbage at write time - that is how a
  !> stale `is_conv_outlet(1)` used to send the isothermal pre-run into
  !> `write_flow_restart_xoutlet_state_compact` with unallocated boundary planes.
  !>
  !> The rule is: the target restart must be readable by a run that differs from
  !> the source only in its mesh, so every non-geometry setting is the source's.
  !> `is_record_xoutlet` / `is_read_xinlet` are the exception - the inlet/outlet
  !> databases are plane recordings on the source mesh, they are not remapped
  !> here, and leaving them on would make the decomposition allocate database
  !> buffers the pre-run never fills.
  !>
  !> - dm_src (in): Source domain descriptor.
  !> - dm_tgt (inout): Target domain descriptor.
  subroutine configure_interp_target_domain(dm_src, dm_tgt)
    use domain_decomposition_mod
    use geometry_mod
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(in)    :: dm_src
    type(t_domain), intent(inout) :: dm_tgt

    dm_tgt%idom                      = dm_src%idom
    dm_tgt%dt                        = dm_src%dt
    dm_tgt%is_thermo                 = dm_src%is_thermo
    dm_tgt%is_periodic(:)            = dm_src%is_periodic(:)
    dm_tgt%is_conv_outlet(:)         = dm_src%is_conv_outlet(:)
    dm_tgt%ibcx_qx                   = dm_src%ibcx_qx
    dm_tgt%ibcy_qy                   = dm_src%ibcy_qy
    dm_tgt%ibcz_qz                   = dm_src%ibcz_qz
    dm_tgt%existing_output_policy    = dm_src%existing_output_policy
    dm_tgt%restart_data_layout_read  = dm_src%restart_data_layout_read
    dm_tgt%restart_data_layout_write = dm_src%restart_data_layout_write
    dm_tgt%restart_history_mode      = dm_src%restart_history_mode
    dm_tgt%reset_unit_massflux       = dm_src%reset_unit_massflux
    dm_tgt%is_record_xoutlet         = .false.
    dm_tgt%is_read_xinlet            = .false.

    call Buildup_geometry_mesh_info(dm_tgt)
    call initialise_domain_decomposition(dm_tgt)

    return
  end subroutine configure_interp_target_domain
!==============================================================================
  !> Allocate the target field set the restart writers touch.
  !>
  !> This mirrors `Allocate_flow_variables` / `Allocate_thermo_variables`, but
  !> cannot call them: `initialisation.o` is built after `io_restart.o`. Only the
  !> fields that are written are allocated - the pre-run never advances a
  !> timestep, so the working arrays (`m*_rhs`, `pcor`, `dDens0`, ...) are not
  !> needed. Keep this list in step with `flow_restart_fields` and
  !> `thermo_restart_fields`.
  !>
  !> - fl_tgt (inout): Target flow field.
  !> - tm_tgt (inout): Target thermal field.
  !> - dm_tgt (inout): Target domain descriptor, receiving the boundary planes.
  subroutine allocate_interp_target_variables(fl_tgt, tm_tgt, dm_tgt)
    use boundary_conditions_mod, only: allocate_fbc_flow, allocate_fbc_thermo
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm_tgt
    type(t_flow)  , intent(inout) :: fl_tgt
    type(t_thermo), intent(inout) :: tm_tgt

    call allocate_fbc_flow(dm_tgt)
    call allocate_fbc_thermo(dm_tgt)

    call alloc_x(fl_tgt%qx,      dm_tgt%dpcc) ; fl_tgt%qx      = ZERO
    call alloc_x(fl_tgt%qy,      dm_tgt%dcpc) ; fl_tgt%qy      = ZERO
    call alloc_x(fl_tgt%qz,      dm_tgt%dccp) ; fl_tgt%qz      = ZERO
    call alloc_x(fl_tgt%pres,    dm_tgt%dccc) ; fl_tgt%pres    = ZERO
    call alloc_x(fl_tgt%mx_rhs0, dm_tgt%dpcc) ; fl_tgt%mx_rhs0 = ZERO
    call alloc_x(fl_tgt%my_rhs0, dm_tgt%dcpc) ; fl_tgt%my_rhs0 = ZERO
    call alloc_x(fl_tgt%mz_rhs0, dm_tgt%dccp) ; fl_tgt%mz_rhs0 = ZERO

    if(dm_tgt%is_thermo) then
      call alloc_x(fl_tgt%gx,       dm_tgt%dpcc) ; fl_tgt%gx       = ZERO
      call alloc_x(fl_tgt%gy,       dm_tgt%dcpc) ; fl_tgt%gy       = ZERO
      call alloc_x(fl_tgt%gz,       dm_tgt%dccp) ; fl_tgt%gz       = ZERO
      call alloc_x(fl_tgt%dDens,    dm_tgt%dccc) ; fl_tgt%dDens    = ONE
      call alloc_x(fl_tgt%mVisc,    dm_tgt%dccc) ; fl_tgt%mVisc    = ONE
      call alloc_x(tm_tgt%rhoh,     dm_tgt%dccc) ; tm_tgt%rhoh     = ZERO
      call alloc_x(tm_tgt%hEnth,    dm_tgt%dccc) ; tm_tgt%hEnth    = ZERO
      call alloc_x(tm_tgt%tTemp,    dm_tgt%dccc) ; tm_tgt%tTemp    = ONE
      call alloc_x(tm_tgt%kCond,    dm_tgt%dccc) ; tm_tgt%kCond    = ONE
      call alloc_x(tm_tgt%eCond,    dm_tgt%dccc) ; tm_tgt%eCond    = ONE
      call alloc_x(tm_tgt%ene_rhs0, dm_tgt%dccc) ; tm_tgt%ene_rhs0 = ZERO
      if(is_bc_recovery) then
        call alloc_x(fl_tgt%dDens_hat, dm_tgt%dccc) ; fl_tgt%dDens_hat = ONE
        call alloc_x(fl_tgt%drhodt_m1, dm_tgt%dccc) ; fl_tgt%drhodt_m1 = ZERO
      end if
    end if

    if(dm_tgt%is_conv_outlet(1)) then
      allocate (fl_tgt%fbcx_a0cc_rhs0(dm_tgt%dpcc%xsz(2), dm_tgt%dpcc%xsz(3))); fl_tgt%fbcx_a0cc_rhs0 = ZERO
      allocate (fl_tgt%fbcx_a0pc_rhs0(dm_tgt%dcpc%xsz(2), dm_tgt%dcpc%xsz(3))); fl_tgt%fbcx_a0pc_rhs0 = ZERO
      allocate (fl_tgt%fbcx_a0cp_rhs0(dm_tgt%dccp%xsz(2), dm_tgt%dccp%xsz(3))); fl_tgt%fbcx_a0cp_rhs0 = ZERO
      if(dm_tgt%is_thermo) then
        allocate (tm_tgt%fbcx_rhoh_rhs0(dm_tgt%dccc%xsz(2), dm_tgt%dccc%xsz(3))); tm_tgt%fbcx_rhoh_rhs0 = ZERO
        allocate (tm_tgt%fbcx_dhat(4, dm_tgt%dccc%xsz(2), dm_tgt%dccc%xsz(3))); tm_tgt%fbcx_dhat = ONE
        allocate (tm_tgt%fbcx_dhat_rhs0(dm_tgt%dccc%xsz(2), dm_tgt%dccc%xsz(3))); tm_tgt%fbcx_dhat_rhs0 = ZERO
      end if
    end if

    return
  end subroutine allocate_interp_target_variables
!==============================================================================
  !> Remap one x-boundary plane set onto the target cross-section.
  !>
  !> An fbcx array is (nslot, ny, nz): a handful of x-boundary layers, each of
  !> them a profile over the cross-section. The slot index is a boundary layer,
  !> not a coordinate, so slots are carried across one for one and only y and z
  !> are remapped - x clamping/extension never applies here. The y extent is
  !> identical on both meshes (asserted in build_up_interp_target_field_flow),
  !> so only z can need the extension treatment.
  !>
  !> - dm_tgt (in): Target domain descriptor, supplying the target y/z coordinates.
  !> - ysrc, zsrc (in): Source coordinates of the plane's storage location.
  !> - fsrc (in): Source plane set, x-pencil distributed in y and z.
  !> - ftgt (inout): Target plane set.
  !> - dsrc, dtgt (in): Their decompositions (d4** for boundary values, d1** for history).
  !> - yloc_tgt, zloc_tgt (in): Target storage location codes.
  !> - extend_mode_z, extend_length_z (in): z extension controls, as in the 3-D remap.
  subroutine interp_fbcx_plane_generic(dm_tgt, ysrc, zsrc, fsrc, ftgt, dsrc, dtgt, &
                                       yloc_tgt, zloc_tgt, extend_mode_z, extend_length_z)
    use m_halo, only: update_halo
    use mpi_mod
    use parameters_constant_mod
    use print_msg_mod
    use udf_type_mod
    implicit none
    type(t_domain)   , intent(in)    :: dm_tgt
    real(WP)         , intent(in)    :: ysrc(:), zsrc(:)
    real(WP)         , intent(in)    :: fsrc(:, :, :)
    real(WP)         , intent(inout) :: ftgt(:, :, :)
    type(DECOMP_INFO), intent(in)    :: dsrc, dtgt
    integer          , intent(in)    :: yloc_tgt, zloc_tgt
    integer          , intent(in)    :: extend_mode_z
    real(WP)         , intent(in)    :: extend_length_z
    !
    integer  :: j, k, n, jj, kk, jsrc, ksrc, nslot
    integer  :: min_j, max_j, min_k, max_k, halo_local, halo_level
    real(WP) :: y_target, z_target, z_tgt_eff
    real(WP) :: dy, dz, eta, zeta, c00, c01, c10, c11
    real(WP), allocatable :: fsrc_halo(:, :, :)

    nslot = size(fsrc, 1)
    if(size(ftgt, 1) /= nslot) &
      call Print_error_msg('interp_fbcx_plane_generic: boundary-layer count mismatch.')
    !--------------------------------------------------------------------------
    ! halo depth: how far outside this rank's source slab the target points reach
    !--------------------------------------------------------------------------
    min_j =  huge(1); max_j = -huge(1)
    min_k =  huge(1); max_k = -huge(1)
    do j = 1, dtgt%xsz(2)
      jj = dtgt%xst(2) + j - 1
      y_target = get_coord_from_loc(2, jj, dm_tgt, yloc_tgt)
      call binary_search_loc2index(y_target, ysrc, jsrc)
      jsrc = min(jsrc, size(ysrc) - 1)
      min_j = min(min_j, jsrc)
      max_j = max(max_j, jsrc + 1)
    end do
    do k = 1, dtgt%xsz(3)
      kk = dtgt%xst(3) + k - 1
      z_target  = get_coord_from_loc(3, kk, dm_tgt, zloc_tgt)
      z_tgt_eff = map_coord_to_src_bounds(z_target, zsrc(1), zsrc(size(zsrc)), &
                                          extend_length_z, extend_mode_z)
      call binary_search_loc2index(z_tgt_eff, zsrc, ksrc)
      ksrc = min(ksrc, size(zsrc) - 1)
      min_k = min(min_k, ksrc)
      max_k = max(max_k, ksrc + 1)
    end do
    halo_local = 1
    halo_local = max(halo_local, dsrc%xst(2) - min_j)
    halo_local = max(halo_local, max_j - dsrc%xen(2))
    halo_local = max(halo_local, dsrc%xst(3) - min_k)
    halo_local = max(halo_local, max_k - dsrc%xen(3))
    call mpi_allreduce(halo_local, halo_level, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierror)
    halo_level = max(1, halo_level)

    call update_halo(fsrc, fsrc_halo, halo_level, dsrc, opt_global=.true., opt_pencil=IPENCIL(1))
    !--------------------------------------------------------------------------
    ! bilinear remap in (y, z), identical in form to the (y, z) part of
    ! trilinear_interp_point_halo
    !--------------------------------------------------------------------------
    do k = 1, dtgt%xsz(3)
      kk = dtgt%xst(3) + k - 1
      z_target  = get_coord_from_loc(3, kk, dm_tgt, zloc_tgt)
      z_tgt_eff = map_coord_to_src_bounds(z_target, zsrc(1), zsrc(size(zsrc)), &
                                          extend_length_z, extend_mode_z)
      call binary_search_loc2index(z_tgt_eff, zsrc, ksrc)
      ksrc = min(ksrc, size(zsrc) - 1)
      dz = zsrc(ksrc + 1) - zsrc(ksrc)
      if(dabs(dz) <= MINP) dz = ONE
      zeta = max(ZERO, min(ONE, (z_tgt_eff - zsrc(ksrc)) / dz))

      do j = 1, dtgt%xsz(2)
        jj = dtgt%xst(2) + j - 1
        y_target = get_coord_from_loc(2, jj, dm_tgt, yloc_tgt)
        call binary_search_loc2index(y_target, ysrc, jsrc)
        jsrc = min(jsrc, size(ysrc) - 1)
        dy = ysrc(jsrc + 1) - ysrc(jsrc)
        if(dabs(dy) <= MINP) dy = ONE
        eta = max(ZERO, min(ONE, (y_target - ysrc(jsrc)) / dy))

        if(jsrc < lbound(fsrc_halo, 2) .or. jsrc + 1 > ubound(fsrc_halo, 2) .or. &
           ksrc < lbound(fsrc_halo, 3) .or. ksrc + 1 > ubound(fsrc_halo, 3)) &
          call Print_error_msg('interp_fbcx_plane_generic: source halo is too small.')

        do n = 1, nslot
          c00 = fsrc_halo(n, jsrc    , ksrc    )
          c01 = fsrc_halo(n, jsrc    , ksrc + 1)
          c10 = fsrc_halo(n, jsrc + 1, ksrc    )
          c11 = fsrc_halo(n, jsrc + 1, ksrc + 1)
          ftgt(n, j, k) = (c00 * (ONE - zeta) + c01 * zeta) * (ONE - eta) + &
                          (c10 * (ONE - zeta) + c11 * zeta) * eta
        end do
      end do
    end do

    deallocate(fsrc_halo)

    return
  end subroutine interp_fbcx_plane_generic
!==============================================================================
  !> Remap a single-layer x-boundary history plane, stored as a bare (y, z) array.
  !>
  !> Thin shape adapter over interp_fbcx_plane_generic: the convective-outlet
  !> AB2 histories are held as 2-D cross-sections in t_flow but are decomposed
  !> and written as one-layer d1** plane sets.
  subroutine interp_fbcx_history_plane(dm_tgt, ysrc, zsrc, hsrc, htgt, dsrc, dtgt, &
                                       yloc_tgt, zloc_tgt, extend_mode_z, extend_length_z)
    use parameters_constant_mod
    use udf_type_mod
    implicit none
    type(t_domain)   , intent(in)    :: dm_tgt
    real(WP)         , intent(in)    :: ysrc(:), zsrc(:)
    real(WP)         , intent(in)    :: hsrc(:, :)
    real(WP)         , intent(inout) :: htgt(:, :)
    type(DECOMP_INFO), intent(in)    :: dsrc, dtgt
    integer          , intent(in)    :: yloc_tgt, zloc_tgt
    integer          , intent(in)    :: extend_mode_z
    real(WP)         , intent(in)    :: extend_length_z
    !
    real(WP) :: p_src(1, size(hsrc, 1), size(hsrc, 2))
    real(WP) :: p_tgt(1, size(htgt, 1), size(htgt, 2))

    p_src(1, :, :) = hsrc(:, :)
    p_tgt = ZERO
    call interp_fbcx_plane_generic(dm_tgt, ysrc, zsrc, p_src, p_tgt, dsrc, dtgt, &
                                   yloc_tgt, zloc_tgt, extend_mode_z, extend_length_z)
    htgt(:, :) = p_tgt(1, :, :)

    return
  end subroutine interp_fbcx_history_plane
!==============================================================================
  !> Carry the stored convective-outlet state across to the target mesh.
  !>
  !> A convective outlet integrates its own boundary plane in time, so a restart
  !> has to carry that plane; `read_flow_restart_xoutlet_state` puts it straight
  !> back into `dm%fbcx_*` and `fl%fbcx_a0*_rhs0`, and the first substep of the
  !> restarted run uses it before any boundary routine has a chance to rebuild
  !> it. The plane is therefore remapped from the source, not re-derived from the
  !> target interior.
  !>
  !> Re-deriving it was the earlier attempt and it is wrong. The stored layers
  !> are not "the interior field evaluated at i = 1 and i = nx": at an inlet the
  !> transverse layers are the prescribed boundary values (zero cross-flow for a
  !> Poiseuille inlet, whatever the source run holds in general), the streamwise
  !> layer at the inlet is the prescribed profile, and the outlet layers are the
  !> convective condition's own integrated state. Substituting first/last cell
  !> centres for those turned an identity remap - target mesh equal to source
  !> mesh - into a run whose inlet flux was wrong by the whole inlet mass flux,
  !> and which then relaxed back onto the analytic inlet profile.
  !>
  !> Each stored quantity is remapped in exactly the form it is stored in, the
  !> same rule the interior remap follows: `fbcx_ftp` carries (d, rhoh) and the
  !> rest of the boundary thermal state is rebuilt from the property table, as
  !> `read_flow_restart_xoutlet_state` does on read.
  !>
  !> One approximation is left in: `preserve_interp_streamwise_bulk` rescales the
  !> interior by 1 + O(interpolation error) and that factor is not applied to
  !> these planes, so inlet flux and interior bulk differ at that order. The
  !> outlet mass balance (`enforce_domain_mass_balance_dyn_fbc`) drives the
  !> global residual to zero every step regardless, so this is a small initial
  !> transient, not a conservation error.
  !>
  !> - dm_src (in): Source domain descriptor, holding the source boundary planes.
  !> - fl_src (in): Source flow field, holding the outlet AB2 histories.
  !> - dm_tgt (inout): Target domain descriptor, receiving the planes.
  !> - fl_tgt (inout): Target flow field, receiving the histories.
  subroutine build_up_interp_target_xoutlet_state(dm_src, fl_src, dm_tgt, fl_tgt)
    use parameters_constant_mod
    use thermo_info_mod, only: ftp_refresh_thermal_properties_from_DH
    use udf_type_mod
    implicit none
    type(t_domain), intent(in)    :: dm_src
    type(t_flow)  , intent(in)    :: fl_src
    type(t_domain), intent(inout) :: dm_tgt
    type(t_flow)  , intent(inout) :: fl_tgt
    !
    integer  :: imode_z, j, k, n
    real(WP) :: Lbuf_z
    real(WP) :: zc_src(dm_src%nc(3)), zp_src(dm_src%np(3))
    real(WP) :: ftp_src(4, dm_src%d4cc%xsz(2), dm_src%d4cc%xsz(3))
    real(WP) :: ftp_tgt(4, dm_tgt%d4cc%xsz(2), dm_tgt%d4cc%xsz(3))

    if(.not. dm_tgt%is_conv_outlet(1)) return

    call setup_extension_mapping(dm_src%lzz, dm_tgt%lzz, dm_tgt%h(3), imode_z, Lbuf_z)
    zc_src = dm_src%h(3) * ([(real(k-1,WP) + HALF, k=1,dm_src%nc(3))])
    zp_src = dm_src%h(3) * ([(real(k-1,WP)       , k=1,dm_src%np(3))])
    !--------------------------------------------------------------------------
    ! boundary values of the primitive velocities
    !--------------------------------------------------------------------------
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yc, zc_src,               &
        dm_src%fbcx_qx, dm_tgt%fbcx_qx, dm_src%d4cc, dm_tgt%d4cc,           &
        YLOC_CELL, ZLOC_CELL, imode_z, Lbuf_z)
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yp, zc_src,               &
        dm_src%fbcx_qy, dm_tgt%fbcx_qy, dm_src%d4pc, dm_tgt%d4pc,           &
        YLOC_FACE, ZLOC_CELL, imode_z, Lbuf_z)
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yc, zp_src,               &
        dm_src%fbcx_qz, dm_tgt%fbcx_qz, dm_src%d4cp, dm_tgt%d4cp,           &
        YLOC_CELL, ZLOC_FACE, imode_z, Lbuf_z)
    !--------------------------------------------------------------------------
    ! outlet AB2 histories (written only by the exact-history layout, but they
    ! cost nothing to fill and the compact reader zeroes them itself)
    !--------------------------------------------------------------------------
    call interp_fbcx_history_plane(dm_tgt, dm_src%yc, zc_src,               &
        fl_src%fbcx_a0cc_rhs0, fl_tgt%fbcx_a0cc_rhs0, dm_src%d1cc, dm_tgt%d1cc, &
        YLOC_CELL, ZLOC_CELL, imode_z, Lbuf_z)
    call interp_fbcx_history_plane(dm_tgt, dm_src%yp, zc_src,               &
        fl_src%fbcx_a0pc_rhs0, fl_tgt%fbcx_a0pc_rhs0, dm_src%d1pc, dm_tgt%d1pc, &
        YLOC_FACE, ZLOC_CELL, imode_z, Lbuf_z)
    call interp_fbcx_history_plane(dm_tgt, dm_src%yc, zp_src,               &
        fl_src%fbcx_a0cp_rhs0, fl_tgt%fbcx_a0cp_rhs0, dm_src%d1cp, dm_tgt%d1cp, &
        YLOC_CELL, ZLOC_FACE, imode_z, Lbuf_z)

    if(.not. dm_tgt%is_thermo) return
    !--------------------------------------------------------------------------
    ! boundary values of the conservative momentum
    !--------------------------------------------------------------------------
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yc, zc_src,               &
        dm_src%fbcx_gx, dm_tgt%fbcx_gx, dm_src%d4cc, dm_tgt%d4cc,           &
        YLOC_CELL, ZLOC_CELL, imode_z, Lbuf_z)
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yp, zc_src,               &
        dm_src%fbcx_gy, dm_tgt%fbcx_gy, dm_src%d4pc, dm_tgt%d4pc,           &
        YLOC_FACE, ZLOC_CELL, imode_z, Lbuf_z)
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yc, zp_src,               &
        dm_src%fbcx_gz, dm_tgt%fbcx_gz, dm_src%d4cp, dm_tgt%d4cp,           &
        YLOC_CELL, ZLOC_FACE, imode_z, Lbuf_z)
    !--------------------------------------------------------------------------
    ! boundary thermal state: only (d, rhoh) are independent, the rest of the
    ! ftp record follows from the property table
    !--------------------------------------------------------------------------
    do k = 1, dm_src%d4cc%xsz(3)
      do j = 1, dm_src%d4cc%xsz(2)
        do n = 1, 4
          ftp_src(n, j, k) = dm_src%fbcx_ftp(n, j, k)%d
        end do
      end do
    end do
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yc, zc_src, ftp_src, ftp_tgt, &
        dm_src%d4cc, dm_tgt%d4cc, YLOC_CELL, ZLOC_CELL, imode_z, Lbuf_z)
    do k = 1, dm_tgt%d4cc%xsz(3)
      do j = 1, dm_tgt%d4cc%xsz(2)
        do n = 1, 4
          dm_tgt%fbcx_ftp(n, j, k)%d = ftp_tgt(n, j, k)
        end do
      end do
    end do

    do k = 1, dm_src%d4cc%xsz(3)
      do j = 1, dm_src%d4cc%xsz(2)
        do n = 1, 4
          ftp_src(n, j, k) = dm_src%fbcx_ftp(n, j, k)%rhoh
        end do
      end do
    end do
    call interp_fbcx_plane_generic(dm_tgt, dm_src%yc, zc_src, ftp_src, ftp_tgt, &
        dm_src%d4cc, dm_tgt%d4cc, YLOC_CELL, ZLOC_CELL, imode_z, Lbuf_z)
    do k = 1, dm_tgt%d4cc%xsz(3)
      do j = 1, dm_tgt%d4cc%xsz(2)
        do n = 1, 4
          dm_tgt%fbcx_ftp(n, j, k)%rhoh = ftp_tgt(n, j, k)
          call ftp_refresh_thermal_properties_from_DH(dm_tgt%fbcx_ftp(n, j, k))
        end do
      end do
    end do

    return
  end subroutine build_up_interp_target_xoutlet_state
!==============================================================================
  !> Driver for writing interpolated target restart fields.
  !> - dm_src (in): Source domain descriptor.
  !> - fl_src (in): Source flow field.
  !> - tm_src (in): Source thermal field.
  subroutine output_interp_target_field(dm_src, fl_src, tm_src)
    use input_general_mod, only: Read_input_parameters_target
    use io_files_mod
    use io_restart_mod
    use parameters_constant_mod
    use print_msg_mod
    use thermo_info_mod, only: check_property_range
    use udf_type_mod
   !use visualisation_field_mod
    implicit none
    type(t_domain), intent(in) :: dm_src
    type(t_flow)  , intent(in) :: fl_src
    type(t_thermo), intent(in), optional :: tm_src

    if(.not.file_exists(trim(input_tgt))) then
      call Print_warning_msg('No field interpolation is carried out.')
      return
    end if

    call Read_input_parameters_target(domain_tgt, input_tgt)
    call configure_interp_target_domain(dm_src, domain_tgt)
    call allocate_interp_target_variables(flow_tgt, thermo_tgt, domain_tgt)
    !--------------------------------------------------------------------------
    ! The remapped field is a fresh start: it is written as iteration 0 so the
    ! follow-on run restarts from 0 on the new mesh, and the source time is not
    ! carried over because the two runs no longer share a trajectory.
    !--------------------------------------------------------------------------
    flow_tgt%iteration   = 0
    flow_tgt%time        = ZERO
    thermo_tgt%iteration = 0
    thermo_tgt%time      = ZERO

    call build_up_interp_target_field_flow(fl_src, dm_src, flow_tgt, domain_tgt)
    if(domain_tgt%is_thermo) then
      if(.not. present(tm_src)) &
      call Print_error_msg("A thermal interpolation target needs the source thermal field.")
      call build_up_interp_target_field_thermo(tm_src, fl_src, dm_src, thermo_tgt, flow_tgt, domain_tgt)
    end if
    call build_up_interp_target_xoutlet_state(dm_src, fl_src, domain_tgt, flow_tgt)
    !--------------------------------------------------------------------------
    ! Bc state of the remapped fresh start: rho_hat = rho (cells and outlet
    ! plane) and a zero rho_t history, i.e. the fresh-initialisation state.
    !--------------------------------------------------------------------------
    if(domain_tgt%is_thermo .and. is_bc_recovery) then
      flow_tgt%dDens_hat = flow_tgt%dDens
      flow_tgt%drhodt_m1 = ZERO
      if(domain_tgt%is_conv_outlet(1)) then
        thermo_tgt%fbcx_dhat = domain_tgt%fbcx_ftp(:, :, :)%d
        thermo_tgt%is_fbcx_dhat_valid = .true.
        thermo_tgt%fbcx_dhat_rhs0 = ZERO
      end if
    end if
    if(domain_tgt%is_thermo) call check_property_range(0, ZERO, 'mesh-mapping target')

    call write_instantaneous_flow(flow_tgt, domain_tgt)
    !call write_visu_flow(flow_tgt, domain_tgt)
    if(domain_tgt%is_thermo) then
      call write_instantaneous_thermo(thermo_tgt, flow_tgt, domain_tgt)
      !call write_visu_thermo(thermo_tgt, flow_tgt, domain_tgt)
    end if

    if(nrank == 0) call Print_debug_mid_msg("Fields interpolation is completed successfully.")

    return
  end subroutine
end module
