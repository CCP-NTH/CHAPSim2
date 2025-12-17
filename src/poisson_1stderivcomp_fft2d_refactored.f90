
! ============================================================================
! module fft2decomp_interface_mod
! module pentadiagonal_solver_mod
! module wave_number_mod
! module matrix_refinement_mod
! ============================================================================
! MODULE fft2decomp_interface_mod
! ============================================================================
module fft2decomp_interface_mod
  use decomp_2d
  use mpi_mod
  use parameters_constant_mod
  use math_mod
  use print_msg_mod
  implicit none
  ! ============================================================================
  ! MODULE PARAMETERS
  ! ============================================================================
  integer, parameter :: IFORWARD  = 1
  integer, parameter :: IBACKWARD = -1
  ! ============================================================================
  ! GRID AND DOMAIN PARAMETERS
  ! ============================================================================
  integer :: istret
  logical, dimension(3) :: skip_c2c
  ! Domain dimensions
  real(mytype) :: xlx, yly, zlz
  ! Grid points
  integer, save :: nx, ny, nz, nxm, nym, nzm
  ! Grid spacing
  real(mytype), save :: dx, dy, dz, alpha, beta
  ! Boundary conditions
  logical :: nclx, ncly, nclz
  ! ============================================================================
  ! FINITE DIFFERENCE COEFFICIENTS
  ! ============================================================================
  type :: fd_coeffs
    real(mytype) :: alcai, aci, bci
    real(mytype) :: ailcai, aici, bici, cici, dici
  end type fd_coeffs
  type(fd_coeffs), save :: coeffs_x, coeffs_y, coeffs_z
  ! Legacy coefficient names (for backward compatibility)
  real(mytype), save :: alcaix6, acix6, bcix6
  real(mytype), save :: alcaiy6, aciy6, bciy6
  real(mytype), save :: alcaiz6, aciz6, bciz6
  real(mytype), save :: ailcaix6, aicix6, bicix6, cicix6, dicix6
  real(mytype), save :: ailcaiy6, aiciy6, biciy6, ciciy6, diciy6
  real(mytype), save :: ailcaiz6, aiciz6, biciz6, ciciz6, diciz6
  ! ============================================================================
  ! WAVE NUMBERS AND TRANSFORMS
  ! ============================================================================
  complex(mytype), allocatable, dimension(:), save :: zkz, zk2, ezs
  complex(mytype), allocatable, dimension(:), save :: yky, yk2, eys
  complex(mytype), allocatable, dimension(:), save :: xkx, xk2, exs
  ! ============================================================================
  ! TRIDIAGONAL SOLVER ARRAYS (for cylindrical coordinates)
  ! ============================================================================
  real(mytype), allocatable, save :: aa(:), bb(:), cc(:)
  real(mytype), allocatable, save :: bbb_real(:), bbb_imag(:)
  real(mytype), allocatable, save :: rc2(:), ty_real(:), ty_imag(:)

  public :: build_up_fft2decomp_interface
  private :: initialize_domain
  private :: initialize_fd_coefficients
  private :: allocate_wave_arrays
  private :: initialize_fft2d_tdma_arrays

contains
  ! ==========================================================================
  ! MAIN INITIALIZATION ROUTINE
  ! ==========================================================================
  subroutine build_up_fft2decomp_interface(dm)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    !
    if (nrank == 0) call Print_debug_mid_msg("Building Poisson solver interface...")
    !
    call initialize_domain(dm)
    call initialize_fd_coefficients(dm)
    call allocate_wave_arrays()
    call initialize_fft2d_tdma_arrays(dm)
    
    if (nrank == 0) call Print_debug_end_msg()
  end subroutine build_up_fft2decomp_interface
  ! ==========================================================================
  ! INITIALIZATION HELPER ROUTINES
  ! ==========================================================================
  subroutine initialize_domain(dm)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    ! stretching parameters
    istret = dm%istret
    if (istret /= 0) then
      beta = dm%rstret
      alpha = (-ONE + sqrt_wp(ONE + FOUR * PI * PI * beta * beta)) / (TWO * beta)
    else
      alpha = ZERO
      beta = ZERO
    end if
    ! domain size
    xlx = dm%lxx
    yly = dm%lyt - dm%lyb
    zlz = dm%lzz
    ! boundary conditions
    nclx = dm%is_periodic(1)
    ncly = dm%is_periodic(2)
    nclz = dm%is_periodic(3)
    !
    nx = dm%np_geo(1) - 1
    ny = dm%np_geo(2) - 1
    nz = dm%np_geo(3) - 1
    !
    nxm = nx
    nym = ny
    nzm = nz
    !
    dx = dm%h(1)
    dy = dm%h(2)
    dz = dm%h(3)
    return
  end subroutine initialize_grid
  ! ==========================================================================
  subroutine initialize_fd_coefficients(dm)
    use udf_type_mod
    use operations
    implicit none
    type(t_domain), intent(in) :: dm
    ! X-direction coefficients
    alcaix6 = d1fC2P(3, 1, IBC_PERIODIC, dm%iAccuracy)
    acix6   = d1rC2P(3, 1, IBC_PERIODIC, dm%iAccuracy) / dx
    bcix6   = d1rC2P(3, 2, IBC_PERIODIC, dm%iAccuracy) / dx
    !
    ailcaix6 = m1fC2P(3, 1, IBC_PERIODIC, dm%iAccuracy)
    aicix6   = m1rC2P(3, 1, IBC_PERIODIC, dm%iAccuracy)
    bicix6   = m1rC2P(3, 2, IBC_PERIODIC, dm%iAccuracy)
    cicix6   = ZERO
    dicix6   = ZERO
    !
    ! Y-direction coefficients (same as X)
    alcaiy6 = d1fC2P(3, 1, IBC_PERIODIC, dm%iAccuracy)
    aciy6   = d1rC2P(3, 1, IBC_PERIODIC, dm%iAccuracy) / dy
    bciy6   = d1rC2P(3, 2, IBC_PERIODIC, dm%iAccuracy) / dy
    !
    ailcaiy6 = ailcaix6
    aiciy6   = aicix6
    biciy6   = bicix6
    ciciy6   = cicix6
    diciy6   = dicix6
    !
    ! Z-direction coefficients (same as X)
    alcaiz6 = d1fC2P(3, 1, IBC_PERIODIC, dm%iAccuracy)
    aciz6   = d1rC2P(3, 1, IBC_PERIODIC, dm%iAccuracy) / dz
    bciz6   = d1rC2P(3, 2, IBC_PERIODIC, dm%iAccuracy) / dz
    !
    ailcaiz6 = ailcaix6
    aiciz6   = aicix6
    biciz6   = bicix6
    ciciz6   = cicix6
    diciz6   = dicix6
    return 
  end subroutine initialize_fd_coefficients
  ! =====================================================================
  subroutine allocate_wave_arrays()
    implicit none
    allocate(zkz(nz/2+1), zk2(nz/2+1), ezs(nz/2+1))
    allocate(yky(ny), yk2(ny), eys(ny))
    allocate(xkx(nx), xk2(nx), exs(nx))
    !
    zkz = ZERO; zk2 = ZERO; ezs = ZERO
    yky = ZERO; yk2 = ZERO; eys = ZERO
    xkx = ZERO; xk2 = ZERO; exs = ZERO
    return
  end subroutine allocate_wave_arrays
  ! ==========================================================================
  subroutine initialize_fft2d_tdma_arrays(dm)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    integer :: j
    !
    skip_c2c(:) = dm%fft_skip_c2c(:)
    if (nrank == 0) then
      do j = 1, 3
        if (skip_c2c(j)) write(*, *) 'FFT: Skipping C2C in direction', j
      end do
    end if
    !
    if (.not. skip_c2c(2)) return
    !
    allocate(ty_real(ny), ty_imag(ny))
    allocate(aa(ny), bb(ny), cc(ny), bbb_real(ny), bbb_imag(ny))
    allocate(rc2(ny))
    ! Compute coefficients for cylindrical coordinates
    do j = 1, dm%nc(2)
      aa(j) = dm%h2r(2) * &
              dm%rp(j) * &
              dm%rc(j) * &
              dm%yMappingcc(j, 1) * &
              dm%yMappingpt(j, 1) 
      cc(j) = dm%h2r(2) * &
              dm%rp(j+1) * &
              dm%rc(j) * &
              dm%yMappingcc(j, 1) * &
              dm%yMappingpt(j+1, 1) 
    end do
    bb = -(aa + cc)
    ! Apply boundary conditions
    if (.not. dm%is_periodic(2)) then
      bb(1) = bb(1) + aa(1)
      aa(1) = ZERO
      bb(dm%nc(2)) = bb(dm%nc(2)) + cc(dm%nc(2))
      cc(dm%nc(2)) = ZERO
    end if
    rc2(:) = ONE / (dm%rci(:) * dm%rci(:))
    !
    return
  end subroutine initialize_fft2d_tdma_arrays
end module fft2decomp_interface_mod
! ============================================================================
! PENTADIAGONAL MATRIX SOLVERS
! ============================================================================
module pentadiagonal_solver_mod
  use decomp_2d
  use fft2decomp_interface_mod
  use math_mod, only: abs_prec
  implicit none
  !
  private
  private :: forward_elimination
  private :: backward_substitution
  private :: compute_elimination_ratio
  private :: update_rhs
  private :: update_matrix_band
  private :: handle_final_rows
  public :: inversion5_half_ny
  public :: inversion5_ny

contains
  ! ==========================================================================
  ! Pentadiagonal solver for ny/2 grid (version 1)
  ! ==========================================================================
  subroutine inversion5_half_ny(aaa_in, eee, spI)
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    complex(mytype), dimension(spI%yst(1):spI%yen(1), ny/2, &
                               spI%yst(3):spI%yen(3), 5), &
                               intent(in) :: aaa_in
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(2):spI%yen(2), &
                               spI%yst(3):spI%yen(3)), &
                               intent(inout) :: eee
    complex(mytype), dimension(spI%yst(1):spI%yen(1), ny/2, &
                               spI%yst(3):spI%yen(3), 5) :: aaa
    ! Copy input matrix
    aaa = aaa_in
    ! Forward elimination
    call forward_elimination(aaa, eee, spI, ny/2)
    ! Backward substitution
    call backward_substitution(aaa, eee, spI, ny/2)
    return
  end subroutine inversion5_half_ny
  ! ==========================================================================
  ! Pentadiagonal solver for nym grid (version 2)
  ! ==========================================================================
  subroutine inversion5_ny(aaa, eee, spI)
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    complex(mytype), dimension(spI%yst(1):spI%yen(1), nym, &
                               spI%yst(3):spI%yen(3), 5), &
                               intent(inout) :: aaa
    complex(mytype), dimension(spI%yst(1):spI%yen(1), nym, &
                               spI%yst(3):spI%yen(3)), &
                               intent(inout) :: eee
    !
    ! Forward elimination
    call forward_elimination(aaa, eee, spI, nym)
    !
    ! Backward substitution
    call backward_substitution(aaa, eee, spI, nym)
  end subroutine inversion5_ny
  ! ==========================================================================
  ! Forward elimination phase of pentadiagonal solver
  ! ==========================================================================
  subroutine forward_elimination(aaa, eee, spI, n_rows)
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    integer, intent(in) :: n_rows
    complex(mytype), dimension(spI%yst(1):spI%yen(1), n_rows, &
                               spI%yst(3):spI%yen(3), 5), intent(inout) :: aaa
    complex(mytype), dimension(spI%yst(1):spI%yen(1), :, &
                               spI%yst(3):spI%yen(3)), intent(inout) :: eee
    
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)) :: sr, a1, b1
    real(mytype) :: tmp1, tmp2, tmp3, tmp4
    integer :: i, j, k, m, mi, jc
    integer, dimension(2) :: ja, jb
    !
    ! Index arrays for band structure
    ja = [3, 2]  ! ja(i) = 4 - i
    jb = [4, 3]  ! jb(i) = 5 - i
    ! Eliminate lower bands
    do m = 1, n_rows - 2
      do i = 1, 2
        mi = m + i
        ! Compute elimination ratio
        call compute_elimination_ratio(aaa(:,m,:,3), aaa(:,mi,:,3-i), sr, spI)
        ! Update right-hand side
        call update_rhs(eee(:,mi,:), eee(:,m,:), sr, spI)
        ! Update matrix bands
        do jc = ja(i), jb(i)
          call update_matrix_band(aaa(:,mi,:,jc), aaa(:,m,:,jc+i), sr, spI)
        end do
      end do
    end do
    ! Handle last two rows specially
    call handle_final_rows(aaa, eee, spI, n_rows)
  end subroutine forward_elimination
  ! ==========================================================================
  ! Backward substitution phase
  ! ==========================================================================
  subroutine backward_substitution(aaa, eee, spI, n_rows)
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    integer, intent(in) :: n_rows
    complex(mytype), dimension(spI%yst(1):spI%yen(1), n_rows, &
                                spI%yst(3):spI%yen(3), 5), intent(in) :: aaa
    complex(mytype), dimension(spI%yst(1):spI%yen(1), :, &
                                spI%yst(3):spI%yen(3)), intent(inout) :: eee
    
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)) :: sr, a1, b1
    real(mytype) :: tmp1, tmp2
    integer :: i, j, k
    
    do i = n_rows - 2, 1, -1
      do k = spI%yst(3), spI%yen(3)
        do j = spI%yst(1), spI%yen(1)
          ! Compute inverse with safe division
          tmp1 = safe_divide(ONE, rl(aaa(j,i,k,3)))
          tmp2 = safe_divide(ONE, iy(aaa(j,i,k,3)))
          sr(j,k) = cx(tmp1, tmp2)
          !
          ! Compute coefficients
          a1(j,k) = cx(rl(aaa(j,i,k,4)) * tmp1, iy(aaa(j,i,k,4)) * tmp2)
          b1(j,k) = cx(rl(aaa(j,i,k,5)) * tmp1, iy(aaa(j,i,k,5)) * tmp2)
          !
          ! Update solution
          eee(j,i,k) = cx( &
            rl(eee(j,i,k)) * tmp1 - rl(a1(j,k)) * rl(eee(j,i+1,k)) - rl(b1(j,k)) * rl(eee(j,i+2,k)), &
            iy(eee(j,i,k)) * tmp2 - iy(a1(j,k)) * iy(eee(j,i+1,k)) - iy(b1(j,k)) * iy(eee(j,i+2,k)))
        end do
      end do
    end do
  end subroutine backward_substitution

  ! ==========================================================================
  ! Helper: Compute elimination ratio with safe division
  ! ==========================================================================
  subroutine compute_elimination_ratio(pivot, target, ratio, spI)
    use math_mod, only: safe_divide
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)), &
                     intent(in) :: pivot, target
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)), &
                     intent(out) :: ratio
    real(mytype) :: tmp1, tmp2
    integer :: j, k
    
    do k = spI%yst(3), spI%yen(3)
      do j = spI%yst(1), spI%yen(1)
        tmp1 = safe_divide(rl(target(j,k)), rl(pivot(j,k)))
        tmp2 = safe_divide(iy(target(j,k)), iy(pivot(j,k)))
        ratio(j,k) = cx(tmp1, tmp2)
      end do
    end do
  end subroutine compute_elimination_ratio

  ! ==========================================================================
  ! Helper: Update right-hand side
  ! ==========================================================================
  subroutine update_rhs(rhs_new, rhs_old, ratio, spI)
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)), &
                     intent(inout) :: rhs_new
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)), &
                     intent(in) :: rhs_old, ratio
    integer :: j, k
    
    do k = spI%yst(3), spI%yen(3)
      do j = spI%yst(1), spI%yen(1)
        rhs_new(j,k) = cx( &
          rl(rhs_new(j,k)) - rl(ratio(j,k)) * rl(rhs_old(j,k)), &
          iy(rhs_new(j,k)) - iy(ratio(j,k)) * iy(rhs_old(j,k)))
      end do
    end do
  end subroutine update_rhs

  ! ==========================================================================
  ! Helper: Update matrix band
  ! ==========================================================================
  subroutine update_matrix_band(band_new, band_old, ratio, spI)
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)), &
                     intent(inout) :: band_new
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)), &
                     intent(in) :: band_old, ratio
    integer :: j, k
    
    do k = spI%yst(3), spI%yen(3)
      do j = spI%yst(1), spI%yen(1)
        band_new(j,k) = cx( &
          rl(band_new(j,k)) - rl(ratio(j,k)) * rl(band_old(j,k)), &
          iy(band_new(j,k)) - iy(ratio(j,k)) * iy(band_old(j,k)))
      end do
    end do
  end subroutine update_matrix_band

  ! ==========================================================================
  ! Helper: Handle special processing for last two rows
  ! ==========================================================================
  subroutine handle_final_rows(aaa, eee, spI, n_rows)
    implicit none
    type(DECOMP_INFO), intent(in) :: spI
    integer, intent(in) :: n_rows
    complex(mytype), dimension(spI%yst(1):spI%yen(1), n_rows, &
                                spI%yst(3):spI%yen(3), 5), intent(inout) :: aaa
    complex(mytype), dimension(spI%yst(1):spI%yen(1), :, &
                                spI%yst(3):spI%yen(3)), intent(inout) :: eee
    
    complex(mytype), dimension(spI%yst(1):spI%yen(1), spI%yst(3):spI%yen(3)) :: sr, a1, b1
    real(mytype) :: tmp1, tmp2, tmp3, tmp4
    integer :: j, k, n1, n2
    
    n1 = n_rows - 1
    n2 = n_rows
    
    do k = spI%yst(3), spI%yen(3)
      do j = spI%yst(1), spI%yen(1)
        ! First step: compute sr and b1
        tmp1 = safe_divide(rl(aaa(j,n2,k,2)), rl(aaa(j,n1,k,3)))
        tmp2 = safe_divide(iy(aaa(j,n2,k,2)), iy(aaa(j,n1,k,3)))
        sr(j,k) = cx(tmp1, tmp2)
        !
        b1(j,k) = cx( &
          rl(aaa(j,n2,k,3)) - tmp1 * rl(aaa(j,n1,k,4)), &
          iy(aaa(j,n2,k,3)) - tmp2 * iy(aaa(j,n1,k,4)))
        !
        ! Second step: update a1 and eee(n2)
        tmp1 = safe_divide(rl(sr(j,k)), rl(b1(j,k)))
        tmp2 = safe_divide(iy(sr(j,k)), iy(b1(j,k)))
        tmp3 = safe_divide(rl(eee(j,n2,k)), rl(b1(j,k))) - tmp1 * rl(eee(j,n1,k))
        tmp4 = safe_divide(iy(eee(j,n2,k)), iy(b1(j,k))) - tmp2 * iy(eee(j,n1,k))
        !
        a1(j,k) = cx(tmp1, tmp2)
        eee(j,n2,k) = cx(tmp3, tmp4)
        !
        ! Third step: solve for eee(n1)
        tmp1 = safe_divide(ONE, rl(aaa(j,n1,k,3)))
        tmp2 = safe_divide(ONE, iy(aaa(j,n1,k,3)))
        b1(j,k) = cx(tmp1, tmp2)
        !
        a1(j,k) = cx(rl(aaa(j,n1,k,4)) * tmp1, iy(aaa(j,n1,k,4)) * tmp2)
        !
        eee(j,n1,k) = cx( &
          rl(eee(j,n1,k)) * tmp1 - rl(a1(j,k)) * rl(eee(j,n2,k)), &
          iy(eee(j,n1,k)) * tmp2 - iy(a1(j,k)) * iy(eee(j,n2,k)))
      end do
    end do
    return
  end subroutine handle_final_rows

end module pentadiagonal_solver_mod
! ============================================================================
! WAVE NUMBER CALCULATION MODULE
! ============================================================================

module wave_number_mod
  use decomp_2d
  use decomp_2d_fft
  use fft2decomp_interface_mod
  use math_mod, only: sin_prec, cos_prec
  implicit none

  private
  public :: compute_sine_cosine_factors, compute_wave_numbers

contains

  ! ==========================================================================
  ! Compute sine/cosine factors for spectral transforms
  ! ==========================================================================
  subroutine compute_sine_cosine_factors(ax, ay, az, bx, by, bz, &
                                         nx, ny, nz, bcx, bcy, bcz)
    implicit none
    integer, intent(in) :: nx, ny, nz, bcx, bcy, bcz
    real(mytype), dimension(:), intent(out) :: ax, bx, ay, by, az, bz
    
    call compute_direction_factors(ax, bx, nx, bcx)
    call compute_direction_factors(ay, by, ny, bcy)
    call compute_direction_factors(az, bz, nz, bcz)
  end subroutine compute_sine_cosine_factors

  ! ==========================================================================
  ! Compute factors for a single direction
  ! ==========================================================================
  subroutine compute_direction_factors(a, b, n, bc)
    implicit none
    real(mytype), dimension(:), intent(out) :: a, b
    integer, intent(in) :: n, bc
    real(mytype) :: angle, scale
    integer :: i
    
    scale = merge(ONE, HALF, bc == 0)
    
    do i = 1, n
      angle = real(i-1, mytype) * PI * scale / real(n, mytype)
      a(i) = sin_prec(angle)
      b(i) = cos_prec(angle)
    end do
  end subroutine compute_direction_factors

  ! ==========================================================================
  ! Main routine to compute all wave numbers
  ! ==========================================================================
  subroutine compute_wave_numbers(sp, ph)
    implicit none
    type(DECOMP_INFO), intent(in) :: sp, ph
    logical, parameter :: use_filter = .false.
    
    ! Initialize arrays
    xkx = ZERO; xk2 = ZERO; exs = ZERO
    yky = ZERO; yk2 = ZERO; eys = ZERO
    zkz = ZERO; zk2 = ZERO; ezs = ZERO
    
    ! Compute wave numbers in each direction
    call compute_x_wave_numbers()
    call compute_y_wave_numbers()
    call compute_z_wave_numbers()
    
    ! Compute composite wave numbers for Poisson equation
    call compute_composite_wave_numbers(sp, use_filter)
  end subroutine compute_wave_numbers

  ! ==========================================================================
  ! Compute wave numbers in X direction
  ! ==========================================================================
  subroutine compute_x_wave_numbers()
    implicit none
    real(mytype) :: w, wp
    integer :: i, n_half
    
    if (bcx == 0) then
      ! Periodic boundary conditions
      n_half = nx/2 + 1
      do i = 1, n_half
        call compute_wave_pair(i-1, nx, nx, w, wp, alcaix6, acix6, bcix6, dx)
        call assign_wave_values(xkx(i), exs(i), xk2(i), w, wp, nx, xlx)
      end do
      call mirror_wave_numbers(xkx, exs, xk2, nx, n_half)
    else
      ! Neumann boundary conditions
      do i = 1, nx
        call compute_wave_pair_neumann(i-1, nxm, w, wp, alcaix6, acix6, bcix6, dx)
        call assign_wave_values(xkx(i), exs(i), xk2(i), w, wp, nxm, xlx)
      end do
      xkx(1) = ZERO; exs(1) = ZERO; xk2(1) = ZERO
    end if
  end subroutine compute_x_wave_numbers

  ! ==========================================================================
  ! Compute wave numbers in Y direction
  ! ==========================================================================
  subroutine compute_y_wave_numbers()
    implicit none
    real(mytype) :: w, wp, scale
    integer :: j, n_half
    
    scale = merge(ONE, yly, istret == 0)
    
    if (bcy == 0) then
      ! Periodic boundary conditions
      n_half = ny/2 + 1
      do j = 1, n_half
        call compute_wave_pair(j-1, ny, ny, w, wp, alcaiy6, aciy6, bciy6, dy)
        call assign_wave_values_y(yky(j), eys(j), yk2(j), w, wp, ny, scale)
      end do
      call mirror_wave_numbers(yky, eys, yk2, ny, n_half)
    else
      ! Neumann boundary conditions
      do j = 1, ny
        call compute_wave_pair_neumann(j-1, nym, w, wp, alcaiy6, aciy6, bciy6, dy)
        call assign_wave_values_y(yky(j), eys(j), yk2(j), w, wp, nym, scale)
      end do
      yky(1) = ZERO; eys(1) = ZERO; yk2(1) = ZERO
    end if
  end subroutine compute_y_wave_numbers

  ! ==========================================================================
  ! Compute wave numbers in Z direction
  ! ==========================================================================
  subroutine compute_z_wave_numbers()
    implicit none
    real(mytype) :: w, wp, w1, w1p
    integer :: k
    
    if (bcz == 0) then
      ! Periodic boundary conditions
      do k = 1, nz/2 + 1
        call compute_wave_pair(k-1, nz, nz, w, wp, alcaiz6, aciz6, bciz6, dz)
        call assign_wave_values(zkz(k), ezs(k), zk2(k), w, wp, nz, zlz)
      end do
    else
      ! Neumann boundary conditions (complex wave numbers)
      do k = 1, nz/2 + 1
        call compute_wave_pair_neumann_complex(k-1, nzm, w, wp, w1, w1p, &
                                               alcaiz6, aciz6, bciz6, dz)
        zkz(k) = cx(nzm * wp / zlz, -nzm * w1p / zlz)
        ezs(k) = cx(nzm * w / zlz, nzm * w1 / zlz)
        zk2(k) = cx((nzm * wp / zlz)**2, (nzm * w1p / zlz)**2)
      end do
    end if
  end subroutine compute_z_wave_numbers

  ! ==========================================================================
  ! Helper: Compute wave number pair (w, wp) for periodic BC
  ! ==========================================================================
  subroutine compute_wave_pair(index, n_points, n_scale, w, wp, &
                                alca, ac, bc, h)
    implicit none
    integer, intent(in) :: index, n_points, n_scale
    real(mytype), intent(out) :: w, wp
    real(mytype), intent(in) :: alca, ac, bc, h
    
    w = TWOPI * real(index, mytype) / real(n_points, mytype)
    wp = ac * TWO * h * sin_prec(w * HALF) + &
         bc * TWO * h * sin_prec(THREE * HALF * w)
    wp = wp / (ONE + TWO * alca * cos_prec(w))
  end subroutine compute_wave_pair

  ! ==========================================================================
  ! Helper: Compute wave number pair for Neumann BC
  ! ==========================================================================
  subroutine compute_wave_pair_neumann(index, nm, w, wp, alca, ac, bc, h)
    implicit none
    integer, intent(in) :: index, nm
    real(mytype), intent(out) :: w, wp
    real(mytype), intent(in) :: alca, ac, bc, h
    
    w = PI * real(index, mytype) / real(nm, mytype)
    wp = ac * TWO * h * sin_prec(w * HALF) + &
         bc * TWO * h * sin_prec(THREE * HALF * w)
    wp = wp / (ONE + TWO * alca * cos_prec(w))
  end subroutine compute_wave_pair_neumann

  ! ==========================================================================
  ! Helper: Compute complex wave numbers for Neumann BC in Z
  ! ==========================================================================
  subroutine compute_wave_pair_neumann_complex(index, nm, w, wp, w1, w1p, &
                                                alca, ac, bc, h)
    implicit none
    integer, intent(in) :: index, nm
    real(mytype), intent(out) :: w, wp, w1, w1p
    real(mytype), intent(in) :: alca, ac, bc, h
    
    ! Forward wave
    w = PI * real(index, mytype) / real(nm, mytype)
    wp = ac * TWO * h * sin_prec(w * HALF) + &
         bc * TWO * h * sin_prec(THREE * HALF * w)
    wp = wp / (ONE + TWO * alca * cos_prec(w))
    
    ! Backward wave
    w1 = PI * real(nm - index + 1, mytype) / real(nm, mytype)
    w1p = ac * TWO * h * sin_prec(w1 * HALF) + &
          bc * TWO * h * sin_prec(THREE * HALF * w1)
    w1p = w1p / (ONE + TWO * alca * cos_prec(w1))
  end subroutine compute_wave_pair_neumann_complex

  ! ==========================================================================
  ! Helper: Assign wave values (standard directions)
  ! ==========================================================================
  subroutine assign_wave_values(k_val, e_val, k2_val, w, wp, n, length)
    implicit none
    complex(mytype), intent(out) :: k_val, e_val, k2_val
    real(mytype), intent(in) :: w, wp
    integer, intent(in) :: n
    real(mytype), intent(in) :: length
    real(mytype) :: scale
    
    scale = real(n, mytype) / length
    k_val = cx_one_one * scale * wp
    e_val = cx_one_one * scale * w
    k2_val = cx_one_one * (scale * wp)**2
  end subroutine assign_wave_values

  ! ==========================================================================
  ! Helper: Assign wave values for Y direction (with stretching)
  ! ==========================================================================
  subroutine assign_wave_values_y(k_val, e_val, k2_val, w, wp, n, scale)
    implicit none
    complex(mytype), intent(out) :: k_val, e_val, k2_val
    real(mytype), intent(in) :: w, wp
    integer, intent(in) :: n
    real(mytype), intent(in) :: scale
    
    k_val = cx_one_one * real(n, mytype) * wp / scale
    e_val = cx_one_one * real(n, mytype) * w / yly
    k2_val = cx_one_one * (real(n, mytype) * wp / yly)**2
  end subroutine assign_wave_values_y

  ! ==========================================================================
  ! Helper: Mirror wave numbers for second half of spectrum
  ! ==========================================================================
  subroutine mirror_wave_numbers(k_arr, e_arr, k2_arr, n, n_half)
    implicit none
    complex(mytype), dimension(:), intent(inout) :: k_arr, e_arr, k2_arr
    integer, intent(in) :: n, n_half
    integer :: i
    
    do i = n_half + 1, n
      k_arr(i) = k_arr(n - i + 2)
      e_arr(i) = e_arr(n - i + 2)
      k2_arr(i) = k2_arr(n - i + 2)
    end do
  end subroutine mirror_wave_numbers

  ! ==========================================================================
  ! Compute composite wave numbers for Poisson equation
  ! ==========================================================================
  subroutine compute_composite_wave_numbers(sp, use_filter)
    implicit none
    type(DECOMP_INFO), intent(in) :: sp
    logical, intent(in) :: use_filter
    
    if (bcx == 0 .and. bcz == 0 .and. bcy /= 0) then
      call compute_kxyz_case1(sp, use_filter)
    else if (bcz == 0) then
      call compute_kxyz_case2(sp, use_filter)
    else
      call compute_kxyz_case3(sp, use_filter)
    end if
  end subroutine compute_composite_wave_numbers

  ! ==========================================================================
  ! Case 1: Periodic in X,Z; Neumann in Y
  ! ==========================================================================
  subroutine compute_kxyz_case1(sp, use_filter)
    implicit none
    type(DECOMP_INFO), intent(in) :: sp
    logical, intent(in) :: use_filter
    integer :: i, j, k
    complex(mytype) :: xt2, yt2, zt2
    
    do k = sp%yst(3), sp%yen(3)
      do j = sp%yst(2), sp%yen(2)
        do i = sp%yst(1), sp%yen(1)
          if (use_filter) then
            call compute_filtered_k2(i, j, k, xt2, yt2, zt2)
          else
            xt2 = xk2(i)
            yt2 = yk2(j)
            zt2 = zk2(k)
          end if
          kxyz(i,j,k) = xt2 + yt2 + zt2
        end do
      end do
    end do
  end subroutine compute_kxyz_case1

  ! ==========================================================================
  ! Case 2: Periodic in Z; Neumann in X,Y
  ! ==========================================================================
  subroutine compute_kxyz_case2(sp, use_filter)
    implicit none
    type(DECOMP_INFO), intent(in) :: sp
    logical, intent(in) :: use_filter
    integer :: i, j, k
    complex(mytype) :: xt2, yt2, zt2
    
    do k = sp%xst(3), sp%xen(3)
      do j = sp%xst(2), sp%xen(2)
        do i = sp%xst(1), sp%xen(1)
          if (use_filter) then
            call compute_filtered_k2(i, j, k, xt2, yt2, zt2)
          else
            xt2 = xk2(i)
            yt2 = yk2(j)
            zt2 = zk2(k)
          end if
          kxyz(i,j,k) = xt2 + yt2 + zt2
        end do
      end do
    end do
  end subroutine compute_kxyz_case2

  ! ==========================================================================
  ! Case 3: Neumann in all directions (complex Z wave numbers)
  ! ==========================================================================
  subroutine compute_kxyz_case3(sp, use_filter)
    implicit none
    type(DECOMP_INFO), intent(in) :: sp
    logical, intent(in) :: use_filter
    integer :: i, j, k
    complex(mytype) :: xyzk
    
    do k = sp%xst(3), sp%xen(3)
      do j = sp%xst(2), sp%xen(2)
        do i = sp%xst(1), sp%xen(1)
          if (use_filter) then
            call compute_filtered_k2_complex(i, j, k, xyzk)
          else
            xyzk = xk2(i) + yk2(j) + zk2(k)
          end if
          kxyz(i,j,k) = xyzk
        end do
      end do
    end do
  end subroutine compute_kxyz_case3

  ! ==========================================================================
  ! Compute filtered k2 values (real case)
  ! ==========================================================================
  subroutine compute_filtered_k2(i, j, k, xt2, yt2, zt2)
    implicit none
    integer, intent(in) :: i, j, k
    complex(mytype), intent(out) :: xt2, yt2, zt2
    real(mytype) :: fx, fy, fz
    
    ! Compute filter factors
    call compute_filter_factor(rl(exs(i)) * dx, fx, &
                               aicix6, bicix6, cicix6, dicix6, ailcaix6)
    call compute_filter_factor(rl(eys(j)) * dy, fy, &
                               aiciy6, biciy6, ciciy6, diciy6, ailcaiy6)
    call compute_filter_factor(rl(ezs(k)) * dz, fz, &
                               aiciz6, biciz6, ciciz6, diciz6, ailcaiz6)
    
    ! Apply filters
    xt2 = xk2(i) * ((fy * fz)**2)
    yt2 = yk2(j) * ((fx * fz)**2)
    zt2 = zk2(k) * ((fx * fy)**2)
  end subroutine compute_filtered_k2

  ! ==========================================================================
  ! Compute filter factor for one direction
  ! ==========================================================================
  subroutine compute_filter_factor(phase, factor, ai, bi, ci, di, ailca)
    implicit none
    real(mytype), intent(in) :: phase, ai, bi, ci, di, ailca
    real(mytype), intent(out) :: factor
    real(mytype) :: term1, term2
    
    term1 = TWO * ai * cos_prec(phase * HALF)
    term2 = TWO * (bi * cos_prec(phase * ONEPFIVE) + &
                   ci * cos_prec(phase * TWOPFIVE) + &
                   di * cos_prec(phase * THREEPFIVE))
    factor = (term1 + term2) / (ONE + TWO * ailca * cos_prec(phase))
  end subroutine compute_filter_factor

  ! ==========================================================================
  ! Compute filtered k2 values (complex Z case)
  ! ==========================================================================
  subroutine compute_filtered_k2_complex(i, j, k, xyzk)
    implicit none
    integer, intent(in) :: i, j, k
    complex(mytype), intent(out) :: xyzk
    real(mytype) :: fx, fy
    complex(mytype) :: fz, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6
    
    ! Compute filter factors
    call compute_filter_factor(rl(exs(i)) * dx, fx, &
                               aicix6, bicix6, cicix6, dicix6, ailcaix6)
    call compute_filter_factor(rl(eys(j)) * dy, fy, &
                               aiciy6, biciy6, ciciy6, diciy6, ailcaiy6)
    call compute_filter_factor_complex(ezs(k), dz, fz)
    
    ! Complex arithmetic for filter application
    tmp1 = cx(rl(fz), iy(fz))
    tmp2 = cx_one_one * fy
    tmp3 = cx_one_one * fx
    
    tmp4 = rl(tmp2)**2 * cx(rl(tmp1)**2, iy(tmp1)**2)
    tmp5 = rl(tmp3)**2 * cx(rl(tmp1)**2, iy(tmp1)**2)
    tmp6 = (rl(tmp3) * rl(tmp2))**2 * cx_one_one
    
    xyzk = cx(rl(tmp4) * rl(xk2(i)), iy(tmp4) * iy(xk2(i))) + &
           cx(rl(tmp5) * rl(yk2(j)), iy(tmp5) * iy(yk2(j))) + &
           rl(tmp6) * zk2(k)
  end subroutine compute_filtered_k2_complex

  ! ==========================================================================
  ! Compute complex filter factor
  ! ==========================================================================
  subroutine compute_filter_factor_complex(e_val, h, factor)
    implicit none
    complex(mytype), intent(in) :: e_val
    real(mytype), intent(in) :: h
    complex(mytype), intent(out) :: factor
    real(mytype) :: rl_phase, iy_phase
    complex(mytype) :: term1, term2, denom
    
    rl_phase = rl(e_val) * h
    iy_phase = iy(e_val) * h
    
    term1 = TWO * cx(aiciz6 * cos_prec(rl_phase * HALF), &
                     aiciz6 * cos_prec(iy_phase * HALF))
    term2 = TWO * cx( &
      biciz6 * cos_prec(rl_phase * ONEPFIVE) + &
      ciciz6 * cos_prec(rl_phase * TWOPFIVE) + &
      diciz6 * cos_prec(rl_phase * THREEPFIVE), &
      biciz6 * cos_prec(iy_phase * ONEPFIVE) + &
      ciciz6 * cos_prec(iy_phase * TWOPFIVE) + &
      diciz6 * cos_prec(iy_phase * THREEPFIVE))
    denom = cx(ONE + TWO * ailcaiz6 * cos_prec(rl_phase), &
               ONE + TWO * ailcaiz6 * cos_prec(iy_phase))
    
    factor = cx(rl(term1 + term2) / rl(denom), &
                iy(term1 + term2) / iy(denom))
  end subroutine compute_filter_factor_complex

end module wave_number_mod