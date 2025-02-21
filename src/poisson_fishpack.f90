module fishpack_fft
  use precision_mod
  private 

  integer, save :: LPx, LPy, LPz
  real(WP), save :: SCALx, SCALz
  real(WP), allocatable, save :: XRT(:), WX(:)
  real(WP), allocatable, save :: ZRT(:), WZ(:)
  real(WP), allocatable, save :: a(:), b(:), c(:), bb(:)

  private :: TRID0
  private :: fishpack_root_1D
  private :: fishpack_fft_1D

  public :: fishpack_fft_init
  public :: fishpack_fft_simple
contains
!==========================================================================================================
!==========================================================================================================
  SUBROUTINE TRID0(NG, T) !A, BB, C, T)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: NG
    !REAL(WP), DIMENSION(NG), INTENT(IN)    :: A
    !REAL(WP), DIMENSION(NG), INTENT(IN)    :: BB
    !REAL(WP), DIMENSION(NG), INTENT(IN)    :: C
    REAL(WP), DIMENSION(NG), INTENT(INOUT) :: T
    

    INTEGER :: NR, MM1, I, IP
    REAL(WP) :: Z
    REAL(WP), DIMENSION(NG) :: D

    NR = NG
    MM1 = NR - 1
    Z = 1.0_WP / BB(1)
    D(1) = C(1) * Z
    T(1) = T(1) * Z
    DO I = 2, MM1
        Z = 1.0_WP / (BB(I) - A(I) * D(I - 1))
        D(I) = C(I) * Z
        T(I) = (T(I) - A(I) * T(I - 1)) * Z
    END DO
    Z = BB(NR) - A(NR) * D(MM1)
    IF (DABS(Z) > 1.0E-10_WP) THEN
      T(NR) = (T(NR) - A(NR) * T(MM1)) /Z
    ELSE
      T(NR) = 0.0_WP
    END IF

    DO IP = 1, MM1
      I = NR-IP
      T(I) = T(I) - D(I) * T(I + 1)
    END DO

    RETURN
  END SUBROUTINE
!==========================================================================================================
!==========================================================================================================
  SUBROUTINE fishpack_root_1D(L, LP, C1, SCALX, WX, XRT)
  !----------------------------------------------------------------------------------------------------------
  ! Generate FFT transform roots for 1-D direction
  ! Arguments:
  !   L   (INTEGER(4), IN): Grid dimensions in X direction, global
  !   LP  (INTEGER(4), IN): Parameters determining transform type
  !   C1   (REAL(WP), IN): Scaling factors for X transform
  !   SCALX(REAL(WP), IN): scaling factor 
  !   WX   (REAL(WP), DIMENSION(:), OUT): Work arrays for FFT
  !   XRT  (REAL(WP), DIMENSION(:), OUT): Transform roots for X
  !----------------------------------------------------------------------------------------------------------
    IMPLICIT NONE

    ! Arguments
    INTEGER(4), INTENT(IN) :: L, LP
    REAL(WP), INTENT(IN) :: C1
    REAL(WP), INTENT(OUT) :: SCALX
    REAL(WP), DIMENSION(:), INTENT(INOUT) :: WX
    REAL(WP), DIMENSION(:), INTENT(INOUT) :: XRT
    

    ! Local variables
    REAL(WP) :: PI, DX, DI
    INTEGER(4) :: LR, LRDEL, I

    ! Compute PI
    PI = 2.0_WP * DASIN(1.0_WP)
    WX = 0.0_WP
    XRT = 0.0_WP
    !-----------------------------------------------------------
    ! X direction transform roots
    !-----------------------------------------------------------
    LR = L
    LRDEL = ((LP - 1) * (LP - 3) * (LP - 5)) / 3
    SCALX = DBLE(LR + LRDEL)
    DX = PI / (2.0_WP * SCALX)

    SELECT CASE (LP)
    CASE (1)
      ! RFFTI     INITIALIZE  RFFTF AND RFFTB
      ! RFFTF     FORWARD TRANSFORM OF A REAL PERIODIC SEQUENCE
      ! RFFTB     BACKWARD TRANSFORM OF A REAL COEFFICIENT ARRAY
      XRT(1) = 0.0_WP
      XRT(LR) = -4.0_WP * C1
      DO I = 3, LR, 2
        XRT(I - 1) = -4.0_WP * C1 * (DSIN(DBLE((I - 1)) * DX))**2  ! WW: This is for 2nd order central difference only
        XRT(I) = XRT(I - 1)
      END DO
      CALL RFFTI(LR, WX)
    CASE (2)
      ! SINTI     INITIALIZE SINT
      ! SINT      SINE TRANSFORM OF A REAL ODD SEQUENCE
      DI = 0.00_WP
      DO I = 1, LR
        XRT(I) = -4.0_WP * C1 * (DSIN((DBLE(I) - DI) * DX))**2
      END DO
      SCALX = 2.0_WP * SCALX
      CALL SINTI (LR, WX)

    CASE (3)
    ! SINQI     INITIALIZE SINQF AND SINQB
    ! SINQF     FORWARD SINE TRANSFORM WITH ODD WAVE NUMBERS
    ! SINQB     UNNORMALIZED INVERSE OF SINQF
      DI = 0.50_WP
      SCALX = 2.0_WP * SCALX
      DO I = 1, LR
        XRT(I) = -4.0_WP * C1 * (DSIN((DBLE(I) - DI) * DX))**2
      ENDDO
      SCALX = 2.0_WP * SCALX
      CALL SINQI (LR, WX)

    CASE (4)
    ! COSTI     INITIALIZE COST
    ! COST      COSINE TRANSFORM OF A REAL EVEN SEQUENCE
      DI = 1.00_WP
      DO I = 1, LR
          XRT(I) = -4.0_WP * C1 * (DSIN((DBLE(I) - DI) * DX))**2
      END DO
      SCALX = 2.0_WP * SCALX
      CALL COSTI (LR, WX)

    CASE (5)
    ! COSQI     INITIALIZE COSQF AND COSQB
    ! COSQF     FORWARD COSINE TRANSFORM WITH ODD WAVE NUMBERS
    ! COSQB     UNNORMALIZED INVERSE OF COSQF
      DI = 0.50_WP
      SCALX = 2.0_WP * SCALX
      DO I = 1, LR
        XRT(I) = -4.0_WP * C1 * (DSIN((DBLE(I) - DI) * DX))**2
      END DO
      SCALX = 2.0_WP * SCALX
      CALL COSQI (LR, WX)

    END SELECT


    RETURN
  END SUBROUTINE 

!==========================================================================================================
!==========================================================================================================
  subroutine fishpack_fft_1D(ifwrd, lp, t, w)
    implicit none
    integer(4), intent(in)  :: ifwrd, lp
    real(WP), intent(inout) :: t(:)
    real(WP), intent(in)    :: w(:)

    integer(4) :: nx

    nx = size(t)
    if(ifwrd == 1) then
    !-----------------------------------------------------------  
    ! forward 1-D FFT, physical space to wave number space
    !-----------------------------------------------------------
      select case (lp)
      case (1)
        call RFFTF(nx, t, w)
      case (2)  
        call SINT(nx, t, w) 
      case (3)  
        call SINQF(nx, t, w)
      case (4)
        call COST(nx, t, w)
      case (5)
        call COSQF(nx, t, w)
      end select
    
    else if(ifwrd == 2) then
    !-----------------------------------------------------------  
    ! backward 1-D FFT, wave number space to physical space
    !-----------------------------------------------------------
      select case (lp)
        case (1)
          call RFFTB(nx, t, w)
        case (2)  
          call SINT(nx, t, w) 
        case (3)  
          call SINQB(nx, t, w)
        case (4)
          call COST(nx, t, w)
        case (5)
          call COSQB(nx, t, w)
        end select
        
    else
      error stop 'Error: ifwrd is not 1 or 2'
    end if 


  return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine fishpack_fft_init(dm)
    use udf_type_mod
    use parameters_constant_mod
    use io_files_mod
    implicit none
    type(t_domain), intent(in) :: dm

    !integer :: wsz
    integer :: ibcx(2), ibcz(2)
    integer :: nx, ny, nz, np
    integer :: j, i, k, ii, kk
    !real(WP) :: dyfi(dm%nc(2)), dyci(dm%np_geo(2)) 
    !-----------------------------------------------------------
    ! assign key info from domain
    !-----------------------------------------------------------
    nx = dm%nc(1)
    ny = dm%nc(2)
    nz = dm%nc(3)
    np = dm%np_geo(2)
    ibcx(1:2) = dm%ibcx_qx(1:2)
    ibcz(1:2) = dm%ibcz_qx(1:2)
    !-----------------------------------------------------------
    ! check input grid size
    !-----------------------------------------------------------
    if(nx <= 3 .or. ny <= 3 .or. nz <= 3) then
      error stop  'Error: Grid size is too small for Fishpack FFT'
    end if
    !-----------------------------------------------------------
    ! assign FFT transform type, LP->x, MP->z, NP->y
    !-----------------------------------------------------------
    if(ibcx(1) == IBC_PERIODIC  .and. ibcx(2) == IBC_PERIODIC ) LPx = 0
    if(ibcx(1) == IBC_DIRICHLET .and. ibcx(2) == IBC_DIRICHLET) LPx = 1
    if(ibcx(1) == IBC_DIRICHLET .and. ibcx(2) == IBC_NEUMANN  ) LPx = 2
    if(ibcx(1) == IBC_NEUMANN   .and. ibcx(2) == IBC_NEUMANN  ) LPx = 3
    if(ibcx(1) == IBC_NEUMANN   .and. ibcx(2) == IBC_DIRICHLET) LPx = 4

    if(ibcz(1) == IBC_PERIODIC  .and. ibcz(2) == IBC_PERIODIC ) LPz = 0
    if(ibcz(1) == IBC_DIRICHLET .and. ibcz(2) == IBC_DIRICHLET) LPz = 1
    if(ibcz(1) == IBC_DIRICHLET .and. ibcz(2) == IBC_NEUMANN  ) LPz = 2
    if(ibcz(1) == IBC_NEUMANN   .and. ibcz(2) == IBC_NEUMANN  ) LPz = 3
    if(ibcz(1) == IBC_NEUMANN   .and. ibcz(2) == IBC_DIRICHLET) LPz = 4

    LPy = 1

    LPx = LPx + 1
    LPy = LPy + 1
    LPz = LPz + 1

    !wsz = 30 + nx + ny * 2 + nz + MAX(nx, ny, nz) + 7 * (INT((nx+1)/2) + INT((nz+1)/2)) + 128
    !-----------------------------------------------------------
    ! Build up fft root for x
    !-----------------------------------------------------------
    allocate(XRT(nx), WX(2*nx + 15))
    call fishpack_root_1D(nx, LPx, dm%h2r(1), SCALx, Wx, xRT)
    !-----------------------------------------------------------
    ! Build up fft root for z
    !-----------------------------------------------------------
    allocate(ZRT(nz), WZ(2*nz + 15))
    call fishpack_root_1D(nz, LPz, dm%h2r(3), SCALz, Wz, zRT)

! if(nrank==0) WRITE(*,*)'fft-scl ', SCALX, SCALz
! if(nrank==0) WRITE(*,*)'fft-xrt  ', XRT
! if(nrank==0) WRITE(*,*)'fft-zrt  ', zRT
    !-----------------------------------------------------------
    ! cylinderical poisson equation solved is:
    ! r^2 * d^2f/dx^2 + r * d(r df/dr)/dr + d^2f/dz^2 = rhs * r^2
    !-----------------------------------------------------------
    ! allocate work arrays for TMDA, and coefficients
    ! note: this is for 2nd order central difference only
    ! r * d(r*df/dy)/dy at j = a j_{j-1} + b j_{j} + c j_{j+1} <=>
    ! +f_{j+1} / ( (y_{j+1} - y_j) * (y'_{j+1}-y'_{j}) ) ! c = 1 / ( (y_{j+1} - y_{j}) * (y'_{j+1}-y'_{j}) )
    ! +f_{j-1} / ( (y_j - y_{j-1}) * (y'_{j+1}-y'_{j}) ) ! a = 1 / ( (y_{j} - y_{j-1}) * (y'_{j+1}-y'_{j}) )
    ! -f_{j}   * (a+b)
    !-----------------------------------------------------------
    allocate(a(ny), b(ny), c(ny), bb(ny))

    ! do j = 1, dm%nc(2)
    !   dyfi(j) = 1.0_WP / (dm%yp(j+1) - dm%yp(j)) ! node to node spacing
    ! end do
    ! do j = 2, dm%nc(2)
    !   dyci(j) = 1.0_WP / (dm%yc(j) - dm%yc(j-1)) ! cell centre to centre spacing
    ! end do
    ! dyci(1 ) = 1.0_WP / (( dm%yc(1) - dm%yp(1)  ) * TWO)! 
    ! dyci(np) = 1.0_WP / (( dm%yp(np)- dm%yc(np-1) ) * TWO)!

    ! do j = 1, dm%nc(2)
    !   a(j) = (dyci(j  )/dm%rpi(j  )) * (dyfi(j)/dm%rci(j))
    !   c(j) = (dyci(j+1)/dm%rpi(j+1)) * (dyfi(j)/dm%rci(j))
    ! end do
    ! if(.not. dm%is_periodic(2)) then
    !   b(1) = b(1) + a(1)
    !   a(1) = 0.0_WP
    !   b(dm%nc(2)) = b(dm%nc(2)) + c(dm%nc(2))
    !   c(dm%nc(2)) = 0.0_WP
    ! end if
    ! write(*,*) 'a', a
    ! write(*,*) 'b', b
    ! write(*,*) 'c', c

    do j = 1, dm%nc(2)
      a(j) = dm%h2r(2) * dm%yMappingcc(j, 1) * dm%yMappingpt(j  , 1) * dm%rp(j  ) * dm%rc(j)
      c(j) = dm%h2r(2) * dm%yMappingcc(j, 1) * dm%yMappingpt(j+1, 1) * dm%rp(j+1) * dm%rc(j)
    end do
    b = -(a + c)
    if(.not. dm%is_periodic(2)) then
      ! here apply the second order ghost cell configuration
      b(1) = b(1) + a(1)
      a(1) = 0.0_WP
      b(dm%nc(2)) = b(dm%nc(2)) + c(dm%nc(2))
      c(dm%nc(2)) = 0.0_WP
    end if
    ! write(*,*) 'a', a
    ! write(*,*) 'b', b
    ! write(*,*) 'c', c
    !-----------------------------------------------------------
    ! data check
    !-----------------------------------------------------------
    ! if(nrank == 0) then
    !   open(221, file = trim(dir_chkp)//'/check_mesh_dyfi.dat')
    !   write(221, *) 'index, dyfi'
    !   do j = 1, dm%nc(2)
    !     write (221, *) j, dyfi(j)
    !   end do
    !   open(223, file = trim(dir_chkp)//'/check_mesh_dyci.dat')
    !   write(223, *) 'index, dyci'
    !   do j = 1, dm%np_geo(2)
    !     write (223, *) j, dyci(j)
    !   end do
    !   open(224, file = trim(dir_chkp)//'/check_mesh_abc.dat')
    !   write(224, *) 'index, a, b, c'
    !   do j = 1, dm%nc(2)
    !     write (224, *) j, a(j), b(j), c(j)
    !   end do
    ! end if
    

  return
  end subroutine

!==========================================================================================================
!==========================================================================================================
  subroutine fishpack_fft_simple(rhs_xpencil, dm)
    use udf_type_mod
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP), intent(inout) :: rhs_xpencil(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3))

    integer :: ifwrd
    integer :: i, j, k, ii, kk
    real(WP) :: tx(dm%dccc%xsz(1)), ty(dm%dccc%ysz(2)), tz(dm%dccc%zsz(3))
    real(WP) :: rhs_ypencil(dm%dccc%ysz(1), dm%dccc%ysz(2), dm%dccc%ysz(3))
    real(WP) :: rhs_zpencil(dm%dccc%zsz(1), dm%dccc%zsz(2), dm%dccc%zsz(3))
    
!write(*,*) 'fft-in ', rhs_xpencil
    !-----------------------------------------------------------
    ! forward FFT in x direction
    ! x - pencil
    !-----------------------------------------------------------
    ifwrd = 1
    do j = 1, dm%dccc%xsz(2)
      do k = 1, dm%dccc%xsz(3)

        do i = 1, dm%dccc%xsz(1)
          tx(i) = rhs_xpencil(i, j, k)
        end do
        call fishpack_fft_1D(ifwrd, LPx, tx, WX)
        do i = 1, dm%dccc%xsz(1)
          rhs_xpencil(i, j, k) = tx(i)
          !if(dabs(tx(i)) > 1.E+8) write(*,*) 'test1', tx(i)
        end do

      end do 
    end do
    !-----------------------------------------------------------
    ! transfer data to z-pencil for z-FFT
    !-----------------------------------------------------------
    call transpose_x_to_y(rhs_xpencil, rhs_ypencil, dm%dccc)
    call transpose_y_to_z(rhs_ypencil, rhs_zpencil, dm%dccc)
    !-----------------------------------------------------------
    ! forward FFT in z direction
    !-----------------------------------------------------------
    ifwrd = 1
    do i = 1, dm%dccc%zsz(1)
      do j = 1, dm%dccc%zsz(2)

        do k = 1, dm%dccc%zsz(3)
          tz(k) = rhs_zpencil(i, j, k)
        end do
        call fishpack_fft_1D(ifwrd, LPz, tz, WZ)
        do k = 1, dm%dccc%zsz(3)
          rhs_zpencil(i, j, k) = tz(k)
          !if(dabs(tz(k)) > 1.E+8) write(*,*) 'test2', tz(k)
        end do

      end do 
    end do  
    !write(*,'(A, I3, 1ES13.5)') ('test2', k, rhs_zpencil(16,32,k), k=1, dm%dccc%zsz(3))
    !-----------------------------------------------------------
    ! transfer data to Y-pencil for Y-TDMA
    !-----------------------------------------------------------
    call transpose_z_to_y(rhs_zpencil, rhs_ypencil, dm%dccc)
    !-----------------------------------------------------------
    ! TMDA in the Y direction (stretching grids direction)
    !-----------------------------------------------------------
!!write(*,*) 'fft-xzfft ', rhs_ypencil  
    do i = 1, dm%dccc%ysz(1)
       ii = dm%dccc%yst(1) + i - 1
      do k = 1, dm%dccc%ysz(3)
        kk = dm%dccc%yst(3) + k - 1

        do j = 1, dm%dccc%ysz(2)
          bb(j) = b(j) + xrt(ii) / (dm%rci(j) * dm%rci(j)) + zrt(kk)
          ty(j) = rhs_ypencil(i, j, k)
          !if(dabs(ty(j)) > 1.E+8) write(*,*) 'test31', ty(j), i, j, k
        end do
        call TRID0(dm%dccc%ysz(2), ty)!a, bb, c, ty)
        do j = 1, dm%dccc%ysz(2)
          rhs_ypencil(i, j, k) = ty(j)
          !if(dabs(ty(j)) > 1.E+8) write(*,*) 'test32', ty(j), i, j, k
        end do  
      end do 
    end do
!write(*,*) 'fft-ytdma ', rhs_ypencil
    !write(*,'(A, I3, 1ES13.5)') ('test3', j, rhs_ypencil(16,j,8), j=1, dm%dccc%ysz(2))
    !-----------------------------------------------------------
    ! transfer data to Z-pencil for backward z-FFT
    !-----------------------------------------------------------
    call transpose_y_to_z(rhs_ypencil, rhs_zpencil, dm%dccc)
    !-----------------------------------------------------------
    ! backward FFT in z direction
    !-----------------------------------------------------------
    ifwrd = 2
    do i = 1, dm%dccc%zsz(1)
      do j = 1, dm%dccc%zsz(2)

        do k = 1, dm%dccc%zsz(3)
          tz(k) = rhs_zpencil(i, j, k)
        end do
        call fishpack_fft_1D(ifwrd, LPz, tz, WZ)
        do k = 1, dm%dccc%zsz(3)
          rhs_zpencil(i, j, k) = tz(k)
          !if(dabs(tz(k)) > 1.E+8) write(*,*) 'test4', tz(k)
        end do

      end do 
    end do
    !write(*,'(A, I3, 1ES13.5)') ('test4', k, rhs_zpencil(16,32,k), k=1, dm%dccc%zsz(3))
    !-----------------------------------------------------------
    ! transfer data to X-pencil for backward x-FFT
    !-----------------------------------------------------------
    call transpose_z_to_y(rhs_zpencil, rhs_ypencil, dm%dccc) 
    call transpose_y_to_x(rhs_ypencil, rhs_xpencil, dm%dccc)
    !-----------------------------------------------------------
    ! backward FFT in x direction
    ! x - pencil
    !-----------------------------------------------------------
    ifwrd = 2
    do j = 1, dm%dccc%xsz(2)
      do k = 1, dm%dccc%xsz(3)

        do i = 1, dm%dccc%xsz(1)
          tx(i) = rhs_xpencil(i, j, k)
        end do
        call fishpack_fft_1D(ifwrd, LPx, tx, WX)
        do i = 1, dm%dccc%xsz(1)
          rhs_xpencil(i, j, k) = tx(i)
          !if(dabs(tx(i)) > 1.E+8) write(*,*) 'test5', tx(i)
        end do
      end do 
    end do
    !write(*,'(A, I3, 1ES13.5)') ('test5', i, rhs_xpencil(i,32,8), i=1, dm%dccc%xsz(1))
    !-----------------------------------------------------------
    ! scale the result
    !-----------------------------------------------------------
    rhs_xpencil = rhs_xpencil / SCALX / SCALZ
!WRITE(*,*)'fft-out',rhs_xpencil
  return
  end subroutine

end module