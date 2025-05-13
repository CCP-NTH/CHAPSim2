!Copyright (c) 2012-2022, Xcompact3d
!This file is part of Xcompact3d (xcompact3d.com)
!SPDX-License-Identifier: BSD 3-Clause

module decomp_2d_poisson

  use decomp_2d
  use decomp_2d_fft
  use poisson_interface_mod
  !use param
  !use variables

  implicit none

  private        ! Make everything private unless declared public

  !  real(mytype), private, parameter :: PI = 3.14159265358979323846_mytype

#ifdef DOUBLE_PREC
  real(mytype), parameter :: epsilon = 1.e-20_mytype
#else
  real(mytype), parameter :: epsilon = 1.e-8_mytype
#endif

  ! boundary conditions
  integer, save :: bcx, bcy, bcz

  ! decomposition object for physical space
  type(DECOMP_INFO), save :: ph

  ! decomposition object for spectral space
  type(DECOMP_INFO), save :: sp

  ! store sine/cosine factors
  complex(mytype), save, allocatable, dimension(:) :: cplx_circle_unit_halfx(:), &
                                                      cplx_circle_unit_halfy(:), &
                                                      cplx_circle_unit_halfz(:) 
  real(mytype), save, allocatable, dimension(:) :: wx(:), wy(:), wz(:)

  ! wave numbers
  complex(mytype), save, allocatable, dimension(:,:,:) :: kxyz
  !wave numbers for stretching in a pentadiagonal matrice
  complex(mytype), save, allocatable, dimension(:,:,:,:) :: a,a2,a3
  ! work arrays, 
  ! naming convention: cw (complex); rw (real); 
  !                    1 = X-pencil; 2 = Y-pencil; 3 = Z-pencil
  real(mytype), allocatable, dimension(:,:,:) :: rw1,rw1b,rw2,rw2b,rw3
  complex(mytype), allocatable, dimension(:,:,:) :: cw1,cw1b,cw2,cw22,cw2b,cw2c

  ! underlying FFT library only needs to be initialised once
  logical, save :: fft_initialised = .false.

  abstract interface
     subroutine poisson_xxx(rhs)
       use decomp_2d, only : mytype
       real(mytype), dimension(:,:,:), intent(inout) :: rhs
     end subroutine poisson_xxx
  end interface
  procedure (poisson_xxx), pointer :: poisson

  public :: decomp_2d_poisson_init,decomp_2d_poisson_finalize,poisson
  private :: fft_shift_ppp2ccc
  private :: complex_half_unit
contains

  



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Initialise Poisson solver for given boundary conditions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine decomp_2d_poisson_init()

    implicit none

    integer :: nx, ny, nz, i
    
    real(mytype) :: rl, iy
    external  rl, iy

    if (nclx) then
       bcx=0
    else
       bcx=1
    endif
    if (ncly) then
       bcy=0
    else
       bcy=1
    endif
    if (nclz) then
       bcz=0
    else
       bcz=1
    endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Top level wrapper
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (bcx==0 .and. bcy==0 .and. bcz==0) then
       poisson => poisson_000
    else if (bcx==1 .and. bcy==0 .and. bcz==0) then
       poisson => poisson_100
    else if (bcx==0 .and. bcy==1 .and. bcz==0) then
       poisson => poisson_010
    else if (bcx==1 .and. bcy==1) then   ! 110 & 111
       poisson => poisson_11x
    else
       stop 'boundary condition not supported'
    end if

    nx = nx_global
    ny = ny_global
    nz = nz_global

    ! pressure-grid having 1 fewer point for non-periodic directions
    if (bcx==1) nx=nx-1
    if (bcy==1) ny=ny-1
    if (bcz==1) nz=nz-1

#ifdef DEBUG_FFT 
    if (nrank .eq. 0) write(*,*)'# decomp_2d_poisson_init start'
#endif

    allocate( cmplx_circle_unit_halfx(nx) ) ! unit circle in complex plane = (cos(wx), sin(wx) )
    allocate( cmplx_circle_unit_halfy(ny) ) ! unit circle in complex plane = (cos(wy), sin(wy) )
    allocate( cmplx_circle_unit_halfz(nz) ) ! unit circle in complex plane = (cos(wz), sin(wz) )
    allocate( wx(nx) ) ! angle = (i-1) * pi/n for periodic, (i-1) * pi/2/n for non-periodic
    allocate( wy(ny) ) ! angle = (j-1) * pi/n for periodic, (i-1) * pi/2/n for non-periodic
    allocate( wz(nz) ) ! angle = (k-1) * pi/n for periodic, (i-1) * pi/2/n for non-periodic
    call complex_half_unit

#ifdef DEBUG_FFT 
    if (nrank .eq. 0) write(*,*)'# decomp_2d_poisson_init decomp_info_init'
#endif

    call decomp_info_init(nx, ny, nz, ph)
    call decomp_info_init(nx, ny, nz/2+1, sp)

#ifdef DEBUG_FFT 
    if (nrank .eq. 0) write(*,*)'# decomp_2d_poisson_init decomp_info_init ok'
#endif

    ! allocate work space
    if (bcx==0 .and. bcy==0 .and. bcz==0) then
       allocate(cw1(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(kxyz(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(a(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a2(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a3(sp%yst(1):sp%yen(1),ny,sp%yst(3):sp%yen(3),5))
    else if (bcx==1 .and. bcy==0 .and. bcz==0) then
       allocate(cw1(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(cw1b(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(rw1(ph%xst(1):ph%xen(1),ph%xst(2):ph%xen(2), &
            ph%xst(3):ph%xen(3)))
       allocate(rw1b(ph%xst(1):ph%xen(1),ph%xst(2):ph%xen(2), &
            ph%xst(3):ph%xen(3)))
       allocate(rw2(ph%yst(1):ph%yen(1),ph%yst(2):ph%yen(2), &
            ph%yst(3):ph%yen(3)))
       allocate(kxyz(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(a(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a2(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a3(sp%yst(1):sp%yen(1),ny,sp%yst(3):sp%yen(3),5))
    else if (bcx==0 .and. bcy==1 .and. bcz==0) then
       allocate(rw2(ph%yst(1):ph%yen(1),ph%yst(2):ph%yen(2), &
            ph%yst(3):ph%yen(3)))
       allocate(rw2b(ph%yst(1):ph%yen(1),ph%yst(2):ph%yen(2), &
            ph%yst(3):ph%yen(3)))
       allocate(cw1(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(cw2(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(cw22(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(cw2b(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(cw2c(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(kxyz(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(a(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a2(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a3(sp%yst(1):sp%yen(1),ny,sp%yst(3):sp%yen(3),5))
    else if (bcx==1 .and. bcy==1) then
       allocate(cw1(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(cw1b(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))
       allocate(cw2(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(cw22(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(cw2b(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(cw2c(sp%yst(1):sp%yen(1),sp%yst(2):sp%yen(2), &
            sp%yst(3):sp%yen(3)))
       allocate(rw1(ph%xst(1):ph%xen(1),ph%xst(2):ph%xen(2), &
            ph%xst(3):ph%xen(3)))
       allocate(rw1b(ph%xst(1):ph%xen(1),ph%xst(2):ph%xen(2), &
            ph%xst(3):ph%xen(3)))
       allocate(rw2(ph%yst(1):ph%yen(1),ph%yst(2):ph%yen(2), &
            ph%yst(3):ph%yen(3)))
       allocate(rw2b(ph%yst(1):ph%yen(1),ph%yst(2):ph%yen(2), &
            ph%yst(3):ph%yen(3)))
       if (bcz==1) then  
          allocate(rw3(ph%zsz(1),ph%zsz(2),ph%zsz(3)))
       end if
       allocate(kxyz(sp%xst(1):sp%xen(1),sp%xst(2):sp%xen(2), &
            sp%xst(3):sp%xen(3)))    
       allocate(a(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a2(sp%yst(1):sp%yen(1),ny/2,sp%yst(3):sp%yen(3),5))
       allocate(a3(sp%yst(1):sp%yen(1),nym,sp%yst(3):sp%yen(3),5))      
    end if

#ifdef DEBUG_FFT 
    if (nrank .eq. 0) write(*,*)'# decomp_2d_poisson_init before waves'
#endif

    call waves()
    if (bcy == 1 .and. istret /= ISTRET_NO) call matrice_refinement()
    !write(*,*) 'POinit ii1 arl ', rl(a(1,1,1,1)),rl(a(1,1,1,2)),rl(a(1,1,1,3)),&
    !                              rl(a(1,1,1,4)),rl(a(1,1,1,5))
    !write(*,*) 'POinit ii1 aiy ', iy(a(1,1,1,1)),iy(a(1,1,1,2)),iy(a(1,1,1,3)),&
    !                              iy(a(1,1,1,4)),iy(a(1,1,1,5))
    !!                     
    !write(*,*) 'POinit ii5 arl ', rl(a(5,5,5,1)),rl(a(5,5,5,2)),rl(a(5,5,5,3)),&
    !                              rl(a(5,5,5,4)),rl(a(5,5,5,5))
    !write(*,*) 'POinit ii5 aiy ', iy(a(5,5,5,1)),iy(a(5,5,5,2)),iy(a(5,5,5,3)),&
    !                              iy(a(5,5,5,4)),iy(a(5,5,5,5))
    !!!
    !write(*,*) 'POinit ii1 a2rl ', rl(a2(1,1,1,1)),rl(a2(1,1,1,2)),rl(a2(1,1,1,3)),&
    !                               rl(a2(1,1,1,4)),rl(a2(1,1,1,5))
    !write(*,*) 'POinit ii1 a2iy ', iy(a2(1,1,1,1)),iy(a2(1,1,1,2)),iy(a2(1,1,1,3)),&
    !                               iy(a2(1,1,1,4)),iy(a2(1,1,1,5))
    !!                     
    !write(*,*) 'POinit ii5 a2rl ', rl(a2(5,5,5,1)),rl(a2(5,5,5,2)),rl(a2(5,5,5,3)),&
    !                               rl(a2(5,5,5,4)),rl(a2(5,5,5,5))
    !write(*,*) 'POinit ii5 a2iy ', iy(a2(5,5,5,1)),iy(a2(5,5,5,2)),iy(a2(5,5,5,3)),&
    !                               iy(a2(5,5,5,4)),iy(a2(5,5,5,5))
    !!!
    !write(*,*) 'POinit ii1 a3rl ', rl(a3(1,1,1,1)),rl(a3(1,1,1,2)),rl(a3(1,1,1,3)),&
    !                               rl(a3(1,1,1,4)),rl(a3(1,1,1,5))
    !write(*,*) 'POinit ii1 a3iy ', iy(a3(1,1,1,1)),iy(a3(1,1,1,2)),iy(a3(1,1,1,3)),&
    !                               iy(a3(1,1,1,4)),iy(a3(1,1,1,5))
    !!                     
    !write(*,*) 'POinit ii5 a3rl ', rl(a3(5,5,5,1)),rl(a3(5,5,5,2)),rl(a3(5,5,5,3)),&
    !                               rl(a3(5,5,5,4)),rl(a3(5,5,5,5))
    !write(*,*) 'POinit ii5 a3iy ', iy(a3(5,5,5,1)),iy(a3(5,5,5,2)),iy(a3(5,5,5,3)),&
    !                               iy(a3(5,5,5,4)),iy(a3(5,5,5,5))

#ifdef DEBUG_FFT 
    if (nrank .eq. 0) write(*,*)'# decomp_2d_poisson_init end'
#endif

    return
  end subroutine decomp_2d_poisson_init


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Release memory used by Poisson solver
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine decomp_2d_poisson_finalize

    implicit none

    deallocate(ax,bx,ay,by,az,bz)

    call decomp_info_finalize(ph)
    call decomp_info_finalize(sp)

    call decomp_2d_fft_finalize
    fft_initialised = .false.

    deallocate(kxyz)

    if (bcx==0 .and. bcy==0 .and. bcz==0) then
       deallocate(cw1)
       deallocate(a,a2,a3)
    else if (bcx==1 .and. bcy==0 .and. bcz==0) then
       deallocate(cw1,cw1b,rw1,rw1b,rw2)
       deallocate(a,a2,a3)
    else if (bcx==0 .and. bcy==1 .and. bcz==0) then
       deallocate(cw1,cw2,cw2b,rw2,rw2b)
       deallocate(a,a2,a3)
    else if (bcx==1 .and. bcy==1) then
       deallocate(cw1,cw1b,cw2,cw2b,rw1,rw1b,rw2,rw2b)
       deallocate(a,a2,a3)
       if (bcz==1) then
          deallocate(rw3)
       end if
    end if

    return
  end subroutine decomp_2d_poisson_finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Solving 3D Poisson equation with periodic B.C in all 3 dimensions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine poisson_000(rhs)

    !use derivX
    !use derivY
    !use derivZ

    ! right-hand-side of Poisson as input
    ! solution of Poisson as output
    real(mytype), dimension(:,:,:), intent(INOUT) :: rhs

    integer, dimension(3) :: fft_start, fft_end, fft_size

    complex(mytype) :: xyzk

    complex(mytype) :: ytt,xtt,ztt,yt1,xt1,yt2,xt2
    complex(mytype) :: xtt1,ytt1,ztt1,zt1,zt2


    real(mytype) :: tmp1, tmp2,x ,y, z, avg_param

    integer :: nx,ny,nz, i,j,k

    complex(mytype) :: cx
    real(mytype) :: rl, iy
    external cx, rl, iy

    nx = nx_global
    ny = ny_global
    nz = nz_global

    if (.not. fft_initialised) then
       call decomp_2d_fft_init(PHYSICAL_IN_Z)
       fft_initialised = .true.
    end if
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (rhs, avg_param)
    if (nrank == 0) write(*,*)'## rhs_ijk physical ', avg_param
#endif

! #ifdef DEBUG_FFT
!     do k = ph%zst(3), ph%zen(3)
!       do j = ph%zst(2), ph%zen(2)
!         do i = ph%zst(1), ph%zen(1)
!           write(*, *) 'd-orgn', k, j, i, rhs(i, j, k)
!         end do
!       end do
!     end do
! #endif
!----------------------------------------------------------------------------------------------------------
! FFT of data sequence without grid information
    ! compute r2c transform 
    call decomp_2d_fft_3d(rhs,cw1)

    ! normalisation
    cw1 = cw1 / real(nx, kind=mytype) /real(ny, kind=mytype) &
         / real(nz, kind=mytype)
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (abs_prec(cw1), avg_param)
    if (nrank == 0) write(*,*)'## hat_lmn(rhs_ijk) ', avg_param
#endif
!----------------------------------------------------------------------------------------------------------
! FFT of data located at cell centre
    call fft3d_sp_shift_half_z0(cw1, IBACKWARD)
    call fft3d_sp_shift_half_y0(cw1, IBACKWARD)
    call fft3d_sp_shift_half_x0(cw1, IBACKWARD)
!----------------------------------------------------------------------------------------------------------
! calculation in spectral domain
    do k = sp%xst(3), sp%xen(3)
      do j = sp%xst(2), sp%xen(2)
        do i = sp%xst(1), sp%xen(1)
          tmp1 = rl(kxyz(i,j,k))
          tmp2 = iy(kxyz(i,j,k))
          ! CANNOT DO A DIVISION BY ZERO
          if ((abs_prec(tmp1) < epsilon).or.(abs_prec(tmp2) < epsilon)) then
            cw1(i,j,k) = zero
          else
            cw1(i,j,k) = cx(rl(cw1(i,j,k)) / (-tmp1), &
                            iy(cw1(i,j,k)) / (-tmp2))
          end if
        end do
      end do 
    end do
!----------------------------------------------------------------------------------------------------------
!   spectral data back to grid
    call fft3d_sp_shift_half_z0(cw1, IFORWARD)
    call fft3d_sp_shift_half_y0(cw1, IFORWARD)
    call fft3d_sp_shift_half_x0(cw1, IFORWARD)
!----------------------------------------------------------------------------------------------------------

#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (abs_prec(cw1), avg_param)
    if (nrank == 0) write(*,*)'## hat_lmn(rhs_ijk/wave) ', avg_param
#endif
    ! compute c2r transform
    call decomp_2d_fft_3d(cw1,rhs)

! #ifdef DEBUG_FFT
!     do k = ph%zst(3), ph%zen(3)
!       do j = ph%zst(2), ph%zen(2)
!         do i = ph%zst(1), ph%zen(1)
!           write(*, *) 'd-back', k, j, i, rhs(i, j, k)
!         end do
!       end do
!     end do
! #endif

#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (rhs, avg_param)
    if (nrank == 0) write(*,*)'## rhs_ijk physical new', avg_param
#endif
    !   call decomp_2d_fft_finalize

    return
  end subroutine poisson_000


  subroutine poisson_100(rhs)

    !use dbg_schemes, only: abs_prec
    use math_mod, only: abs_prec

    implicit none

    real(mytype), dimension(:,:,:), intent(INOUT) :: rhs

    complex(mytype) :: xyzk
    real(mytype) :: tmp1, tmp2, tmp3, tmp4
    real(mytype) :: xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8

    integer :: nx,ny,nz, i,j,k, itmp

    complex(mytype) :: cx
    real(mytype) :: rl, iy
    external cx, rl, iy

100 format(1x,a8,3I4,2F12.6)

    nx = nx_global - 1
    ny = ny_global
    nz = nz_global

    !write(*,*) 'Poisson_100'
    ! rhs is in Z-pencil but requires global operations in X
    call transpose_z_to_y(rhs,rw2,ph)
    call transpose_y_to_x(rw2,rw1,ph)
    do k = ph%xst(3), ph%xen(3)
       do j = ph%xst(2), ph%xen(2)
          do i = 1, nx/2
             rw1b(i,j,k) = rw1(2*(i-1)+1,j,k)
          enddo
          do i = nx/2 + 1, nx
             rw1b(i,j,k) = rw1(2*nx-2*i+2,j,k)
          enddo
       enddo
    end do

    call transpose_x_to_y(rw1b,rw2,ph)
    call transpose_y_to_z(rw2,rhs,ph)

    if (.not. fft_initialised) then
       call decomp_2d_fft_init(PHYSICAL_IN_Z,nx,ny,nz)
       fft_initialised = .true.
    end if

    ! compute r2c transform 
    call decomp_2d_fft_3d(rhs,cw1)

    ! normalisation
    cw1 = cw1 / real(nx, kind=mytype) /real(ny, kind=mytype) &
         / real(nz, kind=mytype)
#ifdef DEBUG_FFT
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'START', i, j, k, cw1(i,j,k)
             end if
          end do
       end do
    end do
#endif

    ! post-processing in spectral space

    ! POST PROCESSING IN Z
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bz(k) + tmp2 * az(k), &
                             tmp2 * bz(k) - tmp1 * az(k))
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'after z',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! POST PROCESSING IN Y
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * by(j) + tmp2 * ay(j), &
                             tmp2 * by(j) - tmp1 * ay(j))
             if (j > (ny/2+1)) cw1(i,j,k) = -cw1(i,j,k)
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'after y',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! POST PROCESSING IN X
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          cw1b(1,j,k) = cw1(1,j,k)
          do i = 2, nx
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             tmp3 = rl(cw1(nx-i+2,j,k))
             tmp4 = iy(cw1(nx-i+2,j,k))
             xx1=tmp1 * bx(i)
             xx2=tmp1 * ax(i)
             xx3=tmp2 * bx(i)
             xx4=tmp2 * ax(i)
             xx5=tmp3 * bx(i)
             xx6=tmp3 * ax(i)
             xx7=tmp4 * bx(i)
             xx8=tmp4 * ax(i)
             cw1b(i,j,k) = half * cx(xx1 + xx4 + xx5 - xx8, &
                                    -xx2 + xx3 + xx6 + xx7)
          end do
       end do
    end do
#ifdef DEBUG_FFT
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             if (abs_prec(cw1b(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'after x',i,j,k,cw1b(i,j,k)
             end if
          end do
       end do
    end do
#endif

    ! Solve Poisson
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(kxyz(i,j,k))
             tmp2 = iy(kxyz(i,j,k))
             ! CANNOT DO A DIVISION BY ZERO
             if ((abs_prec(tmp1) < epsilon).and.(abs_prec(tmp2) < epsilon)) then    
                cw1b(i,j,k)=cx(zero, zero)
             end if
             if ((abs_prec(tmp1) < epsilon).and.(abs_prec(tmp2) >= epsilon)) then
                cw1b(i,j,k)=cx(zero, iy(cw1b(i,j,k)) / (-tmp2))
             end if
             if ((abs_prec(tmp1) >= epsilon).and.(abs_prec(tmp2) < epsilon)) then    
                cw1b(i,j,k)=cx(rl(cw1b(i,j,k)) / (-tmp1), zero)
             end if
             if ((abs_prec(tmp1) >= epsilon).and.(abs_prec(tmp2) >= epsilon)) then
                cw1b(i,j,k)=cx(rl(cw1b(i,j,k)) / (-tmp1), iy(cw1b(i,j,k)) / (-tmp2))
             end if
#ifdef DEBUG_FFT
             if (abs_prec(cw1b(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'AFTER',i,j,k,cw1b(i,j,k)
#endif
          end do
       end do
    end do

    ! post-processing backward

    ! POST PROCESSING IN X
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          cw1(1,j,k) = cw1b(1,j,k)
          do i = 2, nx 
             tmp1 = rl(cw1b(i,j,k))
             tmp2 = iy(cw1b(i,j,k))
             tmp3 = rl(cw1b(nx-i+2,j,k))
             tmp4 = iy(cw1b(nx-i+2,j,k))
             xx1 = tmp1 * bx(i)
             xx2 = tmp1 * ax(i)
             xx3 = tmp2 * bx(i)
             xx4 = tmp2 * ax(i)
             xx5 = tmp3 * bx(i)
             xx6 = tmp3 * ax(i)
             xx7 = tmp4 * bx(i)
             xx8 = tmp4 * ax(i)
             cw1(i,j,k) = cx(xx1-xx4+xx6+xx7, &
                           -(-xx2-xx3+xx5-xx8))
          end do
       end do
    end do
#ifdef DEBUG_FFT
    do k = sp%xst(3),sp%xen(3)
       do j = sp%xst(2),sp%xen(2)
          do i = sp%xst(1),sp%xen(1)
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'AFTER X',i,j,k,cw1(i,j,k)
             end if
          end do
       end do
    end do
#endif

    ! POST PROCESSING IN Y
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * by(j) - tmp2 * ay(j), &
                             tmp2 * by(j) + tmp1 * ay(j))
             if (j > (ny/2+1)) cw1(i,j,k) = -cw1(i,j,k)
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'AFTER Y',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! POST PROCESSING IN Z
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bz(k) - tmp2 * az(k), &
                             tmp2 * bz(k) + tmp1 * az(k))
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'END',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! compute c2r transform
    call decomp_2d_fft_3d(cw1,rhs)

    ! rhs is in Z-pencil but requires global operations in X
    call transpose_z_to_y(rhs,rw2,ph)
    call transpose_y_to_x(rw2,rw1,ph)
    do k = ph%xst(3), ph%xen(3)
       do j = ph%xst(2), ph%xen(2)
          do i = 1, nx/2
             rw1b(2*i-1,j,k) = rw1(i,j,k)
          enddo
          do i = 1, nx/2
             rw1b(2*i,j,k) = rw1(nx-i+1,j,k)
          enddo
       enddo
    end do
    call transpose_x_to_y(rw1b,rw2,ph)
    call transpose_y_to_z(rw2,rhs,ph)

    !  call decomp_2d_fft_finalize

    return
  end subroutine poisson_100


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Solving 3D Poisson equation: Neumann in Y; periodic in X & Z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine poisson_010(rhs)

    !use dbg_schemes, only: abs_prec
    use math_mod, only: abs_prec
    
    

    implicit none

    real(mytype), dimension(:,:,:), intent(INOUT) :: rhs

    complex(mytype) :: xyzk
    real(mytype) :: tmp1, tmp2, tmp3, tmp4
    real(mytype) :: xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8

    integer :: nx,ny,nz, i,j,k

    complex(mytype) :: cx
    real(mytype) :: rl, iy
    external cx, rl, iy

    !real(mytype) :: avg_param

100 format(1x,a8,3I4,2F12.6)

    nx = nx_global
    ny = ny_global - 1
    nz = nz_global

#ifdef DEBUG_FFT
    if (nrank .eq. 0) write(*,*)'# Poisoon_010 Init'
#endif
    ! rhs is in Z-pencil but requires global operations in Y
    call transpose_z_to_y(rhs,rw2,ph)
    do k = ph%yst(3), ph%yen(3)
       do i = ph%yst(1), ph%yen(1)
          do j = 1, ny/2
             rw2b(i,j,k) = rw2(i,2*(j-1)+1,k)
          enddo
          do j = ny/2 + 1, ny
             rw2b(i,j,k) = rw2(i,2*ny-2*j+2,k)
          enddo
       enddo
    end do
    call transpose_y_to_z(rw2b,rhs,ph)

    if (.not. fft_initialised) then
       call decomp_2d_fft_init(PHYSICAL_IN_Z,nx,ny,nz)
       fft_initialised = .true.
    end if
    ! compute r2c transform 
    call decomp_2d_fft_3d(rhs,cw1)

    ! normalisation
    cw1 = cw1 / real(nx, kind=mytype) /real(ny, kind=mytype) &
         / real(nz, kind=mytype)
#ifdef DEBUG_FFT
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'START',i,j,k,cw1(i,j,k)
             end if
          end do
       end do
    end do
#endif

    ! post-processing in spectral space

    ! POST PROCESSING IN Z
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bz(k) + tmp2 * az(k), &
                             tmp2 * bz(k) - tmp1 * az(k))
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'after z',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! POST PROCESSING IN X
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bx(i) + tmp2 * ax(i), &
                             tmp2 * bx(i) - tmp1 * ax(i))
             if (i.gt.(nx/2+1)) cw1(i,j,k)=-cw1(i,j,k)
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'after x',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! POST PROCESSING IN Y
    ! NEED TO BE IN Y PENCILS!!!!!!!!!!!!!!!
    call transpose_x_to_y(cw1,cw2,sp)

    do k = sp%yst(3), sp%yen(3)
       do i = sp%yst(1), sp%yen(1)
          cw2b(i,1,k) = cw2(i,1,k)
          do j = 2, ny      
             tmp1 = rl(cw2(i,j,k))
             tmp2 = iy(cw2(i,j,k))
             tmp3 = rl(cw2(i,ny-j+2,k))
             tmp4 = iy(cw2(i,ny-j+2,k))
             xx1 = tmp1 * by(j)
             xx2 = tmp1 * ay(j)
             xx3 = tmp2 * by(j)
             xx4 = tmp2 * ay(j)
             xx5 = tmp3 * by(j)
             xx6 = tmp3 * ay(j)
             xx7 = tmp4 * by(j)
             xx8 = tmp4 * ay(j)
             cw2b(i,j,k) = half * cx(xx1+xx4+xx5-xx8, &
                                    -xx2+xx3+xx6+xx7)
          end do
       end do
    end do
#ifdef DEBUG_FFT
    do k = sp%yst(3), sp%yen(3)
       do j = sp%yst(2), sp%yen(2)
          do i = sp%yst(1), sp%yen(1)
             if (abs_prec(cw2b(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'after y',i,j,k,cw2b(i,j,k)
                write(*,*)kxyz(i,j,k)
             end if
          end do
       end do
    end do
#endif

    if (istret == ISTRET_NO) then 

       ! Solve Poisson
       ! doing wave number division in Y-pencil
       do k = sp%yst(3), sp%yen(3)
          do j = sp%yst(2), sp%yen(2)
             do i = sp%yst(1), sp%yen(1)
                tmp1 = rl(kxyz(i,j,k))
                tmp2 = iy(kxyz(i,j,k))
                !CANNOT DO A DIVISION BY ZERO
                if ((abs_prec(tmp1) < epsilon).and.(abs_prec(tmp2) < epsilon)) then    
                   cw2b(i,j,k) = cx(zero, zero)
                end if
                if ((abs_prec(tmp1) < epsilon).and.(abs_prec(tmp2) >= epsilon)) then
                   cw2b(i,j,k) = cx(zero, iy(cw2b(i,j,k)) / (-tmp2))
                end if
                if ((abs_prec(tmp1) >= epsilon).and.(abs_prec(tmp2) < epsilon)) then    
                   cw2b(i,j,k) = cx(rl(cw2b(i,j,k)) / (-tmp1), zero)
                end if
                if ((abs_prec(tmp1) >= epsilon).and.(abs_prec(tmp2) >= epsilon)) then
                   cw2b(i,j,k) = cx(rl(cw2b(i,j,k)) / (-tmp1), iy(cw2b(i,j,k)) / (-tmp2))
                end if
             end do
          end do
       end do

    else
       !call matrice_refinement()
       !write(*,*) 'PO_010 ii1 A rl ', rl(a(1,1,1,1)),rl(a(1,1,1,2)),rl(a(1,1,1,3)),&
       !                              rl(a(1,1,1,4)),rl(a(1,1,1,5))
       !write(*,*) 'PO_010 ii1 A iy ', iy(a(1,1,1,1)),iy(a(1,1,1,2)),iy(a(1,1,1,3)),&
       !                              iy(a(1,1,1,4)),iy(a(1,1,1,5))
       !!                 
       !write(*,*) 'PO_010 ii5 A rl ', rl(a(5,5,5,1)),rl(a(5,5,5,2)),rl(a(5,5,5,3)),&
       !                              rl(a(5,5,5,4)),rl(a(5,5,5,5))
       !write(*,*) 'PO_010 ii5 A iy ', iy(a(5,5,5,1)),iy(a(5,5,5,2)),iy(a(5,5,5,3)),&
       !                              iy(a(5,5,5,4)),iy(a(5,5,5,5))
       !!
       !write(*,*) 'PO_010 ii1 A2 rl ', rl(a2(1,1,1,1)),rl(a2(1,1,1,2)),rl(a2(1,1,1,3)),&
       !                               rl(a2(1,1,1,4)),rl(a2(1,1,1,5))
       !write(*,*) 'PO_010 ii1 A2 iy ', iy(a2(1,1,1,1)),iy(a2(1,1,1,2)),iy(a2(1,1,1,3)),&
       !                               iy(a2(1,1,1,4)),iy(a2(1,1,1,5))
       !!                 
       !write(*,*) 'PO_010 ii5 A2 rl ', rl(a2(5,5,5,1)),rl(a2(5,5,5,2)),rl(a2(5,5,5,3)),&
       !                               rl(a2(5,5,5,4)),rl(a2(5,5,5,5))
       !write(*,*) 'PO_010 ii5 A2 iy ', iy(a2(5,5,5,1)),iy(a2(5,5,5,2)),iy(a2(5,5,5,3)),&
       !                               iy(a2(5,5,5,4)),iy(a2(5,5,5,5))
       !!!
       !!!
       !write(*,*) 'PO_010 ii1 A3 rl ', rl(a3(1,1,1,1)),rl(a3(1,1,1,2)),rl(a3(1,1,1,3)),&
       !                          rl(a3(1,1,1,4)),rl(a3(1,1,1,5))
       !write(*,*) 'PO_010 ii1 A3 iy ', iy(a3(1,1,1,1)),iy(a3(1,1,1,2)),iy(a3(1,1,1,3)),&
       !                          iy(a3(1,1,1,4)),iy(a3(1,1,1,5))
       !!
       !write(*,*) 'PO_010 ii5 A3 rl ', rl(a3(5,5,5,1)),rl(a3(5,5,5,2)),rl(a3(5,5,5,3)),&
       !                             rl(a3(5,5,5,4)),rl(a3(5,5,5,5))
       !write(*,*) 'PO_010 ii5 A3 iy ', iy(a3(5,5,5,1)),iy(a3(5,5,5,2)),iy(a3(5,5,5,3)),&
       !                             iy(a3(5,5,5,4)),iy(a3(5,5,5,5))
       if (istret /= ISTRET_BOTTOM) then
          cw2 = zero
          cw2c = zero
          do k = sp%yst(3), sp%yen(3)
             do j = 1, ny/2
                do i = sp%yst(1), sp%yen(1)
                   cw2(i,j,k) = cw2b(i,2*j-1,k) 
                   cw2c(i,j,k) = cw2b(i,2*j,k)
                enddo
             enddo
          enddo

          call inversion5_v1(a,cw2,sp)
          call inversion5_v1(a2,cw2c,sp)

          cw2b = zero
          do k = sp%yst(3), sp%yen(3)
             do j = 1, ny-1,2
                do i = sp%yst(1), sp%yen(1)
                   cw2b(i,j,k) = cw2(i,(j+1)/2,k)
                enddo
             enddo
             do j = 2, ny, 2
                do i=sp%yst(1), sp%yen(1)
                   cw2b(i,j,k) = cw2c(i,j/2,k)
                enddo
             enddo
          enddo
       else
          do k = sp%yst(3), sp%yen(3)
             do j = 1, ny
                do i = sp%yst(1), sp%yen(1)
                   cw2(i,j,k) = cw2b(i,j,k) 
                enddo
             enddo
          enddo
          call inversion5_v2(a3,cw2,sp)
          do k = sp%yst(3), sp%yen(3)
             do j = 1, ny
                do i = sp%yst(1), sp%yen(1)
                   cw2b(i,j,k) = cw2(i,j,k) 
                enddo
             enddo
          enddo
       endif

    endif

    !we are in Y pencil
    do k = sp%yst(3), sp%yen(3)  
       do i = sp%yst(1), sp%yen(1)
          if ((i == nx/2+1).and.(k == nz/2+1)) then
             cw2b(i,:,k) = zero
          endif
       enddo
    enddo
#ifdef DEBUG_FFT
    do k = sp%yst(3), sp%yen(3)
       do j = sp%yst(2), sp%yen(2)
          do i = sp%yst(1), sp%yen(1)
             if (abs_prec(cw2b(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'AFTER',i,j,k,cw2b(i,j,k)
                write(*,*)kxyz(i,j,k)
             end if
          end do
       end do
    end do
#endif
    ! post-processing backward

    ! POST PROCESSING IN Y
    do k = sp%yst(3), sp%yen(3)
       do i = sp%yst(1), sp%yen(1)
          cw2(i,1,k) = cw2b(i,1,k)
          do j = 2, ny 
             tmp1 = rl(cw2b(i,j,k))
             tmp2 = iy(cw2b(i,j,k))
             tmp3 = rl(cw2b(i,ny-j+2,k))
             tmp4 = iy(cw2b(i,ny-j+2,k))
             xx1 = tmp1 * by(j)
             xx2 = tmp1 * ay(j)
             xx3 = tmp2 * by(j)
             xx4 = tmp2 * ay(j)
             xx5 = tmp3 * by(j)
             xx6 = tmp3 * ay(j)
             xx7 = tmp4 * by(j)
             xx8 = tmp4 * ay(j)
             cw2(i,j,k) = cx(xx1-xx4+xx6+xx7, &
                          -(-xx2-xx3+xx5-xx8))
          end do
       end do
    end do

    ! Back to X-pencil
    call transpose_y_to_x(cw2,cw1,sp)
#ifdef DEBUG_FFT
    do k = sp%xst(3),sp%xen(3)
       do j = sp%xst(2),sp%xen(2)
          do i = sp%xst(1),sp%xen(1)
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'AFTER Y',i,j,k,cw1(i,j,k)
             end if
          end do
       end do
    end do
#endif

    ! POST PROCESSING IN X
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bx(i) - tmp2 * ax(i), &
                             tmp2 * bx(i) + tmp1 * ax(i))
             if (i > (nx/2 + 1)) cw1(i,j,k) = -cw1(i,j,k)
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'AFTER X',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! POST PROCESSING IN Z
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bz(k) - tmp2 * az(k), &
                             tmp2 * bz(k) + tmp1 * az(k))
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'END',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do

    ! compute c2r transform, back to physical space
    call decomp_2d_fft_3d(cw1,rhs)

    ! rhs is in Z-pencil but requires global operations in Y
    call transpose_z_to_y(rhs,rw2,ph)
    do k = ph%yst(3), ph%yen(3)
       do i = ph%yst(1), ph%yen(1)
          do j = 1, ny/2
             rw2b(i,2*j-1,k) = rw2(i,j,k)
          enddo
          do j=1,ny/2
             rw2b(i,2*j,k) = rw2(i,ny-j+1,k)
          enddo
       enddo
    end do
    call transpose_y_to_z(rw2b,rhs,ph)

    !  call decomp_2d_fft_finalize

    return
  end subroutine poisson_010


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Solving 3D Poisson equation: Neumann in X, Y; Neumann/periodic in Z
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine poisson_11x(rhs)

    !use dbg_schemes, only: abs_prec
    use math_mod, only: abs_prec
    

    implicit none

    real(mytype), dimension(:,:,:), intent(INOUT) :: rhs

    complex(mytype) :: xyzk
    real(mytype) :: tmp1, tmp2, tmp3, tmp4
    real(mytype) :: xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8

    integer :: nx,ny,nz, i,j,k

    complex(mytype) :: cx
    real(mytype) :: rl, iy
    external cx, rl, iy
#ifdef DEBUG_FFT
    real(mytype) avg_param
#endif

100 format(1x,a8,3I4,2F12.6)

    nx = nx_global - 1
    ny = ny_global - 1
    !write(*,*) 'Poisson_11x'
    if (bcz == 1) then
       nz = nz_global - 1
    else if (bcz == 0) then
       nz = nz_global
    end if

    if (bcz == 1) then  
       do j = 1, ph%zsz(2)
          do i = 1, ph%zsz(1)
             do k = 1, nz/2
                rw3(i,j,k) = rhs(i,j,2*(k-1)+1)
             end do
             do k = nz/2 + 1, nz
                rw3(i,j,k) = rhs(i,j,2*nz-2*k+2)
             end do
          end do
       end do
       call transpose_z_to_y(rw3,rw2,ph)
    else if (bcz == 0) then
       call transpose_z_to_y(rhs,rw2,ph)
    end if


    do k = ph%yst(3), ph%yen(3)
       do i = ph%yst(1), ph%yen(1)
          do j = 1, ny/2
             rw2b(i,j,k) = rw2(i,2*(j-1)+1,k)
          end do
          do j = ny/2 + 1, ny
             rw2b(i,j,k) = rw2(i,2*ny-2*j+2,k)
          end do
       end do
    end do
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (rw2b, avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Start rw2 ', avg_param
#endif

    ! the global operations in X
    call transpose_y_to_x(rw2b,rw1,ph)

    do k = ph%xst(3), ph%xen(3)
       do j = ph%xst(2), ph%xen(2)
          do i = 1, nx/2
             rw1b(i,j,k) = rw1(2*(i-1)+1,j,k)
          end do
          do i = nx/2 + 1, nx
             rw1b(i,j,k) = rw1(2*nx-2*i+2,j,k)
          end do
       end do
    end do
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (rw1b, avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Start rw1 ', avg_param
#endif

    ! back to Z-pencil
    call transpose_x_to_y(rw1b,rw2,ph)
    call transpose_y_to_z(rw2,rhs,ph)

    if (.not. fft_initialised) then
       call decomp_2d_fft_init(PHYSICAL_IN_Z,nx,ny,nz)
       fft_initialised = .true.
    end if

    ! compute r2c transform 

    call decomp_2d_fft_3d(rhs,cw1)

    ! normalisation
    cw1 = cw1 / real(nx, kind=mytype) /real(ny, kind=mytype) &
         / real(nz, kind=mytype)
#ifdef DEBUG_FFT
    do k = sp%xst(3),sp%xen(3)
       do j = sp%xst(2),sp%xen(2)
          do i = sp%xst(1),sp%xen(1)
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'START',i,j,k,cw1(i,j,k)
             end if
          end do
       end do
    end do
#endif

    ! post-processing in spectral space

    ! POST PROCESSING IN Z
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bz(k) + tmp2 * az(k), &
                             tmp2 * bz(k) - tmp1 * az(k))
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'after z',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (abs_prec(cw1), avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Post in Z cw1 ', avg_param
#endif

    ! POST PROCESSING IN Y
    ! WE HAVE TO BE IN Y PENCILS
    call transpose_x_to_y(cw1,cw2,sp)
    do k = sp%yst(3), sp%yen(3)
       do i = sp%yst(1), sp%yen(1)
          cw2b(i,1,k) = cw2(i,1,k)
          do j = 2, ny 
             tmp1 = rl(cw2(i,j,k))
             tmp2 = iy(cw2(i,j,k))
             tmp3 = rl(cw2(i,ny-j+2,k))
             tmp4 = iy(cw2(i,ny-j+2,k))
             xx1 = tmp1 * by(j) * half
             xx2 = tmp1 * ay(j) * half
             xx3 = tmp2 * by(j) * half
             xx4 = tmp2 * ay(j) * half
             xx5 = tmp3 * by(j) * half
             xx6 = tmp3 * ay(j) * half
             xx7 = tmp4 * by(j) * half
             xx8 = tmp4 * ay(j) * half
             cw2b(i,j,k) = cx(xx1+xx4+xx5-xx8, &
                             -xx2+xx3+xx6+xx7)
          end do
       end do
    end do
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (abs_prec(cw2), avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Post in Y cw2 ', avg_param
#endif

    ! back to X-pencil
    call transpose_y_to_x(cw2b,cw1,sp)
#ifdef DEBUG_FFT
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'after y',i,j,k,cw1(i,j,k)
             end if
          end do
       end do
    end do
    avg_param = zero
    call avg3d (abs_prec(cw1), avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Back to X cw1 ', avg_param
#endif

    ! POST PROCESSING IN X
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          cw1b(1,j,k) = cw1(1,j,k)
          do i = 2, nx
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             tmp3 = rl(cw1(nx-i+2,j,k))
             tmp4 = iy(cw1(nx-i+2,j,k))
             xx1 = tmp1 * bx(i) * half
             xx2 = tmp1 * ax(i) * half
             xx3 = tmp2 * bx(i) * half
             xx4 = tmp2 * ax(i) * half
             xx5 = tmp3 * bx(i) * half
             xx6 = tmp3 * ax(i) * half
             xx7 = tmp4 * bx(i) * half
             xx8 = tmp4 * ax(i) * half
             cw1b(i,j,k) = cx(xx1+xx4+xx5-xx8, &
                             -xx2+xx3+xx6+xx7)
          end do
       end do
    end do

#ifdef DEBUG_FFT
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             if (abs_prec(cw1b(i,j,k)) > 1.0e-4_mytype) then
                write(*,*) 'BEFORE',i,j,k,cw1b(i,j,k)
             end if
          end do
       end do
    end do
    avg_param = zero
    call avg3d (abs_prec(cw1b), avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Back to X cw1b ', avg_param
#endif

    if (istret == ISTRET_NO) then

       ! Solve Poisson
       do k = sp%xst(3), sp%xen(3)
          do j = sp%xst(2), sp%xen(2)
             do i = sp%xst(1), sp%xen(1)
                tmp1 = rl(kxyz(i,j,k))
                tmp2 = iy(kxyz(i,j,k))
                !CANNOT DO A DIVISION BY ZERO
                if ((abs_prec(tmp1) < epsilon).and.(abs_prec(tmp2) < epsilon)) then    
                   cw1b(i,j,k) = cx(zero, zero)
                end if
                if ((abs_prec(tmp1) < epsilon).and.(abs_prec(tmp2) >= epsilon)) then
                   cw1b(i,j,k) = cx(zero, iy(cw1b(i,j,k)) / (-tmp2))
                end if
                if ((abs_prec(tmp1) >= epsilon).and.(abs_prec(tmp2) < epsilon)) then    
                   cw1b(i,j,k) = cx(rl(cw1b(i,j,k)) / (-tmp1), zero)
                end if
                if ((abs_prec(tmp1) >= epsilon).and.(abs_prec(tmp2) >= epsilon)) then
                   cw1b(i,j,k) = cx(real(cw1b(i,j,k)) / (-tmp1), iy(cw1b(i,j,k)) / (-tmp2))
                end if
             end do
          end do
       end do
#ifdef DEBUG_FFT
       avg_param = zero
       call avg3d (abs_prec(cw1b), avg_param)
       if (nrank == 0) write(*,*)'## Poisson11X Solve Pois istret 0 ', avg_param
#endif

    else
       call matrice_refinement()
       !write(*,*) 'PO_11X ii1 arl ', rl(a(1,1,1,1)),rl(a(1,1,1,2)),rl(a(1,1,1,3)),&
       !                              rl(a(1,1,1,4)),rl(a(1,1,1,5))
       !write(*,*) 'PO_11X ii1 aiy ', iy(a(1,1,1,1)),iy(a(1,1,1,2)),iy(a(1,1,1,3)),&
       !                              iy(a(1,1,1,4)),iy(a(1,1,1,5))
       !!                 
       !write(*,*) 'PO_11X ii5 arl ', rl(a(5,5,5,1)),rl(a(5,5,5,2)),rl(a(5,5,5,3)),&
       !                              rl(a(5,5,5,4)),rl(a(5,5,5,5))
       !write(*,*) 'PO_11X ii5 aiy ', iy(a(5,5,5,1)),iy(a(5,5,5,2)),iy(a(5,5,5,3)),&
       !                              iy(a(5,5,5,4)),iy(a(5,5,5,5))
       !!
       !write(*,*) 'PO_11X ii1 a2rl ', rl(a2(1,1,1,1)),rl(a2(1,1,1,2)),rl(a2(1,1,1,3)),&
       !                               rl(a2(1,1,1,4)),rl(a2(1,1,1,5))
       !write(*,*) 'PO_11X ii1 a2iy ', iy(a2(1,1,1,1)),iy(a2(1,1,1,2)),iy(a2(1,1,1,3)),&
       !                               iy(a2(1,1,1,4)),iy(a2(1,1,1,5))
       !!                 
       !write(*,*) 'PO_11X ii5 a2rl ', rl(a2(5,5,5,1)),rl(a2(5,5,5,2)),rl(a2(5,5,5,3)),&
       !                               rl(a2(5,5,5,4)),rl(a2(5,5,5,5))
       !write(*,*) 'PO_11X ii5 a2iy ', iy(a2(5,5,5,1)),iy(a2(5,5,5,2)),iy(a2(5,5,5,3)),&
       !                               iy(a2(5,5,5,4)),iy(a2(5,5,5,5))
       !!!
       !write(*,*) 'PO_11X ii1 rl ', rl(a3(1,1,1,1)),rl(a3(1,1,1,2)),rl(a3(1,1,1,3)),&
       !                             rl(a3(1,1,1,4)),rl(a3(1,1,1,5))
       !write(*,*) 'PO_11X ii1 iy ', iy(a3(1,1,1,1)),iy(a3(1,1,1,2)),iy(a3(1,1,1,3)),&
       !                             iy(a3(1,1,1,4)),iy(a3(1,1,1,5))
       !!
       !write(*,*) 'PO_11X ii1 rl ', rl(a3(5,5,5,1)),rl(a3(5,5,5,2)),rl(a3(5,5,5,3)),&
       !                             rl(a3(5,5,5,4)),rl(a3(5,5,5,5))
       !write(*,*) 'PO_11X ii1 iy ', iy(a3(5,5,5,1)),iy(a3(5,5,5,2)),iy(a3(5,5,5,3)),&
       !                             iy(a3(5,5,5,4)),iy(a3(5,5,5,5))
       ! the stretching is only working in Y pencils

       call transpose_x_to_y(cw1b,cw2b,sp)

       !we are now in Y pencil

       if (istret /= ISTRET_BOTTOM) then
          cw2 = zero
          cw2c = zero
          do k = sp%yst(3), sp%yen(3)
             do j = 1, ny/2
                do i = sp%yst(1), sp%yen(1)
                   cw2(i,j,k) = cw2b(i,2*j-1,k) 
                   cw2c(i,j,k) = cw2b(i,2*j,k)
                enddo
             enddo
          enddo

          call inversion5_v1(a,cw2,sp)
          call inversion5_v1(a2,cw2c,sp)

          cw2b = zero
          do k = sp%yst(3), sp%yen(3)
             do j = 1, ny-1, 2
                do i=sp%yst(1), sp%yen(1)
                   cw2b(i,j,k) = cw2(i,(j+1)/2,k)
                enddo
             enddo
             do j = 2, ny, 2
                do i = sp%yst(1), sp%yen(1)
                   cw2b(i,j,k) = cw2c(i,j/2,k)
                enddo
             enddo
          enddo
#ifdef DEBUG_FFT
          avg_param = zero
          call avg3d (abs_prec(cw2b), avg_param)
          if (nrank == 0) write(*,*)'## Poisson11X Solve Pois istret < 3 ', avg_param
#endif
       else
          cw2 = zero
          do k = sp%yst(3), sp%yen(3)
             do j = sp%yst(2), sp%yen(2)
                do i = sp%yst(1), sp%yen(1)
                   cw2(i,j,k) = cw2b(i,j,k) 
                enddo
             enddo
          enddo

          call inversion5_v2(a3,cw2,sp)

          do k = sp%yst(3), sp%yen(3)
             do j = sp%yst(2), sp%yen(2)
                do i = sp%yst(1), sp%yen(1)
                   cw2b(i,j,k) = cw2(i,j,k) 
                enddo
             enddo
          enddo
       endif
#ifdef DEBUG_FFT
          avg_param = zero
          call avg3d (abs_prec(cw2b), avg_param)
          if (nrank == 0) write(*,*)'## Poisson11X Solve Pois istret = 3 ', avg_param
#endif
       !we have to go back in X pencils
       call transpose_y_to_x(cw2b,cw1b,sp)
    endif

#ifdef DEBUG_FFT
    do k = sp%xst(3),sp%xen(3)
       do j = sp%xst(2),sp%xen(2)
          do i = sp%xst(1),sp%xen(1)
             if (abs_prec(cw1b(i,j,k)) > 1.0e-6) then
                write(*,*) 'AFTER',i,j,k,cw1b(i,j,k)
             end if
          end do
       end do
    end do
    avg_param = zero
    call avg3d (abs_prec(cw1b), avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Solve Pois AFTER ', avg_param
#endif
    !stop
    ! post-processing backward

    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          cw1(1,j,k) = cw1b(1,j,k)
          do i = 2, nx
             tmp1 = rl(cw1b(i,j,k))
             tmp2 = iy(cw1b(i,j,k))
             tmp3 = rl(cw1b(nx-i+2,j,k))
             tmp4 = iy(cw1b(nx-i+2,j,k))
             xx1 = tmp1 * bx(i)
             xx2 = tmp1 * ax(i)
             xx3 = tmp2 * bx(i)
             xx4 = tmp2 * ax(i)
             xx5 = tmp3 * bx(i)
             xx6 = tmp3 * ax(i)
             xx7 = tmp4 * bx(i)
             xx8 = tmp4 * ax(i)
             cw1(i,j,k) = cx(xx1-xx4+xx6+xx7, &
                          -(-xx2-xx3+xx5-xx8))
          end do
       end do
    end do
#ifdef DEBUG_FFT
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'AFTER X',i,j,k,cw1(i,j,k)
             end if
          end do
       end do
    end do
    avg_param = zero
    call avg3d (abs_prec(cw1), avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Solve Pois POSTPR X ', avg_param
#endif

    ! POST PROCESSING IN Y
    ! NEED to be in Y-pencil
    call transpose_x_to_y(cw1,cw2,sp)
    do k = sp%yst(3), sp%yen(3)
       do i = sp%yst(1), sp%yen(1)
          cw2b(i,1,k) = cw2(i,1,k)
          do j = 2, ny
             tmp1 = rl(cw2(i,j,k))
             tmp2 = iy(cw2(i,j,k))
             tmp3 = rl(cw2(i,ny-j+2,k))
             tmp4 = iy(cw2(i,ny-j+2,k))
             xx1 = tmp1 * by(j)
             xx2 = tmp1 * ay(j)
             xx3 = tmp2 * by(j)
             xx4 = tmp2 * ay(j)
             xx5 = tmp3 * by(j)
             xx6 = tmp3 * ay(j)
             xx7 = tmp4 * by(j)
             xx8 = tmp4 * ay(j)
             cw2b(i,j,k) = cx(xx1-xx4+xx6+xx7, &
                           -(-xx2-xx3+xx5-xx8))
          end do
       end do
    end do
#ifdef DEBUG_FFT
    do k = sp%yst(3), sp%yen(3)
       do j = sp%yst(2), sp%yen(2)
          do i = sp%yst(1), sp%yen(1)
             if (abs_prec(cw2b(i,j,k)) > 1.0e-4_mytype) then
                write(*,100) 'AFTER Y',i,j,k,cw2b(i,j,k)
             end if
          end do
       end do
    end do
   avg_param = zero
   call avg3d (abs_prec(cw2b), avg_param)
   if (nrank == 0) write(*,*)'## Poisson11X Solve Pois POSTPR Y ', avg_param
#endif
    ! back to X-pencil
    call transpose_y_to_x(cw2b,cw1,sp)

    ! POST PROCESSING IN Z
    do k = sp%xst(3), sp%xen(3)
       do j = sp%xst(2), sp%xen(2)
          do i = sp%xst(1), sp%xen(1)
             tmp1 = rl(cw1(i,j,k))
             tmp2 = iy(cw1(i,j,k))
             cw1(i,j,k) = cx(tmp1 * bz(k) - tmp2 * az(k), &
                             tmp2 * bz(k) + tmp1 * az(k))
#ifdef DEBUG_FFT
             if (abs_prec(cw1(i,j,k)) > 1.0e-4_mytype) &
                  write(*,100) 'END',i,j,k,cw1(i,j,k)
#endif
          end do
       end do
    end do
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (abs_prec(cw1), avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Solve Pois POSTPR Z ', avg_param
#endif

    ! compute c2r transform, back to physical space
    call decomp_2d_fft_3d(cw1,rhs)
#ifdef DEBUG_FFT
    avg_param = zero
    call avg3d (rhs, avg_param)
    if (nrank == 0) write(*,*)'## Poisson11X Solve Pois Back Phy RHS ', avg_param
#endif

    if (bcz == 1) then 
       do j = 1, ph%zsz(2)
          do i = 1, ph%zsz(1)
             do k = 1, nz/2
                rw3(i,j,2*k-1) = rhs(i,j,k)
             end do
             do k = 1, nz/2
                rw3(i,j,2*k) = rhs(i,j,nz-k+1)
             end do
          end do
       end do
       call transpose_z_to_y(rw3,rw2,ph)
    else if (bcz == 0) then 
       call transpose_z_to_y(rhs,rw2,ph)   
    end if

    do k = ph%yst(3), ph%yen(3)
       do i = ph%yst(1), ph%yen(1)
          do j = 1, ny/2
             rw2b(i,2*j-1,k) = rw2(i,j,k)
          end do
          do j = 1, ny/2
             rw2b(i,2*j,k) = rw2(i,ny-j+1,k)
          end do
       enddo
    end do
    call transpose_y_to_x(rw2b,rw1,ph)
    do k = ph%xst(3), ph%xen(3)
       do j = ph%xst(2), ph%xen(2)
          do i = 1, nx/2
             rw1b(2*i-1,j,k) = rw1(i,j,k)
          enddo
          do i = 1, nx/2
             rw1b(2*i,j,k) = rw1(nx-i+1,j,k)
          enddo
       enddo
    end do
    call transpose_x_to_y(rw1b,rw2,ph)
    call transpose_y_to_z(rw2,rhs,ph)

    !  call decomp_2d_fft_finalize

    return
  end subroutine poisson_11x

!==========================================================================================================
! complex unit half:
!   positive = e^{I PI/N * k} = cos(PI/N * k ) + i sin(PI/N * k)
!   negtive = e^{-I PI/N * k} = cos(PI/N * k ) + i sin(-PI/N * k)
!==========================================================================================================
  subroutine complex_half_unit
    integer :: i,j,k
    real(mytype) :: a, b
    real(mytype) :: theta ! theta = pi/n, or 1/2 pi/n

!----------------------------------------------------------------------------------------------------------
! x unit
!----------------------------------------------------------------------------------------------------------
    if (bcx == 0) then
      theta = PI / real(nx, kind=mytype)
    elseif (bcx == 1) then
      theta = PI * half / real(nx, kind=mytype)
    end if

    do i = 1, nx
      wx(i) = real(i-1, kind=mytype) * theta
      a = sin_prec( wx(i) )
      b = cos_prec( wx(i) )
      cplx_circle_unit_halfx(i) = CMPLX(b,  a)
    end do
!----------------------------------------------------------------------------------------------------------
! y unit
!----------------------------------------------------------------------------------------------------------
    if (bcy == 0) then
      theta = PI / real(ny, kind=mytype)
    elseif (bcy == 1) then
      theta = PI * half / real(ny, kind=mytype)
    end if

    do j = 1, ny
      wy(j) = real(j-1, kind=mytype) * theta
      a = sin_prec( wy(j) )
      b = cos_prec( wy(j) )
      cplx_circle_unit_halfy(j) = CMPLX(b,  a)
    end do
!----------------------------------------------------------------------------------------------------------
! z unit
!----------------------------------------------------------------------------------------------------------
    if (bcz == 0) then
      theta = PI / real(nz, kind=mytype)
    elseif (bcz == 1) then
      theta = PI * half / real(nz, kind=mytype)
    end if

    do k = 1, nz
      wz(k) = real(k-1, kind=mytype) * theta
      a = sin_prec( wz(k) )
      b = cos_prec( wz(k) )
      cplx_circle_unit_halfz(k) = CMPLX(b,  a)
    end do

    return
  end subroutine complex_half_unit

!==========================================================================================================
! FFT lib operates below calculation (forward, after fft):
!   <f>_k^i = 1/N * sum_{k = 0}^{N-1} f_i e^{- I k_x x_i} = 1/N * sum_{k = 0}^{N-1} f_i e^{- I 2 pi/N * k i}
! if input is located at i+1/2, then
!  <f>_k^{i+1/2} = 1/N * sum_{k = 0}^{N-1} f_{i+1/2} e^{- I k_x x_{i+1/2}}
!                = 1/N * sum_{k = 0}^{N-1} f_{i+1/2} e^{- I k_x x_{i}} * e^{-I k_x 1/2 dx}
!                = e^{-I k_x 1/2 dx} * 1/N * sum_{k = 0}^{N-1} f_{i+1/2} e^{- I k_x x_{i}}
!                = e^{-I k_x 1/2 dx} *  <f>
!                = <f> shifted backward
!                = [cos(k_x * 1/2 * dx) - I sin(k_x * 1/2 * dx) ] * FFT_lib_caculated a sequence of data without grid info.
!                = (bx - I ax)(tmp1 + I tmp2)
!                =  (bx * tmp1 + ax * tmp2) + I (bx * tmp2 - ax * tmp1)
! FFT lib operates below calculation (backward, before ifft):
!   f_{i} = sum_{k = 0}^{N-1} <f>_k e^{I k_x x_i} = sum_{k = 0}^{N-1} <f>_k e^{I 2 pi/N * k i}
! if input is located at i+1/2, then
!  f_{i+1/2} = sum_{k = 0}^{N-1} <f>_{i+1/2} e^{I k_x x_{i+1/2}}
!            = sum_{k = 0}^{N-1} <f>_{i+1/2} e^{I k_x x_{i}} * e^{I k_x 1/2 dx}
!            = sum_{k = 0}^{N-1} (<f>_{i+1/2} e^{I k_x 1/2 dx}) *  e^{ I k_x x_{i}}
!            = sum_{k = 0}^{N-1} <f>_shifted *  e^{ I k_x x_{i}}
!    <f>_shifted_forward = (<f>_{i+1/2} e^{I k_x 1/2 dx}) 
!                = (bx + I ax)(tmp1 + I tmp2)
!                = (bx * tmp1 - ax * tmp2) + i (ax * tmp1 + bx * tmp2)
! isign = 1,  multiplied by 
!==========================================================================================================
  subroutine fft3d_sp_shift_half_x0(cw1, isign) ! input, output both x-pencil, periodic 
    implicit none
    complex(mytype), dimension(sp%xst(1) : sp%xen(1), &
                                sp%xst(2) : sp%xen(2), &
                                sp%xst(3) : sp%xen(3)), intent(inout) :: cw1
    integer, intent(in) :: isign
    complex(mytype) :: cplx_circle_unit, cw_cp

    ! all based on x-pencil
    do k = sp%xst(3), sp%xen(3)
      do j = sp%xst(2), sp%xen(2)
        do i = sp%xst(1), sp%xen(1)
          if(isign == IBACKWARD) then
            cplx_circle_unit = CONJG(cplx_circle_unit_halfx(i))
          else if(isign == IFORWARD) then
            cplx_circle_unit = cplx_circle_unit_halfx(i)
          end if
          cw1(i,j,k) = cw1(i, j, k) * cplx_circle_unit
          if (i > (nx/2+1)) cw1(i,j,k) = -cw1(i,j,k)
        end do
      end do
    end do

    return
  end subroutine fft3d_sp_shift_half_x0


  subroutine fft3d_sp_shift_half_x1(cw1, isign) ! input, output both x-pencil, x non-periodic
    implicit none
    complex(mytype), dimension(sp%xst(1) : sp%xen(1), &
                                sp%xst(2) : sp%xen(2), &
                                sp%xst(3) : sp%xen(3)), intent(inout) :: cw1
    complex(mytype), dimension(sp%xst(1) : sp%xen(1)) :: cw1b
    complex(mytype) :: cplx_circle_unit_neg, cplx_circle_unit_pos, cw_cp

    ! this is based the specified data reconstruction, not universal.
    nx = nx_global - 1
    do k = sp%xst(3), sp%xen(3)
      do j = sp%xst(2), sp%xen(2)
        cw1b(1) = cw1(1,j,k)
        do i = 2, nx
          cplx_circle_unit_pos = cplx_circle_unit_halfx(i)
          cplx_circle_unit_neg = CONJG(cplx_circle_unit_halfx(i))
          cw_cp = cw1(nx - i + 2, j, k)

          if(isign == IBACKWARD) then
            cw1b(i) = half * cw1(i, j, k) * cplx_circle_unit_neg + &
                             cw_cp *        cplx_circle_unit_pos
          else if(isign == IFORWARD) then
            cw1b(i) =        cw1(i, j, k) * cplx_circle_unit_pos + &
                             cw_cp *        cplx_circle_unit_neg
          end if
        end do
        cw1(i, j, k) = cw1b(i)
      end do
    end do
    

    return
  end subroutine fft3d_sp_shift_half_x1

  subroutine fft3d_sp_shift_half_y0(cw1, isign)
    implicit none
    complex(mytype), dimension( sp%xst(1) : sp%xen(1), &
                                sp%xst(2) : sp%xen(2), &
                                sp%xst(3) : sp%xen(3)), intent(in) :: cw1
    integer, intent(in) :: isign
    complex(mytype) :: cplx_circle_unit_neg, cplx_circle_unit_pos, cw_cp

    ! all based on x-pencil

    do k = sp%xst(3), sp%xen(3)
      do j = sp%xst(2), sp%xen(2)
        if(isign == IBACKWARD) then
          cplx_circle_unit = CONJG(cplx_circle_unit_halfy(j))
        else if(isign == IFORWARD) then
          cplx_circle_unit = cplx_circle_unit_halfy(j)
        end if
        do i = sp%xst(1), sp%xen(1)
          cw1(i,j,k) = cw1(i, j, k) * cplx_circle_unit_neg
        end do
        if (j > (ny/2+1)) cw1(i,j,k) = -cw1(i,j,k)
      end do
    end do

    return
  end subroutine fft3d_sp_shift_half_y0


  subroutine fft3d_sp_shift_half_y1(cw1)
    implicit none
    complex(mytype), dimension( sp%xst(1) : sp%xen(1), &
                                sp%xst(2) : sp%xen(2), &
                                sp%xst(3) : sp%xen(3)), intent(in) :: cw1
    complex(mytype), dimension(sp%yst(1) : sp%yen(1)) :: cw1b
    complex(mytype) :: cplx_circle_unit_neg, cplx_circle_unit_pos, cw_cp

    ! all based on y-pencil

    ! this is based the specified data reconstruction, not universal.
    ny = ny_global - 1

    do k = sp%xst(3), sp%xen(3)
      do i = sp%xst(1), sp%xen(1)
        cw2b(1) = cw2(i,1,k)
        do j = 2, ny      
          cplx_circle_unit_pos = cplx_circle_unit_halfy(j)
          cplx_circle_unit_neg = CONJG(cplx_circle_unit_halfy(j))
          cw_cp = cw1(i, ny - j + 2, k)
          if(isign == IBACKWARD) then
            cw1b(j) = half * cw1(i, j, k) * cplx_circle_unit_neg + &
                             cw_cp        * cplx_circle_unit_pos
          else if(isign == IFORWARD) then
            cw1b(j) =        cw1(i, j, k) * cplx_circle_unit_pos + &
                             cw_cp *        cplx_circle_unit_neg
          end if
          
        end do
        cw1(i, j, k) = cw1b(j)
      end do
    end do

    return
  end subroutine fft3d_sp_shift_half_y1

  subroutine fft3d_sp_shift_half_z0(cw1, isign)
    implicit none
    complex(mytype), dimension( sp%xst(1) : sp%xen(1), &
                                sp%xst(2) : sp%xen(2), &
                                sp%xst(3) : sp%xen(3)), intent(in) :: cw1
    integer, intent(in) :: isign
    complex(mytype) :: cplx_circle_unit_neg, cplx_circle_unit_pos, cw_cp

    ! all based on x-pencil

    do k = sp%xst(3), sp%xen(3)
      if(isign == IBACKWARD) then
        cplx_circle_unit = CONJG(cplx_circle_unit_halfz(k))
      else if(isign == IFORWARD) then
        cplx_circle_unit = cplx_circle_unit_halfz(k)
      end if
      do j = sp%xst(2), sp%xen(2)
        do i = sp%xst(1), sp%xen(1)
          cw1(i,j,k) = cw1(i, j, k) * cplx_circle_unit_neg
        end do
      end do
    end do

    return
  end subroutine fft3d_sp_shift_half_z0


  subroutine 

  ! ***********************************************************
  !
  subroutine waves ()

    use decomp_2d
    use decomp_2d_fft
    use poisson_interface_mod

    implicit none

    integer :: i, j, k, nn
    real(mytype) :: w, wp, w1, w1p 

    xk2 = zero
    ykz = zero
    zk2 = zero
    kxyz = zero
!----------------------------------------------------------------------------------------------------------
!WAVE NUMBER IN X
!----------------------------------------------------------------------------------------------------------

    if (bcx == 0) then
      nn = nx/2 + 1
    else
      nn = nx
    end if

    do i = 1, nn
      w = two * wx(i)
      wp = acix6 * two * sin_prec(half * w) + &
           bcix6 * two * sin_prec(three * half * w)
      xkx(i) = wp / (one + two * alcaix6 * cos_prec(w))
      xk2(i) = cx_one_one * ( xkx(i)**2)
   enddo

    if (bcx == 0) then
       do i = nn + 1, nx
          xk2(i) = xk2(nx - i + 2)
       enddo
    else
       xk2(1) = zero
    endif
!----------------------------------------------------------------------------------------------------------
!WAVE NUMBER IN Y
!----------------------------------------------------------------------------------------------------------
    if (bcy == 0) then
      nn = ny/2 + 1
    else
      nn = ny
    end if

    do j= 1, nn
      w = two * wy(j)
      wp = aciy6 * two * sin_prec(half * w) + &
           bciy6 * two * sin_prec(three * half * w)
      if (istret == ISTRET_NO) yky(j) = wp / (one + two * alcaiy6 * cos_prec(w))
      if (istret /= ISTRET_NO) yky(j) = wp / (one + two * alcaiy6 * cos_prec(w)) * yly
      yk2(j) = cx_one_one * (yky(j)**2)
    enddo

    if (bcy == 0) then
       do j = nn + 1, ny
          yk2(j) = yk2(ny - j + 2)
       enddo
    else
       yk2(1) = zero
    endif
!----------------------------------------------------------------------------------------------------------
!WAVE NUMBER IN Z
!----------------------------------------------------------------------------------------------------------
    nn = nz/2 + 1

    do k= 1, nn
      w = two * wz(k)
      wp = aciz6 * two * sin_prec(half * w) + &
           bciz6 * two * sin_prec(three * half * w)
      zkz(k) = wp / (one + two * alcaiz6 * cos_prec(w))

      if (bcz == 0) then
        zk2(j) = cx_one_one * (zkz(k)**2)
      else 
        w_cp = two * wz(nz - k + 1)
        wp_cp = aciz6 * two * sin_prec(half * w_cp) + &
                bciz6 * two * sin_prec(three * half * w_cp)
        zkz_cp(k) = -wp_cp / (one + two * alcaiz6 * cos_prec(w_cp))


        zk2(k) = cx( zkz(k)**2, zkz_cp(k)**2 )
      end if
    enddo
!----------------------------------------------------------------------------------------------------------
!combine all three directions
!----------------------------------------------------------------------------------------------------------
    if ((bcx == 0).and.(bcz == 0).and.(bcy /= 0)) then
       do k = sp%yst(3), sp%yen(3)
          do j = sp%yst(2), sp%yen(2)
             do i = sp%yst(1), sp%yen(1)
                kxyz(i,j,k) = xk2(i) + yk2(j) + zk2(k)
             enddo
          enddo
       enddo

    else
      do k = sp%xst(3),sp%xen(3)
        do j = sp%xst(2),sp%xen(2)
           do i = sp%xst(1),sp%xen(1)
             kxyz(i,j,k) = xk2(i) + yk2(j) + zk2(k)
           enddo
        enddo
       enddo
    endif

  return
  end subroutine waves

  !**************************************************************************
  !
  subroutine matrice_refinement()
    !
    !**************************************************************************

    use decomp_2d
    !use variables
    !use param
    !use var
    !use MPI
    !use derivX 
    !use derivY 
    !use derivZ 
    !use dbg_schemes, only: cos_prec

    implicit none

    integer :: i,j,k

    complex(mytype),dimension(sp%yst(1):sp%yen(1)) :: transx
    complex(mytype),dimension(sp%yst(2):sp%yen(2)) :: transy
    complex(mytype),dimension(sp%yst(3):sp%yen(3)) :: transz

    real(mytype),dimension(sp%yst(1):sp%yen(1)) :: transx_rl, transx_rl2
    real(mytype),dimension(sp%yst(2):sp%yen(2)) :: transy_rl, transy_rl2
    real(mytype),dimension(sp%yst(3):sp%yen(3)) :: transz_rl, transz_iy, transz_rl2, transz_iy2

    real(mytype) :: xa0,xa1 
    complex(mytype) :: ytt,xtt,ztt,yt1,xt1,yt2,xt2
    complex(mytype) :: xtt1,ytt1,ztt1,zt1,zt2,tmp1,tmp2,tmp3
    

    complex(mytype) :: cx
    real(mytype) :: rl, iy
    external cx, rl, iy

    real(mytype) :: xtt_rl, xtt1_rl, xt1_rl
    real(mytype) :: rlexs

    real(mytype) :: ytt_rl, ytt1_rl, yt1_rl
    real(mytype) :: rleys

    real(mytype) :: ztt_rl, ztt1_rl, zt1_rl
    real(mytype) :: rlezs, iyezs
!
    real(mytype) :: xa0_2, xa1_2, xa01, xa0p1_2
!
    if ((istret == ISTRET_CENTRE) .or. (istret == ISTRET_2SIDES)) then
!
       xa0 = alpha / pi + half / beta / pi
       if (istret == ISTRET_CENTRE) xa1 = +one / four / beta / pi
       if (istret == ISTRET_2SIDES) xa1 = -one / four / beta / pi
!
       xa0_2 = xa0**2
       xa1_2 = xa1**2
       xa01 = xa0 * xa1
       xa0p1_2 = (xa0 + xa1)**2
!
!      construction of the pentadiagonal matrice
!
       do k = sp%yst(3), sp%yen(3)
          do j = 1, ny/2
             do i = sp%yst(1), sp%yen(1)
!
                cw22(i,j,k) = yky(2*j-1)
                cw2(i,j,k) = yky(2*j)
!
             enddo
          enddo
       enddo

       !main diagonal 
       do k = sp%yst(3), sp%yen(3)
          do j = 2, ny/2 - 1
             do i = sp%yst(1), sp%yen(1)
                a(i,j,k,3)=-cx(rl(xk2(i)) + rl(zk2(k))  &
                              + xa0_2 * rl(cw22(i, j, k))**2 &
                              + xa1_2 * rl(cw22(i, j, k)) * (rl(cw22(i, j-1, k)) + rl(cw22(i, j + 1, k))), &
                              iy(xk2(i)) + iy(zk2(k))  &
                              + xa0_2 * iy(cw22(i, j, k))**2 &
                              + xa1_2 * iy(cw22(i, j, k)) * (iy(cw22(i, j-1, k)) + iy(cw22(i, j + 1, k))) )
!
                a2(i,j,k,3)=-cx(rl(xk2(i)) +rl(zk2(k)) &
                               + xa0_2 * rl(cw2(i, j, k))**2 &
                               + xa1_2 * rl(cw2(i, j, k)) * (rl(cw2(i, j-1, k)) + rl(cw2(i, j+1, k))), &
                               iy(xk2(i))  +iy(zk2(k))  &
                               + xa0_2 * iy(cw2(i, j, k))**2 &
                               + xa1_2 * iy(cw2(i, j, k)) * (iy(cw2(i, j-1, k)) + iy(cw2(i, j+1, k))))
            enddo
          enddo
!
          do i=sp%yst(1), sp%yen(1)
!
             a(i,1,k,3)=-cx(rl(xk2(i)) + rl(zk2(k))  &
                           + xa0_2 * rl(cw22(i, 1, k))**2 & 
                           + xa1_2 * rl(cw22(i, 1, k)) * rl(cw22(i, 2, k)),&
                            iy(xk2(i)) +iy(zk2(k)) &
                           + xa0_2 * iy(cw22(i, 1, k))**2 &
                           + xa1_2 * iy(cw22(i, 1, k)) * iy(cw22(i, 2, k)))
!
             a(i,ny/2,k,3)=-cx(rl(xk2(i)) + rl(zk2(k))&
                              +xa0_2 * rl(cw22(i, ny/2, k))**2 &
                              +xa1_2 * rl(cw22(i, ny/2, k)) * rl(cw22(i, ny/2-1, k)), &
                               iy(xk2(i)) + iy(zk2(k))  &
                              +xa0_2 * iy(cw22(i, ny/2, k))**2 &
                              +xa1_2 * iy(cw22(i, ny/2, k)) * iy(cw22(i, ny/2-1, k)))
!
             a2(i,1,k,3)=-cx(rl(xk2(i)) + rl(zk2(k))  &
                            +(xa0_2 - xa1_2) * rl(cw2(i, 1, k))**2 &
                            + xa1_2 *          rl(cw2(i, 1, k)) * rl(cw2(i, 2, k)), &
                             iy(xk2(i)) + iy(zk2(k))  &
                            +(xa0_2 - xa1_2) * iy(cw2(i, 1, k))**2 &
                            + xa1_2  *         iy(cw2(i, 1, k)) * iy(cw2(i, 2, k)))
!
             a2(i,ny/2,k,3)=-cx(rl(xk2(i)) + rl(zk2(k))  &
                               + xa0p1_2 * rl(cw2(i, ny/2, k))**2 &
                               + xa1_2   * rl(cw2(i, ny/2, k)) * rl(cw2(i, ny/2-1, k)), &
                                iy(xk2(i)) + iy(zk2(k))  &
                               + xa0p1_2 * iy(cw2(i, ny/2, k))**2 &
                               + xa1_2   * iy(cw2(i, ny/2, k)) * iy(cw2(i, ny/2-1, k)))
!
          enddo
       enddo

       !sup diag +1
       do k = sp%yst(3), sp%yen(3)
          do j = 2, ny/2 - 1
             do i = sp%yst(1), sp%yen(1)   
!
                a(i,j,k,4)=xa01 * cx(rl(cw22(i, j+1, k)) * (rl(cw22(i, j, k)) + rl(cw22(i, j+1, k))), &
                                     iy(cw22(i, j+1, k)) * (iy(cw22(i, j, k)) + iy(cw22(i, j+1, k))))
!
                a2(i,j,k,4)=xa01 * cx(rl(cw2(i, j+1, k)) * (rl(cw2(i, j, k)) + rl(cw2(i, j+1, k))), &
                                      iy(cw2(i, j+1, k)) * (iy(cw2(i, j, k)) + iy(cw2(i, j+1, k))))
!
             enddo
          enddo
!
          do i=sp%yst(1), sp%yen(1)
!
             a(i,1,k,4)=two*xa01*cx(rl(cw22(i ,1, k)) * rl(cw22(i, 2, k)) + &
                                    rl(cw22(i, 2, k)) * rl(cw22(i, 2, k)), &
                                    iy(cw22(i ,1, k)) * iy(cw22(i, 2, k)) + &
                                    iy(cw22(i, 2, k)) * iy(cw22(i, 2, k)))
!
             a2(i,1,k,4)=cx((xa0 - xa1) * xa1 * (rl(cw2(i, 1, k)) * rl(cw2(i, 2, k))) + &
                                    xa0 * xa1 * (rl(cw2(i, 2, k)) * rl(cw2(i ,2, k))) , &
                            (xa0 - xa1) * xa1 * (iy(cw2(i, 1, k)) * iy(cw2(i, 2, k))) + &
                                    xa0 * xa1 * (iy(cw2(i, 2, k)) * iy(cw2(i ,2, k))) )
!
             a2(i,ny/2-1,k,4)=cx(xa0 *        xa1 *  rl(cw2(i, ny/2-1, k)) *rl(cw2(i, ny/2, k))+ &
                                (xa0 + xa1) * xa1 * (rl(cw2(i, ny/2, k))**2), &
                                 xa0 *        xa1 *  iy(cw2(i, ny/2-1, k)) *iy(cw2(i, ny/2, k))+ &
                                (xa0 + xa1) * xa1 * (iy(cw2(i, ny/2, k))**2))
!
             a2(i,ny/2,k,4) = zero
!
          enddo
       enddo
!
       !sup diag +2
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)   
             do j = 1, ny/2 - 2
!
                a(i,j,k,5)  = xa1_2*cx(-rl(cw22(i, j+1, k))*rl(cw22(i, j+2, k)),&
                                       -iy(cw22(i, j+1, k))*iy(cw22(i, j+2, k)))
                a2(i,j,k,5) = xa1_2*cx(-rl( cw2(i, j+1, k))*rl( cw2(i, j+2, k)),&
                                       -iy( cw2(i, j+1, k))*iy( cw2(i, j+2, k)))
!
             enddo
!
              a(i, 1,      k, 5) = two * cx(rl(a(i, 1, k, 5)), iy(a(i, 1, k, 5)))
              a(i, ny/2-1, k, 5) = zero
              a(i, ny/2,   k, 5) = zero
             a2(i, ny/2-1, k, 5) = zero
             a2(i, ny/2,   k, 5) = zero 
!
          enddo
       enddo

       !inf diag -1
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)   
             do j = 2, ny/2
                a(i,j,k,2)  = xa01 * cx(rl(cw22(i, j-1, k)) * (rl(cw22(i, j, k)) + rl(cw22(i, j-1, k))), &
                                        iy(cw22(i, j-1, k)) * (iy(cw22(i, j, k)) + iy(cw22(i, j-1, k))))
                a2(i,j,k,2) = xa01 * cx(rl( cw2(i, j-1, k)) * (rl( cw2(i, j, k)) + rl( cw2(i, j-1, k))), &
                                        iy( cw2(i, j-1, k)) * (iy( cw2(i, j, k)) + iy( cw2(i, j-1, k))))
             enddo
              a(i, 1,    k, 2) = zero
             a2(i, 1,    k, 2) = zero
             a2(i, 2,    k, 2) = cx( xa0 *        xa1 * (rl(cw2(i, 2,      k)) * rl(cw2(i, 1,      k))) &
                                  + (xa0 + xa1) * xa1 * (rl(cw2(i, 1,      k)) * rl(cw2(i, 1,      k))), &
                                    xa0 *         xa1 * (iy(cw2(i, 2,      k)) * iy(cw2(i, 1,      k))) &
                                  + (xa0 + xa1) * xa1 * (iy(cw2(i, 1,      k)) * iy(cw2(i, 1,      k))))
             a2(i, ny/2, k, 2) = cx((xa0 + xa1) * xa1 * (rl(cw2(i, ny/2,   k)) * rl(cw2(i, ny/2-1, k))) &
                                    + xa0 *       xa1 * (rl(cw2(i, ny/2-1, k)) * rl(cw2(i, ny/2-1, k))), &
                                    (xa0 + xa1) * xa1 * (iy(cw2(i, ny/2,   k)) * iy(cw2(i, ny/2-1, k))) &
                                    + xa0 *       xa1 * (iy(cw2(i, ny/2-1, k)) * iy(cw2(i, ny/2-1, k))))
!
          enddo
       enddo
       !inf diag -2
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)  
             do j = 3, ny/2
                 a(i, j, k, 1) = xa1_2 * cx(-rl(cw22(i, j-1, k)) * rl(cw22(i, j-2, k)),&
                                            -iy(cw22(i, j-1, k)) * iy(cw22(i, j-2, k)))
                a2(i, j, k, 1) = xa1_2 * cx(-rl( cw2(i, j-1, k)) * rl( cw2(i, j-2, k)),&
                                            -iy( cw2(i, j-1, k)) * iy( cw2(i, j-2, k)))
             enddo
              a(i, 1, k, 1) = zero
              a(i, 2, k, 1) = zero
             a2(i, 1, k, 1) = zero
             a2(i, 2, k, 1) = zero
          enddo
       enddo
       !not to have a singular matrice
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)
             if ((rl(xk2(i)) == zero).and.(rl(zk2(k)) == zero)) then
                a(i, 1, k, 3) = cx_one_one
                a(i, 1, k, 4) = zero
                a(i, 1, k, 5) = zero
             endif
          enddo
       enddo
!
    else
!
       xa0 = alpha / pi + half / beta / pi
       xa1 = -one / four / beta / pi 
!
       xa0_2 = xa0**2
       xa1_2 = xa1**2
       xa01 = xa0 * xa1
!
       !construction of the pentadiagonal matrice
       !   
       do k = sp%yst(3), sp%yen(3)
          do j = 1, nym
             do i = sp%yst(1), sp%yen(1)
                cw22(i,j,k) = yky(j)
             enddo
          enddo
       enddo

       !main diagonal 
       do k = sp%yst(3),sp%yen(3)
          do j = 2, nym-1
             do i = sp%yst(1), sp%yen(1)
                a3(i,j,k,3) = -cx(rl(xk2(i)) + rl(zk2(k)) &
                                 + xa0_2 * rl(cw22(i, j, k))**2 &
                                 + xa1_2 * rl(cw22(i, j, k)) * (rl(cw22(i, j-1, k)) + rl(cw22(i, j+1, k))), &
                                  iy(xk2(i)) + iy(zk2(k)) &
                                 + xa0_2 * iy(cw22(i, j, k))**2 &
                                 + xa1_2 * iy(cw22(i, j, k)) * (iy(cw22(i, j-1, k)) + iy(cw22(i, j+1, k))))
             enddo
          enddo
       enddo

       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)
!
             a3(i,1,k,3) = -cx(rl(xk2(i)) + rl(zk2(k)) &
                              + xa0_2 * rl(cw22(i, 1, k))**2 &
                              + xa1_2 * rl(cw22(i, 1, k)) * rl(cw22(i, 2, k)),&
                               iy(xk2(i)) + iy(zk2(k))  &
                              + xa0_2 * iy(cw22(i, 1, k))**2 &
                              + xa1_2 * iy(cw22(i, 1, k)) * iy(cw22(i, 2, k)))
!
             a3(i,nym,k,3) = -cx(rl(xk2(i)) + rl(zk2(k))  &
                               + xa0_2 * rl(cw22(i, nym, k))**2 &
                               + xa1_2 * rl(cw22(i, nym, k)) * rl(cw22(i, nym-1, k)), &
                                iy(xk2(i)) + iy(zk2(k)) &
                               + xa0_2 * iy(cw22(i, nym, k))**2 &
                               + xa1_2 * iy(cw22(i, nym, k)) * iy(cw22(i, nym-1, k)))
!
          enddo
       enddo

       !sup diag +1
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)
             do j = 2, nym - 1
                a3(i,j,k,4) = xa01 * cx(rl(cw22(i, j+1, k)) * (rl(cw22(i, j, k)) + rl(cw22(i, j+1, k))), &
                                        iy(cw22(i, j+1, k)) * (iy(cw22(i, j, k)) + iy(cw22(i, j+1, k))))
             enddo
                a3(i,1,k,4) = xa01 * cx(rl(cw22(i, 2, k)) * (rl(cw22(i, 1, k)) + rl(cw22(i, 2, k))), &
                                        iy(cw22(i, 2, k)) * (iy(cw22(i, 1, k)) + iy(cw22(i, 2, k))))
          enddo
       enddo

       !sup diag +2
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)
             do j = 1, nym - 2
                a3(i, j, k, 5) = -xa1_2 * cx(rl(cw22(i, j+1, k)) * rl(cw22(i, j+2, k)), &
                                             iy(cw22(i, j+1, k)) * iy(cw22(i, j+2, k)))
             enddo
             a3(i, nym-1,k, 5) = zero
             a3(i, nym,  k, 5) = zero
          enddo
       enddo

       !inf diag -1
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)
             do j = 2, nym
                a3(i, j, k, 2) = xa01 * cx(rl(cw22(i, j-1, k)) * (rl(cw22(i, j, k)) + rl(cw22(i, j-1, k))), &
                                           iy(cw22(i, j-1, k)) * (iy(cw22(i, j, k)) + iy(cw22(i, j-1, k))))
             enddo
             a3(i, 1, k, 2) = zero
          enddo
       enddo

       !inf diag -2
       do k = sp%yst(3), sp%yen(3)
          do i = sp%yst(1), sp%yen(1)
             do j = 3, nym
                a3(i, j, k, 1) = -xa1_2 * cx(rl(cw22(i, j-1, k)) * rl(cw22(i, j-2, k)),&
                                             iy(cw22(i, j-1, k)) * iy(cw22(i, j-2, k)))
             enddo
             a3(i, 1, k, 1) = zero
             a3(i, 2, k, 1) = zero
          enddo
       enddo

       !not to have a singular matrice
       if (nrank==0) then
          a3(1, 1, 1, 3) = cx_one_one
          a3(1, 1, 1, 4) = zero
          a3(1, 1, 1, 5) = zero
       endif
    endif

    return
  end subroutine matrice_refinement
!=====================================
subroutine avg3d (var, avg)

  use decomp_2d, only: real_type, xsize, xend
  !use param
  !use dbg_schemes, only: sqrt_prec
  !use variables, only: nx,ny,nz,nxm,nym,nzm
  !use mpi

  implicit none

  real(mytype),dimension(xsize(1),xsize(2),xsize(3)),intent(in) :: var
  real(mytype), intent(out) :: avg
  real(mytype)              :: dep

  integer :: i,j,k, code
  integer :: nxc, nyc, nzc, xsize1, xsize2, xsize3

  if (nclx1==1.and.xend(1)==nx) then
     xsize1=xsize(1)-1
  else
     xsize1=xsize(1)
  endif
  if (ncly1==1.and.xend(2)==ny) then
     xsize2=xsize(2)-1
  else
     xsize2=xsize(2)
  endif
  if (nclz1==1.and.xend(3)==nz) then
     xsize3=xsize(3)-1
  else
     xsize3=xsize(3)
  endif
  if (nclx1==1) then
     nxc=nxm
  else
     nxc=nx
  endif
  if (ncly1==1) then
     nyc=nym
  else
     nyc=ny
  endif
  if (nclz1==1) then
     nzc=nzm
  else
     nzc=nz
  endif

  dep=zero
  do k=1,xsize3
     do j=1,xsize2
        do i=1,xsize1
           !dep=dep+var(i,j,k)**2
           dep=dep+var(i,j,k)
        enddo
     enddo
  enddo
  call MPI_ALLREDUCE(dep,avg,1,real_type,MPI_SUM,MPI_COMM_WORLD,code)
  !avg=sqrt_prec(avg)/(nxc*nyc*nzc)
  avg=avg/(nxc*nyc*nzc)

  return

end subroutine avg3d

end module decomp_2d_poisson

