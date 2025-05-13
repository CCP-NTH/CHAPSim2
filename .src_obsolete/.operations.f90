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
! This program is disatributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
! details.
!
! You should have received a copy of the GNU General Public License along with
! this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
! Street, Fifth Floor, Boston, MA 02110-1301, USA.

!----------------------------------------------------------------------------------------------------------
!==========================================================================================================
!> \file operations.f90
!>
!> \brief A general operation of derivative and interpolation in 1D.
!>
!==========================================================================================================
module operations
  use precision_mod, only : WP
  implicit none

  private
!----------------------------------------------------------------------------------------------------------
! basic coefficients for TDMA of 1st deriviative  
! to store coefficients for TDMA
!     d1fC2C vs d1rC2C :
!       f : coefficients in the LHS, unknown side.
!       r : coefficients in the RHS, known side. 
! eg, d1fC2C(5, 3, 5)
!     First column: 1:2 for one side b.c.
!                   4:5 for the other side b.c.
!                   3   for interior
!     Second column: 1 for coefficients of LHS f^(1)_{i-1}
!                    2 for coefficients of LHS f^(1)_{i}
!                    3 for coefficients of LHS f^(1)_{i+1}
!     Third column:  for b.c. flags
!                 IBC_INTERIOR    = 0, &
!                 IBC_PERIODIC    = 1, &
!                 IBC_SYMMETRIC   = 2, &
!                 IBC_ASYMMETRIC  = 3, &
!                 IBC_DIRICHLET   = 4, &
!                 IBC_NEUMANN     = 5, &
!                 IBC_INTRPL      = 6, &
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
! for 1st derivative
!----------------------------------------------------------------------------------------------------------
  ! collocated C2C
  real(WP), save, public :: d1fC2C(5, 3, 0:6)
  real(WP), save, public :: d1rC2C(5, 4, 0:6)
  
  ! collocated P2P
  real(WP), save :: d1fP2P(5, 3, 0:6)
  real(WP), save :: d1rP2P(5, 4, 0:6)

  ! staggered C2P
  real(WP), save, public :: d1fC2P(5, 3, 0:6)
  real(WP), save, public :: d1rC2P(5, 4, 0:6)

  ! staggered P2C
  real(WP), save :: d1fP2C(5, 3, 0:6)
  real(WP), save :: d1rP2C(5, 4, 0:6)
!----------------------------------------------------------------------------------------------------------
! for 2nd derivative
!----------------------------------------------------------------------------------------------------------
  ! collocated C2C
  real(WP), save, public :: d2fC2C(5, 3, 0:6)
  real(WP), save, public :: d2rC2C(5, 4, 0:6) ! one more value used. 
  
  ! collocated P2P
  real(WP), save :: d2fP2P(5, 3, 0:6)
  real(WP), save :: d2rP2P(5, 4, 0:6)
!----------------------------------------------------------------------------------------------------------
! for iterpolation
!----------------------------------------------------------------------------------------------------------
  ! interpolation P2C
  real(WP), save :: m1fP2C(5, 3, 0:6)
  real(WP), save :: m1rP2C(5, 4, 0:6)

  ! interpolation C2P
  real(WP), save, public :: m1fC2P(5, 3, 0:6)
  real(WP), save, public :: m1rC2P(5, 4, 0:6)

!----------------------------------------------------------------------------------------------------------
! coefficients array for TDMA of 1st deriviative  
! to store coefficients array for TDMA
!----------------------------------------------------------------------------------------------------------
  type t_xtdma_lhs
!----------------------------------------------------------------------------------------------------------
!   x : pre-processed TDMA LHS Matrix for 1st deriviative
!----------------------------------------------------------------------------------------------------------
    real(WP), allocatable :: ad1x_P2P(:, :, :)
    real(WP), allocatable :: bd1x_P2P(:, :, :)
    real(WP), allocatable :: cd1x_P2P(:, :, :)
    real(WP), allocatable :: dd1x_P2P(:, :, :)

    real(WP), allocatable :: ad1x_C2C(:, :, :)
    real(WP), allocatable :: bd1x_C2C(:, :, :)
    real(WP), allocatable :: cd1x_C2C(:, :, :)
    real(WP), allocatable :: dd1x_C2C(:, :, :)

    real(WP), allocatable :: ad1x_P2C(:, :, :)
    real(WP), allocatable :: bd1x_P2C(:, :, :)
    real(WP), allocatable :: cd1x_P2C(:, :, :)
    real(WP), allocatable :: dd1x_P2C(:, :, :)

    real(WP), allocatable :: ad1x_C2P(:, :, :)
    real(WP), allocatable :: bd1x_C2P(:, :, :)
    real(WP), allocatable :: cd1x_C2P(:, :, :)
    real(WP), allocatable :: dd1x_C2P(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   x : pre-processed TDMA LHS Matrix for 2nd deriviative
!----------------------------------------------------------------------------------------------------------
    real(WP), allocatable :: ad2x_P2P(:, :, :)
    real(WP), allocatable :: bd2x_P2P(:, :, :)
    real(WP), allocatable :: cd2x_P2P(:, :, :)
    real(WP), allocatable :: dd2x_P2P(:, :, :)

    real(WP), allocatable :: ad2x_C2C(:, :, :)
    real(WP), allocatable :: bd2x_C2C(:, :, :)
    real(WP), allocatable :: cd2x_C2C(:, :, :)
    real(WP), allocatable :: dd2x_C2C(:, :, :)
!----------------------------------------------------------------------------------------------------------
!   x : pre-processed TDMA LHS Matrix for mid-point interpolation
!----------------------------------------------------------------------------------------------------------
    real(WP), allocatable :: am1x_P2C(:, :, :)
    real(WP), allocatable :: bm1x_P2C(:, :, :)
    real(WP), allocatable :: cm1x_P2C(:, :, :)
    real(WP), allocatable :: dm1x_P2C(:, :, :)

    real(WP), allocatable :: am1x_C2P(:, :, :)
    real(WP), allocatable :: bm1x_C2P(:, :, :)
    real(WP), allocatable :: cm1x_C2P(:, :, :)
    real(WP), allocatable :: dm1x_C2P(:, :, :)
  end type t_xtdma_lhs

  type(t_xtdma_lhs), allocatable :: xtdma_lhs(:) 

!----------------------------------------------------------------------------------------------------------
! y : pre-processed TDMA LHS Matrix for 1st deriviative
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: ad1y_P2P(:, :, :)
  real(WP), allocatable :: bd1y_P2P(:, :, :)
  real(WP), allocatable :: cd1y_P2P(:, :, :)
  real(WP), allocatable :: dd1y_P2P(:, :, :)

  real(WP), allocatable :: ad1y_C2C(:, :, :)
  real(WP), allocatable :: bd1y_C2C(:, :, :)
  real(WP), allocatable :: cd1y_C2C(:, :, :)
  real(WP), allocatable :: dd1y_C2C(:, :, :)

  real(WP), allocatable :: ad1y_P2C(:, :, :)
  real(WP), allocatable :: bd1y_P2C(:, :, :)
  real(WP), allocatable :: cd1y_P2C(:, :, :)
  real(WP), allocatable :: dd1y_P2C(:, :, :)

  real(WP), allocatable :: ad1y_C2P(:, :, :)
  real(WP), allocatable :: bd1y_C2P(:, :, :)
  real(WP), allocatable :: cd1y_C2P(:, :, :)
  real(WP), allocatable :: dd1y_C2P(:, :, :)
!----------------------------------------------------------------------------------------------------------
! y : pre-processed TDMA LHS Matrix for 2nd deriviative
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: ad2y_P2P(:, :, :)
  real(WP), allocatable :: bd2y_P2P(:, :, :)
  real(WP), allocatable :: cd2y_P2P(:, :, :)
  real(WP), allocatable :: dd2y_P2P(:, :, :)

  real(WP), allocatable :: ad2y_C2C(:, :, :)
  real(WP), allocatable :: bd2y_C2C(:, :, :)
  real(WP), allocatable :: cd2y_C2C(:, :, :)
  real(WP), allocatable :: dd2y_C2C(:, :, :)
!----------------------------------------------------------------------------------------------------------
! y : pre-processed TDMA LHS Matrix for mid-point interpolation
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: am1y_P2C(:, :, :)
  real(WP), allocatable :: bm1y_P2C(:, :, :)
  real(WP), allocatable :: cm1y_P2C(:, :, :)
  real(WP), allocatable :: dm1y_P2C(:, :, :)

  real(WP), allocatable :: am1y_C2P(:, :, :)
  real(WP), allocatable :: bm1y_C2P(:, :, :)
  real(WP), allocatable :: cm1y_C2P(:, :, :)
  real(WP), allocatable :: dm1y_C2P(:, :, :)

!----------------------------------------------------------------------------------------------------------
! z : pre-processed TDMA LHS Matrix for 1st deriviative
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: ad1z_P2P(:, :, :)
  real(WP), allocatable :: bd1z_P2P(:, :, :)
  real(WP), allocatable :: cd1z_P2P(:, :, :)
  real(WP), allocatable :: dd1z_P2P(:, :, :)

  real(WP), allocatable :: ad1z_C2C(:, :, :)
  real(WP), allocatable :: bd1z_C2C(:, :, :)
  real(WP), allocatable :: cd1z_C2C(:, :, :)
  real(WP), allocatable :: dd1z_C2C(:, :, :)

  real(WP), allocatable :: ad1z_P2C(:, :, :)
  real(WP), allocatable :: bd1z_P2C(:, :, :)
  real(WP), allocatable :: cd1z_P2C(:, :, :)
  real(WP), allocatable :: dd1z_P2C(:, :, :)

  real(WP), allocatable :: ad1z_C2P(:, :, :)
  real(WP), allocatable :: bd1z_C2P(:, :, :)
  real(WP), allocatable :: cd1z_C2P(:, :, :)
  real(WP), allocatable :: dd1z_C2P(:, :, :)
!----------------------------------------------------------------------------------------------------------
! z : pre-processed TDMA LHS Matrix for 2nd deriviative
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: ad2z_P2P(:, :, :)
  real(WP), allocatable :: bd2z_P2P(:, :, :)
  real(WP), allocatable :: cd2z_P2P(:, :, :)
  real(WP), allocatable :: dd2z_P2P(:, :, :)

  real(WP), allocatable :: ad2z_C2C(:, :, :)
  real(WP), allocatable :: bd2z_C2C(:, :, :)
  real(WP), allocatable :: cd2z_C2C(:, :, :)
  real(WP), allocatable :: dd2z_C2C(:, :, :)
!----------------------------------------------------------------------------------------------------------
! z : pre-processed TDMA LHS Matrix for mid-point interpolation
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: am1z_P2C(:, :, :)
  real(WP), allocatable :: bm1z_P2C(:, :, :)
  real(WP), allocatable :: cm1z_P2C(:, :, :)
  real(WP), allocatable :: dm1z_P2C(:, :, :)

  real(WP), allocatable :: am1z_C2P(:, :, :)
  real(WP), allocatable :: bm1z_C2P(:, :, :)
  real(WP), allocatable :: cm1z_C2P(:, :, :)
  real(WP), allocatable :: dm1z_C2P(:, :, :)
  
!----------------------------------------------------------------------------------------------------------
! processures
!----------------------------------------------------------------------------------------------------------
  private :: Prepare_compact_coefficients
  private :: Buildup_TDMA_LHS_array
  public  :: Prepare_LHS_coeffs_for_operations

  private :: Prepare_TDMA_interp_P2C_RHS_array ! need fbc(1,2) for IBC_INTERIOR
  private :: Get_x_midp_P2C_1D
  private :: Get_y_midp_P2C_1D
  private :: Get_z_midp_P2C_1D
  public  :: Get_x_midp_P2C_3D 
  public  :: Get_y_midp_P2C_3D 
  public  :: Get_z_midp_P2C_3D 

  private :: Prepare_TDMA_interp_C2P_RHS_array ! need fbc(1,2,3,4) for Dirichlet, INTERIOR
  private :: Get_x_midp_C2P_1D
  private :: Get_y_midp_C2P_1D
  private :: Get_z_midp_C2P_1D
  public  :: Get_x_midp_C2P_3D
  public  :: Get_y_midp_C2P_3D
  public  :: Get_z_midp_C2P_3D

  private :: Prepare_TDMA_1deri_C2C_RHS_array ! need fbc(1,2,3,4) for INTERIOR
  private :: Get_x_1st_derivative_C2C_1D
  private :: Get_y_1st_derivative_C2C_1D
  private :: Get_z_1st_derivative_C2C_1D
  public  :: Get_x_1st_derivative_C2C_3D ! only used for thermal flow
  public  :: Get_y_1st_derivative_C2C_3D ! only used for thermal flow
  public  :: Get_z_1st_derivative_C2C_3D ! only used for thermal flow

  private :: Prepare_TDMA_1deri_P2P_RHS_array ! need fbc(1,2,3,4) for Neumann, INTERIOR
  private :: Get_x_1st_derivative_P2P_1D
  private :: Get_y_1st_derivative_P2P_1D
  private :: Get_z_1st_derivative_P2P_1D
  public  :: Get_x_1st_derivative_P2P_3D
  public  :: Get_y_1st_derivative_P2P_3D
  public  :: Get_z_1st_derivative_P2P_3D

  private :: Prepare_TDMA_1deri_C2P_RHS_array ! need fbc(1,2,3,4) for Neumann, INTERIOR
  private :: Get_x_1st_derivative_C2P_1D
  private :: Get_y_1st_derivative_C2P_1D
  private :: Get_z_1st_derivative_C2P_1D
  public  :: Get_x_1st_derivative_C2P_3D ! careful about Dirichlet BC, check 
  public  :: Get_y_1st_derivative_C2P_3D ! careful about Dirichlet BC, check 
  public  :: Get_z_1st_derivative_C2P_3D ! careful about Dirichlet BC, check 

  private :: Prepare_TDMA_1deri_P2C_RHS_array ! need fbc(1,2) for INTERIOR
  private :: Get_x_1st_derivative_P2C_1D
  private :: Get_y_1st_derivative_P2C_1D
  private :: Get_z_1st_derivative_P2C_1D
  public  :: Get_x_1st_derivative_P2C_3D
  public  :: Get_y_1st_derivative_P2C_3D
  public  :: Get_z_1st_derivative_P2C_3D

  private :: Prepare_TDMA_2deri_C2C_RHS_array ! need fbc to INTERIOR
  private :: Get_x_2nd_derivative_C2C_1D
  private :: Get_y_2nd_derivative_C2C_1D
  private :: Get_z_2nd_derivative_C2C_1D
  public  :: Get_x_2nd_derivative_C2C_3D ! not used.
  public  :: Get_y_2nd_derivative_C2C_3D ! not used.
  public  :: Get_z_2nd_derivative_C2C_3D ! not used.

  private :: Prepare_TDMA_2deri_P2P_RHS_array ! need fbc for Neumann, interior
  private :: Get_x_2nd_derivative_P2P_1D
  private :: Get_y_2nd_derivative_P2P_1D
  private :: Get_z_2nd_derivative_P2P_1D
  public  :: Get_x_2nd_derivative_P2P_3D ! not used.
  public  :: Get_y_2nd_derivative_P2P_3D ! not used.
  public  :: Get_z_2nd_derivative_P2P_3D ! not used.

  public  :: Test_interpolation
  public  :: Test_1st_derivative
  public  :: Test_2nd_derivative

contains
!==========================================================================================================
!> \brief Assigned the cooefficients for the compact schemes     
!> Scope:  mpi    called-freq    xdomain     module
!>         all    once           specified   private
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           !
!----------------------------------------------------------------------------------------------------------
!> \param[in]     iaccu         the accuracy given by user
!==========================================================================================================
  subroutine Prepare_compact_coefficients(iaccu)
    use parameters_constant_mod
    use input_general_mod
    use mpi_mod
    implicit none

    integer, intent(in) :: iaccu

    real(WP) :: alpha,  a,  b,  c,  d
    real(WP) :: alpha1, a1, b1, c1, d1
    real(WP) :: alpha2, a2, b2, c2, d2
    real(WP) :: alpha_itf, a_itf, b_itf, c_itf, d_itf ! for interface/interior/reduced to 4th CD

    integer :: n

    if(nrank == 0) then
       call Print_debug_start_msg &
         ("Assigning coefficient matrix for the compact schemes ...")
       !write(*, *) "The given numerical accuracy =", iaccu
    end if

!----------------------------------------------------------------------------------------------------------
!   initialisation
!----------------------------------------------------------------------------------------------------------
    d1fC2C(:, :, :) = ZERO
    d1rC2C(:, :, :) = ZERO
    d1fP2P(:, :, :) = ZERO
    d1rP2P(:, :, :) = ZERO

    d1fC2P(:, :, :) = ZERO
    d1rC2P(:, :, :) = ZERO
    d1fP2C(:, :, :) = ZERO
    d1rP2C(:, :, :) = ZERO

    d2fC2C(:, :, :) = ZERO
    d2rC2C(:, :, :) = ZERO
    d2fP2P(:, :, :) = ZERO
    d2rP2P(:, :, :) = ZERO

    m1fC2P(:, :, :) = ZERO
    m1rC2P(:, :, :) = ZERO
    m1fP2C(:, :, :) = ZERO
    m1rP2C(:, :, :) = ZERO
!==========================================================================================================
! Set 1 : P2P, C2P, periodic & symmetric & asymmetric
!         1st derivative on collocated grids, C2C/P2P bulk coefficients
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
! to solve below matrix:
! eq(1) : a +   b = 2  alpha + 1    O(h2)
! eq(2) : a +  4b = 6  alpha        O(h4)
! eq(3) : a + 16b = 10 alpha        O(h6)
!==========================================================================================================
    alpha = ZERO
        a = ZERO
        b = ZERO
        c = ZERO
alpha_itf = ZERO
    a_itf = FOUR * ONE_THIRD
    b_itf = - ONE_THIRD
    c_itf = ZERO

    if (iaccu == IACCU_CD2) then      ! eq(1) + alpha0 + b0, O(h2)
      alpha = ZERO
          a = ONE
          b = ZERO
    else if (iaccu == IACCU_CD4) then ! eq(1-2) + alpha0, O(h4)
      alpha = ZERO
          a = FOUR * ONE_THIRD
          b = - ONE_THIRD
    else if (iaccu == IACCU_CP4) then ! eq(1-2) + b0, O(h4)
      alpha = QUARTER
          a = ONEPFIVE
          b = ZERO
    else if (iaccu == IACCU_CP6) then ! eq(1-2-3), O(h6)
      alpha = ONE_THIRD
          a = FOURTEEN / NINE
          b = ONE / NINE
    else ! default 2nd CD
      alpha = ZERO
          a = ONE
          b = ZERO
    end if
!----------------------------------------------------------------------------------------------------------
! 1st-derivative :
! C2C : periodic b.c.
! d1fC2C : "d1"=first deriviative, "f"=f'  side, "C2C"= center 2 centre 
! d1rC2C : "d1"=first deriviative, "r"=rhs side, "C2C"= center 2 centre 
! [ 1    alpha                   alpha][f'_1]=[a/2 * (f_{2}   - f_{n})/h   + b/4 * (f_{3}   - f_{n-1})/h]
! [      alpha 1     alpha            ][f'_2] [a/2 * (f_{3}   - f_{1})/h   + b/4 * (f_{4}   - f_{n})/h  ]
! [            alpha 1     alpha      ][f'_i] [a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                  alpha 1     alpha][f'_4] [a/2 * (f_{n}   - f_{n-2})/h + b/4 * (f_{1}   - f_{n-3})/h]
! [alpha                   alpha 1    ][f'_5] [a/2 * (f_{1}   - f_{n-1})/h + b/4 * (f_{2}   - f_{n-2})/h]
!----------------------------------------------------------------------------------------------------------
    d1fC2C(1:5, 1, IBC_PERIODIC) = alpha
    d1fC2C(1:5, 2, IBC_PERIODIC) = ONE
    d1fC2C(1:5, 3, IBC_PERIODIC) = alpha

    d1rC2C(1:5, 1, IBC_PERIODIC) = a * HALF    ! a/2
    d1rC2C(1:5, 2, IBC_PERIODIC) = b * QUARTER ! b/4
    d1rC2C(1:5, 3, IBC_PERIODIC) = c           ! not used
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, interior
!----------------------------------------------------------------------------------------------------------
    d1fC2C(1, 1, IBC_INTERIOR) = alpha_itf
    d1fC2C(1, 2, IBC_INTERIOR) = ONE
    d1fC2C(1, 3, IBC_INTERIOR) = alpha_itf
    d1rC2C(1, 1, IBC_INTERIOR) = a_itf * HALF  ! a/2
    d1rC2C(1, 2, IBC_INTERIOR) = b_itf * QUARTER ! b/4
    d1rC2C(1, 3, IBC_INTERIOR) = c_itf        ! not used

    d1fC2C(5,   :, IBC_INTERIOR) = d1fC2C(1,   :, IBC_INTERIOR)
    d1rC2C(5,   :, IBC_INTERIOR) = d1rC2C(1,   :, IBC_INTERIOR)
    d1fC2C(2:4, :, IBC_INTERIOR) = d1fC2C(2:4, :, IBC_PERIODIC)
    d1rC2C(2:4, :, IBC_INTERIOR) = d1rC2C(2:4, :, IBC_PERIODIC)

!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2P : periodic b.c.  Same as C2C
!----------------------------------------------------------------------------------------------------------
    d1fP2P(:, :, IBC_PERIODIC) = d1fC2C(:, :, IBC_PERIODIC)
    d1rP2P(:, :, IBC_PERIODIC) = d1rC2C(:, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, interior
!----------------------------------------------------------------------------------------------------------    
    d1fP2P(:, :, IBC_INTERIOR) = d1fC2C(:, :, IBC_INTERIOR)
    d1rP2P(:, :, IBC_INTERIOR) = d1rC2C(:, :, IBC_INTERIOR)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2C : symmetric b.c.
! [ 1-alpha  alpha                          ][f'_1]=[a/2 * (f_{2}   - f_{1})/h   + b/4 * (f_{3}   - f_{2})/h  ]
! [          alpha 1     alpha              ][f'_2] [a/2 * (f_{3}   - f_{1})/h   + b/4 * (f_{4}   - f_{1})/h  ]
! [                alpha 1     alpha        ][f'_i] [a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                      alpha 1     alpha  ][f'_4] [a/2 * (f_{n}   - f_{n-2})/h + b/4 * (f_{n}   - f_{n-3})/h]
! [                            alpha 1-alpha][f'_5] [a/2 * (f_{n}   - f_{n-1})/h + b/4 * (f_{n-1} - f_{n-2})/h]
!----------------------------------------------------------------------------------------------------------
    d1fC2C(1,   1, IBC_SYMMETRIC) = ZERO        ! not used
    d1fC2C(1,   2, IBC_SYMMETRIC) = ONE - alpha
    d1fC2C(1,   3, IBC_SYMMETRIC) = alpha
    d1fC2C(5,   1, IBC_SYMMETRIC) = d1fC2C(1,   3, IBC_SYMMETRIC)
    d1fC2C(5,   2, IBC_SYMMETRIC) = d1fC2C(1,   2, IBC_SYMMETRIC)
    d1fC2C(5,   3, IBC_SYMMETRIC) = d1fC2C(1,   1, IBC_SYMMETRIC)

    d1fC2C(2:4, :, IBC_SYMMETRIC) = d1fC2C(2:4, :, IBC_PERIODIC )

    d1rC2C(:,   :, IBC_SYMMETRIC) = d1rC2C(:,   :, IBC_PERIODIC )
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2C : asymmetric b.c.
! [ 1+alpha  alpha                          ][f'_1]=[a/2 * (f_{2}   + f_{1})/h   + b/4 * (f_{3}   + f_{2})/h  ]
! [          alpha 1     alpha              ][f'_2] [a/2 * (f_{3}   - f_{1})/h   + b/4 * (f_{4}   + f_{1})/h  ]
! [                alpha 1     alpha        ][f'_i] [a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                      alpha 1     alpha  ][f'_4] [a/2 * (f_{n}   - f_{n-2})/h + b/4 * (-f_{n}   - f_{n-3})/h]
! [                            alpha 1+alpha][f'_5] [a/2 * (-f_{n}   - f_{n-1})/h + b/4 * (-f_{n-1} - f_{n-2})/h]
!----------------------------------------------------------------------------------------------------------
    d1fC2C(1,   1, IBC_ASYMMETRIC) = ZERO        ! not used
    d1fC2C(1,   2, IBC_ASYMMETRIC) = ONE + alpha
    d1fC2C(1,   3, IBC_ASYMMETRIC) = alpha

    d1fC2C(5,   1, IBC_ASYMMETRIC) = d1fC2C(1,   3, IBC_ASYMMETRIC)
    d1fC2C(5,   2, IBC_ASYMMETRIC) = d1fC2C(1,   2, IBC_ASYMMETRIC)
    d1fC2C(5,   3, IBC_ASYMMETRIC) = d1fC2C(1,   1, IBC_ASYMMETRIC)

    d1fC2C(2:4, :, IBC_ASYMMETRIC) = d1fC2C(2:4, :, IBC_PERIODIC  )

    d1rC2C(:,   :, IBC_ASYMMETRIC) = d1rC2C(:,   :, IBC_PERIODIC  )
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2P : symmetric b.c.
! [ 1  0                              ][f'_{1'}]=[a/2 * (f_{2'}   - f_{2'})/h   + b/4 * (f_{3'}   - f_{3'})/h  ]
! [    alpha 1     alpha              ][f'_{2'}] [a/2 * (f_{3'}   - f_{1'})/h   + b/4 * (f_{4'}   - f_{2'})/h  ]
! [          alpha 1     alpha        ][f'_{i'}] [a/2 * (f_{i'+1} - f_{i'-1})/h + b/4 * (f_{i'+2} - f_{i'-2})/h]
! [                alpha 1     alpha  ][f'_{4'}] [a/2 * (f_{n'}   - f_{n'-2})/h + b/4 * (f_{n'-1} - f_{n'-3})/h]
! [                      0     1      ][f'_{5'}] [a/2 * (f_{n'-1} - f_{n'-1})/h + b/4 * (f_{n'-2} - f_{n'-2})/h]
!----------------------------------------------------------------------------------------------------------
    d1fP2P(1,   1, IBC_SYMMETRIC) = ZERO ! not used
    d1fP2P(1,   2, IBC_SYMMETRIC) = ONE
    d1fP2P(1,   3, IBC_SYMMETRIC) = ZERO

    d1fP2P(5,   1, IBC_SYMMETRIC) = d1fP2P(1,   3, IBC_SYMMETRIC)
    d1fP2P(5,   2, IBC_SYMMETRIC) = d1fP2P(1,   2, IBC_SYMMETRIC)
    d1fP2P(5,   3, IBC_SYMMETRIC) = d1fP2P(1,   1, IBC_SYMMETRIC)

    d1fP2P(2:4, :, IBC_SYMMETRIC) = d1fP2P(2:4, :, IBC_PERIODIC )

    d1rP2P(:,   :, IBC_SYMMETRIC) = d1rP2P(:,   :, IBC_PERIODIC )
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2P : asymmetric b.c.
! [ 1  2alpha                          ][f'_{1'}]=[a/2 * (f_{2'}   + f_{2'})/h   + b/4 * (f_{3'}   + f_{3'})/h  ]
! [    alpha 1     alpha               ][f'_{2'}] [a/2 * (f_{3'}   - f_{1'})/h   + b/4 * (f_{4'}   + f_{2'})/h  ]
! [          alpha 1     alpha         ][f'_{i'}] [a/2 * (f_{i'+1} - f_{i'-1})/h + b/4 * (f_{i'+2} - f_{i'-2})/h]
! [                alpha 1      alpha  ][f'_{4'}] [a/2 * (f_{n'}   - f_{n'-2})/h + b/4 * (-f_{n'-1} - f_{n'-3})/h]
! [                      2alpha 1      ][f'_{5'}] [a/2 * (-f_{n'-1} - f_{n'-1})/h + b/4 * (-f_{n'-2} - f_{n'-2})/h]
!----------------------------------------------------------------------------------------------------------
    d1fP2P(1,   1, IBC_ASYMMETRIC) = ZERO ! not used
    d1fP2P(1,   2, IBC_ASYMMETRIC) = ONE
    d1fP2P(1,   3, IBC_ASYMMETRIC) = TWO * alpha

    d1fP2P(5,   1, IBC_ASYMMETRIC) = d1fP2P(1,   3, IBC_ASYMMETRIC)
    d1fP2P(5,   2, IBC_ASYMMETRIC) = d1fP2P(1,   2, IBC_ASYMMETRIC)
    d1fP2P(5,   3, IBC_ASYMMETRIC) = d1fP2P(1,   1, IBC_ASYMMETRIC)

    d1fP2P(2:4, :, IBC_ASYMMETRIC) = d1fP2P(2:4, :, IBC_PERIODIC)

    d1rP2P(:,   :, IBC_ASYMMETRIC) = d1rP2P(:,   :, IBC_PERIODIC)
!==========================================================================================================
! Set 2: no bc required
!       C2C : no specified, Neumann
!       P2P : no specified, Dirichlet B.C.
! 1st derivative on collocated grids, C2C/P2P coefficients : Dirichlet B.C.
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
! it is:
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1}/h  + b1 * f_{2}/h + c1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2/2 * (f_{3} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i] [ a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4] [a2/2 * (f_{n} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n}/h  - b1 * f_{n-1}/h - c1 * f_{n-2}/h]
! First layer BC:
! eq(1): a + b +  c =  0               O(h0)
! eq(2):     b + 2c =  alpha + 1       O(h1)
! eq(3):     b + 4c = 2alpha           O(h2)
! eq(4):     b + 8c = 3alpha           O(h3)
!==========================================================================================================
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2) then ! eq(1-2)+alpha0+c0, O(h1)
      alpha1 = ZERO
          a1 = - ONE
          b1 = ONE 
          c1 = ZERO
          ! 2nd layer = same as bulk with CD2
      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
    else if (iaccu == IACCU_CD4) then ! eq(1-3)+alpha0, O(h2)
      alpha1 = ZERO
          a1 = -ONEPFIVE
          b1 = TWO
          c1 = -HALF
          ! 2nd layer = same as bulk with CD2
      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
    else if (iaccu == IACCU_CP4) then ! eq(1-3)+c0, O(h2)
      alpha1 = ONE
          a1 = - TWO
          b1 = TWO
          c1 = ZERO
          ! 2nd layer = same as bulk with CP4
      alpha2 = QUARTER
          a2 = ONEPFIVE
          b2 = ZERO
    else if (iaccu == IACCU_CP6) then ! eq(1-4), O(h3)
      alpha1 = TWO
          a1 = -TWOPFIVE
          b1 = TWO
          c1 = HALF
          ! 2nd layer = same as bulk with CP4
      alpha2 = QUARTER
          a2 = ONEPFIVE
          b2 = ZERO
    else ! default 2nd CD
      alpha1 = ZERO
          a1 = - ONE
          b1 = ONE 
          c1 = ZERO
          ! 2nd layer = same as bulk with CD2
      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
    end if
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2C : no specified, Neumann
! P2P : no specified, Dirichlet B.C.
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1}/h  + b1 * f_{2}/h + c1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2/2 * (f_{3} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i] [ a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4] [a2/2 * (f_{n} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n}/h  - b1 * f_{n-1}/h - c1 * f_{n-2}/h]
!----------------------------------------------------------------------------------------------------------
    d1fC2C(1, 1,   IBC_INTRPL) = ZERO ! not used
    d1fC2C(1, 2,   IBC_INTRPL) = ONE
    d1fC2C(1, 3,   IBC_INTRPL) = alpha1
    d1rC2C(1, 1,   IBC_INTRPL) = a1
    d1rC2C(1, 2,   IBC_INTRPL) = b1
    d1rC2C(1, 3,   IBC_INTRPL) = c1

    d1fC2C(5, 1,   IBC_INTRPL) =   d1fC2C(1, 3, IBC_INTRPL)
    d1fC2C(5, 2,   IBC_INTRPL) =   d1fC2C(1, 2, IBC_INTRPL)
    d1fC2C(5, 3,   IBC_INTRPL) =   d1fC2C(1, 1, IBC_INTRPL)
    d1rC2C(5, 1,   IBC_INTRPL) = - d1rC2C(1, 1, IBC_INTRPL)
    d1rC2C(5, 2,   IBC_INTRPL) = - d1rC2C(1, 2, IBC_INTRPL)
    d1rC2C(5, 3,   IBC_INTRPL) = - d1rC2C(1, 3, IBC_INTRPL)
  
    d1fC2C(2, 1,   IBC_INTRPL) = alpha2
    d1fC2C(2, 2,   IBC_INTRPL) = ONE
    d1fC2C(2, 3,   IBC_INTRPL) = alpha2
    d1rC2C(2, 1,   IBC_INTRPL) = a2 * HALF
    d1rC2C(2, 2,   IBC_INTRPL) = b2 * QUARTER ! zero
    d1rC2C(2, 3,   IBC_INTRPL) = c2  ! not used  

    d1fC2C(4, 1,   IBC_INTRPL) = d1fC2C(2, 3, IBC_INTRPL)
    d1fC2C(4, 2,   IBC_INTRPL) = d1fC2C(2, 2, IBC_INTRPL)
    d1fC2C(4, 3,   IBC_INTRPL) = d1fC2C(2, 1, IBC_INTRPL)
    d1rC2C(4, 1,   IBC_INTRPL) = d1rC2C(2, 1, IBC_INTRPL)
    d1rC2C(4, 2,   IBC_INTRPL) = d1rC2C(2, 2, IBC_INTRPL)
    d1rC2C(4, 3,   IBC_INTRPL) = d1rC2C(2, 3, IBC_INTRPL)

    d1fC2C(3, 1:3, IBC_INTRPL) = d1fC2C(3, 1:3, IBC_PERIODIC)
    d1rC2C(3, 1:3, IBC_INTRPL) = d1rC2C(3, 1:3, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2P : no specified
!----------------------------------------------------------------------------------------------------------
    d1fP2P(:, :, IBC_INTRPL) = d1fC2C(:, :, IBC_INTRPL)
    d1rP2P(:, :, IBC_INTRPL) = d1rC2C(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2C : neumann
!----------------------------------------------------------------------------------------------------------
    d1fC2C(:, :, IBC_NEUMANN) = d1fC2C(:, :, IBC_INTRPL)
    d1rC2C(:, :, IBC_NEUMANN) = d1rC2C(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2P : Dirichlet
!----------------------------------------------------------------------------------------------------------
    d1fP2P(:, :, IBC_DIRICHLET) = d1fP2P(:, :, IBC_INTRPL)
    d1rP2P(:, :, IBC_DIRICHLET) = d1rP2P(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2P : NEUMANN
! [ 1     0                                 ][f'_1]=[known]
! [alpha2 1      alpha2                     ][f'_2] [a2/2 * (f_{3} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i] [ a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4] [a2/2 * (f_{n} - f_{n-2})/h]
! [                            0      1     ][f'_5] [known]
!----------------------------------------------------------------------------------------------------------
    d1fP2P(1, 1,   IBC_NEUMANN) = ZERO ! not used
    d1fP2P(1, 2,   IBC_NEUMANN) = ONE
    d1fP2P(1, 3,   IBC_NEUMANN) = ZERO
    d1rP2P(1, 1,   IBC_NEUMANN) = ZERO
    d1rP2P(1, 2,   IBC_NEUMANN) = ZERO
    d1rP2P(1, 3,   IBC_NEUMANN) = ZERO

    d1fP2P(5, 1,   IBC_NEUMANN) =   d1fP2P(1, 3, IBC_NEUMANN)
    d1fP2P(5, 2,   IBC_NEUMANN) =   d1fP2P(1, 2, IBC_NEUMANN)
    d1fP2P(5, 3,   IBC_NEUMANN) =   d1fP2P(1, 1, IBC_NEUMANN)
    d1rP2P(5, 1,   IBC_NEUMANN) = - d1rP2P(1, 1, IBC_NEUMANN)
    d1rP2P(5, 2,   IBC_NEUMANN) = - d1rP2P(1, 2, IBC_NEUMANN)
    d1rP2P(5, 3,   IBC_NEUMANN) = - d1rP2P(1, 3, IBC_NEUMANN)

    d1fP2P(2, 1,   IBC_NEUMANN) = alpha2
    d1fP2P(2, 2,   IBC_NEUMANN) = ONE
    d1fP2P(2, 3,   IBC_NEUMANN) = alpha2
    d1rP2P(2, 1,   IBC_NEUMANN) = a2 * HALF
    d1rP2P(2, 2,   IBC_NEUMANN) = b2 * QUARTER ! not used
    d1rP2P(2, 3,   IBC_NEUMANN) = c2    

    d1fP2P(4, 1,   IBC_NEUMANN) = d1fP2P(2, 3, IBC_NEUMANN)
    d1fP2P(4, 2,   IBC_NEUMANN) = d1fP2P(2, 2, IBC_NEUMANN)
    d1fP2P(4, 3,   IBC_NEUMANN) = d1fP2P(2, 1, IBC_NEUMANN)
    d1rP2P(4, 1,   IBC_NEUMANN) = d1rP2P(2, 1, IBC_NEUMANN)
    d1rP2P(4, 2,   IBC_NEUMANN) = d1rP2P(2, 2, IBC_NEUMANN)
    d1rP2P(4, 3,   IBC_NEUMANN) = d1rP2P(2, 3, IBC_NEUMANN)

    d1fP2P(3, 1:3, IBC_NEUMANN) = d1fP2P(3, 1:3, IBC_PERIODIC)
    d1rP2P(3, 1:3, IBC_NEUMANN) = d1rP2P(3, 1:3, IBC_PERIODIC)
!==========================================================================================================
! Set 3: Dirichlet for C2C (unique), check is involved bc value necessary? !!!
!       influence all 1st_deri_C2C with Dirichlet BC.
! 1st derivative on collocated grids, C2C/P2P coefficients : Dirichlet B.C.
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
! f'{1} + alpha f'{2} = 1/h (a * f{1'} + b * f{1} + c * f{2} + d * f{3})
! up to 4th order, to solve below equations:
!    a +   b + c   +  d = 0
!   -a +  2c +       4d = 2alpha + 2    !O(h1)
!    a +  4c +      16d = 8alpha        !O(h2)
!   -a +  8c +      64d = 24alpha       !O(h3)
!    a + 16c +     256d = 64alpha       !O(h4)
!==========================================================================================================
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
        d1 = ZERO
    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2) then ! degrade to 2nd CD (1st cell), 2nd CD (2nd cell)
      alpha1 = ZERO
          a1 = ZERO
          b1 = -ONE
          c1 = ONE
          d1 = ZERO
      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
    else if (iaccu == IACCU_CD4) then ! degrade to 3rd CD (1st cell), 2nd CD (2nd cell), chech stencil
     ! method 1 = with bc value, a/=0
      ! alpha1 = ZERO
      !     a1 = - SIXTEEN / FIFTEEN
      !     b1 = ONE / TWO
      !     c1 = TWO_THIRD
      !     d1 = - ONE / TEN
      ! method 2 = without bc value, a=0
      alpha1 = ZERO
          a1 = ZERO
          b1 = -THREE / TWO
          c1 = TWO
          d1 = -HALF

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
    else if (iaccu == IACCU_CP4) then ! degrade to 3rd CP (1st cell), 4th CP (2nd cell)
      ! method 1 = with bc value, a/=0
      ! alpha1 = TWO_THIRD
      !     a1 = - THIRTYTWO / FOURTYFIVE
      !     b1 = - ONE / TWO
      !     c1 = TEN / NINE
      !     d1 = ONE * ZPONE
      ! method 2 = without bc value, a=0
      alpha1 = ONE
          a1 = ZERO
          b1 = - TWO
          c1 = TWO
          d1 = ZERO
      alpha2 = QUARTER
          a2 = ONEPFIVE
          b2 = ZERO
    else if (iaccu == IACCU_CP6) then ! degrade to 4th CP (1st cell), 4th CP (2nd cell)
      ! method 1 = with bc value, a/=0
      ! alpha1 = TWO_THIRD
      !     a1 = - THIRTYTWO / FOURTYFIVE
      !     b1 = - ONE / TWO
      !     c1 = TEN / NINE
      !     d1 = ONE * ZPONE
      ! method 2 = without bc value, a=0, same as INTPL
      alpha1 = TWO
          a1 = ZERO
          b1 = - TWOPFIVE
          c1 = TWO
          d1 = HALF

      alpha2 = QUARTER
          a2 = ONEPFIVE
          b2 = ZERO
    else ! default 2nd CD
      alpha1 = ZERO
          a1 = ZERO
          b1 = -ONE
          c1 = ONE
          d1 = ZERO
      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
    end if
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2C : Dirchlet
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1'}/h  + b1 * f_{1}/h + c1 * f_{2}/h + d1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2/2 * (f_{3} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i] [ a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4] [a2/2 * (f_{n} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n'+1}/h -b1 * f_{n}/h  - c1 * f_{n-1}/h - d1 * f_{n-2}/h]
!----------------------------------------------------------------------------------------------------------
    d1fC2C(1, 1,   IBC_DIRICHLET) = ZERO ! not used
    d1fC2C(1, 2,   IBC_DIRICHLET) = ONE
    d1fC2C(1, 3,   IBC_DIRICHLET) = alpha1
    d1rC2C(1, 1,   IBC_DIRICHLET) = a1
    d1rC2C(1, 2,   IBC_DIRICHLET) = b1
    d1rC2C(1, 3,   IBC_DIRICHLET) = c1
    d1rC2C(1, 4,   IBC_DIRICHLET) = d1

    d1fC2C(5, 1,   IBC_DIRICHLET) =   d1fC2C(1, 3, IBC_DIRICHLET)
    d1fC2C(5, 2,   IBC_DIRICHLET) =   d1fC2C(1, 2, IBC_DIRICHLET)
    d1fC2C(5, 3,   IBC_DIRICHLET) =   d1fC2C(1, 1, IBC_DIRICHLET)
    d1rC2C(5, 1,   IBC_DIRICHLET) = - d1rC2C(1, 1, IBC_DIRICHLET)
    d1rC2C(5, 2,   IBC_DIRICHLET) = - d1rC2C(1, 2, IBC_DIRICHLET)
    d1rC2C(5, 3,   IBC_DIRICHLET) = - d1rC2C(1, 3, IBC_DIRICHLET)
    d1rC2C(5, 4,   IBC_DIRICHLET) = - d1rC2C(1, 4, IBC_DIRICHLET)

    d1fC2C(2, 1,   IBC_DIRICHLET) = alpha2
    d1fC2C(2, 2,   IBC_DIRICHLET) = ONE
    d1fC2C(2, 3,   IBC_DIRICHLET) = alpha2
    d1rC2C(2, 1,   IBC_DIRICHLET) = a2 * HALF
    d1rC2C(2, 2,   IBC_DIRICHLET) = b2 * QUARTER ! not used
    d1rC2C(2, 3,   IBC_DIRICHLET) = c2    

    d1fC2C(4, 1,   IBC_DIRICHLET) = d1fC2C(2, 3, IBC_DIRICHLET)
    d1fC2C(4, 2,   IBC_DIRICHLET) = d1fC2C(2, 2, IBC_DIRICHLET)
    d1fC2C(4, 3,   IBC_DIRICHLET) = d1fC2C(2, 1, IBC_DIRICHLET)
    d1rC2C(4, 1,   IBC_DIRICHLET) = d1rC2C(2, 1, IBC_DIRICHLET)
    d1rC2C(4, 2,   IBC_DIRICHLET) = d1rC2C(2, 2, IBC_DIRICHLET)
    d1rC2C(4, 3,   IBC_DIRICHLET) = d1rC2C(2, 3, IBC_DIRICHLET)

    d1fC2C(3, 1:3, IBC_DIRICHLET) = d1fC2C(3, 1:3, IBC_PERIODIC)
    d1rC2C(3, 1:3, IBC_DIRICHLET) = d1rC2C(3, 1:3, IBC_PERIODIC)
!==========================================================================================================
! 1st derivative on staggered grids P2C and C2P : Periodic or Symmetric B.C.
! P2C ==>
! alpha * f'_{i-1} +  f'_i +  alpha * f'_{i+1}  = a/(h ) * ( f_{i'+1} - f_{i'} ) + &
!                                                 b/(3h) * ( f_{i'+2} - f_{i'-1} )
! C2P ==>
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(h ) * ( f_{i}   - f_{i-1} ) + &
!                                                 b/(3h) * ( f_{i+1} - f_{i-2} )
! eq(1) : a +   b = 2   alpha + 1  !O(h2)
! eq(2) : a +  9b = 24  alpha      !O(h4)
! eq(3) : a + 81b = 160 alpha      !O(h6)
!==========================================================================================================
    alpha = ZERO
        a = ZERO
        b = ZERO
        c = ZERO
        ! interface is a default CD4
alpha_itf = ZERO
    a_itf = NINE * EIGHTH
    b_itf = -ONE * EIGHTH
    c_itf = ZERO

    if (iaccu == IACCU_CD2) then
      alpha = ZERO
          a = ONE
          b = ZERO
    else if (iaccu == IACCU_CD4) then
      alpha = ZERO
          a = NINE * EIGHTH
          b = -ONE * EIGHTH
    else if (iaccu == IACCU_CP4) then
      alpha = ONE / TWENTYTWO
          a = TWELVE / ELEVEN
          b = ZERO
    else if (iaccu == IACCU_CP6) then
      alpha = NINE / SIXTYTWO
          a = SIXTYTHREE / SIXTYTWO
          b = SEVENTEEN / SIXTYTWO
    else  ! default 2nd CD
      alpha = ZERO
          a = ONE
          b = ZERO
    end if
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2P, P2C: periodic b.c. : staggered 
! [ 1    alpha                   alpha][f'_1']=[a * (f_{1}   - f_{n})/h   + b/3 * (f_{2}   - f_{n-1})/h]
! [      alpha 1     alpha            ][f'_2'] [a * (f_{1}   - f_{1})/h   + b/3 * (f_{3}   - f_{n})/h  ]
! [            alpha 1     alpha      ][f'_i'] [a * (f_{i}   - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h]
! [                  alpha 1     alpha][f'_4'] [a * (f_{n-1} - f_{n-2})/h + b/3 * (f_{n}   - f_{n-3})/h]
! [alpha                   alpha 1    ][f'_5'] [a * (f_{n}   - f_{n-1})/h + b/3 * (f_{1}   - f_{n-2})/h]
!----------------------------------------------------------------------------------------------------------
    d1fC2P(1:5, 1, IBC_PERIODIC) = alpha
    d1fC2P(1:5, 2, IBC_PERIODIC) = ONE
    d1fC2P(1:5, 3, IBC_PERIODIC) = alpha
    d1rC2P(1:5, 1, IBC_PERIODIC) = a         ! a
    d1rC2P(1:5, 2, IBC_PERIODIC) = b * ONE_THIRD ! b/3
    d1rC2P(1:5, 3, IBC_PERIODIC) = c         ! not used
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2P : interior
!----------------------------------------------------------------------------------------------------------
    d1fC2P(1, 1, IBC_INTERIOR) = alpha_itf
    d1fC2P(1, 2, IBC_INTERIOR) = ONE
    d1fC2P(1, 3, IBC_INTERIOR) = alpha_itf
    d1rC2P(1, 1, IBC_INTERIOR) = a_itf
    d1rC2P(1, 2, IBC_INTERIOR) = b_itf * ONE_THIRD ! b/3
    d1rC2P(1, 3, IBC_INTERIOR) = c_itf        ! not used

    d1fC2P(5,   :, IBC_INTERIOR) = d1fC2P(1,   :, IBC_INTERIOR)
    d1rC2P(5,   :, IBC_INTERIOR) = d1rC2P(1,   :, IBC_INTERIOR)
    d1fC2P(2:4, :, IBC_INTERIOR) = d1fC2P(2:4, :, IBC_PERIODIC)
    d1rC2P(2:4, :, IBC_INTERIOR) = d1rC2P(2:4, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2C : periodic b.c.  Same as C2P
!----------------------------------------------------------------------------------------------------------
    d1fP2C(:, :, IBC_PERIODIC) = d1fC2P(:, :, IBC_PERIODIC)
    d1rP2C(:, :, IBC_PERIODIC) = d1rC2P(:, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2C : interior.  Same as C2P
!----------------------------------------------------------------------------------------------------------
    d1fP2C(:, :, IBC_INTERIOR) = d1fC2P(:, :, IBC_INTERIOR)
    d1rP2C(:, :, IBC_INTERIOR) = d1rC2P(:, :, IBC_INTERIOR)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2P : symmetric b.c.
! [ 1     0                        ][f'_1]=[a * (f_{1}   - f_{1})/h   + b/3 * (f_{2}   - f_{2})/h   ]
! [ alpha 1     alpha              ][f'_2] [a * (f_{2}   - f_{1})/h   + b/3 * (f_{3}   - f_{1})/h   ]
! [       alpha 1     alpha        ][f'_i] [a * (f_{i}   - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h ]
! [             alpha 1     alpha  ][f'_4] [a * (f_{n-1} - f_{n-2})/h + b/3 * (f_{n-1} - f_{n-3})/h ]
! [                   0     1      ][f'_5] [a * (f_{n-1} - f_{n-1})/h + b/3 * (f_{n-2} - f_{n-2})/h ]
!----------------------------------------------------------------------------------------------------------
    d1fC2P(1,   1, IBC_SYMMETRIC) = ZERO ! not used
    d1fC2P(1,   2, IBC_SYMMETRIC) = ONE
    d1fC2P(1,   3, IBC_SYMMETRIC) = ZERO

    d1fC2P(5,   1, IBC_SYMMETRIC) = d1fC2P(1,   3, IBC_SYMMETRIC)
    d1fC2P(5,   2, IBC_SYMMETRIC) = d1fC2P(1,   2, IBC_SYMMETRIC)
    d1fC2P(5,   3, IBC_SYMMETRIC) = d1fC2P(1,   1, IBC_SYMMETRIC)

    d1fC2P(2:4, :, IBC_SYMMETRIC) = d1fC2P(2:4, :, IBC_PERIODIC)

    d1rC2P(:,   :, IBC_SYMMETRIC) = d1rC2P(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2P : asymmetric b.c.
! [ 1     2alpha                   ][f'_1]=[a * (f_{1}   + f_{1})/h   + b/3 * (f_{2}   + f_{2})/h   ]
! [ alpha 1     alpha              ][f'_2] [a * (f_{2}   - f_{1})/h   + b/3 * (f_{3}   + f_{1})/h   ]
! [       alpha 1     alpha        ][f'_i] [a * (f_{i}   - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h ]
! [             alpha 1     alpha  ][f'_4] [a * (f_{n-1} - f_{n-2})/h + b/3 * (-f_{n-1} - f_{n-3})/h ]
! [                   2alpha     1 ][f'_5] [a * (-f_{n-1} - f_{n-1})/h + b/3 * (-f_{n-2} - f_{n-2})/h ]
!----------------------------------------------------------------------------------------------------------
    d1fC2P(1,   1, IBC_ASYMMETRIC) = ZERO ! not used
    d1fC2P(1,   2, IBC_ASYMMETRIC) = ONE
    d1fC2P(1,   3, IBC_ASYMMETRIC) = TWO * alpha

    d1fC2P(5,   1, IBC_ASYMMETRIC) = d1fC2P(1,   3, IBC_ASYMMETRIC)
    d1fC2P(5,   2, IBC_ASYMMETRIC) = d1fC2P(1,   2, IBC_ASYMMETRIC)
    d1fC2P(5,   3, IBC_ASYMMETRIC) = d1fC2P(1,   1, IBC_ASYMMETRIC)

    d1fC2P(2:4, :, IBC_ASYMMETRIC) = d1fC2P(2:4, :, IBC_PERIODIC)

    d1rC2P(:,   :, IBC_ASYMMETRIC) = d1rC2P(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2C : symmetric b.c.
! [ 1-alpha  alpha                          ][f_1]=[a * (f_{2'}   - f_{1'})/h   + b/3 * (f_{3'}   - f_{2'})/h    ]
! [          alpha 1     alpha              ][f_2] [a * (f_{3'}   - f_{2'})/h   + b/3 * (f_{4'}   - f_{1'})/h    ]
! [                alpha 1     alpha        ][f_i] [a * (f_{i+1}  - f_{i-1})/h  + b/3 * (f_{i+2}  - f_{i-2})/h   ]
! [                      alpha 1     alpha  ][f_4] [a * (f_{n'}   - f_{n'-1})/h + b/3 * (f_{n'+1} - f_{n'-2})/h  ]
! [                            alpha 1-alpha][f_5] [a * (f_{n'+1} - f_{n'})/h   + b/3 * (f_{n'}   - f_{n'-1'})/h ]
!----------------------------------------------------------------------------------------------------------
    d1fP2C(1,   1, IBC_SYMMETRIC) = ZERO ! not used
    d1fP2C(1,   2, IBC_SYMMETRIC) = ONE - alpha
    d1fP2C(1,   3, IBC_SYMMETRIC) = alpha

    d1fP2C(5,   1, IBC_SYMMETRIC) = d1fP2C(1,   3, IBC_SYMMETRIC)
    d1fP2C(5,   2, IBC_SYMMETRIC) = d1fP2C(1,   2, IBC_SYMMETRIC)
    d1fP2C(5,   3, IBC_SYMMETRIC) = d1fP2C(1,   1, IBC_SYMMETRIC)

    d1fP2C(2:4, :, IBC_SYMMETRIC) = d1fP2C(2:4, :, IBC_PERIODIC)

    d1rP2C(:,   :, IBC_SYMMETRIC) = d1rP2C(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2C : asymmetric b.c.
! [ 1+alpha  alpha                          ][f_1]=[a * (f_{2'}   - f_{1'})/h   + b/3 * (f_{3'}   + f_{2'})/h    ]
! [          alpha 1     alpha              ][f_2] [a * (f_{3'}   - f_{2'})/h   + b/3 * (f_{4'}   - f_{1'})/h    ]
! [                alpha 1     alpha        ][f_i] [a * (f_{i+1}  - f_{i-1})/h  + b/3 * (f_{i+2}  - f_{i-2})/h   ]
! [                      alpha 1     alpha  ][f_4] [a * (f_{n'}   - f_{n'-1})/h + b/3 * (f_{n'+1} - f_{n'-2})/h  ]
! [                            alpha 1+alpha][f_5] [a * (f_{n'+1} - f_{n'})/h   + b/3 * (-f_{n'}   - f_{n'-1'})/h ]
!----------------------------------------------------------------------------------------------------------
    d1fP2C(1,   1, IBC_ASYMMETRIC) = ZERO ! not used
    d1fP2C(1,   2, IBC_ASYMMETRIC) = ONE + alpha
    d1fP2C(1,   3, IBC_ASYMMETRIC) = alpha

    d1fP2C(5,   1, IBC_ASYMMETRIC) = d1fP2C(1,   3, IBC_ASYMMETRIC)
    d1fP2C(5,   2, IBC_ASYMMETRIC) = d1fP2C(1,   2, IBC_ASYMMETRIC)
    d1fP2C(5,   3, IBC_ASYMMETRIC) = d1fP2C(1,   1, IBC_ASYMMETRIC)

    d1fP2C(2:4, :, IBC_ASYMMETRIC) = d1fP2C(2:4, :, IBC_PERIODIC)

    d1rP2C(:,   :, IBC_ASYMMETRIC) = d1rP2C(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2P : no specified, interpolation
! [ 1     alpha1                            ][f'_1']=[a1 * f_{1}/h  + b1 * f_{2}/h + c1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2'] [a2 * (f_{2} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i'] [a *  (f_{i} - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4'] [a2 * (f_{n-1} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5'] [-a1 * f_{n-1}/h  - b1 * f_{n-2}/h - c1 * f_{n-3}/h]
! eq(1): a +   b +    c = 0               !O(h1)
! eq(2): a + 3 b +  5 c = 2  alpha + 2    !O(h2)
! eq(3): a + 9 b + 25 c = 8  alpha        !O(h3)
! eq(4): a + 27b + 125c = 24 alpha        !O(h4)
!----------------------------------------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2) then ! eq(1-2)+alpha0+c0, O(h2)
      alpha1 = ZERO
          a1 = -ONE
          b1 = ONE
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CD4) then ! eq(1-3)+alpha0, O(h3)
      alpha1 = ZERO
          a1 = -TWO
          b1 = THREE
          c1 = -ONE

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else if (iaccu == IACCU_CP4) then ! eq(1-4), O(h4), no solution for c=0.

      alpha1 = TWENTYTHREE
          a1 = -TWENTYFIVE
          b1 = TWENTYSIX
          c1 = -ONE

      alpha2 = ONE / TWENTYTWO
          a2 = TWELVE / ELEVEN
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CP6) then ! eq(1-4), O(h4)

      alpha1 = TWENTYTHREE
          a1 = -TWENTYFIVE
          b1 = TWENTYSIX
          c1 = -ONE

      alpha2 = ONE / TWENTYTWO
          a2 = TWELVE / ELEVEN
          b2 = ZERO ! not used
          c2 = ZERO ! not used
        
    else  ! default 2nd CD
     alpha1 = ZERO
          a1 = -ONE
          b1 = ONE
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    end if

    d1fC2P(1, 1, IBC_INTRPL) = ZERO ! not used
    d1fC2P(1, 2, IBC_INTRPL) = ONE
    d1fC2P(1, 3, IBC_INTRPL) = alpha1
    d1rC2P(1, 1, IBC_INTRPL) = a1
    d1rC2P(1, 2, IBC_INTRPL) = b1
    d1rC2P(1, 3, IBC_INTRPL) = c1

    d1fC2P(5, 1, IBC_INTRPL) = d1fC2P(1, 3, IBC_INTRPL)
    d1fC2P(5, 2, IBC_INTRPL) = d1fC2P(1, 2, IBC_INTRPL)
    d1fC2P(5, 3, IBC_INTRPL) = d1fC2P(1, 1, IBC_INTRPL)
    d1rC2P(5, 1, IBC_INTRPL) = - d1rC2P(1, 1, IBC_INTRPL)
    d1rC2P(5, 2, IBC_INTRPL) = - d1rC2P(1, 2, IBC_INTRPL)
    d1rC2P(5, 3, IBC_INTRPL) = - d1rC2P(1, 3, IBC_INTRPL)

    d1fC2P(2, 1, IBC_INTRPL) = alpha2
    d1fC2P(2, 2, IBC_INTRPL) = ONE
    d1fC2P(2, 3, IBC_INTRPL) = alpha2
    d1rC2P(2, 1, IBC_INTRPL) = a2
    d1rC2P(2, 2, IBC_INTRPL) = b2 * ONE_THIRD ! not used
    d1rC2P(2, 3, IBC_INTRPL) = c2 ! not used

    d1fC2P(4, 1, IBC_INTRPL) = d1fC2P(2, 1, IBC_INTRPL)
    d1fC2P(4, 2, IBC_INTRPL) = d1fC2P(2, 2, IBC_INTRPL)
    d1fC2P(4, 3, IBC_INTRPL) = d1fC2P(2, 3, IBC_INTRPL)
    d1rC2P(4, 1, IBC_INTRPL) = d1rC2P(2, 1, IBC_INTRPL)
    d1rC2P(4, 2, IBC_INTRPL) = d1rC2P(2, 2, IBC_INTRPL)
    d1rC2P(4, 3, IBC_INTRPL) = d1rC2P(2, 3, IBC_INTRPL)

    d1fC2P(3, :, IBC_INTRPL) = d1fC2P(3, :, IBC_PERIODIC)
    d1rC2P(3, :, IBC_INTRPL) = d1rC2P(3, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2P : neumann
! [ 1     0                                 ][f'_1']=[known]
! [alpha2 1      alpha2                     ][f'_2'] [a2 * (f_{2} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i'] [a *  (f_{i} - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4'] [a2 * (f_{n-1} - f_{n-2})/h]
! [                            0      1     ][f'_5'] [known]
!----------------------------------------------------------------------------------------------------------
    d1fC2P(1, 1, IBC_NEUMANN) = ZERO ! not used
    d1fC2P(1, 2, IBC_NEUMANN) = ONE
    d1fC2P(1, 3, IBC_NEUMANN) = ZERO
    d1rC2P(1, 1, IBC_NEUMANN) = ZERO ! not used
    d1rC2P(1, 2, IBC_NEUMANN) = ZERO ! not used
    d1rC2P(1, 3, IBC_NEUMANN) = ZERO ! not used

    d1fC2P(5, 1, IBC_NEUMANN) = d1fC2P(1, 3, IBC_NEUMANN)
    d1fC2P(5, 2, IBC_NEUMANN) = d1fC2P(1, 2, IBC_NEUMANN)
    d1fC2P(5, 3, IBC_NEUMANN) = d1fC2P(1, 1, IBC_NEUMANN)
    d1rC2P(5, 1, IBC_NEUMANN) = d1rC2P(1, 1, IBC_NEUMANN)
    d1rC2P(5, 2, IBC_NEUMANN) = d1rC2P(1, 2, IBC_NEUMANN)
    d1rC2P(5, 3, IBC_NEUMANN) = d1rC2P(1, 3, IBC_NEUMANN)

    d1fC2P(2, 1, IBC_NEUMANN) = alpha2
    d1fC2P(2, 2, IBC_NEUMANN) = ONE
    d1fC2P(2, 3, IBC_NEUMANN) = alpha2
    d1rC2P(2, 1, IBC_NEUMANN) = a2
    d1rC2P(2, 2, IBC_NEUMANN) = b2 * ONE_THIRD ! not used
    d1rC2P(2, 3, IBC_NEUMANN) = c2 ! not used

    d1fC2P(4, 1, IBC_NEUMANN) = d1fC2P(2, 1, IBC_NEUMANN)
    d1fC2P(4, 2, IBC_NEUMANN) = d1fC2P(2, 2, IBC_NEUMANN)
    d1fC2P(4, 3, IBC_NEUMANN) = d1fC2P(2, 3, IBC_NEUMANN)
    d1rC2P(4, 1, IBC_NEUMANN) = d1rC2P(2, 1, IBC_NEUMANN)
    d1rC2P(4, 2, IBC_NEUMANN) = d1rC2P(2, 2, IBC_NEUMANN)
    d1rC2P(4, 3, IBC_NEUMANN) = d1rC2P(2, 3, IBC_NEUMANN)

    d1fC2P(3, :, IBC_NEUMANN) = d1fC2P(3, :, IBC_PERIODIC)
    d1rC2P(3, :, IBC_NEUMANN) = d1rC2P(3, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! C2P : Dirichlet
! [ 1     alpha1                            ][f'_1']=[a1 * f_{1'}/h + b1 * f_{1}/h  + c1 * f_{2}/h + d1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2'] [a2 * (f_{2} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i'] [a *  (f_{i} - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4'] [a2 * (f_{n-1} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5'] [-a1 * f_{n'}/h - b1 * f_{n-1}/h  - c1 * f_{n-2}/h - d1 * f_{n-3}/h]
!----------------------------------------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
        d1 = ZERO
    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2) then ! degrade to 1st CD, check other accuracy in this set. Check!!!
      alpha1 = ZERO
          a1 = ZERO! not used. 
          b1 = -ONE ! check. If a default asymmetric is used to achive dirichlet 0, a1=2, b1=0.
          c1 = ONE
          d1 = ZERO
      ! method 2 not to use the Dirichlet B.C. value, tested it and it is wrong. 
      ! alpha1 = ZERO
      !     a1 = - EIGHT * ONE_THIRD
      !     b1 = THREE
      !     c1 = - ONE_THIRD
      !     d1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CD4) then ! degrade to 2nd CD
     ! method 1 to use the Dirichlet B.C. value, check is this necessary? !!!
      ! alpha1 = ZERO
      !     a1 = - EIGHT * ONE_THIRD
      !     b1 = THREE
      !     c1 = - ONE_THIRD
      !     d1 = ZERO
      ! method 2 not to use the Dirichlet B.C. value
      alpha1 = ZERO
          a1 = ZERO
          b1 = - TWO
          c1 = THREE
          d1 = - ONE

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CP4) then ! degrade to 3rd CP
      ! method 1 to use the Dirichlet B.C. value, check is this necessary? !!!
      ! alpha1 = THREE
      !     a1 = - EIGHT * ONE_THIRD
      !     b1 = ZERO
      !     c1 = EIGHT * ONE_THIRD
      !     d1 = ZERO
      ! method 2 not to use the Dirichlet B.C. value
      alpha1 = TWENTYTHREE
          a1 = ZERO
          b1 = - TWENTYFIVE
          c1 = TWENTYSIX
          d1 = -ONE

      alpha2 = ONE / TWENTYTWO
          a2 = TWELVE / ELEVEN
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CP6) then ! degrade to 4th CP
     ! method 1 to use the Dirichlet B.C. value, check is this necessary? !!!
      ! alpha1 = FIFTEEN
      !     a1 = - SIXTEEN / FIFTEEN
      !     b1 = - FIFTEEN
      !     c1 = FIFTY * ONE_THIRD
      !     d1 = - THREE * ZPTWO

      alpha1 = TWENTYTHREE
          a1 = ZERO
          b1 = - TWENTYFIVE
          c1 = TWENTYSIX
          d1 = -ONE

      alpha2 = ONE / TWENTYTWO
          a2 = TWELVE / ELEVEN
          b2 = ZERO ! not used
          c2 = ZERO ! not used
      
    else  ! default 2nd CD
     alpha1 = ZERO
          a1 = ZERO
          b1 = - ONE
          c1 = ONE
          d1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    end if
  
    d1fC2P(1,   1, IBC_DIRICHLET) = ZERO ! not used
    d1fC2P(1,   2, IBC_DIRICHLET) = ONE
    d1fC2P(1,   3, IBC_DIRICHLET) = alpha1
    d1rC2P(1,   1, IBC_DIRICHLET) = a1
    d1rC2P(1,   2, IBC_DIRICHLET) = b1
    d1rC2P(1,   3, IBC_DIRICHLET) = c1
    d1rC2P(1,   4, IBC_DIRICHLET) = d1

    d1fC2P(5,   1, IBC_DIRICHLET) =   d1fC2P(1, 3, IBC_DIRICHLET)
    d1fC2P(5,   2, IBC_DIRICHLET) =   d1fC2P(1, 2, IBC_DIRICHLET)
    d1fC2P(5,   3, IBC_DIRICHLET) =   d1fC2P(1, 1, IBC_DIRICHLET)
    d1rC2P(5,   1, IBC_DIRICHLET) = - d1rC2P(1, 1, IBC_DIRICHLET)
    d1rC2P(5,   2, IBC_DIRICHLET) = - d1rC2P(1, 2, IBC_DIRICHLET)
    d1rC2P(5,   3, IBC_DIRICHLET) = - d1rC2P(1, 3, IBC_DIRICHLET)
    d1rC2P(5,   4, IBC_DIRICHLET) = - d1rC2P(1, 4, IBC_DIRICHLET)

    d1fC2P(2,   1, IBC_DIRICHLET) = alpha2
    d1fC2P(2,   2, IBC_DIRICHLET) = ONE
    d1fC2P(2,   3, IBC_DIRICHLET) = alpha2
    d1rC2P(2,   1, IBC_DIRICHLET) = a2
    d1rC2P(2,   2, IBC_DIRICHLET) = b2 * ONE_THIRD ! not used
    d1rC2P(2,   3, IBC_DIRICHLET) = c2 ! not used

    d1fC2P(4,   1, IBC_DIRICHLET) = d1fC2P(2, 1, IBC_DIRICHLET)
    d1fC2P(4,   2, IBC_DIRICHLET) = d1fC2P(2, 2, IBC_DIRICHLET)
    d1fC2P(4,   3, IBC_DIRICHLET) = d1fC2P(2, 3, IBC_DIRICHLET)
    d1rC2P(4,   1, IBC_DIRICHLET) = d1rC2P(2, 1, IBC_DIRICHLET)
    d1rC2P(4,   2, IBC_DIRICHLET) = d1rC2P(2, 2, IBC_DIRICHLET)
    d1rC2P(4,   3, IBC_DIRICHLET) = d1rC2P(2, 3, IBC_DIRICHLET)

    d1fC2P(3,   :, IBC_DIRICHLET) = d1fC2P(3, :, IBC_PERIODIC)
    d1rC2P(3,   :, IBC_DIRICHLET) = d1rC2P(3, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2C : no specified = Dirichlet B.C.
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1'}/h  + b1 * f_{2'}/h + c1 * f_{3'}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2 * (f_{3'} - f_{2'})/h  ]
! [       alpha  1      alpha               ][f'_i] [a *  (f_{i'+1} - f_{i'})/h + b/3 * (f_{i'+2} - f_{i'-1})/h]
! [                     alpha2 1      alpha2][f'_4] [a2 * (f_{n'} - f_{n'-1})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n'+1}/h  - b1 * f_{n'}/h - c1 * f_{n'-1}/h]
! eq(1): a +   b +    c = 0               !O(h1)
! eq(2):-a +   b +  3 c = 2  alpha + 2    !O(h2)
! eq(3): a +   b +  9 c = 8  alpha        !O(h3)
! eq(4):-a +   b +  27c = 24 alpha        !O(h4)
!----------------------------------------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
      alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2 ) then! eq(1-2)+alpha0+c0, O(h2)
      alpha1 = ZERO
          a1 = -ONE
          b1 = ONE
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CD4) then! eq(1-3)+alpha0, O(h3)
      alpha1 = ZERO
          a1 = -ONE
          b1 = ONE
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else if (iaccu == IACCU_CP4 ) then ! eq(1-3)+c0, O(h3)

      alpha1 = ZERO
          a1 = -ONE
          b1 = ONE
          c1 = ZERO

      alpha2 = ONE / TWENTYTWO
          a2 = TWELVE / ELEVEN
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CP6) then ! eq(1-4), O(h4)

      alpha1 = -ONE
          a1 = -ONE
          b1 = TWO
          c1 = -ONE

      alpha2 = ONE / TWENTYTWO
          a2 = TWELVE / ELEVEN
          b2 = ZERO ! not used
          c2 = ZERO ! not used
      
    else  ! default 2nd CD
      alpha1 = ZERO
          a1 = -ONE
          b1 = ONE
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    end if

    d1fP2C(1, 1, IBC_INTRPL) = alpha1 ! not used
    d1fP2C(1, 2, IBC_INTRPL) = ONE
    d1fP2C(1, 3, IBC_INTRPL) = alpha1
    d1rP2C(1, 1, IBC_INTRPL) = a1
    d1rP2C(1, 2, IBC_INTRPL) = b1
    d1rP2C(1, 3, IBC_INTRPL) = c1

    d1fP2C(5, 1, IBC_INTRPL) =   d1fP2C(1, 3, IBC_INTRPL)
    d1fP2C(5, 2, IBC_INTRPL) =   d1fP2C(1, 2, IBC_INTRPL)
    d1fP2C(5, 3, IBC_INTRPL) =   d1fP2C(1, 1, IBC_INTRPL)
    d1rP2C(5, 1, IBC_INTRPL) = - d1rP2C(1, 1, IBC_INTRPL)
    d1rP2C(5, 2, IBC_INTRPL) = - d1rP2C(1, 2, IBC_INTRPL)
    d1rP2C(5, 3, IBC_INTRPL) = - d1rP2C(1, 3, IBC_INTRPL)

    d1fP2C(2:4, :, IBC_INTRPL) = d1fP2C(2:4, :, IBC_PERIODIC)
    d1rP2C(2:4, :, IBC_INTRPL) = d1rP2C(2:4, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : 
! P2C : no specified = Dirichlet B.C. = Neumann
!----------------------------------------------------------------------------------------------------------
    d1fP2C(:, :, IBC_DIRICHLET) = d1fP2C(:, :, IBC_INTRPL)
    d1rP2C(:, :, IBC_DIRICHLET) = d1rP2C(:, :, IBC_INTRPL)

    d1fP2C(:, :, IBC_NEUMANN  ) = d1fP2C(:, :, IBC_INTRPL)
    d1rP2C(:, :, IBC_NEUMANN  ) = d1rP2C(:, :, IBC_INTRPL)
!==========================================================================================================
!interpolation. P2C and C2P Periodic or Symmetric B.C.
! P2C : i_max = nc
! alpha * f_{i-1} + f_i + alpha * f_{i+1} =    a/2 * ( f_{i'}   + f_{i'+1} ) + &
!                                              b/2 * ( f_{i'+2} + f_{i'-1} )
! C2P : i'_max = np
! alpha * f_{i'-1} + f_i' + alpha * f_{i'+1} = a/2 * ( f_{i}   + f_{i-1} ) + &
!                                              b/2 * ( f_{i+1} + f_{i-2} )
! eq(1): a +   b = 2  alpha + 1   !O(h2)
! eq(2): a + 9 b = 8  alpha       !O(h4)
! eq(3): a + 81b = 32 alpha       !O(h6)
!==========================================================================================================
    alpha = ZERO
        a = ONE
        b = ZERO
        c = ZERO
        ! interface = default CD4
alpha_itf = ZERO
    a_itf = NINE * EIGHTH
    b_itf = -ONE * EIGHTH
    c_itf = ZERO
    if (iaccu == IACCU_CD2) then
      alpha = ZERO
          a = ONE
          b = ZERO
    else if (iaccu == IACCU_CD4) then
      alpha = ZERO
          a = NINE * EIGHTH
          b = -ONE * EIGHTH
    else if (iaccu == IACCU_CP4) then
      alpha = ONE / SIX
          a = FOUR * ONE_THIRD
          b = ZERO
    else if (iaccu == IACCU_CP6) then
      alpha = THREE * ZPONE
          a = ONEPFIVE
          b = ONE * ZPONE
    else  ! default 2nd CD
      alpha = ZERO
          a = ONE
          b = ZERO
    end if
!----------------------------------------------------------------------------------------------------------
! interpolation. C2P: periodic & symmetric 
! [ 1    alpha                   alpha][f_1']=[a/2 * (f_{1}   + f_{n})   + b/2 * (f_{2}   + f_{n-1})]
! [      alpha 1     alpha            ][f_2'] [a/2 * (f_{3}   + f_{n})   + b/2 * (f_{2}   + f_{1})  ]
! [            alpha 1     alpha      ][f_i'] [a/2 * (f_{i}   + f_{i-1}) + b/2 * (f_{i+1} + f_{i-2})]
! [                  alpha 1     alpha][f_4'] [a/2 * (f_{n}   + f_{n-3}) + b/2 * (f_{n-1} + f_{n-2})]
! [alpha                   alpha 1    ][f_5'] [a/2 * (f_{1}   + f_{n-2}) + b/2 * (f_{n}   + f_{n-1})]
!-----------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!interpolation. C2P for periodic b.c.
!----------------------------------------------------------------------------------------------------------
    m1fC2P(1:5, 1, IBC_PERIODIC) = alpha
    m1fC2P(1:5, 2, IBC_PERIODIC) = ONE
    m1fC2P(1:5, 3, IBC_PERIODIC) = alpha
    m1rC2P(1:5, 1, IBC_PERIODIC) = a * HALF
    m1rC2P(1:5, 2, IBC_PERIODIC) = b * HALF
    m1rC2P(1:5, 3, IBC_PERIODIC) = c ! not used
!----------------------------------------------------------------------------------------------------------
! interpolation : 
! C2P : interior
!----------------------------------------------------------------------------------------------------------
    m1fC2P(1, 1, IBC_INTERIOR) = alpha_itf
    m1fC2P(1, 2, IBC_INTERIOR) = ONE
    m1fC2P(1, 3, IBC_INTERIOR) = alpha_itf
    m1rC2P(1, 1, IBC_INTERIOR) = a_itf * HALF  ! a/2
    m1rC2P(1, 2, IBC_INTERIOR) = b_itf * HALF ! b/4
    m1rC2P(1, 3, IBC_INTERIOR) = c_itf        ! not used

    m1fC2P(5,   :, IBC_INTERIOR) = m1fC2P(1,   :, IBC_INTERIOR)
    m1rC2P(5,   :, IBC_INTERIOR) = m1rC2P(1,   :, IBC_INTERIOR)
    m1fC2P(2:4, :, IBC_INTERIOR) = m1fC2P(2:4, :, IBC_PERIODIC)
    m1rC2P(2:4, :, IBC_INTERIOR) = m1rC2P(2:4, :, IBC_PERIODIC)
    
!----------------------------------------------------------------------------------------------------------
!interpolation. P2C for periodic b.c.
!----------------------------------------------------------------------------------------------------------
    m1fP2C(:, :, IBC_PERIODIC) = m1fC2P(:, :, IBC_PERIODIC)
    m1rP2C(:, :, IBC_PERIODIC) = m1rC2P(:, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! interpolation : 
! P2C : interior
!----------------------------------------------------------------------------------------------------------
    m1fP2C(:, :, IBC_INTERIOR) = m1fC2P(:, :, IBC_INTERIOR)
    m1rP2C(:, :, IBC_INTERIOR) = m1rC2P(:, :, IBC_INTERIOR)
!----------------------------------------------------------------------------------------------------------
!interpolation. C2P. symmetric, orthogonal, eg. u in y direction.
!----------------------------------------------------------------------------------------------------------
    m1fC2P(1,   1, IBC_SYMMETRIC) = ZERO ! not used
    m1fC2P(1,   2, IBC_SYMMETRIC) = ONE
    m1fC2P(1,   3, IBC_SYMMETRIC) = alpha + alpha
    m1fC2P(5,   1, IBC_SYMMETRIC) = m1fC2P(1,   3, IBC_SYMMETRIC)
    m1fC2P(5,   2, IBC_SYMMETRIC) = m1fC2P(1,   2, IBC_SYMMETRIC)
    m1fC2P(5,   3, IBC_SYMMETRIC) = m1fC2P(1,   1, IBC_SYMMETRIC)
    m1fC2P(2:4, :, IBC_SYMMETRIC) = m1fC2P(2:4, :, IBC_PERIODIC)

    m1rC2P(:,   :, IBC_SYMMETRIC) = m1rC2P(:, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
!interpolation. C2P. asymmetric, orthogonal, eg. v in y direction.
!----------------------------------------------------------------------------------------------------------
    m1fC2P(1,   1, IBC_ASYMMETRIC) = ZERO ! not used
    m1fC2P(1,   2, IBC_ASYMMETRIC) = ONE
    m1fC2P(1,   3, IBC_ASYMMETRIC) = ZERO
    m1fC2P(5,   1, IBC_ASYMMETRIC) = m1fC2P(1,   3, IBC_ASYMMETRIC)
    m1fC2P(5,   2, IBC_ASYMMETRIC) = m1fC2P(1,   2, IBC_ASYMMETRIC)
    m1fC2P(5,   3, IBC_ASYMMETRIC) = m1fC2P(1,   1, IBC_ASYMMETRIC)
    m1fC2P(2:4, :, IBC_ASYMMETRIC) = m1fC2P(2:4, :, IBC_PERIODIC)

    m1rC2P(:,   :, IBC_ASYMMETRIC) = m1rC2P(:, :, IBC_PERIODIC)
    m1rC2P(1,   :, IBC_ASYMMETRIC) = ZERO ! double safe, not necessary
    m1rC2P(5,   :, IBC_ASYMMETRIC) = ZERO
!----------------------------------------------------------------------------------------------------------
!interpolation. P2C. symmetric, orthogonal, eg. u in y direction.
!----------------------------------------------------------------------------------------------------------
    m1fP2C(1,   1, IBC_SYMMETRIC) = ZERO ! not used
    m1fP2C(1,   2, IBC_SYMMETRIC) = ONE + alpha
    m1fP2C(1,   3, IBC_SYMMETRIC) = alpha
    m1fP2C(5,   1, IBC_SYMMETRIC) = m1fP2C(1,   3, IBC_SYMMETRIC)
    m1fP2C(5,   2, IBC_SYMMETRIC) = m1fP2C(1,   2, IBC_SYMMETRIC)
    m1fP2C(5,   3, IBC_SYMMETRIC) = m1fP2C(1,   1, IBC_SYMMETRIC)
    m1fP2C(2:4, :, IBC_SYMMETRIC) = m1fP2C(2:4, :, IBC_PERIODIC)

    m1rP2C(:,   :, IBC_SYMMETRIC) = m1rP2C(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
!interpolation. P2C. asymmetric, orthogonal, eg. v in y direction.
!----------------------------------------------------------------------------------------------------------
    m1fP2C(1,   1, IBC_ASYMMETRIC) = ZERO ! not used
    m1fP2C(1,   2, IBC_ASYMMETRIC) = ONE - alpha
    m1fP2C(1,   3, IBC_ASYMMETRIC) = alpha
    m1fP2C(5,   1, IBC_ASYMMETRIC) = m1fP2C(1,   3, IBC_ASYMMETRIC)
    m1fP2C(5,   2, IBC_ASYMMETRIC) = m1fP2C(1,   2, IBC_ASYMMETRIC)
    m1fP2C(5,   3, IBC_ASYMMETRIC) = m1fP2C(1,   1, IBC_ASYMMETRIC)
    m1fP2C(2:4, :, IBC_ASYMMETRIC) = m1fP2C(2:4, :, IBC_PERIODIC)

    m1rP2C(:,   :, IBC_ASYMMETRIC) = m1rP2C(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! interpolation. P2C: Dirichlet = no specified = Neumann
! [ 1    alpha1                          ][f_1']=[a1 * f_{1'} + b1 * f_{2'} + c1 * f_{3'}  ]
! [      alpha2 1     alpha2             ][f_2'] [a2/2 * (f_{2'}   + f_{3'})]
! [             alpha 1      alpha       ][f_i'] [a/2  * (f_{i'}   + f_{i'+1}) + b/2 * (f_{i'+2} + f_{i'-1})]
! [                   alpha2 1     alpha2][f_4'] [a2/2 * (f_{n'-1}   + f_{n'})]
! [                          alpha1 1    ][f_5'] [a1   * f_{n'+1} + b1 * f_{n'} + c1 * f_{n'-1}]
! eq(1): a + b +   c =   alpha + 1   !O(h1)
! eq(2):-a + b + 3 c = 2 alpha       !O(h2)
! eq(3): a + b + 9 c = 4 alpha       !O(h3)
! eq(4):-a + b + 27c = 8 alpha       !O(h4)
!-----------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2) then ! eq(1-2)+alpha0+c0, O(h2)

      alpha1 = ZERO
          a1 = HALF
          b1 = HALF
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CD4) then ! eq(1-3)+alpha0, O(h3)

      alpha1 = ZERO
          a1 = THREE * EIGHTH
          b1 = THREE * QUARTER
          c1 = -ONE * EIGHTH

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else if (iaccu == IACCU_CP4) then ! eq(1-3)+c0, O(h3)

      alpha1 = ONE_THIRD
          a1 = ONE_THIRD
          b1 = ONE
          c1 = ZERO

      alpha2 = ONE / SIX
          a2 = FOUR * ONE_THIRD
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else if (iaccu == IACCU_CP6) then ! eq(1-4), O(h4)

      alpha1 = ONE
          a1 = QUARTER
          b1 = ONEPFIVE
          c1 = QUARTER

      alpha2 = ONE / SIX
          a2 = FOUR * ONE_THIRD
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else  ! default 2nd CD
      alpha1 = ZERO
          a1 = HALF
          b1 = HALF
          c1 = ONE

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    end if
    !P2C
    m1fP2C(1, 1, IBC_INTRPL) = ZERO ! not used
    m1fP2C(1, 2, IBC_INTRPL) = ONE
    m1fP2C(1, 3, IBC_INTRPL) = alpha1
    m1rP2C(1, 1, IBC_INTRPL) = a1
    m1rP2C(1, 2, IBC_INTRPL) = b1
    m1rP2C(1, 3, IBC_INTRPL) = c1

    m1fP2C(5, 1, IBC_INTRPL) = m1fP2C(1, 3, IBC_INTRPL)
    m1fP2C(5, 2, IBC_INTRPL) = m1fP2C(1, 2, IBC_INTRPL)
    m1fP2C(5, 3, IBC_INTRPL) = m1fP2C(1, 1, IBC_INTRPL)
    m1rP2C(5, 1, IBC_INTRPL) = m1rP2C(1, 1, IBC_INTRPL)
    m1rP2C(5, 2, IBC_INTRPL) = m1rP2C(1, 2, IBC_INTRPL)
    m1rP2C(5, 3, IBC_INTRPL) = m1rP2C(1, 3, IBC_INTRPL)

    m1fP2C(2:4, :, IBC_INTRPL) = m1fP2C(2:4, :, IBC_PERIODIC)
    m1rP2C(2:4, :, IBC_INTRPL) = m1rP2C(2:4, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! interpolation. P2C: Dirichlet = no specified = Neumann
!----------------------------------------------------------------------------------------------------------
    m1fP2C(:, :, IBC_DIRICHLET) = m1fP2C(:, :, IBC_INTRPL)
    m1rP2C(:, :, IBC_DIRICHLET) = m1rP2C(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! interpolation. P2C: Dirichlet = no specified = Neumann
!----------------------------------------------------------------------------------------------------------
    m1fP2C(:, :, IBC_NEUMANN) = m1fP2C(:, :, IBC_INTRPL)
    m1rP2C(:, :, IBC_NEUMANN) = m1rP2C(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! interpolation. C2P: No specified = neumann
! [ 1    alpha1                          ][f_1']=[a1 * f_{1} + b1 * f_{2} + c1 * f_{3}  ]
! [      alpha2 1     alpha2             ][f_2'] [a2/2 * (f_{2}   + f_{1})]
! [             alpha 1      alpha       ][f_i'] [a/2 * (f_{i}   + f_{i-1}) + b/2 * (f_{i+1} + f_{i-2})]
! [                   alpha2 1     alpha2][f_4'] [a2/2 * (f_{n-1}   + f_{n-2})]
! [                          alpha1 1    ][f_5'] [a1 * f_{n-1} + b1 * f_{n-2} + c1 * f_{n-3}]
! eq(1): a +   b +    c =   alpha + 1   !O(h1)
! eq(2): a + 3 b +  5 c = 2 alpha       !O(h2)
! eq(3): a + 9 b + 25 c = 4 alpha       !O(h3)
! eq(4): a + 27b + 125c = 8 alpha       !O(h4)
!-----------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
      alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2) then ! eq(1-2)+alpha0+c0, O(h2)

      alpha1 = ZERO
          a1 = ONEPFIVE
          b1 = -HALF
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used

      else if (iaccu == IACCU_CD4) then ! eq(1-3)+alpha0, O(h3)

      alpha1 = ZERO
          a1 = FIFTEEN * EIGHTH
          b1 = - FIVE * QUARTER
          c1 = THREE * EIGHTH

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CP4) then ! eq(1-3)+c0, O(h3)

      alpha1 = THREE
          a1 = THREE
          b1 = ONE
          c1 = ZERO

      alpha2 = ONE / SIX 
          a2 = FOUR * ONE_THIRD
          b2 = ZERO ! not used
          c2 = ZERO ! not used
    else if (iaccu == IACCU_CP6) then ! eq(1-4), O(h4)

      alpha1 = FIVE
          a1 = FIFTEEN * QUARTER
          b1 = TWOPFIVE
          c1 = -QUARTER

      alpha2 = ONE / SIX 
          a2 = FOUR * ONE_THIRD
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else  ! default 2nd CD

      alpha1 = ZERO
          a1 = ONEPFIVE
          b1 = -HALF
          c1 = ZERO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
     
    end if

    m1fC2P(1, 1, IBC_INTRPL) = ZERO ! not used
    m1fC2P(1, 2, IBC_INTRPL) = ONE
    m1fC2P(1, 3, IBC_INTRPL) = alpha1
    m1rC2P(1, 1, IBC_INTRPL) = a1
    m1rC2P(1, 2, IBC_INTRPL) = b1
    m1rC2P(1, 3, IBC_INTRPL) = c1

    m1fC2P(5, 1, IBC_INTRPL) = m1fC2P(1, 3, IBC_INTRPL)
    m1fC2P(5, 2, IBC_INTRPL) = m1fC2P(1, 2, IBC_INTRPL)
    m1fC2P(5, 3, IBC_INTRPL) = m1fC2P(1, 1, IBC_INTRPL)
    m1rC2P(5, 1, IBC_INTRPL) = m1rC2P(1, 1, IBC_INTRPL)
    m1rC2P(5, 2, IBC_INTRPL) = m1rC2P(1, 2, IBC_INTRPL)
    m1rC2P(5, 3, IBC_INTRPL) = m1rC2P(1, 3, IBC_INTRPL)

    m1fC2P(2, 1, IBC_INTRPL) = alpha2
    m1fC2P(2, 2, IBC_INTRPL) = ONE
    m1fC2P(2, 3, IBC_INTRPL) = alpha2
    m1rC2P(2, 1, IBC_INTRPL) = a2 * HALF
    m1rC2P(2, 2, IBC_INTRPL) = ZERO ! not used
    m1rC2P(2, 3, IBC_INTRPL) = ZERO ! not used

    m1fC2P(4, 1, IBC_INTRPL) = m1fC2P(2, 1, IBC_INTRPL)
    m1fC2P(4, 2, IBC_INTRPL) = m1fC2P(2, 2, IBC_INTRPL)
    m1fC2P(4, 3, IBC_INTRPL) = m1fC2P(2, 3, IBC_INTRPL)
    m1rC2P(4, 1, IBC_INTRPL) = m1rC2P(2, 1, IBC_INTRPL)
    m1rC2P(4, 2, IBC_INTRPL) = m1rC2P(2, 2, IBC_INTRPL)
    m1rC2P(4, 3, IBC_INTRPL) = m1rC2P(2, 3, IBC_INTRPL)

    m1fC2P(3, :, IBC_INTRPL) = m1fC2P(3, :, IBC_PERIODIC)
    m1rC2P(3, :, IBC_INTRPL) = m1rC2P(3, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! interpolation. C2P: No specified = neumann
!----------------------------------------------------------------------------------------------------------
    m1fC2P(:, :, IBC_NEUMANN) = m1fC2P(:, :, IBC_INTRPL)
    m1rC2P(:, :, IBC_NEUMANN) = m1rC2P(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! interpolation. C2P: Dirichlet
! [ 1    0                              ][f_1']=known
! [      alpha2 1     alpha2             ][f_2'] [a2/2 * (f_{2}   + f_{1})]
! [             alpha 1      alpha       ][f_i'] [a/2 * (f_{i}   + f_{i-1}) + b/2 * (f_{i+1} + f_{i-2})]
! [                   alpha2 1     alpha2][f_4'] [a2/2 * (f_{n-1}   + f_{n-2})]
! [                          0     1    ][f_5'] known
!-----------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
      alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
    if (iaccu == IACCU_CD2 .or. iaccu == IACCU_CD4) then ! degrade to 2nd CD

      alpha1 = ZERO
          a1 = ZERO ! not used
          b1 = ZERO ! not used
          c1 = ZERO ! not used

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else if (iaccu == IACCU_CP4 .or. iaccu == IACCU_CP6) then ! degrade to 3rd CP

      alpha1 = ZERO
          a1 = ZERO ! not used
          b1 = ZERO ! not used
          c1 = ZERO ! not used

      alpha2 = ONE / SIX 
          a2 = FOUR * ONE_THIRD
          b2 = ZERO ! not used
          c2 = ZERO ! not used

    else  ! default 2nd CD

      alpha1 = ZERO
          a1 = ZERO ! not used
          b1 = ZERO ! not used
          c1 = ZERO ! not used

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO ! not used
          c2 = ZERO ! not used
     
    end if

    m1fC2P(1, 1, IBC_DIRICHLET) = ZERO ! not used
    m1fC2P(1, 2, IBC_DIRICHLET) = ONE
    m1fC2P(1, 3, IBC_DIRICHLET) = alpha1
    m1rC2P(1, 1, IBC_DIRICHLET) = a1
    m1rC2P(1, 2, IBC_DIRICHLET) = b1
    m1rC2P(1, 3, IBC_DIRICHLET) = c1

    m1fC2P(5, 1, IBC_DIRICHLET) = m1fC2P(1, 3, IBC_DIRICHLET)
    m1fC2P(5, 2, IBC_DIRICHLET) = m1fC2P(1, 2, IBC_DIRICHLET)
    m1fC2P(5, 3, IBC_DIRICHLET) = m1fC2P(1, 1, IBC_DIRICHLET)
    m1rC2P(5, 1, IBC_DIRICHLET) = m1rC2P(1, 1, IBC_DIRICHLET)
    m1rC2P(5, 2, IBC_DIRICHLET) = m1rC2P(1, 2, IBC_DIRICHLET)
    m1rC2P(5, 3, IBC_DIRICHLET) = m1rC2P(1, 3, IBC_DIRICHLET)

    m1fC2P(2, 1, IBC_DIRICHLET) = alpha2
    m1fC2P(2, 2, IBC_DIRICHLET) = ONE
    m1fC2P(2, 3, IBC_DIRICHLET) = alpha2
    m1rC2P(2, 1, IBC_DIRICHLET) = a2 * HALF
    m1rC2P(2, 2, IBC_DIRICHLET) = ZERO ! not used
    m1rC2P(2, 3, IBC_DIRICHLET) = ZERO ! not used

    m1fC2P(4, 1, IBC_DIRICHLET) = m1fC2P(2, 1, IBC_DIRICHLET)
    m1fC2P(4, 2, IBC_DIRICHLET) = m1fC2P(2, 2, IBC_DIRICHLET)
    m1fC2P(4, 3, IBC_DIRICHLET) = m1fC2P(2, 3, IBC_DIRICHLET)
    m1rC2P(4, 1, IBC_DIRICHLET) = m1rC2P(2, 1, IBC_DIRICHLET)
    m1rC2P(4, 2, IBC_DIRICHLET) = m1rC2P(2, 2, IBC_DIRICHLET)
    m1rC2P(4, 3, IBC_DIRICHLET) = m1rC2P(2, 3, IBC_DIRICHLET)

    m1fC2P(3, :, IBC_DIRICHLET) = m1fC2P(3, :, IBC_PERIODIC)
    m1rC2P(3, :, IBC_DIRICHLET) = m1rC2P(3, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P and C2C, periodic, symmetric, asymmetric
!----------------------------------------------------------------------------------------------------------
    alpha = ZERO
        a = ONE
        b = ZERO
        c = ZERO ! not used
        d = ZERO ! not used
alpha_itf = ZERO
    a_itf = FOUR * ONE_THIRD
    b_itf = - ONE_THIRD
    c_itf = ZERO
    d_itf = ZERO
    if (iaccu == IACCU_CD2) then
      alpha = ZERO
          a = ONE
          b = ZERO
    else if (iaccu == IACCU_CD4) then
      alpha = ZERO
          a = FOUR * ONE_THIRD
          b = - ONE_THIRD
    else if (iaccu == IACCU_CP4) then
      alpha = ONE * ZPONE
          a = SIX * ZPTWO
          b = ZERO
    else if (iaccu == IACCU_CP6) then
      alpha = TWO / ELEVEN
          a = TWELVE / ELEVEN
          b = THREE / ELEVEN
    else  ! default 2nd CD
      alpha = ZERO
          a = ONE
          b = ZERO
    end if
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative C2C, periodic
!----------------------------------------------------------------------------------------------------------
    d2fC2C(1:5, 1, IBC_PERIODIC) = alpha
    d2fC2C(1:5, 2, IBC_PERIODIC) = ONE
    d2fC2C(1:5, 3, IBC_PERIODIC) = alpha

    d2rC2C(1:5, 1, IBC_PERIODIC) = a
    d2rC2C(1:5, 2, IBC_PERIODIC) = b * QUARTER
    d2rC2C(1:5, 3, IBC_PERIODIC) = c ! not used
    d2rC2C(1:5, 4, IBC_PERIODIC) = d ! not used
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P , interior
!----------------------------------------------------------------------------------------------------------
    d2fC2C(1, 1, IBC_INTERIOR) = alpha_itf
    d2fC2C(1, 2, IBC_INTERIOR) = ONE
    d2fC2C(1, 3, IBC_INTERIOR) = alpha_itf
    d2rC2C(1, 1, IBC_INTERIOR) = a_itf
    d2rC2C(1, 2, IBC_INTERIOR) = b_itf * QUARTER
    d2rC2C(1, 3, IBC_INTERIOR) = c_itf ! not used       
    d2rC2C(1, 4, IBC_INTERIOR) = d_itf ! not used       

    d2fC2C(5,   :, IBC_INTERIOR) = d2fC2C(1,   :, IBC_INTERIOR)
    d2rC2C(5,   :, IBC_INTERIOR) = d2rC2C(1,   :, IBC_INTERIOR)
    d2fC2C(2:4, :, IBC_INTERIOR) = d2fC2C(2:4, :, IBC_PERIODIC)
    d2rC2C(2:4, :, IBC_INTERIOR) = d2rC2C(2:4, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P , periodic
!----------------------------------------------------------------------------------------------------------
    d2fP2P(:, :, IBC_PERIODIC) = d2fC2C(:, :, IBC_PERIODIC)
    d2rP2P(:, :, IBC_PERIODIC) = d2rC2C(:, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P , interior
!----------------------------------------------------------------------------------------------------------
    d2fP2P(:, :, IBC_INTERIOR) = d2fC2C(:, :, IBC_INTERIOR)
    d2rP2P(:, :, IBC_INTERIOR) = d2rC2C(:, :, IBC_INTERIOR)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative C2C, symmetric
!----------------------------------------------------------------------------------------------------------
    d2fC2C(1,   1, IBC_SYMMETRIC) = ZERO ! not used
    d2fC2C(1,   2, IBC_SYMMETRIC) = ONE + alpha
    d2fC2C(1,   3, IBC_SYMMETRIC) = alpha
    d2fC2C(5,   1, IBC_SYMMETRIC) = d2fC2C(1,   3, IBC_SYMMETRIC)
    d2fC2C(5,   2, IBC_SYMMETRIC) = d2fC2C(1,   2, IBC_SYMMETRIC)
    d2fC2C(5,   3, IBC_SYMMETRIC) = d2fC2C(1,   1, IBC_SYMMETRIC)
    d2fC2C(2:4, :, IBC_SYMMETRIC) = d2fC2C(2:4, :, IBC_PERIODIC)

    d2rC2C(:,   :, IBC_SYMMETRIC) = d2rC2C(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative C2C, asymmetric
!----------------------------------------------------------------------------------------------------------
    d2fC2C(1,   1, IBC_ASYMMETRIC) = ZERO ! not used
    d2fC2C(1,   2, IBC_ASYMMETRIC) = ONE - alpha
    d2fC2C(1,   3, IBC_ASYMMETRIC) = alpha
    d2fC2C(5,   1, IBC_ASYMMETRIC) = d2fC2C(1,   3, IBC_ASYMMETRIC)
    d2fC2C(5,   2, IBC_ASYMMETRIC) = d2fC2C(1,   2, IBC_ASYMMETRIC)
    d2fC2C(5,   3, IBC_ASYMMETRIC) = d2fC2C(1,   1, IBC_ASYMMETRIC)
    d2fC2C(2:4, :, IBC_ASYMMETRIC) = d2fC2C(2:4, :, IBC_PERIODIC)

    d2rC2C(:,   :, IBC_ASYMMETRIC) = d2rC2C(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P, symmetric
!----------------------------------------------------------------------------------------------------------
    d2fP2P(1,   1, IBC_SYMMETRIC) = ZERO ! not used
    d2fP2P(1,   2, IBC_SYMMETRIC) = ONE
    d2fP2P(1,   3, IBC_SYMMETRIC) = alpha + alpha
    d2fP2P(5,   1, IBC_SYMMETRIC) = d2fP2P(1,   3, IBC_SYMMETRIC)
    d2fP2P(5,   2, IBC_SYMMETRIC) = d2fP2P(1,   2, IBC_SYMMETRIC)
    d2fP2P(5,   3, IBC_SYMMETRIC) = d2fP2P(1,   1, IBC_SYMMETRIC)
    d2fP2P(2:4, :, IBC_SYMMETRIC) = d2fP2P(2:4, :, IBC_PERIODIC)
    d2rP2P(:,   :, IBC_SYMMETRIC) = d2rP2P(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P, asymmetric
!----------------------------------------------------------------------------------------------------------
    d2fP2P(1,   1, IBC_ASYMMETRIC) = ZERO ! not used
    d2fP2P(1,   2, IBC_ASYMMETRIC) = ONE
    d2fP2P(1,   3, IBC_ASYMMETRIC) = ZERO
    d2fP2P(5,   1, IBC_ASYMMETRIC) = d2fP2P(1,   3, IBC_ASYMMETRIC)
    d2fP2P(5,   2, IBC_ASYMMETRIC) = d2fP2P(1,   2, IBC_ASYMMETRIC)
    d2fP2P(5,   3, IBC_ASYMMETRIC) = d2fP2P(1,   1, IBC_ASYMMETRIC)
    d2fP2P(2:4, :, IBC_ASYMMETRIC) = d2fP2P(2:4, :, IBC_PERIODIC)
    d2rP2P(:,   :, IBC_ASYMMETRIC) = d2rP2P(:,   :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P, no specified = dirichlet = neumann 
!----------------------------------------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
        d1 = ZERO

    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
        d2 = ZERO
    if (iaccu == IACCU_CD2 .or. iaccu == IACCU_CD4) then
      alpha1 = ZERO
          a1 = TWO
          b1 = -FIVE
          c1 = FOUR
          d1 = -ONE

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
          c2 = ZERO ! not used
          d2 = ZERO ! not used
    else if (iaccu == IACCU_CP4 .or. iaccu == IACCU_CP6 ) then ! degrade to 3rd CP
      alpha1 = ELEVEN
          a1 = THIRTEEN
          b1 = -TWENTYSEVEN
          c1 = FIFTEEN
          d1 = -ONE

      alpha2 = ONE * ZPONE
          a2 = SIX * ZPTWO
          b2 = ZERO
          c2 = ZERO ! not used
          d2 = ZERO ! not used
    else  ! default 2nd CD
      alpha1 = ZERO
          a1 = TWO
          b1 = -FIVE
          c1 = FOUR
          d1 = -ONE

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
          c2 = ZERO ! not used
          d2 = ZERO ! not used
    end if

    d2fP2P(1, 1, IBC_INTRPL) = ZERO ! not used
    d2fP2P(1, 2, IBC_INTRPL) = ONE
    d2fP2P(1, 3, IBC_INTRPL) = alpha1
    d2rP2P(1, 1, IBC_INTRPL) = a1
    d2rP2P(1, 2, IBC_INTRPL) = b1
    d2rP2P(1, 3, IBC_INTRPL) = c1
    d2rP2P(1, 4, IBC_INTRPL) = d1

    d2fP2P(5, 1, IBC_INTRPL) = d2fP2P(1, 3, IBC_INTRPL)
    d2fP2P(5, 2, IBC_INTRPL) = d2fP2P(1, 2, IBC_INTRPL)
    d2fP2P(5, 3, IBC_INTRPL) = d2fP2P(1, 1, IBC_INTRPL)
    d2rP2P(5, 1, IBC_INTRPL) = d2rP2P(1, 1, IBC_INTRPL)
    d2rP2P(5, 2, IBC_INTRPL) = d2rP2P(1, 2, IBC_INTRPL)
    d2rP2P(5, 3, IBC_INTRPL) = d2rP2P(1, 3, IBC_INTRPL)
    d2rP2P(5, 4, IBC_INTRPL) = d2rP2P(1, 4, IBC_INTRPL)

    d2fP2P(2, 1, IBC_INTRPL) = alpha2
    d2fP2P(2, 2, IBC_INTRPL) = ONE
    d2fP2P(2, 3, IBC_INTRPL) = alpha2
    d2rP2P(2, 1, IBC_INTRPL) = a2
    d2rP2P(2, 2, IBC_INTRPL) = ZERO ! not used
    d2rP2P(2, 3, IBC_INTRPL) = ZERO ! not used
    d2rP2P(2, 4, IBC_INTRPL) = ZERO ! not used

    d2fP2P(4, 1, IBC_INTRPL) = d2fP2P(2, 1, IBC_INTRPL)
    d2fP2P(4, 2, IBC_INTRPL) = d2fP2P(2, 2, IBC_INTRPL)
    d2fP2P(4, 3, IBC_INTRPL) = d2fP2P(2, 3, IBC_INTRPL)
    d2rP2P(4, 1, IBC_INTRPL) = d2rP2P(2, 1, IBC_INTRPL)
    d2rP2P(4, 2, IBC_INTRPL) = d2rP2P(2, 2, IBC_INTRPL)
    d2rP2P(4, 3, IBC_INTRPL) = d2rP2P(2, 3, IBC_INTRPL)
    d2rP2P(4, 4, IBC_INTRPL) = d2rP2P(2, 4, IBC_INTRPL)

    d2fP2P(3, :, IBC_INTRPL) = d2fP2P(3, :, IBC_PERIODIC)
    d2rP2P(3, :, IBC_INTRPL) = d2rP2P(3, :, IBC_PERIODIC)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P, no specified = dirichlet 
!----------------------------------------------------------------------------------------------------------
    d2fP2P(:, :, IBC_DIRICHLET) = d2fP2P(:, :, IBC_INTRPL)
    d2rP2P(:, :, IBC_DIRICHLET) = d2rP2P(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative P2P, no specified = neumann 
!----------------------------------------------------------------------------------------------------------
    d2fP2P(:, :, IBC_NEUMANN) = d2fP2P(:, :, IBC_INTRPL)
    d2rP2P(:, :, IBC_NEUMANN) = d2rP2P(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative C2C, no specified =  neumann = P2C unspecified
!----------------------------------------------------------------------------------------------------------
    d2fC2C(:, :, IBC_INTRPL) = d2fP2P(:, :, IBC_INTRPL)
    d2rC2C(:, :, IBC_INTRPL) = d2rP2P(:, :, IBC_INTRPL)

    d2fC2C(:, :, IBC_NEUMANN) = d2fC2C(:, :, IBC_INTRPL)
    d2rC2C(:, :, IBC_NEUMANN) = d2rC2C(:, :, IBC_INTRPL)
!----------------------------------------------------------------------------------------------------------
! 2nd diriviative C2C, Dirchilet
!----------------------------------------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
        d1 = ZERO

    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO
        c2 = ZERO
        d2 = ZERO
    if (iaccu == IACCU_CD2 .or. iaccu == IACCU_CD4) then
      alpha1 = ZERO
          a1 = SIXTEEN * ZPTWO
          b1 = -FIVE
          c1 = TWO
          d1 = -ONE * ZPTWO

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
          c2 = ZERO ! not used
          d2 = ZERO ! not used
    else if (iaccu == IACCU_CP4 .or. iaccu == IACCU_CP6 ) then ! degrade to 3rd CP
      alpha1 = HALF
          a1 = SIXTEEN * ZPTWO
          b1 = -NINE * HALF
          c1 = ONE
          d1 = THREE * ZPONE

      alpha2 = ONE * ZPONE
          a2 = SIX * ZPTWO
          b2 = ZERO
          c2 = ZERO ! not used
          d2 = ZERO ! not used
    else  ! default 2nd CD
      alpha1 = ZERO
          a1 = SIXTEEN / SEVEN
          b1 = -TWENTYFIVE / SEVEN
          c1 = TEN / SEVEN
          d1 = -ONE / SEVEN

      alpha2 = ZERO
          a2 = ONE
          b2 = ZERO
          c2 = ZERO ! not used
          d2 = ZERO ! not used
    end if

    d2fC2C(1, 1, IBC_DIRICHLET) = ZERO ! not used
    d2fC2C(1, 2, IBC_DIRICHLET) = ONE
    d2fC2C(1, 3, IBC_DIRICHLET) = alpha1
    d2rC2C(1, 1, IBC_DIRICHLET) = a1
    d2rC2C(1, 2, IBC_DIRICHLET) = b1
    d2rC2C(1, 3, IBC_DIRICHLET) = c1
    d2rC2C(1, 4, IBC_DIRICHLET) = d1

    d2fC2C(5, 1, IBC_DIRICHLET) = d2fC2C(1, 3, IBC_DIRICHLET)
    d2fC2C(5, 2, IBC_DIRICHLET) = d2fC2C(1, 2, IBC_DIRICHLET)
    d2fC2C(5, 3, IBC_DIRICHLET) = d2fC2C(1, 1, IBC_DIRICHLET)
    d2rC2C(5, 1, IBC_DIRICHLET) = d2rC2C(1, 1, IBC_DIRICHLET)
    d2rC2C(5, 2, IBC_DIRICHLET) = d2rC2C(1, 2, IBC_DIRICHLET)
    d2rC2C(5, 3, IBC_DIRICHLET) = d2rC2C(1, 3, IBC_DIRICHLET)
    d2rC2C(5, 4, IBC_DIRICHLET) = d2rC2C(1, 4, IBC_DIRICHLET)

    d2fC2C(2, 1, IBC_DIRICHLET) = alpha2
    d2fC2C(2, 2, IBC_DIRICHLET) = ONE
    d2fC2C(2, 3, IBC_DIRICHLET) = alpha2
    d2rC2C(2, 1, IBC_DIRICHLET) = a2
    d2rC2C(2, 2, IBC_DIRICHLET) = ZERO ! not used
    d2rC2C(2, 3, IBC_DIRICHLET) = ZERO ! not used
    d2rC2C(2, 4, IBC_DIRICHLET) = ZERO ! not used

    d2fC2C(4, 1, IBC_DIRICHLET) = d2fC2C(2, 1, IBC_DIRICHLET)
    d2fC2C(4, 2, IBC_DIRICHLET) = d2fC2C(2, 2, IBC_DIRICHLET)
    d2fC2C(4, 3, IBC_DIRICHLET) = d2fC2C(2, 3, IBC_DIRICHLET)
    d2rC2C(4, 1, IBC_DIRICHLET) = d2rC2C(2, 1, IBC_DIRICHLET)
    d2rC2C(4, 2, IBC_DIRICHLET) = d2rC2C(2, 2, IBC_DIRICHLET)
    d2rC2C(4, 3, IBC_DIRICHLET) = d2rC2C(2, 3, IBC_DIRICHLET)
    d2rC2C(4, 4, IBC_DIRICHLET) = d2rC2C(2, 4, IBC_DIRICHLET)
    
    d2fC2C(3, :, IBC_DIRICHLET) = d2fC2C(3, :, IBC_PERIODIC)
    d2rC2C(3, :, IBC_DIRICHLET) = d2rC2C(3, :, IBC_PERIODIC)


    if(nrank == 0) call Print_debug_end_msg
    return
  end subroutine Prepare_compact_coefficients
!==========================================================================================================
!> \brief Assigning the sparse matrix in the LHS of the compact scheme, and
!> calculating the geometry-only dependent variables for the TDMA scheme.
!>
!> This subroutine is called once locally.
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           !
!----------------------------------------------------------------------------------------------------------
!> \param[in]     n             the number of unknown array
!> \param[in]     bc            the boundary condition at two ends of the unknown
!> \param[in]     coeff         the basic TDMA coefficients defined above.
!> \param[out]    a             the coefficients for TDMA
!> \param[out]    b             a_i * x_(i-1) + b_i * x_(i) + c_i * x_(i+1)
!> \param[out]    c             = RHS
!> \param[out]    d             An assisting coeffients for the TDMA scheme.
!----------------------------------------------------------------------------------------------------------
  subroutine Buildup_TDMA_LHS_array(n, is_periodic, coeff, a, b, c, d)
    use tridiagonal_matrix_algorithm
    use parameters_constant_mod
    implicit none

    integer, intent(in) :: n
    logical,  intent(in)   :: is_periodic
    real(WP), intent(in)   :: coeff(5, 3, 0:6)
    real(WP), intent(out)  :: a(n, 0:6, 0:6), &
                              b(n, 0:6, 0:6), &
                              c(n, 0:6, 0:6), &
                              d(n, 0:6, 0:6)

    integer :: i, j

    a(:, :, :) =  ZERO
    b(:, :, :) =  ZERO
    c(:, :, :) =  ZERO
    d(:, :, :) =  ZERO

    do j = 0, 6
      do i = 0, 6

        if (j == IBC_PERIODIC .and. i /= IBC_PERIODIC) cycle
        if (j /= IBC_PERIODIC .and. i == IBC_PERIODIC) cycle

        a(1,         i, j) = coeff( 1, 1, i )
        a(2,         i, j) = coeff( 2, 1, i )
        a(3 : n - 2, i, j) = coeff( 3, 1, IBC_PERIODIC )
        a(n - 1,     i, j) = coeff( 4, 1, j )
        a(n,         i, j) = coeff( 5, 1, j )

        b(1,         i, j) = coeff( 1, 2, i )
        b(2,         i, j) = coeff( 2, 2, i )
        b(3 : n - 2, i, j) = coeff( 3, 2, IBC_PERIODIC )
        b(n - 1,     i, j) = coeff( 4, 2, j )
        b(n,         i, j) = coeff( 5, 2, j )

        c(1,         i, j) = coeff( 1, 3, i )
        c(2,         i, j) = coeff( 2, 3, i )
        c(3 : n - 2, i, j) = coeff( 3, 3, IBC_PERIODIC )
        c(n - 1,     i, j) = coeff( 4, 3, j )
        c(n,         i, j) = coeff( 5, 3, j )

        if (is_periodic) then
          call Preprocess_TDMA_coeffs( a(1:n-1, i, j), &
                                       b(1:n-1, i, j), &
                                       c(1:n-1, i, j), &
                                       d(1:n-1, i, j), &
                                       n-1)
        else 
          call Preprocess_TDMA_coeffs( a(:, i, j), &
                                       b(:, i, j), &
                                       c(:, i, j), &
                                       d(:, i, j), &
                                       n)
        end if 
      end do
    end do

    return
  end subroutine Buildup_TDMA_LHS_array
!==========================================================================================================
!> \brief Preparing coefficients for TDMA calculation.
!----------------------------------------------------------------------------------------------------------
!> Scope:  mpi    called-freq    xdomain
!>         all    once           all
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           !
!----------------------------------------------------------------------------------------------------------
!==========================================================================================================
  subroutine Prepare_LHS_coeffs_for_operations
    use vars_df_mod, only : domain
    use mpi_mod
    use parameters_constant_mod
    implicit none
    integer :: i, nsz

!==========================================================================================================
!   building up the basic lhs coeffients for compact schemes, based on the given
!   accuracy
!==========================================================================================================
    call Prepare_compact_coefficients
!----------------------------------------------------------------------------------------------------------
!   building up the full size lhs coeffients for compact schemes
!----------------------------------------------------------------------------------------------------------
!==========================================================================================================
! y-direction, with nc unknows
!==========================================================================================================
    i = 2
    nsz = domain(1)%nc(i)
!----------------------------------------------------------------------------------------------------------
!   1st derivative in y direction with nc unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad1y_C2C ( nsz, 0:6, 0:6 ) ); ad1y_C2C(:, :, :) = ZERO
    allocate (bd1y_C2C ( nsz, 0:6, 0:6 ) ); bd1y_C2C(:, :, :) = ZERO
    allocate (cd1y_C2C ( nsz, 0:6, 0:6 ) ); cd1y_C2C(:, :, :) = ZERO
    allocate (dd1y_C2C ( nsz, 0:6, 0:6 ) ); dd1y_C2C(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fC2C, &
          ad1y_C2C, bd1y_C2C, cd1y_C2C, dd1y_C2C)

    allocate (ad1y_P2C ( nsz, 0:6, 0:6 ) ); ad1y_P2C(:, :, :) = ZERO
    allocate (bd1y_P2C ( nsz, 0:6, 0:6 ) ); bd1y_P2C(:, :, :) = ZERO
    allocate (cd1y_P2C ( nsz, 0:6, 0:6 ) ); cd1y_P2C(:, :, :) = ZERO
    allocate (dd1y_P2C ( nsz, 0:6, 0:6 ) ); dd1y_P2C(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fP2C, &
          ad1y_P2C, bd1y_P2C, cd1y_P2C, dd1y_P2C)
!----------------------------------------------------------------------------------------------------------
!   mid-point interpolation in y direction with nc unknows
!----------------------------------------------------------------------------------------------------------
    allocate (am1y_P2C ( nsz, 0:6, 0:6 ) ); am1y_P2C(:, :, :) = ZERO
    allocate (bm1y_P2C ( nsz, 0:6, 0:6 ) ); bm1y_P2C(:, :, :) = ZERO
    allocate (cm1y_P2C ( nsz, 0:6, 0:6 ) ); cm1y_P2C(:, :, :) = ZERO
    allocate (dm1y_P2C ( nsz, 0:6, 0:6 ) ); dm1y_P2C(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), m1fP2C, &
          am1y_P2C, bm1y_P2C, cm1y_P2C, dm1y_P2C)
!----------------------------------------------------------------------------------------------------------
!   2nd order deriviative in y direction with nc unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad2y_C2C ( nsz, 0:6, 0:6 ) ); ad2y_C2C(:, :, :) = ZERO
    allocate (bd2y_C2C ( nsz, 0:6, 0:6 ) ); bd2y_C2C(:, :, :) = ZERO
    allocate (cd2y_C2C ( nsz, 0:6, 0:6 ) ); cd2y_C2C(:, :, :) = ZERO
    allocate (dd2y_C2C ( nsz, 0:6, 0:6 ) ); dd2y_C2C(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array( nsz, domain(1)%is_periodic(i), d2fC2C, &
          ad2y_C2C, bd2y_C2C, cd2y_C2C, dd2y_C2C)
!==========================================================================================================
! y-direction, with np unknows
!==========================================================================================================
    nsz = domain(1)%np(i)
!----------------------------------------------------------------------------------------------------------
!   1st derivative in y direction with np unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad1y_P2P ( nsz, 0:6, 0:6 ) ); ad1y_P2P(:, :, :) = ZERO
    allocate (bd1y_P2P ( nsz, 0:6, 0:6 ) ); bd1y_P2P(:, :, :) = ZERO
    allocate (cd1y_P2P ( nsz, 0:6, 0:6 ) ); cd1y_P2P(:, :, :) = ZERO
    allocate (dd1y_P2P ( nsz, 0:6, 0:6 ) ); dd1y_P2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fP2P, &
          ad1y_P2P, bd1y_P2P, cd1y_P2P, dd1y_P2P)

    allocate (ad1y_C2P ( nsz, 0:6, 0:6 ) ); ad1y_C2P(:, :, :) = ZERO
    allocate (bd1y_C2P ( nsz, 0:6, 0:6 ) ); bd1y_C2P(:, :, :) = ZERO
    allocate (cd1y_C2P ( nsz, 0:6, 0:6 ) ); cd1y_C2P(:, :, :) = ZERO
    allocate (dd1y_C2P ( nsz, 0:6, 0:6 ) ); dd1y_C2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fC2P, &
          ad1y_C2P, bd1y_C2P, cd1y_C2P, dd1y_C2P) 
!----------------------------------------------------------------------------------------------------------
!   mid-point interpolation in y direction with np unknows
!----------------------------------------------------------------------------------------------------------
    allocate (am1y_C2P ( nsz, 0:6, 0:6 ) ); am1y_C2P(:, :, :) = ZERO
    allocate (bm1y_C2P ( nsz, 0:6, 0:6 ) ); bm1y_C2P(:, :, :) = ZERO
    allocate (cm1y_C2P ( nsz, 0:6, 0:6 ) ); cm1y_C2P(:, :, :) = ZERO
    allocate (dm1y_C2P ( nsz, 0:6, 0:6 ) ); dm1y_C2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), m1fC2P, &
          am1y_C2P, bm1y_C2P, cm1y_C2P, dm1y_C2P)
!----------------------------------------------------------------------------------------------------------
! 2nd order deriviative in y direction with np unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad2y_P2P ( nsz, 0:6, 0:6 ) ); ad2y_P2P(:, :, :) = ZERO
    allocate (bd2y_P2P ( nsz, 0:6, 0:6 ) ); bd2y_P2P(:, :, :) = ZERO
    allocate (cd2y_P2P ( nsz, 0:6, 0:6 ) ); cd2y_P2P(:, :, :) = ZERO
    allocate (dd2y_P2P ( nsz, 0:6, 0:6 ) ); dd2y_P2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array( nsz, domain(1)%is_periodic(i), d2fP2P, &
          ad2y_P2P, bd2y_P2P, cd2y_P2P, dd2y_P2P)
!==========================================================================================================
! z-direction, with nc unknows
!==========================================================================================================
    i = 3
    nsz = domain(1)%nc(i)
!----------------------------------------------------------------------------------------------------------
!   1st derivative in z direction with nc unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad1z_C2C ( nsz, 0:6, 0:6 ) ); ad1z_C2C(:, : ,:) = ZERO
    allocate (bd1z_C2C ( nsz, 0:6, 0:6 ) ); bd1z_C2C(:, : ,:) = ZERO
    allocate (cd1z_C2C ( nsz, 0:6, 0:6 ) ); cd1z_C2C(:, : ,:) = ZERO
    allocate (dd1z_C2C ( nsz, 0:6, 0:6 ) ); dd1z_C2C(:, : ,:) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fC2C, &
          ad1z_C2C, bd1z_C2C, cd1z_C2C, dd1z_C2C)

    allocate (ad1z_P2C ( nsz, 0:6, 0:6 ) ); ad1z_P2C(:, :, :) = ZERO
    allocate (bd1z_P2C ( nsz, 0:6, 0:6 ) ); bd1z_P2C(:, :, :) = ZERO
    allocate (cd1z_P2C ( nsz, 0:6, 0:6 ) ); cd1z_P2C(:, :, :) = ZERO
    allocate (dd1z_P2C ( nsz, 0:6, 0:6 ) ); dd1z_P2C(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fP2C, &
          ad1z_P2C, bd1z_P2C, cd1z_P2C, dd1z_P2C)
!----------------------------------------------------------------------------------------------------------
!   mid-point interpolation in z direction with nc unknows
!----------------------------------------------------------------------------------------------------------
    allocate (am1z_P2C ( nsz, 0:6, 0:6 ) ); am1z_P2C(:, :, :) = ZERO
    allocate (bm1z_P2C ( nsz, 0:6, 0:6 ) ); bm1z_P2C(:, :, :) = ZERO
    allocate (cm1z_P2C ( nsz, 0:6, 0:6 ) ); cm1z_P2C(:, :, :) = ZERO
    allocate (dm1z_P2C ( nsz, 0:6, 0:6 ) ); dm1z_P2C(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array( nsz, domain(1)%is_periodic(i), m1fP2C, &
          am1z_P2C, bm1z_P2C, cm1z_P2C, dm1z_P2C)

    ! do j = 1, 6
    !   write(*, *) 'bc type= ', j
    !   do k = 1, 5
    !     write(*,*) k, m1fP2C(k, 1, j), m1fP2C(k, 2, j), m1fP2C(k, 3, j)
    !   end do
    ! end do

    ! do j = 1, nsz
    !   write(*, *) 'periodic', j, am1z_P2C ( j, 1, 1 ), &
    !                              bm1z_P2C ( j, 1, 1 ), &
    !                              cm1z_P2C ( j, 1, 1 ), &
    !                              dm1z_P2C ( j, 1, 1 )
    !   write(*, *) 'symmetry', j, am1z_P2C ( j, 2, 2 ), &
    !                              bm1z_P2C ( j, 2, 2 ), &
    !                              cm1z_P2C ( j, 2, 2 ), &
    !                              dm1z_P2C ( j, 2, 2 )
    !   write(*, *) 'dirichle', j, am1z_P2C ( j, 4, 4 ), &
    !                              bm1z_P2C ( j, 4, 4 ), &
    !                              cm1z_P2C ( j, 4, 4 ), &
    !                              dm1z_P2C ( j, 4, 4 )
    ! end do
!----------------------------------------------------------------------------------------------------------
!   2nd order deriviative in z direction with nc unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad2z_C2C ( nsz, 0:6, 0:6 ) ); ad2z_C2C(:, :, :) = ZERO
    allocate (bd2z_C2C ( nsz, 0:6, 0:6 ) ); bd2z_C2C(:, :, :) = ZERO
    allocate (cd2z_C2C ( nsz, 0:6, 0:6 ) ); cd2z_C2C(:, :, :) = ZERO
    allocate (dd2z_C2C ( nsz, 0:6, 0:6 ) ); dd2z_C2C(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array( nsz, domain(1)%is_periodic(i), d2fC2C, &
          ad2z_C2C, bd2z_C2C, cd2z_C2C, dd2z_C2C)
!==========================================================================================================
! z-direction, with np unknows
!==========================================================================================================
    nsz = domain(1)%np(i)
!----------------------------------------------------------------------------------------------------------
! 1st derivative in z direction with np unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad1z_P2P ( nsz, 0:6, 0:6 ) ); ad1z_P2P(:, :, :) = ZERO
    allocate (bd1z_P2P ( nsz, 0:6, 0:6 ) ); bd1z_P2P(:, :, :) = ZERO
    allocate (cd1z_P2P ( nsz, 0:6, 0:6 ) ); cd1z_P2P(:, :, :) = ZERO
    allocate (dd1z_P2P ( nsz, 0:6, 0:6 ) ); dd1z_P2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fP2P, &
          ad1z_P2P, bd1z_P2P, cd1z_P2P, dd1z_P2P)

    allocate (ad1z_C2P ( nsz, 0:6, 0:6 ) ); ad1z_C2P(:, :, :) = ZERO
    allocate (bd1z_C2P ( nsz, 0:6, 0:6 ) ); bd1z_C2P(:, :, :) = ZERO
    allocate (cd1z_C2P ( nsz, 0:6, 0:6 ) ); cd1z_C2P(:, :, :) = ZERO
    allocate (dd1z_C2P ( nsz, 0:6, 0:6 ) ); dd1z_C2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array(nsz, domain(1)%is_periodic(i), d1fC2P, &
          ad1z_C2P, bd1z_C2P, cd1z_C2P, dd1z_C2P)
!----------------------------------------------------------------------------------------------------------
! mid-point interpolation in z direction with np unknows
!----------------------------------------------------------------------------------------------------------
    allocate (am1z_C2P ( nsz, 0:6, 0:6 ) ); am1z_C2P(:, :, :) = ZERO
    allocate (bm1z_C2P ( nsz, 0:6, 0:6 ) ); bm1z_C2P(:, :, :) = ZERO
    allocate (cm1z_C2P ( nsz, 0:6, 0:6 ) ); cm1z_C2P(:, :, :) = ZERO
    allocate (dm1z_C2P ( nsz, 0:6, 0:6 ) ); dm1z_C2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array( nsz, domain(1)%is_periodic(i), m1fC2P, &
          am1z_C2P, bm1z_C2P, cm1z_C2P, dm1z_C2P)
!----------------------------------------------------------------------------------------------------------
! 2nd order deriviative in z direction with np unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad2z_P2P ( nsz, 0:6, 0:6 ) ); ad2z_P2P(:, :, :) = ZERO
    allocate (bd2z_P2P ( nsz, 0:6, 0:6 ) ); bd2z_P2P(:, :, :) = ZERO
    allocate (cd2z_P2P ( nsz, 0:6, 0:6 ) ); cd2z_P2P(:, :, :) = ZERO
    allocate (dd2z_P2P ( nsz, 0:6, 0:6 ) ); dd2z_P2P(:, :, :) = ZERO
    call Buildup_TDMA_LHS_array( nsz, domain(1)%is_periodic(i), d2fP2P, &
        ad2z_P2P, bd2z_P2P, cd2z_P2P, dd2z_P2P)
!==========================================================================================================
! x-direction
!==========================================================================================================
    allocate ( xtdma_lhs (nxdomain) )
    do i = 1, nxdomain
!==========================================================================================================
! x-direction, with nc unknows
!==========================================================================================================
      nsz = domain(i)%nc(1)
!----------------------------------------------------------------------------------------------------------
! 1st derivative in x direction with nc unknows
!----------------------------------------------------------------------------------------------------------
      allocate (xtdma_lhs(i)%ad1x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%ad1x_C2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bd1x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bd1x_C2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cd1x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cd1x_C2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dd1x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dd1x_C2C(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array(nsz, domain(i)%is_periodic(1), d1fC2C, &
            xtdma_lhs(i)%ad1x_C2C, &
            xtdma_lhs(i)%bd1x_C2C, &
            xtdma_lhs(i)%cd1x_C2C, &
            xtdma_lhs(i)%dd1x_C2C)
  
      allocate (xtdma_lhs(i)%ad1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%ad1x_P2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bd1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bd1x_P2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cd1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cd1x_P2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dd1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dd1x_P2C(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array(nsz, domain(i)%is_periodic(1), d1fP2C, &
            xtdma_lhs(i)%ad1x_P2C, &
            xtdma_lhs(i)%bd1x_P2C, &
            xtdma_lhs(i)%cd1x_P2C, &
            xtdma_lhs(i)%dd1x_P2C)
!----------------------------------------------------------------------------------------------------------
! 2nd order deriviative in x direction with nc unknows
!----------------------------------------------------------------------------------------------------------
      allocate (xtdma_lhs(i)%ad2x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%ad2x_C2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bd2x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bd2x_C2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cd2x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cd2x_C2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dd2x_C2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dd2x_C2C(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array( nsz, domain(i)%is_periodic(1), d2fC2C, &
          xtdma_lhs(i)%ad2x_C2C, &
          xtdma_lhs(i)%bd2x_C2C, &
          xtdma_lhs(i)%cd2x_C2C, &
          xtdma_lhs(i)%dd2x_C2C)
!----------------------------------------------------------------------------------------------------------
! mid-point interpolation in x direction with nc unknows
!----------------------------------------------------------------------------------------------------------
      allocate (xtdma_lhs(i)%am1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%am1x_P2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bm1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bm1x_P2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cm1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cm1x_P2C(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dm1x_P2C ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dm1x_P2C(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array(nsz, domain(i)%is_periodic(1), m1fP2C, &
          xtdma_lhs(i)%am1x_P2C, &
          xtdma_lhs(i)%bm1x_P2C, &
          xtdma_lhs(i)%cm1x_P2C, &
          xtdma_lhs(i)%dm1x_P2C)      
!==========================================================================================================
! x-direction, with np unknows
!==========================================================================================================
      nsz = domain(i)%np(1)
!----------------------------------------------------------------------------------------------------------
! 1st derivative in x direction with np unknows
!----------------------------------------------------------------------------------------------------------
      allocate (xtdma_lhs(i)%ad1x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%ad1x_P2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bd1x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bd1x_P2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cd1x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cd1x_P2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dd1x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dd1x_P2P(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array(nsz, domain(i)%is_periodic(1), d1fP2P, &
            xtdma_lhs(i)%ad1x_P2P, &
            xtdma_lhs(i)%bd1x_P2P, &
            xtdma_lhs(i)%cd1x_P2P, &
            xtdma_lhs(i)%dd1x_P2P)
  
      allocate (xtdma_lhs(i)%ad1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%ad1x_C2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bd1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bd1x_C2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cd1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cd1x_C2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dd1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dd1x_C2P(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array(nsz, domain(i)%is_periodic(1), d1fC2P, &
            xtdma_lhs(i)%ad1x_C2P, &
            xtdma_lhs(i)%bd1x_C2P, &
            xtdma_lhs(i)%cd1x_C2P, &
            xtdma_lhs(i)%dd1x_C2P)
!----------------------------------------------------------------------------------------------------------
! 2nd order deriviative in x direction with np unknows
!----------------------------------------------------------------------------------------------------------
      allocate (xtdma_lhs(i)%ad2x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%ad2x_P2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bd2x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bd2x_P2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cd2x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cd2x_P2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dd2x_P2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dd2x_P2P(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array( nsz, domain(i)%is_periodic(1), d2fP2P, &
            xtdma_lhs(i)%ad2x_P2P, &
            xtdma_lhs(i)%bd2x_P2P, &
            xtdma_lhs(i)%cd2x_P2P, &
            xtdma_lhs(i)%dd2x_P2P)
!----------------------------------------------------------------------------------------------------------
! mid-point interpolation in x direction with np unknows
!----------------------------------------------------------------------------------------------------------
      allocate (xtdma_lhs(i)%am1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%am1x_C2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%bm1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%bm1x_C2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%cm1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%cm1x_C2P(:, :, :) = ZERO
      allocate (xtdma_lhs(i)%dm1x_C2P ( nsz, 0:6, 0:6 ) ); xtdma_lhs(i)%dm1x_C2P(:, :, :) = ZERO
      call Buildup_TDMA_LHS_array(nsz, domain(i)%is_periodic(1), m1fC2P, &
          xtdma_lhs(i)%am1x_C2P, &
          xtdma_lhs(i)%bm1x_C2P, &
          xtdma_lhs(i)%cm1x_C2P, &
          xtdma_lhs(i)%dm1x_C2P)      

    end do

    return
  end subroutine Prepare_LHS_coeffs_for_operations
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for interpolation.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the interpolation.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       given pencil     needed         specified   private
!----------------------------------------------------------------------------------------------------------
!>  index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! interpolation. P2C: periodic
! [ 1     alpha                       alpha] [f_1] = [a/2 * ( f_{1'}   + f_{2'}   ) + b/2 * ( f_{3'}   + f_{n'}   ) ]
! [       alpha  1      alpha              ] [f_2]   [a/2 * ( f_{2'}   + f_{3'}   ) + b/2 * ( f_{4'}   + f_{1'}   ) ]
! [              alpha  1      alpha       ] [f_i]   [a/2 * ( f_{i'}   + f_{i'+1} ) + b/2 * ( f_{i'+2} + f_{i'-1} ) ]
! [                     alpha  1      alpha] [f_4]   [a/2 * ( f_{n'-1} + f_{n'}   ) + b/2 * ( f_{1'}   + f_{n'-2} ) ]
! [alpha                       alpha  1    ] [f_5]   [a/2 * ( f_{n'}   + f_{1'}   ) + b/2 * ( f_{2'}   + f_{n'-1} ) ]
!----------------------------------------------------------------------------------------------------------
! interpolation. P2C: Dirichlet
! [ 1     alpha1                              ] [f_1] = [a1 *  f_{1'}   + b1 * f_{2'} + c1 * f_{3'}                       ]
! [alpha2 1      alpha2                       ] [f_2]   [a2/2 * ( f_{2'}   + f_{3'}   )                                 ] 
! [              alpha  1       alpha         ] [f_i]   [a/2 * ( f_{i'}   + f_{i'+1} ) + b/2 * ( f_{i'+2} + f_{i'-1} ) ]
! [                     alpha2  1       alpha2] [f_4]   [a2/2 * ( f_{n'-1} + f_{n'}   )                                 ]
! [                             alpha1  1     ] [f_5]   [a1 *  f_{n'+1}   + b1 * f_{n'}  + c1 * f_{n'-1}                  ]
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is nc
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_interp_P2C_RHS_array(fi, fo, nc, coeff, ibc, fbc)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: nc ! unknow numbers, nc
    real(WP), intent(out) :: fo(nc)
    real(WP), intent(in ) :: coeff(5, 4, 0:6)
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: i, m, l

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, nc - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i - 1) + fi(i + 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for either perirodic or non-periodic of nc
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(nc'-1)-(nc-1)-(nc')-(nc)-(nc'+1)|-(nc+1)-(nc'+2)---
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_interp_P2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(1   ) + fi(i + 2) )
    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0' = nc = np
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(nc   ) + fi(i + 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0' = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(2    ) + fi(i + 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0' = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(2    ) + fi(i + 2) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    l = 2
    fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i + 1) ) + &
            coeff( l, 2, IBC_PERIODIC ) * ( fi(i - 1) + fi(i + 2) )
!----------------------------------------------------------------------------------------------------------
!   i = nc - 1
!----------------------------------------------------------------------------------------------------------
    i = nc - 1
    m = 2
    l = 4
    if ( ibc(m) == IBC_PERIODIC) then
      ! i + 2 = nc' + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i - 1) + fi(1    ) )
    else
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i - 1) + fi(i + 2) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = nc
!----------------------------------------------------------------------------------------------------------
    i = nc
    m = 2
    l = 5
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_interp_P2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i - 1) + fbc(2   ) )
    else if ( ibc(m) == IBC_PERIODIC) then
      ! i + 1 = nc' + 1 = 1
      ! i + 2 = nc' + 2 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i - 1) + fi(2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! i + 2 = nc' + 2 = nc'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i - 1) + fi(nc   ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! i + 2 = nc' + 2 = nc'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i - 1) - fi(nc   ) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 1) 
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    ! nothing.
    return
  end subroutine Prepare_TDMA_interp_P2C_RHS_array

!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for interpolation.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the interpolation.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   private
!----------------------------------------------------------------------------------------------------------
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! interpolation. C2P: periodic
! [ 1    alpha                   alpha][f_1']=[a/2 * (f_{1}   + f_{n})   + b/2 * (f_{2}   + f_{n-1})]
! [      alpha 1     alpha            ][f_2'] [a/2 * (f_{2}   + f_{1})   + b/2 * (f_{3}   + f_{n})  ]
! [            alpha 1     alpha      ][f_i'] [a/2 * (f_{i}   + f_{i-1}) + b/2 * (f_{i+1} + f_{i-2})]
! [                  alpha 1     alpha][f_4'] [a/2 * (f_{n-1} + f_{n-2}) + b/2 * (f_{n}   + f_{n-3})]
! [alpha                   alpha 1    ][f_5'] [a/2 * (f_{1}   + f_{n-2}) + b/2 * (f_{n}   + f_{n-1})]
!----------------------------------------------------------------------------------------------------------
! interpolation. C2P: Dirichlet
! [ 1    alpha1                          ][f_1']=[a1 * f_{1} + b1 * f_{2} + c1 * f_{3}  ]
! [      alpha2 1     alpha2             ][f_2'] [a2/2 * (f_{2}   + f_{1})]
! [             alpha 1      alpha       ][f_i'] [a/2 * (f_{i}   + f_{i-1}) + b/2 * (f_{i+1} + f_{i-2})]
! [                   alpha2 1     alpha2][f_4'] [a2/2 * (f_{n-1}   + f_{n-2})]
! [                          alpha1 1    ][f_5'] [a1 * f_{n-1} + b1 * f_{n-2} + c1 * f_{n-3}]
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is np
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_interp_C2P_RHS_array(fi, fo, np, coeff, ibc, fbc)
    use parameters_constant_mod
    implicit none

    real(WP),           intent(in ) :: fi(:)
    integer,            intent(in ) :: np ! unknow numbers, np
    real(WP),           intent(out) :: fo(np)
    real(WP),           intent(in ) :: coeff(5, 4, 0:6)
    integer,            intent(in ) :: ibc(2)
    real(WP), optional, intent(in)  :: fbc(4) ! used for Dirichlet B.C. (1||2) & interior (3, 1,|| 2, 4)

    integer :: i, m, l

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, np - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 1) + fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for non-periodic: 
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np'|)-(np)-(np'+1)-(np+1)-(np'+2)---
!   for periodic, nc = np 
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np')-(np)-(np'+1|)-(np+1)-(np'+2)---
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_interp_C2P_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fbc(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fbc(3) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = np
      !-1 = np - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(np    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fi(np - 1) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      !-1 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fi(2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fi(2) )
    else if (ibc(m) == IBC_DIRICHLET) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_interp_C2P_RHS_array')
      fo(i) = fbc(m)
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    m = 1
    l = 2

    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_interp_C2P_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fbc(1) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = np
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fi(np   ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fi(1    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fi(1    ) )
    else if (ibc(m) == IBC_DIRICHLET) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i - 1) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i    ) + fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np - 1
!----------------------------------------------------------------------------------------------------------
    i = np - 1
    m = 2
    l = 4
    
    if ( ibc(m) == IBC_INTERIOR) then

      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_interp_C2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i)  + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(2) + fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      !
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i     ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1 ) + fi(i - 2 ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! np     = np - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i     ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np - 1) + fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i     ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np - 1) + fi(i - 2) )
    else if (ibc(m) == IBC_DIRICHLET) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i     ) + fi(i - 1) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i     ) + fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np
!----------------------------------------------------------------------------------------------------------
    i = np
    m = 2
    l = 5
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_interp_C2P_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fbc(2   ) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(4   ) + fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! np + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(i - 1 ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(1    ) + fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! np     = np - 1
      ! np + 1 = np - 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(np - 1) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np - 2) + fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * (-fi(np - 1) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np - 2) + fi(i - 2) )
    else if (ibc(m) == IBC_DIRICHLET) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_interp_C2P_RHS_array')
      fo(i) = fbc(m)
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 2) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 3) 
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    ! nothing
    return
  end subroutine Prepare_TDMA_interp_C2P_RHS_array
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the 1st derivative.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   private
!----------------------------------------------------------------------------------------------------------
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! 1st derivative on collocated grids, C2C/P2P coefficients : Periodic or Symmetric B.C.
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
! 1st-derivative. C2C or P2P: periodic
! [ 1    alpha                   alpha][f'_1]=[a/2 * (f_{2}   - f_{n})/h   + b/4 * (f_{3}   - f_{n-1})/h]
! [      alpha 1     alpha            ][f'_2] [a/2 * (f_{3}   - f_{1})/h   + b/4 * (f_{4}   - f_{n})/h  ]
! [            alpha 1     alpha      ][f'_i] [a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                  alpha 1     alpha][f'_4] [a/2 * (f_{n}   - f_{n-2})/h + b/4 * (f_{1}   - f_{n-3})/h]
! [alpha                   alpha 1    ][f'_5] [a/2 * (f_{1}   - f_{n-1})/h + b/4 * (f_{2}   - f_{n-2})/h]
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : C2C or P2P : Dirichlet B.C.
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1}/h  + b1 * f_{2}/h + c1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2/2 * (f_{3} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i] [ a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4] [a2/2 * (f_{n} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n}/h  - b1 * f_{n-1}/h - c1 * f_{n-2}/h]
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is np
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     dd            1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_C2C_RHS_array(fi, fo, nc, coeff, dd, ibc, fbc)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: nc ! unknow numbers
    real(WP), intent(out) :: fo(nc)
    real(WP), intent(in ) :: coeff(5, 4, 0:6)
    real(WP), intent(in ) :: dd
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in)  :: fbc(4) ! used for Dirichlet B.C. or interior

    integer :: i, m, l

    fo(:) = ZERO


!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, nc - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for non-periodic or periodic
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(nc'-1)-(nc-1)-(nc')-(nc)-(nc'+1|)-(nc+1)-(nc'+2)---
!  interior bc
!    3-1|| ---||2-4
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2C_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fbc(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fbc(3) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = nc
      !-1 = nc - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(nc   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(nc -1) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      !-1 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(1   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(2   ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0 = 1
      !-1 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) + fi(1   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) + fi(2   ) )
    else if (ibc(m) == IBC_DIRICHLET) then
      if(present(fbc)) then !call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_1deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * fbc(m ) + &
              coeff( l, 2, ibc(m) ) * fi(i  )  + &
              coeff( l, 3, ibc(m) ) * fi(i+1)  + &
              coeff( l, 4, ibc(m) ) * fi(i+2) 
      else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
      end if
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    m = 1
    l = 2
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2C_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fbc(1) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = nc
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(nc   ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(1    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) + fi(1    ) )
    else if (ibc(m) == IBC_DIRICHLET) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np - 1
!----------------------------------------------------------------------------------------------------------
    i = nc - 1
    m = 2
    l = 4
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2C_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(2)    - fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      !i + 2 = nc + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(1    ) - fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      !i + 2 = nc + 1 = nc
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(nc   ) - fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      !i + 2 = nc + 1 = nc
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(nc   ) - fi(i - 2) )
    else if (ibc(m) == IBC_DIRICHLET) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = nc
!----------------------------------------------------------------------------------------------------------
    i = nc
    m = 2
    l = 5
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2C_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fbc(2) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(4) - fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! nc + 1 = 1
      ! nc + 2 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(1    ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(2    ) - fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! nc + 1 = nc
      ! nc + 2 = nc - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(nc   ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(nc -1) - fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! nc + 1 = nc
      ! nc + 2 = nc - 1
      fo(i) = coeff( l, 1, ibc(m) ) * (-fi(nc   ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(nc -1) - fi(i - 2) )
    else if (ibc(m) == IBC_DIRICHLET) then
      if(present(fbc)) then!call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_1deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * fbc(m   ) + &
              coeff( l, 2, ibc(m) ) * fi(i    ) + &
              coeff( l, 3, ibc(m) ) * fi(i - 1) + &
              coeff( l, 4, ibc(m) ) * fi(i - 2) 
      else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 2) 
      end if
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd

    return
  end subroutine Prepare_TDMA_1deri_C2C_RHS_array
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the 1st derivative.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   private
!----------------------------------------------------------------------------------------------------------
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! 1st derivative on collocated grids, C2C/P2P coefficients : Periodic or Symmetric B.C.
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
! 1st-derivative. C2C or P2P: periodic
! [ 1    alpha                   alpha][f'_1]=[a/2 * (f_{2}   - f_{n})/h   + b/4 * (f_{3}   - f_{n-1})/h]
! [      alpha 1     alpha            ][f'_2] [a/2 * (f_{3}   - f_{1})/h   + b/4 * (f_{4}   - f_{n})/h  ]
! [            alpha 1     alpha      ][f'_i] [a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                  alpha 1     alpha][f'_4] [a/2 * (f_{n}   - f_{n-2})/h + b/4 * (f_{1}   - f_{n-3})/h]
! [alpha                   alpha 1    ][f'_5] [a/2 * (f_{1}   - f_{n-1})/h + b/4 * (f_{2}   - f_{n-2})/h]
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : C2C or P2P : Dirichlet B.C.
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1}/h  + b1 * f_{2}/h + c1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2/2 * (f_{3} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i] [ a/2 * (f_{i+1} - f_{i-1})/h + b/4 * (f_{i+2} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4] [a2/2 * (f_{n} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n}/h  - b1 * f_{n-1}/h - c1 * f_{n-2}/h]
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is np
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     dd            1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_P2P_RHS_array(fi, fo, np,coeff, dd, ibc, fbc)
    use parameters_constant_mod
    implicit none
    real(WP),           intent(in ) :: fi(:)
    integer,            intent(in ) :: np ! unknow numbers
    real(WP),           intent(out) :: fo(np)
    real(WP),           intent(in ) :: coeff(5, 4, 0:6)
    real(WP),           intent(in ) :: dd
    integer,            intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: fbc(4) ! used for IBC_NEUMANN or interior

    integer :: m, l, i

    fo(:) = ZERO

!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, np - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for periodic
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np')-(np)-(np'+1|)-(np+1)-(np'+2)---
!   for non-periodic
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np'|)-(np)-(np'+1)-(np+1)-(np'+2)---
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_P2P_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fbc(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fbc(3) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0' = np'
      !-1' = np' - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(np   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(np -1) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0' = 2'
      !-1' = 3'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(2   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(3   ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0' = 2'
      !-1' = 3'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) + fi(2   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) + fi(3   ) )
    else if (ibc(m) == IBC_NEUMANN) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN')
      fo(i) = fbc(m)
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    m = 1
    l = 2
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_P2P_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fbc(1) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0' = np'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(np   ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0' = 2'
      !write(*,*) 'bcp2p1', coeff( l, 1:2, ibc(m) )
      !write(*,*) coeff( l, 1, ibc(m) ), fi(i + 1), fi(i - 1), coeff( l, 2, ibc(m) ), fi(i + 2), fi(2    )
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(2    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0' = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) + fi(2    ) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np - 1
!----------------------------------------------------------------------------------------------------------
    i = np - 1
    m = 2
    l = 4
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_P2P_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(2)    - fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      !np' + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(1    ) - fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      !np' + 1 = np' - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np- 1) - fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      !np' + 1 = np' - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np- 1) - fi(i - 2) )
    else 
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np
!----------------------------------------------------------------------------------------------------------
    i = np
    m = 2
    l = 5
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_P2P_RHS_array')
      
      fo(i) = coeff( l, 1, ibc(m) ) * ( fbc(2) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(4) - fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! np' + 1 = 1'
      ! np' + 2 = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(1    ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(2    ) - fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! np' + 1 = np' - 1
      ! np' + 2 = np' - 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(np -1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np -2) - fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! np' + 1 = np' - 1
      ! np' + 2 = np' - 2
      fo(i) = coeff( l, 1, ibc(m) ) * (-fi(np -1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np -2) - fi(i - 2) )
    else if (ibc(m) == IBC_NEUMANN) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN')
      fo(i) = fbc(m)
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd
!----------------------------------------------------------------------------------------------------------
!   direct bc, correction
!----------------------------------------------------------------------------------------------------------
    do m = 1, 2
      if (ibc(m) == IBC_NEUMANN) then
        if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN')
        if(m == 1) fo(1 ) = fbc(m)
        if(m == 2) fo(np) = fbc(m)
      end if
    end do

    return
  end subroutine Prepare_TDMA_1deri_P2P_RHS_array
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the 1st derivative.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   private
!----------------------------------------------------------------------------------------------------------
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! 1st derivative on staggered grids C2P
! C2P ==>
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(h ) * ( f_{i}   - f_{i-1} ) + &
!                                                 b/(3h) * ( f_{i+1} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
! 1st-derivative. C2P: periodic
! [ 1    alpha                   alpha][f_1']=[a/2 * (f_{1}   + f_{n})   + b/2 * (f_{2}   + f_{n-1})]
! [      alpha 1     alpha            ][f_2'] [a/2 * (f_{2}   + f_{1})   + b/2 * (f_{3}   + f_{n})  ]
! [            alpha 1     alpha      ][f_i'] [a/2 * (f_{i}   + f_{i-1}) + b/2 * (f_{i+1} + f_{i-2})]
! [                  alpha 1     alpha][f_4'] [a/2 * (f_{n-1} + f_{n-2}) + b/2 * (f_{n}   + f_{n-3})]
! [alpha                   alpha 1    ][f_5'] [a/2 * (f_{1}   + f_{n-2}) + b/2 * (f_{n}   + f_{n-1})]
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : C2P : Dirichlet B.C.
! [ 1     alpha1                            ][f'_1']=[a1 * f_{1}/h  + b1 * f_{2}/h + c1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2'] [a2 * (f_{2} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i'] [a *  (f_{i} - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4'] [a2 * (f_{n-1} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5'] [-a1 * f_{n-1}/h  - b1 * f_{n-2}/h - c1 * f_{n-3}/h]
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is np
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     dd            1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_C2P_RHS_array(fi, fo, np, coeff, dd, ibc, fbc)
    use parameters_constant_mod
    implicit none
    real(WP),           intent(in ) :: fi(:)
    integer,            intent(in ) :: np ! unknow numbers, np
    real(WP),           intent(out) :: fo(np)
    real(WP),           intent(in ) :: coeff(5, 4, 0:6)
    real(WP),           intent(in ) :: dd
    integer,            intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: fbc(4) ! used for IBC_NEUMANN, and interior

    integer :: i, m, l

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, np - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i    ) - fi(i - 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 1) - fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for non-periodic: 
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np'|)-(np)-(np'+1)-(np+1)-(np'+2)---
!   for periodic:
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np')-(np)-(np'+1|)-(np+1)-(np'+2)---
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fbc(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fbc(3) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = np
      !-1 = np - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(np    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fi(np - 1) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      !-1 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fi(2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) + fi(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fi(2) )
    else if (ibc(m) == IBC_NEUMANN) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN @ Prepare_TDMA_1deri_C2P_RHS_array')
      fo(i) = fbc(m)
    else if (ibc(m) == IBC_DIRICHLET) then
      if( present(fbc)) then !call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_1deri_C2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * fbc(m)    + &
              coeff( l, 2, ibc(m) ) * fi(i    ) + &
              coeff( l, 3, ibc(m) ) * fi(i + 1) + &
              coeff( l, 4, ibc(m) ) * fi(i + 2) 
      else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
      end if
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    m = 1
    l = 2
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fbc(1) )
              
    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = np
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fi(np   ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) - fi(1    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1) + fi(1    ) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i    ) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np - 1
!----------------------------------------------------------------------------------------------------------
    i = np - 1
    m = 2
    l = 4
    
    if ( ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(2) - fi(i - 2 ) )
    
    else if ( ibc(m) == IBC_PERIODIC ) then
      !
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i     ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 1 ) - fi(i - 2 ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! np     = np - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i     ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np - 1) - fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i     ) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np - 1) - fi(i - 2) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i     ) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np
!----------------------------------------------------------------------------------------------------------
    i = np
    m = 2
    l = 5
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_C2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fbc(2) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(4) - fi(i - 2) )
              
    else if ( ibc(m) == IBC_PERIODIC) then
      ! np + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i    ) - fi(i - 1 ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(1    ) - fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! np     = np - 1
      ! np + 1 = np - 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(np - 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np - 2) - fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      fo(i) = coeff( l, 1, ibc(m) ) * (-fi(np - 1) - fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np - 2) - fi(i - 2) )
    else if (ibc(m) == IBC_NEUMANN) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN')
      fo(i) = fbc(m)
    else if (ibc(m) == IBC_DIRICHLET) then
      if(present(fbc)) then !call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_1deri_C2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m)) * fbc(m)    + &
              coeff( l, 2, ibc(m)) * fi(i - 1) + &
              coeff( l, 3, ibc(m)) * fi(i - 2) + &
              coeff( l, 4, ibc(m)) * fi(i - 3) 
      else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 2) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 3) 
      end if
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 2) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 3) 
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd
!----------------------------------------------------------------------------------------------------------
!   direct bc, correction
!----------------------------------------------------------------------------------------------------------
    do m = 1, 2
      if (ibc(m) == IBC_NEUMANN) then
        if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN')
        if(m == 1) fo(1 ) = fbc(m)
        if(m == 2) fo(np) = fbc(m)
      end if
    end do

    return
  end subroutine Prepare_TDMA_1deri_C2P_RHS_array

!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the 1st derivative.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   private
!----------------------------------------------------------------------------------------------------------
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
! 1st derivative on staggered grids P2C and C2P : Periodic or Symmetric B.C.
! P2C ==>
! alpha * f'_{i-1} +  f'_i +  alpha * f'_{i+1}  = a/(h ) * ( f_{i'+1} - f_{i'} ) + &
!                                                 b/(3h) * ( f_{i'+2} - f_{i'-1} )
!----------------------------------------------------------------------------------------------------------
! 1st-derivative. P2C: Dirichlet
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1'}/h  + b1 * f_{2'}/h + c1 * f_{3'}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2 * (f_{3'} - f_{2'})/h  ]
! [       alpha  1      alpha               ][f'_i] [a *  (f_{i'+1} - f_{i'})/h + b/3 * (f_{i'+2} - f_{i'-1})/h]
! [                     alpha2 1      alpha2][f'_4] [a2 * (f_{n'} - f_{n'-1})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n'+1}/h  - b1 * f_{n'}/h - c1 * f_{n'-1}/h]
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is np
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     dd            1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_P2C_RHS_array(fi, fo, nc, coeff, dd, ibc, fbc)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: nc ! unknow numbers, nc
    real(WP), intent(out) :: fo(nc)
    real(WP), intent(in ) :: coeff(5, 4, 0:6)
    real(WP), intent(in ) :: dd
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: i, l, m

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, nc - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 1) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for either perirodic or non-periodic of nc
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(nc'-1)-(nc-1)-(nc')-(nc)-(nc'+1)|-(nc+1)-(nc'+2)---
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_P2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fbc(1)    )
              
    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0' = nc
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(nc   ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0' = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - fi(2    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0' = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) -  fi(i    )) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) +  fi(2    ))
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) 
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    l = 2
    fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i    ) ) + &
            coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 1) )
!----------------------------------------------------------------------------------------------------------
!   i = nc - 1
!----------------------------------------------------------------------------------------------------------
    i = nc - 1
    m = 2
    l = 4
    if ( ibc(m) == IBC_PERIODIC) then
      ! i + 2 = nc' + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(1    ) - fi(i - 1) )
    else
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = nc
!----------------------------------------------------------------------------------------------------------
    i = nc
    m = 2
    l = 5
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_1deri_P2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(2)    - fi(i - 1) )
              
    else if ( ibc(m) == IBC_PERIODIC) then
      ! i + 1 = nc' + 1 = 1
      ! i + 2 = nc' + 2 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(1    ) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(2    ) - fi(i - 1) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! i + 2 = nc' + 2 = nc'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(nc   ) - fi(i - 1) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! i + 2 = nc' + 2 = nc'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(nc   ) - fi(i - 1) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 1) 
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd

    return
  end subroutine Prepare_TDMA_1deri_P2C_RHS_array
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the 1st derivative.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   private
!----------------------------------------------------------------------------------------------------------
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! 2nd derivative on collocated grids, C2C/P2P coefficients : Periodic or Symmetric B.C.
! alpha * f"_{i-1} + f"_i + alpha * f"_{i+1} = a/(2h) * ( f_{i+1} - 2f_{i} + f(i-1) ) + &
!                                              b/(4h) * ( f_{i+2} - 2f_{i} + f_{i-2} )
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is nc
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     dd            1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_2deri_C2C_RHS_array(fi, fo, nc, coeff, dd, ibc, fbc)
    use parameters_constant_mod
    implicit none
    real(WP),           intent(in ) :: fi(:)
    integer,            intent(in ) :: nc ! unknow numbers
    real(WP),           intent(out) :: fo(nc)
    real(WP),           intent(in ) :: coeff(5, 4, 0:6)
    real(WP),           intent(in ) :: dd
    integer,            intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: i, l, m

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, nc - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 2) - TWO * fi(i) + fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for non-periodic or periodic
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(nc'-1)-(nc-1)-(nc')-(nc)-(nc'+1|)-(nc+1)-(nc'+2)---
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fbc(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fbc(3) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = nc
      !-1 = nc - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(nc   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(nc- 1) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      !-1 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(1    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(2    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0 = 1
      !-1 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) - fi(1    ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) - fi(2    ) )
    else if (ibc(m) == IBC_DIRICHLET) then
      if(present(fbc)) then!call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_2deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * fbc(m ) + &
              coeff( l, 2, ibc(m) ) * fi(i  )  + &
              coeff( l, 3, ibc(m) ) * fi(i+1)  + &
              coeff( l, 4, ibc(m) ) * fi(i+2) 
      else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( l, 4, IBC_INTRPL) * fi(i + 3) 
      end if
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( l, 4, IBC_INTRPL) * fi(i + 3) 
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    m = 1
    l = 2
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fbc(1) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0 = nc
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(nc   ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(1    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) - fi(1    ) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np - 1
!----------------------------------------------------------------------------------------------------------
    i = nc - 1
    m = 2
    l = 4
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(2)    - TWO * fi(i) + fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      !nc + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(1    ) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      !i + 2 = nc + 1 = nc
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(nc   ) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      !i + 2 = nc + 1 = nc
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(nc   ) - TWO * fi(i) + fi(i - 2) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = nc
!----------------------------------------------------------------------------------------------------------
    i = nc
    m = 2
    l = 5
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fbc(2) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(4) - TWO * fi(i) + fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! nc + 1 = 1
      ! nc + 2 = 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(1    ) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(2    ) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! nc + 1 = nc
      ! nc + 2 = nc - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(nc   ) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(nc- 1) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! nc + 1 = nc
      ! nc + 2 = nc - 1
      fo(i) = coeff( l, 1, ibc(m) ) * (-fi(nc   ) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(nc- 1) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_DIRICHLET) then
      if(present(fbc)) then!call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ Prepare_TDMA_2deri_C2C_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * fbc(m   ) + &
              coeff( l, 2, ibc(m) ) * fi(i    ) + &
              coeff( l, 3, ibc(m) ) * fi(i - 1) + &
              coeff( l, 4, ibc(m) ) * fi(i - 2) 
      else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 2) + &
              coeff( l, 4, IBC_INTRPL) * fi(i - 3)  
      end if
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 2) + &
              coeff( l, 4, IBC_INTRPL) * fi(i - 3)  
    end if

!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd ! dd = (1/dx)^2

    return
  end subroutine Prepare_TDMA_2deri_C2C_RHS_array
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> for the 1st derivative.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   private
!----------------------------------------------------------------------------------------------------------
!> Y: index arrangment
!>      j'-1   j'-1  j'    j'+1  j'+2
!>      _|__.__|__.__|__.__|__.__|__.__
!>         j-2   j-1   j     j+1    j+2
!----------------------------------------------------------------------------------------------------------
! 2nd derivative on collocated grids, C2C/P2P coefficients : Periodic or Symmetric B.C.
! alpha * f"_{i-1} + f"_i + alpha * f"_{i+1} = a/(2h) * ( f_{i+1} - 2f_{i} + f(i-1) ) + &
!                                              b/(4h) * ( f_{i+2} - 2f_{i} + f_{i-2} )
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     n             the number of unknowns, here is nc
!> \param[in]    ibc            the b.c. type at two ends of the unknown array
!> \param[in]    fbc            the b.c. values for the given ibc
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     coeff         the defined TDMA coefficients
!> \param[in]     dd            1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]     fi            the input variable to build up the RHS array
!> \param[out]    fo            the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_2deri_P2P_RHS_array(fi, fo, np, coeff, dd, ibc, fbc)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: np ! unknow numbers
    real(WP), intent(out) :: fo(np)
    real(WP), intent(in ) :: coeff(5, 4, 0:6)
    real(WP), intent(in ) :: dd
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer  :: i, l, m

    fo(:) = ZERO


!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    l = 3
    do i = 3, np - 2
      fo(i) = coeff( l, 1, IBC_PERIODIC ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, IBC_PERIODIC ) * ( fi(i + 2) - TWO * fi(i) + fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!   for periodic
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np')-(np)-(np'+1|)-(np+1)-(np'+2)---
!   for non-periodic
!   ---(-1')-(-1)-(0')-(0)-(|1')-(1)-(2')-(2)-(3')---(i')---(np'-1)-(np-1)-(np'|)-(np)-(np'+1)-(np+1)-(np'+2)---
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
!   i = 1
!----------------------------------------------------------------------------------------------------------
    i = 1
    m = 1
    l = 1
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_P2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fbc(1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fbc(3) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0' = np'
      !-1' = np' - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(np   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(np- 1) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0' = 2'
      !-1' = 3'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(2   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(3   ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0' = 2'
      !-1' = 3'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) - fi(2   ) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) - fi(3   ) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( l, 4, IBC_INTRPL) * fi(i + 3)  
    end if
!----------------------------------------------------------------------------------------------------------
!   i = 2
!----------------------------------------------------------------------------------------------------------
    i = 2
    m = 1
    l = 2
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_P2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fbc(1) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! 0' = np'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(np   ) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! 0' = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) + fi(2    ) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! 0' = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(i + 2) - TWO * fi(i) - fi(2    ) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np - 1
!----------------------------------------------------------------------------------------------------------
    i = np - 1
    m = 2
    l = 4
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_P2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(2   ) - TWO * fi(i) + fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      !np' + 1 = 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(1    ) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      !np' + 1 = np' - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np- 1) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      !i + 2 = np + 1 = np - 1
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np- 1) - TWO * fi(i) + fi(i - 2) )
    else 
      fo(i) = coeff( l, 1, IBC_INTRPL ) * ( fi(i + 1) - TWO * fi(i) + fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   i = np
!----------------------------------------------------------------------------------------------------------
    i = np
    m = 2
    l = 5
    if (ibc(m) == IBC_INTERIOR) then
      if(.not. present(fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ Prepare_TDMA_2deri_P2P_RHS_array')
      fo(i) = coeff( l, 1, ibc(m) ) * ( fbc(2) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fbc(4) - TWO * fi(i) + fi(i - 2) )

    else if ( ibc(m) == IBC_PERIODIC) then
      ! np' + 1 = 1'
      ! np' + 2 = 2'
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(1   ) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(2   ) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_SYMMETRIC ) then
      ! np' + 1 = np' - 1
      ! np' + 2 = np' - 2
      fo(i) = coeff( l, 1, ibc(m) ) * ( fi(np- 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * ( fi(np- 2) - TWO * fi(i) + fi(i - 2) )
    else if (ibc(m) == IBC_ASYMMETRIC) then
      ! np' + 1 = nc' - 1
      ! np' + 2 = nc' - 2
      fo(i) = coeff( l, 1, ibc(m) ) * (-fi(np- 1) - TWO * fi(i) + fi(i - 1) ) + &
              coeff( l, 2, ibc(m) ) * (-fi(np- 2) - TWO * fi(i) + fi(i - 2) )
    else
      fo(i) = coeff( l, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( l, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( l, 3, IBC_INTRPL) * fi(i - 2) + &
              coeff( l, 4, IBC_INTRPL) * fi(i - 3) 
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd ! dd = (1/dx)^2

    return
  end subroutine Prepare_TDMA_2deri_P2P_RHS_array
!==========================================================================================================
!> \brief To caculate the mid-point interpolation in 1D.
!> This subroutine is called as required to get the mid-point interpolation.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   pubic
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ixsub         x-subdomain index
!> \param[in]     ibc           bc type
!> \param[in]     fbc           bc value
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     fi            the input array of original variable
!> \param[out]    fo            the output array of interpolated variable
!_______________________________________________________________________________
  subroutine Get_x_midp_C2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz

    integer :: i
    integer :: ibc(2)

    ibc = ibc0
    do i = 1, 2
      if (ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_midp_C2P_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
      if (ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_DIRICHLET @ Get_x_midp_C2P_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
    end do

    ixsub = dm%idom
    
    nsz = size(fo)
    fo = ZERO
    
    call Prepare_TDMA_interp_C2P_RHS_array(fi(:), fo(:), nsz, m1rC2P(:, :, :), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%am1x_C2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bm1x_C2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cm1x_C2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dm1x_C2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_midp_C2P_1D
!==========================================================================================================
  subroutine Get_x_midp_P2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: ixsub, nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if (ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_midp_P2C_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    ixsub = dm%idom

    call Prepare_TDMA_interp_P2C_RHS_array(fi(:), fo(:), nsz, m1rP2C(:, :, :), ibc(:), fbc)
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%am1x_P2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bm1x_P2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cm1x_P2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dm1x_P2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_midp_P2C_1D
!==========================================================================================================
  subroutine Get_y_midp_C2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use tridiagonal_matrix_algorithm
    use udf_type_mod
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if (ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_y_midp_C2P_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
      if (ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_DIRICHLET @ Get_y_midp_C2P_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_interp_C2P_RHS_array(fi(:), fo(:), nsz, m1rC2P(:, :, :), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          am1y_C2P(:, ibc(1), ibc(2)), &
          bm1y_C2P(:, ibc(1), ibc(2)), &
          cm1y_C2P(:, ibc(1), ibc(2)), &
          dm1y_C2P(:, ibc(1), ibc(2)), &
          nsz)
! stretching? No stretching conversion
    return
  end subroutine Get_y_midp_C2P_1D
!==========================================================================================================
  subroutine Get_y_midp_P2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz
    
    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if (ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_y_midp_P2C_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_interp_P2C_RHS_array(fi(:), fo(:), nsz, m1rP2C(:, :, :), ibc(:), fbc(:) )
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          am1y_P2C(:, ibc(1), ibc(2)), &
          bm1y_P2C(:, ibc(1), ibc(2)), &
          cm1y_P2C(:, ibc(1), ibc(2)), &
          dm1y_P2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_y_midp_P2C_1D
!==========================================================================================================
  subroutine Get_z_midp_C2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if (ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_z_midp_C2P_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
      if (ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_DIRICHLET @ Get_z_midp_C2P_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    
    call Prepare_TDMA_interp_C2P_RHS_array(fi(:), fo(:), nsz, m1rC2P(:, :, :), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          am1z_C2P(:, ibc(1), ibc(2)), &
          bm1z_C2P(:, ibc(1), ibc(2)), &
          cm1z_C2P(:, ibc(1), ibc(2)), &
          dm1z_C2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_midp_C2P_1D
!==========================================================================================================
  subroutine Get_z_midp_P2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if (ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc) )) then
        call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_z_midp_P2C_1D, degragded to IBC_INTRPL.')
        ibc(i) = IBC_INTRPL
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_interp_P2C_RHS_array(fi(:), fo(:), nsz, m1rP2C(:, :, :), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          am1z_P2C(:, ibc(1), ibc(2)), &
          bm1z_P2C(:, ibc(1), ibc(2)), &
          cm1z_P2C(:, ibc(1), ibc(2)), &
          dm1z_P2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_midp_P2C_1D
!==========================================================================================================
!> \brief To caculate the 1st derivative in 1D.
!> This subroutine is called as required to get the 1st derivative
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   pubic
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ixsub         x-subdomain index
!> \param[in]     ibc           bc type
!> \param[in]     fbc           bc value
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     fi            the input array of original variable
!> \param[out]    fo            the output array of interpolated variable
!==========================================================================================================
  subroutine Get_x_1st_derivative_C2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: ixsub, nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_1st_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) )then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_DIRICHLET @ Get_x_1st_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    ixsub = dm%idom
    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_C2C_RHS_array(fi(:), fo(:), nsz, d1rC2C(:, :, :), dm%h1r(1), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%ad1x_C2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bd1x_C2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cd1x_C2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dd1x_C2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_1st_derivative_C2C_1D
!==========================================================================================================
  subroutine Get_x_1st_derivative_P2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_1st_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_NEUMANN  .and. (.not. present(fbc)) )then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ Get_x_1st_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    ixsub = dm%idom

    call Prepare_TDMA_1deri_P2P_RHS_array(fi(:), fo(:), nsz, d1rP2P(:, :, :), dm%h1r(1), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%ad1x_P2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bd1x_P2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cd1x_P2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dd1x_P2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_1st_derivative_P2P_1D
!==========================================================================================================
  subroutine Get_x_1st_derivative_C2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_NEUMANN  .and. (.not. present(fbc)) )then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for DIRICHLET @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    ixsub = dm%idom

    call Prepare_TDMA_1deri_C2P_RHS_array(fi(:), fo(:), nsz, d1rC2P(:, :, :), dm%h1r(1), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%ad1x_C2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bd1x_C2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cd1x_C2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dd1x_C2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_1st_derivative_C2P_1D
!==========================================================================================================
  subroutine Get_x_1st_derivative_P2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz, ixsub

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_1st_derivative_P2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    ixsub = dm%idom

    call Prepare_TDMA_1deri_P2C_RHS_array(fi(:), fo(:), nsz, d1rP2C(:, :, :), dm%h1r(1), ibc(:), fbc(:) )
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%ad1x_P2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bd1x_P2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cd1x_P2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dd1x_P2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_1st_derivative_P2C_1D
!==========================================================================================================
! y - Get_1st_derivative_1D
!==========================================================================================================
  subroutine Get_y_1st_derivative_C2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_y_1st_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_DIRICHLET @ Get_y_1st_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_C2C_RHS_array(fi(:), fo(:), nsz, d1rC2C(:, :, :), dm%h1r(2), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          ad1y_C2C(:, ibc(1), ibc(2)), &
          bd1y_C2C(:, ibc(1), ibc(2)), &
          cd1y_C2C(:, ibc(1), ibc(2)), &
          dd1y_C2C(:, ibc(1), ibc(2)), &
          nsz)

    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingcc(:, 1)

    return
  end subroutine Get_y_1st_derivative_C2C_1D
!==========================================================================================================
  subroutine Get_y_1st_derivative_P2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_y_1st_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_NEUMANN  .and. (.not. present(fbc)) )then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ Get_y_1st_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_P2P_RHS_array(fi(:), fo(:), nsz, d1rP2P(:, :, :), dm%h1r(2), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          ad1y_P2P(:, ibc(1), ibc(2)), &
          bd1y_P2P(:, ibc(1), ibc(2)), &
          cd1y_P2P(:, ibc(1), ibc(2)), &
          dd1y_P2P(:, ibc(1), ibc(2)), &
          nsz)
    
    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingpt(:, 1)

    return
  end subroutine Get_y_1st_derivative_P2P_1D
!==========================================================================================================
  subroutine Get_y_1st_derivative_C2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_NEUMANN  .and. (.not. present(fbc)) )then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for DIRICHLET @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_C2P_RHS_array(fi(:), fo(:), nsz, d1rC2P(:, :, :), dm%h1r(2), ibc(:), fbc(:) )
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          ad1y_C2P(:, ibc(1), ibc(2)), &
          bd1y_C2P(:, ibc(1), ibc(2)), &
          cd1y_C2P(:, ibc(1), ibc(2)), &
          dd1y_C2P(:, ibc(1), ibc(2)), &
          nsz)
  
    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingpt(:, 1)

    return
  end subroutine Get_y_1st_derivative_C2P_1D
!==========================================================================================================
  subroutine Get_y_1st_derivative_P2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_y_1st_derivative_P2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_P2C_RHS_array(fi(:), fo(:), nsz, d1rP2C(:, :, :), dm%h1r(2), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          ad1y_P2C(:, ibc(1), ibc(2)), &
          bd1y_P2C(:, ibc(1), ibc(2)), &
          cd1y_P2C(:, ibc(1), ibc(2)), &
          dd1y_P2C(:, ibc(1), ibc(2)), &
          nsz)

    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingcc(:, 1)

    return
  end subroutine Get_y_1st_derivative_P2C_1D
!==========================================================================================================
! z - Get_1st_derivative_1D
!==========================================================================================================
  subroutine Get_z_1st_derivative_C2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_1st_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_DIRICHLET @ Get_x_1st_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_C2C_RHS_array(fi(:), fo(:), nsz, d1rC2C(:, :, :), dm%h1r(3), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          ad1z_C2C(:, ibc(1), ibc(2)), &
          bd1z_C2C(:, ibc(1), ibc(2)), &
          cd1z_C2C(:, ibc(1), ibc(2)), &
          dd1z_C2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_1st_derivative_C2C_1D
!==========================================================================================================
  subroutine Get_z_1st_derivative_P2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_z_1st_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_NEUMANN  .and. (.not. present(fbc)) )then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ Get_z_1st_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_P2P_RHS_array(fi(:), fo(:), nsz, d1rP2P(:, :, :), dm%h1r(3), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          ad1z_P2P(:, ibc(1), ibc(2)), &
          bd1z_P2P(:, ibc(1), ibc(2)), &
          cd1z_P2P(:, ibc(1), ibc(2)), &
          dd1z_P2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_1st_derivative_P2P_1D
!==========================================================================================================
  subroutine Get_z_1st_derivative_C2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_NEUMANN  .and. (.not. present(fbc)) )then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for DIRICHLET @ Get_x_1st_derivative_C2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_C2P_RHS_array(fi(:), fo(:), nsz, d1rC2P(:, :, :), dm%h1r(3), ibc(:), fbc(:) )
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          ad1z_C2P(:, ibc(1), ibc(2)), &
          bd1z_C2P(:, ibc(1), ibc(2)), &
          cd1z_C2P(:, ibc(1), ibc(2)), &
          dd1z_C2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_1st_derivative_C2P_1D
!==========================================================================================================
  subroutine Get_z_1st_derivative_P2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_z_1st_derivative_P2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_1deri_P2C_RHS_array(fi(:), fo(:), nsz, d1rP2C(:, :, :), dm%h1r(3), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          ad1z_P2C(:, ibc(1), ibc(2)), &
          bd1z_P2C(:, ibc(1), ibc(2)), &
          cd1z_P2C(:, ibc(1), ibc(2)), &
          dd1z_P2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_1st_derivative_P2C_1D
!==========================================================================================================
!> \brief To caculate the 2nd derivative in 1D.
!> This subroutine is called as required to get the 2nd derivative
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   pubic
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ixsub         x-subdomain index
!> \param[in]     ibc           bc type
!> \param[in]     fbc           bc value
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     fi            the input array of original variable
!> \param[out]    fo            the output array of interpolated variable
!==========================================================================================================
  subroutine Get_x_2nd_derivative_C2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in)  :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz, ixsub

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_2nd_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for DIRICHLET @ Get_x_2nd_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    ixsub = dm%idom

    call Prepare_TDMA_2deri_C2C_RHS_array(fi(:), fo(:), nsz, d2rC2C(:, :, :), dm%h2r(1), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%ad2x_C2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bd2x_C2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cd2x_C2C(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dd2x_C2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_2nd_derivative_C2C_1D
!==========================================================================================================
  subroutine Get_x_2nd_derivative_P2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz, ixsub

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_x_2nd_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    ixsub = dm%idom

    call Prepare_TDMA_2deri_P2P_RHS_array(fi(:), fo(:), nsz, d2rP2P(:, :, :), dm%h2r(1), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(1), fo(:), &
          xtdma_lhs(ixsub)%ad2x_P2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%bd2x_P2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%cd2x_P2P(:, ibc(1), ibc(2)), &
          xtdma_lhs(ixsub)%dd2x_P2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_x_2nd_derivative_P2P_1D
!==========================================================================================================
! y - Get_2nd_derivative_1D
!==========================================================================================================
  subroutine Get_y_2nd_derivative_C2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz
    real(WP), allocatable :: fo1(:)

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_y_2nd_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for DIRICHLET @ Get_y_2nd_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_2deri_C2C_RHS_array(fi(:), fo(:), nsz, d2rC2C(:, :, :), dm%h2r(2), ibc(:), fbc(:) )
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          ad2y_C2C(:, ibc(1), ibc(2)), &
          bd2y_C2C(:, ibc(1), ibc(2)), &
          cd2y_C2C(:, ibc(1), ibc(2)), &
          dd2y_C2C(:, ibc(1), ibc(2)), &
          nsz)

    if(dm%is_stretching(2)) then 

      allocate ( fo1(nsz) ); fo1(:) = ZERO
      call Prepare_TDMA_1deri_C2C_RHS_array(fi(:), fo1(:), nsz, d1rC2C(:, :, :), dm%h1r(2), ibc(:), fbc(:))
      if (dm%is_compact_scheme) &
      call Solve_TDMA(dm%is_periodic(2), fo1(:), &
           ad1y_C2C(:, ibc(1), ibc(2)), &
           bd1y_C2C(:, ibc(1), ibc(2)), &
           cd1y_C2C(:, ibc(1), ibc(2)), &
           dd1y_C2C(:, ibc(1), ibc(2)), &
           nsz)
      fo(:) = fo(:) * dm%yMappingcc(:, 2) + fo1(:) * dm%yMappingcc(:, 3)
      deallocate (fo1)
    end if

    return
  end subroutine Get_y_2nd_derivative_C2C_1D
!==========================================================================================================
  subroutine Get_y_2nd_derivative_P2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    real(WP), allocatable :: fo1(:)

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_y_2nd_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_2deri_P2P_RHS_array(fi(:), fo(:), nsz, d2rP2P(:, :, :), dm%h2r(2), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(2), fo(:), &
          ad2y_P2P(:, ibc(1), ibc(2)), &
          bd2y_P2P(:, ibc(1), ibc(2)), &
          cd2y_P2P(:, ibc(1), ibc(2)), &
          dd2y_P2P(:, ibc(1), ibc(2)), &
          nsz)

    if(dm%is_stretching(2)) then 
      allocate ( fo1(nsz) ); fo1(:) = ZERO

      do i = 1, 2
        if(ibc(i) == IBC_NEUMANN  .and. (.not. present(fbc)) )then
            ibc(i) = IBC_INTRPL
            call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ Get_y_2nd_derivative_P2P_1D, degragded to IBC_INTRPL.')
        end if
      end do

      call Prepare_TDMA_1deri_P2P_RHS_array(fi(:), fo1(:), nsz, d1rP2P(:, :, :), dm%h1r(2), ibc(:), fbc(:))
      if (dm%is_compact_scheme) &
      call Solve_TDMA(dm%is_periodic(2), fo1(:), &
           ad1y_P2P(:, ibc(1), ibc(2)), &
           bd1y_P2P(:, ibc(1), ibc(2)), &
           cd1y_P2P(:, ibc(1), ibc(2)), &
           dd1y_P2P(:, ibc(1), ibc(2)), &
           nsz)
      fo(:) = fo(:) * dm%yMappingpt(:, 2) + fo1(:) * dm%yMappingpt(:, 3)
      deallocate (fo1)
    end if

    return
  end subroutine Get_y_2nd_derivative_P2P_1D
!==========================================================================================================
! z - Get_2nd_derivative_1D
!==========================================================================================================
  subroutine Get_z_2nd_derivative_C2C_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_z_2nd_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
      if(ibc(i) == IBC_DIRICHLET  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for DIRICHLET @ Get_z_2nd_derivative_C2C_1D, degragded to IBC_INTRPL.')
      end if
    end do
    

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_2deri_C2C_RHS_array(fi(:), fo(:), nsz, d2rC2C(:, :, :), dm%h2r(3), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          ad2z_C2C(:, ibc(1), ibc(2)), &
          bd2z_C2C(:, ibc(1), ibc(2)), &
          cd2z_C2C(:, ibc(1), ibc(2)), &
          dd2z_C2C(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_2nd_derivative_C2C_1D
!==========================================================================================================
  subroutine Get_z_2nd_derivative_P2P_1D (fi, fo, dm, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)
    integer :: nsz

    integer :: i
    integer :: ibc(2)
    
    ibc = ibc0
    do i = 1, 2
      if(ibc(i) == IBC_INTERIOR  .and. (.not. present(fbc)) ) then
          ibc(i) = IBC_INTRPL
          call Print_warning_msg('Lack of fbc info for IBC_INTERIOR @ Get_z_2nd_derivative_P2P_1D, degragded to IBC_INTRPL.')
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    call Prepare_TDMA_2deri_P2P_RHS_array(fi(:), fo(:), nsz, d2rP2P(:, :, :), dm%h2r(3), ibc(:), fbc(:))
    if (dm%is_compact_scheme) &
    call Solve_TDMA(dm%is_periodic(3), fo(:), &
          ad2z_P2P(:, ibc(1), ibc(2)), &
          bd2z_P2P(:, ibc(1), ibc(2)), &
          cd2z_P2P(:, ibc(1), ibc(2)), &
          dd2z_P2P(:, ibc(1), ibc(2)), &
          nsz)

    return
  end subroutine Get_z_2nd_derivative_P2P_1D
!==========================================================================================================
!> \brief To caculate the mid-point interpolation in 3D.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   pubic
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ixsub         x-subdomain index
!> \param[in]     ibc           bc type
!> \param[in]     fbc           bc value
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     fi            the input array of original variable
!> \param[out]    fo            the output array of interpolated variable
!==========================================================================================================
  subroutine Get_x_midp_C2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use tridiagonal_matrix_algorithm
    use udf_type_mod
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j

!----------------------------------------------------------------------------------------------------------
!  default : x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(1:4, j, k)
        call Get_x_midp_C2P_1D (fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_midp_C2P_3D
!==========================================================================================================
  subroutine Get_x_midp_P2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :) !2 layer each side
    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    integer :: k, j
    real(WP)   :: fbc(4)
!----------------------------------------------------------------------------------------------------------
!  default : x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(1:4, j, k)
        call Get_x_midp_P2C_1D (fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_midp_P2C_3D
!==========================================================================================================
  subroutine Get_y_midp_C2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i

!----------------------------------------------------------------------------------------------------------
!  default : y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_midp_C2P_1D (fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_midp_C2P_3D
!==========================================================================================================
  subroutine Get_y_midp_P2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    integer :: k, i
    real(WP) :: fbc(4)
!----------------------------------------------------------------------------------------------------------
!  y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_midp_P2C_1D (fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_midp_P2C_3D
  !==========================================================================================================
  subroutine Get_z_midp_C2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i

!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_midp_C2P_1D (fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_midp_C2P_3D
!==========================================================================================================
  subroutine Get_z_midp_P2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    integer :: j, i
    real(WP)   :: fbc(4)
!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_midp_P2C_1D (fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_midp_P2C_3D
!==========================================================================================================
!> \brief To caculate the 1st-deriviate in 3D.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   pubic
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ixsub         x-subdomain index
!> \param[in]     ibc           bc type
!> \param[in]     fbc           bc value
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     fi            the input array of original variable
!> \param[out]    fo            the output array of interpolated variable
!==========================================================================================================
  subroutine Get_x_1st_derivative_C2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j
!----------------------------------------------------------------------------------------------------------
!  x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(1:4, j, k)
        call Get_x_1st_derivative_C2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1st_derivative_C2C_3D
!==========================================================================================================
  subroutine Get_x_1st_derivative_P2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j

!----------------------------------------------------------------------------------------------------------
!  x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(1:4, j, k)
        call Get_x_1st_derivative_P2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1st_derivative_P2P_3D
!==========================================================================================================
  subroutine Get_x_1st_derivative_C2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j

!----------------------------------------------------------------------------------------------------------
!  x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(1:4, j, k)
        call Get_x_1st_derivative_C2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1st_derivative_C2P_3D
!==========================================================================================================
  subroutine Get_x_1st_derivative_P2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j
!----------------------------------------------------------------------------------------------------------
!  x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(1:4, j, k)
        call Get_x_1st_derivative_P2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1st_derivative_P2C_3D
!==========================================================================================================
  subroutine Get_y_1st_derivative_C2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i

    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_1st_derivative_C2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1st_derivative_C2C_3D
!==========================================================================================================
  subroutine Get_y_1st_derivative_P2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i

!!----------------------------------------------------------------------------------------------------------
!  y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_1st_derivative_P2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1st_derivative_P2P_3D
!==========================================================================================================
  subroutine Get_y_1st_derivative_C2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i
!----------------------------------------------------------------------------------------------------------
!  y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_1st_derivative_C2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1st_derivative_C2P_3D

!==========================================================================================================
  subroutine Get_y_1st_derivative_P2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i
!----------------------------------------------------------------------------------------------------------
!  y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_1st_derivative_P2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1st_derivative_P2C_3D
!==========================================================================================================
  subroutine Get_z_1st_derivative_C2C_3D (fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i
!----------------------------------------------------------------------------------------------------------
!  z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_1st_derivative_C2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1st_derivative_C2C_3D

!==========================================================================================================
  subroutine Get_z_1st_derivative_P2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i

!----------------------------------------------------------------------------------------------------------
!  z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_1st_derivative_P2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1st_derivative_P2P_3D
!==========================================================================================================
  subroutine Get_z_1st_derivative_C2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i

!----------------------------------------------------------------------------------------------------------
!  z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_1st_derivative_C2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1st_derivative_C2P_3D
  !==========================================================================================================
  subroutine Get_z_1st_derivative_P2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i
!----------------------------------------------------------------------------------------------------------
!  z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_1st_derivative_P2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1st_derivative_P2C_3D
!==========================================================================================================
!> \brief To caculate the 2nd-deriviate in 3D.
!---------------------------------------------------------------------------------------------------------- 
!> Scope:  mpi            called-freq    xdomain     module
!>       in-given pencil    needed       specified   pubic
!----------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     ixsub         x-subdomain index
!> \param[in]     ibc           bc type
!> \param[in]     fbc           bc value
!> \param[in]     inbr          the neibouring index of 4 bc nodes
!> \param[in]     fi            the input array of original variable
!> \param[out]    fo            the output array of interpolated variable
!==========================================================================================================
  subroutine Get_x_2nd_derivative_C2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j
!----------------------------------------------------------------------------------------------------------
!  x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(1:4, j, k)
        call Get_x_2nd_derivative_C2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return
  end subroutine Get_x_2nd_derivative_C2C_3D
  !==========================================================================================================
  subroutine Get_x_2nd_derivative_P2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j
!----------------------------------------------------------------------------------------------------------
!  x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) fbc(1:4) =  fbc2d(1:4, j, k)
        call Get_x_2nd_derivative_P2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return
  end subroutine Get_x_2nd_derivative_P2P_3D
!==========================================================================================================
  subroutine Get_y_2nd_derivative_C2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i
!----------------------------------------------------------------------------------------------------------
!  y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_2nd_derivative_C2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return
  end subroutine Get_y_2nd_derivative_C2C_3D
!==========================================================================================================
  subroutine Get_y_2nd_derivative_P2P_3D(fi3d, fo3d, dm, ibc, fbc2d) ! not used.
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i
!----------------------------------------------------------------------------------------------------------
!  y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, 1:4, k)
        call Get_y_2nd_derivative_P2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return
  end subroutine Get_y_2nd_derivative_P2P_3D
!==========================================================================================================
  subroutine Get_z_2nd_derivative_C2C_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i
!----------------------------------------------------------------------------------------------------------
!  z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_2nd_derivative_C2C_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return
  end subroutine Get_z_2nd_derivative_C2C_3D
!==========================================================================================================
  subroutine Get_z_2nd_derivative_P2P_3D(fi3d, fo3d, dm, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i
!----------------------------------------------------------------------------------------------------------
!  z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) fbc(1:4) = fbc2d(i, j, 1:4)
        call Get_z_2nd_derivative_P2P_1D(fi, fo, dm, ibc, fbc)
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return
  end subroutine Get_z_2nd_derivative_P2P_3D


!==========================================================================================================
!==========================================================================================================
!> \brief To test this subroutine for mid-point interpolation.
!>
!> This subroutine is called in \ref Test_algorithms. Define the logicals to choose
!> which test section is required. 
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     d             domain
!_______________________________________________________________________________
  subroutine Test_interpolation(dm)
    use parameters_constant_mod
    use math_mod
    use EvenOdd_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    real(WP) :: xc, yc, zc
    real(WP) :: xp, yp, zp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(4)

    real(WP) :: fbc(4)
    real(WP) :: scale, shift
    integer :: i, j, k

    real(WP), allocatable :: fxc (:), fyc (:), fzc (:)
    real(WP), allocatable :: fxp (:), fyp (:), fzp (:)
    real(WP), allocatable :: fgxc(:), fgyc(:), fgzc(:)
    real(WP), allocatable :: fgxp(:), fgyp(:), fgzp(:)

    if (dm%ibcx_Th(1, IBC_CCC) == IBC_DIRICHLET) then
       if(is_even(dm%np(1))) dm%np(1)=dm%np(1)+1
    end if
    if (dm%ibcy_Th(1, IBC_CCC) == IBC_DIRICHLET) then
       if(is_even(dm%np(2))) dm%np(2)=dm%np(2)+1
    end if
    if (dm%ibcz_Th(1, IBC_CCC) == IBC_DIRICHLET) then
       if(is_even(dm%np(3))) dm%np(3)=dm%np(3)+1
    end if
    allocate ( fxc (dm%nc(1)), fyc (dm%nc(2)), fzc (dm%nc(3)) )
    allocate ( fxp (dm%np(1)), fyp (dm%np(2)), fzp (dm%np(3)) )
    allocate ( fgxc(dm%nc(1)), fgyc(dm%nc(2)), fgzc(dm%nc(3)) )
    allocate ( fgxp(dm%np(1)), fgyp(dm%np(2)), fgzp(dm%np(3)) )
    dm%h(1) = TWOPI / dm%nc(1)
    dm%h(2) = TWOPI / dm%nc(2)
    dm%h(3) = TWOPI / dm%nc(3)

    open (newunit = wrt_unit(4), file = 'test_interpolation.dat', position="append")
    write(wrt_unit(4), *) '# xbc type ', dm%ibcx_Th(1:2, IBC_CCC), " err_inf, err_L2"
    write(wrt_unit(4), *) '# ybc type ', dm%ibcy_Th(1:2, IBC_CCC), " err_inf, err_L2"
    write(wrt_unit(4), *) '# zbc type ', dm%ibcz_Th(1:2, IBC_CCC), " err_inf, err_L2"

    open (newunit = wrt_unit(1), file = 'test_interpolation_x.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_interpolation_y.dat', position="append")
    open (newunit = wrt_unit(3), file = 'test_interpolation_z.dat', position="append")

    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        write(*,*) size(dm%fbcx_pr(:, :, :), 1), size(dm%fbcx_pr(:, :, :), 2) , size(dm%fbcx_pr(:, :, :), 3)
        if(i==1) dm%fbcx_pr(1, :, :) = ZERO
        if(i==2) dm%fbcx_pr(2, :, :) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO

        if(i==1) dm%fbcx_pr(1, :, :) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcx_pr(2, :, :) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcx_pr(1, :, :) = ZERO
        if(i==2) dm%fbcx_pr(2, :, :) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    do i = 1, 2
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ZERO
        if(i==2) dm%fbcy_pr(:, 2, :) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcy_pr(:, 2, :) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ZERO
        if(i==2) dm%fbcy_pr(:, 2, :) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    do i = 1, 2
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ZERO
        if(i==2) dm%fbcz_pr(:, :, 2) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcz_pr(:, :, 2) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ZERO
        if(i==2) dm%fbcz_pr(:, :, 2) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

!----------------------------------------------------------------------------------------------------------
! c2p
!----------------------------------------------------------------------------------------------------------
    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_INTERIOR) then
        scale = THREE
        shift = ZERO
        if(i==1) then
          dm%fbcx_pr(1, :, :) = sin_wp ( dm%h(1) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcx_pr(3, :, :) = sin_wp ( dm%h(1) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcx_pr(2, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcx_pr(4, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i==1) then
          dm%fbcy_pr(:, 1, :) = sin_wp ( dm%h(2) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcy_pr(:, 3, :) = sin_wp ( dm%h(2) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcy_pr(:, 2, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcy_pr(:, 4, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i==1) then
          dm%fbcz_pr(:, :, 1) = sin_wp ( dm%h(3) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcz_pr(:, :, 3) = sin_wp ( dm%h(3) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcz_pr(:, :, 2) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcz_pr(:, :, 4) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
    end do

! x direction, xc
    do i = 1, dm%nc(1)
      xc = dm%h(1) * (real(i - 1, WP) + HALF)
      fxc(i) = sin_wp ( xc / scale + shift)
    end do
! x: c2p
    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    ! do i = 1, 4 
    !   write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-intpbc-c2p ', i, lbcx(i), fbc(i), fbc(i), zero
    ! end do
    call Get_x_midp_C2P_1D (fxc, fgxp, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%np(1)
      xp = dm%h(1) * real(i - 1, WP)
      ref = sin_wp(xp / scale + shift)
      err = abs_wp(fgxp(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-interp-c2p ', i, xp, ref, fgxp(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(1)) 
    write(wrt_unit(4), *) '# x-interp-c2p ', dm%np(1), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-interp-c2p failed.")
      write(wrt_unit(1), *) '# x-interp-c2p ', dm%np(1), err_Linf, err_L2
      do i = 1, dm%np(1)
        xp = dm%h(1) * real(i - 1, WP)
        ref = sin_wp(xp / scale + shift)
        err = abs_wp(fgxp(i) - ref)
        write(wrt_unit(1),*) i, xp, ref, fgxp(i), err !test
      end do
    !end if

! y direction, yc
    do j = 1, dm%nc(2)
      yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
      fyc(j) = sin_wp ( yc / scale + shift)
    end do
! y: c2p
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_midp_C2P_1D (fyc, fgyp, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      ref = sin_wp(yp / scale + shift)
      err = abs_wp(fgyp(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-interp-c2p ', j, yp, ref, fgyp(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(2)) 
    write(wrt_unit(4), *) '# y-interp-c2p ', dm%np(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-interp-c2p failed.")
      write(wrt_unit(2), *) '# y-interp-c2p ', dm%np(2), err_Linf, err_L2
      do j = 1, dm%np(2)
        yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
        ref = sin_wp(yp / scale + shift)
        err = abs_wp(fgyp(j) - ref)
        write(wrt_unit(2),*) j, yp, ref, fgyp(j), err !test
      end do
    !end if
    

 ! z direction, zc
    do k = 1, dm%nc(3)
      zc = dm%h(3) * (real(k - 1, WP) + HALF)
      fzc(k) = sin_wp ( zc / scale + shift)
    end do
! z: c2p
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_midp_C2P_1D (fzc, fgzp, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%np(3)
      zp = dm%h(3) * real(k - 1, WP)
      ref = sin_wp(zp / scale + shift)
      err = abs_wp(fgzp(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-interp-c2p ', k, zp, ref, fgzp(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(3)) 
    write(wrt_unit(4), *) '# z-interp-c2p ', dm%np(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-interp-c2p failed.")
      write(wrt_unit(3), *) '# z-interp-c2p ', dm%np(3), err_Linf, err_L2
      do k = 1, dm%np(3)
        zp = dm%h(3) * real(k - 1, WP)
        ref = sin_wp(zp / scale + shift)
        err = abs_wp(fgzp(k) - ref)
        write(wrt_unit(3),*) k, zp, ref, fgzp(k), err !test
      end do
    !end if
!----------------------------------------------------------------------------------------------------------
! p2c
!----------------------------------------------------------------------------------------------------------
    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_INTERIOR) then
        scale = THREE
        shift = ZERO
        if(i == 1) then
          dm%fbcx_pr(1, :, :) = sin_wp ( dm%h(1) * (real( -1, WP) ) / scale + shift)
          dm%fbcx_pr(3, :, :) = sin_wp ( dm%h(1) * (real( -2, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcx_pr(2, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 1, WP) ) / scale + shift)
          dm%fbcx_pr(4, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 2, WP) ) / scale + shift)
        end if
      end if
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i == 1) then
          dm%fbcy_pr(:, 1, :) = sin_wp ( dm%h(2) * (real(-1, WP) ) / scale + shift)
          dm%fbcy_pr(:, 3, :) = sin_wp ( dm%h(2) * (real(-2, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcy_pr(:, 2, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 1, WP) ) / scale + shift)
          dm%fbcy_pr(:, 4, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 2, WP) ) / scale + shift)
        end if
      end if
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i == 1) then
          dm%fbcz_pr(:, :, 1) = sin_wp ( dm%h(3) * (real( 0 - 1, WP) ) / scale + shift)
          dm%fbcz_pr(:, :,3) = sin_wp ( dm%h(3) * (real(-1 - 1, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcz_pr(:, :, 2) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 1, WP) ) / scale + shift)
          dm%fbcz_pr(:, :, 4) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 2, WP) ) / scale + shift)
        end if
      end if
    end do
! x direction, xp
    do i = 1, dm%np(1)
      xp = dm%h(1) * real(i - 1, WP)
      fxp(i) = sin_wp ( xp / scale + shift)
    end do
! x: p2c
    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    call Get_x_midp_P2C_1D (fxp, fgxc, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_PCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%nc(1)
      xc = dm%h(1) * (real(i - 1, WP) + HALF)
      ref = sin_wp(xc / scale + shift)
      err = abs_wp(fgxc(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-interp-p2c ', i, xc, ref, fgxc(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(1)) 
    write(wrt_unit(4), *) '# x-interp-p2c ', dm%nc(1), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-interp-p2c failed.")
      write(wrt_unit(1), *) '# x-interp-p2c ', dm%nc(1), err_Linf, err_L2
      do i = 1, dm%nc(1)
        xc = dm%h(1) * (real(i - 1, WP) + HALF)
        ref = sin_wp(xc / scale + shift)
        err = abs_wp(fgxc(i) - ref)
        write(wrt_unit(1),*) i, xc, ref, fgxc(i), err !test
      end do
    !end if
    

! y direction, yp
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      fyp(j) = sin_wp ( yp / scale + shift)
    end do
! y: p2c
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_midp_P2C_1D (fyp, fgyc, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%nc(2)
      yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
      ref = sin_wp(yc / scale + shift)
      err = abs_wp(fgyc(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-interp-p2c ', j, yc, ref, fgyc(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(2)) 
    write(wrt_unit(4), *) '# y-interp-p2c ', dm%nc(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-interp-p2c failed.")
      write(wrt_unit(2), *) '# y-interp-p2c ', dm%nc(2), err_Linf, err_L2
      do j = 1, dm%nc(2)
        yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
        ref = sin_wp(yc / scale + shift)
        err = abs_wp(fgyc(j) - ref)
        if(err > err_Linf) err_Linf = err
        err_L2 = err_L2 + err**2
        write(wrt_unit(2),*) j, yc, ref, fgyc(j), err !test
      end do
    !end if
    


! z direction, zp
    do k = 1, dm%np(3)
      zp = dm%h(3) * real(k - 1, WP)
      fzp(k) = sin_wp ( zp / scale + shift)
    end do
! z: p2c
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_midp_P2C_1D (fzp, fgzc, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_PPP), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%nc(3)
      zc = dm%h(3) * (real(k - 1, WP) + HALF)
      ref = sin_wp(zc / scale + shift)
      err = abs_wp(fgzc(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-interp-p2c ', k, zc, ref, fgzc(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(3)) 
    write(wrt_unit(4), *) '# z-interp-p2c ', dm%nc(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-interp-p2c failed.")
      write(wrt_unit(3), *) '# z-interp-p2c ', dm%nc(3), err_Linf, err_L2
      do k = 1, dm%nc(3)
        zc = dm%h(3) * (real(k - 1, WP) + HALF)
        ref = sin_wp(zc / scale + shift)
        err = abs_wp(fgzc(k) - ref)
        if(err > err_Linf) err_Linf = err
        err_L2 = err_L2 + err**2
        write(wrt_unit(3),*) k, zc, ref, fgzc(k), err !test
      end do
    !end if



! test a combination
    ! input: f(yp) = sin(x/3)
    ! intpp2c: f(yc) = sin(x/3)
    ! 1stderic2p: f'(yp) = 1/3 cos(x/3)

    ! y direction, yp
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      fyp(j) = sin_wp ( yp / scale + shift)
    end do
    ! intp p2c + 1st deri c2p
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_midp_P2C_1D          (fyp, fgyc, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_PPP), fbc)
    call Get_y_1st_derivative_C2P_1D(fgyc,fgyp, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)

    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      ref = ONE/scale * cos_wp(yp / scale + shift)
      err = abs_wp(fgyp(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-interp-c2p ', j, yp, ref, fgyp(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(2)) 
    write(wrt_unit(4), *) '# y-intP2C+derC2P ', dm%np(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-intP2C+derC2P failed.")
      write(wrt_unit(2), *) '# y-intP2C+derC2P ', dm%np(2), err_Linf, err_L2
      do j = 1, dm%np(2)
        yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
        ref = ONE/scale * cos_wp(yp / scale + shift)
        err = abs_wp(fgyp(j) - ref)
        write(wrt_unit(2),*) j, yp, ref, fgyp(j), err !test
      end do
    !end if

! ! test a combination
!     ! input: f(yp) = sin(x/3)
!     ! intpp2c: f(yc) = sin(x/3)
!     ! input for deri: f(yc)= sin(x/3) * sin(x/3)
!     ! 1stderic2p: f'(yp) = 2/3 sin(x/3) * cos(x/3)

!     ! y direction, yp
!     do j = 1, dm%np(2)
!       yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
!       fyp(j) = sin_wp ( yp / scale + shift)
!     end do
!     ! intp p2c + 1st deri c2p
!     fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
!     call Get_y_midp_P2C_1D          (fyp,      fgyc, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_PPP), fbc)
!     call Get_y_1st_derivative_C2P_1D(fgyc*fgyc,fgyp, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc*fbc)

!     err_Linf = ZERO
!     err_L2   = ZERO
!     do j = 1, dm%np(2)
!       yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
!       ref = TWO/scale * cos_wp(yp / scale + shift)* sin_wp(yp / scale + shift)
!       err = abs_wp(fgyp(j) - ref)
!       if(err > err_Linf) err_Linf = err
!       err_L2 = err_L2 + err**2
!       !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-interp-c2p ', j, yp, ref, fgyp(j), err !test
!     end do
!     err_L2 = sqrt_wp(err_L2 / dm%np(2)) 
!     write(wrt_unit(4), *) '# y-d(ff)dy ', dm%np(2), err_Linf, err_L2

!     !if(err_L2 > 1.0e-8_WP) then
!       !call Print_warning_msg("Test y-d(ff)dy failed.")
!       write(wrt_unit(2), *) '# y-d(ff)dy ', dm%np(2), err_Linf, err_L2
!       do j = 1, dm%np(2)
!         yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
!         ref = TWO/scale * cos_wp(yp / scale + shift)* sin_wp(yp / scale + shift)
!         err = abs_wp(fgyp(j) - ref)
!         write(wrt_unit(2),*) j, yp, ref, fgyp(j), err !test
!       end do
!     !end if

    close(wrt_unit(1))
    close(wrt_unit(2))
    close(wrt_unit(3))
    close(wrt_unit(4))

    deallocate(fxc )
    deallocate(fxp )
    deallocate(fgxc)
    deallocate(fgxp)
    deallocate(fyc )
    deallocate(fyp )
    deallocate(fgyc)
    deallocate(fgyp)
    deallocate(fzc )
    deallocate(fzp )
    deallocate(fgzc)
    deallocate(fgzp)

    return 
  end subroutine

!==========================================================================================================
!==========================================================================================================
!> \brief To test this subroutine for mid-point interpolation.
!>
!> This subroutine is called in \ref Test_algorithms. Define the logicals to choose
!> which test section is required. 
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     d             domain
!_______________________________________________________________________________
  subroutine Test_1st_derivative(dm)
    use parameters_constant_mod
    use udf_type_mod
    use math_mod
    use EvenOdd_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    integer :: i, j, k
    real(WP) :: xc, yc, zc
    real(WP) :: xp, yp, zp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(4)
    real(WP) :: scale, shift
    real(WP) :: fbc(4)

    real(WP), allocatable :: fxc (:), fyc (:), fzc (:)
    real(WP), allocatable :: fxp (:), fyp (:), fzp (:)
    real(WP), allocatable :: fgxc(:), fgyc(:), fgzc(:)
    real(WP), allocatable :: fgxp(:), fgyp(:), fgzp(:)

    dm%ibcx_Th(:, IBC_CCC) = IBC_DIRICHLET; if(is_even(dm%np(1))) dm%np(1)=dm%np(1)+1
    dm%ibcy_Th(:, IBC_CCC) = IBC_DIRICHLET; if(is_even(dm%np(2))) dm%np(2)=dm%np(2)+1
    dm%ibcz_Th(:, IBC_CCC) = IBC_DIRICHLET; if(is_even(dm%np(3))) dm%np(3)=dm%np(3)+1
    allocate ( fxc (dm%nc(1)), fyc (dm%nc(2)), fzc (dm%nc(3)) )
    allocate ( fxp (dm%np(1)), fyp (dm%np(2)), fzp (dm%np(3)) )
    allocate ( fgxc(dm%nc(1)), fgyc(dm%nc(2)), fgzc(dm%nc(3)) )
    allocate ( fgxp(dm%np(1)), fgyp(dm%np(2)), fgzp(dm%np(3)) )
    dm%h(1) = TWOPI / dm%nc(1)
    dm%h(2) = TWOPI / dm%nc(2)
    dm%h(3) = TWOPI / dm%nc(3)

    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcx_pr(1, :, :) = ZERO
        if(i==2) dm%fbcx_pr(2, :, :) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO

        if(i==1) dm%fbcx_pr(1, :, :) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcx_pr(2, :, :) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcx_pr(1, :, :) = ZERO
        if(i==2) dm%fbcx_pr(2, :, :) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    do i = 1, 2
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ZERO
        if(i==2) dm%fbcy_pr(:, 2, :) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcy_pr(:, 2, :) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ZERO
        if(i==2) dm%fbcy_pr(:, 2, :) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    do i = 1, 2
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ZERO
        if(i==2) dm%fbcz_pr(:, :, 2) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcz_pr(:, :, 2) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ZERO
        if(i==2) dm%fbcz_pr(:, :, 2) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    open (newunit = wrt_unit(4), file = 'test_1st_derivative.dat', position="append")
    write(wrt_unit(4), *) '# xbc type ', dm%ibcx_Th(1:2, IBC_CCC), " err_inf, err_L2"
    write(wrt_unit(4), *) '# ybc type ', dm%ibcy_Th(1:2, IBC_CCC), " err_inf, err_L2"
    write(wrt_unit(4), *) '# zbc type ', dm%ibcz_Th(1:2, IBC_CCC), " err_inf, err_L2"

    open (newunit = wrt_unit(1), file = 'test_1st_derivative_x.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_1st_derivative_y.dat', position="append")
    open (newunit = wrt_unit(3), file = 'test_1st_derivative_z.dat', position="append")

!----------------------------------------------------------------------------------------------------------
! c2c
!----------------------------------------------------------------------------------------------------------
    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_INTERIOR) then
        scale = THREE
        shift = ZERO
        if(i==1) then
          dm%fbcx_pr(1, :, :) = sin_wp ( dm%h(1) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcx_pr(3, :, :) = sin_wp ( dm%h(1) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcx_pr(2, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcx_pr(4, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i==1) then
          dm%fbcy_pr(:, 1, :) = sin_wp ( dm%h(2) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcy_pr(:, 3, :) = sin_wp ( dm%h(2) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcy_pr(:, 2, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcy_pr(:, 4, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i==1) then
          dm%fbcz_pr(:, :, 1) = sin_wp ( dm%h(3) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcz_pr(:, :,3) = sin_wp ( dm%h(3) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcz_pr(:, :, 2) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcz_pr(:, :, 4) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
    end do

! x direction, xc
    do i = 1, dm%nc(1)
      xc = dm%h(1) * (real(i - 1, WP) + HALF)
      fxc(i) = sin_wp ( xc / scale + shift)
    end do
! x: c2c
    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    call Get_x_1st_derivative_C2C_1D (fxc, fgxc, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%nc(1)
      xc = dm%h(1) * (real(i - 1, WP) + HALF)
      ref = ONE/scale * cos_wp(xc / scale + shift)
      err = abs_wp(fgxc(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-1stder-c2c ', i, xc, ref, fgxc(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(1)) 
    write(wrt_unit(4), *) '# x-1stder-c2c ', dm%nc(1), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-1stder-c2c failed.")
      write(wrt_unit(1), *) '# x-1stder-c2c ', dm%nc(1), err_Linf, err_L2
      do i = 1, dm%nc(1)
        xc = dm%h(1) * (real(i - 1, WP) + HALF)
        ref = ONE/scale * cos_wp(xc / scale + shift)
        err = abs_wp(fgxc(i) - ref)
        write(wrt_unit(1),*) i, xc, ref, fgxc(i), err !test
      end do
    !end if

! y direction, yc
    do j = 1, dm%nc(2)
      yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
      fyc(j) = sin_wp ( yc / scale + shift)
    end do
! y: c2c
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_1st_derivative_C2C_1D (fyc, fgyc, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%nc(2)
      yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
      ref = ONE/scale * cos_wp(yc / scale + shift)
      err = abs_wp(fgyc(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-1stder-c2c ', j, yc, ref, fgyc(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(2)) 
    write(wrt_unit(4), *) '# y-1stder-c2c ', dm%nc(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-1stder-c2c failed.")
      write(wrt_unit(2), *) '# y-1stder-c2c ', dm%nc(2), err_Linf, err_L2
      do j = 1, dm%nc(2)
        yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
        ref = ONE/scale * cos_wp(yc / scale + shift)
        err = abs_wp(fgyc(j) - ref)
        write(wrt_unit(2),*) j, yc, ref, fgyc(j), err !test
      end do
    !end if
    

! z direction, zc
    do k = 1, dm%nc(3)
      zc = dm%h(3) * (real(k - 1, WP) + HALF)
      fzc(k) = sin_wp ( zc / scale + shift)
    end do
! z: c2c
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_1st_derivative_C2C_1D (fzc, fgzc, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%nc(3)
      zc = dm%h(3) * (real(k - 1, WP) + HALF)
      ref = ONE/scale * cos_wp(zc / scale + shift)
      err = abs_wp(fgzc(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-1stder-c2c ', k, zc, ref, fgzc(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(3)) 
    write(wrt_unit(4), *) '# z-1stder-c2c ', dm%nc(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-1stder-c2c failed.")
      write(wrt_unit(3), *) '# z-1stder-c2c ', dm%nc(3), err_Linf, err_L2
      do k = 1, dm%nc(3)
        zc = dm%h(3) * (real(k - 1, WP) + HALF)
        ref = ONE/scale * cos_wp(zc / scale + shift)
        err = abs_wp(fgzc(k) - ref)
        write(wrt_unit(3),*) k, zc, ref, fgzc(k), err !test
      end do
    !end if
    
!----------------------------------------------------------------------------------------------------------
! c2p
!----------------------------------------------------------------------------------------------------------
! x: c2p
    ! if (dm%ibcx(1, 5) == IBC_INTERIOR) then
    ! do i = 1, 4 
    !   write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-1stder-c2p ', i, lbcx(i), fbc(i), fbc(i), zero
    ! end do
    ! end if

    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    call Get_x_1st_derivative_C2P_1D (fxc, fgxp, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%np(1)
      xp = dm%h(1) * real(i - 1, WP)
      ref = ONE/scale * cos_wp(xp / scale + shift)
      err = abs_wp(fgxp(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-1stder-c2p ', i, xp, ref, fgxp(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(1)) 
    write(wrt_unit(4), *) '# x-1stder-c2p ', dm%np(1), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-1stder-c2p failed.")
      write(wrt_unit(1), *) '# x-1stder-c2p ', dm%np(1), err_Linf, err_L2
      do i = 1, dm%np(1)
        xp = dm%h(1) * real(i - 1, WP)
        ref = ONE/scale * cos_wp(xp / scale + shift)
        err = abs_wp(fgxp(i) - ref)
        write(wrt_unit(1),*) i, xp, ref, fgxp(i), err !test
      end do
    !end if
    

! y: c2p
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_1st_derivative_C2P_1D (fyc, fgyp, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      ref = ONE/scale * cos_wp(yp / scale + shift)
      err = abs_wp(fgyp(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-1stder-c2p ', j, yp, ref, fgyp(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(2)) 
    write(wrt_unit(4), *) '# y-1stder-c2p ', dm%np(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-1stder-c2p failed.")
      write(wrt_unit(2), *) '# y-1stder-c2p ', dm%np(2), err_Linf, err_L2
      do j = 1, dm%np(2)
        yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
        ref = ONE/scale * cos_wp(yp / scale + shift)
        err = abs_wp(fgyp(j) - ref)
        write(wrt_unit(2),*) j, yp, ref, fgyp(j), err !test
      end do
    !end if
    

! z: c2p
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_1st_derivative_C2P_1D (fzc, fgzp, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%np(3)
      zp = dm%h(3) * real(k - 1, WP)
      ref = ONE/scale * cos_wp(zp / scale + shift)
      err = abs_wp(fgzp(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-1stder-c2p ', k, zp, ref, fgzp(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(3)) 
    write(wrt_unit(4), *) '# z-1stder-c2p ', dm%np(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-1stder-c2p failed.")
      write(wrt_unit(3), *) '# z-1stder-c2p ', dm%np(3), err_Linf, err_L2
      do k = 1, dm%np(3)
        zp = dm%h(3) * real(k - 1, WP)
        ref = ONE/scale * cos_wp(zp / scale + shift)
        err = abs_wp(fgzp(k) - ref)
        if(err > err_Linf) err_Linf = err
        err_L2 = err_L2 + err**2
        write(wrt_unit(3),*) k, zp, ref, fgzp(k), err !test
      end do
    !end if
    

!----------------------------------------------------------------------------------------------------------
! p2p
!----------------------------------------------------------------------------------------------------------
    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_INTERIOR) then
        scale = THREE
        shift = ZERO
        if(i == 1) then
          dm%fbcx_pr(1, :, :) = sin_wp ( dm%h(1) * (real( -1, WP) ) / scale + shift)
          dm%fbcx_pr(3, :, :) = sin_wp ( dm%h(1) * (real( -2, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcx_pr(2, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 1, WP) ) / scale + shift)
          dm%fbcx_pr(4, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 2, WP) ) / scale + shift)
        end if
      end if
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i == 1) then
          dm%fbcy_pr(:, 1, :) = sin_wp ( dm%h(2) * (real(-1, WP) ) / scale + shift)
          dm%fbcy_pr(:, 3, :) = sin_wp ( dm%h(2) * (real(-2, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcy_pr(:, 2, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 1, WP) ) / scale + shift)
          dm%fbcy_pr(:, 4, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 2, WP) ) / scale + shift)
        end if
      end if
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i == 1) then
          dm%fbcz_pr(:, :, 1) = sin_wp ( dm%h(3) * (real( 0 - 1, WP) ) / scale + shift)
          dm%fbcz_pr(:, :,3) = sin_wp ( dm%h(3) * (real(-1 - 1, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcz_pr(:, :, 2) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 1, WP) ) / scale + shift)
          dm%fbcz_pr(:, :, 4) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 2, WP) ) / scale + shift)
        end if
      end if
    end do

 ! x direction, xp
    do i = 1, dm%np(1)
      xp = dm%h(1) * real(i - 1, WP)
      fxp(i) = sin_wp ( xp / scale + shift)
    end do
! x: p2p
    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    call Get_x_1st_derivative_P2P_1D (fxp, fgxp, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%np(1)
      xp = dm%h(1) * real(i - 1, WP)
      ref = ONE/scale * cos_wp(xp / scale + shift)
      err = abs_wp(fgxp(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-1stder-p2p', i, xp, ref, fgxp(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(1)) 
    write(wrt_unit(4), *) '# x-1stder-p2p ', dm%np(1), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-1stder-p2p failed.")
      write(wrt_unit(1), *) '# x-1stder-p2p ', dm%np(1), err_Linf, err_L2
      do i = 1, dm%np(1)
        xp = dm%h(1) * real(i - 1, WP)
        ref = ONE/scale * cos_wp(xp / scale + shift)
        err = abs_wp(fgxp(i) - ref)
        write(wrt_unit(1),*) i, xp, ref, fgxp(i), err !test
      end do
    !end if
    

! y direction, yp
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      fyp(j) = sin_wp ( yp / scale + shift)
    end do
! y: p2p
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_1st_derivative_P2P_1D (fyp, fgyp, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      ref = ONE/scale * cos_wp(yp / scale + shift)
      err = abs_wp(fgyp(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-1stder-p2p ', j, yp, ref, fgyp(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(2)) 
    write(wrt_unit(4), *) '# y-1stder-p2p ', dm%np(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-1stder-p2p failed.")
      write(wrt_unit(2), *) '# y-1stder-p2p ', dm%np(2), err_Linf, err_L2
      do j = 1, dm%np(2)
        yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
        ref = ONE/scale * cos_wp(yp / scale + shift)
        err = abs_wp(fgyp(j) - ref)
        write(wrt_unit(2),*) j, yp, ref, fgyp(j), err !test
      end do
    !end if
    
! z direction, zp
    do k = 1, dm%np(3)
      zp = dm%h(3) * real(k - 1, WP)
      fzp(k) = sin_wp ( zp / scale + shift)
    end do
! z: p2p
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_1st_derivative_P2P_1D (fzp, fgzp, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_PPP), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%np(3)
      zp = dm%h(3) * real(k - 1, WP)
      ref = ONE/scale * cos_wp(zp / scale + shift)
      err = abs_wp(fgzp(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-1stder-p2p ', k, zp, ref, fgzp(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(3)) 
    write(wrt_unit(4), *) '# z-1stder-p2p ', dm%np(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-1stder-p2p failed.")
      write(wrt_unit(3), *) '# z-1stder-p2p ', dm%np(3), err_Linf, err_L2
      do k = 1, dm%np(3)
        zp = dm%h(3) * real(k - 1, WP)
        ref = ONE/scale * cos_wp(zp / scale + shift)
        err = abs_wp(fgzp(k) - ref)
        if(err > err_Linf) err_Linf = err
        err_L2 = err_L2 + err**2
        write(wrt_unit(3),*) k, zp, ref, fgzp(k), err !test
      end do
    !end if
    
!----------------------------------------------------------------------------------------------------------
! p2c
!----------------------------------------------------------------------------------------------------------
! x: p2c
    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    call Get_x_1st_derivative_P2C_1D (fxp, fgxc, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_PCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%nc(1)
      xc = dm%h(1) * (real(i - 1, WP) + HALF)
      ref = ONE/scale * cos_wp(xc / scale + shift)
      err = abs_wp(fgxc(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-1stder-p2c ', i, xc, ref, fgxc(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(1)) 
    write(wrt_unit(4), *) '# x-1stder-p2c ', dm%nc(1), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-1stder-p2c failed.")
      write(wrt_unit(1), *) '# x-1stder-p2c ', dm%nc(1), err_Linf, err_L2
      do i = 1, dm%nc(1)
        xc = dm%h(1) * (real(i - 1, WP) + HALF)
        ref = ONE/scale * cos_wp(xc / scale + shift)
        err = abs_wp(fgxc(i) - ref)
        if(err > err_Linf) err_Linf = err
        err_L2 = err_L2 + err**2
        write(wrt_unit(1),*) i, xc, ref, fgxc(i), err !test
      end do
    !end if

! y: p2c
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_1st_derivative_P2C_1D (fyp, fgyc, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%nc(2)
      yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
      ref = ONE/scale * cos_wp(yc / scale + shift)
      err = abs_wp(fgyc(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-1stder-p2c ', j, yc, ref, fgyc(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(2)) 
    write(wrt_unit(4), *) '# y-1stder-p2c ', dm%nc(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-1stder-p2c failed.")
      write(wrt_unit(2), *) '# y-1stder-p2c ', dm%nc(2), err_Linf, err_L2
      do j = 1, dm%nc(2)
        yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
        ref = ONE/scale * cos_wp(yc / scale + shift)
        err = abs_wp(fgyc(j) - ref)
        write(wrt_unit(2),*) j, yc, ref, fgyc(j), err !test
      end do
    !end if
    

! z: p2c
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_1st_derivative_P2C_1D (fzp, fgzc, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_PPP), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%nc(3)
      zc = dm%h(3) * (real(k - 1, WP) + HALF)
      ref = ONE/scale * cos_wp(zc / scale + shift)
      err = abs_wp(fgzc(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-1stder-p2c ', k, zc, ref, fgzc(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(3)) 
    write(wrt_unit(4), *) '# z-1stder-p2c ', dm%nc(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-1stder-p2c failed.")
      write(wrt_unit(3), *) '# z-1stder-p2c ', dm%nc(3), err_Linf, err_L2
      do k = 1, dm%nc(3)
        zc = dm%h(3) * (real(k - 1, WP) + HALF)
        ref = ONE/scale * cos_wp(zc / scale + shift)
        err = abs_wp(fgzc(k) - ref)
        write(wrt_unit(3),*) k, zc, ref, fgzc(k), err !test
      end do
    !end if
    

    close(wrt_unit(1))
    close(wrt_unit(2))
    close(wrt_unit(3))
    close(wrt_unit(4))

    deallocate(fxc )
    deallocate(fxp )
    deallocate(fgxc)
    deallocate(fgxp)
    deallocate(fyc )
    deallocate(fyp )
    deallocate(fgyc)
    deallocate(fgyp)
    deallocate(fzc )
    deallocate(fzp )
    deallocate(fgzc)
    deallocate(fgzp)


    return 
  end subroutine

!==========================================================================================================
!==========================================================================================================
!> \brief To test this subroutine for mid-point interpolation.
!>
!> This subroutine is called in \ref Test_algorithms. Define the logicals to choose
!> which test section is required. 
!>
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     d             domain
!_______________________________________________________________________________
  subroutine Test_2nd_derivative(dm)
    use parameters_constant_mod
    use udf_type_mod
    use math_mod
    use EvenOdd_mod
    implicit none
    type(t_domain), intent(inout) :: dm
    integer :: i, j, k
    real(WP) :: xc, yc, zc
    real(WP) :: xp, yp, zp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(4)

    real(WP) :: scale, shift
    real(WP) :: fbc(4)

    real(WP), allocatable :: fxc (:), fyc (:), fzc (:)
    real(WP), allocatable :: fxp (:), fyp (:), fzp (:)
    real(WP), allocatable :: fgxc(:), fgyc(:), fgzc(:)
    real(WP), allocatable :: fgxp(:), fgyp(:), fgzp(:)

    dm%ibcx_Th(:, IBC_CCC) = IBC_DIRICHLET; if(is_even(dm%np(1))) dm%np(1)=dm%np(1)+1
    dm%ibcy_Th(:, IBC_CCC) = IBC_DIRICHLET; if(is_even(dm%np(2))) dm%np(2)=dm%np(2)+1
    dm%ibcz_Th(:, IBC_CCC) = IBC_DIRICHLET; if(is_even(dm%np(3))) dm%np(3)=dm%np(3)+1
    allocate ( fxc (dm%nc(1)), fyc (dm%nc(2)), fzc (dm%nc(3)) )
    allocate ( fxp (dm%np(1)), fyp (dm%np(2)), fzp (dm%np(3)) )
    allocate ( fgxc(dm%nc(1)), fgyc(dm%nc(2)), fgzc(dm%nc(3)) )
    allocate ( fgxp(dm%np(1)), fgyp(dm%np(2)), fgzp(dm%np(3)) )
    dm%h(1) = TWOPI / dm%nc(1)
    dm%h(2) = TWOPI / dm%nc(2)
    dm%h(3) = TWOPI / dm%nc(3)

    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcx_Th(i, IBC_CCC)== IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcx_pr(1, :, :) = ZERO
        if(i==2) dm%fbcx_pr(2, :, :) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcx_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO

        if(i==1) dm%fbcx_pr(1, :, :) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcx_pr(2, :, :) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcx_pr(1, :, :) = ZERO
        if(i==2) dm%fbcx_pr(2, :, :) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    do i = 1, 2
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ZERO
        if(i==2) dm%fbcy_pr(:, 2, :) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcy_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcy_pr(:, 2, :) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcy_pr(:, 1, :) = ZERO
        if(i==2) dm%fbcy_pr(:, 2, :) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    do i = 1, 2
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_PERIODIC) then
        scale = ONE
        shift = ZERO
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_SYMMETRIC) then
        scale = ONE
        shift = PI * HALF
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_ASYMMETRIC) then
        scale = TWO
        shift = ZERO
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_DIRICHLET) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ZERO
        if(i==2) dm%fbcz_pr(:, :, 2) = sin_wp(TWOPI * ONE_THIRD)
      else if (dm%ibcz_Th(i, IBC_CCC) == IBC_NEUMANN) then
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ONE_THIRD * cos_wp(ZERO  * ONE_THIRD)
        if(i==2) dm%fbcz_pr(:, :, 2) = ONE_THIRD * cos_wp(TWOPI * ONE_THIRD)
      else 
        scale = THREE
        shift = ZERO
        if(i==1) dm%fbcz_pr(:, :, 1) = ZERO
        if(i==2) dm%fbcz_pr(:, :, 2) = sin_wp(TWOPI * ONE_THIRD)
      end if
    end do

    open (newunit = wrt_unit(4), file = 'test_2nd_derivative.dat', position="append")
    write(wrt_unit(4), *) '# xbc type ', dm%ibcx_Th(1:2, IBC_CCC), " err_inf, err_L2"
    write(wrt_unit(4), *) '# ybc type ', dm%ibcy_Th(1:2, IBC_CCC), " err_inf, err_L2"
    write(wrt_unit(4), *) '# zbc type ', dm%ibcz_Th(1:2, IBC_CCC), " err_inf, err_L2"

    open (newunit = wrt_unit(1), file = 'test_2nd_derivative_x.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_2nd_derivative_y.dat', position="append")
    open (newunit = wrt_unit(3), file = 'test_2nd_derivative_z.dat', position="append")

!----------------------------------------------------------------------------------------------------------
! c2c
!----------------------------------------------------------------------------------------------------------
    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_INTERIOR) then
        scale = THREE
        shift = ZERO
        if(i==1) then
          dm%fbcx_pr(1, :, :) = sin_wp ( dm%h(1) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcx_pr(3, :, :) = sin_wp ( dm%h(1) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcx_pr(2, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcx_pr(4, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i==1) then
          dm%fbcy_pr(:, 1, :) = sin_wp ( dm%h(2) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcy_pr(:, 3, :) = sin_wp ( dm%h(2) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcy_pr(:, 2, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcy_pr(:, 4, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i==1) then
          dm%fbcz_pr(:, :, 1) = sin_wp ( dm%h(3) * (real( 0 - 1, WP) + HALF) / scale + shift)
          dm%fbcz_pr(:, :,3) = sin_wp ( dm%h(3) * (real(-1 - 1, WP) + HALF) / scale + shift)
        else if(i ==2) then
          dm%fbcz_pr(:, :, 2) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 1 - 1, WP) + HALF) / scale + shift)
          dm%fbcz_pr(:, :, 4) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 2 - 1, WP) + HALF) / scale + shift)
        end if
      end if
    end do

! x direction, xc
    do i = 1, dm%nc(1)
      xc = dm%h(1) * (real(i - 1, WP) + HALF)
      fxc(i) = sin_wp ( xc / scale + shift)
    end do
! x: c2c
    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    call Get_x_2nd_derivative_C2C_1D (fxc, fgxc, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%nc(1)
      xc = dm%h(1) * (real(i - 1, WP) + HALF)
      ref = - (ONE/scale)**2 * sin_wp(xc / scale + shift)
      err = abs_wp(fgxc(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-2ndder-c2c ', i, xc, ref, fgxc(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(1)) 
    write(wrt_unit(4), *) '# x-2ndder-c2c ', dm%nc(1), err_Linf, err_L2
    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-2ndder-c2c failed.")
      do i = 1, dm%nc(1)
        xc = dm%h(1) * (real(i - 1, WP) + HALF)
        ref = - (ONE/scale)**2 * sin_wp(xc / scale + shift)
        err = abs_wp(fgxc(i) - ref)
        write(wrt_unit(1),*) i, xc, ref, fgxc(i), err !test
      end do
    !end if
    

! y direction, yc
    do j = 1, dm%nc(2)
      yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
      fyc(j) = sin_wp ( yc / scale + shift)
    end do
! y: c2c
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_2nd_derivative_C2C_1D (fyc, fgyc, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%nc(2)
      yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
      ref = - (ONE/scale)**2 * sin_wp(yc / scale + shift)
      err = abs_wp(fgyc(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
     ! !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-2ndder-c2c ', j, yc, ref, fgyc(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(2)) 
    write(wrt_unit(4), *) '# y-2ndder-c2c ', dm%nc(2), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-2ndder-c2c failed.")
      do j = 1, dm%nc(2)
        yc = dm%h(2) * (real(j - 1, WP) + HALF) !yc = dm%yc(j)
        ref = - (ONE/scale)**2 * sin_wp(yc / scale + shift)
        err = abs_wp(fgyc(j) - ref)
        write(wrt_unit(2),*) j, yc, ref, fgyc(j), err !test
      end do
    !end if
    
! z direction, zc
    do k = 1, dm%nc(3)
      zc = dm%h(3) * (real(k - 1, WP) + HALF)
      fzc(k) = sin_wp ( zc / scale + shift)
    end do
! z: c2c
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_2nd_derivative_C2C_1D (fzc, fgzc, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%nc(3)
      zc = dm%h(3) * (real(k - 1, WP) + HALF)
      ref = - (ONE/scale)**2 * sin_wp(zc / scale + shift)
      err = abs_wp(fgzc(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-2ndder-c2c ', k, zc, ref, fgzc(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%nc(3)) 
    write(wrt_unit(4), *) '# z-2ndder-c2c ', dm%nc(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-2ndder-c2c failed.")
      do k = 1, dm%nc(3)
        zc = dm%h(3) * (real(k - 1, WP) + HALF)
        ref = - (ONE/scale)**2 * sin_wp(zc / scale + shift)
        err = abs_wp(fgzc(k) - ref)
        write(wrt_unit(3),*) k, zc, ref, fgzc(k), err !test
      end do
    !end if
    
!----------------------------------------------------------------------------------------------------------
! p2p
!----------------------------------------------------------------------------------------------------------
    do i = 1, 2
      if (dm%ibcx_Th(i, IBC_CCC) == IBC_INTERIOR) then
        scale = THREE
        shift = ZERO
        if(i == 1) then
          dm%fbcx_pr(1, :, :) = sin_wp ( dm%h(1) * (real( -1, WP) ) / scale + shift)
          dm%fbcx_pr(3, :, :) = sin_wp ( dm%h(1) * (real( -2, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcx_pr(2, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 1, WP) ) / scale + shift)
          dm%fbcx_pr(4, :, :) = sin_wp ( dm%h(1) * (real( dm%nc(1) + 2, WP) ) / scale + shift)
        end if
      end if
      if (dm%ibcy_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i == 1) then
          dm%fbcy_pr(:, 1, :) = sin_wp ( dm%h(2) * (real(-1, WP) ) / scale + shift)
          dm%fbcy_pr(:, 3, :) = sin_wp ( dm%h(2) * (real(-2, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcy_pr(:, 2, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 1, WP) ) / scale + shift)
          dm%fbcy_pr(:, 4, :) = sin_wp ( dm%h(2) * (real( dm%nc(2) + 2, WP) ) / scale + shift)
        end if
      end if
      if (dm%ibcz_Th(i, IBC_CCC) == IBC_INTERIOR) then     
        scale = THREE
        shift = ZERO 
        if(i == 1) then
          dm%fbcz_pr(:, :, 1) = sin_wp ( dm%h(3) * (real( 0 - 1, WP) ) / scale + shift)
          dm%fbcz_pr(:, :,3) = sin_wp ( dm%h(3) * (real(-1 - 1, WP) ) / scale + shift)
        else if(i == 2) then
          dm%fbcz_pr(:, :, 2) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 1, WP) ) / scale + shift)
          dm%fbcz_pr(:, :, 4) = sin_wp ( dm%h(3) * (real( dm%nc(3) + 2, WP) ) / scale + shift)
        end if
      end if
    end do
! x direction, xp
    do i = 1, dm%np(1)
      xp = dm%h(1) * real(i - 1, WP)
      fxp(i) = sin_wp ( xp / scale + shift)
    end do
! x: p2p
    fbc(1:4) = dm%fbcx_pr(1:4, 1, 1)
    call Get_x_2nd_derivative_P2P_1D (fxp, fgxp, dm, dm%iAccuracy, dm%ibcx_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do i = 1, dm%np(1)
      xp = dm%h(1) * real(i - 1, WP)
      ref = - (ONE/scale)**2 * sin_wp(xp / scale + shift)
      err = abs_wp(fgxp(i) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'x-2ndder-p2p ', i, xp, ref, fgxp(i), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(1)) 
    write(wrt_unit(4), *) '# x-2ndder-p2p ', dm%np(1), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test x-2ndder-p2p failed.")
      do i = 1, dm%np(1)
        xp = dm%h(1) * real(i - 1, WP)
        ref = - (ONE/scale)**2 * sin_wp(xp / scale + shift)
        err = abs_wp(fgxp(i) - ref)
        write(wrt_unit(1),*) i, xp, ref, fgxp(i), err !test
      end do
    !end if
    
! y direction, yp
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      fyp(j) = sin_wp ( yp / scale + shift)
    end do
! y: p2p
    fbc(1:4) = dm%fbcy_pr(1, 1:4, 1)
    call Get_y_2nd_derivative_P2P_1D (fyp, fgyp, dm, dm%iAccuracy, dm%ibcy_Th(:, IBC_CCC), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do j = 1, dm%np(2)
      yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
      ref = - (ONE/scale)**2 * sin_wp(yp / scale + shift)
      err = abs_wp(fgyp(j) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'y-2ndder-p2p ', j, yp, ref, fgyp(j), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(2)) 
    write(wrt_unit(4), *) '# y-2ndder-p2p ', dm%np(2), err_Linf, err_L2
    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test y-2ndder-p2p failed.")
      do j = 1, dm%np(2)
        yp = dm%h(2) * real(j - 1, WP) !yp = dm%yp(j)
        ref = - (ONE/scale)**2 * sin_wp(yp / scale + shift)
        err = abs_wp(fgyp(j) - ref)
        write(wrt_unit(2),*) j, yp, ref, fgyp(j), err !test
      end do
    !end if
    


! z direction, zp
    do k = 1, dm%np(3)
      zp = dm%h(3) * real(k - 1, WP)
      fzp(k) = sin_wp ( zp / scale + shift)
    end do
! z: p2p
    fbc(1:4) = dm%fbcz_pr(1, 1, 1:4)
    call Get_z_2nd_derivative_P2P_1D (fzp, fgzp, dm, dm%iAccuracy, dm%ibcz_Th(:, IBC_PPP), fbc)
    err_Linf = ZERO
    err_L2   = ZERO
    do k = 1, dm%np(3)
      zp = dm%h(3) * real(k - 1, WP)
      ref = - (ONE/scale)**2 * sin_wp(zp / scale + shift)
      err = abs_wp(fgzp(k) - ref)
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
      !write(wrt_unit,'(A,1I5.1,4ES13.5)') 'z-2ndder-p2p ', k, zp, ref, fgzp(k), err !test
    end do
    err_L2 = sqrt_wp(err_L2 / dm%np(3))
    write(wrt_unit(4), *) '# z-2ndder-p2p ', dm%np(3), err_Linf, err_L2

    !if(err_L2 > 1.0e-8_WP) then
      !call Print_warning_msg("Test z-2ndder-p2p failed.")
      do k = 1, dm%np(3)
        zp = dm%h(3) * real(k - 1, WP)
        ref = - (ONE/scale)**2 * sin_wp(zp / scale + shift)
        err = abs_wp(fgzp(k) - ref)
        write(wrt_unit(3),*) k, zp, ref, fgzp(k), err !test
      end do
    !end if
    
    close(wrt_unit(1))
    close(wrt_unit(2))
    close(wrt_unit(3))
    close(wrt_unit(4))


    deallocate(fxc )
    deallocate(fxp )
    deallocate(fgxc)
    deallocate(fgxp)
    deallocate(fyc )
    deallocate(fyp )
    deallocate(fgyc)
    deallocate(fgyp)
    deallocate(fzc )
    deallocate(fzp )
    deallocate(fgzc)
    deallocate(fgzp)

    return 
  end subroutine

end module