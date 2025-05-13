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
  use print_msg_mod
  use parameters_constant_mod
  implicit none

  private

  logical, save :: bc_ghost_cd = .true.
  logical, save :: bc_intp_upw = .false.

  logical, save :: flg_wrn_xmidp_c2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_xmidp_c2p_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_xmidp_c2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_xmidp_p2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_xmidp_p2c_neumann   (2) = (/.false., .false./)
 
  logical, save :: flg_wrn_ymidp_c2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_ymidp_c2p_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_ymidp_c2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_ymidp_p2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_ymidp_p2c_neumann   (2) = (/.false., .false./)
 
  logical, save :: flg_wrn_zmidp_c2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_zmidp_c2p_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_zmidp_c2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_zmidp_p2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_zmidp_p2c_neumann   (2) = (/.false., .false./)
 
  logical, save :: flg_wrn_x1der_c2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_c2c_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_c2c_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_p2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_p2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_c2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_c2p_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_c2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_p2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_x1der_p2c_neumann   (2) = (/.false., .false./)

  logical, save :: flg_wrn_y1der_c2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_c2c_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_c2c_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_p2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_p2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_c2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_c2p_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_c2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_p2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_y1der_p2c_neumann   (2) = (/.false., .false./)

  logical, save :: flg_wrn_z1der_c2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_c2c_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_c2c_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_p2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_p2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_c2p_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_c2p_dirichlet (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_c2p_neumann   (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_p2c_interior  (2) = (/.false., .false./)
  logical, save :: flg_wrn_z1der_p2c_neumann   (2) = (/.false., .false./)
!----------------------------------------------------------------------------------------------------------
! basic coefficients for TDMA of 1st deriviative  
! to store coefficients for TDMA
!     d1fC2C vs d1rC2C :
!       f : coefficients in the LHS, unknown side.
!       r : coefficients in the RHS, known side. 
! eg, d1fC2C(5, 3, 5)
!     First column: 1:2 for L1,2, one side b.c.
!                   4:5 for L4,5, the other side b.c.
!                   3   for L-bulk, all other cells
!     Second column: 1 for coefficients of LHS f^(1)_{i-1}
!                    2 for coefficients of LHS f^(1)_{i}
!                    3 for coefficients of LHS f^(1)_{i+1}
!     Third column:  for b.c. types
!                    0 = IBC_INTERIOR  
!                    1 = IBC_PERIODIC  
!                    2 = IBC_SYMMETRIC 
!                    3 = IBC_ASYMMETRIC
!                    4 = IBC_DIRICHLET 
!                    5 = IBC_NEUMANN   
!                    6 = IBC_INTRPL    
!     Fourth column:  Accuracy 
!                    1 = IACCU_CD2
!                    2 = IACCU_CD4
!                    3 = IACCU_CP4
!                    4 = IACCU_CP6
!----------------------------------------------------------------------------------------------------------
  integer, parameter :: NL = 5   ! rows/line types
  integer, parameter :: NS = 3   ! how many coefficients
  integer, parameter :: NBCS = 0 ! bc index, start
  integer, parameter :: NBCE = 6 ! bc index, end
  integer, parameter :: NACC = 4 ! accuracy types
!----------------------------------------------------------------------------------------------------------
! for 1st derivative
!----------------------------------------------------------------------------------------------------------
  ! collocated C2C
  real(WP), save, public :: d1fC2C(NL,   NS, NBCS:NBCE, NACC)
  real(WP), save, public :: d1rC2C(NL, 2*NS, NBCS:NBCE, NACC)
  ! collocated P2P
  real(WP), save, public :: d1fP2P(NL,   NS, NBCS:NBCE, NACC)
  real(WP), save, public :: d1rP2P(NL, 2*NS, NBCS:NBCE, NACC)
  ! staggered C2P
  real(WP), save, public :: d1fC2P(NL,   NS, NBCS:NBCE, NACC)
  real(WP), save, public :: d1rC2P(NL, 2*NS, NBCS:NBCE, NACC)
  ! staggered P2C
  real(WP), save, public :: d1fP2C(NL,   NS, NBCS:NBCE, NACC)
  real(WP), save, public :: d1rP2C(NL, 2*NS, NBCS:NBCE, NACC)
!----------------------------------------------------------------------------------------------------------
! for iterpolation
!----------------------------------------------------------------------------------------------------------
  ! interpolation P2C
  real(WP), save, public :: m1fP2C(NL,   NS, NBCS:NBCE, NACC)
  real(WP), save, public :: m1rP2C(NL, 2*NS, NBCS:NBCE, NACC)
  ! interpolation C2P
  real(WP), save, public :: m1fC2P(NL,   NS, NBCS:NBCE, NACC)
  real(WP), save, public :: m1rC2P(NL, 2*NS, NBCS:NBCE, NACC)

!----------------------------------------------------------------------------------------------------------
! coefficients array for TDMA of 1st deriviative  
! to store coefficients array for TDMA
!----------------------------------------------------------------------------------------------------------
  type t_xtdma_lhs
!----------------------------------------------------------------------------------------------------------
!   x : pre-processed TDMA LHS Matrix for 1st deriviative
!----------------------------------------------------------------------------------------------------------
    real(WP), allocatable :: ad1x_P2P(:, :, :, :)
    real(WP), allocatable :: bd1x_P2P(:, :, :, :)
    real(WP), allocatable :: cd1x_P2P(:, :, :, :)
    real(WP), allocatable :: dd1x_P2P(:, :, :, :)

    real(WP), allocatable :: ad1x_C2C(:, :, :, :)
    real(WP), allocatable :: bd1x_C2C(:, :, :, :)
    real(WP), allocatable :: cd1x_C2C(:, :, :, :)
    real(WP), allocatable :: dd1x_C2C(:, :, :, :)

    real(WP), allocatable :: ad1x_P2C(:, :, :, :)
    real(WP), allocatable :: bd1x_P2C(:, :, :, :)
    real(WP), allocatable :: cd1x_P2C(:, :, :, :)
    real(WP), allocatable :: dd1x_P2C(:, :, :, :)

    real(WP), allocatable :: ad1x_C2P(:, :, :, :)
    real(WP), allocatable :: bd1x_C2P(:, :, :, :)
    real(WP), allocatable :: cd1x_C2P(:, :, :, :)
    real(WP), allocatable :: dd1x_C2P(:, :, :, :)
!----------------------------------------------------------------------------------------------------------
!   x : pre-processed TDMA LHS Matrix for mid-point interpolation
!----------------------------------------------------------------------------------------------------------
    real(WP), allocatable :: am1x_P2C(:, :, :, :)
    real(WP), allocatable :: bm1x_P2C(:, :, :, :)
    real(WP), allocatable :: cm1x_P2C(:, :, :, :)
    real(WP), allocatable :: dm2x_P2C(:, :, :, :)

    real(WP), allocatable :: am1x_C2P(:, :, :, :)
    real(WP), allocatable :: bm1x_C2P(:, :, :, :)
    real(WP), allocatable :: cm1x_C2P(:, :, :, :)
    real(WP), allocatable :: dm2x_C2P(:, :, :, :)
  end type t_xtdma_lhs

  type(t_xtdma_lhs), allocatable :: xtdma_lhs(:) 

!----------------------------------------------------------------------------------------------------------
! y : pre-processed TDMA LHS Matrix for 1st deriviative
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: ad1y_P2P(:, :, :, :)
  real(WP), allocatable :: bd1y_P2P(:, :, :, :)
  real(WP), allocatable :: cd1y_P2P(:, :, :, :)
  real(WP), allocatable :: dd1y_P2P(:, :, :, :)

  real(WP), allocatable :: ad1y_C2C(:, :, :, :)
  real(WP), allocatable :: bd1y_C2C(:, :, :, :)
  real(WP), allocatable :: cd1y_C2C(:, :, :, :)
  real(WP), allocatable :: dd1y_C2C(:, :, :, :)

  real(WP), allocatable :: ad1y_P2C(:, :, :, :)
  real(WP), allocatable :: bd1y_P2C(:, :, :, :)
  real(WP), allocatable :: cd1y_P2C(:, :, :, :)
  real(WP), allocatable :: dd1y_P2C(:, :, :, :)

  real(WP), allocatable :: ad1y_C2P(:, :, :, :)
  real(WP), allocatable :: bd1y_C2P(:, :, :, :)
  real(WP), allocatable :: cd1y_C2P(:, :, :, :)
  real(WP), allocatable :: dd1y_C2P(:, :, :, :)
!----------------------------------------------------------------------------------------------------------
! y : pre-processed TDMA LHS Matrix for mid-point interpolation
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: am1y_P2C(:, :, :, :)
  real(WP), allocatable :: bm1y_P2C(:, :, :, :)
  real(WP), allocatable :: cm1y_P2C(:, :, :, :)
  real(WP), allocatable :: dm1y_P2C(:, :, :, :)

  real(WP), allocatable :: am1y_C2P(:, :, :, :)
  real(WP), allocatable :: bm1y_C2P(:, :, :, :)
  real(WP), allocatable :: cm1y_C2P(:, :, :, :)
  real(WP), allocatable :: dm1y_C2P(:, :, :, :)
!----------------------------------------------------------------------------------------------------------
! z : pre-processed TDMA LHS Matrix for 1st deriviative
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: ad1z_P2P(:, :, :, :)
  real(WP), allocatable :: bd1z_P2P(:, :, :, :)
  real(WP), allocatable :: cd1z_P2P(:, :, :, :)
  real(WP), allocatable :: dd1z_P2P(:, :, :, :)

  real(WP), allocatable :: ad1z_C2C(:, :, :, :)
  real(WP), allocatable :: bd1z_C2C(:, :, :, :)
  real(WP), allocatable :: cd1z_C2C(:, :, :, :)
  real(WP), allocatable :: dd1z_C2C(:, :, :, :)

  real(WP), allocatable :: ad1z_P2C(:, :, :, :)
  real(WP), allocatable :: bd1z_P2C(:, :, :, :)
  real(WP), allocatable :: cd1z_P2C(:, :, :, :)
  real(WP), allocatable :: dd1z_P2C(:, :, :, :)

  real(WP), allocatable :: ad1z_C2P(:, :, :, :)
  real(WP), allocatable :: bd1z_C2P(:, :, :, :)
  real(WP), allocatable :: cd1z_C2P(:, :, :, :)
  real(WP), allocatable :: dd1z_C2P(:, :, :, :)
!----------------------------------------------------------------------------------------------------------
! z : pre-processed TDMA LHS Matrix for mid-point interpolation
!----------------------------------------------------------------------------------------------------------
  real(WP), allocatable :: am1z_P2C(:, :, :, :)
  real(WP), allocatable :: bm1z_P2C(:, :, :, :)
  real(WP), allocatable :: cm1z_P2C(:, :, :, :)
  real(WP), allocatable :: dm2z_P2C(:, :, :, :)

  real(WP), allocatable :: am1z_C2P(:, :, :, :)
  real(WP), allocatable :: bm1z_C2P(:, :, :, :)
  real(WP), allocatable :: cm1z_C2P(:, :, :, :)
  real(WP), allocatable :: dm2z_C2P(:, :, :, :)
!----------------------------------------------------------------------------------------------------------
! processures
!----------------------------------------------------------------------------------------------------------
  private :: Prepare_compact_coefficients
  private :: Buildup_TDMA_LHS_array
  public  :: Prepare_LHS_coeffs_for_operations

  private :: buildup_ghost_cells_P
  private :: buildup_ghost_cells_C

  private :: Prepare_TDMA_interp_P2C_RHS_array
  private :: Get_x_midp_P2C_1D
  private :: Get_y_midp_P2C_1D
  private :: Get_z_midp_P2C_1D
  public  :: Get_x_midp_P2C_3D
  public  :: Get_y_midp_P2C_3D 
  public  :: Get_z_midp_P2C_3D 

  private :: Prepare_TDMA_interp_C2P_RHS_array
  private :: Get_x_midp_C2P_1D
  private :: Get_y_midp_C2P_1D
  private :: Get_z_midp_C2P_1D
  public  :: Get_x_midp_C2P_3D
  public  :: Get_y_midp_C2P_3D
  public  :: Get_z_midp_C2P_3D

  private :: Prepare_TDMA_1deri_C2C_RHS_array
  private :: Get_x_1der_C2C_1D
  private :: Get_y_1der_C2C_1D
  private :: Get_z_1der_C2C_1D
  public  :: Get_x_1der_C2C_3D
  public  :: Get_y_1der_C2C_3D
  public  :: Get_z_1der_C2C_3D

  private :: Prepare_TDMA_1deri_P2P_RHS_array
  private :: Get_x_1der_P2P_1D
  private :: Get_y_1der_P2P_1D
  private :: Get_z_1der_P2P_1D
  public  :: Get_x_1der_P2P_3D
  public  :: Get_y_1der_P2P_3D
  public  :: Get_z_1der_P2P_3D

  private :: Prepare_TDMA_1deri_C2P_RHS_array
  private :: Get_x_1der_C2P_1D
  private :: Get_y_1der_C2P_1D
  private :: Get_z_1der_C2P_1D
  public  :: Get_x_1der_C2P_3D
  public  :: Get_y_1der_C2P_3D
  public  :: Get_z_1der_C2P_3D

  private :: Prepare_TDMA_1deri_P2C_RHS_array
  private :: Get_x_1der_P2C_1D
  private :: Get_y_1der_P2C_1D
  private :: Get_z_1der_P2C_1D
  public  :: Get_x_1der_P2C_3D
  public  :: Get_y_1der_P2C_3D
  public  :: Get_z_1der_P2C_3D

  public  :: Test_interpolation
  public  :: Test_1st_derivative

  private :: reduce_bc_to_interp
  private :: check_size

contains

  subroutine check_size(var_name, dim, expected, actual, message)
    integer, intent(in) :: dim, expected, actual
    character(*), intent(in) :: var_name, message

    if (expected /= actual) then
      write(*, *) 'nrank, ', var_name, nrank, expected, actual
      call Print_error_msg("Error: " // message)
    end if
  end subroutine check_size

  subroutine reduce_bc_to_interp(ibc, flg, strbc, strcode)
    use parameters_constant_mod
    use mpi_mod
    implicit none
    integer, intent(inout) :: ibc
    logical, intent(inout) :: flg
    character(*), intent(in) :: strbc
    character(*), intent(in) :: strcode
    
    if((.not. flg) .and. nrank==0) &
    call Print_warning_msg('Lack of fbc for '//trim(strbc)//', which is reduced to IBC_INTRPL in subroutine: '//trim(strcode))
    flg = .true.
    ibc = IBC_INTRPL
    return 
  end subroutine 
!==========================================================================================================
!> \brief Assigned the cooefficients for the compact schemes     
!> Scope:  mpi    called-freq    xdomain     module
!>         all    once           specified   privatee
!> reference: 
!> [Gaitonde1998] Gaitonde, D.V. and Visbal, M., 1998. High-order schemes for Navier-Stokes quations: algorithm 
!> and implementation into FDL3DI. Air Vehicles Directorte, Air Force Research Laboratory, Air Force Materiel Command.
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!  mode           name          role                                           !
!----------------------------------------------------------------------------------------------------------
!> \param[in]     iaccu         the accuracy given by user
!==========================================================================================================
  subroutine Prepare_compact_coefficients
    use parameters_constant_mod
    use input_general_mod
    use mpi_mod
    implicit none

    real(WP) :: alpha (NACC),  a(NACC),  b(NACC),  c(NACC)
    real(WP) :: alpha1(NACC), a1(NACC), b1(NACC), c1(NACC), d1(NACC), e1(NACC), f1(NACC)
    real(WP) :: alpha2(NACC), a2(NACC), b2(NACC), c2(NACC), d2(NACC), e2(NACC), f2(NACC)

    integer :: n

    if(nrank == 0) then
       call Print_debug_start_msg &
         ("Assigning coefficient matrix for the compact schemes ...")
       !write(*, *) "The given numerical accuracy =", iaccu
    end if

    if(bc_ghost_cd .and. bc_intp_upw) call Print_error_msg("Please choose a boundary treatment method correctly.")

!----------------------------------------------------------------------------------------------------------
!   initialisation
!----------------------------------------------------------------------------------------------------------
    d1fC2C(:, :, :, :) = MAXP
    d1rC2C(:, :, :, :) = MAXP
    d1fP2P(:, :, :, :) = MAXP
    d1rP2P(:, :, :, :) = MAXP

    d1fC2P(:, :, :, :) = MAXP
    d1rC2P(:, :, :, :) = MAXP
    d1fP2C(:, :, :, :) = MAXP
    d1rP2C(:, :, :, :) = MAXP

    m1fC2P(:, :, :, :) = MAXP
    m1rC2P(:, :, :, :) = MAXP
    m1fP2C(:, :, :, :) = MAXP
    m1rP2C(:, :, :, :) = MAXP
!==========================================================================================================
! Set 1 : C2C, periodic & symmetric & asymmetric
!         1st derivative on collocated grids, C2C/P2P bulk coefficients
! d1fC2C : "d1"=first deriviative, "f"=f'  side, "C2C"= center 2 centre 
! d1rC2C : "d1"=first deriviative, "r"=rhs side, "C2C"= center 2 centre 
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
! C2C unknows: 
! when i=1,    need: LHS: f'_0;      RHS: f_0, f_{-1}
! when i=2,    need:                 RHS: f_0
! when i=nc-1, need:                 RHS: f_{nc+1}
! when i=nc,   need: LHS: f'_{nc+1}; RHS: f_{nc+1}, f_{nc+2}
!==========================================================================================================
! below ref: Table 2.1 in [Gaitonde1998]
    alpha = 0.0_WP
        a = 0.0_WP
        b = 0.0_WP
        c = 0.0_WP
    do n = 1, NACC
      select case (n)
        case (IACCU_CD2)  
          alpha(n) = 0.0_WP
              a(n) = 1.0_WP
              b(n) = 0.0_WP
        case (IACCU_CD4)
          alpha(n) =  0.0_WP
              a(n) =  4.0_WP / 3.0_WP
              b(n) = -1.0_WP / 3.0_WP
        case (IACCU_CP4)
          alpha(n) = 1.0_WP / 4.0_WP
              a(n) = 3.0_WP / 2.0_WP
              b(n) = 0.0_WP
        case (IACCU_CP6) 
          alpha(n) =  1.0_WP / 3.0_WP
              a(n) = 14.0_WP / 9.0_WP
              b(n) =  1.0_WP / 9.0_WP
        case default
          print*, "Invalid accuracy" 
      end select
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2C, IBC_PERIODIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fC2C(1:5, 1, IBC_PERIODIC, n) = alpha(n)
      d1fC2C(1:5, 2, IBC_PERIODIC, n) = ONE
      d1fC2C(1:5, 3, IBC_PERIODIC, n) = alpha(n)

      d1rC2C(1:5, 1, IBC_PERIODIC, n) = a(n) * HALF    ! a/2
      d1rC2C(1:5, 2, IBC_PERIODIC, n) = b(n) * QUARTER ! b/4
    end do 
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2C, IBC_SYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fC2C(1,   1, IBC_SYMMETRIC, n) = ZERO        ! not used
      d1fC2C(1,   2, IBC_SYMMETRIC, n) = ONE - alpha(n)
      d1fC2C(1,   3, IBC_SYMMETRIC, n) = alpha(n)
      d1fC2C(5,   1, IBC_SYMMETRIC, n) = d1fC2C(1,   3, IBC_SYMMETRIC, n)
      d1fC2C(5,   2, IBC_SYMMETRIC, n) = d1fC2C(1,   2, IBC_SYMMETRIC, n)
      d1fC2C(5,   3, IBC_SYMMETRIC, n) = d1fC2C(1,   1, IBC_SYMMETRIC, n)
      d1fC2C(2:4, :, IBC_SYMMETRIC, n) = d1fC2C(2:4, :, IBC_PERIODIC, n)
      d1rC2C(:,   :, IBC_SYMMETRIC, n) = d1rC2C(:,   :, IBC_PERIODIC, n)
    end do 
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2C, IBC_ASYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fC2C(1,   1, IBC_ASYMMETRIC, n) = ZERO        ! not used
      d1fC2C(1,   2, IBC_ASYMMETRIC, n) = ONE + alpha(n)
      d1fC2C(1,   3, IBC_ASYMMETRIC, n) = alpha(n)
      d1fC2C(5,   1, IBC_ASYMMETRIC, n) = d1fC2C(1,   3, IBC_ASYMMETRIC, n)
      d1fC2C(5,   2, IBC_ASYMMETRIC, n) = d1fC2C(1,   2, IBC_ASYMMETRIC, n)
      d1fC2C(5,   3, IBC_ASYMMETRIC, n) = d1fC2C(1,   1, IBC_ASYMMETRIC, n)
      d1fC2C(2:4, :, IBC_ASYMMETRIC, n) = d1fC2C(2:4, :, IBC_PERIODIC  , n)
      d1rC2C(:,   :, IBC_ASYMMETRIC, n) = d1rC2C(:,   :, IBC_PERIODIC  , n)
    end do 
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2C, IBC_INTERIOR, f unknowns only from rhs could be reconstructed from bc, thus explicit
! f' unknow is only first layer 
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
    d1fC2C(:, :, IBC_INTERIOR, :) = d1fC2C(:, :, IBC_PERIODIC, :)
    d1rC2C(:, :, IBC_INTERIOR, :) = d1rC2C(:, :, IBC_PERIODIC, :)
    do n = 1, NACC
      if(n == IACCU_CP4 .or. n == IACCU_CP6) then
          d1fC2C(1, :, IBC_INTERIOR, n) = d1fC2C(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1rC2C(1, :, IBC_INTERIOR, n) = d1rC2C(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1fC2C(5, :, IBC_INTERIOR, n) = d1fC2C(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1rC2C(5, :, IBC_INTERIOR, n) = d1rC2C(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
      end if
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2C, IBC_INTRPL, no bc, no reconstuction. exterpolation only. for the first point
!                    f'_1 + alpha * f'_{2}   = a1 * f_1 + b1 * f_2 + c1 * f-3 + ...
! alpha * f'_{1}   + f'_2 + alpha * f'_{3}   = a2 * f_1 + b2 * f_2 + c2 * f-3 + ...
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
    ! below ref: Table 2.2 in [Gaitonde1998]
    alpha1 = 0.0_WP
        a1 = 0.0_WP
        b1 = 0.0_WP 
        c1 = 0.0_WP
        d1 = 0.0_WP
        e1 = 0.0_WP
        f1 = 0.0_WP 
    do n = 1, NACC
      select case (n)
        case (IACCU_CD2)  
          alpha1(n) = 0.0_WP
              a1(n) = -3.0_WP / 2.0_WP
              b1(n) =  2.0_WP
              c1(n) = -1.0_WP / 2.0_WP
        case (IACCU_CD4)
          alpha1(n) =   0.0_WP
              a1(n) = -25.0_WP / 12.0_WP
              b1(n) =   4.0_WP
              c1(n) =  -3.0_WP
              d1(n) =   4.0_WP / 3.0_WP
              e1(n) =  -1.0_WP / 4.0_WP
        case (IACCU_CP4)
          alpha1(n) =   3.0_WP
              a1(n) = -17.0_WP / 6.0_WP
              b1(n) =   3.0_WP / 2.0_WP
              c1(n) =   3.0_WP / 2.0_WP
              d1(n) =  -1.0_WP / 6.0_WP
        case (IACCU_CP6) 
          alpha1(n) =    5.0_WP
              a1(n) = -197.0_WP / 60.0_WP
              b1(n) =   -5.0_WP / 12.0_WP
              c1(n) =    5.0_WP
              d1(n) =   -5.0_WP / 3.0_WP
              e1(n) =    5.0_WP / 12.0_WP
              f1(n) =   -1.0_WP / 20.0_WP
        case default
            print*, "Invalid accuracy" 
      end select
    end do

    do n = 1, NACC
      d1fC2C(1, 1, IBC_INTRPL, n) = ZERO ! not used
      d1fC2C(1, 2, IBC_INTRPL, n) = ONE
      d1fC2C(1, 3, IBC_INTRPL, n) = alpha1(n)

      d1rC2C(1, 1, IBC_INTRPL, n) = a1(n)
      d1rC2C(1, 2, IBC_INTRPL, n) = b1(n)
      d1rC2C(1, 3, IBC_INTRPL, n) = c1(n)
      d1rC2C(1, 4, IBC_INTRPL, n) = d1(n)
      d1rC2C(1, 5, IBC_INTRPL, n) = e1(n)
      d1rC2C(1, 6, IBC_INTRPL, n) = f1(n)

      d1fC2C(5, 1, IBC_INTRPL, n) =   d1fC2C(1, 3, IBC_INTRPL, n)
      d1fC2C(5, 2, IBC_INTRPL, n) =   d1fC2C(1, 2, IBC_INTRPL, n)
      d1fC2C(5, 3, IBC_INTRPL, n) =   d1fC2C(1, 1, IBC_INTRPL, n)
      d1rC2C(5, :, IBC_INTRPL, n) = - d1rC2C(1, :, IBC_INTRPL, n)
    end do

    do n = 1, NACC
      d1fC2C(3, :, IBC_INTRPL, n) = d1fC2C(3, :, IBC_PERIODIC, n)
      d1rC2C(3, :, IBC_INTRPL, n) = d1rC2C(3, :, IBC_PERIODIC, n)
    end do

    ! below ref: Table 2.6 & 2.3 in [Gaitonde1998]
    alpha2 = 0.0_WP
        a2 = 0.0_WP
        b2 = 0.0_WP 
        c2 = 0.0_WP
        d2 = 0.0_WP
        e2 = 0.0_WP
        f2 = 0.0_WP 

    do n = 1, NACC
      select case (n)
        case (IACCU_CD2)  
          alpha2(n) =  0.0_WP
              a2(n) = -1.0_WP / 2.0_WP
              b2(n) =  0.0_WP
              c2(n) =  1.0_WP / 2.0_WP
        case (IACCU_CD4)
          alpha2(n) =  0.0_WP
              a2(n) = -1.0_WP /  4.0_WP
              b2(n) = -5.0_WP /  6.0_WP
              c2(n) =  3.0_WP /  2.0_WP
              d2(n) = -1.0_WP /  2.0_WP
              e2(n) =  1.0_WP / 12.0_WP
        case (IACCU_CP4)
          alpha2(n) =  1.0_WP / 4.0_WP
              a2(n) = -3.0_WP / 4.0_WP
              b2(n) =  0.0_WP
              c2(n) =  3.0_WP / 4.0_WP
        case (IACCU_CP6) 
          alpha2(n) =   2.0_WP / 11.0_WP
              a2(n) = -20.0_WP / 33.0_WP
              b2(n) = -35.0_WP / 132.0_WP
              c2(n) =  34.0_WP / 33.0_WP
              d2(n) =  -7.0_WP / 33.0_WP
              e2(n) =   2.0_WP / 33.0_WP
              f2(n) =  -1.0_WP / 132.0_WP
        case default
            print*, "Invalid accuracy" 
      end select
    end do

    do n = 1, NACC
      d1fC2C(2, 1, IBC_INTRPL, n) = alpha2(n)
      d1fC2C(2, 2, IBC_INTRPL, n) = ONE
      d1fC2C(2, 3, IBC_INTRPL, n) = alpha2(n)

      d1rC2C(2, 1, IBC_INTRPL, n) = a2(n)
      d1rC2C(2, 2, IBC_INTRPL, n) = b2(n)
      d1rC2C(2, 3, IBC_INTRPL, n) = c2(n)
      d1rC2C(2, 4, IBC_INTRPL, n) = d2(n)
      d1rC2C(2, 5, IBC_INTRPL, n) = e2(n)
      d1rC2C(2, 6, IBC_INTRPL, n) = f2(n)

      d1fC2C(4, 1, IBC_INTRPL, n) =   d1fC2C(2, 3, IBC_INTRPL, n)
      d1fC2C(4, 2, IBC_INTRPL, n) =   d1fC2C(2, 2, IBC_INTRPL, n)
      d1fC2C(4, 3, IBC_INTRPL, n) =   d1fC2C(2, 1, IBC_INTRPL, n)
      d1rC2C(4, :, IBC_INTRPL, n) = - d1rC2C(2, :, IBC_INTRPL, n)
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2C: IBC_DIRICHLET, IBC_NEUMANN
!----------------------------------------------------------------------------------------------------------
    if(bc_ghost_cd) then 
      d1fC2C(:, :, IBC_DIRICHLET, :) = d1fC2C(:, :, IBC_INTERIOR, :)
      d1rC2C(:, :, IBC_DIRICHLET, :) = d1rC2C(:, :, IBC_INTERIOR, :)
      d1fC2C(:, :, IBC_NEUMANN,   :) = d1fC2C(:, :, IBC_INTERIOR, :)
      d1rC2C(:, :, IBC_NEUMANN,   :) = d1rC2C(:, :, IBC_INTERIOR, :)
    end if
    if(bc_intp_upw) then 
      d1fC2C(:, :, IBC_DIRICHLET, :) = d1fC2C(:, :, IBC_INTRPL, :)
      d1rC2C(:, :, IBC_DIRICHLET, :) = d1rC2C(:, :, IBC_INTRPL, :)
      d1fC2C(:, :, IBC_NEUMANN,   :) = d1fC2C(:, :, IBC_INTRPL, :)
      d1rC2C(:, :, IBC_NEUMANN,   :) = d1rC2C(:, :, IBC_INTRPL, :)
    end if
!==========================================================================================================
! 1st-derivative, P2P :
! d1fP2P : "d1"=first deriviative, "f"=f'  side, "P2P"= point(node) 2 point 
! d1rP2P : "d1"=first deriviative, "r"=rhs side, "P2P"= point(node) 2 point 
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(2h) * ( f_{i'+1} - f_{i'-1} ) + &
!                                                 b/(4h) * ( f_{i'+2} - f_{i'-2} )
! P2P unknows: 
! when i'=1,     need: LHS: f'_0';      RHS: f_0', f_{-1'}
! when i'=2,     need:                  RHS: f_0'
! when i'=np'-1, need:                  RHS: f_{np'+1}
! when i'=np',   need: LHS: f'_{np'+1}; RHS: f_{np'+1}, f_{np'+2}
!==========================================================================================================
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2P, IBC_PERIODIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    d1fP2P(:, :, IBC_PERIODIC,  :) = d1fC2C(:, :, IBC_PERIODIC,  :)
    d1rP2P(:, :, IBC_PERIODIC,  :) = d1rC2C(:, :, IBC_PERIODIC,  :)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2P : IBC_SYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fP2P(1,   1, IBC_SYMMETRIC, n) = ZERO ! not used
      d1fP2P(1,   2, IBC_SYMMETRIC, n) = ONE
      d1fP2P(1,   3, IBC_SYMMETRIC, n) = ZERO
      d1fP2P(5,   1, IBC_SYMMETRIC, n) = d1fP2P(1,   3, IBC_SYMMETRIC, n)
      d1fP2P(5,   2, IBC_SYMMETRIC, n) = d1fP2P(1,   2, IBC_SYMMETRIC, n)
      d1fP2P(5,   3, IBC_SYMMETRIC, n) = d1fP2P(1,   1, IBC_SYMMETRIC, n)
      d1fP2P(2:4, :, IBC_SYMMETRIC, n) = d1fP2P(2:4, :, IBC_PERIODIC,  n)
      d1rP2P(:,   :, IBC_SYMMETRIC, n) = d1rP2P(:,   :, IBC_PERIODIC,  n)
    end do 
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2P : IBC_ASYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fP2P(1,   1, IBC_ASYMMETRIC, n) = ZERO ! not used
      d1fP2P(1,   2, IBC_ASYMMETRIC, n) = ONE
      d1fP2P(1,   3, IBC_ASYMMETRIC, n) = TWO * alpha(n)
      d1fP2P(5,   1, IBC_ASYMMETRIC, n) = d1fP2P(1,   3, IBC_ASYMMETRIC, n)
      d1fP2P(5,   2, IBC_ASYMMETRIC, n) = d1fP2P(1,   2, IBC_ASYMMETRIC, n)
      d1fP2P(5,   3, IBC_ASYMMETRIC, n) = d1fP2P(1,   1, IBC_ASYMMETRIC, n)
      d1fP2P(2:4, :, IBC_ASYMMETRIC, n) = d1fP2P(2:4, :, IBC_PERIODIC  , n)
      d1rP2P(:,   :, IBC_ASYMMETRIC, n) = d1rP2P(:,   :, IBC_PERIODIC  , n)
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2P : IBC_INTERIOR, unknowns only from only rhs could be reconstructed from bc, thus explicit
!----------------------------------------------------------------------------------------------------------
    d1fP2P(:, :, IBC_INTERIOR, :) = d1fP2P(:, :, IBC_PERIODIC, :)
    d1rP2P(:, :, IBC_INTERIOR, :) = d1rP2P(:, :, IBC_PERIODIC, :)
    do n = 1, NACC
      if(n == IACCU_CP4 .or. n == IACCU_CP6) then
          d1fP2P(1, :, IBC_INTERIOR, n) = d1fP2P(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1rP2P(1, :, IBC_INTERIOR, n) = d1rP2P(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1fP2P(5, :, IBC_INTERIOR, n) = d1fP2P(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1rP2P(5, :, IBC_INTERIOR, n) = d1rP2P(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
      end if
    end do

!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2P : exterpolation
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(2h) * ( f_{i'+1} - f_{i'-1} ) + &
!                                                 b/(4h) * ( f_{i'+2} - f_{i'-2} )
! 1st-derivative, C2C, IBC_INTRPL, no bc, no reconstuction. exterpolation only. 
!                     f'_1' + alpha * f'_{2'}   = a1 * f_1' + b1 * f_2' + c1 * f_3'
! alpha * f'_{1'}   + f'_2' + alpha * f'_{3'}   = a/(2h) * ( f_{i'+1} - f_{i'-1} ) 
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(2h) * ( f_{i'+1} - f_{i'-1} ) + &
!                                                 b/(4h) * ( f_{i'+2} - f_{i'-2} )
!----------------------------------------------------------------------------------------------------------
    d1fP2P(:, :, IBC_INTRPL,    :) = d1fC2C(:, :, IBC_INTRPL, :)
    d1rP2P(:, :, IBC_INTRPL,    :) = d1rC2C(:, :, IBC_INTRPL, :)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2P : NEUMANN, unknowns only from only rhs could be reconstructed from bc, thus explicit
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fP2P(1, 1, IBC_NEUMANN, n) = ZERO ! not used
      d1fP2P(1, 2, IBC_NEUMANN, n) = ONE
      d1fP2P(1, 3, IBC_NEUMANN, n) = ZERO
      d1rP2P(1, :, IBC_NEUMANN, n) = ZERO

      d1fP2P(5, 1, IBC_NEUMANN, n) = ZERO
      d1fP2P(5, 2, IBC_NEUMANN, n) = ONE
      d1fP2P(5, 3, IBC_NEUMANN, n) = ZERO
      d1rP2P(5, :, IBC_NEUMANN, n) = ZERO
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2P : IBC_DIRICHLET, unknowns only from only rhs could be reconstructed from bc, thus explicit
!----------------------------------------------------------------------------------------------------------
    if(bc_intp_upw) then
      d1fP2P(:,   :, IBC_DIRICHLET, :) = d1fP2P(:,   :, IBC_INTRPL, :)
      d1rP2P(:,   :, IBC_DIRICHLET, :) = d1rP2P(:,   :, IBC_INTRPL, :)
      d1fP2P(2:4, :, IBC_NEUMANN,   :) = d1fP2P(2:4, :, IBC_INTRPL, :)
      d1rP2P(2:4, :, IBC_NEUMANN,   :) = d1rP2P(2:4, :, IBC_INTRPL, :)
    end if
    if(bc_ghost_cd) then
      d1fP2P(:,   :, IBC_DIRICHLET, :) = d1fP2P(:,   :, IBC_INTERIOR, :)
      d1rP2P(:,   :, IBC_DIRICHLET, :) = d1rP2P(:,   :, IBC_INTERIOR, :)
      d1fP2P(2:4, :, IBC_NEUMANN,   :) = d1fP2P(2:4, :, IBC_INTERIOR, :)
      d1rP2P(2:4, :, IBC_NEUMANN,   :) = d1rP2P(2:4, :, IBC_INTERIOR, :)
    end if
!==========================================================================================================
! 1st derivative on staggered grids C2P
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(h ) * ( f_{i}   - f_{i-1} ) + &
!                                                 b/(3h) * ( f_{i+1} - f_{i-2} )
! when i' = 1',    need: f'_0', f_0, f_{-1}
! when i' = 2',    need: f_0
! when i' = np-1', need: f_{np}
! when i' = np',   need: f'_{np+1'}, f_{np}, f_{np+1}
!==========================================================================================================
! below ref: Table 2.10 in [Gaitonde1998]
    alpha = 0.0_WP
        a = 0.0_WP
        b = 0.0_WP
        c = 0.0_WP
    do n = 1, NACC
      select case (n)
        case (IACCU_CD2)  
          alpha(n) = 0.0_WP
              a(n) = 1.0_WP
              b(n) = 0.0_WP
        case (IACCU_CD4)
          alpha(n) =  0.0_WP
              a(n) = ( 9.0_WP -  6.0_WP * alpha(n)) / 8.0_WP
              b(n) = (-1.0_WP + 22.0_WP * alpha(n)) / 8.0_WP
        case (IACCU_CP4)
          alpha(n) =  1.0_WP / 22.0_WP
              a(n) = ( 9.0_WP -  6.0_WP * alpha(n)) / 8.0_WP
              b(n) = (-1.0_WP + 22.0_WP * alpha(n)) / 8.0_WP
        case (IACCU_CP6) 
          alpha(n) =  9.0_WP / 62.0_WP
              a(n) = 63.0_WP / 62.0_WP
              b(n) = 17.0_WP / 62.0_WP
        case default
          print*, "Invalid accuracy" 
      end select
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2P, IBC_PERIODIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fC2P(1:5, 1, IBC_PERIODIC, n) = alpha(n)
      d1fC2P(1:5, 2, IBC_PERIODIC, n) = ONE
      d1fC2P(1:5, 3, IBC_PERIODIC, n) = alpha(n)
      d1rC2P(1:5, 1, IBC_PERIODIC, n) = a(n)             ! a
      d1rC2P(1:5, 2, IBC_PERIODIC, n) = b(n) * ONE_THIRD ! b/3
      d1rC2P(1:5, 3, IBC_PERIODIC, n) = ZERO             ! not used.
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2P, IBC_SYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fC2P(1,   1, IBC_SYMMETRIC, n) = ZERO ! not used
      d1fC2P(1,   2, IBC_SYMMETRIC, n) = ONE
      d1fC2P(1,   3, IBC_SYMMETRIC, n) = ZERO
      d1fC2P(5,   1, IBC_SYMMETRIC, n) = d1fC2P(1,   3, IBC_SYMMETRIC, n)
      d1fC2P(5,   2, IBC_SYMMETRIC, n) = d1fC2P(1,   2, IBC_SYMMETRIC, n)
      d1fC2P(5,   3, IBC_SYMMETRIC, n) = d1fC2P(1,   1, IBC_SYMMETRIC, n)
      d1fC2P(2:4, :, IBC_SYMMETRIC, n) = d1fC2P(2:4, :, IBC_PERIODIC,  n)
      d1rC2P(:,   :, IBC_SYMMETRIC, n) = d1rC2P(:,   :, IBC_PERIODIC,  n)
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2P : IBC_ASYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fC2P(1,   1, IBC_ASYMMETRIC, n) = ZERO ! not used
      d1fC2P(1,   2, IBC_ASYMMETRIC, n) = ONE
      d1fC2P(1,   3, IBC_ASYMMETRIC, n) = TWO * alpha(n)
      d1fC2P(5,   1, IBC_ASYMMETRIC, n) = d1fC2P(1,   3, IBC_ASYMMETRIC, n)
      d1fC2P(5,   2, IBC_ASYMMETRIC, n) = d1fC2P(1,   2, IBC_ASYMMETRIC, n)
      d1fC2P(5,   3, IBC_ASYMMETRIC, n) = d1fC2P(1,   1, IBC_ASYMMETRIC, n)
      d1fC2P(2:4, :, IBC_ASYMMETRIC, n) = d1fC2P(2:4, :, IBC_PERIODIC,   n)
      d1rC2P(:,   :, IBC_ASYMMETRIC, n) = d1rC2P(:,   :, IBC_PERIODIC,   n)
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2P, IBC_INTERIOR, unknowns only from only rhs could be reconstructed from bc, thus explicit
!----------------------------------------------------------------------------------------------------------
    d1fC2P(:, :, IBC_INTERIOR, :) = d1fC2P(:, :, IBC_PERIODIC, :)
    d1rC2P(:, :, IBC_INTERIOR, :) = d1rC2P(:, :, IBC_PERIODIC, :)

    do n = 1, NACC
      if(n == IACCU_CP4 .or. n == IACCU_CP6) then
          d1fC2P(1, :, IBC_INTERIOR, n) = d1fC2P(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1rC2P(1, :, IBC_INTERIOR, n) = d1rC2P(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1fC2P(5, :, IBC_INTERIOR, n) = d1fC2P(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
          d1rC2P(5, :, IBC_INTERIOR, n) = d1rC2P(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
      end if
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : C2P, IBC_INTRPL, no bc, no reconstuction. exterpolation only. 
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(h ) * ( f_{i}   - f_{i-1} ) + &
!                                                 b/(3h) * ( f_{i+1} - f_{i-2} )
! when i' = 1',    need: f'_0', f_0, f_{-1}
! when i' = 2',    need: f_0
! when i' = np-1', need: f_{np}
! when i' = np',   need: f'_{np+1'}, f_{np}, f_{np+1}
! [ 1     alpha1                            ][f'_1']=[a1 * f_{1}/h  + b1 * f_{2}/h + c1 * f_{3}/h  ]
! [alpha2 1      alpha2                     ][f'_2'] [a2 * (f_{2} - f_{1})/h  ]
! [       alpha  1      alpha               ][f'_i'] [a *  (f_{i} - f_{i-1})/h + b/3 * (f_{i+1} - f_{i-2})/h]
! [                     alpha2 1      alpha2][f'_4'] [a2 * (f_{n-1} - f_{n-2})/h]
! [                            alpha1 1     ][f'_5'] [-a1 * f_{n-1}/h  - b1 * f_{n-2}/h - c1 * f_{n-3}/h]
! tested: low accuracy at the line 2 and 4.
!----------------------------------------------------------------------------------------------------------
!    ref: [Gaitonde1998] Table 2.12
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO 
        c1 = ZERO
        d1 = ZERO
        e1 = ZERO
        f1 = ZERO 

    do n = 1, NACC
      select case (n)
        case (IACCU_CD2)  
          alpha1(n) = ZERO
              a1(n) = -2.0_WP - alpha1(n)
              b1(n) =  3.0_WP + alpha1(n)
              c1(n) = -1.0_WP
        case (IACCU_CD4, IACCU_CP4)
          alpha1(n) = ZERO ! 22.0_WP invalids TDMA
              a1(n) = (-93.0_WP - 22.0_WP * alpha1(n)) / 24.0_WP
              b1(n) = (229.0_WP + 17.0_WP * alpha1(n)) / 24.0_WP
              c1(n) = (-75.0_WP +  3.0_WP * alpha1(n)) / 8.0_WP
              d1(n) = (111.0_WP -  5.0_WP * alpha1(n)) / 24.0_WP
              e1(n) = (-22.0_WP +  1.0_WP * alpha1(n)) / 24.0_WP
        case (IACCU_CP6) 
          alpha1(n) = 1689.0_WP / 71.0_WP  ! this is 5th order Compact
              a1(n) = ( -3043.0_WP - 563.0_WP * alpha1(n)) /  640.0_WP
              b1(n) = (  5353.0_WP + 201.0_WP * alpha1(n)) /  384.0_WP
              c1(n) = ( -3489.0_WP + 143.0_WP * alpha1(n)) /  192.0_WP
              d1(n) = (   859.0_WP -  37.0_WP * alpha1(n)) /  64.0_WP
              e1(n) = ( -2041.0_WP +  87.0_WP * alpha1(n)) /  384.0_WP
              f1(n) = (  1689.0_WP -  71.0_WP * alpha1(n)) / 1920.0_WP

              ! alpha1(n) =     1627.0_WP /     62.0_WP
              !     a1(n) = -1104667.0_WP /  39680.0_WP     
              !     b1(n) =   658913.0_WP /  23808.0_WP  
              !     c1(n) =    16343.0_WP /  11904.0_WP    
              !     d1(n) =    -6941.0_WP /   3968.0_WP 
              !     e1(n) =    15007.0_WP /  23808.0_WP 
              !     f1(n) =   -10799.0_WP / 119040.0_WP 
        case default
            print*, "Invalid accuracy" 
      end select
    end do

    do n = 1, NACC
      d1fC2P(1, 1, IBC_INTRPL, n) = ZERO ! not used
      d1fC2P(1, 2, IBC_INTRPL, n) = ONE
      d1fC2P(1, 3, IBC_INTRPL, n) = alpha1(n)

      d1rC2P(1, 1, IBC_INTRPL, n) = a1(n)
      d1rC2P(1, 2, IBC_INTRPL, n) = b1(n)
      d1rC2P(1, 3, IBC_INTRPL, n) = c1(n)
      d1rC2P(1, 4, IBC_INTRPL, n) = d1(n)
      d1rC2P(1, 5, IBC_INTRPL, n) = e1(n)
      d1rC2P(1, 6, IBC_INTRPL, n) = f1(n)

      d1fC2P(5, 1, IBC_INTRPL, n) =   d1fC2P(1, 3, IBC_INTRPL, n)
      d1fC2P(5, 2, IBC_INTRPL, n) =   d1fC2P(1, 2, IBC_INTRPL, n)
      d1fC2P(5, 3, IBC_INTRPL, n) =   d1fC2P(1, 1, IBC_INTRPL, n)
      d1rC2P(5, :, IBC_INTRPL, n) = - d1rC2P(1, :, IBC_INTRPL, n)
    end do

    do n = 1, NACC
      d1fC2P(3, :, IBC_INTRPL, n) = d1fC2P(3, :, IBC_PERIODIC, n)
      d1rC2P(3, :, IBC_INTRPL, n) = d1rC2P(3, :, IBC_PERIODIC, n)
    end do
!    ref: [Gaitonde1998] Table 2.13
    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO 
        c2 = ZERO
        d2 = ZERO
        e2 = ZERO
        f2 = ZERO 
    do n = 1, NACC
      select case (n)
        case (IACCU_CD2)  
          alpha2(n) =  0.0_WP
              a2(n) = -1.0_WP
              b2(n) =  1.0_WP
        case (IACCU_CD4)
          alpha2(n) = 0.0_WP
              a2(n) = (-11.0_WP -  46.0_WP * alpha2(n)) / 12.0_WP
              b2(n) = ( 17.0_WP + 202.0_WP * alpha2(n)) / 24.0_WP
              c2(n) = (  3.0_WP -  66.0_WP * alpha2(n)) /  8.0_WP
              d2(n) = ( -5.0_WP + 110.0_WP * alpha2(n)) / 24.0_WP
              e2(n) = (  1.0_WP -  22.0_WP * alpha2(n)) / 24.0_WP
        case (IACCU_CP4)
          alpha2(n) = 1.0_WP / 22.0_WP
              a2(n) = (-11.0_WP -  46.0_WP * alpha2(n)) / 12.0_WP
              b2(n) = ( 17.0_WP + 202.0_WP * alpha2(n)) / 24.0_WP
              c2(n) = (  3.0_WP -  66.0_WP * alpha2(n)) /  8.0_WP
              d2(n) = ( -5.0_WP + 110.0_WP * alpha2(n)) / 24.0_WP
              e2(n) = (  1.0_WP -  22.0_WP * alpha2(n)) / 24.0_WP
        case (IACCU_CP6) 
          alpha2(n) =    31.0_WP / 818.0_WP
              a2(n) = -5195.0_WP / 4908.0_WP     
              b2(n) =  4957.0_WP / 4908.0_WP  
              c2(n) =   119.0_WP / 1227.0_WP    
              d2(n) =   -85.0_WP / 1227.0_WP 
              e2(n) =   119.0_WP / 4908.0_WP 
              f2(n) =   -17.0_WP / 4908.0_WP 
        case default
            print*, "Invalid accuracy" 
      end select
    end do
    
    do n = 1, NACC
      d1fC2P(2, 1, IBC_INTRPL, n) = alpha2(n)
      d1fC2P(2, 2, IBC_INTRPL, n) = ONE
      d1fC2P(2, 3, IBC_INTRPL, n) = alpha2(n)

      d1rC2P(2, 1, IBC_INTRPL, n) = a2(n)
      d1rC2P(2, 2, IBC_INTRPL, n) = b2(n)
      d1rC2P(2, 3, IBC_INTRPL, n) = c2(n)
      d1rC2P(2, 4, IBC_INTRPL, n) = d2(n)
      d1rC2P(2, 5, IBC_INTRPL, n) = e2(n)
      d1rC2P(2, 6, IBC_INTRPL, n) = f2(n)

      d1fC2P(4, 1, IBC_INTRPL, n) =   d1fC2P(2, 3, IBC_INTRPL, n)
      d1fC2P(4, 2, IBC_INTRPL, n) =   d1fC2P(2, 2, IBC_INTRPL, n)
      d1fC2P(4, 3, IBC_INTRPL, n) =   d1fC2P(2, 1, IBC_INTRPL, n)
      d1rC2P(4, :, IBC_INTRPL, n) = - d1rC2P(2, :, IBC_INTRPL, n)
    end do

!----------------------------------------------------------------------------------------------------------
! 1st-derivative, C2P : IBC_NEUMANN, unknowns only from only rhs could be reconstructed from bc, thus explicit
! 1st-derivative, C2P : IBC_DIRICHLET, unknowns only from only rhs could be reconstructed from bc, thus explicit
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fC2P(1, 1,   IBC_NEUMANN, n) = ZERO ! not used
      d1fC2P(1, 2,   IBC_NEUMANN, n) = ONE
      d1fC2P(1, 3,   IBC_NEUMANN, n) = ZERO
      d1rC2P(1, :,   IBC_NEUMANN, n) = ZERO ! not used
      d1fC2P(5, 1,   IBC_NEUMANN, n) = ZERO
      d1fC2P(5, 2,   IBC_NEUMANN, n) = ONE
      d1fC2P(5, 3,   IBC_NEUMANN, n) = ZERO
      d1rC2P(5, :,   IBC_NEUMANN, n) = ZERO
    end do

    if(bc_intp_upw) then 
      d1fC2P(:,   :, IBC_DIRICHLET, :) = d1fC2P(:,   :, IBC_INTRPL, :)
      d1rC2P(:,   :, IBC_DIRICHLET, :) = d1rC2P(:,   :, IBC_INTRPL, :)
      d1fC2P(2:4, :, IBC_NEUMANN,   :) = d1fC2P(2:4, :, IBC_INTRPL, :)
      d1rC2P(2:4, :, IBC_NEUMANN,   :) = d1rC2P(2:4, :, IBC_INTRPL, :)
    end if
    if(bc_ghost_cd) then 
      d1fC2P(:,   :, IBC_DIRICHLET, :) = d1fC2P(:,   :, IBC_INTERIOR, :)
      d1rC2P(:,   :, IBC_DIRICHLET, :) = d1rC2P(:,   :, IBC_INTERIOR, :)
      d1fC2P(2:4, :, IBC_NEUMANN,   :) = d1fC2P(2:4, :, IBC_INTERIOR, :)
      d1rC2P(2:4, :, IBC_NEUMANN,   :) = d1rC2P(2:4, :, IBC_INTERIOR, :)
    end if
!==========================================================================================================
! 1st derivative on staggered grids P2C
! alpha * f'_{i-1} +  f'_i +  alpha * f'_{i+1}  = a/(h ) * ( f_{i'+1} - f_{i'} ) + &
!                                                 b/(3h) * ( f_{i'+2} - f_{i'-1} )
! when i = 1,    need: f'_0, f_0'
! when i = 2,    need: nothing
! when i = nc-1, need: nothing
! when i = nc,   need: f'_{nc+1'}, f_{np'}, f_{np'+1}
!==========================================================================================================
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2C, IBC_PERIODIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    d1fP2C(:, :, IBC_PERIODIC, :) = d1fC2P(:, :, IBC_PERIODIC, :)
    d1rP2C(:, :, IBC_PERIODIC, :) = d1rC2P(:, :, IBC_PERIODIC, :)
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : P2C : IBC_SYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
! alpha * f'_{i-1} +  f'_i +  alpha * f'_{i+1}  = a/(h ) * ( f_{i'+1} - f_{i'} ) + &
!                                                 b/(3h) * ( f_{i'+2} - f_{i'-1} )
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fP2C(1,   1, IBC_SYMMETRIC, n) = ZERO ! not used
      d1fP2C(1,   2, IBC_SYMMETRIC, n) = ONE - alpha(n)
      d1fP2C(1,   3, IBC_SYMMETRIC, n) = alpha(n)
      d1fP2C(5,   1, IBC_SYMMETRIC, n) = d1fP2C(1,   3, IBC_SYMMETRIC, n)
      d1fP2C(5,   2, IBC_SYMMETRIC, n) = d1fP2C(1,   2, IBC_SYMMETRIC, n)
      d1fP2C(5,   3, IBC_SYMMETRIC, n) = d1fP2C(1,   1, IBC_SYMMETRIC, n)
      d1fP2C(2:4, :, IBC_SYMMETRIC, n) = d1fP2C(2:4, :, IBC_PERIODIC , n)
      d1rP2C(:,   :, IBC_SYMMETRIC, n) = d1rP2C(:,   :, IBC_PERIODIC , n)
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : IBC_ASYMMETRIC, unknowns from both rhs and lhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      d1fP2C(1,   1, IBC_ASYMMETRIC, n) = ZERO ! not used
      d1fP2C(1,   2, IBC_ASYMMETRIC, n) = ONE + alpha(n)
      d1fP2C(1,   3, IBC_ASYMMETRIC, n) = alpha(n)
      d1fP2C(5,   1, IBC_ASYMMETRIC, n) = d1fP2C(1,   3, IBC_ASYMMETRIC, n)
      d1fP2C(5,   2, IBC_ASYMMETRIC, n) = d1fP2C(1,   2, IBC_ASYMMETRIC, n)
      d1fP2C(5,   3, IBC_ASYMMETRIC, n) = d1fP2C(1,   1, IBC_ASYMMETRIC, n)
      d1fP2C(2:4, :, IBC_ASYMMETRIC, n) = d1fP2C(2:4, :, IBC_PERIODIC  , n)
      d1rP2C(:,   :, IBC_ASYMMETRIC, n) = d1rP2C(:,   :, IBC_PERIODIC  , n)
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2C, IBC_INTERIOR, unknowns only from only rhs could be reconstructed from bc, thus explicit
!----------------------------------------------------------------------------------------------------------
    d1fP2C(:, :, IBC_INTERIOR, :) = d1fP2C(:, :, IBC_PERIODIC, :)
    d1rP2C(:, :, IBC_INTERIOR, :) = d1rP2C(:, :, IBC_PERIODIC, :)

    do n = 1, NACC
      if(n == IACCU_CP4 .or. n == IACCU_CP6) then
        d1fP2C(1, :, IBC_INTERIOR, n) = d1fP2C(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        d1rP2C(1, :, IBC_INTERIOR, n) = d1rP2C(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        d1fP2C(5, :, IBC_INTERIOR, n) = d1fP2C(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        d1rP2C(5, :, IBC_INTERIOR, n) = d1rP2C(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
      end if
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative : ! P2C : exterpolation, no bc, no reconstuction. exterpolation only. 
! [ 1     alpha1                            ][f'_1]=[a1 * f_{1'}/h  + b1 * f_{2'}/h + c1 * f_{3'}/h  ]
! [alpha2 1      alpha2                     ][f'_2] [a2 * (f_{3'} - f_{2'})/h  ]
! [       alpha  1      alpha               ][f'_i] [a *  (f_{i'+1} - f_{i'})/h + b/3 * (f_{i'+2} - f_{i'-1})/h]
! [                     alpha2 1      alpha2][f'_4] [a2 * (f_{n'} - f_{n'-1})/h]
! [                            alpha1 1     ][f'_5] [-a1 * f_{n'+1}/h  - b1 * f_{n'}/h - c1 * f_{n'-1}/h]
!----------------------------------------------------------------------------------------------------------
!    ref: [Gaitonde1998] Table 2.11
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO 
        c1 = ZERO
        d1 = ZERO
        e1 = ZERO
        f1 = ZERO 
    do n = 1, NACC
      select case (n)
        case (IACCU_CD2)  
          alpha1(n) = ZERO
              a1(n) = -ONE
              b1(n) = ONE
        case (IACCU_CD4, IACCU_CP4)
          alpha1(n) = ZERO
              a1(n) = (-22.0_WP +           alpha1(n)) / 24.0_WP
              b1(n) = ( 17.0_WP - 27.0_WP * alpha1(n)) / 24.0_WP
              c1(n) = (  3.0_WP +  9.0_WP * alpha1(n)) / 8.0_WP
              d1(n) = ( -5.0_WP -           alpha1(n)) / 24.0_WP
              e1(n) = 1.0_WP / 24.0_WP
        case (IACCU_CP6) 
          
          alpha1(n) = 71.0_WP / 9.0_WP  ! this is 5th order Compact
              a1(n) = ( -1689.0_WP +  71.0_WP * alpha1(n)) / 1920.0_WP
              b1(n) = (    67.0_WP - 141.0_WP * alpha1(n)) /  128.0_WP
              c1(n) = (   143.0_WP + 207.0_WP * alpha1(n)) /  192.0_WP
              d1(n) = (  -111.0_WP +   1.0_WP * alpha1(n)) /  192.0_WP
              e1(n) = (    29.0_WP -   3.0_WP * alpha1(n)) /  128.0_WP
              f1(n) = (   -71.0_WP +   9.0_WP * alpha1(n)) / 1920.0_WP

          ! alpha1(n) =     62.0_WP /     9.0_WP
          !     a1(n) = -10799.0_WP / 17280.0_WP
          !     b1(n) =  -2713.0_WP /   384.0_WP
          !     c1(n) =    523.0_WP /    64.0_WP
          !     d1(n) =   -937.0_WP /  1728.0_WP
          !     e1(n) =     25.0_WP /   384.0_WP
          !     f1(n) =     -3.0_WP /   640.0_WP
        case default
           print*, "Invalid accuracy" 
      end select
    end do

    do n = 1, NACC
      d1fP2C(1, 1, IBC_INTRPL, n) = ZERO ! not used
      d1fP2C(1, 2, IBC_INTRPL, n) = ONE
      d1fP2C(1, 3, IBC_INTRPL, n) = alpha1(n)

      d1rP2C(1, 1, IBC_INTRPL, n) = a1(n)
      d1rP2C(1, 2, IBC_INTRPL, n) = b1(n)
      d1rP2C(1, 3, IBC_INTRPL, n) = c1(n)
      d1rP2C(1, 4, IBC_INTRPL, n) = d1(n)
      d1rP2C(1, 5, IBC_INTRPL, n) = e1(n)
      d1rP2C(1, 6, IBC_INTRPL, n) = f1(n)

      d1fP2C(5, 1, IBC_INTRPL, n) =   d1fP2C(1, 3, IBC_INTRPL, n)
      d1fP2C(5, 2, IBC_INTRPL, n) =   d1fP2C(1, 2, IBC_INTRPL, n)
      d1fP2C(5, 3, IBC_INTRPL, n) =   d1fP2C(1, 1, IBC_INTRPL, n)
      d1rP2C(5, :, IBC_INTRPL, n) = - d1rP2C(1, :, IBC_INTRPL, n)

      d1fP2C(2:4, :, IBC_INTRPL, n) = d1fP2C(2:4, :, IBC_PERIODIC, n)
      d1rP2C(2:4, :, IBC_INTRPL, n) = d1rP2C(2:4, :, IBC_PERIODIC, n)
    end do
!----------------------------------------------------------------------------------------------------------
! 1st-derivative, P2C, IBC_DIRICHLET, IBC_NEUMANN
!----------------------------------------------------------------------------------------------------------
    if(bc_intp_upw) then
      d1fP2C(:, :, IBC_DIRICHLET, :) = d1fP2C(:, :, IBC_INTRPL, :)
      d1rP2C(:, :, IBC_DIRICHLET, :) = d1rP2C(:, :, IBC_INTRPL, :)
      d1fP2C(:, :, IBC_NEUMANN,   :) = d1fP2C(:, :, IBC_INTRPL, :)
      d1rP2C(:, :, IBC_NEUMANN,   :) = d1rP2C(:, :, IBC_INTRPL, :)
    end if
    if(bc_ghost_cd) then
      d1fP2C(:, :, IBC_DIRICHLET, :) = d1fP2C(:, :, IBC_INTERIOR, :)
      d1rP2C(:, :, IBC_DIRICHLET, :) = d1rP2C(:, :, IBC_INTERIOR, :)
      d1fP2C(:, :, IBC_NEUMANN,   :) = d1fP2C(:, :, IBC_INTERIOR, :)
      d1rP2C(:, :, IBC_NEUMANN,   :) = d1rP2C(:, :, IBC_INTERIOR, :)
    end if
!==========================================================================================================
!interpolation. C2P 
! alpha * f_{i'-1} + f_i' + alpha * f_{i'+1} = a/2 * ( f_{i}   + f_{i-1} ) + &
!                                              b/2 * ( f_{i+1} + f_{i-2} )
! when i' = 1,    need: f_{0'}, f_{0}, f_{-1}
! when i' = 2,    need:         f_{0}
! when i' = np-1, need:         f_{np'}
! when i' = np,   need: f_{np'+1}, f_{np'}, f_{np'+1}
!==========================================================================================================
!    ref: [Gaitonde1998] Table 2.7

      alpha = ZERO
          a = ZERO
          b = ZERO
          c = ZERO 

      do n = 1, NACC
        select case (n)
          case (IACCU_CD2) 
            alpha(n) = 0.0_WP
                a(n) = 1.0_WP
                b(n) = 0.0_WP 
          case (IACCU_CD4)
            alpha(n) = 0.0_WP
                a(n) =  9.0_WP / 8.0_WP + 5.0_WP / 4.0_WP * alpha(n)
                b(n) = -1.0_WP / 8.0_WP + 3.0_WP / 4.0_WP * alpha(n)
          case (IACCU_CP4)
            alpha(n) = 1.0_WP / 6.0_WP
                a(n) =  9.0_WP / 8.0_WP + 5.0_WP / 4.0_WP * alpha(n)
                b(n) = -1.0_WP / 8.0_WP + 3.0_WP / 4.0_WP * alpha(n)
          case (IACCU_CP6)
            alpha(n) = 3.0_WP / 10.0_WP
                a(n) = 5.0_WP * ( 15.0_WP +  14.0_WP * alpha(n)) / 64.0_WP
                b(n) =          (-25.0_WP + 126.0_WP * alpha(n)) / 128.0_WP
                c(n) =          (  3.0_WP -  10.0_WP * alpha(n)) / 128.0_WP
          case default
            print*, "Invalid accuracy" 
        end select
      end do
!----------------------------------------------------------------------------------------------------------
!interpolation : C2P for IBC_PERIODIC, unknowns from both lhs and rhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      m1fC2P(1:5, 1, IBC_PERIODIC, n) = alpha(n)
      m1fC2P(1:5, 2, IBC_PERIODIC, n) = ONE
      m1fC2P(1:5, 3, IBC_PERIODIC, n) = alpha(n)
      m1rC2P(1:5, 1, IBC_PERIODIC, n) = a(n) * HALF
      m1rC2P(1:5, 2, IBC_PERIODIC, n) = b(n) * HALF
      m1rC2P(1:5, 3, IBC_PERIODIC, n) = ZERO ! not used
    end do 
!----------------------------------------------------------------------------------------------------------
!interpolation. C2P for IBC_SYMMETRIC, unknowns from both lhs and rhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      m1fC2P(1,   1, IBC_SYMMETRIC, n) = ZERO ! not used
      m1fC2P(1,   2, IBC_SYMMETRIC, n) = ONE
      m1fC2P(1,   3, IBC_SYMMETRIC, n) = alpha(n) + alpha(n)
      m1fC2P(5,   1, IBC_SYMMETRIC, n) = m1fC2P(1,   3, IBC_SYMMETRIC, n)
      m1fC2P(5,   2, IBC_SYMMETRIC, n) = m1fC2P(1,   2, IBC_SYMMETRIC, n)
      m1fC2P(5,   3, IBC_SYMMETRIC, n) = m1fC2P(1,   1, IBC_SYMMETRIC, n)
      m1fC2P(2:4, :, IBC_SYMMETRIC, n) = m1fC2P(2:4, :, IBC_PERIODIC , n)
      m1rC2P(:,   :, IBC_SYMMETRIC, n) = m1rC2P(:,   :, IBC_PERIODIC , n)
    end do
!----------------------------------------------------------------------------------------------------------
!interpolation. C2P for IBC_ASYMMETRIC, unknowns from both lhs and rhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      m1fC2P(1,   1, IBC_ASYMMETRIC, n) = ZERO ! not used
      m1fC2P(1,   2, IBC_ASYMMETRIC, n) = ONE
      m1fC2P(1,   3, IBC_ASYMMETRIC, n) = ZERO
      m1fC2P(5,   1, IBC_ASYMMETRIC, n) = m1fC2P(1,   3, IBC_ASYMMETRIC, n)
      m1fC2P(5,   2, IBC_ASYMMETRIC, n) = m1fC2P(1,   2, IBC_ASYMMETRIC, n)
      m1fC2P(5,   3, IBC_ASYMMETRIC, n) = m1fC2P(1,   1, IBC_ASYMMETRIC, n)
      m1fC2P(2:4, :, IBC_ASYMMETRIC, n) = m1fC2P(2:4, :, IBC_PERIODIC  , n)
      m1rC2P(:,   :, IBC_ASYMMETRIC, n) = m1rC2P(:,   :, IBC_PERIODIC  , n)
    end do
!----------------------------------------------------------------------------------------------------------
! interpolation : C2P for IBC_INTERIOR, unknowns only from only rhs could be reconstructed from bc, thus explicit
!----------------------------------------------------------------------------------------------------------
    m1fC2P(:, :, IBC_INTERIOR, :) = m1fC2P(:, :, IBC_PERIODIC, :)
    m1rC2P(:, :, IBC_INTERIOR, :) = m1rC2P(:, :, IBC_PERIODIC, :)

    do n = 1, NACC
      if(n == IACCU_CP4 .or. n == IACCU_CP6) then
        m1fC2P(1, :, IBC_INTERIOR, n) = m1fC2P(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        m1rC2P(1, :, IBC_INTERIOR, n) = m1rC2P(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        m1fC2P(5, :, IBC_INTERIOR, n) = m1fC2P(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        m1rC2P(5, :, IBC_INTERIOR, n) = m1rC2P(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
      end if
    end do
!----------------------------------------------------------------------------------------------------------
! interpolation. C2P, exterpolation
!----------------------------------------------------------------------------------------------------------
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO 
        c1 = ZERO
        d1 = ZERO
        e1 = ZERO
        f1 = ZERO 
    do n = 1, NACC
      select case (n)
        case (IACCU_CD2) 
          alpha1(n) = 0.0_WP
              a1(n) = ( 3.0_WP + alpha1(n)) / 2.0_WP
              b1(n) = (-1.0_WP + alpha1(n)) / 2.0_WP
        case (IACCU_CD4) 
          alpha1(n) = 0.0_WP
              a1(n) = ( 35.0_WP +  5.0_WP * alpha1(n)) / 16.0_WP
              b1(n) = (-35.0_WP + 15.0_WP * alpha1(n)) / 16.0_WP
              c1(n) = ( 21.0_WP -  5.0_WP * alpha1(n)) / 16.0_WP
              d1(n) = ( -5.0_WP +           alpha1(n)) / 16.0_WP
        case (IACCU_CP4) 
          alpha1(n) = 5.0_WP
              a1(n) = ( 35.0_WP +  5.0_WP * alpha1(n)) / 16.0_WP
              b1(n) = (-35.0_WP + 15.0_WP * alpha1(n)) / 16.0_WP
              c1(n) = ( 21.0_WP -  5.0_WP * alpha1(n)) / 16.0_WP
              d1(n) = ( -5.0_WP +           alpha1(n)) / 16.0_WP
        case (IACCU_CP6) 
          alpha1(n) = 9.0_WP
              a1(n) = (  693.0_WP +  63.0_WP * alpha1(n)) / 256.0_WP
              b1(n) = (-1155.0_WP + 315.0_WP * alpha1(n)) / 256.0_WP
              c1(n) = (  693.0_WP - 105.0_WP * alpha1(n)) / 128.0_WP
              d1(n) = ( -495.0_WP +  63.0_WP * alpha1(n)) / 128.0_WP
              e1(n) = (  385.0_WP -  45.0_WP * alpha1(n)) / 256.0_WP
              f1(n) = (  -63.0_WP +   7.0_WP * alpha1(n)) / 256.0_WP
        case default
          print*, "Invalid accuracy" 
      end select
    end do

    do n = 1, NACC
      m1fC2P(1, 1,   IBC_INTRPL, n) = ZERO ! not used
      m1fC2P(1, 2,   IBC_INTRPL, n) = ONE
      m1fC2P(1, 3,   IBC_INTRPL, n) = alpha1(n)
      
      m1rC2P(1, 1,   IBC_INTRPL, n) = a1(n)
      m1rC2P(1, 2,   IBC_INTRPL, n) = b1(n)
      m1rC2P(1, 3,   IBC_INTRPL, n) = c1(n)
      m1rC2P(1, 4,   IBC_INTRPL, n) = d1(n)
      m1rC2P(1, 5,   IBC_INTRPL, n) = e1(n)
      m1rC2P(1, 6,   IBC_INTRPL, n) = f1(n)

      m1fC2P(5, 1,   IBC_INTRPL, n) = m1fC2P(1, 3, IBC_INTRPL, n)
      m1fC2P(5, 2,   IBC_INTRPL, n) = m1fC2P(1, 2, IBC_INTRPL, n)
      m1fC2P(5, 3,   IBC_INTRPL, n) = m1fC2P(1, 1, IBC_INTRPL, n)
      m1rC2P(5, :,   IBC_INTRPL, n) = m1rC2P(1, :, IBC_INTRPL, n)
    end do
    
    do n = 1, NACC
      m1fC2P(3, :, IBC_INTRPL, n) = m1fC2P(3, :, IBC_PERIODIC, n)
      m1rC2P(3, :, IBC_INTRPL, n) = m1rC2P(3, :, IBC_PERIODIC, n)
    end do

    alpha2 = ZERO
        a2 = ZERO
        b2 = ZERO 
        c2 = ZERO
        d2 = ZERO
        e2 = ZERO
        f2 = ZERO 
    do n = 1, NACC
      select case (n)
        case (IACCU_CD2) 
          alpha2(n) = ZERO
              a2(n) = (1.0_WP + 2.0_WP * alpha2(n)) / 2.0_WP
              b2(n) = (1.0_WP + 2.0_WP * alpha2(n)) / 2.0_WP
        case (IACCU_CD4) 
          alpha2(n) = ZERO
              a2(n) = ( 5.0_WP + 34.0_WP * alpha2(n)) / 16.0_WP
              b2(n) = (15.0_WP - 26.0_WP * alpha2(n)) / 16.0_WP
              c2(n) = (-5.0_WP + 30.0_WP * alpha2(n)) / 16.0_WP
              d2(n) = ( 1.0_WP -  6.0_WP * alpha2(n)) / 16.0_WP
        case (IACCU_CP4) 
          alpha2(n) = 1.0_WP/6.0_WP
              a2(n) = ( 5.0_WP + 34.0_WP * alpha2(n)) / 16.0_WP
              b2(n) = (15.0_WP - 26.0_WP * alpha2(n)) / 16.0_WP
              c2(n) = (-5.0_WP + 30.0_WP * alpha2(n)) / 16.0_WP
              d2(n) = ( 1.0_WP -  6.0_WP * alpha2(n)) / 16.0_WP
        case (IACCU_CP6) 
          alpha2(n) = 9.0_WP 
              a2(n) = (  63.0_WP +  686.0_WP * alpha2(n)) / 256.0_WP
              b2(n) = ( 315.0_WP - 1050.0_WP * alpha2(n)) / 256.0_WP
              c2(n) = (-105.0_WP +  798.0_WP * alpha2(n)) / 128.0_WP
              d2(n) = (  63.0_WP -  530.0_WP * alpha2(n)) / 128.0_WP
              e2(n) = ( -45.0_WP +  406.0_WP * alpha2(n)) / 256.0_WP
              f2(n) = (   7.0_WP -   66.0_WP * alpha2(n)) / 256.0_WP
        case default
          print*, "Invalid accuracy" 
      end select
    end do

    do n = 1, NACC
      m1fC2P(2, 1, IBC_INTRPL, n) = alpha2(n)
      m1fC2P(2, 2, IBC_INTRPL, n) = ONE
      m1fC2P(2, 3, IBC_INTRPL, n) = alpha2(n)

      m1rC2P(2, 1, IBC_INTRPL, n) = a2(n)
      m1rC2P(2, 2, IBC_INTRPL, n) = b2(n)
      m1rC2P(2, 3, IBC_INTRPL, n) = c2(n)
      m1rC2P(2, 4, IBC_INTRPL, n) = d2(n)
      m1rC2P(2, 5, IBC_INTRPL, n) = e2(n)
      m1rC2P(2, 6, IBC_INTRPL, n) = f2(n)

      m1fC2P(4, 1,   IBC_INTRPL, n) = m1fC2P(2, 3, IBC_INTRPL, n)
      m1fC2P(4, 2,   IBC_INTRPL, n) = m1fC2P(2, 2, IBC_INTRPL, n)
      m1fC2P(4, 3,   IBC_INTRPL, n) = m1fC2P(2, 1, IBC_INTRPL, n)
      m1rC2P(4, :,   IBC_INTRPL, n) = m1rC2P(2, :, IBC_INTRPL, n)
    end do
!----------------------------------------------------------------------------------------------------------
! interpolation : C2P, IBC_DIRICHLET, IBC_NEUMANN
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      m1fC2P(1, 1,   IBC_DIRICHLET, n) = ZERO ! not used
      m1fC2P(1, 2,   IBC_DIRICHLET, n) = ONE
      m1fC2P(1, 3,   IBC_DIRICHLET, n) = ZERO
      m1rC2P(1, :,   IBC_DIRICHLET, n) = ZERO ! not used
      m1fC2P(5, 1,   IBC_DIRICHLET, n) = ZERO
      m1fC2P(5, 2,   IBC_DIRICHLET, n) = ONE
      m1fC2P(5, 3,   IBC_DIRICHLET, n) = ZERO
      m1rC2P(5, :,   IBC_DIRICHLET, n) = ZERO
    end do

    if(bc_intp_upw) then
      m1fC2P(:,   :, IBC_NEUMANN,   :) = m1fC2P(:,   :, IBC_INTRPL, :)
      m1rC2P(:,   :, IBC_NEUMANN,   :) = m1rC2P(:,   :, IBC_INTRPL, :)
      m1fC2P(2:4, :, IBC_DIRICHLET, :) = m1fC2P(2:4, :, IBC_INTRPL, :)
      m1rC2P(2:4, :, IBC_DIRICHLET, :) = m1rC2P(2:4, :, IBC_INTRPL, :)
    end if

    if(bc_ghost_cd) then
      m1fC2P(:,   :, IBC_NEUMANN,   :) = m1fC2P(:,   :, IBC_INTERIOR, :)
      m1rC2P(:,   :, IBC_NEUMANN,   :) = m1rC2P(:,   :, IBC_INTERIOR, :)
      m1fC2P(2:4, :, IBC_DIRICHLET, :) = m1fC2P(2:4, :, IBC_INTERIOR, :)
      m1rC2P(2:4, :, IBC_DIRICHLET, :) = m1rC2P(2:4, :, IBC_INTERIOR, :)
    end if

!==========================================================================================================
!interpolation. P2C 
! P2C : i_max = nc
! alpha * f_{i-1} + f_i + alpha * f_{i+1} =    a/2 * ( f_{i'}   + f_{i'+1} ) + &
!                                              b/2 * ( f_{i'+2} + f_{i'-1} )
! when i = 1,    need: LHS: f_{0}, RHS: f_{0'}
! when i = 2,    need: nothing
! when i = nc-1, need: nothing
! when i = nc,   need: LHS: f_{np}, RHS: f_{np'+1}
!==========================================================================================================
!----------------------------------------------------------------------------------------------------------
!interpolation : P2C for IBC_PERIODIC, unknowns from both lhs and rhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    m1fP2C(:, :, IBC_PERIODIC, :) = m1fC2P(:, :, IBC_PERIODIC, :)
    m1rP2C(:, :, IBC_PERIODIC, :) = m1rC2P(:, :, IBC_PERIODIC, :)
!----------------------------------------------------------------------------------------------------------
!interpolation. P2C. IBC_SYMMETRIC, unknowns from both lhs and rhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      m1fP2C(1,   1, IBC_SYMMETRIC, n) = ZERO ! not used
      m1fP2C(1,   2, IBC_SYMMETRIC, n) = ONE + alpha(n)
      m1fP2C(1,   3, IBC_SYMMETRIC, n) = alpha(n)
      m1fP2C(5,   1, IBC_SYMMETRIC, n) = m1fP2C(1,   3, IBC_SYMMETRIC, n)
      m1fP2C(5,   2, IBC_SYMMETRIC, n) = m1fP2C(1,   2, IBC_SYMMETRIC, n)
      m1fP2C(5,   3, IBC_SYMMETRIC, n) = m1fP2C(1,   1, IBC_SYMMETRIC, n)
      m1fP2C(2:4, :, IBC_SYMMETRIC, n) = m1fP2C(2:4, :, IBC_PERIODIC , n)
      m1rP2C(:,   :, IBC_SYMMETRIC, n) = m1rP2C(:,   :, IBC_PERIODIC , n)
    end do
!----------------------------------------------------------------------------------------------------------
!interpolation. P2C. IBC_ASYMMETRIC, unknowns from both lhs and rhs could be reconstructed from bc.
!----------------------------------------------------------------------------------------------------------
    do n = 1, NACC
      m1fP2C(1,   1, IBC_ASYMMETRIC, n) = ZERO ! not used
      m1fP2C(1,   2, IBC_ASYMMETRIC, n) = ONE - alpha(n)
      m1fP2C(1,   3, IBC_ASYMMETRIC, n) = alpha(n)
      m1fP2C(5,   1, IBC_ASYMMETRIC, n) = m1fP2C(1,   3, IBC_ASYMMETRIC, n)
      m1fP2C(5,   2, IBC_ASYMMETRIC, n) = m1fP2C(1,   2, IBC_ASYMMETRIC, n)
      m1fP2C(5,   3, IBC_ASYMMETRIC, n) = m1fP2C(1,   1, IBC_ASYMMETRIC, n)
      m1fP2C(2:4, :, IBC_ASYMMETRIC, n) = m1fP2C(2:4, :, IBC_PERIODIC  , n)
      m1rP2C(:,   :, IBC_ASYMMETRIC, n) = m1rP2C(:,   :, IBC_PERIODIC  , n)
    end do
!----------------------------------------------------------------------------------------------------------
! interpolation : P2C, IBC_INTERIOR
!----------------------------------------------------------------------------------------------------------
    m1fP2C(:, :, IBC_INTERIOR, :) = m1fP2C(:, :, IBC_PERIODIC, :)
    m1rP2C(:, :, IBC_INTERIOR, :) = m1rP2C(:, :, IBC_PERIODIC, :)

    do n = 1, NACC
      if(n == IACCU_CP4 .or. n == IACCU_CP6) then
        m1fP2C(1, :, IBC_INTERIOR, n) = m1fP2C(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        m1rP2C(1, :, IBC_INTERIOR, n) = m1rP2C(1, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        m1fP2C(5, :, IBC_INTERIOR, n) = m1fP2C(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
        m1rP2C(5, :, IBC_INTERIOR, n) = m1rP2C(5, :, IBC_PERIODIC, IACCU_CD4) ! 5 cell stencil, 6th CP --> 4th CD
      end if
    end do
!----------------------------------------------------------------------------------------------------------
! interpolation. P2C: exterpolation
! [ 1    alpha1                          ][f_1']=[a1 * f_{1'} + b1 * f_{2'} + c1 * f_{3'}  ]
! [      alpha2 1     alpha2             ][f_2'] [a2/2 * (f_{2'}   + f_{3'})]
! [             alpha 1      alpha       ][f_i'] [a/2  * (f_{i'}   + f_{i'+1}) + b/2 * (f_{i'+2} + f_{i'-1})]
! [                   alpha2 1     alpha2][f_4'] [a2/2 * (f_{n'-1}   + f_{n'})]
! [                          alpha1 1    ][f_5'] [a1   * f_{n'+1} + b1 * f_{n'} + c1 * f_{n'-1}]
!-----------------------------------------------------------------------------
!    ref: [Gaitonde1998] Table 2.8
    alpha1 = ZERO
        a1 = ZERO
        b1 = ZERO
        c1 = ZERO
        d1 = ZERO
        e1 = ZERO
        f1 = ZERO

    do n = 1, NACC
      select case (n)
        case (IACCU_CD2) 
          alpha1(n) = 0.0_WP
              a1(n) = 0.5_WP
              b1(n) = 0.5_WP
              c1(n) = 0.0_WP
        case (IACCU_CD4) 
          alpha1(n) = 0.0_WP
              a1(n) = ( 5.0_WP -          alpha1(n)) / 16.0_WP
              b1(n) = (15.0_WP + 9.0_WP * alpha1(n)) / 16.0_WP
              c1(n) = (-5.0_WP + 9.0_WP * alpha1(n)) / 16.0_WP
              d1(n) = ( 1.0_WP -          alpha1(n)) / 16.0_WP
        case (IACCU_CP4) 
          alpha1(n) = 1.0_WP
              a1(n) = ( 5.0_WP -          alpha1(n)) / 16.0_WP
              b1(n) = (15.0_WP + 9.0_WP * alpha1(n)) / 16.0_WP
              c1(n) = (-5.0_WP + 9.0_WP * alpha1(n)) / 16.0_WP
              d1(n) = ( 1.0_WP -          alpha1(n)) / 16.0_WP          
        case (IACCU_CP6) 
          alpha1(n) = 7.0_WP / 3.0_WP
              a1(n) = ( 63.0_WP -   7.0_WP * alpha1(n)) / 256.0_WP
              b1(n) = (315.0_WP + 105.0_WP * alpha1(n)) / 256.0_WP
              c1(n) = ( -7.0_WP +   7.0_WP * alpha1(n)) * 15.0_WP / 128.0_WP
              d1(n) = ( 63.0_WP -  35.0_WP * alpha1(n)) / 128.0_WP
              e1(n) = (-15.0_WP +   7.0_WP * alpha1(n)) * 3.0_WP / 256.0_WP
              f1(n) = (  7.0_WP -   3.0_WP * alpha1(n)) / 256.0_WP
        case default
          print*, "Invalid accuracy" 
      end select
    end do

    do n = 1, NACC
      m1fP2C(1, 1, IBC_INTRPL, n) = ZERO ! not used
      m1fP2C(1, 2, IBC_INTRPL, n) = ONE
      m1fP2C(1, 3, IBC_INTRPL, n) = alpha1(n)

      m1rP2C(1, 1, IBC_INTRPL, n) = a1(n)
      m1rP2C(1, 2, IBC_INTRPL, n) = b1(n)
      m1rP2C(1, 3, IBC_INTRPL, n) = c1(n)
      m1rP2C(1, 4, IBC_INTRPL, n) = d1(n)
      m1rP2C(1, 5, IBC_INTRPL, n) = e1(n)
      m1rP2C(1, 6, IBC_INTRPL, n) = f1(n)

      m1fP2C(5, 1, IBC_INTRPL, n) = m1fP2C(1, 3, IBC_INTRPL, n)
      m1fP2C(5, 2, IBC_INTRPL, n) = m1fP2C(1, 2, IBC_INTRPL, n)
      m1fP2C(5, 3, IBC_INTRPL, n) = m1fP2C(1, 1, IBC_INTRPL, n)
      m1rP2C(5, :, IBC_INTRPL, n) = m1rP2C(1, :, IBC_INTRPL, n)

      m1fP2C(2:4, :, IBC_INTRPL, n) = m1fP2C(2:4, :, IBC_PERIODIC, n)
      m1rP2C(2:4, :, IBC_INTRPL, n) = m1rP2C(2:4, :, IBC_PERIODIC, n)
    end do
!----------------------------------------------------------------------------------------------------------
!interpolation. P2C. IBC_DIRICHLET, IBC_NEUMANN
!----------------------------------------------------------------------------------------------------------
    if(bc_intp_upw) then
      m1fP2C(:, :, IBC_NEUMANN,   :) = m1fP2C(:, :, IBC_INTRPL, :)
      m1rP2C(:, :, IBC_NEUMANN,   :) = m1rP2C(:, :, IBC_INTRPL, :)
      m1fP2C(:, :, IBC_DIRICHLET, :) = m1fP2C(:, :, IBC_INTRPL, :)
      m1rP2C(:, :, IBC_DIRICHLET, :) = m1rP2C(:, :, IBC_INTRPL, :)
    end if

    if(bc_ghost_cd) then
      m1fP2C(:, :, IBC_NEUMANN,   :) = m1fP2C(:, :, IBC_INTERIOR, :)
      m1rP2C(:, :, IBC_NEUMANN,   :) = m1rP2C(:, :, IBC_INTERIOR, :)
      m1fP2C(:, :, IBC_DIRICHLET, :) = m1fP2C(:, :, IBC_INTERIOR, :)
      m1rP2C(:, :, IBC_DIRICHLET, :) = m1rP2C(:, :, IBC_INTERIOR, :)
    end if
    
    if(nrank == 0) call Print_debug_end_msg()
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
  subroutine Buildup_TDMA_LHS_array(n, coeff, a, b, c, d)
    use tridiagonal_matrix_algorithm
    use parameters_constant_mod
    implicit none

    integer, intent(in) :: n
    real(WP), intent(in)   :: coeff(NL, NS, NBCS:NBCE, NACC)
    real(WP), intent(out)  :: a(n, NBCS:NBCE, NBCS:NBCE, NACC), &
                              b(n, NBCS:NBCE, NBCS:NBCE, NACC), &
                              c(n, NBCS:NBCE, NBCS:NBCE, NACC), &
                              d(n, NBCS:NBCE, NBCS:NBCE, NACC)

    integer :: i, j, m, k

    a(:, :, :, :) =  ZERO
    b(:, :, :, :) =  ZERO
    c(:, :, :, :) =  ZERO
    d(:, :, :, :) =  ZERO
    k = IBC_PERIODIC
    do m = 1, NACC
      do j = NBCS, NBCE
        do i = NBCS, NBCE

          a(1,         i, j, m) = coeff( 1, 1, i, m)
          a(2,         i, j, m) = coeff( 2, 1, i, m)
          a(3 : n - 2, i, j, m) = coeff( 3, 1, k, m)
          a(n - 1,     i, j, m) = coeff( 4, 1, j, m)
          a(n,         i, j, m) = coeff( 5, 1, j, m)

          b(1,         i, j, m) = coeff( 1, 2, i, m)
          b(2,         i, j, m) = coeff( 2, 2, i, m)
          b(3 : n - 2, i, j, m) = coeff( 3, 2, k, m)
          b(n - 1,     i, j, m) = coeff( 4, 2, j, m)
          b(n,         i, j, m) = coeff( 5, 2, j, m)

          c(1,         i, j, m) = coeff( 1, 3, i, m)
          c(2,         i, j, m) = coeff( 2, 3, i, m)
          c(3 : n - 2, i, j, m) = coeff( 3, 3, k, m)
          c(n - 1,     i, j, m) = coeff( 4, 3, j, m)
          c(n,         i, j, m) = coeff( 5, 3, j, m)

          if (j == k .and. i == k) then
            call Preprocess_TDMA_coeffs( a(1:n-1, i, j, m), &
                                         b(1:n-1, i, j, m), &
                                         c(1:n-1, i, j, m), &
                                         d(1:n-1, i, j, m), &
                                         n-1)
          else 
            call Preprocess_TDMA_coeffs( a(:, i, j, m), &
                                         b(:, i, j, m), &
                                         c(:, i, j, m), &
                                         d(:, i, j, m), &
                                         n)
          end if 
        end do
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
!   building up the basic lhs coeffients for compact schemes
!==========================================================================================================
    call Prepare_compact_coefficients
!==========================================================================================================
!   building up the full size lhs coeffients for compact schemes
!==========================================================================================================
!----------------------------------------------------------------------------------------------------------
! y-direction, with nc unknows
!----------------------------------------------------------------------------------------------------------
    i = 2
    nsz = domain(1)%nc(i)

    allocate (ad1y_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1y_C2C = ZERO
    allocate (bd1y_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1y_C2C = ZERO
    allocate (cd1y_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1y_C2C = ZERO
    allocate (dd1y_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1y_C2C = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fC2C, &
          ad1y_C2C, bd1y_C2C, cd1y_C2C, dd1y_C2C)

    allocate (ad1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1y_P2C = ZERO
    allocate (bd1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1y_P2C = ZERO
    allocate (cd1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1y_P2C = ZERO
    allocate (dd1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1y_P2C = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fP2C, &
          ad1y_P2C, bd1y_P2C, cd1y_P2C, dd1y_P2C)

    allocate (am1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); am1y_P2C = ZERO
    allocate (bm1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bm1y_P2C = ZERO
    allocate (cm1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cm1y_P2C = ZERO
    allocate (dm1y_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dm1y_P2C = ZERO
    call Buildup_TDMA_LHS_array(nsz, m1fP2C, &
          am1y_P2C, bm1y_P2C, cm1y_P2C, dm1y_P2C)

!----------------------------------------------------------------------------------------------------------
! y-direction, with np unknows
!----------------------------------------------------------------------------------------------------------
    nsz = domain(1)%np(i)

    allocate (ad1y_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1y_P2P = ZERO
    allocate (bd1y_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1y_P2P = ZERO
    allocate (cd1y_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1y_P2P = ZERO
    allocate (dd1y_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1y_P2P = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fP2P, &
          ad1y_P2P, bd1y_P2P, cd1y_P2P, dd1y_P2P)

    allocate (ad1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1y_C2P = ZERO
    allocate (bd1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1y_C2P = ZERO
    allocate (cd1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1y_C2P = ZERO
    allocate (dd1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1y_C2P = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fC2P, &
          ad1y_C2P, bd1y_C2P, cd1y_C2P, dd1y_C2P) 

    allocate (am1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); am1y_C2P = ZERO
    allocate (bm1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bm1y_C2P = ZERO
    allocate (cm1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cm1y_C2P = ZERO
    allocate (dm1y_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dm1y_C2P = ZERO
    call Buildup_TDMA_LHS_array(nsz, m1fC2P, &
          am1y_C2P, bm1y_C2P, cm1y_C2P, dm1y_C2P)

!----------------------------------------------------------------------------------------------------------
! z-direction, with nc unknows
!----------------------------------------------------------------------------------------------------------
    i = 3
    nsz = domain(1)%nc(i)

    allocate (ad1z_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1z_C2C = ZERO
    allocate (bd1z_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1z_C2C = ZERO
    allocate (cd1z_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1z_C2C = ZERO
    allocate (dd1z_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1z_C2C = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fC2C, &
          ad1z_C2C, bd1z_C2C, cd1z_C2C, dd1z_C2C)

    allocate (ad1z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1z_P2C = ZERO
    allocate (bd1z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1z_P2C = ZERO
    allocate (cd1z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1z_P2C = ZERO
    allocate (dd1z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1z_P2C = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fP2C, &
          ad1z_P2C, bd1z_P2C, cd1z_P2C, dd1z_P2C)

    allocate (am1z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); am1z_P2C = ZERO
    allocate (bm1z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bm1z_P2C = ZERO
    allocate (cm1z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cm1z_P2C = ZERO
    allocate (dm2z_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dm2z_P2C = ZERO
    call Buildup_TDMA_LHS_array(nsz, m1fP2C, &
          am1z_P2C, bm1z_P2C, cm1z_P2C, dm2z_P2C)

!----------------------------------------------------------------------------------------------------------
! z-direction, with np unknows
!----------------------------------------------------------------------------------------------------------
    nsz = domain(1)%np(i)
!----------------------------------------------------------------------------------------------------------
! 1st derivative in z direction with np unknows
!----------------------------------------------------------------------------------------------------------
    allocate (ad1z_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1z_P2P = ZERO
    allocate (bd1z_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1z_P2P = ZERO
    allocate (cd1z_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1z_P2P = ZERO
    allocate (dd1z_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1z_P2P = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fP2P, &
          ad1z_P2P, bd1z_P2P, cd1z_P2P, dd1z_P2P)

    allocate (ad1z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); ad1z_C2P = ZERO
    allocate (bd1z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bd1z_C2P = ZERO
    allocate (cd1z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cd1z_C2P = ZERO
    allocate (dd1z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dd1z_C2P = ZERO
    call Buildup_TDMA_LHS_array(nsz, d1fC2P, &
          ad1z_C2P, bd1z_C2P, cd1z_C2P, dd1z_C2P)

    allocate (am1z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); am1z_C2P = ZERO
    allocate (bm1z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); bm1z_C2P = ZERO
    allocate (cm1z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); cm1z_C2P = ZERO
    allocate (dm2z_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); dm2z_C2P = ZERO
    call Buildup_TDMA_LHS_array(nsz, m1fC2P, &
          am1z_C2P, bm1z_C2P, cm1z_C2P, dm2z_C2P)

!----------------------------------------------------------------------------------------------------------
! x-direction
!----------------------------------------------------------------------------------------------------------
    allocate ( xtdma_lhs (nxdomain) )
    do i = 1, nxdomain
!----------------------------------------------------------------------------------------------------------
! x-direction, with nc unknows
!----------------------------------------------------------------------------------------------------------
      nsz = domain(i)%nc(1)

      allocate (xtdma_lhs(i)%ad1x_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%ad1x_C2C = ZERO
      allocate (xtdma_lhs(i)%bd1x_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%bd1x_C2C = ZERO
      allocate (xtdma_lhs(i)%cd1x_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%cd1x_C2C = ZERO
      allocate (xtdma_lhs(i)%dd1x_C2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%dd1x_C2C = ZERO
      call Buildup_TDMA_LHS_array(nsz , d1fC2C, &
            xtdma_lhs(i)%ad1x_C2C, &
            xtdma_lhs(i)%bd1x_C2C, &
            xtdma_lhs(i)%cd1x_C2C, &
            xtdma_lhs(i)%dd1x_C2C)

      allocate (xtdma_lhs(i)%ad1x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%ad1x_P2C = ZERO
      allocate (xtdma_lhs(i)%bd1x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%bd1x_P2C = ZERO
      allocate (xtdma_lhs(i)%cd1x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%cd1x_P2C = ZERO
      allocate (xtdma_lhs(i)%dd1x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%dd1x_P2C = ZERO
      call Buildup_TDMA_LHS_array(nsz , d1fP2C, &
            xtdma_lhs(i)%ad1x_P2C, &
            xtdma_lhs(i)%bd1x_P2C, &
            xtdma_lhs(i)%cd1x_P2C, &
            xtdma_lhs(i)%dd1x_P2C)

      allocate (xtdma_lhs(i)%am1x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%am1x_P2C = ZERO
      allocate (xtdma_lhs(i)%bm1x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%bm1x_P2C = ZERO
      allocate (xtdma_lhs(i)%cm1x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%cm1x_P2C = ZERO
      allocate (xtdma_lhs(i)%dm2x_P2C ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%dm2x_P2C = ZERO
      call Buildup_TDMA_LHS_array(nsz , m1fP2C, &
          xtdma_lhs(i)%am1x_P2C, &
          xtdma_lhs(i)%bm1x_P2C, &
          xtdma_lhs(i)%cm1x_P2C, &
          xtdma_lhs(i)%dm2x_P2C)      

!----------------------------------------------------------------------------------------------------------
! x-direction, with np unknows
!----------------------------------------------------------------------------------------------------------
      nsz = domain(i)%np(1)

      allocate (xtdma_lhs(i)%ad1x_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%ad1x_P2P = ZERO
      allocate (xtdma_lhs(i)%bd1x_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%bd1x_P2P = ZERO
      allocate (xtdma_lhs(i)%cd1x_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%cd1x_P2P = ZERO
      allocate (xtdma_lhs(i)%dd1x_P2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%dd1x_P2P = ZERO
      call Buildup_TDMA_LHS_array(nsz , d1fP2P, &
            xtdma_lhs(i)%ad1x_P2P, &
            xtdma_lhs(i)%bd1x_P2P, &
            xtdma_lhs(i)%cd1x_P2P, &
            xtdma_lhs(i)%dd1x_P2P)

      allocate (xtdma_lhs(i)%ad1x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%ad1x_C2P = ZERO
      allocate (xtdma_lhs(i)%bd1x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%bd1x_C2P = ZERO
      allocate (xtdma_lhs(i)%cd1x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%cd1x_C2P = ZERO
      allocate (xtdma_lhs(i)%dd1x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%dd1x_C2P = ZERO
      call Buildup_TDMA_LHS_array(nsz , d1fC2P, &
            xtdma_lhs(i)%ad1x_C2P, &
            xtdma_lhs(i)%bd1x_C2P, &
            xtdma_lhs(i)%cd1x_C2P, &
            xtdma_lhs(i)%dd1x_C2P)

      allocate (xtdma_lhs(i)%am1x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%am1x_C2P = ZERO
      allocate (xtdma_lhs(i)%bm1x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%bm1x_C2P = ZERO
      allocate (xtdma_lhs(i)%cm1x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%cm1x_C2P = ZERO
      allocate (xtdma_lhs(i)%dm2x_C2P ( nsz, NBCS:NBCE, NBCS:NBCE, NACC ) ); xtdma_lhs(i)%dm2x_C2P = ZERO
      call Buildup_TDMA_LHS_array(nsz , m1fC2P, &
          xtdma_lhs(i)%am1x_C2P, &
          xtdma_lhs(i)%bm1x_C2P, &
          xtdma_lhs(i)%cm1x_C2P, &
          xtdma_lhs(i)%dm2x_C2P)      

    end do

    return
  end subroutine Prepare_LHS_coeffs_for_operations
!==========================================================================================================
!==========================================================================================================
  subroutine buildup_ghost_cells_C(fi, ibc, fc, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: opt_dp(4)
    real(WP), intent(inout ) :: fc(-1:2)
    real(WP), optional, intent(in ) :: opt_fbc(4)

    integer :: nc ! cell number

    nc = size(fi)
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>         -1     0      1     2            nc-1   nc    nc+1   nc+2 
!----------------------------------------------------------------------------------------------------------
    if ( ibc(1) == IBC_INTERIOR) then
      if(.not. present(opt_fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ buildup_ghost_cells_C')
      fc(0 ) = opt_fbc(1)
      fc(-1) = opt_fbc(3)
    else if ( ibc(1) == IBC_PERIODIC) then
      fc(0 ) = fi(nc   )
      fc(-1) = fi(nc -1)
    else if (ibc(1) == IBC_SYMMETRIC ) then
      fc(0 ) = fi(1)
      fc(-1) = fi(2)
    else if (ibc(1) == IBC_ASYMMETRIC) then
      fc(0 ) = -fi(1)
      fc(-1) = -fi(2)
    else if (ibc(1) == IBC_DIRICHLET) then
      !if(.not. present(opt_fbc)) call Print_warning_msg('Lack of fbc info for IBC_DIRICHLET @ buildup_ghost_cells_C')
      fc(0 ) = TWO * opt_fbc(1) - fi(1)
      fc(-1) = TWO * opt_fbc(1) - fi(2)
    else if (ibc(1) == IBC_NEUMANN) then
      !if(.not. present(opt_fbc)) call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ buildup_ghost_cells_C')
      fc(0 ) = fi(1) - opt_fbc(1) * opt_dp(1)
      fc(-1) = fi(2) - opt_fbc(1) * opt_dp(3)
    else
      fc(0 ) = MAXP
      fc(-1) = MAXP
    end if
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>         -1     0      1     2            nc-1   nc    nc+1   nc+2 
!----------------------------------------------------------------------------------------------------------
    if ( ibc(2) == IBC_INTERIOR) then
      if(.not. present(opt_fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ buildup_ghost_cells_C2C')
      fc(1) = opt_fbc(2)
      fc(2) = opt_fbc(4)
    else if ( ibc(2) == IBC_PERIODIC) then
      fc(1) = fi(1)
      fc(2) = fi(2)
    else if (ibc(2) == IBC_SYMMETRIC ) then
      fc(1) = fi(nc   )
      fc(2) = fi(nc -1)
    else if (ibc(2) == IBC_ASYMMETRIC) then
      fc(1) = -fi(nc   )
      fc(2) = -fi(nc -1)
    else if (ibc(2) == IBC_DIRICHLET) then
      if(.not. present(opt_fbc)) call Print_error_msg('Lack of fbc info for IBC_DIRICHLET @ buildup_ghost_cells_C2C')
      fc(1) = TWO * opt_fbc(2) - fi(nc)
      fc(2) = TWO * opt_fbc(2) - fi(nc-1)
    else if (ibc(2) == IBC_NEUMANN) then
      !if(.not. present(opt_fbc)) call Print_warning_msg('Lack of fbc info for IBC_NEUMANN @ buildup_ghost_cells_C2C')
      fc(1) = fi(nc)     + opt_fbc(2) * opt_dp(2)
      fc(2) = fi(nc - 1) + opt_fbc(2) * opt_dp(4)
    else
      fc(1) = MAXP
      fc(2) = MAXP
    end if

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine buildup_ghost_cells_P(fi, ibc, fp, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: opt_dp(4)
    real(WP), intent(inout ) :: fp(-1:2)
    real(WP), optional, intent(in ) :: opt_fbc(4)

    integer :: np

    np = size(fi)
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>      -1     0      1     2     3      np-2   np-1  np    np+1  np+2 (non-periodic)
!>      -1     0      1     2     3      np-1   np    np+1  np+2  np+3 (periodic)
!> dp(1) = ( yp(2) - yp(1) ) * 2
!> dp(3) = ( yp(3) - yp(1) ) * 2
!> dp(2) = ( yp(np) - yp(np-1) ) * 2
!> dp(4) = ( yp(np) - yp(np-2) ) * 2
!----------------------------------------------------------------------------------------------------------
    if ( ibc(1) == IBC_INTERIOR) then
      if(.not. present(opt_fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ buildup_ghost_cells_P')
      fp( 0) = opt_fbc(1)
      fp(-1) = opt_fbc(3)
    else if ( ibc(1) == IBC_PERIODIC) then
      fp( 0) = fi(np   )
      fp(-1) = fi(np -1)
    else if (ibc(1) == IBC_SYMMETRIC ) then
      fp( 0) = fi(2)
      fp(-1) = fi(3)
    else if (ibc(1) == IBC_ASYMMETRIC) then
      fp( 0) = -fi(2)
      fp(-1) = -fi(3)
    else if (ibc(1) == IBC_DIRICHLET) then
      if(present(opt_fbc)) then
        fp( 0) = TWO * opt_fbc(1) - fi(2)
        fp(-1) = TWO * opt_fbc(1) - fi(3)
      else
        fp( 0) = TWO * fi(1) - fi(2)
        fp(-1) = TWO * fi(1) - fi(3)
      end if
    else if (ibc(1) == IBC_NEUMANN) then
      if(.not. present(opt_fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN @ buildup_ghost_cells_P')
      fp( 0) = fi(2) - opt_fbc(1) * opt_dp(1)
      fp(-1) = fi(3) - opt_fbc(1) * opt_dp(3)
    else
      fp( 0) = MAXP
      fp(-1) = MAXP
    end if
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>      -1     0      1     2     3      np-2   np-1  np    np+1  np+2 (non-periodic)
!>      -1     0      1     2     3      np-1   np    np+1  np+2  np+3 (periodic)
!----------------------------------------------------------------------------------------------------------
    if ( ibc(2) == IBC_INTERIOR) then
      if(.not. present(opt_fbc)) call Print_error_msg('Lack of fbc info for IBC_INTERIOR @ buildup_ghost_cells_P')
      fp(1) = opt_fbc(2)
      fp(2) = opt_fbc(4)
    else if ( ibc(2) == IBC_PERIODIC) then
      fp(1) = fi(1)
      fp(2) = fi(2)
    else if (ibc(2) == IBC_SYMMETRIC ) then
      fp(1) = fi(np - 1)
      fp(2) = fi(np - 2)
    else if (ibc(2) == IBC_ASYMMETRIC) then
      fp(1) = - fi(np - 1)
      fp(2) = - fi(np - 2)
    else if (ibc(2) == IBC_NEUMANN) then
      if(.not. present(opt_fbc)) call Print_error_msg('Lack of fbc info for IBC_NEUMANN @ buildup_ghost_cells_P')
      fp(1) = fi(np - 1) + opt_fbc(2) * opt_dp(2)
      fp(2) = fi(np - 2) + opt_fbc(2) * opt_dp(4)
    else if (ibc(2) == IBC_DIRICHLET) then
      if(present(opt_fbc)) then
        fp(1) = TWO * opt_fbc(2) - fi(np - 1)
        fp(2) = TWO * opt_fbc(2) - fi(np - 2)
      else
        fp(1) = TWO * fi(np) - fi(np - 1)
        fp(2) = TWO * fi(np) - fi(np - 2)
      end if
    else
      fp(1) = MAXP
      fp(2) = MAXP
    end if

    return
  end subroutine
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for interpolation.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> \param[in]   np      the number of unknowns, here is np
!> \param[in]   ibc     the b.c. type at two ends of the unknown array
!> \param[in]   fbc     the b.c. values for the given ibc
!> \param[in]   coeff   the defined TDMA coefficients
!> \param[in]   d1      spacing
!> \param[in]   fi      the input variable to build up the RHS array
!> \param[out]  fo      the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_interp_P2C_RHS_array(fi, fo, nc, coeff, ibc, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: nc ! unknow numbers, nc
    real(WP), intent(out) :: fo(nc)
    real(WP), intent(in ) :: coeff(1:NL, 1:2*NS, NBCS:NBCE)
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: opt_dp(4)
    real(WP), optional, intent(in ) :: opt_fbc(4)

    integer :: i!, m, l
    real(WP) :: fp(-1:2)
    logical :: is_bc1, is_bc5

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    do i = 2, nc - 2
      fo(i) = coeff( 3, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( 3, 2, IBC_PERIODIC ) * ( fi(i - 1) + fi(i + 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!                        1     2             nc-1   nc    nc+1  nc+2
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>      -1     0      1     2     3      np-2   np-1  np    np+1  np+2 (non-periodic)
!>      -1     0      1     2     3      np-1   np    np+1  np+2  np+3 (periodic)
!----------------------------------------------------------------------------------------------------------
    call buildup_ghost_cells_P(fi(:), ibc(:), fp(-1:2), opt_fbc(:), opt_dp(:))
    is_bc1 = (ibc(1) == IBC_INTERIOR   .or. &
              ibc(1) == IBC_PERIODIC   .or. &
              ibc(1) == IBC_SYMMETRIC  .or. &
              ibc(1) == IBC_ASYMMETRIC )
    if(bc_ghost_cd) then
      is_bc1 =(is_bc1 .or. &
               ibc(1) == IBC_DIRICHLET .or. &
               ibc(1) == IBC_NEUMANN)
    end if
    is_bc5 = (ibc(2) == IBC_INTERIOR   .or. &
              ibc(2) == IBC_SYMMETRIC  .or. &
              ibc(2) == IBC_ASYMMETRIC )
    if(bc_ghost_cd) then
      is_bc5 =(is_bc5 .or. &
               ibc(2) == IBC_DIRICHLET .or. &
               ibc(2) == IBC_NEUMANN)
    end if 
!----------------------------------------------------------------------------------------------------------
    i = 1
    if(is_bc1) then
      fo(i) = coeff( 1, 1, ibc(1) ) * ( fi(i) + fi(i + 1) ) + &
              coeff( 1, 2, ibc(1) ) * ( fp(0) + fi(i + 2) )
    else
      fo(i) = coeff( 1, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 1, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( 1, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( 1, 4, IBC_INTRPL) * fi(i + 3) + &
              coeff( 1, 5, IBC_INTRPL) * fi(i + 4) + &
              coeff( 1, 6, IBC_INTRPL) * fi(i + 5)
    end if
!----------------------------------------------------------------------------------------------------------
    i = nc
    if(is_bc5) then
      fo(i) = coeff( 5, 1, ibc(2) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( 5, 2, ibc(2) ) * ( fi(i - 1) + fp(1) )
    else if( ibc(2) == IBC_PERIODIC ) then
      fo(i) = coeff( 5, 1, IBC_PERIODIC ) * ( fi(i    ) + fp(1) ) + &
              coeff( 5, 2, IBC_PERIODIC ) * ( fi(i - 1) + fp(2) )
    else
      fo(nc) = coeff( 5, 1, IBC_INTRPL) * fi(i + 1) + &
               coeff( 5, 2, IBC_INTRPL) * fi(i    ) + &
               coeff( 5, 3, IBC_INTRPL) * fi(i - 1) + &
               coeff( 5, 4, IBC_INTRPL) * fi(i - 2) + &
               coeff( 5, 5, IBC_INTRPL) * fi(i - 3) + &
               coeff( 5, 6, IBC_INTRPL) * fi(i - 4)
    end if
!----------------------------------------------------------------------------------------------------------
    i = nc - 1
    if( ibc(2) == IBC_PERIODIC ) then
      fo(i) = coeff( 4, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( 4, 2, IBC_PERIODIC ) * ( fi(i - 1) + fp(1)     )
    else
      fo(i) = coeff( 4, 1, ibc(2) ) * ( fi(i    ) + fi(i + 1) ) + &
              coeff( 4, 2, ibc(2) ) * ( fi(i - 1) + fi(i + 2) )
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
!> \param[in]   np      the number of unknowns, here is np
!> \param[in]   ibc     the b.c. type at two ends of the unknown array
!> \param[in]   fbc     the b.c. values for the given ibc
!> \param[in]   coeff   the defined TDMA coefficients
!> \param[in]   d1      spacing
!> \param[in]   fi      the input variable to build up the RHS array
!> \param[out]  fo      the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_interp_C2P_RHS_array(fi, fo, np, coeff, ibc, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none

    real(WP),           intent(in ) :: fi(:)
    integer,            intent(in ) :: np ! unknow numbers, np
    real(WP),           intent(out) :: fo(np)
    real(WP),           intent(in ) :: coeff(1:NL, 1:2*NS, NBCS:NBCE)
    real(WP), optional, intent(in ) :: opt_dp(4)
    integer,            intent(in ) :: ibc(2)
    real(WP), optional, intent(in)  :: opt_fbc(4) ! used for Dirichlet B.C. (1||2) & interior (3, 1,|| 2, 4)

    integer :: i
    real(WP) :: fc(-1:2)
    logical :: is_bc1, is_bc2, is_bc4, is_bc5
!==========================================================================================================
!interpolation. C2P 
! alpha * f_{i'-1} + f_i' + alpha * f_{i'+1} = a/2 * ( f_{i}   + f_{i-1} ) + &
!                                              b/2 * ( f_{i+1} + f_{i-2} )
! when i' = 1,    need: f_{0'}, f_{0}, f_{-1}
! when i' = 2,    need:         f_{0}
! when i' = np-1, need:         f_{np'}
! when i' = np,   need: f_{np'+1}, f_{np'}, f_{np'+1}
!==========================================================================================================
    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    do i = 3, np - 2
      fo(i) = coeff( 3, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( 3, 2, IBC_PERIODIC ) * ( fi(i + 1) + fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>         -1     0      1     2            nc-1   nc    nc+1   nc+2 
!----------------------------------------------------------------------------------------------------------
    call buildup_ghost_cells_C(fi(:), ibc(:), fc(-1:2), opt_fbc(:), opt_dp(:))
    
    is_bc1 = (ibc(1) == IBC_INTERIOR   .or. &
              ibc(1) == IBC_PERIODIC   .or. &
              ibc(1) == IBC_SYMMETRIC  .or. &
              ibc(1) == IBC_ASYMMETRIC)
    if(bc_ghost_cd) then
      is_bc1 = (is_bc1 .or. &
              ibc(1) == IBC_NEUMANN)
    end if

    is_bc5 = (ibc(2) == IBC_INTERIOR   .or. &
              ibc(2) == IBC_SYMMETRIC  .or. &
              ibc(2) == IBC_ASYMMETRIC)
    if(bc_ghost_cd) then
      is_bc5 = (is_bc5 .or. &
              ibc(2) == IBC_NEUMANN)
    end if

    is_bc2 = is_bc1
    if(bc_ghost_cd) then
        is_bc2 = (is_bc2 .or. &
            ibc(1) == IBC_DIRICHLET)
    end if

    is_bc4 = is_bc5
    if(bc_ghost_cd) then
      is_bc4 = (is_bc4 .or. &
            ibc(2) == IBC_DIRICHLET)
    end if
!----------------------------------------------------------------------------------------------------------
    i = 1    
    if(is_bc1) then
      fo(i) = coeff( 1, 1, ibc(1)) * ( fi(i    ) + fc( 0) )+ &
              coeff( 1, 2, ibc(1)) * ( fi(i + 1) + fc(-1) )
    else if (ibc(1) == IBC_DIRICHLET) then
      fo(i) = opt_fbc(1)
    else
      fo(i) = coeff( 1, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 1, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( 1, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( 1, 4, IBC_INTRPL) * fi(i + 3) + &
              coeff( 1, 5, IBC_INTRPL) * fi(i + 4) + &
              coeff( 1, 6, IBC_INTRPL) * fi(i + 5)
    end if
!----------------------------------------------------------------------------------------------------------
    i = 2 
    if(is_bc2) then
      fo(i) = coeff( 2, 1, ibc(1)) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( 2, 2, ibc(1)) * ( fi(i + 1) + fc(0)     )
    else 
      fo(i) = coeff( 2, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( 2, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( 2, 3, IBC_INTRPL) * fi(i + 1) + &
              coeff( 2, 4, IBC_INTRPL) * fi(i + 2) + &
              coeff( 2, 5, IBC_INTRPL) * fi(i + 3) + &
              coeff( 2, 6, IBC_INTRPL) * fi(i + 4)
    end if
!----------------------------------------------------------------------------------------------------------
    i = np
    if(is_bc5) then
      fo(i) = coeff( 5, 1, ibc(2) ) * ( fc(1) + fi(i - 1) ) + &
              coeff( 5, 2, ibc(2) ) * ( fc(2) + fi(i - 2) )
    else if (ibc(2) == IBC_PERIODIC) then
      fo(i) = coeff( 5, 1, IBC_PERIODIC ) * ( fi(i) + fi(i - 1) ) + &
              coeff( 5, 2, IBC_PERIODIC ) * ( fc(1) + fi(i - 2) )
    else if (ibc(2) == IBC_DIRICHLET) then
      fo(i) = opt_fbc(2)
    else
      fo(i) = coeff( 5, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( 5, 2, IBC_INTRPL) * fi(i - 2) + &
              coeff( 5, 3, IBC_INTRPL) * fi(i - 3) + &
              coeff( 5, 4, IBC_INTRPL) * fi(i - 4) + &
              coeff( 5, 5, IBC_INTRPL) * fi(i - 5) + &
              coeff( 5, 6, IBC_INTRPL) * fi(i - 6)
    end if
!----------------------------------------------------------------------------------------------------------
    i = np - 1
    if(is_bc4) then
      fo(i) = coeff( 4, 1, ibc(2) ) * ( fi(i) + fi(i - 1) ) + &
              coeff( 4, 2, ibc(2) ) * ( fc(1) + fi(i - 2) )
    else if (ibc(2) == IBC_PERIODIC) then
      fo(i) = coeff( 4, 1, IBC_PERIODIC ) * ( fi(i    ) + fi(i - 1) ) + &
              coeff( 4, 2, IBC_PERIODIC ) * ( fi(i + 1) + fi(i - 2) )
    else 
      fo(i) = coeff( 4, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 4, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( 4, 3, IBC_INTRPL) * fi(i - 2) + &
              coeff( 4, 4, IBC_INTRPL) * fi(i - 3) + &
              coeff( 4, 5, IBC_INTRPL) * fi(i - 4) + &
              coeff( 4, 6, IBC_INTRPL) * fi(i - 5)
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
! 1st derivative on collocated grids, C2C/P2P coefficients : Periodic or Symmetric B.C.
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
!> \param[in]   nc      the number of unknowns, here is nc
!> \param[in]   ibc     the b.c. type at two ends of the unknown array
!> \param[in]   fbc     the b.c. values for the given ibc
!> \param[in]   coeff   the defined TDMA coefficients
!> \param[in]   dd      1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]   fi      the input variable to build up the RHS array
!> \param[out]  fo      the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_C2C_RHS_array(fi, fo, nc, coeff, dd, ibc, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: nc ! unknow numbers
    real(WP), intent(out) :: fo(nc)
    real(WP), intent(in ) :: coeff(1:NL, 1:2*NS, NBCS:NBCE)
    real(WP), optional, intent(in ) :: opt_dp(4)
    real(WP), intent(in ) :: dd
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in)  :: opt_fbc(4) ! used for Dirichlet B.C. or interior

    integer :: i, l
    real(WP) :: fc(-1:2)
    logical :: is_bc(2)

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
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>         -1     0      1     2            nc-1   nc    nc+1   nc+2 
!----------------------------------------------------------------------------------------------------------
    call buildup_ghost_cells_C(fi(:), ibc(:), fc(-1:2), opt_fbc(:), opt_dp(:))
    do i = 1, 2 
      is_bc(i) = (ibc(i) == IBC_INTERIOR   .or. &
                  ibc(i) == IBC_PERIODIC   .or. &
                  ibc(i) == IBC_SYMMETRIC  .or. &
                  ibc(i) == IBC_ASYMMETRIC)
      if(bc_ghost_cd) then
          is_bc(i) = (is_bc(i)             .or. &
                  ibc(i) == IBC_DIRICHLET  .or. &
                  ibc(i) == IBC_NEUMANN)
      end if
    end do
!----------------------------------------------------------------------------------------------------------
    i = 1
    if(is_bc(1)) then
      fo(i) = coeff( 1, 1, ibc(1) ) * ( fi(i + 1) - fc( 0) ) + &
              coeff( 1, 2, ibc(1) ) * ( fi(i + 2) - fc(-1) )

    else
      fo(i) = coeff( 1, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 1, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( 1, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( 1, 4, IBC_INTRPL) * fi(i + 3) + &
              coeff( 1, 5, IBC_INTRPL) * fi(i + 4) + &
              coeff( 1, 6, IBC_INTRPL) * fi(i + 5)
    end if
!----------------------------------------------------------------------------------------------------------
    i = 2
    if(is_bc(1)) then
      fo(i) = coeff( 2, 1, ibc(1) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( 2, 2, ibc(1) ) * ( fi(i + 2) - fc(0)     )
    else
      fo(i) = coeff( 2, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( 2, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( 2, 3, IBC_INTRPL) * fi(i + 1) + &
              coeff( 2, 4, IBC_INTRPL) * fi(i + 2) + &
              coeff( 2, 5, IBC_INTRPL) * fi(i + 3) + &
              coeff( 2, 6, IBC_INTRPL) * fi(i + 4)
    end if
!----------------------------------------------------------------------------------------------------------
    i = nc  
    if(is_bc(2)) then
      fo(i) = coeff( 5, 1, ibc(2) ) * ( fc(1) - fi(i - 1) ) + &
              coeff( 5, 2, ibc(2) ) * ( fc(2) - fi(i - 2) )
    else
      fo(i) = coeff( 5, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 5, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( 5, 3, IBC_INTRPL) * fi(i - 2) + &
              coeff( 5, 4, IBC_INTRPL) * fi(i - 3) + &
              coeff( 5, 5, IBC_INTRPL) * fi(i - 4) + &
              coeff( 5, 6, IBC_INTRPL) * fi(i - 5)

    end if
!----------------------------------------------------------------------------------------------------------
    i = nc - 1  
    if(is_bc(2)) then
      fo(i) = coeff( 4, 1, ibc(2) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( 4, 2, ibc(2) ) * ( fc(1)     - fi(i - 2) )
    else
      fo(i) = coeff( 4, 1, IBC_INTRPL) * fi(i + 1) + &
              coeff( 4, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( 4, 3, IBC_INTRPL) * fi(i - 1) + &
              coeff( 4, 4, IBC_INTRPL) * fi(i - 2) + &
              coeff( 4, 5, IBC_INTRPL) * fi(i - 3) + &
              coeff( 4, 6, IBC_INTRPL) * fi(i - 4)
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
!----------------------------------------------------------------------------------------------------------
! 1st derivative on collocated grids, C2C/P2P coefficients : Periodic or Symmetric B.C.
! alpha * f'_{i-1} + f'_i + alpha * f'_{i+1} = a/(2h) * ( f_{i+1} - f_{i-1} ) + &
!                                              b/(4h) * ( f_{i+2} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!> \param[in]   np      the number of unknowns, here is np
!> \param[in]   ibc     the b.c. type at two ends of the unknown array
!> \param[in]   fbc     the b.c. values for the given ibc
!> \param[in]   coeff   the defined TDMA coefficients
!> \param[in]   dd      1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]   fi      the input variable to build up the RHS array
!> \param[out]  fo      the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_P2P_RHS_array(fi, fo, np, coeff, dd, ibc, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none
    real(WP),           intent(in ) :: fi(:)
    integer,            intent(in ) :: np ! unknow numbers
    real(WP),           intent(out) :: fo(np)
    real(WP),           intent(in ) :: coeff(1:NL, 1:2*NS, NBCS:NBCE)
    real(WP), optional, intent(in ) :: opt_dp(4)
    real(WP),           intent(in ) :: dd
    integer,            intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: opt_fbc(4) ! used for IBC_NEUMANN or interior

    integer :: i
    real(WP) :: fp(-1:2)
    logical  :: is_bc1(2), is_bc2(2)

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    do i = 3, np - 2
      fo(i) = coeff( 3, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( 3, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>      -1     0      1     2     3      np-2   np-1  np    np+1  np+2 (non-periodic)
!>      -1     0      1     2     3      np-1   np    np+1  np+2  np+3 (periodic)
!----------------------------------------------------------------------------------------------------------
    call buildup_ghost_cells_P(fi(:), ibc(:), fp(-1:2), opt_fbc(:), opt_dp(:))
    do i = 1, 2
      is_bc1(i) = (ibc(i) == IBC_INTERIOR   .or. &
                   ibc(i) == IBC_PERIODIC   .or. &
                   ibc(i) == IBC_SYMMETRIC  .or. &
                   ibc(i) == IBC_ASYMMETRIC)
      if(bc_ghost_cd) then
        is_bc1(i) = (is_bc1(i) .or. &
                   ibc(i) == IBC_DIRICHLET)
      end if
      is_bc2(i) = is_bc1(i)
      if(bc_ghost_cd) then
        is_bc2(i) = (is_bc2(i) .or. &
                   ibc(i) == IBC_DIRICHLET .or. &
                   ibc(i) == IBC_NEUMANN)
      end if
    end do
    
!----------------------------------------------------------------------------------------------------------
    i = 1
    if(is_bc1(1)) then
      fo(i) = coeff( 1, 1, ibc(1) ) * ( fi(i + 1) - fp( 0) ) + &
              coeff( 1, 2, ibc(1) ) * ( fi(i + 2) - fp(-1) )
    else if(ibc(1) == IBC_NEUMANN) then
      fo(i) = opt_fbc(1)/dd
    else
      fo(i) = coeff( 1, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 1, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( 1, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( 1, 4, IBC_INTRPL) * fi(i + 3) + &
              coeff( 1, 5, IBC_INTRPL) * fi(i + 4) + &
              coeff( 1, 6, IBC_INTRPL) * fi(i + 5) 
    end if

!----------------------------------------------------------------------------------------------------------    
    i = 2
    if(is_bc2(1)) then
      fo(i) = coeff( 2, 1, ibc(1) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( 2, 2, ibc(1) ) * ( fi(i + 2) - fp(0)     )
    else
      fo(i) = coeff( 2, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( 2, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( 2, 3, IBC_INTRPL) * fi(i + 1) + &
              coeff( 2, 4, IBC_INTRPL) * fi(i + 2) + &
              coeff( 2, 5, IBC_INTRPL) * fi(i + 3) + &
              coeff( 2, 6, IBC_INTRPL) * fi(i + 4)
    end if
!----------------------------------------------------------------------------------------------------------
    i = np
    if(is_bc1(2)) then
      fo(i) = coeff( 5, 1, ibc(2) ) * ( fp(1) - fi(i - 1) ) + &
              coeff( 5, 2, ibc(2) ) * ( fp(2) - fi(i - 2) )
    else if(ibc(2) == IBC_NEUMANN) then
      fo(i) = opt_fbc(2)/dd
    else
      fo(i) = coeff( 5, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 5, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( 5, 3, IBC_INTRPL) * fi(i - 2) + &
              coeff( 5, 4, IBC_INTRPL) * fi(i - 3) + &
              coeff( 5, 5, IBC_INTRPL) * fi(i - 4) + &
              coeff( 5, 6, IBC_INTRPL) * fi(i - 5)
    end if
!----------------------------------------------------------------------------------------------------------
    i = np - 1
    if(is_bc2(2)) then
      fo(i) = coeff( 4, 1, ibc(2) ) * ( fi(i + 1) - fi(i - 1) ) + &
              coeff( 4, 2, ibc(2) ) * ( fp(1)     - fi(i - 2) )
    else
      fo(i) = coeff( 4, 1, IBC_INTRPL) * fi(i + 1) + &
              coeff( 4, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( 4, 3, IBC_INTRPL) * fi(i - 1) + &
              coeff( 4, 4, IBC_INTRPL) * fi(i - 2) + &
              coeff( 4, 5, IBC_INTRPL) * fi(i - 3) + &
              coeff( 4, 6, IBC_INTRPL) * fi(i - 4)
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd

    return
  end subroutine Prepare_TDMA_1deri_P2P_RHS_array
!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!----------------------------------------------------------------------------------------------------------
! 1st derivative on staggered grids C2P
! C2P ==>
! alpha * f'_{i'-1} + f'_i' + alpha * f'_{i'+1} = a/(h ) * ( f_{i}   - f_{i-1} ) + &
!                                                 b/(3h) * ( f_{i+1} - f_{i-2} )
!----------------------------------------------------------------------------------------------------------
! Arguments
!----------------------------------------------------------------------------------------------------------
!> \param[in]   np      the number of unknowns, here is np
!> \param[in]   ibc     the b.c. type at two ends of the unknown array
!> \param[in]   fbc     the b.c. values for the given ibc
!> \param[in]   coeff   the defined TDMA coefficients
!> \param[in]   dd      1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]   fi      the input variable to build up the RHS array
!> \param[out]  fo      the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_C2P_RHS_array(fi, fo, np, coeff, dd, ibc, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none
    real(WP),           intent(in ) :: fi(:)
    integer,            intent(in ) :: np ! unknow numbers, np
    real(WP),           intent(out) :: fo(np)
    real(WP),           intent(in ) :: coeff(1:NL, 1:2*NS, NBCS:NBCE)
    real(WP), optional, intent(in ) :: opt_dp(4)
    real(WP),           intent(in ) :: dd
    integer,            intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: opt_fbc(4) ! used for IBC_NEUMANN, and interior

    integer :: i!, m, l
    real(WP) :: fc(-1:2)
    logical  :: is_bc1, is_bc2, is_bc4, is_bc5

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    do i = 3, np - 2
      fo(i) = coeff( 3, 1, IBC_PERIODIC ) * ( fi(i    ) - fi(i - 1) ) + &
              coeff( 3, 2, IBC_PERIODIC ) * ( fi(i + 1) - fi(i - 2) )
    end do
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>         -1     0      1     2            nc-1   nc    nc+1   nc+2 
!----------------------------------------------------------------------------------------------------------
    call buildup_ghost_cells_C(fi(:), ibc(:), fc(-1:2), opt_fbc(:), opt_dp(:))
    is_bc1 = (ibc(1) == IBC_INTERIOR   .or. &
              ibc(1) == IBC_PERIODIC   .or. &
              ibc(1) == IBC_SYMMETRIC  .or. &
              ibc(1) == IBC_ASYMMETRIC)
    if(bc_ghost_cd) then
      is_bc1 = (is_bc1 .or. &
              ibc(1) == IBC_DIRICHLET)
    end if
    is_bc2 = is_bc1
    if(bc_ghost_cd) then
        is_bc2 = (is_bc2 .or. &
            ibc(1) == IBC_DIRICHLET .or. &
            ibc(1) == IBC_NEUMANN)
    end if
    is_bc5 = (ibc(2) == IBC_INTERIOR   .or. &
              ibc(2) == IBC_SYMMETRIC  .or. &
              ibc(2) == IBC_ASYMMETRIC)
    if(bc_ghost_cd) then
      is_bc5 = (is_bc5 .or. &
              ibc(2) == IBC_DIRICHLET)
    end if
    is_bc4 = is_bc5
    if(bc_ghost_cd) then
      is_bc4 = (is_bc4 .or. &
            ibc(1) == IBC_DIRICHLET .or. &
            ibc(1) == IBC_NEUMANN)
    end if
!----------------------------------------------------------------------------------------------------------
    i = 1
    if(is_bc1) then
      fo(i) = coeff( 1, 1, ibc(1) ) * ( fi(i    ) - fc( 0) ) + &
              coeff( 1, 2, ibc(1) ) * ( fi(i + 1) - fc(-1) )
    else if(ibc(1) == IBC_NEUMANN) then
      fo(i) = opt_fbc(1)/dd
    else
      fo(i) = coeff( 1, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 1, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( 1, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( 1, 4, IBC_INTRPL) * fi(i + 3) + &
              coeff( 1, 5, IBC_INTRPL) * fi(i + 4) + &
              coeff( 1, 6, IBC_INTRPL) * fi(i + 5)
    end if
!----------------------------------------------------------------------------------------------------------
    i = 2
    if(is_bc2) then
      fo(i) = coeff( 2, 1, ibc(1) ) * ( fi(i    ) - fi(i - 1) ) + &
              coeff( 2, 2, ibc(1) ) * ( fi(i + 1) - fc(0)     )
    else
      fo(i) = coeff( 2, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( 2, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( 2, 3, IBC_INTRPL) * fi(i + 1) + &
              coeff( 2, 4, IBC_INTRPL) * fi(i + 2) + &
              coeff( 2, 5, IBC_INTRPL) * fi(i + 3) + &
              coeff( 2, 6, IBC_INTRPL) * fi(i + 4)
    end if
!----------------------------------------------------------------------------------------------------------
    i = np
    if(is_bc5) then
      fo(i) = coeff( 5, 1, ibc(2) ) * ( fc(1) - fi(i - 1) ) + &
              coeff( 5, 2, ibc(2) ) * ( fc(2) - fi(i - 2) )
    else if(ibc(2) == IBC_PERIODIC) then
      fo(i) = coeff( 5, 1, IBC_PERIODIC ) * ( fi(i) - fi(i - 1) ) + &
              coeff( 5, 2, IBC_PERIODIC ) * ( fc(1) - fi(i - 2) )
    else if(ibc(2) == IBC_NEUMANN) then
      fo(i) = opt_fbc(2)/dd
    else
      fo(i) = coeff( 5, 1, IBC_INTRPL) * fi(i - 1) + &
              coeff( 5, 2, IBC_INTRPL) * fi(i - 2) + &
              coeff( 5, 3, IBC_INTRPL) * fi(i - 3) + &
              coeff( 5, 4, IBC_INTRPL) * fi(i - 4) + &
              coeff( 5, 5, IBC_INTRPL) * fi(i - 5) + &
              coeff( 5, 6, IBC_INTRPL) * fi(i - 6)
    end if
!----------------------------------------------------------------------------------------------------------
    i = np - 1
    if(is_bc4) then
      fo(i) = coeff( 4, 1, ibc(2) ) * ( fi(i) - fi(i - 1) ) + &
              coeff( 4, 2, ibc(2) ) * ( fc(1) - fi(i - 2) )
    else if(ibc(2) == IBC_PERIODIC) then
      fo(i) = coeff( 4, 1, IBC_PERIODIC ) * ( fi(i)   - fi(i - 1) ) + &
              coeff( 4, 2, IBC_PERIODIC ) * ( fi(i+1) - fi(i - 2) )
    else
      fo(i) = coeff( 4, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 4, 2, IBC_INTRPL) * fi(i - 1) + &
              coeff( 4, 3, IBC_INTRPL) * fi(i - 2) + &
              coeff( 4, 4, IBC_INTRPL) * fi(i - 3) + &
              coeff( 4, 5, IBC_INTRPL) * fi(i - 4) + &
              coeff( 4, 6, IBC_INTRPL) * fi(i - 5)
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd

    return
  end subroutine Prepare_TDMA_1deri_C2P_RHS_array

!==========================================================================================================
!> \brief Preparing the RHS array for the TDMA algorithm for 1st derivative - P2C.
!> This subroutine is called repeatly to update the RHS of the TDMA algorithm
!> \param[in]   nc      the number of unknowns, here is nc
!> \param[in]   ibc     the b.c. type at two ends of the unknown array
!> \param[in]   fbc     the b.c. values for the given ibc
!> \param[in]   coeff   the defined TDMA coefficients
!> \param[in]   dd      1/spacing, ie. 1/dx, 1/dy, 1/dz
!> \param[in]   fi      the input variable to build up the RHS array
!> \param[out]  fo      the output RHS array
!==========================================================================================================
  subroutine Prepare_TDMA_1deri_P2C_RHS_array(fi, fo, nc, coeff, dd, ibc, opt_fbc, opt_dp)
    use parameters_constant_mod
    implicit none
    real(WP), intent(in ) :: fi(:)
    integer,  intent(in ) :: nc ! unknow numbers, nc
    real(WP), intent(out) :: fo(nc)
    real(WP), intent(in ) :: coeff(1:NL, 1:2*NS, NBCS:NBCE)
    real(WP), intent(in ) :: dd
    real(WP), optional, intent(in ) :: opt_dp(4)
    integer,  intent(in ) :: ibc(2)
    real(WP), optional, intent(in ) :: opt_fbc(4)

    integer :: i!, l, m
    real(WP) :: fp(-1:2)
    logical :: is_bc1, is_bc5

    fo(:) = ZERO
!----------------------------------------------------------------------------------------------------------
!   i = bulk
!----------------------------------------------------------------------------------------------------------
    do i = 2, nc - 2
      fo(i) = coeff( 3, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( 3, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 1) )
    end do
!----------------------------------------------------------------------------------------------------------
!>                   BC                                BC
!>      _|__.__|__.__||__.__|__.__|__...___|__.__|__.__||__.__|__.__|__.__
!>      -1     0      1     2     3      np-2   np-1  np    np+1  np+2 (non-periodic)
!>      -1     0      1     2     3      np-1   np    np+1  np+2  np+3 (periodic)
!----------------------------------------------------------------------------------------------------------
    call buildup_ghost_cells_P(fi(:), ibc(:), fp(-1:2), opt_fbc(:), opt_dp(:))

    is_bc1 = (ibc(1) == IBC_INTERIOR   .or. &
              ibc(1) == IBC_PERIODIC   .or. &
              ibc(1) == IBC_SYMMETRIC  .or. &
              ibc(1) == IBC_ASYMMETRIC )
    if(bc_ghost_cd) then
      is_bc1 =(is_bc1 .or. &
               ibc(1) == IBC_DIRICHLET .or. &
               ibc(1) == IBC_NEUMANN)
    end if
    is_bc5 = (ibc(2) == IBC_INTERIOR   .or. &
              ibc(2) == IBC_SYMMETRIC  .or. &
              ibc(2) == IBC_ASYMMETRIC )
    if(bc_ghost_cd) then
      is_bc5 =(is_bc5 .or. &
               ibc(2) == IBC_DIRICHLET .or. &
               ibc(2) == IBC_NEUMANN)
    end if
!----------------------------------------------------------------------------------------------------------
    i = 1
    if(is_bc1) then
      fo(i) = coeff( 1, 1, ibc(1) ) * ( fi(i + 1) - fi(i) ) + &
              coeff( 1, 2, ibc(1) ) * ( fi(i + 2) - fp(0) )
    else
      fo(i) = coeff( 1, 1, IBC_INTRPL) * fi(i    ) + &
              coeff( 1, 2, IBC_INTRPL) * fi(i + 1) + &
              coeff( 1, 3, IBC_INTRPL) * fi(i + 2) + &
              coeff( 1, 4, IBC_INTRPL) * fi(i + 3) + &
              coeff( 1, 5, IBC_INTRPL) * fi(i + 4) + &
              coeff( 1, 6, IBC_INTRPL) * fi(i + 5)
    end if
!----------------------------------------------------------------------------------------------------------
    i = nc
    if(is_bc5) then
      fo(i) = coeff( 5, 1, ibc(2) ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( 5, 2, ibc(2) ) * ( fp(1)     - fi(i - 1) )
    else if(ibc(2) == IBC_PERIODIC) then
      fo(i) = coeff( 5, 1, IBC_PERIODIC ) * ( fp(1) - fi(i    ) ) + &
              coeff( 5, 2, IBC_PERIODIC ) * ( fp(2) - fi(i - 1) )
    else
      fo(i) = coeff( 5, 1, IBC_INTRPL) * fi(i + 1) + &
              coeff( 5, 2, IBC_INTRPL) * fi(i    ) + &
              coeff( 5, 3, IBC_INTRPL) * fi(i - 1) + &
              coeff( 5, 4, IBC_INTRPL) * fi(i - 2) + &
              coeff( 5, 5, IBC_INTRPL) * fi(i - 3) + &
              coeff( 5, 6, IBC_INTRPL) * fi(i - 4)
    end if
!----------------------------------------------------------------------------------------------------------
    i = nc - 1
    if(ibc(2) == IBC_PERIODIC) then
      fo(i) = coeff( 4, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( 4, 2, IBC_PERIODIC ) * ( fp(1)     - fi(i - 1) )
    else
      fo(i) = coeff( 4, 1, IBC_PERIODIC ) * ( fi(i + 1) - fi(i    ) ) + &
              coeff( 4, 2, IBC_PERIODIC ) * ( fi(i + 2) - fi(i - 1) )
    end if
!----------------------------------------------------------------------------------------------------------
!   mesh-based scaling
!----------------------------------------------------------------------------------------------------------
    fo(:) = fo(:) * dd

    return
  end subroutine Prepare_TDMA_1deri_P2C_RHS_array
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
  subroutine Get_x_midp_C2P_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic

    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_xmidp_c2p_interior(i), 'IBC_INTERIOR', 'Get_x_midp_C2P_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_xmidp_c2p_dirichlet(i), 'IBC_DIRICHLET', 'Get_x_midp_C2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_xmidp_c2p_neumann(i), 'IBC_NEUMANN', 'Get_x_midp_C2P_1D')
        end select
      end if
    end do
    
    ixsub = dm%idom
    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(1)
    dp(3) = dm%h(1) * TWO
    dp(2) = dm%h(1)
    dp(4) = dm%h(1) * TWO
    call Prepare_TDMA_interp_C2P_RHS_array(fi(:), fo(:), nsz, m1rC2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            xtdma_lhs(ixsub)%am1x_C2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%bm1x_C2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%cm1x_C2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%dm2x_C2P(:, ibc(1), ibc(2), iacc), &
            nsz)
      end if

#ifdef DEBUG_ALGO
    do i = 1, NL
      write(*,'(A, 3I3.1, 5F7.3)') 'm1fC2P, bc, acc, ln, lhs, rhs:', ibc(1), iacc, i, m1fC2P(i, 1:3, ibc(1), iacc), &
                               m1rC2P(i, 1:2, ibc(1), iacc)
    end do
#endif

    return
  end subroutine Get_x_midp_C2P_1D
!==========================================================================================================
  subroutine Get_x_midp_P2C_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_xmidp_p2c_interior(i), 'IBC_INTERIOR', 'Get_x_midp_P2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_xmidp_p2c_neumann(i), 'IBC_NEUMANN', 'Get_x_midp_P2C_1D')
        end select
      end if
    end do
    

    nsz = size(fo)
    fo = ZERO
    ixsub = dm%idom
    dp(1) = dm%h(1) * TWO
    dp(3) = dm%h(1) * FOUR
    dp(2) = dm%h(1) * TWO
    dp(4) = dm%h(1) * FOUR
    call Prepare_TDMA_interp_P2C_RHS_array(fi(:), fo(:), nsz, m1rP2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            xtdma_lhs(ixsub)%am1x_P2C(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%bm1x_P2C(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%cm1x_P2C(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%dm2x_P2C(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if

#ifdef DEBUG_ALGO
    do i = 1, NL
      write(*,'(A, 3I3.1, 5F7.3)') 'm1fP2C, bc, acc, ln, lhs, rhs:', ibc(1), iacc, i, m1fP2C(i, 1:3, ibc(1), iacc), &
                               m1rP2C(i, 1:2, ibc(1), iacc)
    end do
#endif 

    return
  end subroutine Get_x_midp_P2C_1D
!==========================================================================================================
  subroutine Get_y_midp_C2P_1D(fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    !----------------------------------------------------------------------
    ! check the boundary conditions
    !----------------------------------------------------------------------
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_ymidp_c2p_interior(i), 'IBC_INTERIOR', 'Get_y_midp_C2P_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_ymidp_c2p_dirichlet(i), 'IBC_DIRICHLET', 'Get_y_midp_C2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_ymidp_c2p_neumann(i), 'IBC_NEUMANN', 'Get_y_midp_C2P_1D')
        end select
      end if
    end do
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  f(y_i') = g(s_i') = interp of g(s_i) at s_i'
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------
    ! prepare the physical spacing for neumann boundary conditions
    !----------------------------------------------------------------------
    nsz = size(fo)
    fo = ZERO
    ! dp = physical spacings are used to build up ghost cells
    dp(1) = (dm%yc(1) - dm%yp(1)) * TWO
    dp(3) = (dm%yc(2) - dm%yp(1)) * TWO
    dp(2) = (dm%yp(dm%np(2)) - dm%yc(dm%nc(2)  )) * TWO
    dp(4) = (dm%yp(dm%np(2)) - dm%yc(dm%nc(2)-1)) * TWO
    !----------------------------------------------------------------------
    ! prepare the RHS array for the TDMA algorithm
    !----------------------------------------------------------------------
    call Prepare_TDMA_interp_C2P_RHS_array(fi(:), fo(:), nsz, m1rC2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            am1y_C2P(:, ibc(1), ibc(2), iacc), &
            bm1y_C2P(:, ibc(1), ibc(2), iacc), &
            cm1y_C2P(:, ibc(1), ibc(2), iacc), &
            dm1y_C2P(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if

    return
  end subroutine Get_y_midp_C2P_1D
!==========================================================================================================
  subroutine Get_y_midp_P2C_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    !----------------------------------------------------------------------
    ! check the boundary conditions
    !----------------------------------------------------------------------
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_ymidp_p2c_interior(i), 'IBC_INTERIOR', 'Get_y_midp_P2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_ymidp_p2c_neumann(i), 'IBC_NEUMANN', 'Get_y_midp_P2C_1D')
        end select
      end if
    end do
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  f(y_i') = g(s_i') = interp of g(s_i) at s_i'
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------
    ! prepare the physical spacing for neumann boundary conditions
    !----------------------------------------------------------------------
    nsz = size(fo)
    fo = ZERO
    ! dp = physical spacings are used to build up ghost grids
    dp(1) = ( dm%yp(2) - dm%yp(1) ) * TWO
    dp(3) = ( dm%yp(3) - dm%yp(1) ) * TWO
    dp(2) = ( dm%yp(dm%np(2)) - dm%yp(dm%np(2)-1) ) * TWO
    dp(4) = ( dm%yp(dm%np(2)) - dm%yp(dm%np(2)-2) ) * TWO
    !----------------------------------------------------------------------
    ! prepare the RHS array for the TDMA algorithm
    !----------------------------------------------------------------------
    call Prepare_TDMA_interp_P2C_RHS_array(fi(:), fo(:), nsz, m1rP2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), ibc(:), fbc(:), dp(:) )
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            am1y_P2C(:, ibc(1), ibc(2), iacc), &
            bm1y_P2C(:, ibc(1), ibc(2), iacc), &
            cm1y_P2C(:, ibc(1), ibc(2), iacc), &
            dm1y_P2C(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if

    return
  end subroutine Get_y_midp_P2C_1D
!==========================================================================================================
  subroutine Get_z_midp_C2P_1D(fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_zmidp_c2p_interior(i), 'IBC_INTERIOR', 'Get_z_midp_C2P_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_zmidp_c2p_dirichlet(i), 'IBC_DIRICHLET', 'Get_z_midp_C2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_zmidp_c2p_neumann(i), 'IBC_NEUMANN', 'Get_z_midp_C2P_1D')
        end select
      end if
    end do
    

    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(3)
    dp(3) = dm%h(3) * TWO
    dp(2) = dm%h(3)
    dp(4) = dm%h(3) * TWO
    call Prepare_TDMA_interp_C2P_RHS_array(fi(:), fo(:), nsz, m1rC2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            am1z_C2P(:, ibc(1), ibc(2), iacc), &
            bm1z_C2P(:, ibc(1), ibc(2), iacc), &
            cm1z_C2P(:, ibc(1), ibc(2), iacc), &
            dm2z_C2P(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if

    return
  end subroutine Get_z_midp_C2P_1D
!==========================================================================================================
  subroutine Get_z_midp_P2C_1D(fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_zmidp_p2c_interior(i), 'IBC_INTERIOR', 'Get_z_midp_P2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_zmidp_p2c_neumann(i), 'IBC_NEUMANN', 'Get_z_midp_P2C_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(3) * TWO
    dp(3) = dm%h(3) * FOUR
    dp(2) = dm%h(3) * TWO
    dp(4) = dm%h(3) * FOUR
    call Prepare_TDMA_interp_P2C_RHS_array(fi(:), fo(:), nsz, m1rP2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            am1z_P2C(:, ibc(1), ibc(2), iacc), &
            bm1z_P2C(:, ibc(1), ibc(2), iacc), &
            cm1z_P2C(:, ibc(1), ibc(2), iacc), &
            dm2z_P2C(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if

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
  subroutine Get_x_1der_C2C_1D(fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_c2c_interior(i), 'IBC_INTERIOR', 'Get_x_1der_C2C_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_c2c_dirichlet(i), 'IBC_DIRICHLET', 'Get_x_1der_C2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_c2c_neumann(i), 'IBC_NEUMANN', 'Get_x_1der_C2C_1D')
        end select
      end if
    end do
    
    ixsub = dm%idom
    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(1)
    dp(3) = dm%h(1) * TWO
    dp(2) = dm%h(1)
    dp(4) = dm%h(1) * TWO
    call Prepare_TDMA_1deri_C2C_RHS_array(fi(:), fo(:), nsz, d1rC2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(1), ibc(:), fbc(:), dp(:))

    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if

      call Solve_TDMA(is_periodic, fo(:), &
            xtdma_lhs(ixsub)%ad1x_C2C(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%bd1x_C2C(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%cd1x_C2C(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%dd1x_C2C(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if

#ifdef DEBUG_ALGO
    do i = 1, NL
      write(*,'(A, 3I3.1, 5F7.3)') 'd1fC2C, bc, acc, ln, lhs, rhs:', ibc(1), iacc, i, d1fC2C(i, 1:3, ibc(1), iacc), &
                               d1rC2C(i, 1:2, ibc(1), iacc)
    end do
#endif 

    return
  end subroutine Get_x_1der_C2C_1D
!==========================================================================================================
  subroutine Get_x_1der_P2P_1D(fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_p2p_interior(i), 'IBC_INTERIOR', 'Get_x_1der_P2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_p2p_neumann(i), 'IBC_NEUMANN', 'Get_x_1der_P2P_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    ixsub = dm%idom
    dp(1) = dm%h(1) * TWO
    dp(3) = dm%h(1) * FOUR
    dp(2) = dm%h(1) * TWO
    dp(4) = dm%h(1) * FOUR
    call Prepare_TDMA_1deri_P2P_RHS_array(fi(:), fo(:), nsz, d1rP2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(1), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            xtdma_lhs(ixsub)%ad1x_P2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%bd1x_P2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%cd1x_P2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%dd1x_P2P(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if


#ifdef DEBUG_ALGO
    do i = 1, NL
      write(*,'(A, 3I3.1, 5F7.3)') 'd1fP2P, bc, acc, ln, lhs, rhs:', ibc(1), iacc, i, d1fP2P(i, 1:3, ibc(1), iacc), &
                               d1rP2P(i, 1:2, ibc(1), iacc)
    end do
#endif 

    return
  end subroutine Get_x_1der_P2P_1D
!==========================================================================================================
  subroutine Get_x_1der_C2P_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_c2p_interior(i), 'IBC_INTERIOR', 'Get_x_1der_C2P_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_c2p_dirichlet(i), 'IBC_DIRICHLET', 'Get_x_1der_C2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_c2p_neumann(i), 'IBC_NEUMANN', 'Get_x_1der_C2P_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    ixsub = dm%idom
    dp(1) = dm%h(1)
    dp(3) = dm%h(1) * TWO
    dp(2) = dm%h(1)
    dp(4) = dm%h(1) * TWO
    call Prepare_TDMA_1deri_C2P_RHS_array(fi(:), fo(:), nsz, d1rC2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(1), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            xtdma_lhs(ixsub)%ad1x_C2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%bd1x_C2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%cd1x_C2P(:, ibc(1), ibc(2), iacc), &
            xtdma_lhs(ixsub)%dd1x_C2P(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if


#ifdef DEBUG_ALGO
    do i = 1, NL
      write(*,'(A, 3I3.1, 5F7.3)') 'd1fC2P, bc, acc, ln, lhs, rhs:', ibc(1), iacc, i, d1fC2P(i, 1:3, ibc(1), iacc), &
                               d1rC2P(i, 1:2, ibc(1), iacc)
    end do
#endif

    return
  end subroutine Get_x_1der_C2P_1D
!==========================================================================================================
  subroutine Get_x_1der_P2C_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: ixsub, nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_p2c_interior(i), 'IBC_INTERIOR', 'Get_x_1der_P2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_x1der_p2c_neumann(i), 'IBC_NEUMANN', 'Get_x_1der_P2C_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO

    ixsub = dm%idom
    dp(1) = dm%h(1) * TWO
    dp(3) = dm%h(1) * FOUR
    dp(2) = dm%h(1) * TWO
    dp(4) = dm%h(1) * FOUR
    call Prepare_TDMA_1deri_P2C_RHS_array(fi(:), fo(:), nsz, d1rP2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(1), ibc(:), fbc(:), dp(:) )
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          xtdma_lhs(ixsub)%ad1x_P2C(:, ibc(1), ibc(2), iacc), &
          xtdma_lhs(ixsub)%bd1x_P2C(:, ibc(1), ibc(2), iacc), &
          xtdma_lhs(ixsub)%cd1x_P2C(:, ibc(1), ibc(2), iacc), &
          xtdma_lhs(ixsub)%dd1x_P2C(:, ibc(1), ibc(2), iacc), &
          nsz)
    end if

#ifdef DEBUG_ALGO
    do i = 1, NL
      write(*,'(A, 3I3.1, 5F7.3)') 'd1fP2C, bc, acc, ln, lhs, rhs:', ibc(1), iacc, i, d1fP2C(i, 1:3, ibc(1), iacc), &
                               d1rP2C(i, 1:2, ibc(1), iacc)
    end do
#endif 

    return
  end subroutine Get_x_1der_P2C_1D
!==========================================================================================================
! y - Get_1der_1D
!==========================================================================================================
  subroutine Get_y_1der_C2C_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    !----------------------------------------------------------------------
    ! check the boundary conditions
    !----------------------------------------------------------------------
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_c2c_interior(i), 'IBC_INTERIOR', 'Get_y_1der_C2C_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_c2c_dirichlet(i), 'IBC_DIRICHLET', 'Get_y_1der_C2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_c2c_neumann(i), 'IBC_NEUMANN', 'Get_y_1der_C2C_1D')
        end select
      end if
    end do
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------
    ! prepare the physical spacing for neumann boundary conditions
    !----------------------------------------------------------------------
    nsz = size(fo)
    fo = ZERO
    ! dp = physical spacings are used to build up ghost cells
    dp(1) = (dm%yc(1) - dm%yp(1)) * TWO
    dp(3) = (dm%yc(2) - dm%yp(1)) * TWO
    dp(2) = (dm%yp(dm%np(2)) - dm%yc(dm%nc(2)  )) * TWO
    dp(4) = (dm%yp(dm%np(2)) - dm%yc(dm%nc(2)-1)) * TWO
    !----------------------------------------------------------------------
    ! prepare the RHS array for the TDMA algorithm
    !----------------------------------------------------------------------
    call Prepare_TDMA_1deri_C2C_RHS_array(fi(:), fo(:), nsz, d1rC2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(2), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          ad1y_C2C(:, ibc(1), ibc(2), iacc), &
          bd1y_C2C(:, ibc(1), ibc(2), iacc), &
          cd1y_C2C(:, ibc(1), ibc(2), iacc), &
          dd1y_C2C(:, ibc(1), ibc(2), iacc), &
          nsz)
    end if
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingcc(:, 1)

    return
  end subroutine Get_y_1der_C2C_1D
!==========================================================================================================
  subroutine Get_y_1der_P2P_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    !----------------------------------------------------------------------
    ! check the boundary conditions
    !----------------------------------------------------------------------
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_p2p_interior(i), 'IBC_INTERIOR', 'Get_y_1der_P2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_p2p_neumann(i), 'IBC_NEUMANN', 'Get_y_1der_P2P_1D')
        end select
      end if
    end do
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------
    ! prepare the physical spacing for neumann boundary conditions
    !----------------------------------------------------------------------
    nsz = size(fo)
    fo = ZERO
    ! dp = physical spacings are used to build up ghost grids
    dp(1) = ( dm%yp(2) - dm%yp(1) ) * TWO
    dp(3) = ( dm%yp(3) - dm%yp(1) ) * TWO
    dp(2) = ( dm%yp(dm%np(2)) - dm%yp(dm%np(2)-1) ) * TWO
    dp(4) = ( dm%yp(dm%np(2)) - dm%yp(dm%np(2)-2) ) * TWO
    !----------------------------------------------------------------------
    ! prepare the RHS array for the TDMA algorithm
    !----------------------------------------------------------------------
    call Prepare_TDMA_1deri_P2P_RHS_array(fi(:), fo(:), nsz, d1rP2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(2), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          ad1y_P2P(:, ibc(1), ibc(2), iacc), &
          bd1y_P2P(:, ibc(1), ibc(2), iacc), &
          cd1y_P2P(:, ibc(1), ibc(2), iacc), &
          dd1y_P2P(:, ibc(1), ibc(2), iacc), &
          nsz)
    end if
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingpt(:, 1)
    if(ibc(1) == IBC_NEUMANN .and. present(fbc)) fo(1)   = fbc(1)
    if(ibc(2) == IBC_NEUMANN .and. present(fbc)) fo(nsz) = fbc(2)   

    return
  end subroutine Get_y_1der_P2P_1D
!==========================================================================================================
  subroutine Get_y_1der_C2P_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    !----------------------------------------------------------------------
    ! check the boundary conditions
    !----------------------------------------------------------------------
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_c2p_interior(i), 'IBC_INTERIOR', 'Get_y_1der_C2P_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_c2p_dirichlet(i), 'IBC_DIRICHLET', 'Get_y_1der_C2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_c2p_neumann(i), 'IBC_NEUMANN', 'Get_y_1der_C2P_1D')
        end select
      end if
    end do
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------
    ! prepare the physical spacing for neumann boundary conditions
    !----------------------------------------------------------------------
    nsz = size(fo)
    fo = ZERO
    ! dp = physical spacings are used to build up ghost cells
    dp(1) = (dm%yc(1) - dm%yp(1)) * TWO
    dp(3) = (dm%yc(2) - dm%yp(1)) * TWO
    dp(2) = (dm%yp(dm%np(2)) - dm%yc(dm%nc(2)  )) * TWO
    dp(4) = (dm%yp(dm%np(2)) - dm%yc(dm%nc(2)-1)) * TWO
    !----------------------------------------------------------------------
    ! prepare the RHS array for the TDMA algorithm
    !----------------------------------------------------------------------
    call Prepare_TDMA_1deri_C2P_RHS_array(fi(:), fo(:), nsz, d1rC2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(2), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
            ad1y_C2P(:, ibc(1), ibc(2), iacc), &
            bd1y_C2P(:, ibc(1), ibc(2), iacc), &
            cd1y_C2P(:, ibc(1), ibc(2), iacc), &
            dd1y_C2P(:, ibc(1), ibc(2), iacc), &
            nsz)
    end if
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingpt(:, 1)
    if(ibc(1) == IBC_NEUMANN .and. present(fbc)) fo(1)   = fbc(1)
    if(ibc(2) == IBC_NEUMANN .and. present(fbc)) fo(nsz) = fbc(2)   
    return
  end subroutine Get_y_1der_C2P_1D
!==========================================================================================================
  subroutine Get_y_1der_P2C_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    !----------------------------------------------------------------------
    ! check the boundary conditions
    !----------------------------------------------------------------------
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_p2c_interior(i), 'IBC_INTERIOR', 'Get_y_1der_P2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_y1der_p2c_neumann(i), 'IBC_NEUMANN', 'Get_y_1der_P2C_1D')
        end select
      end if
    end do
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    !----------------------------------------------------------------------
    ! prepare the physical spacing for neumann boundary conditions
    !----------------------------------------------------------------------
    nsz = size(fo)
    fo = ZERO
    ! dp = physical spacings are used to build up ghost grids
    dp(1) = ( dm%yp(2) - dm%yp(1) ) * TWO
    dp(3) = ( dm%yp(3) - dm%yp(1) ) * TWO
    dp(2) = ( dm%yp(dm%np(2)) - dm%yp(dm%np(2)-1) ) * TWO
    dp(4) = ( dm%yp(dm%np(2)) - dm%yp(dm%np(2)-2) ) * TWO
    !----------------------------------------------------------------------
    ! prepare the RHS array for the TDMA algorithm
    !----------------------------------------------------------------------
    call Prepare_TDMA_1deri_P2C_RHS_array(fi(:), fo(:), nsz, d1rP2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(2), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          ad1y_P2C(:, ibc(1), ibc(2), iacc), &
          bd1y_P2C(:, ibc(1), ibc(2), iacc), &
          cd1y_P2C(:, ibc(1), ibc(2), iacc), &
          dd1y_P2C(:, ibc(1), ibc(2), iacc), &
          nsz)
    end if
    !----------------------------------------------------------------------
    ! mapping function: y_i = h(s_i)
    ! known: f(y_i) = f(h(s_i)) = g(s_i)
    ! then:  d(f(y_i))/dy = d(f(h(s_i)))/ds * ds/dy = d(g(s_i))/ds * (ds/dy)
    !----------------------------------------------------------------------
    if(dm%is_stretching(2)) fo(:) = fo(:) * dm%yMappingcc(:, 1)

    return
  end subroutine Get_y_1der_P2C_1D
!==========================================================================================================
! z - Get_1der_1D
!==========================================================================================================
  subroutine Get_z_1der_C2C_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_c2c_interior(i), 'IBC_INTERIOR', 'Get_z_1der_C2C_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_c2c_dirichlet(i), 'IBC_DIRICHLET', 'Get_z_1der_C2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_c2c_neumann(i), 'IBC_NEUMANN', 'Get_z_1der_C2C_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(3)
    dp(3) = dm%h(3) * TWO
    dp(2) = dm%h(3)
    dp(4) = dm%h(3) * TWO
    call Prepare_TDMA_1deri_C2C_RHS_array(fi(:), fo(:), nsz, d1rC2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(3), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          ad1z_C2C(:, ibc(1), ibc(2), iacc), &
          bd1z_C2C(:, ibc(1), ibc(2), iacc), &
          cd1z_C2C(:, ibc(1), ibc(2), iacc), &
          dd1z_C2C(:, ibc(1), ibc(2), iacc), &
          nsz)
    end if

    return
  end subroutine Get_z_1der_C2C_1D
!==========================================================================================================
  subroutine Get_z_1der_P2P_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_p2p_interior(i), 'IBC_INTERIOR', 'Get_z_1der_P2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_p2p_neumann(i), 'IBC_NEUMANN', 'Get_z_1der_P2P_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(3) * TWO
    dp(3) = dm%h(3) * FOUR
    dp(2) = dm%h(3) * TWO
    dp(4) = dm%h(3) * FOUR
    call Prepare_TDMA_1deri_P2P_RHS_array(fi(:), fo(:), nsz, d1rP2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(3), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          ad1z_P2P(:, ibc(1), ibc(2), iacc), &
          bd1z_P2P(:, ibc(1), ibc(2), iacc), &
          cd1z_P2P(:, ibc(1), ibc(2), iacc), &
          dd1z_P2P(:, ibc(1), ibc(2), iacc), &
          nsz)
      end if

    return
  end subroutine Get_z_1der_P2P_1D
!==========================================================================================================
  subroutine Get_z_1der_C2P_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_c2p_interior(i), 'IBC_INTERIOR', 'Get_z_1der_C2P_1D')
          case (IBC_DIRICHLET)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_c2p_dirichlet(i), 'IBC_DIRICHLET', 'Get_z_1der_C2P_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_c2p_neumann(i), 'IBC_NEUMANN', 'Get_z_1der_C2P_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(3)
    dp(3) = dm%h(3) * TWO
    dp(2) = dm%h(3)
    dp(4) = dm%h(3) * TWO
    call Prepare_TDMA_1deri_C2P_RHS_array(fi(:), fo(:), nsz, d1rC2P(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(3), ibc(:), fbc(:), dp(:) )
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          ad1z_C2P(:, ibc(1), ibc(2), iacc), &
          bd1z_C2P(:, ibc(1), ibc(2), iacc), &
          cd1z_C2P(:, ibc(1), ibc(2), iacc), &
          dd1z_C2P(:, ibc(1), ibc(2), iacc), &
          nsz)
    end if

    return
  end subroutine Get_z_1der_C2P_1D
!==========================================================================================================
  subroutine Get_z_1der_P2C_1D (fi, fo, dm, iacc, ibc0, fbc)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    
    real(WP),           intent(in ) :: fi(:)
    real(WP),           intent(out) :: fo(:)
    type(t_domain),     intent(in ) :: dm
    integer,            intent(in ) :: iacc
    integer,            intent(in ) :: ibc0(2)
    real(WP), optional, intent(in ) :: fbc(4)

    integer :: nsz
    integer :: i
    integer :: ibc(2)
    real(WP) :: dp(4)
    logical :: is_periodic
    
    ibc = ibc0
    do i = 1, 2
      if (.not. present(fbc)) then
        select case (ibc(i))
          case (IBC_INTERIOR)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_p2c_interior(i), 'IBC_INTERIOR', 'Get_z_1der_P2C_1D')
          case (IBC_NEUMANN)
            call reduce_bc_to_interp(ibc(i), flg_wrn_z1der_p2c_neumann(i), 'IBC_NEUMANN', 'Get_z_1der_P2C_1D')
        end select
      end if
    end do

    nsz = size(fo)
    fo = ZERO
    dp(1) = dm%h(3) * TWO
    dp(3) = dm%h(3) * FOUR
    dp(2) = dm%h(3) * TWO
    dp(4) = dm%h(3) * FOUR
    call Prepare_TDMA_1deri_P2C_RHS_array(fi(:), fo(:), nsz, d1rP2C(1:NL, 1:2*NS, NBCS:NBCE, iacc), dm%h1r(3), ibc(:), fbc(:), dp(:))
    if (iacc == IACCU_CP4 .or. iacc == IACCU_CP6) then 
      if(ibc(1) == IBC_PERIODIC) then
        is_periodic = .true.
      else
        is_periodic = .false.
      end if
      call Solve_TDMA(is_periodic, fo(:), &
          ad1z_P2C(:, ibc(1), ibc(2), iacc), &
          bd1z_P2C(:, ibc(1), ibc(2), iacc), &
          cd1z_P2C(:, ibc(1), ibc(2), iacc), &
          dd1z_P2C(:, ibc(1), ibc(2), iacc), &
          nsz)
    end if

    return
  end subroutine Get_z_1der_P2C_1D
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
  subroutine Get_x_midp_C2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use tridiagonal_matrix_algorithm
    use udf_type_mod
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j

!----------------------------------------------------------------------------------------------------------
!  default : x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_x_midp_C2P_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_x_midp_C2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_x_midp_C2P_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_x_midp_C2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(1:4, j, k)
          call Get_x_midp_C2P_1D (fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_x_midp_C2P_1D (fi, fo, dm, iacc, ibc)
        end if
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_midp_C2P_3D
!==========================================================================================================
  subroutine Get_x_midp_P2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :) !2 layer each side
    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    integer :: k, j
    real(WP)   :: fbc(4)
    
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(1:4, j, k)
          call Get_x_midp_P2C_1D (fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_x_midp_P2C_1D (fi, fo, dm, iacc, ibc)
        end if
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_midp_P2C_3D
!==========================================================================================================
  subroutine Get_y_midp_C2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i

!----------------------------------------------------------------------------------------------------------
!  default : y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_y_midp_C2P_3D")
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_y_midp_C2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_y_midp_C2P_3D")
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_y_midp_C2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, 1:4, k)
          call Get_y_midp_C2P_1D (fi, fo, dm, iacc, ibc, fbc)
        else 
          call Get_y_midp_C2P_1D (fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_midp_C2P_3D
!==========================================================================================================
  subroutine Get_y_midp_P2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    integer :: k, i
    real(WP) :: fbc(4)
!----------------------------------------------------------------------------------------------------------
!  default : y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_y_midp_P2C_3D")
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_y_midp_P2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_y_midp_C2P_3D")
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_y_midp_C2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) then 
          fbc(1:4) = fbc2d(i, 1:4, k)
          call Get_y_midp_P2C_1D (fi, fo, dm, iacc, ibc, fbc)
        else 
          call Get_y_midp_P2C_1D (fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_midp_P2C_3D
  !==========================================================================================================
  subroutine Get_z_midp_C2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i

!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_z_midp_C2P_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_z_midp_C2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_z_midp_C2P_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_z_midp_C2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, j, 1:4)
          call Get_z_midp_C2P_1D (fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_z_midp_C2P_1D (fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_midp_C2P_3D
!==========================================================================================================
  subroutine Get_z_midp_P2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    integer :: j, i
    real(WP)   :: fbc(4)
!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_z_midp_P2C_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_z_midp_P2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_z_midp_P2C_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_z_midp_P2C_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, j, 1:4)
          call Get_z_midp_P2C_1D (fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_z_midp_P2C_1D (fi, fo, dm, iacc, ibc)
        end if
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
  subroutine Get_x_1der_C2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j
!----------------------------------------------------------------------------------------------------------
!  default : x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_x_1der_C2C_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_x_1der_C2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_x_1der_C2C_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_x_1der_C2C_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(1:4, j, k)
          call Get_x_1der_C2C_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_x_1der_C2C_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1der_C2C_3D
!==========================================================================================================
  subroutine Get_x_1der_P2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j

!----------------------------------------------------------------------------------------------------------
!  default : x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_x_1der_P2P_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_x_1der_P2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_x_1der_P2P_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_x_1der_P2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) then 
          fbc(1:4) = fbc2d(1:4, j, k)
          call Get_x_1der_P2P_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_x_1der_P2P_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1der_P2P_3D
!==========================================================================================================
  subroutine Get_x_1der_C2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j
!----------------------------------------------------------------------------------------------------------
!  default : x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_x_1der_C2P_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_x_1der_C2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_x_1der_C2P_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_x_1der_C2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) then 
          fbc(1:4) = fbc2d(1:4, j, k)
          call Get_x_1der_C2P_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_x_1der_C2P_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1der_C2P_3D
!==========================================================================================================
  subroutine Get_x_1der_P2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 1) )
    real(WP)   :: fo( size(fo3d, 1) )
    real(WP)   :: fbc(4)
    integer :: k, j
!----------------------------------------------------------------------------------------------------------
!  default : x-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_x_1der_P2C_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_x_1der_P2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_x_1der_P2C_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_x_1der_P2C_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do j = 1, size(fi3d, 2)
        fi(:) = fi3d(:, j, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(1:4, j, k)
          call Get_x_1der_P2C_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_x_1der_P2C_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(:, j, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_x_1der_P2C_3D
!==========================================================================================================
  subroutine Get_y_1der_C2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i

!----------------------------------------------------------------------------------------------------------
!  default : y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_y_1der_C2C_3D")
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_y_1der_C2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_y_1der_C2C_3D")
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_y_1der_C2C_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, 1:4, k)
          call Get_y_1der_C2C_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_y_1der_C2C_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1der_C2C_3D
!==========================================================================================================
  subroutine Get_y_1der_P2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i

!----------------------------------------------------------------------------------------------------------
!  default : y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_y_1der_P2P_3D")
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_y_1der_P2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_y_1der_P2P_3D")
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_y_1der_P2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) then 
          fbc(1:4) = fbc2d(i, 1:4, k)
          call Get_y_1der_P2P_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_y_1der_P2P_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1der_P2P_3D
!==========================================================================================================
  subroutine Get_y_1der_C2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i
!----------------------------------------------------------------------------------------------------------
!  default : y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_y_1der_C2P_3D")
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_y_1der_C2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_y_1der_C2P_3D")
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_y_1der_C2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, 1:4, k)
          call Get_y_1der_C2P_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_y_1der_C2P_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1der_C2P_3D

!==========================================================================================================
  subroutine Get_y_1der_P2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 2) )
    real(WP)   :: fo( size(fo3d, 2) )
    real(WP)   :: fbc(4)
    integer :: k, i
!----------------------------------------------------------------------------------------------------------
!  default : y-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_y_1der_P2C_3D")
    call check_size("fo/fi", 3, size(fo3d,3), size(fi3d,3), "nz mismatch in Get_y_1der_P2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_y_1der_P2C_3D")
      call check_size("fbc/fi", 3, size(fbc2d,3), size(fi3d,3), "nz mismatch in Get_y_1der_P2C_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do k = 1, size(fi3d, 3)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, :, k)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, 1:4, k)
          call Get_y_1der_P2C_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_y_1der_P2C_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, :, k) = fo(:)
      end do
    end do

    return 
  end subroutine Get_y_1der_P2C_3D
!==========================================================================================================
  subroutine Get_z_1der_C2C_3D (fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i
!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_z_1der_C2C_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_z_1der_C2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_z_1der_C2C_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_z_1der_C2C_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) then 
          fbc(1:4) = fbc2d(i, j, 1:4)
          call Get_z_1der_C2C_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_z_1der_C2C_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1der_C2C_3D

!==========================================================================================================
  subroutine Get_z_1der_P2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i

!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_z_1der_P2P_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_z_1der_P2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_z_1der_P2P_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_z_1der_P2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, j, 1:4)
          call Get_z_1der_P2P_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_z_1der_P2P_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1der_P2P_3D
!==========================================================================================================
  subroutine Get_z_1der_C2P_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)
    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i

!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_z_1der_C2P_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_z_1der_C2P_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_z_1der_C2P_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_z_1der_C2P_3D")
    end if
!----------------------------------------------------------------------------------------------------------

    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, j, 1:4)
          call Get_z_1der_C2P_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_z_1der_C2P_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1der_C2P_3D
  !==========================================================================================================
  subroutine Get_z_1der_P2C_3D(fi3d, fo3d, dm, iacc, ibc, fbc2d)
    use parameters_constant_mod
    use udf_type_mod
    use tridiagonal_matrix_algorithm
    implicit none
    real(WP),           intent(in) :: fi3d(:, :, :)
    real(WP),           intent(out):: fo3d(:, :, :)
    type(t_domain),     intent(in) :: dm
    integer,            intent(in) :: iacc
    integer,            intent(in) :: ibc(2)
    real(WP), optional, intent(in) :: fbc2d(:, :, :)

    real(WP)   :: fi( size(fi3d, 3) )
    real(WP)   :: fo( size(fo3d, 3) )
    real(WP)   :: fbc(4)
    integer :: j, i
!----------------------------------------------------------------------------------------------------------
!  default : z-pencil calculation
!----------------------------------------------------------------------------------------------------------
    ! Check sizes for fo3d and fi3d
    call check_size("fo/fi", 1, size(fo3d,1), size(fi3d,1), "nx mismatch in Get_z_1der_P2C_3D")
    call check_size("fo/fi", 2, size(fo3d,2), size(fi3d,2), "ny mismatch in Get_z_1der_P2C_3D")

    ! Check sizes for fbc2d if present
    if (present(fbc2d)) then
      call check_size("fbc/fi", 1, size(fbc2d,1), size(fi3d,1), "nx mismatch in Get_z_1der_P2C_3D")
      call check_size("fbc/fi", 2, size(fbc2d,2), size(fi3d,2), "ny mismatch in Get_z_1der_P2C_3D")
    end if
!----------------------------------------------------------------------------------------------------------
    fo3d(:, :, :) = ZERO
    do j = 1, size(fi3d, 2)
      do i = 1, size(fi3d, 1)
        fi(:) = fi3d(i, j, :)
        if(present(fbc2d)) then
          fbc(1:4) = fbc2d(i, j, 1:4)
          call Get_z_1der_P2C_1D(fi, fo, dm, iacc, ibc, fbc)
        else
          call Get_z_1der_P2C_1D(fi, fo, dm, iacc, ibc)
        end if
        fo3d(i, j, :) = fo(:)
      end do
    end do

    return 
  end subroutine Get_z_1der_P2C_3D

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
  subroutine test_function_setup(ibc, dd, fbc, scale, shift)
    use parameters_constant_mod
    use math_mod
    implicit none
    integer, intent(in) :: ibc(2)
    real(WP), intent(in) :: dd
    real(WP), intent(out) :: fbc(4)
    real(WP), intent(out) :: scale
    real(WP), intent(out) :: shift

    integer :: i
    fbc = MAXP
    do i = 1, 2
      if (ibc(i) == IBC_PERIODIC) then
        ! f = sin(x/scale + shift) : f = sin(x)
        scale = ONE
        shift = ZERO
      else if (ibc(i) == IBC_SYMMETRIC) then
      ! f = sin(x/scale + shift) : f = sin(x + pi/2)
        scale = ONE
        shift = PI * HALF
      else if (ibc(i) == IBC_ASYMMETRIC) then
      ! f = sin(x/scale + shift) : f = sin(x/2)
        scale = TWO
        shift = ZERO
      else if (ibc(i) == IBC_DIRICHLET) then
      ! f = sin(x/scale + shift) : f = sin(x/2)
        scale = TWO
        shift = ZERO
        if(i==1) fbc(1) = ZERO
        if(i==2) fbc(2) = sin_wp(TWOPI / TWO)
      else if (ibc(i) == IBC_NEUMANN) then
      ! f = sin(x/scale + shift) : f = sin(x/2) : f'=1/2cos(x/2)
        scale = TWO
        shift = ZERO
        if(i==1) fbc(1) = HALF * cos_wp(ZERO )
        if(i==2) fbc(2) = HALF * cos_wp(TWOPI * HALF)
      else 
        scale = TWO
        shift = ZERO
        if(i==1) fbc(1) = ZERO
        if(i==2) fbc(2) = sin_wp(TWOPI * HALF)
      end if
    end do
    return
  end subroutine 

  subroutine test_interp_c2p_comparison(nc, np, dd, scale, shift, iacc, ibc, fbc, dm, str)
    use parameters_constant_mod
    use math_mod
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: nc
    integer, intent(in) :: np
    real(WP), intent(in) :: dd
    real(WP), intent(in) :: scale
    real(WP), intent(in) :: shift
    integer, intent(in) :: iacc
    integer, intent(in) :: ibc(2)
    real(WP), intent(in) :: fbc(4)
    character(1), intent(in) :: str
    type(t_domain), intent(in) :: dm

    integer :: i
    real(WP) :: xc, xp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(2)
    real(WP) :: fgxp(np), fxc(nc)

    open (newunit = wrt_unit(1), file = 'test_interp_'//trim(int2str(nc))//'.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_interp_'//trim(str)//'_'//trim(int2str(nc))//'.dat', position="append")

  ! x direction, xc
    do i = 1, nc
      xc = dd * (real(i - 1, WP) + HALF)
      fxc(i) = sin_wp ( xc / scale + shift)
    end do
  ! x: c2p
    if(trim(str)=='x') then
      call Get_x_midp_C2P_1D (fxc, fgxp, dm, iacc, ibc, fbc)
    else if(trim(str)=='y') then
      call Get_y_midp_C2P_1D (fxc, fgxp, dm, iacc, ibc, fbc)
    else if(trim(str)=='z') then
      call Get_z_midp_C2P_1D (fxc, fgxp, dm, iacc, ibc, fbc)
    else
    end if
  ! x: error 
    err_Linf = ZERO
    err_L2   = ZERO
    write(wrt_unit(2), *) '# interp-c2p-'//trim(str), ', iacc=', iacc, ', np=', np
    write(wrt_unit(2), *) '# i, xp, ref, cal, err'
    do i = 1, np
      xp = dd * real(i - 1, WP)
      ref = sin_wp(xp / scale + shift)
      err = abs_wp(fgxp(i) - ref)
      write(wrt_unit(2), *) i, xp, ref, fgxp(i), err !test
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
    end do
    err_L2 = sqrt_wp(err_L2 / np) 
    write(wrt_unit(1), '(A, 2I2, 1I5, 2ES15.7)') &
      '# interp-c2p-'//trim(str)//', iacc, ibc, np, eInf, eL2: ', iacc, ibc(1), np, err_Linf, err_L2
    
    close(wrt_unit(1))
    close(wrt_unit(2))
    return
  end subroutine

  subroutine test_interp_p2c_comparison(nc, np, dd, scale, shift, iacc,  ibc, fbc, dm, str)
    use parameters_constant_mod
    use math_mod
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: nc
    integer, intent(in) :: np
    real(WP), intent(in) :: dd
    real(WP), intent(in) :: scale
    real(WP), intent(in) :: shift
    integer, intent(in) :: iacc
    integer, intent(in) :: ibc(2)
    real(WP), intent(in) :: fbc(4)
    character(1), intent(in) :: str
    type(t_domain), intent(in) :: dm

    integer :: i
    real(WP) :: xc, xp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(2)
    real(WP) :: fgxc(nc)
    real(WP) :: fxp(np)

    open (newunit = wrt_unit(1), file = 'test_interp_'//trim(int2str(nc))//'.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_interp_'//trim(str)//'_'//trim(int2str(nc))//'.dat', position="append")


    ! x direction, xp
    do i = 1, np
      xp = dd * real(i - 1, WP)
      fxp(i) = sin_wp ( xp / scale + shift)
    end do
! x: p2c
    if(trim(str)=='x') then
      call Get_x_midp_P2C_1D (fxp, fgxc, dm, iacc, ibc, fbc)
    else if(trim(str)=='y') then
      call Get_y_midp_P2C_1D (fxp, fgxc, dm, iacc, ibc, fbc)
    else if(trim(str)=='z') then
      call Get_z_midp_P2C_1D (fxp, fgxc, dm, iacc, ibc, fbc)
    else
    end if

    err_Linf = ZERO
    err_L2   = ZERO
    write(wrt_unit(2), *) '# interp-p2c-'//trim(str), ', iacc=', iacc, ', nc=', nc
    write(wrt_unit(2), *) '# i, xc, ref, cal, err'
    do i = 1, nc
      xc = dd * (real(i - 1, WP) + HALF)
      ref = sin_wp(xc / scale + shift)
      err = abs_wp(fgxc(i) - ref)
      write(wrt_unit(2),*) i, xc, ref, fgxc(i), err !test
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
    end do
    err_L2 = sqrt_wp(err_L2 / nc) 
    write(wrt_unit(1), '(A, 2I2, 1I5, 2ES15.7)') &
      '# interp-p2c-'//trim(str)//', iacc, ibc, nc, eInf, eL2: ', iacc, ibc(1), nc, err_Linf, err_L2
    close(wrt_unit(1))
    close(wrt_unit(2))
    return
  end subroutine

  subroutine Test_interpolation(dm)
    use parameters_constant_mod
    use math_mod
    use EvenOdd_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm

    real(WP) :: scale, shift
    real(WP) :: fbcx(4), fbcy(4), fbcz(4), fbc(4)
    integer  :: ibcx(2), ibcy(2), ibcz(2), ibc(2)
    integer :: iacc, n, i
    character(1) :: str

    dm%h(1) = TWOPI / dm%nc(1)
    dm%h(2) = TWOPI / dm%nc(2)
    dm%h(3) = TWOPI / dm%nc(3)
    dm%h1r(1) = ONE / dm%h(1)
    dm%h1r(2) = ONE / dm%h(2)
    dm%h1r(3) = ONE / dm%h(3)
    ibcx(:) = dm%ibcx_nominal(:, 5)
    ibcy(:) = dm%ibcy_nominal(:, 5)
    ibcz(:) = dm%ibcz_nominal(:, 5)


    do i = 1, 3
      if (i == 1) then
        ibc(:) = ibcx(:)
        str = 'x'
      else if (i == 2) then
        ibc(:) = ibcy(:)
        str = 'y'
      else if (i == 3) then
        ibc(:) = ibcz(:)
        str = 'z'
      else 
      end if
      call test_function_setup(ibc, dm%h(i), fbc, scale, shift)
      do n = 1, NACC
        iacc = n
        call test_interp_p2c_comparison(dm%nc(i), dm%np(i), dm%h(i), scale, shift, iacc, ibc, fbc, dm, str)
      end do
      do n = 1, NACC
        iacc = n
        call test_interp_c2p_comparison(dm%nc(i), dm%np(i), dm%h(i), scale, shift, iacc, ibc, fbc, dm, str)
      end do
    end do 

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
  subroutine test_1stder_p2c_comparison(nc, np, dd, scale, shift, iacc,  ibc, fbc, dm, str)
    use parameters_constant_mod
    use math_mod
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: nc
    integer, intent(in) :: np
    real(WP), intent(in) :: dd
    real(WP), intent(in) :: scale
    real(WP), intent(in) :: shift
    integer, intent(in) :: iacc
    integer, intent(in) :: ibc(2)
    real(WP), intent(in) :: fbc(4)
    character(1), intent(in) :: str
    type(t_domain), intent(in) :: dm

    integer :: i
    real(WP) :: xc, xp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(2)
    real(WP) :: fgxc(nc)
    real(WP) :: fxp(np)

    open (newunit = wrt_unit(1), file = 'test_1stder_'//trim(int2str(nc))//'.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_1stder_'//trim(str)//'_'//trim(int2str(nc))//'.dat', position="append")

    do i = 1, np
      xp = dd * real(i - 1, WP)
      fxp(i) = sin_wp ( xp / scale + shift)
    end do

    if(trim(str)=='x') then
      call Get_x_1der_P2C_1D(fxp, fgxc, dm, iacc, ibc, fbc)
    else if(trim(str)=='y') then
      call Get_y_1der_P2C_1D(fxp, fgxc, dm, iacc, ibc, fbc)
    else if(trim(str)=='z') then
      call Get_z_1der_P2C_1D(fxp, fgxc, dm, iacc, ibc, fbc)
    else
    end if
    
    err_Linf = ZERO
    err_L2   = ZERO
    write(wrt_unit(2), *) '# 1stder-p2c-'//trim(str), ', iacc=', iacc, ', nc=', nc
    write(wrt_unit(2), *) '# i, xc, ref, cal, err'
    do i = 1, nc
      xc = dd * (real(i - 1, WP) + HALF)
      ref = ONE/scale * cos_wp(xc / scale + shift)
      err = abs_wp(fgxc(i) - ref)
      write(wrt_unit(2),*) i, xc, ref, fgxc(i), err
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
    end do
    err_L2 = sqrt_wp(err_L2 / nc) 
    write(wrt_unit(1), '(A, 2I2, 1I5, 2ES15.7)') &
      '# 1stder-p2c-'//trim(str)//', iacc, ibc, nc, eInf, eL2: ', iacc, ibc(1), nc, err_Linf, err_L2

    close(wrt_unit(1))
    close(wrt_unit(2))
    
    return
  end subroutine

  subroutine test_1stder_p2p_comparison(nc, np, dd, scale, shift, iacc,  ibc, fbc, dm, str)
    use parameters_constant_mod
    use math_mod
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: nc
    integer, intent(in) :: np
    real(WP), intent(in) :: dd
    real(WP), intent(in) :: scale
    real(WP), intent(in) :: shift
    integer, intent(in) :: iacc
    integer, intent(in) :: ibc(2)
    real(WP), intent(in) :: fbc(4)
    character(1), intent(in) :: str
    type(t_domain), intent(in) :: dm

    integer :: i
    real(WP) :: xc, xp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(2)
    real(WP) :: fgxp(np)
    real(WP) :: fxp(np)

    open (newunit = wrt_unit(1), file = 'test_1stder_'//trim(int2str(nc))//'.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_1stder_'//trim(str)//'_'//trim(int2str(nc))//'.dat', position="append")

    do i = 1, np
      xp = dd * real(i - 1, WP)
      fxp(i) = sin_wp ( xp / scale + shift)
    end do

    if(trim(str)=='x') then
      call Get_x_1der_P2P_1D(fxp, fgxp, dm, iacc, ibc, fbc)
    else if(trim(str)=='y') then
      call Get_y_1der_P2P_1D(fxp, fgxp, dm, iacc, ibc, fbc)
    else if(trim(str)=='z') then
      call Get_z_1der_P2P_1D(fxp, fgxp, dm, iacc, ibc, fbc)
    else
    end if

    err_Linf = ZERO
    err_L2   = ZERO
    write(wrt_unit(2), *) '# 1stder-p2p-'//trim(str),  ', iacc=', iacc, ', np=', np
    write(wrt_unit(2), *) '# i, xp, ref, cal, err'
    do i = 1, np
      xp = dd * real(i - 1, WP)
      ref = ONE/scale * cos_wp(xp / scale + shift)
      err = abs_wp(fgxp(i) - ref)
      write(wrt_unit(2),*) i, xp, ref, fgxp(i), err
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
    end do
    err_L2 = sqrt_wp(err_L2 / nc) 
    write(wrt_unit(1), '(A, 2I2, 1I5, 2ES15.7)') &
      '# 1stder-p2p-'//trim(str)//', iacc, ibc, np, eInf, eL2: ', iacc, ibc(1), np, err_Linf, err_L2
    close(wrt_unit(1))
    close(wrt_unit(2))

    return
  end subroutine

  subroutine test_1stder_c2p_comparison(nc, np, dd, scale, shift, iacc,  ibc, fbc, dm, str)
    use parameters_constant_mod
    use math_mod
    use udf_type_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: nc
    integer, intent(in) :: np
    real(WP), intent(in) :: dd
    real(WP), intent(in) :: scale
    real(WP), intent(in) :: shift
    integer, intent(in) :: iacc
    integer, intent(in) :: ibc(2)
    real(WP), intent(in) :: fbc(4)
    character(1), intent(in) :: str
    type(t_domain), intent(in) :: dm

    integer :: i
    real(WP) :: xc, xp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(2)
    real(WP) :: fgxp(np)
    real(WP) :: fxc(nc)

    open (newunit = wrt_unit(1), file = 'test_1stder_'//trim(int2str(nc))//'.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_1stder_'//trim(str)//'_'//trim(int2str(nc))//'.dat', position="append")

    do i = 1, nc
      xc =  dm%h(1) * (real(i - 1, WP) + HALF)
      fxc(i) = sin_wp ( xc / scale + shift)
    end do

    
    if(trim(str)=='x') then
      call Get_x_1der_C2P_1D(fxc, fgxp, dm, iacc, ibc, fbc)
    else if(trim(str)=='y') then
      call Get_y_1der_C2P_1D(fxc, fgxp, dm, iacc, ibc, fbc)
    else if(trim(str)=='z') then
      call Get_z_1der_C2P_1D(fxc, fgxp, dm, iacc, ibc, fbc)
    else
    end if

    err_Linf = ZERO
    err_L2   = ZERO
    write(wrt_unit(2), *) '# 1stder-c2p-'//trim(str), ', iacc=', iacc, ', np=', np
    write(wrt_unit(2), *) '# i, xp, ref, cal, err'
    do i = 1, np
      xp = dd * real(i - 1, WP)
      ref = ONE/scale * cos_wp(xp / scale + shift)
      err = abs_wp(fgxp(i) - ref)
      write(wrt_unit(2),*) i, xp, ref, fgxp(i), err
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
    end do
    err_L2 = sqrt_wp(err_L2 / nc) 
    write(wrt_unit(1), '(A, 2I2, 1I5, 2ES15.7)') &
      '# 1stder-c2p-'//trim(str)//', iacc, ibc, np, eInf, eL2: ', iacc, ibc(1), np, err_Linf, err_L2
    close(wrt_unit(1))
    close(wrt_unit(2))

    return
  end subroutine

  subroutine test_1stder_c2c_comparison(nc, np, dd, scale, shift, iacc,  ibc, fbc, dm, str)
    use parameters_constant_mod
    use udf_type_mod
    use math_mod
    use typeconvert_mod
    implicit none
    integer, intent(in) :: nc
    integer, intent(in) :: np
    real(WP), intent(in) :: dd
    real(WP), intent(in) :: scale
    real(WP), intent(in) :: shift
    integer, intent(in) :: iacc
    integer, intent(in) :: ibc(2)
    real(WP), intent(in) :: fbc(4)
    character(1), intent(in) :: str
    type(t_domain), intent(in) :: dm

    integer :: i
    real(WP) :: xc, xp
    real(WP) :: ref
    real(WP) :: err, err_Linf, err_L2
    integer  :: wrt_unit(2)
    real(WP) :: fgxc(nc)
    real(WP) :: fxc(nc)

    open (newunit = wrt_unit(1), file = 'test_1stder_'//trim(int2str(nc))//'.dat', position="append")
    open (newunit = wrt_unit(2), file = 'test_1stder_'//trim(str)//'_'//trim(int2str(nc))//'.dat', position="append")

    do i = 1, nc
      xc =  dd * (real(i - 1, WP) + HALF)
      fxc(i) = sin_wp ( xc / scale + shift)
    end do

    
    if(trim(str)=='x') then
      call Get_x_1der_C2C_1D(fxc, fgxc, dm, iacc, ibc, fbc)
    else if(trim(str)=='y') then
      call Get_y_1der_C2C_1D(fxc, fgxc, dm, iacc, ibc, fbc)
    else if(trim(str)=='z') then
      call Get_z_1der_C2C_1D(fxc, fgxc, dm, iacc, ibc, fbc)
    else
    end if


    err_Linf = ZERO
    err_L2   = ZERO
    write(wrt_unit(2), *) '# 1stder-c2c-'//trim(str), ', iacc=', iacc, ', nc=', nc
    write(wrt_unit(2), *) '# i, xc, ref, cal, err'
    do i = 1, nc
      xc =  dd * (real(i - 1, WP) + HALF)
      ref = ONE/scale * cos_wp(xc / scale + shift)
      err = abs_wp(fgxc(i) - ref)
      write(wrt_unit(2),*) i, xc, ref, fgxc(i), err
      if(err > err_Linf) err_Linf = err
      err_L2 = err_L2 + err**2
    end do
    err_L2 = sqrt_wp(err_L2 / nc) 
    write(wrt_unit(1), '(A, 2I2, 1I5, 2ES15.7)') &
      '# 1stder-c2c-'//trim(str)//', iacc, ibc, nc, eInf, eL2: ', iacc, ibc(1), nc, err_Linf, err_L2
    close(wrt_unit(1))
    close(wrt_unit(2))
    
    return
  end subroutine

  subroutine Test_1st_derivative(dm)
    use parameters_constant_mod
    use EvenOdd_mod
    use udf_type_mod
    implicit none
    type(t_domain), intent(inout) :: dm

    real(WP) :: scale, shift
    real(WP) :: fbcx(4), fbcy(4), fbcz(4), fbc(4)
    integer  :: ibcx(2), ibcy(2), ibcz(2), ibc(2)
    integer :: n, iacc, i
    character(1) :: str

    dm%h(1) = TWOPI / dm%nc(1)
    dm%h(2) = TWOPI / dm%nc(2)
    dm%h(3) = TWOPI / dm%nc(3)
    ibcx(:) = dm%ibcx_nominal(:, 5)
    ibcy(:) = dm%ibcy_nominal(:, 5)
    ibcz(:) = dm%ibcz_nominal(:, 5)


    do i = 1, 3
      if (i == 1) then
        ibc(:) = ibcx(:)
        str = 'x'
      else if (i == 2) then
        ibc(:) = ibcy(:)
        str = 'y'
      else if (i == 3) then
        ibc(:) = ibcz(:)
        str = 'z'
      else 
      end if
      call test_function_setup(ibc, dm%h(i), fbc, scale, shift)
      write(*,*) 'input=sin(x/',scale,'+',shift,')'
      do n = 1, NACC
        iacc = n
        call test_1stder_p2p_comparison(dm%nc(i), dm%np(i), dm%h(i), scale, shift, iacc, ibc, fbc, dm, str)
      end do
      do n = 1, NACC
        iacc = n
        call test_1stder_c2c_comparison(dm%nc(i), dm%np(i), dm%h(i), scale, shift, iacc, ibc, fbc, dm, str)
      end do
      do n = 1, NACC
        iacc = n
        call test_1stder_p2c_comparison(dm%nc(i), dm%np(i), dm%h(i), scale, shift, iacc, ibc, fbc, dm, str)
      end do
      do n = 1, NACC
        iacc = n
        call test_1stder_c2p_comparison(dm%nc(i), dm%np(i), dm%h(i), scale, shift, iacc, ibc, fbc, dm, str)
      end do
    end do 
    
    return 
  end subroutine

end module
