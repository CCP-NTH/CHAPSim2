!##############################################################################
module mpi_mod
  !include "mpif.h"
  use MPI
  use decomp_2d
  use decomp_2d_mpi
  !use iso_fortran_env
  implicit none
  integer :: ierror
  integer :: nxdomain
  integer :: p_row ! y-dim
  integer :: p_col ! z-dim

  public :: initialise_mpi
  public :: Finalise_mpi

contains 
!==========================================================================================================
!> \brief mpi initialisation.   
!>
!> this initialisation is a simple one.
!  only used before calling decomp_2d_init, 
!  where there is a complicted one used for 2-d decompoistion.
!  nrank = myid
!  nproc = size of processor 
!  both wil be replaced after calling decomp_2d_init
!----------------------------------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> \param[in]     d          domain type
!==========================================================================================================
  subroutine initialise_mpi()
    implicit none
    call MPI_INIT(IERROR)
    call MPI_COMM_RANK(MPI_COMM_WORLD, nrank, IERROR)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, IERROR)
    return
  end subroutine initialise_mpi
!==========================================================================================================
!==========================================================================================================
  subroutine Finalise_mpi()  
    implicit none
    call MPI_FINALIZE(IERROR)
    return
  end subroutine Finalise_mpi

end module mpi_mod

!==========================================================================================================
module precision_mod
  use mpi_mod
  implicit none

  public
  integer, parameter :: I4 = selected_int_kind( 4 )
  integer, parameter :: I8 = selected_int_kind( 8 )
  integer, parameter :: I15 = selected_int_kind( 15 )
  integer, parameter :: S6P = selected_real_kind( p = 6, r = 37 )
  integer, parameter :: D15P = selected_real_kind( p = 15, r = 307 )
  integer, parameter :: Q33P = selected_real_kind( p = 33, r = 4931 )
!#ifdef DOUBLE_PREC
  integer, parameter :: WP = D15P
  integer, parameter :: MPI_REAL_WP = MPI_DOUBLE_PRECISION
  integer, parameter :: MPI_CPLX_WP = MPI_DOUBLE_COMPLEX
! #else
!   integer, parameter :: WP = S6P !D15P
!   integer, parameter :: MPI_REAL_WP = MPI_REAL
!   integer, parameter :: MPI_CPLX_WP = MPI_COMPLEX
! #endif

end module precision_mod
!==========================================================================================================
module parameters_constant_mod
  use precision_mod
  implicit none
!----------------------------------------------------------------------------------------------------------
! user defined methods
!----------------------------------------------------------------------------------------------------------
  logical, parameter :: is_IO_off = .false.         ! true for code performance evaluation without IO
  !logical, parameter :: is_strong_coupling = .true. ! true = RK(rhoh, g)); false = RK(rhoh) + RK(g)
  !logical, parameter :: is_drhodt_chain = .false.   ! false = (d1-d0)/dt; true = d(rhoh)/dt / (drhoh/drho)
  !logical :: is_two_potential_splitting ! true = stable solver but twice fft 
  logical :: is_single_RK_projection ! true = projection only at last RK sub-step, time (o(dt^3)),
  logical :: is_damping_drhodt
  logical :: is_global_mass_correction
!----------------------------------------------------------------------------------------------------------
! constants
!----------------------------------------------------------------------------------------------------------  
  real(WP), parameter :: ZPONE       = 0.1_WP
  real(WP), parameter :: EIGHTH      = 0.125_WP
  real(WP), parameter :: ZPTWO       = 0.2_WP
  real(WP), parameter :: QUARTER     = 0.25_WP
  real(WP), parameter :: ZPTHREE     = 0.3_WP
  real(WP), parameter :: ZPFOUR      = 0.4_WP
  real(WP), parameter :: HALF        = 0.5_WP
  real(WP), parameter :: ZPSIX       = 0.6_WP
  real(WP), parameter :: ZPSEVEN     = 0.7_WP
  real(WP), parameter :: ZPEIGHT     = 0.8_WP
  real(WP), parameter :: ZPNINE      = 0.9_WP

  real(WP), parameter :: ZERO        = 0.0_WP
  real(WP), parameter :: ONE         = 1.0_WP
  real(WP), parameter :: ONEPFIVE    = 1.5_WP
  real(WP), parameter :: TWO         = 2.0_WP
  real(WP), parameter :: TWOPFIVE    = 2.5_WP
  real(WP), parameter :: THREE       = 3.0_WP
  real(WP), parameter :: threepfive  = 3.5_WP
  real(WP), parameter :: FOUR        = 4.0_WP
  real(WP), parameter :: FIVE        = 5.0_WP
  real(WP), parameter :: SIX         = 6.0_WP
  real(WP), parameter :: SEVEN       = 7.0_WP
  real(WP), parameter :: EIGHT       = 8.0_WP
  real(WP), parameter :: NINE        = 9.0_WP
  real(WP), parameter :: ONE_THIRD   = 0.33333333333333333333_WP
  real(WP), parameter :: TWO_THIRD   = 0.66666666666666666667_WP

  real(WP), parameter :: TEN         = 10.0_WP
  real(WP), parameter :: ELEVEN      = 11.0_WP
  real(WP), parameter :: TWELVE      = 12.0_WP
  real(WP), parameter :: THIRTEEN    = 13.0_WP
  real(WP), parameter :: FOURTEEN    = 14.0_WP
  real(WP), parameter :: FIFTEEN     = 15.0_WP
  real(WP), parameter :: SIXTEEN     = 16.0_WP
  real(WP), parameter :: SEVENTEEN   = 17.0_WP
  real(WP), parameter :: TWENTY      = 20.0_WP

  real(WP), parameter :: TWENTYTWO   = 22.0_WP
  real(WP), parameter :: TWENTYTHREE = 23.0_WP
  real(WP), parameter :: TWENTYFOUR  = 24.0_WP
  real(WP), parameter :: TWENTYFIVE  = 25.0_WP
  real(WP), parameter :: TWENTYSIX   = 26.0_WP
  real(WP), parameter :: TWENTYSEVEN = 27.0_WP

  real(WP), parameter :: THIRTYTWO   = 32.0_WP
  real(WP), parameter :: THIRTYFIVE  = 35.0_WP
  real(WP), parameter :: THIRTYSIX   = 36.0_WP
  real(WP), parameter :: THIRTYSEVEN = 37.0_WP

  real(WP), parameter :: FOURTYFIVE  = 45.0_WP

  real(WP), parameter :: FIFTY       = 50.0_WP

  real(WP), parameter :: SIXTY       = 60.0_WP
  real(WP), parameter :: SIXTYTWO    = 62.0_WP
  real(WP), parameter :: SIXTYTHREE  = 63.0_WP

  real(WP), parameter :: EIGHTYSEVEN = 87.0_WP

  real(WP), parameter :: MAXP        = 1.0E16_WP
  real(WP), parameter :: MAXVELO     = 1.0E3_WP
  real(WP), parameter :: MINN        = -1.0E16_WP
#ifdef SINGLE_PREC
  real(WP), parameter :: MINP = 1.0E-8_WP
  real(WP), parameter :: MAXN = -1.0E-8_WP
#else
  real(WP), parameter :: MINP = 1.0E-16_WP
  real(WP), parameter :: MAXN = -1.0E-16_WP
#endif
  

  real(WP), parameter :: PI          = 2.0_WP*(DASIN(1.0_WP)) !3.14159265358979323846_WP !dacos( -ONE ) 
  real(WP), parameter :: TWOPI       = TWO * PI !6.28318530717958647692_WP!TWO * dacos( -ONE )

  complex(mytype),parameter :: cx_one_one=cmplx(one, one, kind=mytype)

  real(WP), parameter, dimension(3, 3) :: KRONECKER_DELTA = &
                                            reshape( (/ &
                                            ONE, ZERO, ZERO, &
                                            ZERO, ONE, ZERO, &
                                            ZERO, ZERO, ONE  /), &
                                            (/3, 3/) )

  real(WP), parameter :: GRAVITY     = 9.80665_WP
!----------------------------------------------------------------------------------------------------------
! fft lib
!----------------------------------------------------------------------------------------------------------
  integer, parameter :: FFT_2DECOMP_3DFFT = 3, &
                        FFT_FISHPACK_2DFFT = 2, &
                        MSTRET_3FMD = 1, &
                        MSTRET_TANH = 2, &
                        MSTRET_POWL = 3
!----------------------------------------------------------------------------------------------------------
! case id
!----------------------------------------------------------------------------------------------------------
  integer, parameter :: ICASE_OTHERS = 0, &
                        ICASE_CHANNEL = 1, &
                        ICASE_PIPE    = 2, &
                        ICASE_ANNULAR = 3, &
                        ICASE_TGV3D   = 4, &
                        ICASE_DUCT    = 5, &
                        ICASE_TGV2D   = 6, &
                        ICASE_BURGERS = 7, &
                        ICASE_ALGTEST = 8
                        
!----------------------------------------------------------------------------------------------------------
! flow initilisation
!----------------------------------------------------------------------------------------------------------     
  integer, parameter :: INIT_RESTART = 0, &
                        INIT_RANDOM  = 2, &
                        INIT_INLET   = 3, &
                        INIT_GVCONST = 4, &
                        INIT_POISEUILLE = 5, &
                        INIT_FUNCTION = 6, &
                        INIT_GVBCLN = 7
!----------------------------------------------------------------------------------------------------------
! coordinates
!----------------------------------------------------------------------------------------------------------
  integer, parameter :: ICARTESIAN   = 1, &
                        ICYLINDRICAL = 2
!----------------------------------------------------------------------------------------------------------
! grid stretching
!----------------------------------------------------------------------------------------------------------               
  integer, parameter :: ISTRET_NO     = 0, &
                        ISTRET_CENTRE = 1, &
                        ISTRET_2SIDES = 2, &
                        ISTRET_BOTTOM = 3, &
                        ISTRET_TOP    = 4, &
                        ISTRET_INPUT  = 5               
!----------------------------------------------------------------------------------------------------------
! time scheme
!----------------------------------------------------------------------------------------------------------
  integer, parameter :: ITIME_RK3    = 3, &
                        ITIME_RK3_CN = 2, &
                        ITIME_AB2    = 1, &
                        ITIME_EULER  = 0
!----------------------------------------------------------------------------------------------------------
! BC
!----------------------------------------------------------------------------------------------------------
  ! warning : Don't change below order for BC types.
  integer, parameter :: IBC_INTERIOR    = 0, & ! basic and nominal, used in operations, bulk, 2 ghost layers
                        IBC_PERIODIC    = 1, & ! basic and nominal, used in operations 
                        IBC_SYMMETRIC   = 2, & ! basic and nominal, used in operations
                        IBC_ASYMMETRIC  = 3, & ! basic and nominal, used in operations
                        IBC_DIRICHLET   = 4, & ! basic and nominal, used in operations
                        IBC_NEUMANN     = 5, & ! basic and nominal, used in operations
                        IBC_INTRPL      = 6, & ! basic only, for all others, used in operations
                        IBC_CONVECTIVE  = 7, & ! nominal only, = IBC_DIRICHLET, dynamic fbc
                        !IBC_TURBGEN     = 8, & ! nominal only, = IBC_PERIODIC, bulk, 2 ghost layers, dynamic fbc
                        IBC_PROFILE1D   = 9, & ! nominal only, = IBC_DIRICHLET, 
                        IBC_DATABASE    = 10, &! nominal only, = IBC_PERIODIC, bulk, 2 ghost layers, dynamic fbc
                        IBC_POISEUILLE  = 11, &! nominal only, = IBC_DIRICHLET, 
                        IBC_OTHERS      = 12   ! interpolation
  integer, parameter :: NBC = 5! u, v, w, p, T
  integer, parameter :: NDIM = 3
  integer, parameter :: IDIM(0:3) = (/0, 1, 2, 3/)
  integer, parameter :: IPENCIL(3) = (/1, 2, 3/)
  integer, parameter :: JBC_SELF = 1, &
                        JBC_GRAD = 2, &
                        JBC_PROD = 3
  integer, parameter :: SPACE_INTEGRAL = 0, &
                        SPACE_AVERAGE = 1
  integer, parameter :: IG2Q = -1, &
                        IQ2G = 1
  integer, parameter :: IBLK = 1, & 
                        IBND = 2, &
                        IALL = 3
!----------------------------------------------------------------------------------------------------------
! numerical accuracy
!----------------------------------------------------------------------------------------------------------             
  integer, parameter :: IACCU_CD2 = 1, &
                        IACCU_CD4 = 2, &
                        IACCU_CP4 = 3, &
                        IACCU_CP6 = 4
!----------------------------------------------------------------------------------------------------------
! numerical scheme for viscous term
!---------------------------------------------------------------------------------------------------------- 
  integer, parameter :: IVIS_EXPLICIT   = 1, &
                        IVIS_SEMIMPLT   = 2
!----------------------------------------------------------------------------------------------------------
! driven force in periodic flow
!---------------------------------------------------------------------------------------------------------- 
  integer, parameter :: IDRVF_NO         = 0, &
                        IDRVF_X_MASSFLUX = 1, &
                        IDRVF_X_TAUW     = 2, &
                        IDRVF_X_DPDX     = 3, &
                        IDRVF_Z_MASSFLUX = 4, &
                        IDRVF_Z_TAUW     = 5, &
                        IDRVF_Z_DPDZ     = 6
!----------------------------------------------------------------------------------------------------------
! BC for thermal
!---------------------------------------------------------------------------------------------------------- 
  integer, parameter :: THERMAL_BC_CONST_T  = 0, &
                        THERMAL_BC_CONST_H  = 1
!----------------------------------------------------------------------------------------------------------
! working fluid media
!---------------------------------------------------------------------------------------------------------- 
  integer, parameter :: ISCP_WATER      = 1, &
                        ISCP_CO2        = 2, &
                        ILIQUID_SODIUM  = 3, &
                        ILIQUID_LEAD    = 4, &
                        ILIQUID_BISMUTH = 5, &
                        ILIQUID_LBE     = 6, &
                        ILIQUID_WATER   = 7, & ! to be updated
                        ILIQUID_LITHIUM = 8, &
                        ILIQUID_FLIBE   = 9, &
                        ILIQUID_PBLI    = 10
!----------------------------------------------------------------------------------------------------------
! physical property
!---------------------------------------------------------------------------------------------------------- 
  integer, parameter :: IPROPERTY_TABLE = 1, &
                        IPROPERTY_FUNCS = 2
!----------------------------------------------------------------------------------------------------------
! database for physical property
!----------------------------------------------------------------------------------------------------------
  character(len = 64), parameter :: INPUT_SCP_WATER = 'NIST_WATER_23.5MP.DAT'
  character(len = 64), parameter :: INPUT_SCP_CO2   = 'NIST_CO2_8MP.DAT'

  real(WP), parameter :: TM0_Na  = 371.0_WP  ! unit: K, melting temperature at 1 atm for Na
  real(WP), parameter :: TM0_Pb  = 600.6_WP  ! unit: K, melting temperature at 1 atm for Lead
  real(WP), parameter :: TM0_BI  = 544.6_WP  ! unit: K, melting temperature at 1 atm for Bismuth
  real(WP), parameter :: TM0_LBE = 398.0_WP  ! unit: K, melting temperature at 1 atm for LBE
  real(WP), parameter :: TM0_H2O = 273.15_WP ! unit: K, melting temperature at 1 atm for water
  real(WP), parameter :: TM0_Li  = 453.65_WP ! unit: K, melting temperature at 1 atm for Lithium
  real(WP), parameter :: TM0_FLiBe = 732.1_WP ! unit: K, melting temperature at 1 atm for FLiBe
  real(WP), parameter :: TM0_PbLi = 508.0_WP ! unit: K, melting temperature at 1 atm for PbLi-17

  real(WP), parameter :: TB0_Na  = 1155.0_WP ! unit: K, boling temperature at 1 atm for Na
  real(WP), parameter :: TB0_Pb  = 2021.0_WP ! unit: K, boling temperature at 1 atm for Lead
  real(WP), parameter :: TB0_BI  = 1831.0_WP ! unit: K, boling temperature at 1 atm for Bismuth
  real(WP), parameter :: TB0_LBE = 1927.0_WP ! unit: K, boling temperature at 1 atm for LBE
  real(WP), parameter :: TB0_H2O = 373.15_WP ! unit: K, boling temperature at 1 atm for water
  real(WP), parameter :: TB0_Li  = 1615.0_WP ! unit: K, boling temperature at 1 atm for Lithium
  real(WP), parameter :: TB0_FLiBe = 1703.0_WP ! unit: K, boling temperature at 1 atm for FLiBe
  real(WP), parameter :: TB0_PbLi = 1943.0_WP ! unit: K, boling temperature at 1 atm for PbLi-17

  real(WP), parameter :: HM0_Na  = 113.0e3_WP ! unit: J / Kg, latent melting heat, enthalpy for Na
  real(WP), parameter :: HM0_Pb  = 23.07e3_WP ! unit: J / Kg, latent melting heat, enthalpy for Lead
  real(WP), parameter :: HM0_BI  =  53.3e3_WP ! unit: J / Kg, latent melting heat, enthalpy for Bismuth
  real(WP), parameter :: HM0_LBE =  38.6e3_WP ! unit: J / Kg, latent melting heat, enthalpy for LBE
  real(WP), parameter :: HM0_H2O = 334.0e3_WP ! unit: J / Kg, latent melting heat, enthalpy for water
  real(WP), parameter :: HM0_Li  =  4.55e5_WP  ! unit: J / Kg, latent melting heat, enthalpy for Lithium
  real(WP), parameter :: HM0_FLiBe = 17.47e5_WP ! integral(Cp(TM0))
  real(WP), parameter :: HM0_PbLi = 33.9e3_WP ! unit: J / Kg, latent melting heat, enthalpy for PbLi-17

  ! D = CoD(0) + CoD(1) * T
  real(WP), parameter :: CoD_Na(0:1)  = (/ 1014.0_WP,  -0.235_WP /)
  real(WP), parameter :: CoD_Pb(0:1)  = (/11441.0_WP, -1.2795_WP /)
  real(WP), parameter :: CoD_Bi(0:1)  = (/10725.0_WP,   -1.22_WP /)
  real(WP), parameter :: CoD_LBE(0:1) = (/11065.0_WP,   1.293_WP /)
  real(WP), parameter :: CoD_Li(0:4)  = (/278.5_WP,  -0.04657_WP, 274.6_WP, 3500.0_WP, 0.467_WP /) ! D = CoD(0) + CoD(1) * T + CoD(2) * (1 - T / CoD(3))^(CoD(4))
  real(WP), parameter :: CoD_FLiBe(0:1) = (/ 2413.03_WP, -0.4884_WP /)
  real(WP), parameter :: CoD_PbLi(0:1) = (/10520.4_WP, -1.1905_WP/)

  ! K = CoK(0) + CoK(1) * T + CoK(2) * T^2
  real(WP), parameter :: CoK_Na(0:2)  = (/104.0_WP,   -0.047_WP,       0.0_WP/)
  real(WP), parameter :: CoK_Pb(0:2)  = (/  9.2_WP,    0.011_WP,       0.0_WP/)
  real(WP), parameter :: CoK_Bi(0:2)  = (/ 7.34_WP,   9.5E-3_WP,       0.0_WP/)
  real(WP), parameter :: CoK_LBE(0:2) = (/3.284_WP, 1.617E-2_WP, -2.305E-6_WP/)
  real(WP), parameter :: CoK_Li(0:2)  = (/22.28_WP,   0.0500_WP, -1.243E-5_WP/)
  real(WP), parameter :: CoK_FLiBe(0:2) = (/1.1_WP,      0.0_WP,       0.0_WP/)
  real(WP), parameter :: CoK_PbLi(0:2) = (/9.148_WP, 1.963E-2_WP,      0.0_WP/)

  ! B = 1 / (CoB - T)
  real(WP), parameter :: CoB_Na = 4316.0_WP
  real(WP), parameter :: CoB_Pb = 8942.0_WP
  real(WP), parameter :: CoB_BI = 8791.0_WP
  real(WP), parameter :: CoB_LBE= 8558.0_WP
  real(WP), parameter :: CoB_Li = 5620.0_WP
  real(WP), parameter :: CoB_FLiBe = 4940.7_WP
  real(WP), parameter :: CoB_PbLi = 8836.8_WP

  ! Cp = CoCp(-2) * T^(-2) + CoCp(-1) * T^(-1) + CoCp(0) + CoCp(1) * T + CoCp(2) * T^2
  real(WP), parameter :: CoCp_Na(-2:2) = (/-3.001e6_WP, 0.0_WP, 1658.0_WP,   -0.8479_WP, 4.454E-4_WP/)
  real(WP), parameter :: CoCp_Pb(-2:2) = (/-1.524e6_WP, 0.0_WP,  176.2_WP, -4.923E-2_WP, 1.544E-5_WP/)
  real(WP), parameter :: CoCp_Bi(-2:2) = (/ 7.183e6_WP, 0.0_WP,  118.2_WP,  5.934E-3_WP,      0.0_WP/)
  real(WP), parameter :: CoCp_LBE(-2:2)= (/-4.56e5_WP, 0.0_WP,  164.8_WP, - 3.94E-2_WP,  1.25E-5_WP/)
  real(WP), parameter :: CoCp_Li(-2:2) = (/    0.0_WP, 0.0_WP, 4754.0_WP,  -9.25E-1_WP,  2.91E-4_WP/)
  real(WP), parameter :: CoCp_FLiBe(-2:2) = (/ 0.0_WP, 0.0_WP, 2386.0_WP,       0.0_WP,      0.0_WP/)
  real(WP), parameter :: CoCp_PbLi(-2:2) = (/  0.0_WP, 0.0_WP,  195.0_WP, -9.116E-3_WP,      0.0_WP/)

  ! H = HM0 + CoH(-1) * (1 / T - 1 / TM0) + CoH(0) + CoH(1) * (T - TM0) +  CoH(2) * (T^2 - TM0^2) +  CoH(3) * (T^3- TM0^3)
  real(WP), parameter :: CoH_Na(-1:3)  = (/  4.56e5_WP, 0.0_WP, 164.8_WP,   -1.97E-2_WP, 4.167E-4_WP/)
  real(WP), parameter :: CoH_Pb(-1:3)  = (/ 1.524e6_WP, 0.0_WP, 176.2_WP, -2.4615E-2_WP, 5.147E-6_WP/)
  real(WP), parameter :: CoH_Bi(-1:3)  = (/-7.183e6_WP, 0.0_WP, 118.2_WP,   2.967E-3_WP,      0.0_WP/)
  real(WP), parameter :: CoH_LBE(-1:3) = (/  4.56e5_WP, 0.0_WP, 164.8_WP,   -1.97E-2_WP, 4.167E-4_WP/)! check, WRong from literature.
  real(WP), parameter :: CoH_Li(-1:3)  = (/     0.0_WP, 0.0_WP, 4754.0_WP,  -4.625E-1_WP, 9.70E-5_WP/) ! derived from Cp
  real(WP), parameter :: CoH_FLiBe(-1:3) = (/   0.0_WP, 0.0_WP, 2386.0_WP,        0.0_WP,     0.0_WP/)
  real(WP), parameter :: CoH_PbLi(-1:3) = (/    0.0_WP, 0.0_WP, 195.0_WP,   -4.558E-3_WP,     0.0_WP/) ! derived from Cp

  ! M = vARies
  real(WP), parameter :: CoM_Na(-1:1) = (/556.835_WP,  -6.4406_WP, -0.3958_WP/) ! M = exp ( CoM(-1) / T + CoM(0) + CoM(1) * ln(T) )
  real(WP), parameter :: CoM_Pb(-1:1) = (/ 1069.0_WP,  4.55E-4_WP,     0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  real(WP), parameter :: CoM_Bi(-1:1) = (/  780.0_WP, 4.456E-4_WP,     0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  real(WP), parameter :: CoM_LBE(-1:1)= (/  754.1_WP,  4.94E-4_WP,     0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  real(WP), parameter :: CoM_Li(-1:1) = (/-4.164_WP, -6.374E-1_WP, 2.921e2_WP/) ! M = exp ( CoM(-1) + CoM(0) * ln(T) + (CoM(1) / T) )
  real(WP), parameter :: CoM_FLiBe(-1:1) = (/4022.0_WP, 7.803E-5_WP,   0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  real(WP), parameter :: CoM_PbLi(0:3) = (/0.0061091_WP, -2.2574E-5_WP, 3.766E-8_WP, -2.2887E-11_WP/) ! M = CoM(0) + CoM(1) * T + CoM(2) * T^2 + CoM(3) * T^3
end module parameters_constant_mod
!==========================================================================================================
module wtformat_mod
  !use iso_fortran_env
  implicit none

  character(len = 19) :: wrtfmt1i   = '(2X, A40, 1I8.1 )'
  character(len = 19) :: wrtfmt1il  = '(2X, A40, 1I15.1)'
  character(len = 19) :: wrtfmt2i   = '(2X, A40, 2I8.1,)'
  character(len = 19) :: wrtfmt2il  = '(2X, A40, 2I15.1)'
  character(len = 19) :: wrtfmt3i   = '(2X, A40, 3I8.1 )'
  character(len = 19) :: wrtfmt4i   = '(2X, A40, 4I8.1 )'
  character(len = 19) :: wrtfmt1r   = '(2X, A40, 1F14.7)'
  character(len = 19) :: wrtfmt2r   = '(2X, A40, 2F14.7)'
  character(len = 19) :: wrtfmt3r   = '(2X, A40, 3F14.7)'
  character(len = 22) :: wrtfmt1el  = '(2X, A40, 1ES23.15)'
  character(len = 22) :: wrtfmt2el  = '(2X, A40, 2ES23.15)'
  character(len = 22) :: wrtfmt1e   = '(2X, A40, 1ES16.8)'
  character(len = 22) :: wrtfmt2e   = '(2X, A40, 2ES16.8)'
  character(len = 22) :: wrtfmt3e   = '(2X, A40, 3ES16.8)'
  character(len = 24) :: wrtfmt2ae  = '(2X, 2(A15, 1ES23.15))'
  character(len = 26) :: wrtfmt1i1r = '(2X, A40, 1I8.1, 1F14.7)'
  character(len = 26) :: wrtfmt1il1r= '(2X, A40, 1I15.1, 1F14.7)'
  character(len = 26) :: wrtfmt2i2r = '(2X, A40, 2I8.1, 2F14.7)'
  character(len = 26) :: wrtfmt4i2r = '(2X, A20, 4I8.1, 2F14.7)'
  character(len = 15) :: wrtfmt3l   = '(2X, A40, 3L4)'
  character(len = 15) :: wrtfmt1l   = '(2X, A40, 1L4)'
  character(len = 17) :: wrtfmt2s   = '(2X, A40, 1A72)'
  character(len = 17) :: wrtfmt3s   = '(2X, A40, 2A15)'
  character(len = 9 ) :: wrtfmt1s   = '(2X, A80)'
  

end module wtformat_mod
!==========================================================================================================
module udf_type_mod
  use parameters_constant_mod, only: NDIM, NBC, WP
  use mpi_mod
  implicit none
!----------------------------------------------------------------------------------------------------------
!  fluid thermal property info
!---------------------------------------------------------------------------------------------------------- 
  type t_fluidThermoProperty
    real(WP) :: t  ! temperature
    real(WP) :: d  ! density
    real(WP) :: m  ! dynviscosity
    real(WP) :: k  ! thermconductivity
    real(WP) :: h  ! enthalpy
    real(WP) :: rhoh ! mass enthalpy
    real(WP) :: cp ! specific heat capacity 
    real(WP) :: b  ! thermal expansion
    real(WP) :: alpha ! thermal diffusivity, alpha = k / (rho * cp)
    real(WP) :: Pr ! Pr = m / (rho * alpha) = m * cp / k
  end type t_fluidThermoProperty
!----------------------------------------------------------------------------------------------------------
!  parameters to calculate the fluid thermal property 
!---------------------------------------------------------------------------------------------------------- 
  type t_fluid_parameter
    character(len = 64) :: inputProperty
    integer :: ifluid
    integer :: ipropertyState
    integer :: nlist
    real(WP) :: TM0
    real(WP) :: TB0
    real(WP) :: HM0
    real(WP) :: CoD(0:4)
    real(WP) :: CoK(0:2)
    real(WP) :: CoB
    real(WP) :: CoCp(-2:2)
    real(WP) :: CoH(-1:3)
    real(WP) :: CoM(-1:3)
    real(WP) :: dhmax ! undim
    real(WP) :: dhmin ! undim
    type(t_fluidThermoProperty) :: ftp0ref    ! dim, reference state
    type(t_fluidThermoProperty) :: ftpini     ! dim, initial state
  end type t_fluid_parameter
!----------------------------------------------------------------------------------------------------------
!  domain info
!---------------------------------------------------------------------------------------------------------- 
  type t_domain
    logical :: is_periodic(NDIM)       ! is this direction periodic bc?
    logical :: is_stretching(NDIM)      ! is this direction of stretching grids?
    logical :: is_compact_scheme     ! is compact scheme applied?
    logical :: is_thermo             ! is thermal field considered? 
    logical :: is_conv_outlet(3)
    logical :: is_record_xoutlet
    logical :: is_read_xinlet
    logical :: is_mhd
    logical :: fft_skip_c2c(3)
    integer :: idom                  ! domain id
    integer :: icase                 ! case id
    integer :: icoordinate           ! coordinate type
    integer :: ifft_lib
    integer :: icht
    integer :: iTimeScheme
    integer :: iviscous
    integer :: iAccuracy
    integer :: ckpt_nfre
    integer :: visu_nfre
    integer :: visu_idim
    integer :: visu_nskip(NDIM)
    integer :: stat_istart
    integer :: stat_nskip(NDIM)
    integer :: nsubitr
    integer :: istret, mstret
    integer :: ndbfre
    integer :: ndbend
    integer :: ndbstart
    integer :: nc(NDIM) ! geometric cell number
    integer :: np_geo(NDIM) ! geometric points
    integer :: np(NDIM) ! calculated points
    integer :: proben   ! global number of probed points
    ! integer  :: ibcx(2, NBC) ! real bc type, (5 variables, 2 sides), u, v, w, p, T
    ! integer  :: ibcy(2, NBC) ! real bc type, (5 variables, 2 sides)
    ! integer  :: ibcz(2, NBC) ! real bc type, (5 variables, 2 sides)
    integer  :: ibcx_qx(2)
    integer  :: ibcy_qx(2)
    integer  :: ibcz_qx(2)
    integer  :: ibcx_qy(2)
    integer  :: ibcy_qy(2)
    integer  :: ibcz_qy(2)
    integer  :: ibcx_qz(2)
    integer  :: ibcy_qz(2)
    integer  :: ibcz_qz(2)
    integer  :: ibcx_pr(2)
    integer  :: ibcy_pr(2)
    integer  :: ibcz_pr(2)
    integer  :: ibcx_Tm(2)
    integer  :: ibcy_Tm(2)
    integer  :: ibcz_Tm(2)
    integer  :: ibcx_ftp(2)
    integer  :: ibcy_ftp(2)
    integer  :: ibcz_ftp(2)
    integer  :: ibcx_nominal(2, NBC) ! nominal (given) bc type, (5 variables, 2 sides), u, v, w, p, T
    integer  :: ibcy_nominal(2, NBC) ! nominal (given) bc type, (5 variables, 2 sides)
    integer  :: ibcz_nominal(2, NBC) ! nominal (given) bc type, (5 variables, 2 sides)
    real(wp) :: fbcx_const(2, NBC) ! bc values, (5 variables, 2 sides)
    real(wp) :: fbcy_const(2, NBC) ! bc values, (5 variables, 2 sides)
    real(wp) :: fbcz_const(2, NBC) ! bc values, (5 variables, 2 sides)
    real(WP) :: inlet_tbuffer_len

    real(wp) :: lxx
    real(wp) :: lyt
    real(wp) :: lyb
    real(wp) :: lzz
    real(wp) :: vol
    real(WP) :: rstret
    real(wp) :: dt

    real(wp) :: h(NDIM) ! uniform dx
    real(wp) :: h1r(NDIM) ! uniform (dx)^(-1)
    real(wp) :: h2r(NDIM) ! uniform (dx)^(-2)
    real(wp) :: tGamma(0:3)
    real(wp) :: tZeta (0:3)
    real(wp) :: tAlpha(0:3)
    real(wp) :: sigma1p, sigma2p

    type(DECOMP_INFO) :: dccc ! eg, p
    type(DECOMP_INFO) :: dpcc ! eg, ux
    type(DECOMP_INFO) :: dcpc ! eg, uy
    type(DECOMP_INFO) :: dccp ! eg, uz
    type(DECOMP_INFO) :: dppc ! eg, <ux>^y, <uy>^x
    type(DECOMP_INFO) :: dpcp ! eg, <ux>^z, <uz>^x
    type(DECOMP_INFO) :: dcpp ! eg, <uy>^z, <uz>^y
    type(DECOMP_INFO) :: dppp

    type(DECOMP_INFO) :: d4cc
    type(DECOMP_INFO) :: d4pc
    type(DECOMP_INFO) :: dc4c

    type(DECOMP_INFO) :: dxcc
    type(DECOMP_INFO) :: dxpc
    type(DECOMP_INFO) :: dxcp
    ! damping func.
    real(wp), allocatable :: xdamping(:)
    real(wp), allocatable :: zdamping(:)
    ! node location, mapping 
    real(wp), allocatable :: yMappingpt(:, :) ! j = 1, first coefficient in first deriviation. 1/h'
                                              ! j = 2, first coefficient in second deriviation 1/h'^2
                                              ! j = 3, second coefficient in second deriviation -h"/h'^3
    ! cell centre location, mapping
    real(wp), allocatable :: yMappingcc(:, :) ! first coefficient in first deriviation. 1/h'
                                              ! first coefficient in second deriviation 1/h'^2
                                              ! second coefficient in second deriviation -h"/h'^3
    real(wp), allocatable :: yp(:)
    real(wp), allocatable :: yc(:)
    real(wp), allocatable :: rc(:) ! =yc * is_cylindrical
    real(wp), allocatable :: rp(:) ! =yp * is_cylindrical
    real(wp), allocatable :: rci(:) ! reciprocal of raidus based on cell centre
    real(wp), allocatable :: rpi(:) ! reciprocal of raidus based on node point
    integer, allocatable :: ijnp_sym(:)
    integer, allocatable :: ijnc_sym(:)
    integer, allocatable :: knc_sym(:) ! knc_sym = knp_sym 

    real(wp), allocatable :: fbcx_qx(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_qx(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_qx(:, :, :) ! variable bc

    real(wp), allocatable :: fbcx_gx(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_gx(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_gx(:, :, :) ! variable bc

    real(wp), allocatable :: fbcx_qy(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_qy(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_qy(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_qyr(:, :, :) ! qy/r = ur bc at y dirction
    real(wp), allocatable :: fbcz_qyr(:, :, :) ! qy/r = ur bc at z dirction

    real(wp), allocatable :: fbcx_gy(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_gy(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_gy(:, :, :) ! variable bc
    !real(wp), allocatable :: fbcy_gyr(:, :, :) ! gy/r = rho * ur bc at y dirction
    !real(wp), allocatable :: fbcz_gyr(:, :, :) ! gy/r = rho * ur bc at z dirction

    real(wp), allocatable :: fbcx_qz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_qz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_qz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_qzr(:, :, :) ! qz/r = u_theta bc at y dirction
    real(wp), allocatable :: fbcz_qzr(:, :, :) ! qz/r = u_theta bc at z dirction

    real(wp), allocatable :: fbcx_gz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_gz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_gz(:, :, :) ! variable bc
    !real(wp), allocatable :: fbcy_gzr(:, :, :) ! gz/r = rho * u_theta bc at y dirction
    !real(wp), allocatable :: fbcz_gzr(:, :, :) ! gz/r = rho * u_theta bc at z dirction

    real(wp), allocatable :: fbcx_pr(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_pr(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_pr(:, :, :) ! variable bc

    real(wp), allocatable :: fbcx_qw(:, :, :) ! heat flux at wall x
    real(wp), allocatable :: fbcy_qw(:, :, :) ! heat flux at wall y
    real(wp), allocatable :: fbcz_qw(:, :, :) ! heat flux at wall z

    real(wp), allocatable :: fbcx_qx_outl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qx_outl2(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qy_outl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qy_outl2(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qz_outl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qz_outl2(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_pr_outl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_pr_outl2(:, :, :) ! variable bc

    real(wp), allocatable :: fbcx_qx_inl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qx_inl2(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qy_inl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qy_inl2(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qz_inl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_qz_inl2(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_pr_inl1(:, :, :) ! variable bc
    real(wp), allocatable :: fbcx_pr_inl2(:, :, :) ! variable bc

    type(t_fluidThermoProperty), allocatable :: fbcx_ftp(:, :, :)  ! undim, xbc state
    type(t_fluidThermoProperty), allocatable :: fbcy_ftp(:, :, :)  ! undim, ybc state
    type(t_fluidThermoProperty), allocatable :: fbcz_ftp(:, :, :)  ! undim, zbc state

    real(WP), allocatable :: probexyz(:, :) ! (1:3, xyz coord)
    logical,  allocatable :: probe_is_in(:)
    integer,  allocatable :: probexid(:, :) ! (1:3, local index)
  end type t_domain
!----------------------------------------------------------------------------------------------------------
!  flow info
!---------------------------------------------------------------------------------------------------------- 
  type t_flow
    integer  :: idriven
    integer  :: igravity
    integer  :: inittype
    integer  :: iterfrom
    integer  :: initReTo
    integer  :: nIterFlowStart
    integer  :: nIterFlowEnd
    integer  :: iteration

    real(WP) :: time
    real(WP) :: ren
    real(WP) :: rre
    real(WP) :: init_velo3d(NDIM)
    real(wp) :: reninit
    real(WP) :: drvfc
    real(WP) :: fgravity(NDIM)

    real(wp) :: noiselevel
    real(wp) :: mcon(4)
    real(wp) :: tt_mass_change
    real(wp) :: tt_kinetic_energy

    real(WP), allocatable :: qx(:, :, :)  ! qx = u_x,     axial direction
    real(WP), allocatable :: qy(:, :, :)  ! qy = u_r * r, radial direction
    real(WP), allocatable :: qz(:, :, :)  ! qz = u_theta, azimuthal direction
    real(WP), allocatable :: gx(:, :, :)  ! gx = rho * q_x
    real(WP), allocatable :: gy(:, :, :)  ! gy = rho * q_y
    real(WP), allocatable :: gz(:, :, :)  ! gz = rho * q_z
    real(WP), allocatable :: gx0(:, :, :)
    real(WP), allocatable :: gy0(:, :, :)
    real(WP), allocatable :: gz0(:, :, :)
    real(WP), allocatable :: qx0(:, :, :)
    real(WP), allocatable :: qy0(:, :, :)
    real(WP), allocatable :: qz0(:, :, :)

    real(WP), allocatable :: pres(:, :, :)
    real(WP), allocatable :: pcor(:, :, :)
    real(WP), allocatable :: pcor_zpencil_ggg(:, :, :)

    real(WP), allocatable :: dDens(:, :, :)
    real(WP), allocatable :: drhodt(:, :, :)
    real(WP), allocatable :: mVisc(:, :, :)
    real(WP), allocatable :: dDens0(:, :, :)
    real(WP), allocatable :: mVisc0(:, :, :)

    real(WP), allocatable :: mx_rhs(:, :, :) ! current step rhs in x
    real(WP), allocatable :: my_rhs(:, :, :) ! current step rhs in y
    real(WP), allocatable :: mz_rhs(:, :, :) ! current step rhs in z

    real(WP), allocatable :: mx_rhs0(:, :, :)! last step rhs in x
    real(WP), allocatable :: my_rhs0(:, :, :)! last step rhs in y
    real(WP), allocatable :: mz_rhs0(:, :, :)! last step rhs in z

    real(WP), allocatable :: fbcx_a0cc_rhs0(:, :)  !
    real(WP), allocatable :: fbcx_a0pc_rhs0(:, :)
    real(WP), allocatable :: fbcx_a0cp_rhs0(:, :)
    real(WP), allocatable :: fbcz_apc0_rhs0(:, :)  !
    real(WP), allocatable :: fbcz_acp0_rhs0(:, :)
    real(WP), allocatable :: fbcz_acc0_rhs0(:, :)

    real(WP), allocatable :: lrfx(:, :, :) ! Lorentz force  !
    real(WP), allocatable :: lrfy(:, :, :) ! Lorentz force
    real(WP), allocatable :: lrfz(:, :, :) ! Lorentz force
    ! post processing - sharing
    real(WP), allocatable :: tavg_u   (:, :, :, :)  ! 3  = u, v, w
    real(WP), allocatable :: tavg_pr  (:, :, :)
    real(WP), allocatable :: tavg_pru (:, :, :, :)  ! 3  = pu, pv, pw
    real(WP), allocatable :: tavg_uu  (:, :, :, :)  ! 6  = uu, uv, uw, vv, vw, ww
    real(WP), allocatable :: tavg_uuu (:, :, :, :)  ! 10 = uuu, uuv, uuw, uvv, uvw, uww, vvv, vvw, vww, www
    real(WP), allocatable :: tavg_dudu(:, :, :, :)  ! 6  = dui/dxk * duj/duk (covers 45 = dui/dxj * dum/dun)
    ! du/dx * du/dx, du/dx * du/dy, du/dx * du/dz (1 2 3)
    ! du/dx * dv/dx, du/dx * dv/dy, du/dx * dv/dz (4 5 6)
    ! du/dx * dw/dz, du/dx * dw/dy, du/dx * dw/dz (7 8 9)
    !                du/dy * du/dy, du/dy * du/dz, (10, 11)
    ! du/dy * dv/dx, du/dy * dv/dy, du/dy * dv/dz (12, 13, 14)
    ! du/dy * dw/dx, du/dy * dw/dy, du/dy * dw/dz (15, 16, 27)
    !                               du/dz * du/dz, (18)
    !                du/dz * dv/dy, du/dz * dv/dz, (19, 20, 21)
    ! du/dz * dw/dx, du/dz * dw/dy, du/dz * dw/dy, (22, 23, 24)
    ! dv/dx * dv/dx, dv/dx * dv/dy, dv/dx * dv/dz, (25, 26, 27)
    ! dv/dx * dw/dx, dv/dx * dw/dy, dv/dx * dw/dz, (28, 29, 30)
    !                dv/dy * dv/dy, dv/dy * dv/dz, (30, 31)
    ! dv/dy * dw/dx, dv/dy * dw/dy, dv/dy * dw/dz, (32, 33, 34)
    !                               dv/dy * dw/dz, (35)
    !                               dv/dz * dv/dz, (36)
    ! dv/dz * dw/dx, dv/dz * dw/dy, dv/dz * dw/dz, (37, 38, 39)
    ! dw/dx * dw/dx, dw/dx * dw/dy, dw/dx * dw/dz, (40, 41, 42)
    !                dw/dy * dw/dy, dw/dy * dw/dz, (43, 44)
    !                               dw/dw * dw/dz, (45)
    ! post processing - thermal
    real(WP), allocatable :: tavg_f   (:, :, :)    ! f = rho
    real(WP), allocatable :: tavg_fu  (:, :, :, :) ! 3 = rhou, rhov, rhow
    real(WP), allocatable :: tavg_fuu (:, :, :, :) ! 6 = rho*uu, rho*uv, rho*uw, rho*vv, rho*vw, rho*ww
    real(WP), allocatable :: tavg_fuuu(:, :, :, :) ! 10 = uuu, uuv, uuw, uvv, uvw, uww, vvv, vvw, vww, www
    !
    real(WP), allocatable :: tavg_fh  (:, :, :)    ! fh= rho * h
    real(WP), allocatable :: tavg_fuh (:, :, :, :) ! 3 = rho*u*h, rho*v*h, rho*w*h
    real(WP), allocatable :: tavg_fuuh(:, :, :, :) ! 6 = rho*uu*h, rho*uv*h, rho*uw*h, rho*vv*h, rho*vw*h, rho*ww*h
    ! MHD
    real(WP), allocatable :: tavg_eu  (:, :, :, :)  ! 3 = phi * u, phi * v, phi * w
    
  end type t_flow
!----------------------------------------------------------------------------------------------------------
!  thermo info
!---------------------------------------------------------------------------------------------------------- 
  type t_thermo
    integer :: ifluid
    integer  :: inittype
    integer  :: iterfrom
    integer  :: iteration
    integer  :: nIterThermoStart
    integer  :: nIterThermoEnd
    real(WP) :: ref_l0  ! dim
    real(WP) :: ref_T0  ! '0' means dimensional 
    real(WP) :: init_T0 ! dim
    real(WP) :: time
    real(WP) :: phy_time
    real(WP) :: rPrRen
    real(WP) :: tt_enthalpy

    real(WP), allocatable :: rhoh(:, :, :)
    real(WP), allocatable :: hEnth(:, :, :)
    real(WP), allocatable :: kCond(:, :, :)
    real(WP), allocatable :: tTemp(:, :, :)
    real(WP), allocatable :: ene_rhs(:, :, :)  ! current step rhs
    real(WP), allocatable :: ene_rhs0(:, :, :) ! last step rhs
    real(WP), allocatable :: fbcx_rhoh_rhs0(:, :)  !
    real(WP), allocatable :: fbcz_rhoh_rhs0(:, :)  !

    real(WP), allocatable :: tavg_h(:, :, :)
    !real(WP), allocatable :: tavg_hh(:, :, :)
    real(WP), allocatable :: tavg_T(:, :, :)
    real(WP), allocatable :: tavg_TT(:, :, :)
    !real(WP), allocatable :: tavg_dTdT(:, :, :, :)   ! 6 = dt/dx * dt/dx, dt/dx * dt/dy, dt/dx * dt/dz, dt/dy * dt/dy, dt/dy * dt/dz, dt/dz * dt/dz

    type(t_fluidThermoProperty) :: ftp_ini ! undimensional
  end type t_thermo
  type(t_fluid_parameter) :: fluidparam ! dimensional
!----------------------------------------------------------------------------------------------------------
!  mhd info
!---------------------------------------------------------------------------------------------------------- 
  type t_mhd
    integer  :: iterfrom
    integer  :: iteration
    logical :: is_NStuart
    logical :: is_NHartmn
    real(WP) :: NStuart
    real(WP) :: NHartmn
    real(WP) :: B_static(3) ! scaled B.
    real(WP), allocatable :: ep(:, :, :) ! electric potential, scalar
    real(WP), allocatable :: jx(:, :, :) ! current density in x
    real(WP), allocatable :: jy(:, :, :) ! current density in y
    real(WP), allocatable :: jz(:, :, :) ! current density in z
    real(WP), allocatable :: bx(:, :, :) ! magnetic field in x
    real(WP), allocatable :: by(:, :, :) ! current density in x
    real(WP), allocatable :: bz(:, :, :) ! current density in x

    integer  :: ibcx_ep(2)
    integer  :: ibcy_ep(2)
    integer  :: ibcz_ep(2)

    integer  :: ibcx_jx(2)
    integer  :: ibcy_jx(2)
    integer  :: ibcz_jx(2)
    integer  :: ibcx_jy(2)
    integer  :: ibcy_jy(2)
    integer  :: ibcz_jy(2)
    integer  :: ibcx_jz(2)
    integer  :: ibcy_jz(2)
    integer  :: ibcz_jz(2)

    integer  :: ibcx_bx(2)
    integer  :: ibcy_bx(2)
    integer  :: ibcz_bx(2)
    integer  :: ibcx_by(2)
    integer  :: ibcy_by(2)
    integer  :: ibcz_by(2)
    integer  :: ibcx_bz(2)
    integer  :: ibcy_bz(2)
    integer  :: ibcz_bz(2)

    real(WP), allocatable :: fbcx_ep(:, :, :)
    real(WP), allocatable :: fbcy_ep(:, :, :)
    real(WP), allocatable :: fbcz_ep(:, :, :)

    real(WP), allocatable :: fbcx_jx(:, :, :)
    real(WP), allocatable :: fbcy_jx(:, :, :)
    real(WP), allocatable :: fbcz_jx(:, :, :)
    real(WP), allocatable :: fbcx_jy(:, :, :)
    real(WP), allocatable :: fbcy_jy(:, :, :)
    real(WP), allocatable :: fbcz_jy(:, :, :)
    real(WP), allocatable :: fbcx_jz(:, :, :)
    real(WP), allocatable :: fbcy_jz(:, :, :)
    real(WP), allocatable :: fbcz_jz(:, :, :)

    real(WP), allocatable :: fbcx_bx(:, :, :)
    real(WP), allocatable :: fbcy_bx(:, :, :)
    real(WP), allocatable :: fbcz_bx(:, :, :)
    real(WP), allocatable :: fbcx_by(:, :, :)
    real(WP), allocatable :: fbcy_by(:, :, :)
    real(WP), allocatable :: fbcz_by(:, :, :)
    real(WP), allocatable :: fbcx_bz(:, :, :)
    real(WP), allocatable :: fbcy_bz(:, :, :)
    real(WP), allocatable :: fbcz_bz(:, :, :)
    !
    real(WP), allocatable :: tavg_e (:, :, :)    ! e = electric potential, phi
    real(WP), allocatable :: tavg_j (:, :, :, :) ! 3 = j1 , j2, j3
    real(WP), allocatable :: tavg_ej(:, :, :, :) ! 3 = phi * j1 , phi * j2, phi * j3
    real(WP), allocatable :: tavg_jj(:, :, :, :) ! 6 = jj11, jj12, jj13, jj22, jj23, jj33
  end type


end module
!==========================================================================================================
!==========================================================================================================
module vars_df_mod
  use udf_type_mod
  implicit none

  type(t_domain), allocatable, save :: domain(:)
  type(t_flow),   allocatable, save :: flow(:)
  type(t_thermo), allocatable, save :: thermo(:)
  type(t_mhd),    allocatable, save :: mhd(:)
end module
!==========================================================================================================
module io_files_mod
  implicit none
  character(8) :: dir_code='0_src'
  character(9) :: dir_data='1_data'
  character(6) :: dir_visu='2_visu'
  character(9) :: dir_moni='3_monitor'
  character(9) :: dir_chkp='4_check'
  public :: create_directory

  interface operator( .f. )
    module procedure file_exists
  end interface

contains
  function file_exists(filename) result(res)
    implicit none
    character(len=*),intent(in) :: filename
    logical                     :: res

    ! Check if the file exists
    inquire( file=trim(filename), exist=res )
  end function

  subroutine create_directory
    implicit none
    call system('mkdir -p '//dir_code)
    call system('mkdir -p '//dir_data)
    call system('mkdir -p '//dir_visu)
    call system('mkdir -p '//dir_moni)
    call system('mkdir -p '//dir_chkp)
    return
  end subroutine
end module
!==========================================================================================================
module math_mod
  use precision_mod
  use parameters_constant_mod
  implicit none

  interface sqrt_wp
    module procedure sqrt_sp
    module procedure sqrt_dp
  end interface sqrt_wp

  interface tanh_wp
    module procedure tanh_sp
    module procedure tanh_dp
  end interface tanh_wp

  interface abs_wp
    module procedure abs_sp
    module procedure abs_dp
  end interface abs_wp

  interface abs_prec
    module procedure abs_sp
    module procedure abs_dp
    module procedure abs_csp
    module procedure abs_cdp
  end interface abs_prec

  interface sin_wp
    module procedure sin_sp
    module procedure sin_dp
  end interface sin_wp

  interface sin_prec
    module procedure sin_sp
    module procedure sin_dp
  end interface sin_prec

  interface cos_wp
    module procedure cos_sp
    module procedure cos_dp
  end interface cos_wp

  interface cos_prec
    module procedure cos_sp
    module procedure cos_dp
  end interface cos_prec

  interface tan_wp
    module procedure tan_sp
    module procedure tan_dp
  end interface tan_wp

  interface atan_wp
    module procedure atan_sp
    module procedure atan_dp
  end interface atan_wp

  public :: compute_dfdx_central2
  
contains

  ! abs
  elemental function abs_sp ( r ) result(d)
  real(kind = S6P), intent(in) :: r
  real(kind = S6P) :: d
    d = abs ( r )
  end function

  elemental function abs_dp ( r ) result (d)
  real(kind = D15P), intent(in) :: r
  real(kind = D15P) :: d
    d = dabs ( r ) 
  end function

  elemental function abs_csp ( r ) result(d)
  COMPLEX(kind = S6P), intent(in) :: r
  real(kind = S6P) :: d
    d = abs ( r )
  end function

  elemental function abs_cdp ( r ) result (d)
  COMPLEX(kind = D15P), intent(in) :: r
  real(kind = D15P) :: d
    d = abs ( r ) 
  end function

  ! sqrt
  pure function sqrt_sp ( r ) result(d)
    real(kind = S6P), intent(in) :: r
    real(kind = S6P) :: d
    d = sqrt ( r )
  end function

  pure function sqrt_dp ( r ) result (d)
    real(kind = D15P), intent(in) :: r
    real(kind = D15P) :: d
    d = dsqrt ( r ) 
  end function

  ! sin
  pure function sin_sp ( r ) result(d)
    real(kind = S6P), intent(in) :: r
    real(kind = S6P) :: d
    d = sin ( r )
  end function

  pure function sin_dp ( r ) result (d)
    real(kind = D15P), intent(in) :: r
    real(kind = D15P) :: d
    d = dsin ( r ) 
  end function

  ! cos
  pure function cos_sp ( r ) result(d)
    real(kind = S6P), intent(in) :: r
    real(kind = S6P) :: d
    d = cos ( r )
  end function

  pure function cos_dp ( r ) result (d)
    real(kind = D15P), intent(in) :: r
    real(kind = D15P) :: d
    d = dcos ( r ) 
  end function

  ! tanh
  pure function tanh_sp ( r ) result(d)
    real(kind = S6P), intent(in) :: r
    real(kind = S6P) :: d
    d = tanh ( r )
  end function

  pure function tanh_dp ( r ) result (d)
    real(kind = D15P), intent(in) :: r
    real(kind = D15P) :: d
    d = dtanh ( r ) 
  end function

  ! tan
  pure function tan_sp ( r ) result(d)
    real(kind = S6P), intent(in) :: r
    real(kind = S6P) :: d
    d = tan ( r )
  end function

  pure function tan_dp ( r ) result (d)
    real(kind = D15P), intent(in) :: r
    real(kind = D15P) :: d
    d = tan ( r ) 
  end function

  ! atan
  pure function atan_sp ( r ) result(d)
    real(kind = S6P), intent(in) :: r
    real(kind = S6P) :: d
    d = atan ( r )
  end function

  pure function atan_dp ( r ) result (d)
    real(kind = D15P), intent(in) :: r
    real(kind = D15P) :: d
    d = atan ( r ) 
  end function

  pure function rl(complexnumber) result(res)
    use decomp_2d_mpi, only: mytype
    implicit none
    real(mytype) :: res
    complex(mytype), intent(in) :: complexnumber
    res = real(complexnumber, kind=mytype)
  end function rl

  pure function iy(complexnumber) result(res)
    use decomp_2d_constants, only: mytype
    implicit none
    real(mytype) :: res
    complex(mytype), intent(in) :: complexnumber
    res = aimag(complexnumber)
  end function iy

  pure function cx(realpart, imaginarypart) result(res)
    use decomp_2d_constants, only: mytype
    implicit none
    complex(mytype) :: res
    real(mytype), intent(in) :: realpart, imaginarypart
    res = cmplx(realpart, imaginarypart, kind=mytype)
  end function cx

  ! Safe division with MINP check
  pure function safe_divide(numerator, denominator) result(res)
    use decomp_2d_constants, only: mytype
    real(mytype), intent(in) :: numerator, denominator
    real(mytype) :: res
    
    if (abs_prec(denominator) > MINP) then
      res = numerator / denominator
    else
      res = ZERO
    end if
  end function safe_divide

  ! heaviside_step
  pure function heaviside_step ( r ) result (d)
    real(kind = WP), intent(in) :: r
    real(kind = WP) :: d
    d = ZERO
    if (r > MINP) then  ! MINP = 1.0e-20 
      d = ONE
    else if (r < MAXN) then ! MAXN = -1.0e-20
      d = ZERO
    else 
      d = HALF
    end if
  end function

  subroutine compute_dfdx_central2(N, f, x, dfdx)
    integer, intent(in)  :: N
    real(WP), intent(in)  :: f(N), x(N)
    real(WP), intent(out) :: dfdx(N)

    integer :: i
    real(WP) :: h1, h2

    ! Forward 2nd-order difference at the first point
    h1 = x(2) - x(1)
    h2 = x(3) - x(2)
    dfdx(1) = (-h2/(h1*(h1 + h2))) * f(1) + &
              ((h2 - h1)/(h1*h2))     * f(2) + &
              (h1/(h2*(h1 + h2)))     * f(3)

    ! Centered 2nd-order difference for interior points
    do i = 2, N-1
      h1 = x(i) - x(i-1)
      h2 = x(i+1) - x(i)
      dfdx(i) = (-h2/(h1*(h1 + h2))) * f(i-1) + &
                ((h2 - h1)/(h1*h2)) * f(i)   + &
                (h1/(h2*(h1 + h2))) * f(i+1)
    end do

    ! Backward 2nd-order difference at the last point
    h1 = x(N-1) - x(N-2)
    h2 = x(N) - x(N-1)
    dfdx(N) = (-h2/(h1*(h1 + h2))) * f(N-2) + &
              ((h2 - h1)/(h1*h2)) * f(N-1) + &
              (h1/(h2*(h1 + h2))) * f(N)
    return
  end subroutine

end module math_mod
!==========================================================================================================
!==========================================================================================================
module typeconvert_mod
contains
  character(len=20) function int2str(k)
    implicit none
    integer, intent(in) :: k
    write (int2str, *) k
    int2str = trim(adjustl(int2str))
  end function int2str
  character(len=20) function real2str(r)
    use precision_mod
    implicit none
    real(wp), intent(in) :: r
    write (real2str, '(F10.4)') r
    real2str = trim(adjustl(real2str))
  end function real2str
end module typeconvert_mod

module EvenOdd_mod
  implicit none
contains
  logical function is_even(number)  
    implicit none
    integer, intent(in) :: number  
    ! Check if the number is even or odd
    if (mod(number, 2) == 0) then
        is_even = .true.
    else
        is_even = .false.
    end if
  end function
end module

!==========================================================================================================
module flatten_index_mod
 implicit none 
 
 interface flatten_index
   module procedure flatten_3d_to_1d
   module procedure flatten_2d_to_1d
 end interface
 
contains

 function flatten_3d_to_1d(i, j, k, Nx, Ny) result(n)
   integer, intent(in) :: i, j, k, Nx, Ny
   integer :: n
   n = i + Nx * (j - 1)  + Nx * Ny * (k - 1)
 end function
 
 function flatten_2d_to_1d(i, j, Nx) result(n)
   integer, intent(in) :: i, j, Nx
   integer :: n
   n = i + Nx * (j - 1)
 end function
 
end module flatten_index_mod

