!##############################################################################
module mpi_mod
  !include "mpif.h"
  use decomp_2d
  use decomp_2d_mpi
  use MPI
  !use iso_fortran_env
  implicit none
  integer :: ierror
  integer :: nxdomain
  integer :: p_row ! y-dim
  integer :: p_col ! z-dim

  public :: initialise_mpi
  public :: Finalise_mpi

contains
!==============================================================================
!> mpi initialisation.
!>
!> this initialisation is a simple one.
!  only used before calling decomp_2d_init,
!  where there is a complicted one used for 2-d decompoistion.
!  nrank = myid
!  nproc = size of processor
!  both wil be replaced after calling decomp_2d_init
!------------------------------------------------------------------------------
! Arguments
!______________________________________________________________________________.
!  mode           name          role                                           !
!______________________________________________________________________________!
!> - d (in): domain type
!==============================================================================
  subroutine initialise_mpi()
    implicit none
    call MPI_INIT(IERROR)
    call MPI_COMM_RANK(MPI_COMM_WORLD, nrank, IERROR)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, IERROR)
    return
  end subroutine initialise_mpi
!==============================================================================
!==============================================================================
  subroutine Finalise_mpi()
    implicit none
    call MPI_FINALIZE(IERROR)
    return
  end subroutine Finalise_mpi

end module mpi_mod

!==============================================================================
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
!==============================================================================
module parameters_constant_mod
  use precision_mod
  implicit none
!------------------------------------------------------------------------------
! user defined methods
!------------------------------------------------------------------------------
  logical, parameter :: is_IO_off = .false.         ! true for code performance evaluation without IO
  !logical, parameter :: is_strong_coupling = .true. ! true = RK(rhoh, g)); false = RK(rhoh) + RK(g)
  !logical, parameter :: is_drhodt_chain = .false.   ! false = (d1-d0)/dt; true = d(rhoh)/dt / (drhoh/drho)
  !logical :: is_two_potential_splitting ! true = stable solver but twice fft
  logical :: is_single_RK_projection ! true = projection only at last RK sub-step, time (o(dt^3)),
  logical :: is_damping_drhodt
  logical :: is_global_mass_correction
!------------------------------------------------------------------------------
! constants
!------------------------------------------------------------------------------
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
  real(WP), parameter :: RU_GAS      = 8.314_WP ! unit: J / (mol K), molar gas constant
!------------------------------------------------------------------------------
! fft lib
!------------------------------------------------------------------------------
  integer, parameter :: FFT_2DECOMP_3DFFT = 3, &
                        FFT_FISHPACK_2DFFT = 2, &
                        MSTRET_NONE = 0, &
                        MSTRET_3FMD = 1, &
                        MSTRET_TANH = 2, &
                        MSTRET_POWL = 3
  integer, parameter :: IPOISSON_Y_AUTO = 0, &
                        IPOISSON_Y_FFT  = 1, &
                        IPOISSON_Y_TDMA = 2
!------------------------------------------------------------------------------
! case id
!------------------------------------------------------------------------------
  integer, parameter :: ICASE_OTHERS = 0, &
                        ICASE_CHANNEL = 1, &
                        ICASE_PIPE    = 2, &
                        ICASE_ANNULAR = 3, &
                        ICASE_TGV3D   = 4, &
                        ICASE_DUCT    = 5, &
                        ICASE_TGV2D   = 6, &
                        ICASE_BURGERS = 7, &
                        ICASE_ALGTEST = 8

!------------------------------------------------------------------------------
! flow initilisation
!------------------------------------------------------------------------------
  integer, parameter :: INIT_RESTART = 0, &
                        INIT_RANDOM  = 2, &
                        INIT_INLET   = 3, &
                        INIT_GVCONST = 4, &
                        INIT_POISEUILLE = 5, &
                        INIT_FUNCTION = 6, &
                        INIT_GVBCLN = 7, &
                        INIT_GVBCSMOOTH = 8
!------------------------------------------------------------------------------
! coordinates
!------------------------------------------------------------------------------
  integer, parameter :: ICARTESIAN   = 1, &
                        ICYLINDRICAL = 2
!------------------------------------------------------------------------------
! grid stretching
!------------------------------------------------------------------------------
  integer, parameter :: ISTRET_NO     = 0, &
                        ISTRET_CENTRE = 1, &
                        ISTRET_2SIDES = 2, &
                        ISTRET_BOTTOM = 3, &
                        ISTRET_TOP    = 4, &
                        ISTRET_INPUT  = 5
!------------------------------------------------------------------------------
! time scheme
!------------------------------------------------------------------------------
  integer, parameter :: ITIME_RK3    = 3, &
                        ITIME_RK3_CN = 2, &
                        ITIME_AB2    = 1, &
                        ITIME_EULER  = 0
!------------------------------------------------------------------------------
! BC
!------------------------------------------------------------------------------
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
  ! Electrical boundary condition for the MHD electric potential, as named in the
  ! [mhd] input section. It is deliberately a separate input from the pressure BC:
  ! an insulating wall and a Neumann-pressure wall coincide in every case shipped
  ! today, but they are independent physics, and a conducting-wall case added later
  ! must not silently inherit the pressure condition. EBC_INHERIT reproduces the
  ! pre-existing behaviour and is the default when the key is absent.
  integer, parameter :: EBC_INHERIT     = -1, & ! copy the pressure BC (default)
                        EBC_INSULATING  = 1,  & ! j.n = 0     => IBC_NEUMANN on ep
                        EBC_CONDUCTING  = 2,  & ! ep = const  => IBC_DIRICHLET on ep
                        EBC_PERIODIC    = 3     !             => IBC_PERIODIC on ep
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
!------------------------------------------------------------------------------
! numerical accuracy
!------------------------------------------------------------------------------
  integer, parameter :: IACCU_CD2 = 1, &
                        IACCU_CD4 = 2, &
                        IACCU_CP4 = 3, &
                        IACCU_CP6 = 4
!------------------------------------------------------------------------------
! numerical scheme for viscous term
!------------------------------------------------------------------------------
  integer, parameter :: IVIS_EXPLICIT = 1, &
                        IVIS_SEMIMPLT = 2
!------------------------------------------------------------------------------
! LES model
!------------------------------------------------------------------------------
  integer, parameter :: ILES_NONE = 0, &
                        ILES_WALE = 1
!------------------------------------------------------------------------------
! driven force in periodic flow
!------------------------------------------------------------------------------
  integer, parameter :: IDRVF_NO         = 0, &
                        IDRVF_X_MASSFLUX = 1, &
                        IDRVF_X_TAUW     = 2, &
                        IDRVF_X_DPDX     = 3, &
                        IDRVF_Z_MASSFLUX = 4, &
                        IDRVF_Z_TAUW     = 5, &
                        IDRVF_Z_DPDZ     = 6
!------------------------------------------------------------------------------
! BC for thermal
!------------------------------------------------------------------------------
  integer, parameter :: THERMAL_BC_CONST_T  = 0, &
                        THERMAL_BC_CONST_H  = 1
!------------------------------------------------------------------------------
! working fluid media
!------------------------------------------------------------------------------
  integer, parameter :: ISCP_WATER      = 1, &
                        ISCP_CO2        = 2, &
                        ILIQUID_SODIUM  = 3, &
                        ILIQUID_LEAD    = 4, &
                        ILIQUID_BISMUTH = 5, &
                        ILIQUID_LBE     = 6, &
                        ILIQUID_WATER   = 7, & ! reserved, no property correlation exists; rejected by the input parser
                        ILIQUID_LITHIUM = 8, &
                        ILIQUID_FLIBE   = 9, &
                        ILIQUID_PBLI    = 10
!------------------------------------------------------------------------------
! statistics
!------------------------------------------------------------------------------
  integer, parameter :: ISTATL0 = 0, & ! no statistics
                        ISTATL1 = 1, & ! first moment
                        ISTATL2 = 2    ! second moment
  integer, parameter :: STAT_VISU_MODE_ALL      = 0, & ! write t_avg and tsp_avg visualised statistics
                        STAT_VISU_MODE_TSP_ONLY = 1    ! write only tsp_avg visualised statistics
  integer, parameter :: OUTPUT_POLICY_OVERWRITE = 0, &  ! overwrite existing file
                        OUTPUT_POLICY_SKIP      = 1, &  ! skip write if file exists
                        OUTPUT_POLICY_RENAME_EXISTING    = 2     ! rename existing file
  integer, parameter :: RESTART_LAYOUT_PER_FIELD = 0, & ! one restart file per field
                        RESTART_LAYOUT_BUNDLED    = 1    ! one restart bundle per field group
  integer, parameter :: RESTART_HISTORY_EXACT   = 0, & ! store/read RHS and derived-property history
                        RESTART_HISTORY_COMPACT = 1    ! rebuild history after restart startup step
  integer, parameter :: RESTART_CLOCK_CONTINUE = 0, & ! the checkpoint iteration/time is the run clock
                        RESTART_CLOCK_RESET    = 1    ! the checkpoint is an initial condition at iteration 0
  integer, parameter :: VISU_PRECISION_SINGLE = 4, & ! single precision visualisation fields
                        VISU_PRECISION_DOUBLE = 8    ! double precision visualisation fields
!------------------------------------------------------------------------------
! physical property
!------------------------------------------------------------------------------
  integer, parameter :: IPROPERTY_TABLE = 1, &
                        IPROPERTY_FUNCS = 2
!------------------------------------------------------------------------------
! database for physical property
!------------------------------------------------------------------------------
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

  ! Validity intervals of individual property correlations.
  !
  ! These are *correlation* limits and are deliberately separate from the melting
  ! and boiling temperatures above, which are *phase* limits. A fit does not hold
  ! over the whole liquid range merely because the material is liquid there.
  ! input_thermo intersects whichever of these apply to a fluid with its phase
  ! range to get the single interval the property table spans, and records which
  ! property binds each end so the diagnostics can name it.
  !
  ! PbLi dynamic viscosity (KfK-4144, see CoM_PbLi below). The expression itself
  ! is now confirmed against the primary report, but its range is not: the INL
  ! MOOSE implementation of the same expression restricts it to melting point -
  ! 625 K, while the liquid-breeder compilation that quotes the equivalent
  ! 1.87e-4*exp(1400/T) lists 521 - 900 K. The interval below is their overlap.
  ! It is an implementation policy adopted because the sources disagree -- not a
  ! universally established physical validity interval -- and 900 K is
  ! deliberately not taken silently.
  !
  ! KfK-4144 prints no validity range beside the viscosity equation, so reading
  ! it did not settle this. Two things in it bear on the question without
  ! deciding it. Its Fig. 5 (printed p. 41) plots the measured points over
  ! roughly 521 - 925 K; that is read off the published Arrhenius axis, so it is
  ! approximate, but it is at least viscosity-specific. Its section 5 (printed
  ! p. 42) says the highest temperature reached by *any* of the measurements was
  ! 933 K and proposes extrapolating the data to 1250 K; that is a statement
  ! about the whole property set, not a viscosity bound, and is not used here.
  ! Separately, 625 K is where the report's own density and conductivity
  ! measurements stop, which is the likeliest origin of the MOOSE limit -- an
  ! inference from the report, not something it states.
  real(WP), parameter :: TMUmin_PbLi = 521.0_WP ! unit: K
  real(WP), parameter :: TMUmax_PbLi = 625.0_WP ! unit: K
  ! PbLi specific heat: KfK-4144 prints this fit with an explicit range, and our
  ! coefficients reproduce it exactly (see CoCp_PbLi), so unlike the viscosity
  ! this one is a stated validity range rather than a policy. It currently binds
  ! nothing -- TM0_PbLi is already 508 K and the viscosity's 625 K cuts the top
  ! well below 800 K -- and is recorded so that it would bind if the viscosity
  ! range were ever widened.
  real(WP), parameter :: TCPmin_PbLi = 508.0_WP ! unit: K
  real(WP), parameter :: TCPmax_PbLi = 800.0_WP ! unit: K
  ! The remaining two PbLi correlations, CoD_PbLi and CoK_PbLi, do *not* match
  ! KfK-4144 (see the notes on each) and carry no other identified source, so no
  ! range is claimed for them and they are left unrestricted. Both are smooth,
  ! positive and monotonic over 508 - 1943 K, so neither is visibly
  ! extrapolating; that is an absence of evidence of a problem, not evidence of
  ! validity.

  real(WP), parameter :: HM0_Na  = 113.0e3_WP ! unit: J / Kg, latent melting heat, enthalpy for Na
  real(WP), parameter :: HM0_Pb  = 23.07e3_WP ! unit: J / Kg, latent melting heat, enthalpy for Lead
  real(WP), parameter :: HM0_BI  =  53.3e3_WP ! unit: J / Kg, latent melting heat, enthalpy for Bismuth
  real(WP), parameter :: HM0_LBE =  38.6e3_WP ! unit: J / Kg, latent melting heat, enthalpy for LBE
  real(WP), parameter :: HM0_H2O = 334.0e3_WP ! unit: J / Kg, latent melting heat, enthalpy for water
  real(WP), parameter :: HM0_Li  =  4.55e5_WP  ! unit: J / Kg, latent melting heat, enthalpy for Lithium
  real(WP), parameter :: HM0_FLiBe = 17.47e5_WP ! integral(Cp(TM0))
  real(WP), parameter :: HM0_PbLi = 33.9e3_WP ! unit: J / Kg, latent melting heat, enthalpy for PbLi-17
  ! HM0_PbLi and TM0_PbLi are both as measured in KfK-4144 (see CoM_PbLi):
  ! "Melting temperature 508 K, dHf = (33.9 +- 0.34) J/g" (printed p. 30).

  ! D = CoD(0) + CoD(1) * T
  real(WP), parameter :: CoD_Na(0:1)  = (/ 1014.0_WP,  -0.235_WP /)
  real(WP), parameter :: CoD_Pb(0:1)  = (/11441.0_WP, -1.2795_WP /)
  real(WP), parameter :: CoD_Bi(0:1)  = (/10725.0_WP,   -1.22_WP /)
  real(WP), parameter :: CoD_LBE(0:1) = (/11065.0_WP,   -1.293_WP /)
  real(WP), parameter :: CoD_Li(0:4)  = (/278.5_WP,  -0.04657_WP, 274.6_WP, 3500.0_WP, 0.467_WP /) ! D = CoD(0) + CoD(1) * T + CoD(2) * (1 - T / CoD(3))^(CoD(4))
  real(WP), parameter :: CoD_FLiBe(0:1) = (/ 2413.03_WP, -0.4884_WP /)
  real(WP), parameter :: CoD_PbLi(0:1) = (/10520.4_WP, -1.1905_WP/)
  ! CoD_PbLi is *not* the KfK-4144 density (see CoM_PbLi for the report). That
  ! report measured rho_l = 10.45 * (1 - 161e-6 * T) g/cm3 over 508 - 625 K with
  ! drho/rho = +-5% (printed p. 33), i.e. 10450 - 1.6825 * T, whose slope is 41%
  ! steeper than the -1.1905 used here; at 550 K the two differ by 3.6%, which is
  ! inside the report's own error band but is plainly a different fit. The
  ! coefficients here are therefore left unattributed and unrestricted.

  ! K = CoK(0) + CoK(1) * T + CoK(2) * T^2
  real(WP), parameter :: CoK_Na(0:2)  = (/104.0_WP,   -0.047_WP,       0.0_WP/)
  real(WP), parameter :: CoK_Pb(0:2)  = (/  9.2_WP,    0.011_WP,       0.0_WP/)
  real(WP), parameter :: CoK_Bi(0:2)  = (/ 7.34_WP,   9.5E-3_WP,       0.0_WP/)
  real(WP), parameter :: CoK_LBE(0:2) = (/3.284_WP, 1.617E-2_WP, -2.305E-6_WP/)
  real(WP), parameter :: CoK_Li(0:2)  = (/22.28_WP,   0.0500_WP, -1.243E-5_WP/)
  real(WP), parameter :: CoK_FLiBe(0:2) = (/1.1_WP,      0.0_WP,       0.0_WP/)
  real(WP), parameter :: CoK_PbLi(0:2) = (/9.148_WP, 1.963E-2_WP,      0.0_WP/)
  ! CoK_PbLi half-matches KfK-4144 (see CoM_PbLi for the report), which is why no
  ! reference is attached to it. The report measured
  ! lambda_l = 1.95e-2 + 19.6e-5 * T W/cmK over 508 - 625 K with dlambda/lambda
  ! ~ +-10% (printed p. 37), i.e. 1.95 + 1.96e-2 * T in W/mK. The *slope* agrees
  ! with 1.963e-2 to the three figures the report prints, so the two are very
  ! likely related; the *intercept* does not, 1.95 against 9.148. At 550 K that
  ! is 12.7 W/mK from the report against 19.9 W/mK here, 57% apart. The report's
  ! own value is self-consistent -- its thermal diffusivity, density and cp give
  ! 13.2 W/mK at 550 K independently -- so the gap is not a misread equation.
  ! Published PbLi conductivities genuinely scatter over roughly this range, so
  ! this is left as an open question rather than corrected here: changing it
  ! would move every PbLi thermal baseline.

  ! B = 1 / (CoB - T), which is -(1/rho)*drho/dT rewritten for a density that is
  ! linear in T, so CoB = -CoD(0) / CoD(1).
  !
  ! That identity is an internal consistency check on the density coefficients,
  ! and every linear-density fluid here satisfies it to the precision its CoB is
  ! quoted at: Pb 8941.0 vs 8942.0, Bi 8791.0 vs 8791.0, LBE 8557.6 vs 8558.0,
  ! FLiBe 4940.7 vs 4940.7, PbLi 8837.0 vs 8836.8, Na 4314.9 vs 4316.0 (the
  ! loosest, 0.03% in beta at 700 K, from the rounding of the published CoB).
  ! It is also independent evidence for the sign of CoD_LBE(1): -1.293 gives
  ! +8557.6, matching CoB_LBE, whereas +1.293 would give -8558 and a negative
  ! thermal expansion coefficient.
  real(WP), parameter :: CoB_Na = 4316.0_WP
  real(WP), parameter :: CoB_Pb = 8942.0_WP
  real(WP), parameter :: CoB_BI = 8791.0_WP
  real(WP), parameter :: CoB_LBE= 8558.0_WP
  real(WP), parameter :: CoB_Li = 5620.0_WP ! unused: CoD_Li is non-linear, so input_thermo differentiates it instead
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
  ! CoCp_PbLi is KfK-4144 exactly (see CoM_PbLi for the report): "508 <= T <=
  ! 800 K, cp = 0.195 - 9.116e-6 * T, [cp] = J/gK" (printed p. 30), which is
  ! 195.0 - 9.116e-3 * T in J/(kg K). Measured with a differential scanning
  ! calorimeter, standard deviation +-3% in the liquid state. Its stated range is
  ! carried as TCPmin_PbLi / TCPmax_PbLi above.

  ! H = HM0 + CoH(-1) * (1 / T - 1 / TM0) + CoH(0) + CoH(1) * (T - TM0) +  CoH(2) * (T^2 - TM0^2) +  CoH(3) * (T^3- TM0^3)
  !
  ! H is the term-by-term integral of the Cp polynomial above, so that the
  ! thermodynamic identity dH/dT = Cp holds exactly at every temperature. Each
  ! coefficient is therefore *derived* from CoCp rather than transcribed:
  !
  !   CoH(-1) = -CoCp(-2)      ! integral of CoCp(-2)*T^(-2) is -CoCp(-2)/T
  !   CoH( 1) =  CoCp( 0)
  !   CoH( 2) =  CoCp( 1) / 2
  !   CoH( 3) =  CoCp( 2) / 3
  !
  ! CoCp(-1), a 1/T term in Cp, would integrate to a ln(T) term that this H
  ! polynomial has no slot for; it is zero for every fluid here and must stay
  ! zero unless the H form gains that term too.
  ! CoH(0) and HM0 are the enthalpy datum. They cancel identically in the
  ! non-dimensionalisation h = (H - H0ref) / (cp0ref * T0ref) performed in
  ! input_thermo.f90, because H0ref is evaluated with this same expression, so
  ! their values affect the printed dimensional enthalpy only.
  real(WP), parameter :: CoH_Na(-1:3)  = (/-CoCp_Na(-2),    0.0_WP, CoCp_Na(0),    CoCp_Na(1)/TWO,    CoCp_Na(2)/THREE/)
  real(WP), parameter :: CoH_Pb(-1:3)  = (/-CoCp_Pb(-2),    0.0_WP, CoCp_Pb(0),    CoCp_Pb(1)/TWO,    CoCp_Pb(2)/THREE/)
  real(WP), parameter :: CoH_Bi(-1:3)  = (/-CoCp_Bi(-2),    0.0_WP, CoCp_Bi(0),    CoCp_Bi(1)/TWO,    CoCp_Bi(2)/THREE/)
  real(WP), parameter :: CoH_LBE(-1:3) = (/-CoCp_LBE(-2),   0.0_WP, CoCp_LBE(0),   CoCp_LBE(1)/TWO,   CoCp_LBE(2)/THREE/)
  real(WP), parameter :: CoH_Li(-1:3)  = (/-CoCp_Li(-2),    0.0_WP, CoCp_Li(0),    CoCp_Li(1)/TWO,    CoCp_Li(2)/THREE/)
  real(WP), parameter :: CoH_FLiBe(-1:3)=(/-CoCp_FLiBe(-2), 0.0_WP, CoCp_FLiBe(0), CoCp_FLiBe(1)/TWO, CoCp_FLiBe(2)/THREE/)
  real(WP), parameter :: CoH_PbLi(-1:3)= (/-CoCp_PbLi(-2),  0.0_WP, CoCp_PbLi(0),  CoCp_PbLi(1)/TWO,  CoCp_PbLi(2)/THREE/)

  ! M = vARies
  real(WP), parameter :: CoM_Na(-1:1) = (/556.835_WP,  -6.4406_WP, -0.3958_WP/) ! M = exp ( CoM(-1) / T + CoM(0) + CoM(1) * ln(T) )
  real(WP), parameter :: CoM_Pb(-1:1) = (/ 1069.0_WP,  4.55E-4_WP,     0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  real(WP), parameter :: CoM_Bi(-1:1) = (/  780.0_WP, 4.456E-4_WP,     0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  real(WP), parameter :: CoM_LBE(-1:1)= (/  754.1_WP,  4.94E-4_WP,     0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  real(WP), parameter :: CoM_Li(-1:1) = (/-4.164_WP, -6.374E-1_WP, 2.921e2_WP/) ! M = exp ( CoM(-1) + CoM(0) * ln(T) + (CoM(1) / T) )
  real(WP), parameter :: CoM_FLiBe(-1:1) = (/4022.0_WP, 7.803E-5_WP,   0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
  ! PbLi follows the same Arrhenius convention as Pb, Bi, LBE and FLiBe above,
  ! M = CoM(0) * exp(CoM(-1) / T), with CoM(-1) the activation temperature Ea/Ru.
  !
  ! The primary source is the measurement report, which has been read directly:
  !
  !   U. Jauch, G. Haase and B. Schulz (1986), "Thermophysical Properties in the
  !   System Li-Pb", KfK-4144, Kernforschungszentrum Karlsruhe. Part II,
  !   section 4.3, printed p. 39:
  !       "Finally for Li(17)Pb(83): eta = 0.187 * e^(11640/RT) mPas"
  !
  ! which is 1.87e-4 * exp( Ea / (Ru * T) ) Pa s with Ea = 11640 J/mol, exactly
  ! as implemented below. The report prints the activation energy's unit as
  ! J/(mol K); that is a typo, since Ea/(Ru*T) is only dimensionless if Ea is in
  ! J/mol, and the pure-lead reference it quotes alongside (Q = 8490) is the
  ! standard J/mol value. Measured with a Searle-type rotational viscosimeter in
  ! argon, calibrated against PTB standard oils; the quoted scatter on the
  ! pure-lead calibration is +-7%. The later journal paper, B. Schulz (1991),
  ! Fusion Engineering and Design 14, 199-205,
  ! doi:10.1016/0920-3796(91)90002-8, reports the same programme of work; its
  ! full text has not been read here, so nothing is claimed from it.
  !
  ! The same expression is implemented in INL MOOSE
  ! (LeadLithiumFluidProperties.C), and appears in the liquid-breeder compilation
  ! in the rounded form 1.87e-4 * exp(1400 / T); with Ru = 8.314 the activation
  ! temperature here is 11640 / 8.314 = 1400.048 K, so the two agree.
  ! Supported over TMUmin_PbLi - TMUmax_PbLi only, see the note on those above.
  !
  ! This replaces the cubic 0.0061091 - 2.2574e-5 T + 3.766e-8 T^2
  ! - 2.2887e-11 T^3 that stood here, which is the fit printed by Martelli,
  ! Venturini & Utili (2019), doi:10.1016/j.fusengdes.2018.11.028. That source is
  ! internally inconsistent: its table states the range 508 - 873 K, but the
  ! printed coefficients cross zero at 858.996 K and give M = -1.2383e-4 Pa s at
  ! 873 K, i.e. a negative viscosity inside the source's own stated range. The
  ! cubic is therefore not used, and not simply range-limited.
  real(WP), parameter :: EA_PbLi = 11640.0_WP ! unit: J / mol, KfK-4144 activation energy
  real(WP), parameter :: CoM_PbLi(-1:1) = (/EA_PbLi / RU_GAS, 1.87E-4_WP, 0.0_WP/) ! M = CoM(0) * exp (CoM(-1) / T)
end module parameters_constant_mod
!==============================================================================
module wtformat_mod
  implicit none
  private
  public :: wrtfmt1i, wrtfmt1il, wrtfmt2i
  public :: wrtfmt3i, wrtfmt4i, wrtfmt1r, wrtfmt2r, wrtfmt3r
  public :: wrtfmt1ela, wrtfmt1el, wrtfmt1e, wrtfmt2e, wrtfmt3e
  public :: wrtfmt2ae, wrtfmt2aea, wrtfmt1il1r
  public :: wrtfmt3l, wrtfmt1l, wrtfmt2s, wrtfmt3s, wrtfmt1s

  ! Named write formats used across the codebase.
  character(len=*), parameter :: wrtfmt1i    = '(2X, A56, I8)'
  character(len=*), parameter :: wrtfmt1il   = '(2X, A56, I15)'
  character(len=*), parameter :: wrtfmt2i    = '(2X, A56, 2I8)'
  character(len=*), parameter :: wrtfmt3i    = '(2X, A56, 3I8)'
  character(len=*), parameter :: wrtfmt4i    = '(2X, A56, 4I8)'
  character(len=*), parameter :: wrtfmt1ela  = '(2X, A56,   ES16.8, F9.2, A)'
  character(len=*), parameter :: wrtfmt1el   = '(2X, A56,   ES16.8)'
  character(len=*), parameter :: wrtfmt1e    = '(2X, A56,   ES16.8)'
  character(len=*), parameter :: wrtfmt2e    = '(2X, A56,  2ES16.8)'
  character(len=*), parameter :: wrtfmt3e    = '(2X, A56,  3ES16.8)'
  character(len=*), parameter :: wrtfmt2ae   = '(2X, 2(A15, ES16.8))'
  character(len=*), parameter :: wrtfmt2aea  = '(2X, 2(A15, ES16.8, F9.2, A))'
  character(len=*), parameter :: wrtfmt1r    = '(2X, A56,       F15.8)'
  character(len=*), parameter :: wrtfmt2r    = '(2X, A56,      2F15.8)'
  character(len=*), parameter :: wrtfmt3r    = '(2X, A56,      3F15.8)'
  character(len=*), parameter :: wrtfmt1il1r = '(2X, A56, I15,  F15.8)'
  character(len=*), parameter :: wrtfmt3l    = '(2X, A56, 3L4)'
  character(len=*), parameter :: wrtfmt1l    = '(2X, A56, L4)'
  character(len=*), parameter :: wrtfmt2s    = '(2X, A56, A72)'
  character(len=*), parameter :: wrtfmt3s    = '(2X, A56, 2A15)'
  character(len=*), parameter :: wrtfmt1s    = '(2X, A80)'


end module wtformat_mod
!==============================================================================
module udf_type_mod
  use mpi_mod
  use parameters_constant_mod, only: NDIM, NBC, WP, &
                                     IACCU_CD2, IACCU_CD4, IACCU_CP4, IACCU_CP6
  implicit none
!------------------------------------------------------------------------------
!  fluid thermal property info
!------------------------------------------------------------------------------
  type t_fluidThermoProperty
    real(WP) :: t  ! temperature
    real(WP) :: d  ! density
    real(WP) :: m  ! dynviscosity
    real(WP) :: k  ! thermconductivity
    real(WP) :: sigma_e ! electrical conductivity
    real(WP) :: h  ! enthalpy
    real(WP) :: rhoh ! mass enthalpy
    real(WP) :: cp ! specific heat capacity
    real(WP) :: b  ! thermal expansion
    real(WP) :: alpha ! thermal diffusivity, alpha = k / (rho * cp)
    real(WP) :: Pr ! Pr = m / (rho * alpha) = m * cp / k
  end type t_fluidThermoProperty
!------------------------------------------------------------------------------
!  parameters to calculate the fluid thermal property
!------------------------------------------------------------------------------
  type t_fluid_parameter
    character(len = 64) :: inputProperty
    integer :: ifluid
    integer :: ipropertyState
    integer :: nlist
    real(WP) :: TM0
    real(WP) :: TB0
    ! the interval the property table spans: the phase range narrowed by every
    ! property correlation that is valid over less than it, with the binding
    ! property named so the diagnostics can report what restricts the run.
    real(WP) :: TP0min ! lowest  T at which the property correlations are evaluated
    real(WP) :: TP0max ! highest T at which the property correlations are evaluated
    character(len = 64) :: TP0minsrc ! what sets TP0min
    character(len = 64) :: TP0maxsrc ! what sets TP0max
    real(WP) :: HM0
    real(WP) :: CoD(0:4)
    real(WP) :: CoK(0:2)
    real(WP) :: CoB
    real(WP) :: CoCp(-2:2)
    real(WP) :: CoH(-1:3)
    real(WP) :: CoM(-1:1)
    real(WP) :: dhmax ! undim
    real(WP) :: dhmin ! undim
    type(t_fluidThermoProperty) :: ftp0ref    ! dim, reference state
    type(t_fluidThermoProperty) :: ftpini     ! dim, initial state
  end type t_fluid_parameter
!------------------------------------------------------------------------------
!  domain info
!------------------------------------------------------------------------------
  type t_domain
    logical :: is_periodic(NDIM)       ! is this direction periodic bc?
    logical :: is_stretching(NDIM)      ! is this direction of stretching grids?
    logical :: is_compact_scheme     ! is compact scheme applied?
    logical :: is_thermo             ! is thermal field considered?
    logical :: is_conv_outlet(3)
    logical :: is_record_xoutlet
    logical :: is_read_xinlet
    logical :: reset_unit_massflux
    logical :: is_mhd
    logical :: fft_skip_c2c(3)
    integer :: existing_output_policy
    integer :: restart_data_layout_read
    integer :: restart_data_layout_write
    integer :: restart_history_mode
    integer :: restart_clock         ! how a restart maps onto the run timeline
    integer :: iteration_start       ! single run-clock origin shared by flow, thermo and mhd
    integer :: idom                  ! domain id
    integer :: icase                 ! case id
    integer :: icoordinate           ! coordinate type
    integer :: ifft_lib
    integer :: ipoisson_y_method
    integer :: LES_model
    integer :: icht
    integer :: iTimeScheme
    integer :: iviscous
    integer :: iAccuracy
    integer :: ckpt_nfre
    integer :: visu_nfre
    integer :: visu_idim
    integer :: visu_precision
    integer :: visu_nskip(NDIM)
    integer :: stat_istart
    integer :: stat_level
    integer :: stat_visu_nfre
    integer :: stat_visu_mode
    integer :: stat_nskip(NDIM)
    integer :: nsubitr
    integer :: istret, mstret
    integer :: ndbfre
    integer :: ndbbuf
    integer :: ndbend
    integer :: ndbstart
    integer :: ndb_file_offset
    logical :: xinlet_database_checked = .false.
    logical :: xinlet_database_interp_yz = .false.
    logical :: xinlet_database_warning_issued = .false.
    integer :: xinlet_database_nc(2) = 0
    integer :: xinlet_database_np(2) = 0
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
    real(WP) :: outlet_sponge_layer(2) ! outlet_sponge_layer(1) = length of sponge layer, outlet_sponge_layer(2) for min. Re_sponge (max. viscosity)

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
    type(DECOMP_INFO) :: d4cp
    type(DECOMP_INFO) :: d1cc
    type(DECOMP_INFO) :: d1pc
    type(DECOMP_INFO) :: d1cp

    type(DECOMP_INFO) :: dxcc
    type(DECOMP_INFO) :: dxpc
    type(DECOMP_INFO) :: dxcp
    type(DECOMP_INFO) :: dxcc_inl_src
    type(DECOMP_INFO) :: dxpc_inl_src
    type(DECOMP_INFO) :: dxcp_inl_src
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
    real(wp), allocatable :: xinlet_database_yp_src(:)
    real(wp), allocatable :: xinlet_database_yc_src(:)
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
    ! ur ON the pipe axis (y-pencil plane, no bc-slot index). qy = r*ur is zero
    ! at r = 0 but ur is not - the m=1 harmonic survives - so the axis node of
    ! qy/r has to be reconstructed and carried separately from the fbcy ghosts.
    real(wp), allocatable :: axisy_qyr(:, :)

    real(wp), allocatable :: fbcx_gy(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_gy(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_gy(:, :, :) ! variable bc
    !real(wp), allocatable :: fbcy_gyr(:, :, :) ! gy/r = rho * ur bc at y dirction
    !real(wp), allocatable :: fbcz_gyr(:, :, :) ! gy/r = rho * ur bc at z dirction

    real(wp), allocatable :: fbcx_qz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_qz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_qz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_qzr(:, :, :) ! qz/r bc at y dirction
    real(wp), allocatable :: fbcz_qzr(:, :, :) ! qz/r bc at z dirction

    real(wp), allocatable :: fbcx_gz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_gz(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_gz(:, :, :) ! variable bc
    !real(wp), allocatable :: fbcy_gzr(:, :, :) ! gz/r bc at y dirction
    !real(wp), allocatable :: fbcz_gzr(:, :, :) ! gz/r bc at z dirction

    real(wp), allocatable :: fbcx_pr(:, :, :) ! variable bc
    real(wp), allocatable :: fbcy_pr(:, :, :) ! variable bc
    real(wp), allocatable :: fbcz_pr(:, :, :) ! variable bc

    real(wp), allocatable :: fbcx_qw(:, :, :) ! heat flux at wall x, qw_norm >0 = heating fluid
    real(wp), allocatable :: fbcy_qw(:, :, :) ! heat flux at wall y, qw_norm <0 = cooling fluid
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
!------------------------------------------------------------------------------
!  flow info
!------------------------------------------------------------------------------
  type t_flow
    integer  :: idriven
    real(WP) :: igravity(NDIM)
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
    logical  :: is_active_tripping
    logical  :: is_compact_restart_startup

    real(wp) :: noiselevel
    real(wp) :: mcon(3)
    real(wp) :: mcon_projected(3)
    real(wp) :: tt_mass_change
    real(wp) :: total_mass
    real(wp) :: total_mass_reference
    real(wp) :: total_mass_drift
    real(wp) :: tt_kinetic_energy
    real(wp) :: physical_poisson_compatibility_defect
    real(wp) :: uniform_poisson_source_correction
    real(wp) :: poisson_projected_source_amplitude
    real(wp) :: poisson_zero_mode_rhs_projection

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
    real(WP), allocatable :: tVisc(:, :, :)
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
    ! Charge-conservation diagnostics, refreshed by check_current_conservation and
    ! exported to regression_test_metrics.json so that a broken div(j) fails a test
    ! rather than only appearing in the run log. They live here, beside the Lorentz
    ! force, because t_mhd is only allocated for an MHD run while the monitor that
    ! writes the metrics always receives t_flow.
    real(WP) :: max_div_j         ! max |div(j_vec)| over the domain
    real(WP) :: current_imbalance ! volume source + net current through the boundaries
    ! LES diagnostic, taken once on the initial field by initialise_flow_fields and
    ! never overwritten afterwards. A solid-body rotation has S_ij = 0 identically,
    ! so this is the gate on the cylindrical velocity-gradient assembly: a Cartesian
    ! tensor differentiating qy = r*u_r instead of u_r leaves S_r,theta = Omega/2.
    ! It must be the *initial* field - one RK3 step from solid-body rotation already
    ! carries O(dt) discretisation error, because the discrete centrifugal term is
    ! not exactly a discrete gradient.
    real(WP) :: max_strain_rate_mag2_init ! max S_ij S_ij over the initial field
    ! post processing - sharing
    ! Number of instantaneous fields folded into every tavg_* array below. It is
    ! counted, not derived from the iteration number: the running average divides
    ! by this, and iter - stat_istart is only equal to it when the sample stream
    ! is unbroken from stat_istart + 1. It is not, in a mixed restart (one field
    ! continued, the other injected fresh) or when a field is frozen by
    ! niterflowfirst / niterthermofirst. Checkpointed and restored with the
    ! averages themselves; see run_stats_action in post_statistics.f90.
    integer :: nstat_samples = 0
    real(WP), allocatable :: tavg_u   (:, :, :, :)  ! 3  = u, v, w
    real(WP), allocatable :: tavg_pr  (:, :, :)
    real(WP), allocatable :: tavg_pru (:, :, :, :)  ! 3  = pu, pv, pw
    real(WP), allocatable :: tavg_uu  (:, :, :, :)  ! 6  = uu, uv, uw, vv, vw, ww
    real(WP), allocatable :: tavg_uuu (:, :, :, :)  ! 10 = uuu, uuv, uuw, uvv, uvw, uww, vvv, vvw, vww, www
    real(WP), allocatable :: tavg_prdu(:, :, :, :)  ! 9  = pr * dui/dxk
    real(WP), allocatable :: tavg_dudx(:, :, :, :)  ! 9  = dui/dxj
    real(WP), allocatable :: tavg_vort(:, :, :, :)  ! 3  = vort_x, vort_r, vort_theta
    real(WP), allocatable :: tavg_vortvort(:, :, :, :)  ! 6  = vort_i * vort_j
    real(WP), allocatable :: tavg_dudu(:, :, :, :)  ! storage keeps 45 slots for future full extensions
                                                    ! current post-processing uses first 6 symmetric contracted components only
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
    real(WP), allocatable :: tavg_Tu  (:, :, :, :) ! 3 = T*u, T*v, T*w
    real(WP), allocatable :: tavg_fuuh(:, :, :, :) ! 6 = rho*uu*h, rho*uv*h, rho*uw*h, rho*vv*h, rho*vw*h, rho*ww*h
    ! One-dimensional streamwise-velocity spectra accumulated online.
    real(WP), allocatable :: spec_uu_kx(:, :) ! (kx, local y in x-pencil)
    real(WP), allocatable :: spec_uu_kz(:, :) ! (kz, local y in z-pencil)
    real(WP), allocatable :: spec_fft_wx(:)
    real(WP), allocatable :: spec_fft_wz(:)
    integer :: nspec_samples = 0
    ! First iteration folded into the spectra above. Unlike the tavg_* fields the
    ! spectra are not checkpointed, so after a restart their averaging window is
    ! shorter than stat_istart would suggest; this records the true window start
    ! so the written file says which samples it covers. See write_spectrum_uu.
    integer :: nspec_istart = 0
    !
    real(WP), allocatable :: rre_sponge_p(:)         ! vis=1/Re_sponge at centre in sponge layer
    real(WP), allocatable :: rre_sponge_c(:)         ! vis=1/Re_sponge at node in sponge layer

  end type t_flow
!------------------------------------------------------------------------------
!  thermo info
!------------------------------------------------------------------------------
  type t_thermo
    integer :: ifluid
    integer  :: inittype
    integer  :: iterfrom
    integer  :: iteration
    integer  :: nIterThermoStart
    integer  :: nIterThermoEnd
    logical  :: is_rhoh_compensated
    logical  :: is_use_qw_ramp
    integer  :: istt_qw_ramp
    integer  :: iend_qw_ramp
    real(WP) :: ref_l0  ! dim
    real(WP) :: ref_T0  ! '0' means dimensional
    real(WP) :: init_T0 ! dim
    real(WP) :: time
    real(WP) :: phy_time
    real(WP) :: rPrRen
    real(WP) :: tt_enthalpy
    real(WP) :: thermo_buffer_layer(2)

    real(WP), allocatable :: rhoh(:, :, :)
    real(WP), allocatable :: hEnth(:, :, :)
    real(WP), allocatable :: kCond(:, :, :)
    real(WP), allocatable :: eCond(:, :, :)
    real(WP), allocatable :: tTemp(:, :, :)
    ! LES subgrid turbulent Prandtl number from Kays' correlation, cell centred.
    ! Allocated only when the run is both thermal and LES; see
    ! Allocate_thermo_variables. Refreshed cell by cell in
    ! Update_thermal_properties, i.e. after the enthalpy solve and the property
    ! lookup, so it is consistent with the molecular Pr it is built from.
    real(WP), allocatable :: prSgs(:, :, :)
    real(WP), allocatable :: ene_rhs(:, :, :)  ! current step rhs
    real(WP), allocatable :: ene_rhs0(:, :, :) ! last step rhs
    real(WP), allocatable :: fbcx_rhoh_rhs0(:, :)  !
    real(WP), allocatable :: fbcz_rhoh_rhs0(:, :)  !

    ! Counted sample population of the tavg_* arrays below; see the same member
    ! of t_flow. The thermal field keeps its own count because it keeps its own
    ! clock - tm%iteration only advances while is_thermo is true.
    integer :: nstat_samples = 0
    real(WP), allocatable :: tavg_h(:, :, :)
    !real(WP), allocatable :: tavg_hh(:, :, :)
    real(WP), allocatable :: tavg_T(:, :, :)
    real(WP), allocatable :: tavg_TT(:, :, :)
    !real(WP), allocatable :: tavg_dTdT(:, :, :, :)   ! 6 = dt/dx * dt/dx, dt/dx * dt/dy, dt/dx * dt/dz, dt/dy * dt/dy, dt/dy * dt/dz, dt/dz * dt/dz

    type(t_fluidThermoProperty) :: ftp_ini ! undimensional
  end type t_thermo
  type(t_fluid_parameter) :: fluidparam ! dimensional
!------------------------------------------------------------------------------
!  mhd info
!------------------------------------------------------------------------------
  type t_mhd
    ! Default-initialised because init_stats_mhd reads iterfrom from
    ! Buildup_mpi_domain_decomposition, before initialise_mhd has run. The value
    ! is fixed in Read_input_parameters next to iteration_start, so that it does
    ! not depend on where [mhd] sits relative to [flow] in the input file.
    integer  :: iterfrom = 0
    integer  :: iteration = 0
    ! Counted sample population of the tavg_* arrays; see the same member of
    ! t_flow.
    integer  :: nstat_samples = 0
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

    ! Electrical BC as named in [mhd] (EBC_*), before it is mapped onto the IBC_*
    ! codes the operators understand. EBC_INHERIT means "copy the pressure BC".
    integer  :: ebcx_nominal(2)
    integer  :: ebcy_nominal(2)
    integer  :: ebcz_nominal(2)

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
    real(WP), allocatable :: tavg_eu(:, :, :, :) ! 3 = phi * u, phi * v, phi * w
    real(WP), allocatable :: tavg_ej(:, :, :, :) ! 3 = phi * j1 , phi * j2, phi * j3
    real(WP), allocatable :: tavg_ju(:, :, :, :) ! 9 = j1u1, j1u2, j1u3, j2u1, ..., j3u3
    real(WP), allocatable :: tavg_jj(:, :, :, :) ! 6 = jj11, jj12, jj13, jj22, jj23, jj33
  end type

contains
!==========================================================================================
!> \brief Scheme used by the pressure projection, one value per direction.
!> The three sites that make up the projection - the divergence in eq_continuity, the
!> pressure gradient in eq_momentum2 and the Poisson wavenumbers in
!> poisson_1stderivcomp_fft2d - must all call this function and must use component i for
!> direction i, or the projection stops being exact. It is a function rather than three
!> copies of a cascade precisely so they cannot drift apart.
!>
!> Why a per-direction choice is legitimate. The operator the Poisson solver inverts is a
!> separable sum, kxyz(i,j,k) = xk2(i) + yk2(j) + zk2(k) (the interpolation cross-terms in
!> `waves` sit behind ftr = .false. and never run), and each term is the SQUARE of that
!> direction's own staggered first-derivative modified wavenumber. So
!>   D.G = D_x G_x + D_y G_y + D_z G_z
!> reproduces that operator if and only if D_i G_i = L_i holds separately in each
!> direction. One scheme shared by all three directions is therefore an over-restriction,
!> not a safety margin: a direction is only obliged to match itself.
!>
!> The rule, per direction:
!>   - solved by TDMA, not FFT -> CD2. The y-TDMA path builds D_CD2.G_CD2 literally,
!>     including the stretching metric and the cylindrical area factor, so CD2 is exact
!>     there rather than merely second order.
!>   - periodic -> whatever iAccuracy asks for. A compact operator on a periodic line is
!>     circulant, the FFT diagonalises it exactly, and the modified wavenumber is exactly
!>     its eigenvalue.
!>   - non-periodic and compact -> CD4. A non-periodic direction is converted to periodic
!>     data and given the circulant symbol. An explicit scheme's boundary row is the
!>     interior stencil acting on mirror-extended ghosts, which is what the circulant
!>     symbol describes; a compact scheme's reduced tridiagonal boundary row is not, so
!>     CP4/CP6 must step down to the explicit CD4 they already fall back to at the wall.
!>
!> This does not raise the formal order of the solution. The projection gradient is also
!> the momentum pressure gradient, so a CD2 direction caps the velocity at O(h^2) there
!> whatever the other two do. What it removes is a reduction in directions that never
!> needed one - in a pipe, the axial and azimuthal directions, both periodic, previously
!> dragged down to CD2 by the radial TDMA alone.
!==========================================================================================
  pure function get_projection_accuracy(dm) result(iacc)
    type(t_domain), intent(in) :: dm
    integer :: iacc(NDIM)

    integer :: i
    logical :: is_tdma(NDIM)
!----------------------------------------------------------------------------------------
!   Only y can be taken out of the FFT; x and z are always spectral.
!   Cylindrical always reaches here with fft_skip_c2c(2) = .true. - poisson_y_method
!   auto and tdma both set it and fft is rejected outright (input_general:2193-2261) -
!   so the radial direction is caught by the TDMA branch and needs no separate test.
!----------------------------------------------------------------------------------------
    is_tdma(:) = .false.
    is_tdma(2) = dm%fft_skip_c2c(2)

    do i = 1, NDIM
      if (is_tdma(i)) then
        iacc(i) = IACCU_CD2
      else if (dm%is_periodic(i)) then
        iacc(i) = dm%iAccuracy
      else if (dm%iAccuracy == IACCU_CP4 .or. dm%iAccuracy == IACCU_CP6) then
        iacc(i) = IACCU_CD4
      else
        iacc(i) = dm%iAccuracy
      end if
    end do

  end function get_projection_accuracy

end module
!==============================================================================
!==============================================================================
module vars_df_mod
  use udf_type_mod
  implicit none

  type(t_domain), allocatable, save :: domain(:)
  type(t_flow),   allocatable, save :: flow(:)
  type(t_thermo), allocatable, save :: thermo(:)
  type(t_mhd),    allocatable, save :: mhd(:)
end module
!==============================================================================
module io_files_mod
  use parameters_constant_mod, only : is_IO_off
  implicit none
  character(8) :: dir_code='0_src'
  character(9) :: dir_data='1_data'
  character(6) :: dir_visu='2_visu'
  character(16) :: dir_visu_data='2_visu/data'
  character(16) :: dir_visu_xdmf='2_visu/xdmf'
  character(16) :: dir_visu_mesh='2_visu/mesh'
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
    if(is_IO_off) return
    call system('mkdir -p '//dir_code)
    call system('mkdir -p '//dir_data)
    call system('mkdir -p '//dir_visu)
    call system('mkdir -p '//dir_visu_data)
    call system('mkdir -p '//dir_visu_xdmf)
    call system('mkdir -p '//dir_visu_mesh)
    call system('mkdir -p '//dir_moni)
    call system('mkdir -p '//dir_chkp)
    return
  end subroutine
end module
!==============================================================================
module math_mod
  use parameters_constant_mod
  use precision_mod
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

  interface acos_wp
    module procedure acos_sp
    module procedure acos_dp
  end interface acos_wp

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

  ! acos
  pure function acos_sp ( r ) result(d)
    real(kind = S6P), intent(in) :: r
    real(kind = S6P) :: d
    d = acos ( r )
  end function

  pure function acos_dp ( r ) result (d)
    real(kind = D15P), intent(in) :: r
    real(kind = D15P) :: d
    d = dacos ( r )
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
!==============================================================================
!==============================================================================
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

!==============================================================================
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
