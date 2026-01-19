module print_msg_mod
  public :: Print_error_msg
  public :: Print_warning_msg
  public :: Print_note_msg
  public :: Print_debug_start_msg
  public :: Print_debug_mid_msg
  public :: Print_debug_end_msg
  public :: Print_3d_array
contains
!==========================================================================================================
  subroutine Print_error_msg(msg)
    !use iso_fortran_env
    implicit none
    character(len=*), intent(IN) :: msg
    
    write (*, *) '>> !ERROR! <<' // msg

    error stop  'Execution terminated unexpectedly due to an error.'

    return
  end subroutine Print_error_msg
!==========================================================================================================
  subroutine Print_warning_msg(msg)
    !use iso_fortran_env
    implicit none
    character(len=*), intent(IN) :: msg
    
    write (*, *) '>> !WARNNING! << ' // msg

    return
  end subroutine Print_warning_msg
  !==========================================================================================================
  subroutine Print_note_msg(msg)
    !use iso_fortran_env
    implicit none
    character(len=*), intent(IN) :: msg
    
    write (*, *) '  [NOTE] ' // msg

    return
  end subroutine Print_note_msg
  !==========================================================================================================
  subroutine Print_debug_start_msg(msg)
    !use iso_fortran_env
    implicit none
    character(len=*), optional, intent(IN) :: msg

    write (*, *) "=========================================================================================================="
    if(present(msg)) write (*, *) msg

    return
  end subroutine Print_debug_start_msg
!==========================================================================================================
  subroutine Print_debug_inline_msg(msg)
    !use iso_fortran_env
    implicit none
    character(len=*), intent(IN) :: msg

    write (*, *) "    "//msg
    return
  end subroutine Print_debug_inline_msg
  !==========================================================================================================
  subroutine Print_debug_mid_msg(msg)
    !use iso_fortran_env
    implicit none
    character(len=*), intent(IN) :: msg

    write (*, *) "  ------ "//msg//" ------"
    return
  end subroutine Print_debug_mid_msg
!==========================================================================================================
  subroutine Print_debug_end_msg
    !use iso_fortran_env
    implicit none

    write (*, *) "        ... done."
    return
  end subroutine Print_debug_end_msg
!==========================================================================================================
  subroutine Print_3d_array(var, nx, ny, nz, str)
    use precision_mod
    !use iso_fortran_env
    implicit none
    integer, intent(in) :: nx, ny, nz
    real(wp), intent(in) :: var(nx, ny, nz)
    character(len=*),  intent(in) :: str

    integer :: i, j, k

    write (*, *) str
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          write (*, *) k, j, i, var(i, j, k)
        end do
      end do
    end do

    return
  end subroutine Print_3d_array
end module

!==========================================================================================================
module decomp_operation_mod
  implicit none 
contains
    
  function is_same_decomp ( a, b ) result(f)
    use decomp_2d
    type(DECOMP_INFO), intent(in) :: a, b
    logical :: f
    integer :: i

    f = .true.
    do i = 1, 3
      if(a%xst(i) /= b%xst(i)) f = .false.
      if(a%xen(i) /= b%xen(i)) f = .false.
      if(a%yst(i) /= b%yst(i)) f = .false.
      if(a%yen(i) /= b%yen(i)) f = .false.
      if(a%zst(i) /= b%zst(i)) f = .false.
      if(a%zen(i) /= b%zen(i)) f = .false.
    end do
  end function
end module

!==========================================================================================================
module code_performance_mod
  use parameters_constant_mod
  use typeconvert_mod
  use mpi_mod
  use print_msg_mod
  implicit none
  
  integer, parameter :: CPU_TIME_CODE_START = 1, &
                        CPU_TIME_STEP_START = 2, &
                        CPU_TIME_ITER_START = 3, &
                        CPU_TIME_ITER_END   = 4, &
                        CPU_TIME_STEP_END   = 5, &
                        CPU_TIME_CODE_END   = 6

  real(wp), save :: t_code_start
  real(wp), save :: t_step_start
  real(wp), save :: t_iter_start
  real(wp), save :: t_iter_end
  real(wp), save :: t_step_end
  real(wp), save :: t_code_end
  integer :: cpu_nfre 

  private :: Convert_sec_to_hms
  public :: call_cpu_time

  contains

  subroutine Convert_sec_to_hms (s, hrs, mins, secs)
    real(wp), intent(in) :: s
    integer, intent(out) :: hrs
    integer, intent(out) :: mins
    real(wp), intent(out) :: secs

    secs = s

    hrs = floor(secs / SIXTY / SIXTY)
    
    secs = secs - real(hrs, WP) * SIXTY * SIXTY
    mins = floor(secs / SIXTY)

    secs = secs - real(mins, WP) * SIXTY
    return
  end subroutine 

  subroutine call_cpu_time(itype, iterfrom, niter, iter)
    integer, intent(in) :: itype
    integer, intent(in) :: iterfrom, niter
    integer, intent(in), optional :: iter
    integer :: hrs, mins
    real(wp) :: secs, t(4), t_work(4)
    real(WP) :: t_total, t_elaspsed, t_remaining, t_aveiter, t_this_iter, t_preparation, t_postprocessing
    real(WP) :: t_total0, t_elaspsed0,t_remaining0, t_aveiter0, t_this_iter0, t_preparation0, t_postprocessing0
!----------------------------------------------------------------------------------------------------------
    if(itype == CPU_TIME_CODE_START) then
      call cpu_time(t_code_start)
!----------------------------------------------------------------------------------------------------------
    else if (itype == CPU_TIME_STEP_START) then
      call cpu_time(t_step_start)
      t_preparation = t_step_start - t_code_start
      !call mpi_barrier(MPI_COMM_WORLD, ierror)
      call mpi_allreduce(t_preparation, t_preparation0, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
      if(nrank == 0) call Print_debug_mid_msg ("Code Performance Info")
      if(nrank == 0) call Print_debug_inline_msg ("    Time for code preparation : " // &
          trim(real2str(t_preparation0))//' s')
!----------------------------------------------------------------------------------------------------------
    else if (itype == CPU_TIME_ITER_START) then
      call cpu_time(t_iter_start)
      if(nrank == 0) call Print_debug_start_msg ("Time Step = "//trim(int2str(iter))// &
          '/'//trim(int2str(niter))) !trim(int2str(niter-iterfrom)))
!----------------------------------------------------------------------------------------------------------
    else if (itype == CPU_TIME_ITER_END) then
      if(.not.present(iter)) call Print_error_msg("Error in calculating CPU Time.")
      call cpu_time(t_iter_end)

      t_this_iter = t_iter_end - t_iter_start
      t_elaspsed  = t_iter_end - t_step_start
      t_aveiter   = t_elaspsed / real(iter - iterfrom, WP)
      t_remaining = t_aveiter * real(niter - iter, wp)

      t(1) = t_this_iter
      t(2) = t_elaspsed
      t(3) = t_aveiter
      t(4) = t_remaining
      call mpi_allreduce(t, t_work, 4, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
      t_this_iter0 = t_work(1)
      t_elaspsed0  = t_work(2)
      t_aveiter0   = t_work(3)
      t_remaining0 = t_work(4)

      if(nrank == 0) call Print_debug_mid_msg ("Code Performance Info")
      if(nrank == 0) call Print_debug_inline_msg ("    Time for this time step : " // &
          trim(real2str(t_this_iter0))//' s')

      call Convert_sec_to_hms (t_elaspsed0, hrs, mins, secs)
      if(nrank == 0) call Print_debug_inline_msg ("    Elaspsed Wallclock Time : "// &
           trim(int2str(hrs)) // ' h ' // &
           trim(int2str(mins)) // ' m ' // &
           trim(real2str(secs)) // ' s ')

      call Convert_sec_to_hms (t_remaining0, hrs, mins, secs)
      if(nrank == 0) then
        call Print_debug_inline_msg ("    Remaning Wallclock Time : "// &
           trim(int2str(hrs)) // ' h ' // &
           trim(int2str(mins)) // ' m ' // &
           trim(real2str(secs)) // ' s ')
        call Print_debug_inline_msg ("    Moving averaged time per iteration  : "// &
           trim(real2str(t_aveiter0))//' s')
      !if(nrank == 0) call Print_debug_mid_msg ("Code Performance Info")  
      end if
!----------------------------------------------------------------------------------------------------------
    else if (itype == CPU_TIME_STEP_END) then

      call cpu_time(t_step_end)
      t_total = t_step_end - t_step_start
      t_aveiter= t_total / real(niter - iterfrom, WP)
      !call mpi_barrier(MPI_COMM_WORLD, ierror)

      t = ZERO
      t(1) = t_total
      t(2) = t_aveiter
      call mpi_allreduce(t, t_work, 4, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
      t_total0   = t_work(1)
      t_aveiter0 = t_work(2)

      call Convert_sec_to_hms (t_total0, hrs, mins, secs)
      if(nrank == 0) then
        call Print_debug_mid_msg ("Code Performance Info")
        call Print_debug_inline_msg   ("    Averaged time per iteration  : "// &
           trim(real2str(t_aveiter0))//' s')
        call Print_debug_inline_msg ("    Wallclock time of all iterations : "// &
           trim(int2str(hrs)) // ' h ' // &
           trim(int2str(mins)) // ' m ' // &
           trim(real2str(secs)) // ' s ')
      end if
!----------------------------------------------------------------------------------------------------------
    else if (itype == CPU_TIME_CODE_END) then

      call cpu_time(t_code_end)
      t_total  = t_code_end - t_code_start
      t_postprocessing = t_code_end - t_step_end

      t = ZERO
      t(1) = t_total
      t(2) = t_postprocessing
      call mpi_allreduce(t, t_work, 4, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
      t_total0          = t_work(1)
      t_postprocessing0 = t_work(2)

      call Convert_sec_to_hms (t_total0, hrs, mins, secs)
      if(nrank == 0) then
        call Print_debug_mid_msg ("Code Performance Info")
        call Print_debug_inline_msg    ("    Wallclock time for postprocessing : "// &
           trim(real2str(t_postprocessing0))//' s')
        call Print_debug_inline_msg ("    Total wallclock time of this run : "// &
           trim(int2str(hrs)) // ' h ' // &
           trim(int2str(mins)) // ' m ' // &
           trim(real2str(secs)) // ' s ')
        call Print_debug_inline_msg("CHAPSim Simulation is finished successfully.")
      end if
    else
    end if

    return
  end subroutine

end module


module cubic_spline_interpolation


  public :: cubic_spline
  public :: spline_interpolation


contains

  !**********************************************************************************************************************************
  subroutine cubic_spline (n, x, y, b, c, d)
  !---------------------------------------------------------------------
  !     this subroutine calculates the coefficients b, c, d of a cubic
  !     spline to best approximate a discreet fonction given by n points
  !
  !     inputs:
  !     n       number of given points
  !     x, y    vectors of dimension n, storing the coordinates
  !             of function f(x)
  !
  !     outputs:
  !     b,c, d   vectors of dimension n, storing the coefficients
  !             of the cubic spline
  !     function:
  !     y =  x
  !     reference:
  !     forsythe, g.e. (1977) computer methods for mathematical
  !     computations. prentice - hall, inc.
  !---------------------------------------------------------------------
      use precision_mod
      implicit none
      integer(4), intent(in) :: n
      real(wp), intent(in) :: x(n), y(n)
      real(wp), intent(out) :: b(n), c(n), d(n)

      integer(4) :: nm1, i, l
      real(wp) :: t


      if (n < 2) return
      if (n < 3) then
          b(1) = (y(2) - y(1)) / (x(2) - x(1))
          c(1) = 0.0_wp
          d(1) = 0.0_wp
          b(2) = b(1)
          c(2) = 0.0_wp
          d(2) = 0.0_wp
          return
      end if

      ! step 1: preparation
      !        build the tridiagonal system
      !        b (diagonal), d (upperdiagonal), c (second member)
      nm1 = n - 1

      d(1) = x(2) - x(1)
      c(2) = (y(2) - y(1)) / d(1)
      do i = 2, nm1
          d(i) = x(i + 1) - x(i)
          b(i) = 2.0_wp * (d(i - 1) + d(i))
          c(i + 1) = (y(i + 1) - y(i)) / d(i)
          c(i) = c(i + 1) - c(i)
      end do

      ! step 2: end conditions
      !     conditions at limits
      !     third derivatives obtained by divided differences
      b(1) = - d(1)
      b(n) = - d(n - 1)
      c(1) = 0.0_wp
      c(n) = 0.0_wp

      if(n /= 3) then
          c(1) = c(3) / (x(4) - x(2)) - c(2) / (x(3) - x(1))
          c(n) = c(n - 1) / (x(n) - x(n - 2)) - c(n - 2) / (x(n - 1) - x(n - 3))
          c(1) = c(1) * d(1) * d(1) / (x(4) - x(1))
          c(n) = - c(n) * d(n - 1)**2 / (x(n) - x(n - 3))
      end if

      ! step 3:     forward elimination
      do i = 2, n
          t = d(i - 1) / b(i - 1)
          b(i) = b(i) - t * d(i - 1)
          c(i) = c(i) - t * c(i - 1)
      end do

      !step 4:     back substitution
      c(n) = c(n) / b(n)
      do  l = 1, nm1
          i = n - l
          c(i) = (c(i) - d(i) * c(i + 1)) / b(i)
      end do

      !step 5: coefficients of 3rd degree polynomial
      b(n) = (y(n) - y(nm1)) / d(nm1) + d(nm1) * (c(nm1) + 2.0_wp * c(n))
      do  i = 1, nm1
          b(i) = (y(i + 1) - y(i)) / d(i) - d(i) * (c(i + 1) + 2.0_wp * c(i))
          d(i) = (c(i + 1) -c(i)) / d(i)
          c(i) = 3.0_wp * c(i)
      end do
      c(n) = 3.0_wp * c(n)
      d(n) = d(nm1)

      return
  end subroutine

  !**********************************************************************************************************************************
  function spline_interpolation(n, yprofile, b, c, d, y) result(eval)
    use precision_mod
    implicit none
    integer, intent(in) :: n
    real(WP), intent(in) :: yprofile(n)
    real(wp), intent(in) :: b(n), c(n), d(n)
    real(WP), intent(in) :: y
    real(WP) :: eval
    
    integer :: i, j, k
    real(WP) :: dy


    !*
    !  binary search for for i, such that x(i) <= u <= x(i + 1)
    !*
    i = 1
    j = n + 1
    do while (j > i + 1)
        k = (i + j) / 2
        if(y < yprofile(k)) then
            j = k
        else
            i = k
        end if
    end do
    !*
    !  evaluate spline interpolation
    !*
    dy = y - yprofile(i)
    eval = yprofile(i) + dy * (b(i) + dy * (c(i) + dy * d(i)))

  end function

end module


!==========================================================================================================

!==========================================================================================================
module random_number_generation_mod
  use precision_mod
  implicit none
  private
  public :: initialise_random_number
  public :: Generate_rvec_random
  public :: Generate_r_random

contains
  subroutine initialise_random_number ( seed )
    !*******************************************************************************
    !
    !! random_initialise initialises the FORTRAN 90 random number seed.
    !
    !
    !  Discussion:
    !
    !    If you don't initialise the random number generator, its behavior
    !    is not specified.  If you initialise it simply by:
    !
    !      CALL random_seed
    !
    !    its behavior is not specified.  On the DEC ALPHA, If that's all you
    !    do, the same random number sequence is returned.  In order to actually
    !    try to scramble up the random number generator a bit, this routine
    !    goes through the tedious process of getting the size of the random
    !    number seed, making up values based on the current time, and setting
    !    the random number seed.
    !
    !  Modified:
    !
    !    19 December 2001
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  parameters:
    !
    !    Input/output, integer seed.
    !    IF seed is zero on input, THEN you're asking this routine to come up
    !    with a seed value, whICh is RETURNed as output.
    !    IF seed is nonzero on input, THEN you're asking this routine to
    !    USE the input value of seed to initialise the random number generator,
    !    and seed is not changed on output.
    !
    implicit none
    !
    integer :: count
    integer :: count_max
    integer :: count_rate
    logical, parameter :: debug = .false.
    integer :: i
    integer :: seed
    integer, allocatable :: seed_vector(:)
    integer :: seed_size
    real(wp) :: t
    !
    !  initialise the random number seed.
    !
    call random_seed
    !
    !  determine the size of the random number seed.
    !
    call random_seed ( size = seed_size )
    !
    !  allocate a seed of the right size.
    !
    allocate ( seed_vector(seed_size) ); seed_vector = 0

    if ( seed /= 0 ) then

        if ( debug ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'random_initialise'
            write ( *, '(a, i20)' ) '  initialise random_number, user seed = ', seed
        end if

    else

        call system_clock ( count, count_rate, count_max )

        seed = count

        if ( debug ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'random_initialise'
            write ( *, '(a, i20)' ) '  initialise random_number, arbitrary seed = ', &
            seed
        end if

    end if
    !
    !  now set the seed.
    !
    seed_vector(1:seed_size) = seed

    call random_seed ( put = seed_vector(1:seed_size) )
    !
    !  free up the seed space.
    !
    deallocate ( seed_vector )
    !
    !  call the random number routine a bunch of times.
    !random_initialise
    do i = 1, 100
        call random_number ( harvest = t )
    end do

    return
  end subroutine initialise_random_number

  !**********************************************************************************************************************************
  subroutine Generate_rvec_random ( alo, ahi, n, a )
    !
    !*******************************************************************************
    !
    !! RVEC_random RETURNs a random REAL(WP) vector in a given range.
    !
    !
    !  ModIFied:
    !
    !    04 Februray 2001
    !
    !  Author:
    !
    !    John BurkARdt
    !
    !  parameters:
    !
    !    Input, REAL(WP) ALO, AHI, the range allowed for the entries.
    !
    !    Input, integer N, the number of entries in the vector.
    !
    !    Output, REAL(WP) A(N), the vector of randomly chosen values.
    !
    implicit none
    !
    integer n
    !
    real(wp) a(n)
    real(wp) ahi
    real(wp) alo
    integer i
    !
    do i = 1, n
        call Generate_r_random ( alo, ahi, a(i) )
    end do

    return
  end subroutine Generate_rvec_random

!**********************************************************************************************************************************
  subroutine Generate_r_random ( rlo, rhi, r )
    !
    !*******************************************************************************
    !
    !! R_random RETURNs a random REAL(WP) in a given range.
    !
    !
    !  ModIFied:
    !
    !    06 April 2001
    !
    !  Author:
    !
    !    John BurkARdt
    !
    !  parameters:
    !
    !    Input, REAL(WP) RLO, RHI, the minimum and maximum values.
    !
    !    Output, REAL(WP) R, the randomly chosen value.
    !
    implicit none
    !
    real(wp) :: r
    real(wp) :: rhi
    real(wp) :: rlo
    real(wp) :: t
    !
    !  pick t, a random number in (0, 1).
    !
    call random_number ( harvest = t )
    !
    !  set r in ( rlo, rhi ).
    !
    r = ( 1.0e+00 - t ) * rlo + t * rhi

    return
  end subroutine Generate_r_random
  
end module random_number_generation_mod

!module index_mod
  !public :: which_pencil
  !public :: local2global_3indices
  !public :: local2global_yid

  !contains 
!==========================================================================================================
!   function which_pencil(dtmp) result(a)
!     use parameters_constant_mod
!     use decomp_2d
!     use print_msg_mod
!     implicit none
!     type(DECOMP_INFO), intent(in) :: dtmp
!     integer :: a

! ! this is wrong as it prefers the order of X, Y, Z
!     if(dtmp%xst(1) == 1 .and. dtmp%xsz(1) == dtmp%xen(1)) then
!       a = IPENCIL(1)
!     else if(dtmp%yst(2) == 1 .and. dtmp%ysz(2) == dtmp%yen(2)) then 
!       a = IPENCIL(2)
!     else if(dtmp%zst(3) == 1 .and. dtmp%zsz(3) == dtmp%zen(3)) then 
!       a = IPENCIL(3)
!     else
!       call Print_error_msg("Error in finding which pencil.")
!     end if

!   end function

!==========================================================================================================
  ! function local2global_3indices(a, dtmp) result(b)
  !   use decomp_2d
  !   use parameters_constant_mod
  !   use print_msg_mod
  !   implicit none
  !   type(DECOMP_INFO), intent(in) :: dtmp
  !   integer, intent(in)  :: a(3)
  !   integer :: b(3)

  !   if(which_pencil(dtmp) == IPENCIL(1)) then
  !     b(1) = dtmp%xst(1) + a(1) - 1
  !     b(2) = dtmp%xst(2) + a(2) - 1
  !     b(3) = dtmp%xst(3) + a(3) - 1
  !   else if (which_pencil(dtmp) == IPENCIL(2)) then
  !     b(1) = dtmp%yst(1) + a(1) - 1
  !     b(2) = dtmp%yst(2) + a(2) - 1
  !     b(3) = dtmp%yst(3) + a(3) - 1
  !   else if (which_pencil(dtmp) == IPENCIL(3)) then
  !     b(1) = dtmp%zst(1) + a(1) - 1
  !     b(2) = dtmp%zst(2) + a(2) - 1
  !     b(3) = dtmp%zst(3) + a(3) - 1
  !   else 
  !     call Print_error_msg("Error in local to global index conversion.")
  !   end if

  ! end function

  ! function local2global_yid(a, dtmp) result(b)
  !   use decomp_2d
  !   use parameters_constant_mod
  !   use print_msg_mod
  !   implicit none
  !   type(DECOMP_INFO), intent(in) :: dtmp
  !   integer, intent(in)  :: a
  !   integer :: b

  !   if(which_pencil(dtmp) == IPENCIL(1)) then
  !     b = dtmp%xst(2) + a - 1
  !   else if (which_pencil(dtmp) == IPENCIL(2)) then
  !     b = dtmp%yst(2) + a - 1
  !   else if (which_pencil(dtmp) == IPENCIL(3)) then
  !     b = dtmp%zst(2) + a - 1
  !   else 
  !     call Print_error_msg("Error in local to global index conversion.")
  !   end if

  ! end function
  

!end module 


!==========================================================================================================
module wrt_debug_field_mod
  public :: wrt_3d_all_debug
  public :: wrt_3d_pt_debug
contains
  subroutine wrt_3d_pt_debug(var, dtmp, iter, irk, loc)
    use precision_mod
    use udf_type_mod
    use print_msg_mod
    use io_files_mod
    
    implicit none 
    type(DECOMP_INFO), intent(in) :: dtmp
    real(wp), intent(in)     :: var(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3))
    character(*), intent(in) :: loc
    
    integer, intent(in) :: iter, irk

    integer, parameter :: npt = 8
    integer, parameter :: nfil = 20
    integer  :: nid(8, 3), a(24)

    character(1) :: pntim
    character(128) :: flnm
    integer :: n, i, j, k, jj, kk

  ! based on x pencil

    a = (/1, 1, 1, 1, 8, 8, 8, 8, &
          1, 2, 3, 4, 1, 2, 3, 4, &
          1, 1, 1, 1, 8, 8, 8, 8/)
    nid = reshape(a, (/8, 3/))
    do n = 1, npt
        write(pntim,'(i1.1)') n
        flnm = 'chapsim2_p'//pntim//'.dat'   
        do k =1, dtmp%xsz(3)
            kk = dtmp%xst(3) + k - 1
            if(kk == nid(n, 3)) then
                do j = 1, dtmp%xsz(2)
                    jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
                    if(jj == nid(n, 2)) then
                      do i = 1, dtmp%xsz(1)
                          if(i == nid(n, 1)) then
                            if(file_exists(trim(adjustl(flnm)))) then
                              open(nfil+n,file=trim(adjustl(flnm)), position='append')
                              !write(nfil+n,*) '# iter = ', iter
                            else
                              open(nfil+n,file=trim(adjustl(flnm)) )
                              !write(nfil+n,*) '# iter = ', iter
                            end if
                            write(nfil+n, '(A20, 2I2.1, 3I4.1, 1ES27.19)') &
                            trim(loc), iter, irk, i, jj, kk, var(i, j, k)
                            close(nfil+n)
                          end if
                        end do
                    end if
                end do
            end if
        end do
      end do

  return

  end subroutine

  !==========================================================================================================
  subroutine wrt_3d_all_debug(var, dtmp, iter, str, loc)
    use precision_mod
    use udf_type_mod
    use print_msg_mod
    use io_files_mod
    
    implicit none 
    type(DECOMP_INFO), intent(in) :: dtmp
    real(wp), intent(in)     :: var(dtmp%xsz(1), dtmp%xsz(2), dtmp%xsz(3))
    character(*), intent(in) :: str
    character(*), intent(in) :: loc
    
    integer, intent(in) :: iter
    integer, parameter :: nfil = 20

    character(128) :: flnm
    integer :: i, j, k, jj, kk
    character(1) :: pntim

    write(pntim,'(i1.1)') nrank
    flnm = 'chapsim2_'//trim(str)//'_at_'//trim(loc)//'_myid'//pntim//'.dat'  
    if(file_exists(trim(adjustl(flnm)))) then
      open(nfil,file=trim(adjustl(flnm)), position='append')
      write(nfil,*) '# iter = ', iter
    else
      open(nfil,file=trim(adjustl(flnm)) )
      write(nfil,*) '# iter = ', iter
    end if

    do j = 1, dtmp%xsz(2)
      jj = dtmp%xst(2) + j - 1 !local2global_yid(j, dtmp)
      do k =1, dtmp%xsz(3)
        kk = dtmp%xst(3) + k - 1
        do i = 1, dtmp%xsz(1)
          write(nfil, '(3I4.1, 1ES27.19)') i, jj, kk, var(i, j, k)  
        end do
      end do
    end do
    close(nfil)

    return
  end subroutine

end module 
!============================================================================
module transpose_extended_mod
  use decomp_2d
  implicit none 
  
  public :: transpose_to_y_pencil
  public :: transpose_to_z_pencil
  public :: transpose_from_z_pencil
  public :: transpose_from_y_pencil
  public :: get_dimensions

contains
  ! Helper subroutine: Get dimensions based on pencil
  subroutine get_dimensions(dtmp, pencil, nx, ny, nz, nyst)
    use parameters_constant_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    integer, intent(in)          :: pencil
    integer, intent(out)         :: nx, ny, nz, nyst

    select case (pencil)
      case (IPENCIL(1))
        nx = dtmp%xsz(1)
        ny = dtmp%xsz(2)
        nz = dtmp%xsz(3)
        nyst = dtmp%xst(2)
      case (IPENCIL(2))
        nx = dtmp%ysz(1)
        ny = dtmp%ysz(2)
        nz = dtmp%ysz(3)
        nyst = dtmp%yst(2)
      case (IPENCIL(3))
        nx = dtmp%zsz(1)
        ny = dtmp%zsz(2)
        nz = dtmp%zsz(3)
        nyst = dtmp%zst(2)
      case default
        nx = 0
        ny = 0
        nz = 0
        nyst = 0
    end select
  end subroutine get_dimensions
!============================================================================
  ! Helper subroutine: Transpose input data to y-pencil
!============================================================================
  subroutine transpose_to_y_pencil(var, var_ypencil, dtmp, pencil)
    use parameters_constant_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), intent(in)         :: var(:, :, :)
    real(WP), intent(out)        :: var_ypencil(:, :, :)
    integer, intent(in)          :: pencil

    select case (pencil)
      case (IPENCIL(1))
        call transpose_x_to_y(var, var_ypencil, dtmp)
      case (IPENCIL(2))
        var_ypencil = var
      case (IPENCIL(3))
        call transpose_z_to_y(var, var_ypencil, dtmp)
      case default
        ! Handle invalid pencil case (optional: add error handling)
    end select
  end subroutine transpose_to_y_pencil
  !============================================================================
  ! Helper subroutine: Transpose input data to z-pencil
  !============================================================================
  subroutine transpose_to_z_pencil(var, var_zpencil, dtmp, pencil)
    use parameters_constant_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), intent(in)         :: var(:, :, :)
    real(WP), intent(out)        :: var_zpencil(:, :, :)
    integer, intent(in)          :: pencil

    real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: var_ypencil

    select case (pencil)
      case (IPENCIL(1))
        call transpose_x_to_y(var, var_ypencil, dtmp)
        call transpose_y_to_z(var_ypencil, var_zpencil, dtmp)
      case (IPENCIL(2))
        call transpose_y_to_z(var, var_zpencil, dtmp)
      case (IPENCIL(3))
        var_zpencil = var
      case default
        ! Handle invalid pencil case (optional: add error handling)
    end select
  end subroutine transpose_to_z_pencil
  !============================================================================
  ! Helper subroutine: Transpose data from z-pencil back to original pencil
  !============================================================================
  subroutine transpose_from_z_pencil(var_zpencil, var, dtmp, pencil)
    use parameters_constant_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), intent(in)         :: var_zpencil(:, :, :)
    real(WP), intent(out)        :: var(:, :, :)
    integer, intent(in)          :: pencil

    real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: var_ypencil

    select case (pencil)
      case (IPENCIL(1))
        call transpose_z_to_y(var_zpencil, var_ypencil, dtmp)
        call transpose_y_to_x(var_ypencil, var, dtmp)
      case (IPENCIL(2))
        call transpose_z_to_y(var_zpencil, var, dtmp)
      case (IPENCIL(3))
        var = var_zpencil
      case default
        ! Handle invalid pencil case (optional: add error handling)
    end select
  end subroutine transpose_from_z_pencil

  !============================================================================
  ! Helper subroutine: Transpose data from y-pencil back to original pencil
  !============================================================================
  subroutine transpose_from_y_pencil(var_ypencil, var, dtmp, pencil)
    use parameters_constant_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), intent(in)         :: var_ypencil(:, :, :)
    real(WP), intent(out)        :: var(:, :, :)
    integer, intent(in)          :: pencil

    select case (pencil)
      case (IPENCIL(1))
        call transpose_y_to_x(var_ypencil, var, dtmp)
      case (IPENCIL(2))
        var = var_ypencil
      case (IPENCIL(3))
        call transpose_y_to_z(var_ypencil, var, dtmp)
      case default
        ! Handle invalid pencil case (optional: add error handling)
    end select
  end subroutine transpose_from_y_pencil

end module

!============================================================================
module decomp_extended_mod
  use parameters_constant_mod
  implicit none


  public :: ypencil_index_lgl2ggl
  public :: zpencil_index_llg2ggg
  public :: zpencil_index_ggg2llg

  contains
!==========================================================================================================
  subroutine ypencil_index_lgl2ggl(vin, vou, dtmp)
    use decomp_2d
    implicit none

    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), dimension(dtmp%ysz(1),               dtmp%ysz(2), dtmp%ysz(3)), intent(in)  :: vin
    real(WP), dimension(dtmp%yst(1) : dtmp%yen(2), dtmp%ysz(2), dtmp%zsz(3)), intent(out) :: vou

    integer :: i, j, k, ii
    vou = ZERO
    do k = 1, dtmp%ysz(3)
      do j = 1, dtmp%ysz(2)
        do i = 1, dtmp%ysz(1)
          ii = dtmp%yst(1) + i - 1
          vou(ii, j, k) = vin(i, j, k)
        end do
      end do
    end do
    return
  end subroutine 
!==========================================================================================================
  subroutine zpencil_index_llg2ggg(vin, vou, dtmp)
    use decomp_2d
    
    implicit none

    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), dimension(dtmp%zsz(1),               dtmp%zsz(2),               dtmp%zsz(3)), intent(in)  :: vin
    real(WP), dimension(dtmp%zst(1) : dtmp%zen(1), dtmp%zst(2) : dtmp%zen(2), dtmp%zsz(3)), intent(out) :: vou

    integer :: i, j, k, jj, ii

    vou = ZERO
    do k = 1, dtmp%zsz(3)
      do j = 1, dtmp%zsz(2)
        jj = dtmp%zst(2) + j - 1 !local2global_yid(j, dtmp)
        do i = 1, dtmp%zsz(1)
          ii = dtmp%zst(1) + i - 1
          vou(ii, jj, k) = vin(i, j, k)
        end do
      end do
    end do
    return
  end subroutine
!==========================================================================================================  
  subroutine zpencil_index_ggg2llg(vin, vou, dtmp)
    use decomp_2d
    
    implicit none

    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), dimension(dtmp%zst(1) : dtmp%zen(1), dtmp%zst(2) : dtmp%zen(2), dtmp%zsz(3)), intent(in)   :: vin
    real(WP), dimension(dtmp%zsz(1),               dtmp%zsz(2),               dtmp%zsz(3)), intent(out)  :: vou
    

    integer :: i, j, k, jj, ii
!write(*,*) 'vin', nrank, size(vin, 1), size(vin, 2),size(vin, 3)
!write(*,*) 'vou', nrank, size(vou, 1), size(vou, 2),size(vou, 3)

    vou = ZERO
    do k = 1, dtmp%zsz(3)
      do j = 1, dtmp%zsz(2)
        jj = dtmp%zst(2) + j - 1 !local2global_yid(j, dtmp)
        do i = 1, dtmp%zsz(1)
          ii = dtmp%zst(1) + i - 1
          vou(i, j, k) = vin(ii, jj, k)
        end do
      end do
    end do
    return
  end subroutine
end module 

!============================================================================
!============================================================================
module cylindrical_rn_mod
  use udf_type_mod
  use parameters_constant_mod
  use print_msg_mod
  use transpose_extended_mod
  implicit none

  public :: axis_estimating_radial_xpx
  !public :: estimate_azimuthal_xpx_on_axis
  public :: multiple_cylindrical_rn
  public :: multiple_cylindrical_rn_xx4
  public :: multiple_cylindrical_rn_x4x

contains

  !============================================================================
  ! Estimate azimuthal component on the axis
  !============================================================================
  ! subroutine estimate_azimuthal_xpx_on_axis(var, dtmp, pencil, dm)
  !   implicit none
  !   type(DECOMP_INFO), intent(in) :: dtmp
  !   type(t_domain), intent(in)    :: dm ! not used
  !   real(WP), intent(inout)      :: var(:, :, :)
  !   integer, intent(in)          :: pencil

  !   real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: var_ypencil
  !   real(WP), dimension(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)) :: var_zpencil

  !   if (dm%icase /= ICASE_PIPE) return

  !   ! Transpose input data to z-pencil
  !   call transpose_to_z_pencil(var, var_zpencil, dtmp, pencil)

  !   ! Set the value on the axis to zero
  !   var_zpencil(:, :, 1) = ZERO

  !   ! Transpose back to the original pencil
  !   call transpose_from_z_pencil(var_zpencil, var, dtmp, pencil)

  ! end subroutine estimate_azimuthal_xpx_on_axis

  !============================================================================
  ! Estimate radial component on the axis
  !============================================================================
  subroutine axis_estimating_radial_xpx(var, dtmp, pencil, dm, idir, is_reversed)
    use math_mod
    use transpose_extended_mod
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    type(t_domain), intent(in)    :: dm
    real(WP), intent(inout)       :: var(:, :, :)
    integer, intent(in)           :: pencil
    logical, optional, intent(in) :: is_reversed
    integer, intent(in)           :: idir

    real(WP), dimension(dtmp%ysz(1), dtmp%ysz(2), dtmp%ysz(3)) :: var_ypencil, var_ypencil1
    real(WP), dimension(dtmp%zsz(1), dtmp%zsz(2), dtmp%zsz(3)) :: var_zpencil, var_zpencil1
    !real(WP), dimension(dtmp%zsz(1)) :: uz, uy
    integer :: k, i
    real(WP) :: theta, sign

    ! only for axis value
    if (dm%icase /= ICASE_PIPE .or. dm%icoordinate /= ICYLINDRICAL) return
    !
    if(idir == IDIM(3)) then ! for qz
      call transpose_to_y_pencil(var, var_ypencil, dtmp, pencil)
      var_ypencil(:, 1, :) = ZERO ! zero qz at axis
      call transpose_from_y_pencil(var_ypencil, var, dtmp, pencil)
    else if(idir == IDIM(1)) then ! for ux, or scalars
      call transpose_to_z_pencil(var, var_zpencil, dtmp, pencil)
      if(dtmp%zst(2) == 1) then  ! for axis jj == 1 only
        do i = 1, dtmp%zsz(1)
          theta = ZERO
          do k = 1, dtmp%zsz(3)
            theta = theta + var_zpencil(i, 1, k)
          end do
          var_zpencil(i, 1, :) = theta / real(dtmp%zsz(3), WP)
        end do 
      end if
      call transpose_from_z_pencil(var_zpencil, var, dtmp, pencil)
    else if(idir == IDIM(2) .or. idir == IDIM(0)) then
      call transpose_to_y_pencil(var, var_ypencil, dtmp, pencil)
      call transpose_to_z_pencil(var, var_zpencil, dtmp, pencil)
      ! Assign a sign
      sign = ONE
      if (present(is_reversed)) then
        if(is_reversed) sign = - ONE
      end if
      ! Apply symmetry condition to find neighboring points
      do k = 1, dtmp%zsz(3)
        var_zpencil1(:, :, k) = sign * var_zpencil(:, :, dm%knc_sym(k))
      end do
      ! Transpose back to y-pencil and get the multiple valued ur at axis
      call transpose_z_to_y(var_zpencil1, var_ypencil1, dtmp)
      var_ypencil(:, 1, :) = (var_ypencil1(:, 2, :) + var_ypencil(:, 2, :)) * HALF
      ! Transpose to z-pencil for decomposition
      !call transpose_y_to_z(var_ypencil, var_zpencil, dtmp)
      ! below is eq(83) & (76) of https://doi.org/10.1016/j.jcp.2003.12.015 (Morinishi2004JCP)
      ! coorindates like: https://en.m.wikipedia.org/wiki/File:3D_coordinate_system.svg
      ! if(dtmp%zst(2) == 1) then ! for axis jj == 1 only
      !   if(idir == IDIM(2)) then ! for qy/r
      !     do i = 1, dtmp%zsz(1)
      !       uy(i) = ZERO
      !       uz(i) = ZERO
      !       do k = 1, dtmp%zsz(3)
      !         theta = dm%h(3) * real((k-1), WP)
      !         uz(i) = uz(i) + var_zpencil1(i, 1, k) * cos_wp(theta)
      !         uy(i) = uy(i) + var_zpencil1(i, 1, k) * sin_wp(theta)
      !       end do
      !       uy(i) = uy(i) * TWO / dtmp%zsz(3)
      !       uz(i) = uz(i) * TWO / dtmp%zsz(3)
      !       do k = 1, dtmp%zsz(3)
      !         theta = dm%h(3) * real((k-1), WP)
      !         var_zpencil1(i, 1, k) = uz(i) * cos_wp(theta) + uy(i) * sin_wp(theta)
      !       end do
      !     end do
      !   end if

      !   if(idir == IDIM(0)) then ! for qz/r only
      !     do i = 1, dtmp%zsz(1)
      !       uy(i) = ZERO
      !       uz(i) = ZERO
      !       do k = 1, dtmp%zsz(3)
      !         theta = dm%h(3) * real((k-1), WP)
      !         uz(i) = uz(i) - var_zpencil1(i, 1, k) * sin_wp(theta)
      !         uy(i) = uy(i) + var_zpencil1(i, 1, k) * cos_wp(theta)
      !       end do
      !       uy(i) = uy(i) * TWO / dtmp%zsz(3)
      !       uz(i) = uz(i) * TWO / dtmp%zsz(3)

      !       do k = 1, dtmp%zsz(3)
      !         theta = dm%h(3) * real((k-1), WP)
      !         var_zpencil1(i, 1, k) = - uz(i) * sin_wp(theta) + uy(i) * cos_wp(theta)
      !       end do
      !     end do
      !   endif
      ! end if
      ! Transpose back to the original pencil
      call transpose_from_y_pencil(var_ypencil, var, dtmp, pencil)
    else 
      call Print_error_msg('Invalid input for IDIM in axis_estimating_radial_xpx')
    end if
    return
  end subroutine axis_estimating_radial_xpx

  !============================================================================
  ! Multiply cylindrical variable by r^n
  !============================================================================
  subroutine multiple_cylindrical_rn(var, dtmp, r, n, pencil)
    implicit none
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), intent(inout)      :: var(:, :, :)
    real(WP), intent(in)         :: r(:)
    integer, intent(in)          :: n
    integer, intent(in)          :: pencil

    integer :: i, j, k, jj, nx, ny, nz, nyst
    real(WP) :: rjn
    !logical :: is_axis

    ! Initialize dimensions based on pencil
    call get_dimensions(dtmp, pencil, nx, ny, nz, nyst)
    !is_axis = .false.
    do j = 1, ny
      jj = nyst + j - 1
      rjn = r(jj)**n
      if (r(jj) > (MAXP * HALF)) then
        !is_axis = .true.
        if (jj /= 1) call Print_error_msg("Error: r(j) = 0 at j /= 1.")
      else
        do k = 1, nz
          do i = 1, nx
            var(i, j, k) = var(i, j, k) * rjn
          end do
        end do
      end if
    end do
  end subroutine multiple_cylindrical_rn

  !============================================================================
  ! Multiply cylindrical variable by r^n (specific for xx4 configuration)
  !============================================================================
  subroutine multiple_cylindrical_rn_xx4(var, dtmp, r, n, pencil)
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), intent(inout)      :: var(:, :, :)
    real(WP), intent(in)         :: r(:)
    integer, intent(in)          :: n
    integer, intent(in)          :: pencil

    integer :: i, j, jj, nx, ny, nz, nyst
    real(WP) :: rjn

    ! Initialize dimensions based on pencil
    call get_dimensions(dtmp, pencil, nx, ny, nz, nyst)

    if (pencil /= IPENCIL(3)) then
      call Print_warning_msg("Warning: This is for z-pencil only.")
    end if

    do j = 1, ny
      jj = nyst + j - 1
      rjn = r(jj)**n
      if (r(jj) > (MAXP * HALF)) then
        if (jj /= 1) call Print_error_msg("Error: r(j) = 0 at j /= 1.")
      else
        do i = 1, nx
          var(i, j, 1) = var(i, j, 1) * rjn
          var(i, j, 2) = var(i, j, 2) * rjn
          var(i, j, 3) = var(i, j, 1)
          var(i, j, 4) = var(i, j, 2)
        end do
      end if
    end do

  end subroutine multiple_cylindrical_rn_xx4

  !============================================================================
  ! Multiply cylindrical variable by r^n (specific for x4x configuration)
  !============================================================================
  subroutine multiple_cylindrical_rn_x4x(var, dtmp, r, n, pencil)
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP), intent(inout)      :: var(:, :, :)
    real(WP), intent(in)         :: r(:)
    integer, intent(in)          :: n
    integer, intent(in)          :: pencil

    integer :: i, k, jmax, nx, ny, nz, nyst
    real(WP) :: r1n, rjn

    ! Initialize dimensions based on pencil
    call get_dimensions(dtmp, pencil, nx, ny, nz, nyst)

    if (pencil /= IPENCIL(2)) then
      call Print_warning_msg("Warning: This is for y-pencil only.")
    end if

    r1n = r(1)**n
    jmax = nyst + ny - 1
    rjn = (r(jmax)**n)

    do k = 1, nz
      do i = 1, nx
        if (r(1) > (MAXP * HALF)) then
          ! Axis handling using estimate_azimuthal_xpx_on_axis or axis_estimating_radial_xpx
        else
          var(i, 1, k) = var(i, 1, k) * r1n
        end if
        var(i, 2, k) = var(i, 2, k) * rjn
        var(i, 3, k) = var(i, 1, k)
        var(i, 4, k) = var(i, 2, k)
      end do
    end do

  end subroutine multiple_cylindrical_rn_x4x

end module cylindrical_rn_mod
!==========================================================================================================
!==========================================================================================================
  subroutine profile_interpolation(nin, yin, uin, nout, ycase, ucase)
    use cubic_spline_interpolation
    use precision_mod
    use print_msg_mod
    implicit none
    integer, intent(in) :: nin
    real(WP), dimension(nin), intent(in) :: yin
    real(WP), dimension(nin), intent(in) :: uin
    integer, intent(in) :: nout
    real(WP), dimension(nout), intent(in)  :: ycase
    real(WP), dimension(nout), intent(out) :: ucase

    integer :: i
    real(WP), allocatable :: cs_b(:), cs_c(:), cs_d(:)

    allocate(cs_b(nin))
    allocate(cs_c(nin))
    allocate(cs_d(nin))

    call cubic_spline (nin, yin, uin, cs_b, cs_c, cs_d)

    do i = 1, nout
      ucase(i) = spline_interpolation(nin, uin, cs_b, cs_c, cs_d, ycase(i))
    end do 

    deallocate(cs_b)
    deallocate(cs_c)
    deallocate(cs_d)

    return
  end subroutine


  !==========================================================================================================


module find_max_min_ave_mod
  use print_msg_mod
  use wtformat_mod
  !
  public  :: Find_maximum_absvar3d_loc
  public  :: Find_max_min_3d
  !public  :: Find_max_min_absvar3d
  public  :: Get_volumetric_average_3d
  public  :: Get_area_average_2d_for_fbcx
  public  :: Get_area_average_2d_for_fbcy
  public  :: Get_area_average_2d_for_fbcz 
contains
!==========================================================================================================
  subroutine is_valid_number_3D(var, varname)
    use parameters_constant_mod
    use ieee_arithmetic
    implicit none

    real(WP), intent(in) :: var(:, :, :)
    character(len=*), intent(in) :: varname
    integer :: nx, ny, nz

    nx = size(var, 1)
    ny = size(var, 2)
    nz = size(var, 3)

    ! Check for large numbers
    if (maxval(dabs(var)) > 1.0e+10) then
        write(*,*) 'Warning: Large number detected (nrank=', nrank, ') in ', trim(varname), ' val = ', maxval(dabs(var))
        write(*,*) 'at rank = ', nrank, ', local index = ', maxloc(var) 
        call Print_error_msg('A large number is found. Stopping execution.')
    end if

    ! Check for NaN values
    if (any(ieee_is_nan(var))) then
        write(*,*) 'NaN detected (nrank=', nrank, ') in ', trim(varname)
        call Print_error_msg('NaN is found. Stopping execution.')
    end if

  end subroutine is_valid_number_3D
!==========================================================================================================
  subroutine Find_maximum_absvar3d_loc(var, varmax_work, dtmp, str, nxst0)
    use precision_mod
    use math_mod
    use mpi_mod
    use parameters_constant_mod
    use typeconvert_mod
    use wtformat_mod
    implicit none

    real(WP), intent(in)  :: var(:, :, :)
    integer, intent(in), optional   :: nxst0
    type(DECOMP_INFO), intent(in) :: dtmp
    character(len = *), intent(in) :: str
    real(WP), intent(out) :: varmax_work

    real(WP)   :: varmax
    integer :: idg(3), idl(3)
#ifdef DEBUG_STEPS 
    integer :: idg_work(3)
#endif

    integer :: i, j, k, nx, ny, nz
    integer :: idgmax(3), nxst

    if(present(nxst0)) then 
      nxst = nxst0
    else
      nxst = 1
    end if
    nx = size(var, 1)
    ny = size(var, 2)
    nz = size(var, 3)

    idgmax(1) = dtmp%xsz(1)
    idgmax(2) = dtmp%ysz(2)
    idgmax(3) = dtmp%zsz(3)

    varmax = ZERO
    idl = 0
    idg = 0 
    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          if(abs_wp(var(i, j, k)) > varmax) then
            varmax = abs_wp(var(i, j, k))
            !idg = local2global_3indices(idl, dtmp)
            idg(1) = nxst + i - 1
            idg(2) = dtmp%xst(2) + j - 1
            idg(3) = dtmp%xst(3) + k - 1
          end if
        end do
      end do
    end do
    !varmax = MAXVAL( abs_wp( var ) ) 
    !call mpi_barrier(MPI_COMM_WORLD, ierror)
    call mpi_allreduce(varmax, varmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)

#ifdef DEBUG_STEPS 
    if(abs_wp(varmax_work - varmax) <= MINP) then
      call mpi_send(idg, 3, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, ierror)
    end if

    if(nrank == 0 .and. .not. is_IO_off) then
      call mpi_recv(idg_work, 3, MPI_INTEGER, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
      !write (*, '(1X, A33, 1ES19.12)') 'maximum '//trim(str), varmax_work
  
      write (*, '(3X, A33, 1ES19.12, A, 3(1I6.1, A6))') 'maximum '//trim(str), varmax_work, ' at index', &
      idg_work(1), '/'//int2str(idgmax(1)), &
      idg_work(2), '/'//int2str(idgmax(2)), &
      idg_work(3), '/'//int2str(idgmax(3))

    end if
#else
    if(nrank == 0 .and. .not. is_IO_off) then
      write (*, wrtfmt1el) 'maximum '//trim(str), varmax_work
    end if
#endif
    return
  end subroutine

  !==========================================================================================================
  subroutine Find_max_min_3d(var, opt_abs, opt_calc, opt_work, opt_name)
    use precision_mod
    use math_mod
    use mpi_mod
    use parameters_constant_mod
    implicit none
    ! arguments 
    real(WP), intent(in)  :: var(:, :, :)
    real(WP), intent(out), optional :: opt_work(2)
    character(len = 3), intent(in), optional :: opt_abs
    character(len = 4), intent(in), optional :: opt_calc
    character(len = *), intent(in), optional :: opt_name
    ! local variables
    real(WP):: varmax_work, varmin_work
    real(WP):: varmax, varmin, dummy
    logical :: imax, imin, iabs
    character(len=5) :: abs

    integer :: i, j, k, nx, ny, nz
    nx = size(var, 1)
    ny = size(var, 2)
    nz = size(var, 3)

    if(present(opt_abs)) then
      iabs = .true.
    else
      iabs = .false.
    end if

    varmax = MINN
    varmin = MAXP
    imax = .true.
    imin = .true.
    if (present(opt_calc)) then
      imax = .false.
      imin = .false.
      select case (opt_calc)
      case ('MAXI')
        varmax = MINN
        imax = .true.
      case ('MINI')
        varmin = MAXP
        imin = .true.
      case default
        varmax = MINN
        varmin = MAXP
        imax = .true.
        imin = .true.
      end select
    end if

    do k = 1, nz
      do j = 1, ny
        do i = 1, nx
          dummy = var(i, j, k)
          if(iabs) dummy = abs_wp(dummy)
          if (imax .and. dummy  > varmax) varmax = dummy
          if (imin .and. dummy  < varmin) varmin = dummy
        end do
      end do
    end do

    !call mpi_barrier(MPI_COMM_WORLD, ierror)
    if (imax) call mpi_allreduce(varmax, varmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
    if (imin) call mpi_allreduce(varmin, varmin_work, 1, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)

    if(present(opt_work)) then
      opt_work = MAXP
      if (imin) opt_work(1) = varmin_work
      if (imax) opt_work(2) = varmax_work
    end if
    if(nrank == 0 .and. present(opt_name)) then
      if(present(opt_abs)) then
        abs = '-abs-'
      else
        abs = '-'
      end if
      if(imax .and. imin) then
        write (*, wrtfmt2ae) 'maximum'//trim(abs)//opt_name, varmax_work, &
                             'minimum'//trim(abs)//opt_name, varmin_work
      else if(imax) then
        if(opt_name == 'Mass Consv. (bulk    ) =') then
          write (*, wrtfmt1el, advance='no') 'maximum'//trim(abs)//opt_name, varmax_work
        else
          write (*, wrtfmt1el) 'maximum'//trim(abs)//opt_name, varmax_work
        end if
      else if(imin) then
        write (*, wrtfmt1el) 'minimum'//trim(abs)//opt_name, varmin_work
      else
      end if
    end if
! #ifdef DEBUG_FFT
!     if(varmax_work >   MAXVELO) call Print_error_msg('varmax_work >   MAXVELO')
!     if(varmin_work < - MAXVELO) call Print_error_msg('varmax_work < - MAXVELO')
! #endif

    return
  end subroutine

!   !==========================================================================================================
!   subroutine Find_max_min_1d(var,  str, fmt)
!     use precision_mod
!     use math_mod
!     use mpi_mod
!     use parameters_constant_mod
!     implicit none

!     real(WP), intent(in)  :: var(:)
!     character(len = *), intent(in) :: str
!     character(len = *), intent(in), optional :: fmt
    
!     real(WP):: varmax_work, varmin_work
!     real(WP)   :: varmax, varmin

!     integer :: i, nx
!     nx = size(var, 1)
!     varmax = MINN
!     varmin = MAXP
!     do i = 1, nx
!       if( var(i)  > varmax) varmax = var(i)
!       if( var(i)  < varmin) varmin = var(i)
!     end do

!     call mpi_barrier(MPI_COMM_WORLD, ierror)
!     call mpi_allreduce(varmax, varmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
!     call mpi_allreduce(varmin, varmin_work, 1, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)

!     if(nrank == 0) then
!       write (*, *) '        maximum '//str, varmax_work
!       write (*, *) '        minimum '//str, varmin_work
!     end if
! #ifdef DEBUG_FFT
!     if(varmax_work >   MAXVELO) stop
!     if(varmin_work < - MAXVELO) stop
! #endif

!     return
!   end subroutine

! !==========================================================================================================
!   subroutine Find_max_min_absvar3d(var,  str, fmt)
!     use precision_mod
!     use math_mod
!     use mpi_mod
!     use parameters_constant_mod
!     implicit none

!     real(WP), intent(in)  :: var(:, :, :)
!     character(len = *), intent(in) :: str
!     character(len = *), intent(in) :: fmt
    
!     real(WP):: varmax_work, varmin_work
!     real(WP)   :: varmax, varmin

!     integer :: i, j, k, nx, ny, nz
!     nx = size(var, 1)
!     ny = size(var, 2)
!     nz = size(var, 3)

!     varmax = MINN
!     varmin = MAXP
!     do k = 1, nz
!       do j = 1, ny
!         do i = 1, nx
!           if( var(i, j, k)  > varmax) varmax = abs_wp( var(i, j, k) )
!           if( var(i, j, k)  < varmin) varmin = abs_wp( var(i, j, k) )
!         end do
!       end do
!     end do

!     !call mpi_barrier(MPI_COMM_WORLD, ierror)
!     call mpi_allreduce(varmax, varmax_work, 1, MPI_REAL_WP, MPI_MAX, MPI_COMM_WORLD, ierror)
!     call mpi_allreduce(varmin, varmin_work, 1, MPI_REAL_WP, MPI_MIN, MPI_COMM_WORLD, ierror)

!     if(nrank == 0) then
!       write (*, fmt) 'max. |'//str//'|', varmax_work, ' min. |'//str//'|', varmin_work
!     end if
! ! #ifdef DEBUG_FFT
! !     if(varmax_work >   MAXVELO) call Print_error_msg('varmax_work >  MAXVELO')
! !     if(varmin_work < - MAXVELO) call Print_error_msg('varmax_work < -MAXVELO')
! ! #endif

!     return
!   end subroutine
!==========================================================================================================
  subroutine Get_volumetric_average_3d(dm, dtmp, var, fo_work, itype, str)
    use mpi_mod
    use udf_type_mod
    use parameters_constant_mod
    use decomp_2d
    use wtformat_mod
    
    implicit none
    type(t_domain),  intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP),          intent(in) :: var(:, :, :)
    real(WP),          intent(out):: fo_work
    integer,           intent(in) :: itype
    character(*), optional, intent(in) :: str
 
    real(WP) :: vol, fo, vol_work, array(2), array_work(2)
#ifdef DEBUG_STEPS 
    real(WP) :: vol_real
#endif
    integer :: i, j, k, jj
    real(WP) :: dx, dy, dz, ymapping

    !----------------------------------------------------------------------------------------------------------
    ! default: x-pencil
    ! use the chain rule to get integral in the stretching function
    ! integral(f(y), dy) = integral(f(y(s)), dy(s)) = integral(f(y(s)) * dy/ds, ds)
    !----------------------------------------------------------------------------------------------------------
      if (dtmp%ysz(2)==dm%np_geo(2)) then
        call Print_error_msg('Get_volumetric_average_3d only supports input of dxcx')
      end if

      vol = ZERO
      fo  = ZERO
      dx = dm%h(1)
      dy = dm%h(2)
      dz = dm%h(3)
      do j = 1, dtmp%xsz(2)
        jj = dtmp%xst(2) + j - 1 !(j, dtmp)
        if(dm%icoordinate == ICYLINDRICAL) &
        dz = dm%h(3) * dm%rc(jj)
        if(dm%is_stretching(2)) &
        dy = dm%h(2) / dm%yMappingcc(jj, 1)
        do k = 1, dtmp%xsz(3)
          do i = 1, dtmp%xsz(1)
            fo = fo + var(i, j, k) * dy * dx * dz
            vol = vol + dy * dx * dz
          end do
        end do
      end do

      !call mpi_barrier(MPI_COMM_WORLD, ierror)
      array(1) = fo
      array(2) = vol
      call mpi_allreduce( array, array_work, 2, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
      fo_work  = array_work(1)
      vol_work = array_work(2)

      if(itype == SPACE_AVERAGE) then
        fo_work = fo_work / vol_work
      else if(itype == SPACE_INTEGRAL) then
        ! do nothing
      end if
!write(*,*) 'test_vol' , vol_work
#ifdef DEBUG_STEPS  
      if (dabs(vol_work - dm%vol) > 1.0e-10_WP) write (*, *) 'volume calc error: ', vol_work, dm%vol
      if(nrank == 0 .and. present(str)) then
        if(itype == SPACE_AVERAGE) then
          write (*, wrtfmt1e) " volumetric average of "//trim(str)//" = ", fo_work
        else 
          write (*, wrtfmt1e) " volumetric integeral of "//trim(str)//" = ", fo_work
        end if
      end if
#endif
    return
  end subroutine 
!==========================================================================================================
  subroutine Get_area_average_2d_for_fbcx(dm, dtmp, var, fo_work, itype, str)
    use mpi_mod
    use udf_type_mod
    use parameters_constant_mod
    use decomp_2d
    use wtformat_mod
    
    implicit none
    type(t_domain),  intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP),          intent(in) :: var(:, :, :)
    real(WP),          intent(out):: fo_work(2)
    integer,           intent(in) :: itype
    character(4),      intent(in) :: str
 
    real(WP) :: area, fo(2), area_work, array(3), array_work(3)
#ifdef DEBUG_STEPS 
    real(WP) :: area_real
#endif
    integer :: j, k, jj, nx, ny, nz
    real(WP) :: dy, dz

    !if(dtmp /= dm%dpcc) call Print_error_msg("Error: Get_area_average_2d_for_yz_pcc is for pcc only.")
    if(dtmp%xsz(1) /= dtmp%xen(1)) call Print_error_msg("Error. This is not x-pencil.")
    ! x pencil only
    !----------------------------------------------------------------------------------------------------------
    ! default: x-pencil
    ! use the chain rule to get integral in the stretching function
    ! integral(f(y), dy) = integral(f(y(s)), dy(s)) = integral(f(y(s)) * dy/ds, ds)
    !----------------------------------------------------------------------------------------------------------
      area = ZERO
      fo  = ZERO
      dy = dm%h(2)
      dz = dm%h(3)
      if(str=='varx') then
        nx = dtmp%xsz(1)
      else if(str=='fbcx') then
        nx = 2
      else
        call Print_error_msg("Error: Get_area_average_2d_for_fbcx is for varx or fbcx only.")
      end if
      ny = dtmp%xsz(2)
      nz = dtmp%xsz(3)
      do j = 1, ny
        jj = dtmp%xst(2) + j - 1 !(j, dtmp)
        !dy = dm%yp(jj+1) - dm%yp(jj)
        if(dm%is_stretching(2)) &
        dy = dm%h(2) / dm%yMappingcc(jj, 1)
        if(dm%icoordinate == ICYLINDRICAL) then
          dz = dm%h(3) * dm%rc(jj)
        end if
        do k = 1, nz
          fo(1) = fo(1) + var(1,  j, k) * dy * dz
          fo(2) = fo(2) + var(nx, j, k) * dy * dz
          area = area + dy * dz
        end do
      end do

      array(1:2) = fo(1:2)
      array(3) = area
      call mpi_allreduce(array,  array_work, 3, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
      fo_work(1:2) = array_work(1:2)
      area_work    = array_work(3)
      
      if(itype == SPACE_AVERAGE) then
        fo_work(:) = fo_work(:) / area_work
      else if(itype == SPACE_INTEGRAL) then
        ! do nothing
      end if

    return
  end subroutine
!==========================================================================================================
!==========================================================================================================
  subroutine Get_area_average_2d_for_fbcz(dm, dtmp, var, fo_work, itype, str)
    use mpi_mod
    use udf_type_mod
    use parameters_constant_mod
    use decomp_2d
    use wtformat_mod
    
    implicit none
    type(t_domain),  intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP),          intent(in) :: var(:, :, :)
    real(WP),          intent(out):: fo_work(2)
    integer,           intent(in) :: itype
    character(4),      intent(in) :: str
 
    real(WP) :: area, fo(2), area_work, array(3), array_work(3)
#ifdef DEBUG_STEPS 
    real(WP) :: area_real
#endif
    integer :: j, i, jj, nx, ny, nz
    real(WP) :: dy, dx

    !if(dtmp /= dm%dccp) call Print_error_msg("Error: Get_area_average_2d_for_yz_pcc is for ccp only.")
    if(dtmp%zsz(3) /= dtmp%zen(3)) call Print_error_msg("Error. This is not z-pencil.")
    !----------------------------------------------------------------------------------------------------------
    ! default: x-pencil
    ! use the chain rule to get integral in the stretching function
    ! integral(f(y), dy) = integral(f(y(s)), dy(s)) = integral(f(y(s)) * dy/ds, ds)
    !----------------------------------------------------------------------------------------------------------
      area = ZERO
      fo  = ZERO
      dy = dm%h(2)
      dx = dm%h(1)
      nx = dtmp%zsz(1)
      ny = dtmp%zsz(2)
      if(str=='varz') then
        nz = dtmp%zsz(3)
      else if(str=='fbcz') then
        nz = 2
      else
        call Print_error_msg("Error: Get_area_average_2d_for_fbcz is for varz or fbcz only.")
      end if
      do j = 1, ny
        jj = dtmp%zst(2) + j - 1 !(j, dtmp)
        if(dm%is_stretching(2)) &
        dy = dm%h(2) / dm%yMappingcc(jj, 1)
        do i = 1, nx
          fo(1) = fo(1) + var(i, j, 1)  * dy * dx
          fo(2) = fo(2) + var(i, j, nz) * dy * dx
          area = area + dy * dx
        end do
      end do

      array(1:2) = fo(1:2)
      array(3) = area
      call mpi_allreduce(array,  array_work, 3, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
      fo_work(1:2) = array_work(1:2)
      area_work    = array_work(3)

      if(itype == SPACE_AVERAGE) then
        fo_work(:) = fo_work(:) / area_work
      else if(itype == SPACE_INTEGRAL) then
        ! do nothing
      end if

    return
  end subroutine
!==========================================================================================================
  subroutine Get_area_average_2d_for_fbcy(dm, dtmp, var, fo_work, itype, str, is_rf)
    use mpi_mod
    use udf_type_mod
    use parameters_constant_mod
    use decomp_2d
    use wtformat_mod
    
    implicit none
    type(t_domain),  intent(in) :: dm
    type(DECOMP_INFO), intent(in) :: dtmp
    real(WP),          intent(in) :: var(:, :, :)
    real(WP),          intent(out):: fo_work(2)
    integer,           intent(in) :: itype
    character(4),      intent(in) :: str
    logical,           intent(in) :: is_rf
 
    real(WP) :: area(2), fo(2), area_work(2), array(4), array_work(4)
#ifdef DEBUG_STEPS 
    real(WP) :: area_real
#endif
    integer :: i, k, ny, nz, nx
    real(WP) :: dz, dx, dz1, dzn

    !if(dtmp /= dm%dcpc) call Print_error_msg("Error: Get_area_average_2d_for_yz_pcc is for pcc only.")
    if(dtmp%ysz(2) /= dtmp%yen(2)) call Print_error_msg("Error. This is not y-pencil.")
    !----------------------------------------------------------------------------------------------------------
    ! default: x-pencil
    ! use the chain rule to get integral in the stretching function
    ! integral(f(y), dy) = integral(f(y(s)), dy(s)) = integral(f(y(s)) * dy/ds, ds)
    !----------------------------------------------------------------------------------------------------------
      area = ZERO
      fo  = ZERO
      
      nx = dtmp%ysz(1)
      if(str=='vary') then
        ny = dtmp%ysz(2)
      else if(str=='fbcy') then
        ny = 2
      else
        call Print_error_msg("Error: Get_area_average_2d_for_fbcy is for vary or fbcy only.")
      end if
      nz = dtmp%ysz(3)

      dx = dm%h(1)
      dz = dm%h(3)
      !! Note: qy = r*ur, integral r*ur * dtheta dx, therefore no extra r here.
      if(.not. is_rf) then
        dz1 = dm%h(3) * dm%rp(1)
        dzn = dm%h(3) * dm%rp(ny)
      else
        dz1 = dz
        dzn = dz
      end if
      do k = 1, nz
        do i = 1, nx
          fo(1) = fo(1) + var(i, 1,  k) * dx * dz1
          fo(2) = fo(2) + var(i, ny, k) * dx * dzn
          area(1) = area(1) + dx * dz1
          area(2) = area(2) + dx * dzn
        end do
      end do

      array(1:2) = fo(1:2)
      array(3:4) = area(1:2)
      call mpi_allreduce(array,  array_work, 4, MPI_REAL_WP, MPI_SUM, MPI_COMM_WORLD, ierror)
      fo_work(1:2)   = array_work(1:2)
      area_work(1:2) = array_work(3:4)

      if(itype == SPACE_AVERAGE) then
        if(dm%icase == ICASE_PIPE) then
          fo_work(2) = fo_work(2) / area_work(2)
          fo_work(1) = ZERO
        else
          fo_work(:) = fo_work(:) / area_work(:)
        end if
      else if(itype == SPACE_INTEGRAL) then
        if(dm%icase == ICASE_PIPE) fo_work(1) = ZERO
      end if

    return
  end subroutine
 end module
