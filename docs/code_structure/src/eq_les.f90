module les_mod
  use decomp_2d
  use operations
  use precision_mod, only: WP
  use udf_type_mod, only: t_domain, t_flow
  implicit none
  !
  private :: calculate_strain_rate_tensor
  private :: calculate_strain_rate_magnitude_square, calculate_wale_tensor
  private :: calculate_wale_tensor_magnitude_square, calculate_eddy_viscosity_wale
  !
  public :: calculate_les_wale ! ref; https://www.cfd-online.com/Wiki/Wall-adapting_local_eddy-viscosity_(WALE)_model
  !public :: calculate_les_smag

contains
!==========================================================================================================
  !> Form the resolved strain-rate tensor S_ij = 0.5*(du_i/dx_j + du_j/dx_i).
  !> - velocity_gradient: cell-centred velocity-gradient tensor.
  !> - strain_rate: output symmetric strain-rate tensor.
  subroutine calculate_strain_rate_tensor(velocity_gradient, strain_rate)
    implicit none
    real(WP), intent(in)  :: velocity_gradient(:, :, :, :, :)
    real(WP), intent(out) :: strain_rate(:, :, :, :, :)
    integer :: i, j
    do i = 1, 3
        do j = 1, 3
            strain_rate(:, :, :, i, j) = 0.5_WP * (velocity_gradient(:, :, :, i, j) + &
                                                   velocity_gradient(:, :, :, j, i))
        end do
    end do
    return
  end subroutine calculate_strain_rate_tensor

!==========================================================================================================
  !> Compute S_ij S_ij at each cell centre.
  !> - strain_rate: resolved strain-rate tensor.
  !> - strain_rate_mag2: output tensor contraction S_ij S_ij.
  subroutine calculate_strain_rate_magnitude_square(strain_rate, strain_rate_mag2)
    implicit none
    real(WP), intent(in)  :: strain_rate(:, :, :, :, :)
    real(WP), intent(out) :: strain_rate_mag2(:, :, :)

    strain_rate_mag2(:, :, :) = strain_rate(:, :, :, 1, 1)**2 + strain_rate(:, :, :, 1, 2)**2 + &
                                strain_rate(:, :, :, 1, 3)**2 + strain_rate(:, :, :, 2, 1)**2 + &
                                strain_rate(:, :, :, 2, 2)**2 + strain_rate(:, :, :, 2, 3)**2 + &
                                strain_rate(:, :, :, 3, 1)**2 + strain_rate(:, :, :, 3, 2)**2 + &
                                strain_rate(:, :, :, 3, 3)**2
    return
  end subroutine calculate_strain_rate_magnitude_square

!==========================================================================================================
  !> Compute the traceless symmetric square-gradient tensor used by the WALE model.
  !> - velocity_gradient: cell-centred velocity-gradient tensor.
  !> - wale_tensor: output S^d_ij tensor.
  subroutine calculate_wale_tensor(velocity_gradient, wale_tensor)
      implicit none
      real(WP), intent(in)  :: velocity_gradient(:, :, :, :, :)
      real(WP), intent(out) :: wale_tensor(:, :, :, :, :)
      integer :: i, j
      real(WP), dimension(size(velocity_gradient,1), size(velocity_gradient,2), size(velocity_gradient,3)) :: &
          trace_gradient_square

      trace_gradient_square = velocity_gradient(:, :, :, 1, 1) * velocity_gradient(:, :, :, 1, 1) + &
                              velocity_gradient(:, :, :, 1, 2) * velocity_gradient(:, :, :, 2, 1) + &
                              velocity_gradient(:, :, :, 1, 3) * velocity_gradient(:, :, :, 3, 1) + &
                              velocity_gradient(:, :, :, 2, 1) * velocity_gradient(:, :, :, 1, 2) + &
                              velocity_gradient(:, :, :, 2, 2) * velocity_gradient(:, :, :, 2, 2) + &
                              velocity_gradient(:, :, :, 2, 3) * velocity_gradient(:, :, :, 3, 2) + &
                              velocity_gradient(:, :, :, 3, 1) * velocity_gradient(:, :, :, 1, 3) + &
                              velocity_gradient(:, :, :, 3, 2) * velocity_gradient(:, :, :, 2, 3) + &
                              velocity_gradient(:, :, :, 3, 3) * velocity_gradient(:, :, :, 3, 3)

      do i = 1, 3
          do j = 1, 3
              wale_tensor(:, :, :, i, j) = 0.5_WP * &
                  (velocity_gradient(:, :, :, i, 1) * velocity_gradient(:, :, :, 1, j) + &
                    velocity_gradient(:, :, :, i, 2) * velocity_gradient(:, :, :, 2, j) + &
                    velocity_gradient(:, :, :, i, 3) * velocity_gradient(:, :, :, 3, j) + &
                    velocity_gradient(:, :, :, j, 1) * velocity_gradient(:, :, :, 1, i) + &
                    velocity_gradient(:, :, :, j, 2) * velocity_gradient(:, :, :, 2, i) + &
                    velocity_gradient(:, :, :, j, 3) * velocity_gradient(:, :, :, 3, i))
              if (i == j) then
                  wale_tensor(:, :, :, i, j) = wale_tensor(:, :, :, i, j) - &
                                                (1.0_WP/3.0_WP) * trace_gradient_square
              end if
          end do
      end do
    return
  end subroutine calculate_wale_tensor
!==========================================================================================================
  !> Compute S^d_ij S^d_ij at each cell centre.
  !> - wale_tensor: WALE traceless symmetric square-gradient tensor.
  !> - wale_tensor_mag2: output tensor contraction S^d_ij S^d_ij.
  subroutine calculate_wale_tensor_magnitude_square(wale_tensor, wale_tensor_mag2)
    implicit none
    real(WP), intent(in)  :: wale_tensor(:, :, :, :, :)
    real(WP), intent(out) :: wale_tensor_mag2(:, :, :)

    wale_tensor_mag2(:, :, :) = wale_tensor(:, :, :, 1, 1)**2 + wale_tensor(:, :, :, 1, 2)**2 + &
                                wale_tensor(:, :, :, 1, 3)**2 + wale_tensor(:, :, :, 2, 1)**2 + &
                                wale_tensor(:, :, :, 2, 2)**2 + wale_tensor(:, :, :, 2, 3)**2 + &
                                wale_tensor(:, :, :, 3, 1)**2 + wale_tensor(:, :, :, 3, 2)**2 + &
                                wale_tensor(:, :, :, 3, 3)**2
    return
  end subroutine calculate_wale_tensor_magnitude_square
!==========================================================================================================
  !> Evaluate the WALE kinematic eddy viscosity from strain and WALE invariants.
  !> - dm: domain descriptor used for grid spacing.
  !> - strain_rate_mag2: S_ij S_ij field.
  !> - wale_tensor_mag2: S^d_ij S^d_ij field.
  !> - eddy_visc_kinematic: output nondimensional kinematic eddy viscosity.
  subroutine calculate_eddy_viscosity_wale(dm, strain_rate_mag2, wale_tensor_mag2, eddy_visc_kinematic)
    use parameters_constant_mod, only: ZERO, ICYLINDRICAL
    implicit none
    type(t_domain), intent(in) :: dm
    real(WP),       intent(in)  :: strain_rate_mag2(:, :, :)
    real(WP),       intent(in)  :: wale_tensor_mag2(:, :, :)
    real(WP),       intent(out) :: eddy_visc_kinematic(:, :, :)
    real(WP) :: Cw, dx, dy, dz
    real(WP), dimension(size(eddy_visc_kinematic,2)) :: delta
    real(WP), dimension(size(eddy_visc_kinematic,1), size(eddy_visc_kinematic,2), size(eddy_visc_kinematic,3)) :: denominator
    integer :: j, jj

    Cw = 0.5_WP
!----------------------------------------------------------------------------------------------------------
!   Filter width = cube root of the local *physical* cell volume, the same measure geometry.f90 uses to
!   build dm%vol: dy is the stretched wall-normal spacing h(2)/yMappingcc, and in cylindrical coordinates
!   the azimuthal arc length is r*dtheta. The computational spacings h(1:3) alone misjudge delta by a
!   factor of ~12 across a stretched channel and overestimate nu_t by ~8x in the first cell off a pipe axis.
!   Limitation: this is an isotropic measure. Near a pipe axis the cell aspect ratio reaches O(50), where
!   an anisotropy correction (e.g. Scotti et al. 1993) would be more appropriate. Not implemented.
!----------------------------------------------------------------------------------------------------------
    dx = dm%h(1)
    do j = 1, size(eddy_visc_kinematic, 2)
      jj = dm%dccc%xst(2) + j - 1
      dy = dm%h(2)
      if(dm%is_stretching(2)) dy = dm%h(2) / dm%yMappingcc(jj, 1)
      dz = dm%h(3)
      if(dm%icoordinate == ICYLINDRICAL) dz = dm%h(3) * dm%rc(jj)
      delta(j) = (dx * dy * dz)**(1.0_WP/3.0_WP)
    end do

    denominator = (strain_rate_mag2)**(5.0_WP/2.0_WP) + (wale_tensor_mag2)**(5.0_WP/4.0_WP)
    do j = 1, size(eddy_visc_kinematic, 2)
      where (denominator(:, j, :) > ZERO)
          eddy_visc_kinematic(:, j, :) = (Cw * delta(j))**2 * &
                                         (wale_tensor_mag2(:, j, :))**(3.0_WP/2.0_WP) / denominator(:, j, :)
      elsewhere
          eddy_visc_kinematic(:, j, :) = ZERO
      end where
    end do
    return
  end subroutine calculate_eddy_viscosity_wale
!==========================================================================================================
  !> Calculate turbulent dynamic viscosity using the WALE LES model.
  !> - fl: flow state; fl%tVisc is overwritten with turbulent dynamic viscosity.
  !> - dm: domain/decomposition and boundary-condition metadata.
  !> - opt_max_strain_rate_mag2: optional global max of S_ij S_ij. The reduction is a
  !>   collective, so it is only performed when the caller asks for it - the timestep
  !>   loop does not, and pays nothing.
  subroutine calculate_les_wale(fl, dm, opt_max_strain_rate_mag2)
    use find_max_min_ave_mod, only: Find_max_min_3d
    use flow_gradient_mod, only: get_velocity_and_gradient_ccc
    use parameters_constant_mod, only: ZERO
    implicit none
    type(t_flow),   intent(inout) :: fl
    type(t_domain), intent(in)    :: dm
    real(WP), intent(out), optional :: opt_max_strain_rate_mag2
    real(WP) :: maxmin_strain(2)
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3) :: velocity_gradient
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3) :: strain_rate
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3), 3, 3) :: wale_tensor
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: strain_rate_mag2
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: wale_tensor_mag2
    real(WP), dimension(dm%dccc%xsz(1), dm%dccc%xsz(2), dm%dccc%xsz(3)) :: eddy_visc_kinematic

    ! physical velocity-gradient tensor; in cylindrical coordinates this is
    ! the (x, r, theta) tensor including the 1/r and u/r metric terms, not the
    ! derivatives of the stored variables qy = r*u_r and qz = u_theta.
    call get_velocity_and_gradient_ccc(fl, dm, velocity_gradient)
    call calculate_strain_rate_tensor(velocity_gradient, strain_rate)
    call calculate_strain_rate_magnitude_square(strain_rate, strain_rate_mag2)
    if(present(opt_max_strain_rate_mag2)) then
      ! opt_work(2) receives the allreduced maximum; opt_name is withheld so the
      ! helper's ‰-drift line is not printed against a meaningless previous value.
      maxmin_strain = ZERO
      call Find_max_min_3d(strain_rate_mag2, opt_calc = 'MAXI', opt_work = maxmin_strain)
      opt_max_strain_rate_mag2 = maxmin_strain(2)
    end if
    call calculate_wale_tensor(velocity_gradient, wale_tensor)
    call calculate_wale_tensor_magnitude_square(wale_tensor, wale_tensor_mag2)
    call calculate_eddy_viscosity_wale(dm, strain_rate_mag2, wale_tensor_mag2, eddy_visc_kinematic)

    if (allocated(fl%dDens)) then
        fl%tVisc = fl%dDens * eddy_visc_kinematic
    else
        fl%tVisc = eddy_visc_kinematic
    end if
    return
  end subroutine calculate_les_wale

end module les_mod
